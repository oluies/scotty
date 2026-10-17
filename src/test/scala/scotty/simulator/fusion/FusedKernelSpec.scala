package scotty.simulator.fusion

import org.scalatest.flatspec.AnyFlatSpec
import scotty.TestHelpers
import scotty.quantum.*
import scotty.quantum.gate.Controlled
import scotty.quantum.gate.StandardGate.*

/** Covers [[FusedKernel.applyBlock]], the single-threaded path.
  *
  * It exists for Scala.js, where `FusedRunner`'s parallel collections cannot be linked, which means
  * nothing in this repository would otherwise exercise it: every other spec goes through
  * `FusedRunner`. The two implementations share their inner loops but duplicate the form dispatch,
  * the stride setup and the slice count, so without this they could drift — and the drift would
  * surface only in the browser build the code exists to serve.
  */
class FusedKernelSpec extends AnyFlatSpec with TestHelpers {
  val Tolerance: Double = 1e-9

  "The single-threaded kernel" should "agree with the parallel runner on a diagonal block" in {
    assertAgrees(Circuit(H(0), H(1), H(2), RZ(0.7, 1), CZ(0, 2)))
  }

  it should "agree on a monomial block" in {
    assertAgrees(Circuit(H(0), H(1), H(2), X(1), CNOT(0, 2), SWAP(1, 2)))
  }

  it should "agree on a dense block" in {
    assertAgrees(Circuit(H(0), RY(0.4, 1), RX(1.1, 2), Controlled(0, H(1))))
  }

  it should "agree across every form at once, at every width" in {
    assertAgrees(Circuit(H(0), RZ(0.3, 1), CNOT(1, 2), RY(0.9, 2), CZ(0, 1), SWAP(0, 2), T(1)))
  }

  /** Applies a circuit through both paths and compares the states amplitude by amplitude. */
  def assertAgrees(circuit: Circuit): Unit =
    (1 to 4).foreach: maxWidth =>
      val fused = GateFusion.fuse(circuit, maxWidth)
      val simulator = scotty.simulator.QuantumSimulator()

      val viaRunner = simulator.registerToState(circuit.register)
      FusedRunner.applyAll(viaRunner, fused, simulator)

      val viaKernel = simulator.registerToState(circuit.register)
      fused.ops.foreach:
        case block: FusedBlock => FusedKernel.applyBlock(viaKernel, fused.qubitCount, block)
        case native: NativeOp  => simulator.applyGate(viaKernel, native.gate)

      assert(viaKernel.length == viaRunner.length)

      viaKernel.indices.foreach: i =>
        withClue(s"maxWidth $maxWidth, amplitude ${i / 2}: ") {
          assert(Math.abs(viaKernel(i) - viaRunner(i)) < Tolerance)
        }

      // And both have to be the simulator, not merely each other.
      val expected = sim.run(circuit) match
        case s: Superposition => s.state
        case other            => fail(s"expected a superposition, got $other")

      expected.indices.foreach: i =>
        withClue(s"maxWidth $maxWidth vs simulator, amplitude ${i / 2}: ") {
          assert(Math.abs(viaKernel(i) - expected(i)) < Tolerance)
        }
}
