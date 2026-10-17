package scotty.algorithm

import org.scalatest.flatspec.AnyFlatSpec
import scotty.TestHelpers
import scotty.quantum.*
import scotty.quantum.gate.StandardGate.X

class QftSpec extends AnyFlatSpec with TestHelpers {
  val Tolerance: Double = 1e-9

  "The QFT" should "match the DFT matrix on every basis state" in {
    // Checked against an explicitly constructed DFT rather than against another circuit, so this
    // pins the convention rather than just confirming two circuits agree.
    for qubitCount <- 1 to 4 do
      val n = 1 << qubitCount

      for x <- 0 until n do
        val state = transformed(qubitCount, x, Qft.gates((0 until qubitCount).toIndexedSeq))

        for y <- 0 until n do
          val phase = 2 * Math.PI * x * y / n
          val expectedRe = Math.cos(phase) / Math.sqrt(n.toDouble)
          val expectedIm = Math.sin(phase) / Math.sqrt(n.toDouble)

          withClue(s"$qubitCount qubits, |$x> -> amplitude $y: ") {
            assert(Math.abs(state(2 * y) - expectedRe) < Tolerance)
            assert(Math.abs(state(2 * y + 1) - expectedIm) < Tolerance)
          }
  }

  it should "be undone by its inverse" in {
    for qubitCount <- 1 to 4 do
      val qubits = (0 until qubitCount).toIndexedSeq
      val roundTrip = Qft.gates(qubits) ++ Qft.inverseGates(qubits)

      for x <- 0 until (1 << qubitCount) do
        val state = transformed(qubitCount, x, roundTrip)

        withClue(s"$qubitCount qubits, |$x>: ") {
          state.indices.foreach: i =>
            val expected = if i == 2 * x then 1d else 0d

            assert(Math.abs(state(i) - expected) < Tolerance)
        }
  }

  it should "produce a uniform superposition from |0>" in {
    val state = transformed(3, 0, Qft.gates((0 until 3).toIndexedSeq))

    (0 until 8).foreach: y =>
      assert(Math.abs(state(2 * y) - 1 / Math.sqrt(8d)) < Tolerance)
  }

  /** `gates` applied to the basis state `x` over `qubitCount` qubits. */
  def transformed(qubitCount: Int, x: Int, gates: Seq[scotty.quantum.gate.Gate]): Array[Double] =
    val preparation = (0 until qubitCount).collect { case q if ((x >> q) & 1) == 1 => X(q) }
    val circuit = Circuit(Circuit.generateRegister(qubitCount), (preparation ++ gates)*)

    sim.run(circuit) match
      case s: Superposition => s.state
      case other            => fail(s"expected a superposition, got $other")
}
