package scotty

import org.scalatest.flatspec.AnyFlatSpec
import scotty.quantum.gate.StandardGate.{CNOT, H, RZ, Z}
import scotty.quantum.gate.Controlled
import scotty.quantum.math.{Complex, MathUtils}
import scotty.quantum.math.MathUtils.~=
import scotty.algorithm.{CompiledShor, Grover, IterativeShor, Qft, SemiclassicalShor, Shor}
import scotty.quantum.{Circuit, Qubit, QubitProbabilityReader, QubitRegister, Superposition}
import scotty.simulator.QuantumSimulator
import scotty.simulator.fusion.{BlockForm, FusedRunner, GateFusion}

/** The examples from README.md, kept as tests so the documented public API cannot drift out of
  * compiling, and so the figures quoted alongside them cannot drift out of being true.
  */
class ReadmeExampleSpec extends AnyFlatSpec:
  "the README teleportation example" should "move the message amplitude to the third qubit" in {
    def entangle(q1: Int, q2: Int) = Circuit(H(q1), CNOT(q1, q2))

    val msg = Qubit(Complex(0.8), Complex(0.6), "message")
    val here = Qubit.zero("here")
    val there = Qubit.zero("there")

    val register = QubitRegister(msg, here, there)

    val circuit = entangle(1, 2)
      .combine(CNOT(0, 1), H(0))
      .combine(CNOT(1, 2), Controlled(0, Z(2)))
      .withRegister(register)

    given sim: QuantumSimulator = QuantumSimulator()

    assert(
      QubitProbabilityReader(register, sim.run(circuit))
        .read("there")
        .fold(false)(_.probabilityOfOne ~= msg.probabilityOfOne)
    )
  }

  "the README algorithm examples" should "do what the README says they do" in {
    Qft.gates((0 until 4).toIndexedSeq)
    Qft.inverseGates((0 until 4).toIndexedSeq)

    given sim: QuantumSimulator = QuantumSimulator()

    val circuit = Grover.circuit(qubitCount = 5, marked = 21)

    assert(Grover.iterations(5) == 4)

    // The README quotes the probability rather than a sample, and so does this: asserting one
    // measurement outcome would fail about one run in 1200 with no defect behind it.
    val state = sim.run(circuit) match
      case s: Superposition => s.state
      case other            => fail(s"expected a superposition, got $other")

    assert(state(2 * 21) * state(2 * 21) + state(2 * 21 + 1) * state(2 * 21 + 1) > 0.999)
    assert(MathUtils.toPaddedBinaryString(21, 5) == "10101")

    Shor.periodCircuit(a = 7, countingQubits = 4)

    assert(Shor.period(a = 7, y = 4, q = 16) == Some(4))
    assert(Shor.factor(a = 7, y = 4, q = 16) == Some((3, 5)))

    val random = new scala.util.Random(21)

    assert(Iterator.continually(SemiclassicalShor.quantumFactor(a = 2, n = 21, random)).take(8).contains(Some((3L, 7L))))
    assert(Iterator.continually(IterativeShor.quantumFactor(a = 2, n = 21, random)).take(8).contains(Some((3L, 7L))))

    val a = CompiledShor.base(p = 11, q = 13)

    assert(a == 12)
    assert(CompiledShor.factors(n = 143, a = a, measured = 1) == Some((BigInt(11), BigInt(13))))
    assert(
      scotty.quantum.Resources.of(CompiledShor.circuit).summary ==
        "2 qubits, 3 gates (1 on two or more qubits), depth 3, 0 arbitrary-angle rotations"
    )
  }

  "the README fusion example" should "report the form mix the README quotes" in {
    // The brickwork circuit the benchmark table is measured on, at the width the README prints the
    // mix for. If the pass stops producing that mix, the README is wrong and this fails.
    val qubitCount = 22
    val ops = (0 until 6).flatMap: layer =>
      (0 until qubitCount).map(H(_)) ++
        (0 until qubitCount).map(q => RZ(0.1 + 0.01 * q, q)) ++
        (layer % 2 until qubitCount - 1 by 2).map(q => CNOT(q, q + 1))

    val circuit = Circuit(Circuit.generateRegister(qubitCount), ops*)
    val fused = GateFusion.fuse(circuit, maxWidth = 6)

    assert(fused.forms == Map(BlockForm.Dense -> 26, BlockForm.Diagonal -> 18, BlockForm.Monomial -> 21))
    assert(fused.blocks.length == 65)
    assert(Math.abs(fused.cost.arithmeticIntensity - 6.49) < 0.01)

    val atFour = GateFusion.fuse(circuit, maxWidth = 4)

    assert(atFour.blocks.length == 98)
    assert(Math.abs(atFour.cost.arithmeticIntensity - 1.57) < 0.01)
  }
