package scotty

import org.scalatest.flatspec.AnyFlatSpec
import scotty.quantum.gate.StandardGate.{CNOT, H, Z}
import scotty.quantum.gate.Controlled
import scotty.quantum.math.Complex
import scotty.quantum.math.MathUtils.~=
import scotty.quantum.{Circuit, Qubit, QubitProbabilityReader, QubitRegister}
import scotty.simulator.QuantumSimulator

/** The teleportation example from README.md, kept as a test so the documented public API cannot
  * drift out of compiling.
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
