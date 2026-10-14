package scotty.simulator

import org.scalatest.flatspec.AnyFlatSpec
import scotty.TestHelpers
import scotty.quantum.gate.StandardGate.*
import scotty.quantum.*
import scotty.quantum.math.Complex

class StandardGatesSpec extends AnyFlatSpec with TestHelpers {
  "CNOT" should "change target qubit when control is 1" in {
    assert(sim.runAndMeasure(Circuit(X(0), CNOT(0, 1))).toBinary == "11")
  }

  it should "not change target qubit when control is 0" in {
    assert(sim.runAndMeasure(Circuit(CNOT(0, 1))).toBinary == "00")
  }

  it should "throw IllegalArgumentException if indices are not unique" in {
    assertThrows[IllegalArgumentException] {
      sim.runAndMeasure(Circuit(CNOT(0, 0)))
    }
  }

  "CCNOT" should "change target qubit when both controls are 1" in {
    assert(sim.runAndMeasure(Circuit(X(0), X(2), CCNOT(0, 2, 4))).toBinary == "10101")
  }

  it should "not change target qubit when one of the controls is 0" in {
    assert(sim.runAndMeasure(Circuit(X(2), CCNOT(0, 2, 4))).toBinary == "00100")
  }

  it should "throw IllegalArgumentException if indices are not unique" in {
    assertThrows[IllegalArgumentException] {
      sim.runAndMeasure(Circuit(CCNOT(0, 1, 0)))
    }
  }

  "H" should "set superposition to 50/50" in {
    assert(QubitProbabilityReader(sim.run(Circuit(H(0)))).read(0).probabilityOfOne === 0.5d)
  }

  "I" should "do nothing" in {
    val r = StateProbabilityReader(sim.run(Circuit(I(0)))).read(0)

    assert(r.state == "0")
    assert(r.amplitude === Complex(1))
    assert(r.probability === 1d)
  }

  "X" should "negate qubit" in {
    val r = StateProbabilityReader(sim.run(Circuit(X(0)))).read(0)

    assert(r.state == "1")
    assert(r.amplitude === Complex(1))
    assert(r.probability === 1d)
  }

  "Y" should "negate qubit" in {
    val r = StateProbabilityReader(sim.run(Circuit(Y(0)))).read(0)

    assert(r.state == "1")
    assert(r.amplitude === Complex(0, 1))
    assert(r.probability === 1d)
  }

  "Z" should "change phase" in {
    val r = StateProbabilityReader(sim.run(Circuit(X(0), Z(0)))).read(0)

    assert(r.state == "1")
    assert(r.amplitude === Complex(-1))
    assert(r.probability === 1d)
  }

  "CZ" should "change phase if control is 1" in {
    val r = StateProbabilityReader(sim.run(Circuit(X(1), X(0), CZ(1, 0)))).read(0)

    assert(r.state == "11")
    assert(r.amplitude === Complex(-1))
    assert(r.probability === 1d)
  }

  it should "not change phase if control is 0" in {
    val r = StateProbabilityReader(sim.run(Circuit(X(0), CZ(1, 0)))).read(0)

    assert(r.state == "01")
    assert(r.amplitude === Complex(1))
    assert(r.probability === 1d)
  }

  "S" should "change phase" in {
    val r = StateProbabilityReader(sim.run(Circuit(S(0)).withRegister("1"))).read(0)

    assert(r.state == "1")
    assert(r.amplitude === Complex(0, 1))
    assert(r.probability === 1d)
  }

  "T" should "change phase" in {
    val r = StateProbabilityReader(sim.run(Circuit(T(0)).withRegister("1"))).read(0)

    assert(r.state == "1")
    assert(r.amplitude === Complex(fiftyPercent, fiftyPercent))
    assert(r.probability === 1d)
  }

  "R" should "change phase" in {
    val r = StateProbabilityReader(sim.run(Circuit(PHASE(Math.PI / 4, 0)).withRegister("1"))).read(0)

    assert(r.state == "1")
    assert(r.amplitude === Complex(fiftyPercent, fiftyPercent))
    assert(r.probability === 1d)
  }

  "RX" should "rotate qubit around X" in {
    val r1 = StateProbabilityReader(sim.run(Circuit(RX(quarterTurn, 0)))).read(0)
    val r2 = StateProbabilityReader(sim.run(Circuit(RX(quarterTurn, 0)))).read(1)

    assert(r1.state == "0")
    assert(r1.amplitude === Complex(fiftyPercent, 0))
    assert(r1.probability === 0.5d)

    assert(r2.state == "1")
    assert(r2.amplitude === Complex(0, -fiftyPercent))
    assert(r2.probability === 0.5d)
  }

  "RY" should "rotate qubit around Y" in {
    val r1 = StateProbabilityReader(sim.run(Circuit(RY(quarterTurn, 0)))).read(0)
    val r2 = StateProbabilityReader(sim.run(Circuit(RY(quarterTurn, 0)))).read(1)

    assert(r1.state == "0")
    assert(r1.amplitude === Complex(fiftyPercent, 0))
    assert(r1.probability === 0.5d)

    assert(r2.state == "1")
    assert(r2.amplitude === Complex(fiftyPercent, 0))
    assert(r2.probability === 0.5d)
  }

  "RZ" should "rotate qubit around Z" in {
    val r = StateProbabilityReader(sim.run(Circuit(RZ(quarterTurn, 0)))).read(0)

    assert(r.state == "0")
    assert(r.amplitude === Complex(fiftyPercent, -fiftyPercent))
    assert(r.probability === 1d)
  }
}
