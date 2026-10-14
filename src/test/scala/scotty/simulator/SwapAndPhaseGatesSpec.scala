package scotty.simulator

import org.scalatest.flatspec.AnyFlatSpec
import scotty.TestHelpers
import scotty.quantum.gate.StandardGate.*
import scotty.quantum.*
import scotty.quantum.math.Complex

class SwapAndPhaseGatesSpec extends AnyFlatSpec with TestHelpers {
  "SWAP" should "swap two nearby qubits 1 and 0" in {
    assert(sim.runAndMeasure(Circuit(X(0), SWAP(0, 1))).toBinary == "10")
  }

  it should "swap two nearby qubits 0 and 1" in {
    assert(sim.runAndMeasure(Circuit(X(1), SWAP(0, 1))).toBinary == "01")
  }

  it should "swap two qubits with a gap of 1 qubit" in {
    assert(sim.runAndMeasure(Circuit(X(0), SWAP(0, 2))).toBinary == "100")
  }

  it should "swap two qubits with a gap of 2 qubits" in {
    assert(sim.runAndMeasure(Circuit(X(0), SWAP(0, 3))).toBinary == "1000")
  }

  it should "swap two qubits in reverse with a gap of 2 qubits" in {
    assert(sim.runAndMeasure(Circuit(X(3), X(2), SWAP(3, 0))).toBinary == "0101")
  }

  it should "have correct amplitudes" in {
    val r1 = StateProbabilityReader(sim.run(Circuit(H(0), SWAP(0, 1)))).read(0)
    val r2 = StateProbabilityReader(sim.run(Circuit(H(0), SWAP(0, 1)))).read(1)

    assert(r1.state == "00")
    assert(r1.amplitude === Complex(fiftyPercent, 0))
    assert(r1.probability === 0.5d)

    assert(r2.state == "10")
    assert(r2.amplitude === Complex(fiftyPercent, 0))
    assert(r2.probability === 0.5d)
  }

  it should "throw IllegalArgumentException if indices are not unique" in {
    assertThrows[IllegalArgumentException] {
      sim.runAndMeasure(Circuit(SWAP(3, 3)))
    }
  }

  "CSWAP" should "swap two qubits when control is 1" in {
    assert(sim.runAndMeasure(Circuit(X(1), X(2), CSWAP(1, 0, 2))).toBinary == "011")
  }

  it should "not swap two qubits when control is 0" in {
    assert(sim.runAndMeasure(Circuit(X(2), CSWAP(1, 0, 2))).toBinary == "100")
  }

  "ISWAP" should "swap qubits and apply an i phase" in {
    val r1 = StateProbabilityReader(sim.run(Circuit(H(0), ISWAP(0, 1)))).read(0)
    val r2 = StateProbabilityReader(sim.run(Circuit(H(0), ISWAP(0, 1)))).read(1)

    assert(r1.state == "00")
    assert(r1.amplitude === Complex(0.7, 0))
    assert(r1.probability === 0.5d)

    assert(r2.state == "10")
    assert(r2.amplitude === Complex(0, 0.7))
    assert(r2.probability === 0.5d)
  }

  "PSWAP" should "swap qubits and apply a phi phase" in {
    val r1 = StateProbabilityReader(sim.run(Circuit(H(0), PSWAP(Math.PI / 3, 0, 1)))).read(0)
    val r2 = StateProbabilityReader(sim.run(Circuit(H(0), PSWAP(Math.PI / 3, 0, 1)))).read(1)

    assert(r1.state == "00")
    assert(r1.amplitude === Complex(0.7, 0))
    assert(r1.probability === 0.5d)

    assert(r2.state == "10")
    assert(r2.amplitude === Complex(0.35, 0.61))
    assert(r2.probability === 0.5d)
  }

  "PHASE" should "apply phase phi to state |1>" in {
    val r1 = StateProbabilityReader(sim.run(Circuit(H(0), PHASE(Math.PI / 4, 0)))).read(0)
    val r2 = StateProbabilityReader(sim.run(Circuit(H(0), PHASE(Math.PI / 4, 0)))).read(1)

    assert(r1.state == "0")
    assert(r1.amplitude === Complex(0.7))
    assert(r1.probability === 0.5d)

    assert(r2.state == "1")
    assert(r2.amplitude === Complex(0.5, 0.5))
    assert(r2.probability === 0.5d)
  }

  "PHASE0" should "apply phase phi to state |0>" in {
    val r1 = StateProbabilityReader(sim.run(Circuit(H(0), PHASE0(Math.PI / 4, 0)))).read(0)
    val r2 = StateProbabilityReader(sim.run(Circuit(H(0), PHASE0(Math.PI / 4, 0)))).read(1)

    assert(r1.state == "0")
    assert(r1.amplitude === Complex(0.5, 0.5))
    assert(r1.probability === 0.5d)

    assert(r2.state == "1")
    assert(r2.amplitude === Complex(0.7))
    assert(r2.probability === 0.5d)
  }

  "CPHASE" should "apply phase phi to |1> when control qubit is 1" in {
    val r1 = StateProbabilityReader(sim.run(Circuit(X(1), H(0), CPHASE(Math.PI / 4, 1, 0)))).read(0)
    val r2 = StateProbabilityReader(sim.run(Circuit(X(1), H(0), CPHASE(Math.PI / 4, 1, 0)))).read(1)

    assert(r1.state == "10")
    assert(r1.amplitude === Complex(0.7))
    assert(r1.probability === 0.5d)

    assert(r2.state == "11")
    assert(r2.amplitude === Complex(0.5, 0.5))
    assert(r2.probability === 0.5d)
  }

  it should "not apply phase phi to state when control qubit is 0" in {
    val r1 = StateProbabilityReader(sim.run(Circuit(H(0), CPHASE(Math.PI / 4, 1, 0)))).read(0)
    val r2 = StateProbabilityReader(sim.run(Circuit(H(0), CPHASE(Math.PI / 4, 1, 0)))).read(1)

    assert(r1.state == "00")
    assert(r1.amplitude === Complex(0.7))
    assert(r1.probability === 0.5d)

    assert(r2.state == "01")
    assert(r2.amplitude === Complex(0.7))
    assert(r2.probability === 0.5d)
  }

  "CPHASE10" should "apply phase phi to |0> when control qubit is 1" in {
    val r1 = StateProbabilityReader(sim.run(Circuit(X(1), H(0), CPHASE10(Math.PI / 4, 1, 0)))).read(0)
    val r2 = StateProbabilityReader(sim.run(Circuit(X(1), H(0), CPHASE10(Math.PI / 4, 1, 0)))).read(1)

    assert(r1.state == "10")
    assert(r1.amplitude === Complex(0.5, 0.5))
    assert(r1.probability === 0.5d)

    assert(r2.state == "11")
    assert(r2.amplitude === Complex(0.7))
    assert(r2.probability === 0.5d)
  }

  it should "not apply phase phi to |0> when control qubit is 0" in {
    val r1 = StateProbabilityReader(sim.run(Circuit(H(0), CPHASE10(Math.PI / 4, 1, 0)))).read(0)
    val r2 = StateProbabilityReader(sim.run(Circuit(H(0), CPHASE10(Math.PI / 4, 1, 0)))).read(1)

    assert(r1.state == "00")
    assert(r1.amplitude === Complex(0.7))
    assert(r1.probability === 0.5d)

    assert(r2.state == "01")
    assert(r2.amplitude === Complex(0.7))
    assert(r2.probability === 0.5d)
  }

  "CPHASE00" should "apply phase phi to |0> when control qubit is 0" in {
    val r1 = StateProbabilityReader(sim.run(Circuit(H(0), CPHASE00(Math.PI / 4, 1, 0)))).read(0)
    val r2 = StateProbabilityReader(sim.run(Circuit(H(0), CPHASE00(Math.PI / 4, 1, 0)))).read(1)

    assert(r1.state == "00")
    assert(r1.amplitude === Complex(0.5, 0.5))
    assert(r1.probability === 0.5d)

    assert(r2.state == "01")
    assert(r2.amplitude === Complex(0.7))
    assert(r2.probability === 0.5d)
  }

  it should "not apply phase phi to |0> when control qubit is 1" in {
    val r1 = StateProbabilityReader(sim.run(Circuit(X(1), H(0), CPHASE00(Math.PI / 4, 1, 0)))).read(0)
    val r2 = StateProbabilityReader(sim.run(Circuit(X(1), H(0), CPHASE00(Math.PI / 4, 1, 0)))).read(1)

    assert(r1.state == "10")
    assert(r1.amplitude === Complex(0.7))
    assert(r1.probability === 0.5d)

    assert(r2.state == "11")
    assert(r2.amplitude === Complex(0.7))
    assert(r2.probability === 0.5d)
  }

  "CPHASE01" should "apply phase phi to |0> when control qubit is 0" in {
    val r1 = StateProbabilityReader(sim.run(Circuit(H(0), CPHASE01(Math.PI / 4, 1, 0)))).read(0)
    val r2 = StateProbabilityReader(sim.run(Circuit(H(0), CPHASE01(Math.PI / 4, 1, 0)))).read(1)

    assert(r1.state == "00")
    assert(r1.amplitude === Complex(0.7))
    assert(r1.probability === 0.5d)

    assert(r2.state == "01")
    assert(r2.amplitude === Complex(0.5, 0.5))
    assert(r2.probability === 0.5d)
  }

  it should "not apply phase phi to |0> when control qubit is 1" in {
    val r1 = StateProbabilityReader(sim.run(Circuit(X(1), H(0), CPHASE01(Math.PI / 4, 1, 0)))).read(0)
    val r2 = StateProbabilityReader(sim.run(Circuit(X(1), H(0), CPHASE01(Math.PI / 4, 1, 0)))).read(1)

    assert(r1.state == "10")
    assert(r1.amplitude === Complex(0.7))
    assert(r1.probability === 0.5d)

    assert(r2.state == "11")
    assert(r2.amplitude === Complex(0.7))
    assert(r2.probability === 0.5d)
  }
}
