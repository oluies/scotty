package scotty.algorithm

import org.scalatest.flatspec.AnyFlatSpec
import scotty.TestHelpers
import scotty.quantum.*

class GroverSpec extends AnyFlatSpec with TestHelpers {
  "Grover's search" should "concentrate amplitude on the marked state" in {
    // The analytic success probability after k iterations is sin((2k+1) * asin(sqrt(M/N)))^2, which
    // is 1.0 at two qubits and never drops below 0.94 over this range.
    for qubitCount <- 2 to 6 do
      val marked = (1 << qubitCount) - 2
      val found = probability(Grover.circuit(qubitCount, marked), marked)

      withClue(s"$qubitCount qubits, marked $marked: ") {
        assert(found > 0.94, f"only $found%.4f of the amplitude landed on the marked state")
      }
  }

  it should "be exact on two qubits" in {
    (0 until 4).foreach: marked =>
      withClue(s"marked $marked: ") {
        assert(probability(Grover.circuit(2, marked), marked) > 0.999)
      }
  }

  it should "reach better than 0.99 by five qubits" in {
    assert(probability(Grover.circuit(5, 21), 21) > 0.99)
  }

  it should "find two marked states at once" in {
    val marked = Seq(3, 9)
    val circuit = Grover.circuit(4, marked, Grover.iterations(4, marked.length))
    val found = marked.map(probability(circuit, _)).sum

    assert(found > 0.94, f"only $found%.4f of the amplitude landed on the marked states")
  }

  it should "leave an unmarked state with almost nothing" in {
    val circuit = Grover.circuit(5, 21)

    (0 until 32).filter(_ != 21).foreach: state =>
      assert(probability(circuit, state) < 0.001)
  }

  "The iteration count" should "be floor(pi/4 * sqrt(N/M))" in {
    assert(Grover.iterations(2) == 1)
    assert(Grover.iterations(3) == 2)
    assert(Grover.iterations(4) == 3)
    assert(Grover.iterations(5) == 4)
    assert(Grover.iterations(6) == 6)
    assert(Grover.iterations(4, markedCount = 2) == 2)
  }

  "The oracle" should "flip only the marked amplitude's sign" in {
    val qubits = (0 until 3).toIndexedSeq
    val circuit = Circuit(
      Circuit.generateRegister(3),
      (qubits.map(scotty.quantum.gate.StandardGate.H(_)) ++ Grover.oracle(qubits, 5))*
    )
    val state = superposition(circuit)

    (0 until 8).foreach: i =>
      val expected = if i == 5 then -1 / Math.sqrt(8d) else 1 / Math.sqrt(8d)

      withClue(s"amplitude $i: ") {
        assert(Math.abs(state(2 * i) - expected) < 1e-9)
      }
  }

  def probability(circuit: Circuit, state: Int): Double =
    val amplitudes = superposition(circuit)
    val re = amplitudes(2 * state)
    val im = amplitudes(2 * state + 1)

    re * re + im * im

  def superposition(circuit: Circuit): Array[Double] =
    sim.run(circuit) match
      case s: Superposition => s.state
      case other            => fail(s"expected a superposition, got $other")
}
