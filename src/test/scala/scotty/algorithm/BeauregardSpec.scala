package scotty.algorithm

import org.scalatest.flatspec.AnyFlatSpec
import scotty.quantum.gate.Gate
import scotty.quantum.gate.StandardGate.X
import scotty.quantum.Circuit
import scotty.simulator.QuantumSimulator
import scotty.simulator.fusion.{FusedRunner, GateFusion}

/** Checks [[Beauregard]]'s arithmetic on classical inputs, against ordinary modular arithmetic.
  *
  * Each block is a permutation of basis states, so the check prepares a basis state, runs the gates
  * and requires a single basis state out, with probability above `1 - 1e-9`, at the index the
  * arithmetic predicts. Inputs are exhaustive over the registers' valid ranges. This is the check
  * Alice & Bob describe for their own compiled circuits (IACR ePrint 2026/2665, Appendix B): run
  * the logical circuit on classical inputs.
  */
class BeauregardSpec extends AnyFlatSpec {
  private val simulator = QuantumSimulator()

  /** The basis state `gates` sends `input` to, on `qubits` qubits, with its probability. */
  private def run(qubits: Int, input: Int, gates: Seq[Gate]): (Int, Double) = {
    val state = new Array[Double](2 << qubits)
    val iterator = simulator.parIndices(state.length / 2)

    state(2 * input) = 1d
    gates.foreach(simulator.applyGate(iterator, state, _))

    val probabilities = Array.tabulate(1 << qubits)(i => state(2 * i) * state(2 * i) + state(2 * i + 1) * state(2 * i + 1))
    val best = probabilities.indices.maxBy(probabilities)

    (best, probabilities(best))
  }

  /** The same through Scotty's gate fusion, fused once and applied to every input: the multiplier
    * is about 1350 gates at N = 21, and `GateFusionSpec` already holds fusion to the unfused result.
    */
  private def fusedRunner(qubits: Int, gates: Seq[Gate]): Int => (Int, Double) = {
    val fused = GateFusion.fuse(Circuit(Circuit.generateRegister(qubits), gates*))

    input => {
      val state = new Array[Double](2 << qubits)

      state(2 * input) = 1d
      FusedRunner.applyAll(state, fused, simulator)

      val probabilities = Array.tabulate(1 << qubits)(i => state(2 * i) * state(2 * i) + state(2 * i + 1) * state(2 * i + 1))
      val best = probabilities.indices.maxBy(probabilities)

      (best, probabilities(best))
    }
  }

  private def expectBasis(qubits: Int, input: Int, gates: Seq[Gate], expected: Int, clue: => String): Unit =
    expectResult(run(qubits, input, gates), expected, clue)

  private def expectResult(result: (Int, Double), expected: Int, clue: => String): Unit = {
    val (out, p) = result

    withClue(s"$clue: ") {
      assert(p > 1 - 1e-9, f"the output is not a basis state, best probability $p%.12f")
      assert(out == expected, s"got ${out.toBinaryString}, expected ${expected.toBinaryString}")
    }
  }

  private def register(start: Int, width: Int): IndexedSeq[Int] = (start until start + width).toIndexedSeq

  /** Places `value` on the qubits of `reg` in a basis index. */
  private def place(value: Long, reg: IndexedSeq[Int]): Int =
    reg.indices.map(i => (((value >> i) & 1L).toInt) << reg(i)).sum

  "phiAdd" should "add a constant modulo 2^width in Fourier space, for every b and a" in {
    val b = register(0, 4)

    for (a <- 0 until 16; v <- 0 until 16)
      expectBasis(4, v, Qft.gates(b) ++ Beauregard.phiAdd(a, b) ++ Qft.inverseGates(b), (a + v) % 16, s"$v + $a")
  }

  it should "be undone by its inverse" in {
    val b = register(0, 4)

    for (a <- 0 until 16; v <- 0 until 16)
      expectBasis(4, v, Beauregard.phiAdd(a, b) ++ Beauregard.inverse(Beauregard.phiAdd(a, b)), v, s"$v + $a - $a")
  }

  "phiAddMod" should "add modulo N when both controls are set, and do nothing otherwise" in {
    val n = 13L
    val c1 = 0
    val c2 = 1
    val b = register(2, Beauregard.width(n) + 1)
    val ancilla = 7
    val qubits = 8

    for (a <- 0L until n; v <- 0L until n; on1 <- Seq(false, true); on2 <- Seq(false, true)) {
      val controls = (if (on1) 1 << c1 else 0) | (if (on2) 1 << c2 else 0)
      val gates = Qft.gates(b) ++ Beauregard.phiAddMod(a, n, Seq(c1, c2), b, ancilla) ++ Qft.inverseGates(b)
      val sum = if (on1 && on2) (a + v) % n else v

      // The ancilla has to come back to |0> as well, or the adder leaks into it.
      expectBasis(qubits, controls | place(v, b), gates, controls | place(sum, b), s"$v + $a mod $n, controls $on1 $on2")
    }
  }

  "controlledMultiply" should "map x to a x mod N when the control is set, and leave b and the ancilla clear" in {
    for ((n, bases) <- Seq(15L -> Seq(2L, 4L, 7L, 8L, 11L, 13L, 14L), 21L -> Seq(2L, 5L, 11L, 20L))) {
      val layout = SemiclassicalShor.Layout(Beauregard.width(n))

      for (a <- bases) {
        val multiply = fusedRunner(layout.qubits, Beauregard.controlledMultiply(a, n, layout.control, layout.x, layout.b, layout.ancilla))

        for (x <- 0L until n; on <- Seq(false, true)) {
          val control = if (on) 1 << layout.control else 0
          val product = if (on) (a * x) % n else x

          expectResult(multiply(control | place(x, layout.x)), control | place(product, layout.x), s"$a * $x mod $n, control $on")
        }
      }
    }
  }

  "modInverse" should "invert every unit modulo N" in {
    for (n <- Seq(15L, 21L, 35L); a <- 1L until n if BigInt(a).gcd(BigInt(n)) == 1)
      assert(a * Beauregard.modInverse(a, n) % n == 1)
  }

  "inverse" should "reject a gate it has no inverse for" in {
    assertThrows[IllegalArgumentException](Beauregard.inverse(Seq(scotty.quantum.gate.StandardGate.T(0))))
  }
}
