package scotty.quantum

import org.scalatest.flatspec.AnyFlatSpec
import scotty.algorithm.{Beauregard, CompiledShor, Shor}
import scotty.quantum.gate.{Controlled, Gate}
import scotty.quantum.gate.StandardGate.{CNOT, CPHASE, H, PHASE, X}

/** Checks [[Resources]] on small hand-counted cases, then pins what it says about Scotty's own
  * Shor circuits, which is what the README's resource table quotes.
  */
class ResourcesSpec extends AnyFlatSpec {
  "Resources" should "count gates, depth, arity and names on a hand-counted sequence" in {
    // Two parallel Hadamards share a layer; the CNOT waits for both; the X on qubit 2 does not.
    val r = Resources.of(Seq(H(0), H(1), X(2), CNOT(0, 1), Controlled(2, CNOT(0, 1))))

    assert(r.qubits == 3)
    assert(r.gates == 5)
    assert(r.depth == 3)
    assert(r.byArity == Map(1 -> 3, 2 -> 1, 3 -> 1))
    assert(r.byName == Map("H" -> 2, "X" -> 1, "CNOT" -> 1, "CCNOT" -> 1))
    assert(r.multiQubit == 2)
  }

  it should "count an angle as a rotation only when it is not a multiple of pi / 4" in {
    assert(!Resources.isArbitraryRotation(PHASE(Math.PI / 4, 0)))
    assert(!Resources.isArbitraryRotation(PHASE(-3 * Math.PI / 2, 0)))
    assert(Resources.isArbitraryRotation(PHASE(Math.PI / 8, 0)))
    assert(Resources.isArbitraryRotation(CPHASE(0.3, 0, 1)))
    assert(Resources.isArbitraryRotation(Controlled(2, PHASE(0.3, 0))))
    assert(!Resources.isArbitraryRotation(H(0)))
  }

  it should "take the qubit count from a circuit's register" in {
    assert(Resources.of(Circuit(Circuit.generateRegister(5), H(0))).qubits == 5)
  }

  // The circuits counted below, laid out as SemiclassicalShor lays them out: control at 0, x at
  // 1 .. n, b at n + 1 .. 2n + 1, the ancilla at 2n + 2.
  private def multiply(a: Long, n: Long): Seq[Gate] = {
    val bits = Beauregard.width(n)
    Beauregard.controlledMultiply(a, n, 0, 1 to bits, bits + 1 to 2 * bits + 1, 2 * bits + 2)
  }

  // A whole run of SemiclassicalShor: per counting bit, a Hadamard, the multiplication by that
  // bit's power, the measurement-dependent phase correction and a Hadamard. The correction's angle
  // depends on bits measured at run time, so it is charged, conservatively, as one arbitrary
  // rotation per bit.
  private def run(a: Long, n: Long): Seq[Gate] = {
    val m = 2 * Beauregard.width(n)
    (0 until m).flatMap { k =>
      val power = BigInt(a).modPow(BigInt(2).pow(m - 1 - k), BigInt(n)).toLong
      Seq(H(0)) ++ multiply(power, n) ++ Seq(PHASE(0.1, 0), H(0))
    }
  }

  "The compiled circuits" should "be as small as the README says" in {
    assert(Resources.of(CompiledShor.circuit) == Resources(2, 3, 3, Map(1 -> 2, 2 -> 1), Map("H" -> 2, "CNOT" -> 1), 0))

    val shor15 = Resources.of(Shor.periodCircuit(7))
    assert((shor15.qubits, shor15.gates, shor15.multiQubit, shor15.depth, shor15.rotations) == (8, 122, 113, 108, 1))
  }

  "Beauregard's arithmetic" should "cost what the README's table says, from 15 to 899" in {
    // (N, base, multiply: qubits, gates, depth, rotations, run: gates, depth, rotations)
    val table = Seq(
      (15L, 7L, (11, 842, 549, 182), (6550, 4077, 1296)),
      (21L, 2L, (13, 1353, 743, 369), (13560, 7466, 3727)),
      (35L, 2L, (15, 2008, 1005, 685), (24537, 12701, 8637)),
      (143L, 2L, (19, 3968, 1649, 1755), (64745, 28041, 29275)),
      (899L, 2L, (23, 6908, 2473, 3581), (141424, 53879, 74802))
    )

    for ((n, a, (q, g, d, rot), (runGates, runDepth, runRot)) <- table) {
      val m = Resources.of(multiply(a, n))
      val r = Resources.of(run(a, n))

      withClue(s"N = $n: ") {
        assert((m.qubits, m.gates, m.depth, m.rotations) == (q, g, d, rot))
        assert((r.gates, r.depth, r.rotations) == (runGates, runDepth, runRot))
      }
    }
  }

  it should "grow as about n^2 per multiplication and n^3 per run, with rotations faster still" in {
    // Slopes on a log-log scale between 5 and 10 bits (21 and 899). Beauregard's multiplication is
    // n modular adders of O(n) rotations each between O(n^2) transforms, so O(n^2) gates, and a run
    // is 2n of them. The exact transforms keep every small angle, which is why the rotation count
    // outgrows the gate count; an approximate transform that drops them would not.
    def slope(f: Resources => Int, of: (Long, Long) => Seq[Gate]): Double =
      Math.log(f(Resources.of(of(2, 899))).toDouble / f(Resources.of(of(2, 21)))) / Math.log(10d / 5)

    val multiplyGates = slope(_.gates, multiply)
    val runGates = slope(_.gates, run)
    val multiplyRotations = slope(_.rotations, multiply)

    assert(multiplyGates > 1.8 && multiplyGates < 2.6, s"multiply gates grow as n^$multiplyGates")
    assert(runGates > 2.8 && runGates < 3.6, s"run gates grow as n^$runGates")
    assert(multiplyRotations > multiplyGates, s"rotations grow as n^$multiplyRotations")
  }
}
