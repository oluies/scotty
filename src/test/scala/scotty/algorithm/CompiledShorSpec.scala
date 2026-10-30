package scotty.algorithm

import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random
import scotty.quantum.Superposition
import scotty.quantum.density.DensityMatrix
import scotty.simulator.QuantumSimulator

/** Checks [[CompiledShor]] against Smolin, Smith and Vargo: two qubits "factor" any semiprime, the
  * base that makes it work is built from the factors, and the computation is a fair coin.
  */
class CompiledShorSpec extends AnyFlatSpec {
  // A 1024-bit semiprime from two generated 512-bit primes, rather than RSA-768 typed in: the paper's
  // point needs a number nobody factors classically, not that particular one.
  private val random = new Random(2013)
  private val (bigP, bigQ) = (BigInt.probablePrime(512, random), BigInt.probablePrime(512, random))

  private def probabilityCountingReadsOne(sp: Superposition): Double =
    sp.state.indices.by(2).collect { case i if (i / 2) % 2 == 1 => sp.state(i) * sp.state(i) + sp.state(i + 1) * sp.state(i + 1) }.sum

  "The compiled base" should "be one of Eq. (4)'s four square roots of 1, with period 2" in {
    for ((p, q) <- Seq((BigInt(3), BigInt(5)), (BigInt(3), BigInt(7)), (BigInt(11), BigInt(13)), (bigP, bigQ))) {
      val n = p * q
      val roots = CompiledShor.squareRootsOfOne(p, q)
      val a = CompiledShor.base(p, q)

      withClue(s"N = $n: ") {
        assert(roots.size == 4)
        assert(roots.forall(_.modPow(2, n) == 1))
        assert(roots.contains(BigInt(1)) && roots.contains(n - 1))
        assert(a != 1 && a != n - 1 && a.modPow(2, n) == 1)
      }
    }
  }

  it should "be the factors in disguise: gcd(a - 1, N) is one of them, with no quantum step at all" in {
    val n = bigP * bigQ

    assert(Set((CompiledShor.base(bigP, bigQ) - 1).gcd(n)).subsetOf(Set(bigP, bigQ)))
  }

  "The compiled circuit" should "read 1 with probability exactly one half, for every N alike" in {
    // The circuit takes no N at all, so there is one distribution to check, not one per modulus.
    val sp = QuantumSimulator(new Random(0)).run(CompiledShor.circuit).asInstanceOf[Superposition]

    assert(Math.abs(probabilityCountingReadsOne(sp) - 0.5) < 1e-12)
  }

  it should "leave both qubits maximally mixed, so the Hadamard does nothing and a coin would do" in {
    // Section 3: after the CNOT the counting qubit's reduced state is I/2, which every unitary,
    // the Hadamard included, leaves alone. Both qubits are checked, so the result does not depend
    // on which subsystem the density-matrix layer lists first.
    val sp = QuantumSimulator(new Random(0)).run(CompiledShor.circuit).asInstanceOf[Superposition]
    val rho = DensityMatrix.fromSuperposition(sp)
    val half = DensityMatrix.maximallyMixed(Vector(2))

    assert(rho.partialTrace(Seq(0)).approximatelyEquals(half, 1e-12))
    assert(rho.partialTrace(Seq(1)).approximatelyEquals(half, 1e-12))
  }

  "Measuring it" should "factor 15, 21, 143 and a 1024-bit semiprime with the same two qubits" in {
    val simulator = QuantumSimulator(new Random(1))
    val cases = Seq((BigInt(3), BigInt(5)), (BigInt(3), BigInt(7)), (BigInt(11), BigInt(13)), (bigP, bigQ))

    for ((p, q) <- cases) {
      val (n, a) = (p * q, CompiledShor.base(p, q))
      val shots = Seq.fill(20)(simulator.runAndMeasure(CompiledShor.circuit).index & 1)

      withClue(s"N = $n, shots $shots: ") {
        assert(shots.contains(1) && shots.contains(0), "twenty shots of a fair coin all came out the same")
        assert(shots.filter(_ == 1).forall(y => CompiledShor.factors(n, a, y).contains((p.min(q), p.max(q)))))
        assert(shots.filter(_ == 0).forall(y => CompiledShor.factors(n, a, y).isEmpty))
      }
    }
  }

  it should "work just as well with a coin toss in place of the circuit, as the paper did" in {
    val (n, a) = (bigP * bigQ, CompiledShor.base(bigP, bigQ))
    val coin = new Random(1968)

    assert(Iterator.continually(coin.nextInt(2)).take(20).flatMap(CompiledShor.factors(n, a, _)).contains((bigP.min(bigQ), bigP.max(bigQ))))
  }

  "factors" should "say nothing for a base whose square is not 1" in {
    assert(CompiledShor.factors(15, 7, 1).isEmpty) // 7 has period 4 mod 15, not 2
  }

  "squareRootsOfOne" should "refuse equal or even primes" in {
    assertThrows[IllegalArgumentException](CompiledShor.squareRootsOfOne(5, 5))
    assertThrows[IllegalArgumentException](CompiledShor.squareRootsOfOne(2, 5))
  }
}
