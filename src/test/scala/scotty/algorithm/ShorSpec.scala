package scotty.algorithm

import org.scalatest.flatspec.AnyFlatSpec
import scotty.TestHelpers
import scotty.quantum.*

class ShorSpec extends AnyFlatSpec with TestHelpers {
  "The compiled multiplier" should "have the period the classical arithmetic says it does" in {
    assert(Shor.classicalPeriod(2) == 4)
    assert(Shor.classicalPeriod(4) == 2)
    assert(Shor.classicalPeriod(7) == 4)
    assert(Shor.classicalPeriod(8) == 4)
    assert(Shor.classicalPeriod(11) == 2)
    assert(Shor.classicalPeriod(13) == 4)
  }

  "The period-finding circuit" should "put all its weight on multiples of 2^c / r" in {
    // The only honest check: the peaks have to sit where the classically computed period says, for
    // every base the multiplier is compiled for.
    for a <- Shor.Bases.toSeq.sorted do
      val countingQubits = 4
      val q = 1 << countingQubits
      val r = Shor.classicalPeriod(a)
      val peaks = (0 until r).map(_ * q / r).toSet
      val marginal = countingMarginal(Shor.periodCircuit(a, countingQubits), countingQubits)
      val onPeaks = peaks.toSeq.map(marginal).sum

      withClue(s"a = $a, period $r, peaks $peaks: ") {
        assert(onPeaks > 0.99, f"only $onPeaks%.4f of the weight is on the peaks")
        peaks.foreach(y => assert(Math.abs(marginal(y) - 1.0 / r) < 0.01))
      }
  }

  it should "normalise" in {
    assert(Math.abs(countingMarginal(Shor.periodCircuit(7), 4).sum - 1d) < 1e-9)
  }

  "The continued-fraction step" should "recover the period from a measurement" in {
    assert(Shor.convergents(4, 16, 15).contains(4))
    assert(Shor.convergents(12, 16, 15).contains(4))
    assert(Shor.period(7, 4, 16) == Some(4))
    assert(Shor.period(7, 12, 16) == Some(4))
    assert(Shor.period(4, 8, 16) == Some(2))
  }

  it should "give nothing for a measurement of zero" in {
    assert(Shor.period(7, 0, 16).isEmpty)
  }

  "The classical tail" should "factor 15 into 3 and 5" in {
    assert(Shor.factor(7, 4, 16) == Some((3, 5)))
    assert(Shor.factor(7, 12, 16) == Some((3, 5)))
    assert(Shor.factor(2, 4, 16) == Some((3, 5)))
  }

  it should "report failure on the trivial square root" in {
    // a = 11 has period 2 and 11^1 = 11 = -4 (mod 15), which is not -1, so this one succeeds;
    // a = 4 has period 2 and 4^1 = 4, gcd(3, 15) = 3 and gcd(5, 15) = 5. Both are real
    // factorisations, so the failure mode to exercise is an odd period, which 15 does not offer.
    assert(Shor.factor(4, 8, 16) == Some((3, 5)))
    assert(Shor.factor(11, 8, 16) == Some((3, 5)))
  }

  "End to end" should "factor 15 from a simulated run" in {
    val a = 7
    val countingQubits = 4
    val q = 1 << countingQubits
    val marginal = countingMarginal(Shor.periodCircuit(a, countingQubits), countingQubits)
    // Every outcome the circuit actually produces with real weight, as a classical post-processor
    // would see them, and at least one has to factor.
    val outcomes = (0 until q).filter(marginal(_) > 0.01)
    val factorisations = outcomes.flatMap(Shor.factor(a, _, q)).distinct

    assert(outcomes.nonEmpty)
    assert(factorisations == Seq((3, 5)), s"outcomes $outcomes gave $factorisations")
  }

  /** The probability of each counting-register value, marginalised over the work register. The
    * counting qubits are the low bits of the amplitude index, so masking is enough.
    */
  def countingMarginal(circuit: Circuit, countingQubits: Int): IndexedSeq[Double] =
    val state = sim.run(circuit) match
      case s: Superposition => s.state
      case other            => fail(s"expected a superposition, got $other")

    val q = 1 << countingQubits
    val mask = q - 1
    val marginal = Array.fill(q)(0d)

    (0 until state.length / 2).foreach: i =>
      val re = state(2 * i)
      val im = state(2 * i + 1)

      marginal(i & mask) += re * re + im * im

    marginal.toIndexedSeq
}
