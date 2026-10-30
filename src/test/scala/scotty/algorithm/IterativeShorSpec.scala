package scotty.algorithm

import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

/** Checks [[IterativeShor]] against the analytic phase-estimation distribution, against
  * [[SemiclassicalShor]], and against the factors ordinary arithmetic gives.
  *
  * The analytic distribution of the measured `y` over a `q`-point transform is
  * `P(y) = (1/q^2) sum_v |sum_(x < q, a^x = v mod N) exp(-2 pi i x y / q)|^2`, the `x` grouped by the
  * residue the work register is left in. It is computed here directly from that sum, with no state
  * vector, so it is independent of everything the algorithm does.
  */
class IterativeShorSpec extends AnyFlatSpec {
  private def analytic(a: Long, n: Long, t: Int): Map[Long, Double] = {
    val q = 1 << t
    val residues = Iterator.iterate(1L)(_ * a % n).take(q).toIndexedSeq
    val groups = (0 until q).groupBy(residues).values.toSeq

    (0 until q).map { y =>
      val p = groups.map { xs =>
        var (re, im) = (0d, 0d)
        xs.foreach { x =>
          val angle = -2 * Math.PI * ((x.toLong * y) % q) / q
          re += Math.cos(angle)
          im += Math.sin(angle)
        }
        re * re + im * im
      }.sum / (q.toDouble * q)

      y.toLong -> p
    }.toMap
  }

  "The exact distribution" should "equal the analytic phase-estimation distribution at every y" in {
    // Moduli whose periods do (15) and do not (21, 33, 35, 39) divide q, so the peaks are both
    // exact and spread; t = 2L is 8 to 12 counting bits.
    for ((a, n) <- Seq((7L, 15L), (2L, 21L), (5L, 33L), (2L, 35L), (2L, 39L))) {
      val finder = IterativeShor.PeriodFinder(a, n)
      val exact = finder.exactDistribution
      val expected = analytic(a, n, finder.t)

      withClue(s"a = $a, N = $n: ") {
        assert(Math.abs(exact.values.sum - 1) < 1e-12)
        expected.foreach { (y, p) => assert(Math.abs(exact.getOrElse(y, 0d) - p) < 1e-12, s"at y = $y") }
      }
    }
  }

  it should "put 90.3% of its weight within one step of a peak modulo 21, as SemiclassicalShorSpec says" in {
    val (a, n, r) = (2L, 21L, 6)
    val finder = IterativeShor.PeriodFinder(a, n)
    val q = (1L << finder.t).toDouble
    val near = finder.exactDistribution.collect {
      case (y, p) if (0 to r).exists(s => Math.abs(y - s * q / r) <= 1) => p
    }.sum

    assert(Math.abs(near - 0.903) < 0.0005, s"weight near a peak is $near")
  }

  "A run modulo 15" should "land exactly on a multiple of q / r, for every base" in {
    val random = new Random(15)

    for (a <- Seq(2L, 4L, 7L, 8L, 11L, 13L, 14L)) {
      val finder = IterativeShor.PeriodFinder(a, 15)
      val r = Shor.classicalPeriod(a.toInt)
      val peaks = (0 until r).map(_.toLong * (1L << finder.t) / r).toSet
      val seen = Seq.fill(12)(finder.sample(random))

      withClue(s"a = $a, measured $seen: ") {
        assert(seen.forall(peaks.contains))
        assert(seen.toSet.size > 1, "every shot gave the same peak")
      }
    }
  }

  "SemiclassicalShor" should "only ever measure a y this distribution gives real weight, and the same periods" in {
    // The same (a, N) through the two implementations: Beauregard's gates and this permutation.
    // Every shot of the gate-level one has to be a y the exact distribution here makes likely, and
    // both have to recover the classical period.
    for ((a, n) <- Seq((2L, 21L), (2L, 35L))) {
      val random = new Random(n)
      val exact = IterativeShor.PeriodFinder(a, n).exactDistribution
      val shots = Seq.fill(8)(SemiclassicalShor.measure(a, n, random).toLong)
      val r = Shor.classicalPeriod(a.toInt, n.toInt).toLong
      val t = 2 * IterativeShor.width(n)

      withClue(s"a = $a, N = $n, shots $shots: ") {
        assert(shots.forall(y => exact.getOrElse(y, 0d) > 1e-4))
        assert(shots.flatMap(IterativeShor.period(a, n, _, t)).toSet.subsetOf(Set(r)))
        assert(Iterator.continually(IterativeShor.period(a, n, IterativeShor.PeriodFinder(a, n).sample(random), t)).take(20).contains(Some(r)))
      }
    }
  }

  "quantumFactor" should "factor through the period, with a base coprime to each modulus" in {
    // Fixed coprime bases with even periods whose half power is not -1, so the factors come from
    // the period and no gcd shortcut can do the work.
    val cases = Seq(
      (15L, 7L, (3L, 5L)),
      (21L, 2L, (3L, 7L)),
      (33L, 5L, (3L, 11L)),
      (35L, 2L, (5L, 7L)),
      (39L, 2L, (3L, 13L)),
      (55L, 2L, (5L, 11L)),
      (77L, 2L, (7L, 11L)),
      (91L, 2L, (7L, 13L))
    )

    for ((n, a, expected) <- cases) {
      val random = new Random(n)
      val found = Iterator.continually(IterativeShor.quantumFactor(a, n, random)).take(8).collectFirst { case Some(f) => f }

      assert(found.contains(expected), s"$n did not factor with base $a in 8 runs")
    }
  }

  it should "factor a 20-bit semiprime on 21 qubits" in {
    // 1009 * 1013: 2^21 amplitudes and 40 counting bits, out of reach of the 2n + 3 circuit's 43
    // qubits. The base is the smallest one for which the period works classically, chosen here so
    // the test is deterministic; the run itself is not told the period.
    val n = 1009L * 1013L
    val base = Iterator.from(2).map(_.toLong).find { a =>
      BigInt(a).gcd(n) == 1 && {
        val r = Shor.classicalPeriod(a.toInt, n.toInt).toLong
        IterativeShor.factorFromPeriod(a, n, r).isDefined
      }
    }.get
    val random = new Random(n)
    val found = Iterator.continually(IterativeShor.quantumFactor(base, n, random)).take(6).collectFirst { case Some(f) => f }

    assert(found.contains((1009L, 1013L)), s"$n did not factor with base $base in 6 runs")
  }

  "factor" should "factor every odd semiprime below 100, by the period or a shared factor" in {
    val primes = Seq(3L, 5L, 7L, 11L, 13L, 17L, 19L, 23L, 29L, 31L)
    val semiprimes = for (p <- primes; q <- primes if p < q && p * q < 100) yield (p * q, (p, q))

    for ((n, expected) <- semiprimes)
      assert(IterativeShor.factor(n, new Random(n)).contains(expected), s"$n")
  }

  "PeriodFinder" should "refuse an even modulus or a base sharing a factor with it" in {
    assertThrows[IllegalArgumentException](IterativeShor.PeriodFinder(3, 16))
    assertThrows[IllegalArgumentException](IterativeShor.PeriodFinder(3, 21))
  }
}
