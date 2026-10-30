package scotty.algorithm

import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

/** Checks [[SemiclassicalShor]] against the periods and factors ordinary arithmetic gives.
  *
  * Seeded, so the sampled checks are reproducible. The thresholds come from the exact phase
  * estimation distribution, `P(y) = (1/r) sum_s |(1/q) sum_x exp(2 pi i x (s/r - y/q))|^2`.
  */
class SemiclassicalShorSpec extends AnyFlatSpec {
  "A run modulo 15" should "land exactly on a multiple of q / r, for every base" in {
    // Every period modulo 15 is a power of two, so every peak is an exact integer and the
    // distribution puts all its weight there: one shot off a peak is a wrong phase somewhere.
    val random = new Random(15)
    val q = 1 << 8

    for (a <- Seq(2L, 4L, 7L, 8L, 11L, 13L, 14L)) {
      val r = Shor.classicalPeriod(a.toInt)
      val peaks = (0 until r).map(_ * q / r).toSet
      val finder = SemiclassicalShor.PeriodFinder(a, 15)
      val seen = Seq.fill(12)(finder.sample(random))

      withClue(s"a = $a, r = $r, measured $seen: ") {
        assert(seen.forall(peaks.contains))
        assert(seen.toSet.size > 1, "every shot gave the same peak")
      }
    }
  }

  "A run modulo 21" should "land within one step of a peak as often as the exact distribution says" in {
    // r = 6 does not divide q = 1024, so the peaks are spread; the exact distribution puts 90.3% of
    // the weight within one step of a multiple of q / r. 40 seeded shots have to reach 75%.
    val random = new Random(21)
    val (a, n, r, q) = (2L, 21L, 6, 1 << 10)
    val finder = SemiclassicalShor.PeriodFinder(a, n)
    val shots = Seq.fill(40)(finder.sample(random))
    val near = shots.count(y => (0 to r).exists(s => Math.abs(y - s * q.toDouble / r) <= 1))

    withClue(s"measured $shots: ")(assert(near >= 30, s"only $near of 40 within one step of a peak"))
  }

  it should "recover the period 6 of 2 from some shot" in {
    val random = new Random(6)

    assert(Iterator.continually(SemiclassicalShor.period(2, 21, random)).take(20).contains(Some(6)))
  }

  "quantumFactor" should "factor 15, 21 and 35 through the period, with a base coprime to each" in {
    // Fixed coprime bases, so no gcd shortcut can do the work: 7 has period 4 mod 15, 2 has period
    // 6 mod 21 and 12 mod 35, and each period's half power gives the factors.
    for ((n, a, expected) <- Seq((15L, 7L, (3L, 5L)), (21L, 2L, (3L, 7L)), (35L, 2L, (5L, 7L)))) {
      val random = new Random(n)
      val found = Iterator.continually(SemiclassicalShor.quantumFactor(a, n, random)).take(8).collectFirst { case Some(f) => f }

      assert(found.contains(expected), s"$n did not factor with base $a in 8 runs")
    }
  }

  "factor" should "factor 21, by the period or by a base that shares a factor" in {
    assert(SemiclassicalShor.factor(21, new Random(1)).contains((3L, 7L)))
  }

  "measure" should "refuse an even modulus or a base sharing a factor with it" in {
    assertThrows[IllegalArgumentException](SemiclassicalShor.measure(3, 16, new Random(0)))
    assertThrows[IllegalArgumentException](SemiclassicalShor.measure(3, 21, new Random(0)))
  }
}
