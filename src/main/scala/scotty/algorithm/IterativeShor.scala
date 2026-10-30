package scotty.algorithm

import scala.util.Random

/** Shor's period finding for any odd modulus on `L + 1` qubits, with the modular multiplication
  * applied as a permutation of basis states.
  *
  * This is the "iterative Shor algorithm" of D. Willsch, M. Willsch, F. Jin, H. De Raedt and
  * K. Michielsen, *Large-Scale Simulation of Shor's Quantum Factoring Algorithm*, Mathematics 11,
  * 4222 (2023), arXiv:2308.05047, Section II and Fig. 2, which they ran on a GPU supercomputer to
  * factor 549 755 813 701 on 40 qubits. Like [[SemiclassicalShor]] it reuses one counting qubit,
  * measured once per exponent bit (Griffiths and Niu); unlike it, the work register is just the
  * `L` bits of `y` and nothing else. The controlled multiplication is their Eq. (6),
  * `|1>|y> -> |1>|a y mod N>` for `y < N`, every other basis state unchanged: a permutation, since
  * `a` is invertible modulo `N`, and so a unitary that needs no adder, no `b` register and no
  * ancilla. Its constants `a^(2^k) mod N` are computed by repeated squaring, which needs no
  * knowledge of the period, so the run still assumes nothing about the answer.
  *
  * What that buys is size: `2^(L + 1)` amplitudes against [[SemiclassicalShor]]'s `2^(2L + 3)`, and
  * a multiplication that costs one pass over the state rather than about 1350 gates at 21. What it
  * gives up is the circuit: the oracle is the arithmetic's effect, not a decomposition into gates,
  * which is exactly the trade Willsch et al. make and the reason the two are kept side by side.
  *
  * Qubit layout: `0` counts, `1 .. L` hold `y`, so basis index `i` is `(y << 1) | counting`. The
  * bits come out least significant first; step `k` multiplies by `a^(2^(t - 1 - k))` and rotates by
  * `-pi j / 2^k`, with `j` the bits measured so far. Willsch et al. write the opposite sign, Eq. (9);
  * the distribution is symmetric under `y -> 2^t - y`, and this sign is [[SemiclassicalShor]]'s, so
  * the two are directly comparable. Pure array code, with nothing from the simulator, so it compiles
  * for JavaScript as well.
  */
object IterativeShor:
  /** Bits needed to hold every residue below `n`. */
  def width(n: Long): Int = 64 - java.lang.Long.numberOfLeadingZeros(n - 1)

  /** Period finding for one base and modulus.
    *
    * `countingBits` defaults to `2L`, the precision at which the continued fractions recover any
    * period below `n`.
    */
  final class PeriodFinder(val a: Long, val n: Long, countingBits: Int = -1):
    require(n % 2 == 1 && n > 2, s"$n has to be odd and above 2")
    require(BigInt(a).gcd(BigInt(n)) == 1, s"$a and $n are not coprime")

    val bits: Int = width(n)
    val t: Int = if countingBits > 0 then countingBits else 2 * bits

    require(bits + 1 <= 28, s"$n needs ${bits + 1} qubits, more than a state vector here should hold")
    require(t <= 62, s"$t counting bits do not fit a Long")

    /** `a^(2^(t - 1 - k)) mod n` for step `k`, by repeated squaring. */
    val powers: IndexedSeq[Long] =
      val squares = Iterator.iterate(BigInt(a) % n)(x => x * x % n).take(t).map(_.toLong).toIndexedSeq

      squares.reverse

    private def amplitudes: Int = 1 << (bits + 1)

    /** `|+>` on the counting qubit and `y = 1`, the multiplicative identity. */
    def initial: Array[Double] =
      val state = new Array[Double](2 * amplitudes)

      state(2 * (1 << 1)) = 1d
      state

    /** Eq. (6): the counting qubit set and `y < n` sends `y` to `a y mod n`. */
    def multiply(state: Array[Double], scratch: Array[Double], power: Long): Unit =
      System.arraycopy(state, 0, scratch, 0, state.length)

      // power and y are both below n < 2^27, so their product fits a Long.
      var y = 0L
      while y < n do
        val from = 2 * ((y.toInt << 1) | 1)
        val to = 2 * (((power * y % n).toInt << 1) | 1)

        state(to) = scratch(from)
        state(to + 1) = scratch(from + 1)
        y += 1

    /** One step after the multiplication: the phase correction, the Hadamard, the measurement. */
    def phaseAndHadamard(state: Array[Double], phase: Double): Unit =
      val (cos, sin) = (Math.cos(phase), Math.sin(phase))
      val half = 1 / Math.sqrt(2)
      var i = 0

      while i < amplitudes do
        val (r0, i0) = (state(2 * i), state(2 * i + 1))
        val (r1, i1) = (state(2 * i + 2) * cos - state(2 * i + 3) * sin, state(2 * i + 2) * sin + state(2 * i + 3) * cos)

        state(2 * i) = (r0 + r1) * half
        state(2 * i + 1) = (i0 + i1) * half
        state(2 * i + 2) = (r0 - r1) * half
        state(2 * i + 3) = (i0 - i1) * half
        i += 2

    /** The probability that the counting qubit reads 1. */
    def probabilityOfOne(state: Array[Double]): Double =
      var one = 0d
      var i = 1

      while i < amplitudes do
        one += state(2 * i) * state(2 * i) + state(2 * i + 1) * state(2 * i + 1)
        i += 2

      one

    /** Projects the counting qubit onto `bit`, renormalises, and resets it to `|+>` for the next
      * step: Eqs. (14) and (15), which move the kept half into both halves scaled by `1/sqrt(2 p)`.
      */
    def collapseAndReset(state: Array[Double], bit: Int, probability: Double): Unit =
      val scale = 1 / Math.sqrt(2 * probability)
      var i = 0

      while i < amplitudes do
        val (r, im) = (state(2 * (i + bit)) * scale, state(2 * (i + bit) + 1) * scale)

        state(2 * i) = r
        state(2 * i + 1) = im
        state(2 * i + 2) = r
        state(2 * i + 3) = im
        i += 2

    /** The correction before step `k`: `-pi j / 2^k` for the bits `j` measured so far. */
    def phase(measured: Long, k: Int): Double = -Math.PI * measured / Math.pow(2, k)

    /** One run: the measured `y`, with `y / 2^t` close to `s / r` for some `s`. */
    def sample(random: Random): Long =
      val state = initial
      val scratch = new Array[Double](state.length)
      // The initial state has y = 1 with the counting qubit at |0>; the first reset puts it in |+>.
      collapseAndReset(state, 0, 1d)

      var y = 0L
      for k <- 0 until t do
        multiply(state, scratch, powers(k))
        phaseAndHadamard(state, phase(y, k))

        val one = probabilityOfOne(state)
        val bit = if random.nextDouble() < one then 1 else 0

        collapseAndReset(state, bit, if bit == 1 then one else 1 - one)
        y |= bit.toLong << k

      y

    /** The whole distribution of `y`, by following both outcomes of every measurement.
      *
      * `2^t` branches, so for small moduli only; it is what lets a test compare the algorithm with
      * the analytic phase-estimation distribution exactly rather than by sampling.
      */
    def exactDistribution: Map[Long, Double] =
      require(t <= 16, s"$t counting bits is ${1L << t} branches, too many to follow")

      val result = Map.newBuilder[Long, Double]
      val scratch = new Array[Double](2 * amplitudes)

      def follow(state: Array[Double], k: Int, measured: Long, weight: Double): Unit =
        if k == t then result += measured -> weight
        else
          multiply(state, scratch, powers(k))
          phaseAndHadamard(state, phase(measured, k))

          val one = probabilityOfOne(state)

          for (bit, p) <- Seq((0, 1 - one), (1, one)) if p > 1e-15 do
            val branch = state.clone()

            collapseAndReset(branch, bit, p)
            follow(branch, k + 1, measured | (bit.toLong << k), weight * p)

      val start = initial
      collapseAndReset(start, 0, 1d)
      follow(start, 0, 0L, 1d)
      result.result()

  /** Denominators of the convergents of `y / q`, up to `limit`: the continued-fraction step, in
    * `Long` because `q = 2^t` passes `Int` once the modulus does 15 bits.
    */
  def convergents(y: Long, q: Long, limit: Long): Vector[Long] =
    val found = Vector.newBuilder[Long]
    var (previous, denominator) = (1L, 0L)
    var (numerator, remainder) = (y, q)

    while remainder != 0 && denominator <= limit do
      val term = numerator / remainder
      val next = term * denominator + previous

      previous = denominator
      denominator = next
      val carried = numerator - term * remainder
      numerator = remainder
      remainder = carried

      if denominator > 0 && denominator <= limit then found += denominator

    found.result().distinct

  /** The period of `a` mod `n` inferred from a measurement `y` of a `2^t`-point transform. */
  def period(a: Long, n: Long, y: Long, t: Int): Option[Long] =
    convergents(y, 1L << t, n).find(r => r > 1 && BigInt(a).modPow(r, n) == 1)

  /** The classical tail: a factorisation unless the period is odd or its half power is `-1`. */
  def factorFromPeriod(a: Long, n: Long, r: Long): Option[(Long, Long)] =
    Option.when(r % 2 == 0)(BigInt(a).modPow(r / 2, n)).filter(_ != n - 1).flatMap: root =>
      val (low, high) = ((root - 1).gcd(n).toLong, (root + 1).gcd(n).toLong)

      Option.when(low > 1 && high > 1 && low * high == n)((Math.min(low, high), Math.max(low, high)))

  /** One run with the base `a`, and the classical tail. */
  def quantumFactor(a: Long, n: Long, random: Random): Option[(Long, Long)] =
    val finder = PeriodFinder(a, n)

    period(a, n, finder.sample(random), finder.t).flatMap(factorFromPeriod(a, n, _))

  /** Random bases until one run yields a factorisation, up to `attempts` runs; a base sharing a
    * factor with `n` factors it classically and is reported as such.
    */
  def factor(n: Long, random: Random, attempts: Int = 20): Option[(Long, Long)] =
    Iterator
      .continually(2 + random.nextLong(n - 3))
      .take(attempts)
      .map: a =>
        val shared = BigInt(a).gcd(BigInt(n)).toLong

        if shared > 1 then Some((Math.min(shared, n / shared), Math.max(shared, n / shared)))
        else quantumFactor(a, n, random)
      .collectFirst { case Some(f) => f }
