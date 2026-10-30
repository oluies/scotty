package scotty.algorithm

import scotty.quantum.Circuit
import scotty.quantum.gate.Gate
import scotty.quantum.gate.StandardGate.{H, PHASE, X}
import scotty.simulator.QuantumSimulator
import scotty.simulator.fusion.{FusedCircuit, FusedRunner, GateFusion}
import scala.collection.mutable
import scala.util.Random

/** Shor's period finding for any odd modulus, on `2n + 3` qubits.
  *
  * The arithmetic is [[Beauregard]]'s. The counting register is a single qubit, measured and reused
  * once per exponent bit, which is R. B. Griffiths and C.-S. Niu, *Semiclassical Fourier Transform
  * for Quantum Computation*, Phys. Rev. Lett. 76, 3228–3231 (1996), arXiv:quant-ph/9511007: the
  * inverse transform's controlled rotations become rotations chosen by the bits already measured.
  * That takes the `2n` counting qubits of the textbook circuit down to one, which is what keeps a
  * state vector for 21 at `2^13` amplitudes rather than `2^22`.
  *
  * The bits come out least significant first. Step `k` controls `U^(2^(m - 1 - k))` with
  * `U = |x> -> |a x mod N>`, rotates by `-2 pi sum_(l < k) y_l / 2^(k - l + 1)`, and measures. The
  * simulator has no mid-circuit measurement, so this drives the state vector itself, and measures
  * and resets the counting qubit between steps.
  *
  * Each controlled multiplication is about 1350 gates at `N = 21`, so it goes through Scotty's gate
  * fusion at the default width of five, which cuts one from about 140 ms to about 25 ms, and a
  * [[PeriodFinder]] keeps the fused multiplier for each power, so repeated shots reuse it.
  *
  * Qubit layout: `0` counts, `1 .. n` hold `x`, `n + 1 .. 2n + 1` hold `b`, and `2n + 2` is the
  * modular adder's ancilla.
  */
object SemiclassicalShor:
  /** The layout for an `n`-bit modulus. */
  final case class Layout(bits: Int):
    val control: Int = 0
    val x: IndexedSeq[Int] = (1 to bits).toIndexedSeq
    val b: IndexedSeq[Int] = (bits + 1 to 2 * bits + 1).toIndexedSeq
    val ancilla: Int = 2 * bits + 2
    val qubits: Int = 2 * bits + 3

  /** Period finding for one base and modulus, keeping each fused multiplier for the next shot.
    *
    * `countingBits` defaults to `2n`, the precision that makes the continued fractions recover any
    * period below `n`.
    */
  final class PeriodFinder(val a: Long, val n: Long, countingBits: Int = -1):
    require(n % 2 == 1 && n > 2, s"$n has to be odd and above 2")
    require(BigInt(a).gcd(BigInt(n)) == 1, s"$a and $n are not coprime")

    val layout: Layout = Layout(Beauregard.width(n))
    val m: Int = if countingBits > 0 then countingBits else 2 * layout.bits

    private val multipliers = mutable.Map.empty[Long, FusedCircuit]

    private def multiplier(power: Long): FusedCircuit =
      multipliers.getOrElseUpdate(
        power,
        GateFusion.fuse(
          Circuit(
            Circuit.generateRegister(layout.qubits),
            Beauregard.controlledMultiply(power, n, layout.control, layout.x, layout.b, layout.ancilla)*
          )
        )
      )

    /** One run: the measured `y`, with `y / 2^m` close to `s / r` for some `s`. */
    def sample(random: Random): Int =
      val simulator = QuantumSimulator(random)
      val state = new Array[Double](2 << layout.qubits)
      val iterator = simulator.parIndices(state.length / 2)

      def apply(gates: Seq[Gate]): Unit = gates.foreach(simulator.applyGate(iterator, state, _))

      state(0) = 1d
      apply(Seq(X(layout.x.head))) // x starts at |1>, the multiplicative identity

      var y = 0

      for k <- 0 until m do
        val power = BigInt(a).modPow(BigInt(2).pow(m - 1 - k), BigInt(n)).toLong
        val correction = -2 * Math.PI * (0 until k).map(l => ((y >> l) & 1) / Math.pow(2, k - l + 1)).sum

        apply(Seq(H(layout.control)))
        FusedRunner.applyAll(state, multiplier(power), simulator)
        if correction != 0d then apply(Seq(PHASE(correction, layout.control)))
        apply(Seq(H(layout.control)))

        val bit = measureQubit(state, layout.control, random)

        if bit == 1 then apply(Seq(X(layout.control)))
        y |= bit << k

      y

  /** One run: the measured `y` of a `2^m`-point transform, `m = 2n` unless `countingBits` is set. */
  def measure(a: Long, n: Long, random: Random, countingBits: Int = -1): Int =
    PeriodFinder(a, n, countingBits).sample(random)

  /** Measures one qubit in the computational basis, collapsing and renormalising `state`. */
  def measureQubit(state: Array[Double], qubit: Int, random: Random): Int =
    val mask = 1 << qubit
    val amplitudes = state.length / 2
    var one = 0d
    var i = 0

    while i < amplitudes do
      if (i & mask) != 0 then one += state(2 * i) * state(2 * i) + state(2 * i + 1) * state(2 * i + 1)
      i += 1

    val bit = if random.nextDouble() < one then 1 else 0
    val norm = Math.sqrt(if bit == 1 then one else 1 - one)

    i = 0
    while i < amplitudes do
      val keep = ((i & mask) != 0) == (bit == 1)

      state(2 * i) = if keep then state(2 * i) / norm else 0d
      state(2 * i + 1) = if keep then state(2 * i + 1) / norm else 0d
      i += 1

    bit

  /** The period of `a` mod `n` from one run, or `None` when the continued fractions miss it. */
  def period(a: Long, n: Long, random: Random): Option[Int] =
    val m = 2 * Beauregard.width(n)

    Shor.period(a.toInt, measure(a, n, random), 1 << m, n.toInt)

  /** One run with the base `a`, and the classical tail: a factorisation, or `None` when the measured
    * phase gives no period, or an odd one, or one whose square root of 1 is `-1`.
    */
  def quantumFactor(a: Long, n: Long, random: Random): Option[(Long, Long)] =
    val m = 2 * Beauregard.width(n)

    Shor.factor(a.toInt, measure(a, n, random), 1 << m, n.toInt).map((p, q) => (p.toLong, q.toLong))

  /** The full algorithm: random bases until one run yields a factorisation, up to `attempts` runs.
    *
    * A base that shares a factor with `n` already factors it classically, and is reported as such;
    * this is the usual first step, and on small moduli it happens often.
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
