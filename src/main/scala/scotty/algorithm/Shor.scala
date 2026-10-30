package scotty.algorithm

import scotty.quantum.Circuit
import scotty.quantum.gate.Gate
import scotty.quantum.gate.StandardGate.{CNOT, CSWAP, H, X}

/** Shor's period finding for N = 15, with a hand-compiled modular exponentiation.
  *
  * The restriction to 15 is a choice. A general modular exponentiation circuit for an `m`-bit
  * modulus needs on the order of `m^3` gates and `2m + 3` qubits in all, with a single counting
  * qubit measured and reused, which a state-vector simulator on one machine outgrows within a few
  * bits; on a GPU supercomputer Willsch et al. ran the general algorithm to factor 549 755 813 701
  * (Mathematics 11, 4222 (2023), arXiv:2308.05047). This circuit instead compiles the arithmetic
  * for one modulus and one base, which keeps it to eight qubits, with the caveat Smolin, Smith and
  * Vargo make (Nature 499, 163 (2013), arXiv:1301.7007): a compiled circuit demonstrates the period
  * it finds, not the size of the number.
  *
  * The compilation is the standard one. Modulo 15, multiplication by 2 is a cyclic rotation of the
  * four work bits, because `2^4 = 16 = 1 (mod 15)`; and bitwise negation is multiplication by -1,
  * because `15 - x = -x (mod 15)`. So every base in [[Bases]] is a rotation, optionally followed by
  * a negation, and the controlled version is `CSWAP`s and `CNOT`s. Nothing else is needed.
  *
  * The quantum part returns a measurement of the counting register; [[period]] and [[factor]] are
  * the ordinary classical post-processing, which is where the continued fractions live.
  */
object Shor:
  val Modulus: Int = 15

  /** The bases coprime to 15 that the compiled multiplier covers. */
  val Bases: Set[Int] = Set(2, 4, 7, 8, 11, 13)

  /** Work-register width for N = 15. */
  val WorkQubits: Int = 4

  /** One controlled multiplication by `a` modulo 15, on four work qubits. */
  def multiplier(a: Int, control: Int, work: IndexedSeq[Int]): Vector[Gate] =
    require(Bases.contains(a), s"$a is not coprime to $Modulus, or not in the compiled set $Bases.")
    require(work.length == WorkQubits, s"N = $Modulus needs $WorkQubits work qubits, not ${work.length}.")

    val builder = Vector.newBuilder[Gate]

    def swap(i: Int, j: Int): Unit =
      val (low, high) = if work(i) < work(j) then (work(i), work(j)) else (work(j), work(i))

      builder += CSWAP(control, low, high)

    // Rotate: multiplication by a power of two.
    if a == 2 || a == 13 then
      swap(2, 3)
      swap(1, 2)
      swap(0, 1)
    if a == 7 || a == 8 then
      swap(0, 1)
      swap(1, 2)
      swap(2, 3)
    if a == 4 || a == 11 then
      swap(1, 3)
      swap(0, 2)

    // Negate: multiplication by -1, for the bases that are the negative of a power of two.
    if a == 7 || a == 11 || a == 13 then work.foreach(q => builder += CNOT(control, q))

    builder.result()

  /** `a^(2^exponent) mod 15`, controlled, by repeating the permutation. */
  def controlledPower(a: Int, exponent: Int, control: Int, work: IndexedSeq[Int]): Vector[Gate] =
    Vector.fill(1 << exponent)(multiplier(a, control, work)).flatten

  /** The period-finding circuit: counting qubits `0 until countingQubits`, then four work qubits.
    *
    * Measuring the counting register gives `y`, and `y / 2^countingQubits` approximates `k / r` for
    * the period `r`. Four counting qubits is enough for every base here, since every period divides
    * 4 and so lands exactly on a multiple of `2^4 / r`.
    */
  def periodCircuit(a: Int, countingQubits: Int = 4): Circuit =
    val counting = (0 until countingQubits).toIndexedSeq
    val work = (countingQubits until countingQubits + WorkQubits).toIndexedSeq
    val gates =
      counting.map(H(_)).toVector ++
        // The work register starts at |1>, the multiplicative identity.
        Vector(X(work.head)) ++
        counting.zipWithIndex.flatMap((control, exponent) => controlledPower(a, exponent, control, work)) ++
        Qft.inverseGates(counting)

    Circuit(Circuit.generateRegister(countingQubits + WorkQubits), gates*)

  /** Denominators of the convergents of `y / q`, up to `limit`.
    *
    * This is the continued-fraction step: the period is the denominator of the best rational
    * approximation to the measured phase, and the convergents are the candidates to test.
    */
  def convergents(y: Int, q: Int, limit: Int): Vector[Int] =
    val found = Vector.newBuilder[Int]
    var previousDenominator = 1
    var denominator = 0
    var numerator = y
    var remainder = q

    while remainder != 0 && denominator <= limit do
      val term = numerator / remainder
      val next = term * denominator + previousDenominator

      previousDenominator = denominator
      denominator = next

      val carried = numerator - term * remainder

      numerator = remainder
      remainder = carried

      if denominator > 0 && denominator <= limit then found += denominator

    found.result().distinct

  /** The period of `a` modulo `n`, inferred from a measurement `y` of a `q`-point transform. */
  def period(a: Int, y: Int, q: Int, n: Int = Modulus): Option[Int] =
    convergents(y, q, n).find(r => r > 1 && modPow(a, r, n) == 1)

  /** The classical tail of Shor: a period gives a factorisation unless it is odd or trivial. */
  def factor(a: Int, y: Int, q: Int, n: Int = Modulus): Option[(Int, Int)] =
    period(a, y, q, n).filter(_ % 2 == 0).flatMap: r =>
      val root = modPow(a, r / 2, n)

      if root == n - 1 then None
      else
        val low = gcd(root - 1, n)
        val high = gcd(root + 1, n)

        if low > 1 && high > 1 && low * high == n then Some((Math.min(low, high), Math.max(low, high)))
        else None

  /** The period of `a` modulo `n`, computed classically. The answer the circuit has to reproduce. */
  def classicalPeriod(a: Int, n: Int = Modulus): Int =
    // Without coprimality there is no period: the powers of a fall into a cycle that never reaches
    // 1, and the loop below would not terminate.
    require(gcd(a, n) == 1, s"$a and $n are not coprime, so $a has no multiplicative order mod $n.")

    // Long for the same reason modPow uses it: the product overflows Int once the modulus passes
    // about 46341, and this is a public helper that does not default its base.
    val base = a.toLong % n
    var power = base
    var r = 1

    while power != 1 do
      power = power * base % n
      r += 1

    r

  /** Square-and-multiply, with `Long` intermediates: `result * base` overflows `Int` once the
    * modulus passes about 46341, and the linear version is needlessly slow for large exponents.
    */
  def modPow(base: Int, exponent: Int, modulus: Int): Int =
    var result = 1L
    var factor = base.toLong % modulus
    var remaining = exponent

    while remaining > 0 do
      if (remaining & 1) == 1 then result = result * factor % modulus
      factor = factor * factor % modulus
      remaining >>= 1

    result.toInt

  def gcd(a: Int, b: Int): Int = if b == 0 then Math.abs(a) else gcd(b, a % b)
