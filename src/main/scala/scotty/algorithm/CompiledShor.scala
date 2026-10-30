package scotty.algorithm

import scotty.quantum.Circuit
import scotty.quantum.gate.StandardGate.{CNOT, H}

/** Shor's algorithm "compiled" with the answer: two qubits that factor a semiprime of any size.
  *
  * This reproduces J. A. Smolin, G. Smith and A. Vargo, *Pretending to factor large numbers on a
  * quantum computer*, Nature 499, 163–165 (2013), arXiv:1301.7007, as a warning label rather than
  * an algorithm. Compiling Shor's algorithm means choosing the base `a` so that its period, and so
  * the circuit, is small. Their point is that for any `N = p q` there is a base of period 2: by the
  * Chinese remainder theorem `a^2 = 1 (mod p q)` whenever `a^2 = 1` modulo `p` and modulo `q`, and
  * `a = ±p p_q ± q q_p` (their Eq. (4), with `p_q = p^-1 mod q` and `q_p = q^-1 mod p`) gives four
  * such square roots of 1, two of them `±1` and two of period 2. With period 2 the work register
  * only ever holds two values, the modular exponentiation is a single `CNOT`, and the Fourier
  * transform is a single Hadamard: their Fig. 2, which is [[circuit]].
  *
  * The catch is the base: building it takes `p` and `q`, so the compiler already knows the answer,
  * and measuring a period of 2 then "finds" `gcd(a - 1, N)`. Their Section 3 goes further: the
  * `CNOT` leaves the counting qubit maximally mixed, which a Hadamard does not change, so the whole
  * computation is a fair coin, and they "factored" RSA-768 by tossing one. Hence their conclusion
  * that a factoring experiment should be judged "not by the size of the number factored but by the
  * size of the period found", and that "it is *not* legitimate for a compiler to know the answer".
  * [[Shor]]'s circuit for 15 is compiled too, in the milder sense the paper describes for every
  * small experiment; [[SemiclassicalShor]] and [[IterativeShor]] are not.
  */
object CompiledShor:
  /** Fig. 2: `|+>` on the counting qubit `0`, a `CNOT` onto the work qubit `1`, a Hadamard. The
    * same circuit for every `N`, which is the whole point.
    */
  val circuit: Circuit = Circuit(Circuit.generateRegister(2), H(0), CNOT(0, 1), H(0))

  /** The four square roots of 1 modulo `p q` from Eq. (4), smallest first. */
  def squareRootsOfOne(p: BigInt, q: BigInt): Seq[BigInt] =
    require(p != q && p > 2 && q > 2, s"$p and $q have to be distinct primes above 2")

    val n = p * q
    val (pq, qp) = (p * p.modInverse(q), q * q.modInverse(p))

    (for (s, t) <- Seq((1, 1), (1, -1), (-1, 1), (-1, -1)) yield (pq * s + qp * t).mod(n)).distinct.sorted

  /** The compiled base: the smallest square root of 1 that is not `±1`, whose period is 2. Built
    * from the factors, which is the cheat.
    */
  def base(p: BigInt, q: BigInt): BigInt =
    val n = p * q

    squareRootsOfOne(p, q).filter(a => a != 1 && a != n - 1).head

  /** The classical tail for a one-bit transform: reading 1 is the phase `1/2`, so the period is 2
    * and `gcd(a ± 1, N)` are the factors; reading 0 is the phase 0, which says nothing.
    */
  def factors(n: BigInt, a: BigInt, measured: Int): Option[(BigInt, BigInt)] =
    Option.when(measured == 1 && a.modPow(2, n) == 1)(((a - 1).gcd(n), (a + 1).gcd(n))).collect:
      case (low, high) if low > 1 && high > 1 && low * high == n => (low.min(high), low.max(high))
