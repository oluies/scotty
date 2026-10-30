package scotty.algorithm

import scotty.quantum.gate.{Controlled, Gate}
import scotty.quantum.gate.StandardGate.{CNOT, CPHASE, CSWAP, H, PHASE, SWAP, X}

/** Modular arithmetic for Shor's algorithm on any odd modulus, after S. Beauregard, *Circuit for
  * Shor's algorithm using 2n+3 qubits*, Quantum Information and Computation 3, 175–185 (2003),
  * arXiv:quant-ph/0205095.
  *
  * Additions happen in Fourier space, as in T. G. Draper, *Addition on a Quantum Computer*,
  * arXiv:quant-ph/0008033 (2000): after a [[Qft]] on the `n + 1`-qubit accumulator `b`, adding a
  * classical constant is one phase rotation per qubit and needs no register for the constant. On top
  * of that sit the doubly controlled modular adder, the controlled modular multiplier, and the
  * controlled multiplication `|x> -> |a x mod N>` that [[SemiclassicalShor]] exponentiates.
  *
  * Registers are lists of qubit indices, least significant first, which is [[Qft]]'s convention.
  * `b` has one more qubit than `x` so that `a + b < 2N` cannot overflow, and its top qubit is the
  * sign the modular adder reads after subtracting `N`.
  */
object Beauregard:
  private val TwoPi = 2 * Math.PI

  /** `|phi(b)> -> |phi(b + a)>` on the Fourier-transformed `b`, modulo `2^b.length`.
    *
    * With the transform `|b> -> sum_y exp(2 pi i b y / M) |y>`, qubit `k` of `|phi(b)>` carries the
    * phase `2 pi b 2^k / M`, so adding `a` is a rotation by `2 pi a 2^k / M` on qubit `k`. Rotations
    * that are a whole number of turns are dropped.
    */
  def phiAdd(a: Long, b: IndexedSeq[Int]): Vector[Gate] =
    val m = BigInt(1) << b.length

    b.indices.toVector.flatMap: k =>
      val turns = (BigInt(a) << k).mod(m)

      Option.when(turns != 0)(PHASE(TwoPi * turns.toDouble / m.toDouble, b(k)))

  /** The inverse of a gate sequence: reversed, with every rotation negated. */
  def inverse(gates: Seq[Gate]): Vector[Gate] = gates.reverse.map(invert).toVector

  private def invert(gate: Gate): Gate = gate match
    case PHASE(phi, index)          => PHASE(-phi, index)
    case CPHASE(phi, control, t)    => CPHASE(-phi, control, t)
    case Controlled(control, inner) => Controlled(control, invert(inner))
    case g @ (_: H | _: X | _: CNOT | _: SWAP | _: CSWAP) => g
    case g => throw new IllegalArgumentException(s"no inverse defined for ${g.name}")

  private def controlled(controls: Seq[Int], gates: Seq[Gate]): Vector[Gate] =
    gates.toVector.map(g => controls.foldRight(g)((c, inner) => Controlled(c, inner)))

  /** `|phi(b)> -> |phi((a + b) mod n)>` when both controls are set, and the identity otherwise.
    *
    * Beauregard's Figure 5: add `a`, subtract `n`, copy the sign into `ancilla`, add `n` back if it
    * was negative, then subtract `a` and read the sign again to return `ancilla` to `|0>`. Needs
    * `a < n` and `b < n`, and leaves `ancilla` as it found it.
    */
  def phiAddMod(a: Long, n: Long, controls: Seq[Int], b: IndexedSeq[Int], ancilla: Int): Vector[Gate] =
    require(a >= 0 && a < n, s"the constant $a has to lie in [0, $n)")
    require(n < (1L << (b.length - 1)), s"$n needs ${b.length} qubits in b, one more than its width")

    val sign = b.last
    val addA = controlled(controls, phiAdd(a, b))
    val addN = phiAdd(n, b)

    addA ++
      inverse(addN) ++
      Qft.inverseGates(b) ++ Vector(CNOT(sign, ancilla)) ++ Qft.gates(b) ++
      controlled(Seq(ancilla), addN) ++
      inverse(addA) ++
      Qft.inverseGates(b) ++ Vector(X(sign), CNOT(sign, ancilla), X(sign)) ++ Qft.gates(b) ++
      addA

  /** `|c>|x>|b> -> |c>|x>|(b + a x) mod n>` when `c` is set, the identity otherwise (Figure 6). */
  def controlledMultiplyAdd(a: Long, n: Long, control: Int, x: IndexedSeq[Int], b: IndexedSeq[Int], ancilla: Int): Vector[Gate] =
    val shifted = x.indices.map(i => (BigInt(a) * (BigInt(1) << i)).mod(n).toLong)

    Qft.gates(b) ++
      x.indices.toVector.flatMap(i => phiAddMod(shifted(i), n, Seq(control, x(i)), b, ancilla)) ++
      Qft.inverseGates(b)

  /** `|c>|x>|0> -> |c>|a x mod n>|0>` when `c` is set, the identity otherwise (Figure 7).
    *
    * Multiply-add into `b`, swap `x` and `b`, then undo the multiply-add with `a^-1`, which clears
    * what is left in `b`. Needs `gcd(a, n) = 1` and `x < n`.
    */
  def controlledMultiply(a: Long, n: Long, control: Int, x: IndexedSeq[Int], b: IndexedSeq[Int], ancilla: Int): Vector[Gate] =
    require(b.length == x.length + 1, "b needs exactly one more qubit than x")

    val reduced = Math.floorMod(a, n)
    val swaps = x.indices.toVector.map: i =>
      val (low, high) = if x(i) < b(i) then (x(i), b(i)) else (b(i), x(i))

      CSWAP(control, low, high)

    controlledMultiplyAdd(reduced, n, control, x, b, ancilla) ++
      swaps ++
      inverse(controlledMultiplyAdd(modInverse(reduced, n), n, control, x, b, ancilla))

  /** `a^-1 mod n`, by the extended Euclidean algorithm. */
  def modInverse(a: Long, n: Long): Long =
    require(BigInt(a).gcd(BigInt(n)) == 1, s"$a has no inverse modulo $n")

    BigInt(a).modInverse(BigInt(n)).toLong

  /** Bits needed for `n`: the width of `x`, and one less than the width of `b`. */
  def width(n: Long): Int = 64 - java.lang.Long.numberOfLeadingZeros(n)
