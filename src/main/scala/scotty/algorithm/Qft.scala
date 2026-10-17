package scotty.algorithm

import scotty.quantum.gate.Gate
import scotty.quantum.gate.StandardGate.{CPHASE, H, SWAP}

/** The quantum Fourier transform, and its inverse.
  *
  * `qubits` is ordered least significant first, matching the simulator's own convention that qubit
  * `i` is bit `i` of the amplitude index. With that reading the transform is the textbook one:
  * `|x> -> (1/sqrt(N)) * sum_y exp(2*pi*i*x*y/N) |y>`, which `QftSpec` checks against an explicitly
  * constructed DFT matrix rather than against another circuit.
  *
  * The closing swaps are the usual bit reversal. They are real `SWAP`s rather than a relabelling so
  * that the transform composes with everything else without the caller having to track a permuted
  * register — at the cost of three CNOTs each, which a fusion pass absorbs anyway.
  */
object Qft:
  def gates(qubits: IndexedSeq[Int]): Vector[Gate] =
    val n = qubits.length
    val builder = Vector.newBuilder[Gate]

    var j = n - 1
    while j >= 0 do
      builder += H(qubits(j))

      var k = j - 1
      while k >= 0 do
        // The controlled rotation by 2*pi / 2^(j - k + 1), written as pi / 2^(j - k).
        builder += CPHASE(Math.PI / (1 << (j - k)), qubits(k), qubits(j))
        k -= 1

      j -= 1

    var i = 0
    while i < n / 2 do
      val a = qubits(i)
      val b = qubits(n - 1 - i)

      builder += (if a < b then SWAP(a, b) else SWAP(b, a))
      i += 1

    builder.result()

  /** The inverse transform: the same gates in reverse with the rotations negated. `H` and `SWAP` are
    * their own inverses.
    */
  def inverseGates(qubits: IndexedSeq[Int]): Vector[Gate] =
    gates(qubits).reverse.map:
      case CPHASE(phi, control, target) => CPHASE(-phi, control, target)
      case gate                         => gate
