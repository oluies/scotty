package scotty.algorithm

import scotty.quantum.Circuit
import scotty.quantum.gate.StandardGate.{H, X, Z}
import scotty.quantum.gate.{Controlled, Gate}

/** Grover's search, as a circuit over `qubitCount` qubits with a phase oracle.
  *
  * Marked states are given as plain integers, read with qubit 0 as the least significant bit, so
  * `marked = 5` over three qubits means qubits 0 and 2 set. The oracle is the usual conjugation of a
  * multiply-controlled `Z` by `X`s on the zero bits, which means it is *constructed* from the answer
  * rather than queried — that is the standard arrangement for a simulated search and makes the
  * circuit a benchmark rather than a search engine.
  *
  * It is a good workload for exactly the reason it is a dull algorithm: the gate count is
  * `O(sqrt(N))` layers of all-qubit operations with known-correct output, so a run is self-checking.
  * The amplitude on the marked state has to approach 1 after the optimal number of iterations, which
  * catches errors an amplitude-by-amplitude diff against another simulator would not — a diff only
  * tells you two implementations agree.
  */
object Grover:
  /** The optimal iteration count, `floor(pi/4 * sqrt(N/M))`, at least one. */
  def iterations(qubitCount: Int, markedCount: Int = 1): Int =
    val n = Math.pow(2d, qubitCount.toDouble)

    Math.max(1, Math.floor(Math.PI / 4 * Math.sqrt(n / markedCount)).toInt)

  /** A multiply-controlled `Z` over every qubit, which flips the phase of `|1...1>`. */
  def controlledZ(qubits: IndexedSeq[Int]): Gate =
    qubits.init.foldRight[Gate](Z(qubits.last))((control, target) => Controlled(control, target))

  /** Flips the phase of `|0...0>`. */
  def phaseFlipZero(qubits: IndexedSeq[Int]): Vector[Gate] =
    val flips = qubits.map(X(_)).toVector

    flips ++ Vector(controlledZ(qubits)) ++ flips

  /** Flips the phase of `|marked>`. */
  def oracle(qubits: IndexedSeq[Int], marked: Int): Vector[Gate] =
    val flips = qubits.zipWithIndex.collect { case (q, j) if ((marked >> j) & 1) == 0 => X(q) }.toVector

    flips ++ Vector(controlledZ(qubits)) ++ flips

  /** The diffusion operator `H (I - 2|0><0|) H = I - 2|psi><psi|`. */
  def diffusion(qubits: IndexedSeq[Int]): Vector[Gate] =
    val hadamards = qubits.map(H(_)).toVector

    hadamards ++ phaseFlipZero(qubits) ++ hadamards

  def gates(qubitCount: Int, marked: Seq[Int], iterationCount: Int): Vector[Gate] =
    val qubits = (0 until qubitCount).toIndexedSeq
    val iteration = marked.toVector.flatMap(oracle(qubits, _)) ++ diffusion(qubits)

    qubits.map(H(_)).toVector ++ Vector.fill(iterationCount)(iteration).flatten

  def circuit(qubitCount: Int, marked: Int): Circuit =
    circuit(qubitCount, Seq(marked), iterations(qubitCount))

  def circuit(qubitCount: Int, marked: Seq[Int], iterationCount: Int): Circuit =
    Circuit(Circuit.generateRegister(qubitCount), gates(qubitCount, marked, iterationCount)*)
