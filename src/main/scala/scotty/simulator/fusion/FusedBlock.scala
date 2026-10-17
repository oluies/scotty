package scotty.simulator.fusion

import scotty.quantum.QuantumContext.QuantumException
import scotty.quantum.density.ComplexMatrix
import scotty.quantum.gate.*
import scotty.quantum.gate.StandardGate.{CPHASE00, CPHASE01, CSWAP, ISWAP, PSWAP, SWAP}
import scotty.simulator.QuantumSimulator

/** A run of gates collapsed into one dense operator over an ascending qubit support.
  *
  * The simulator applies a one-qubit gate by reading an amplitude pair, multiplying it by a 2x2
  * matrix and writing it back — about 28 flops against 64 bytes of traffic, which is far too little
  * arithmetic per byte to keep any wide multiplier array busy. Fusing a run of gates that touch the
  * same `k` qubits into a single `2^k x 2^k` operator raises that ratio by roughly `2^k / 4`,
  * because the operator is loaded once and then reused across every one of the `2^(n-k)` slices of
  * the state. [[FusionCost]] computes the ratio for a given circuit.
  *
  * The support is held ascending, and local bit `j` of an index into [[operator]] is the value of
  * global qubit `qubits(j)`. That matches the simulator, which reads qubit `i` out of bit `i` of the
  * amplitude index (see `QuantumSimulator.nthCleared`), so the lowest-numbered qubit in the support
  * is the least significant bit of the local index.
  */
final case class FusedBlock(qubits: IndexedSeq[Int], operator: ComplexMatrix):
  val width: Int = qubits.length
  val dimension: Int = 1 << width

  require(qubits.nonEmpty, "A fused block needs at least one qubit in its support.")
  require(qubits.distinct.length == width, s"A fused block's support repeats a qubit: $qubits.")
  require(
    width == 1 || qubits.lazyZip(qubits.tail).forall(_ < _),
    s"A fused block's support must be ascending: $qubits."
  )
  require(
    operator.rows == dimension && operator.cols == dimension,
    s"A block over $width qubits needs a ${dimension}x$dimension operator, " +
      s"not ${operator.rows}x${operator.cols}."
  )

  /** The same operator re-expressed over a wider support, by tensoring with the identity on the
    * qubits this block does not touch. `support` must be ascending and contain [[qubits]].
    */
  def expandTo(support: IndexedSeq[Int]): FusedBlock =
    if support == qubits then this
    else
      require(
        qubits.forall(support.contains),
        s"Cannot expand a block over $qubits into the narrower support $support."
      )

      val positions = qubits.map(support.indexOf).toArray
      val d = 1 << support.length
      val spanned = positions.foldLeft(0)((mask, p) => mask | (1 << p))
      val untouched = (d - 1) & ~spanned
      val values = new Array[Double](2 * d * d)

      var row = 0
      while row < d do
        var col = 0
        while col < d do
          // An identity factor contributes nothing unless the untouched bits agree, so only the
          // block-diagonal entries are ever non-zero.
          if (row & untouched) == (col & untouched) then
            val r = FusedBlock.gather(row, positions)
            val c = FusedBlock.gather(col, positions)
            val i = 2 * (row * d + col)

            values(i) = operator.realAt(r, c)
            values(i + 1) = operator.imagAt(r, c)
          col += 1
        row += 1

      FusedBlock(support, ComplexMatrix.fromInterleaved(d, d, values))

  /** This block applied after `first`, over the union of the two supports. */
  def after(first: FusedBlock): FusedBlock =
    val support = (first.qubits ++ qubits).distinct.sorted
    val second = expandTo(support)

    FusedBlock(support, second.operator.multiply(first.expandTo(support).operator))

object FusedBlock:
  /** The widest support this pass will densify.
    *
    * A dense operator over `k` qubits is `4^k` complex entries, so the cost of getting this wrong is
    * severe rather than merely slow: a 20-qubit controlled gate would ask for `10^12` entries. The
    * cap exists because the greedy pass has no sparse representation, and some perfectly ordinary
    * circuits contain gates it therefore cannot express — Grover's oracle and diffusion operator
    * both contain a controlled `Z` over *every* qubit. Those gates are diagonal, and the simulator
    * applies them in one pass over the state; densifying them is the pass's fault, not theirs.
    *
    * Lifting this is the next piece of work, and it is a sparsity-aware lowering rather than a
    * bigger number: keep diagonal and permutation gates in their cheap form instead of expanding
    * them. 10 qubits is 16 MiB of operator, already far past anything worth building.
    */
  val MaxDenseWidth: Int = 10

  /** Every gate the simulator can apply, lowered to a sequence of dense blocks of width 1 to 3.
    *
    * The cases are in the same order as `QuantumSimulator.applyGate`, and reuse the same composite
    * decompositions, so a lowered circuit is the simulator's own semantics by construction rather
    * than by a second reading of the gate definitions. The order matters: `CSWAP`, `CPHASE00` and
    * `CPHASE01` are `ControlGate`s whose `finalTarget` is not a single-qubit gate, and so must be
    * decomposed before the general controlled-gate case is reached.
    */
  def lower(gate: Gate): Seq[FusedBlock] = gate match
    case g: SWAP     => lowerComposite(QuantumSimulator.swap(g.index1, g.index2))
    case g: CSWAP    => lowerComposite(QuantumSimulator.cswap(g.controlIndex, g.index1, g.index2))
    case g: ISWAP    => lowerComposite(QuantumSimulator.iswap(g.index1, g.index2))
    case g: PSWAP    => lowerComposite(QuantumSimulator.pswap(g.phi, g.index1, g.index2))
    case g: CPHASE00 => lowerComposite(QuantumSimulator.cphase00(g.phi, g.controlIndex, g.targetIndex))
    case g: CPHASE01 => lowerComposite(QuantumSimulator.cphase01(g.phi, g.controlIndex, g.targetIndex))
    case g: ControlGate   => Seq(ofControl(g))
    case g: TargetGate    => Seq(ofTarget(g))
    case g: CompositeGate => lowerComposite(g)
    case g                => throw QuantumException(s"Unsupported gate: ${g.name}")

  private def lowerComposite(gate: CompositeGate): Seq[FusedBlock] = gate.gates.flatMap(lower)

  /** A one-qubit gate as a width-1 block. */
  def ofTarget(gate: TargetGate): FusedBlock =
    FusedBlock(IndexedSeq(gate.index), ComplexMatrix.fromRaw(gate.matrix))

  /** A controlled gate as a dense block over its controls and its target.
    *
    * The entry at `(row, col)` is the target gate's own entry when `col` has every control bit set,
    * and the identity otherwise; in both cases the non-target bits have to agree, since a
    * controlled gate moves no amplitude between control settings.
    */
  def ofControl(gate: ControlGate): FusedBlock =
    val target = gate.finalTarget
    val support = (gate.controlIndexes :+ target.index).sorted.toIndexedSeq
    require(
      support.length <= MaxDenseWidth,
      s"${gate.name} spans ${support.length} qubits, past the dense-lowering cap of $MaxDenseWidth. " +
        "A dense operator that wide is 4^k complex entries; this gate needs a sparse lowering " +
        "(see FusedBlock.MaxDenseWidth)."
    )

    val targetBit = support.indexOf(target.index)
    val controlBits = gate.controlIndexes.map(support.indexOf).toArray
    val d = 1 << support.length
    val g = ComplexMatrix.fromRaw(target.matrix)
    val others = (d - 1) & ~(1 << targetBit)
    val values = new Array[Double](2 * d * d)

    var row = 0
    while row < d do
      var col = 0
      while col < d do
        if (row & others) == (col & others) then
          val rt = (row >> targetBit) & 1
          val ct = (col >> targetBit) & 1
          val i = 2 * (row * d + col)

          if controlBits.forall(b => ((col >> b) & 1) == 1) then
            values(i) = g.realAt(rt, ct)
            values(i + 1) = g.imagAt(rt, ct)
          else if rt == ct then values(i) = 1d
        col += 1
      row += 1

    FusedBlock(support, ComplexMatrix.fromInterleaved(d, d, values))

  /** The bits of `index` at `positions`, packed down into a local index. */
  private[fusion] def gather(index: Int, positions: Array[Int]): Int =
    var packed = 0
    var j = 0

    while j < positions.length do
      packed |= ((index >> positions(j)) & 1) << j
      j += 1

    packed
