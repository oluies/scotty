package scotty.simulator.fusion

import scotty.quantum.QuantumContext.QuantumException
import scotty.quantum.density.ComplexMatrix
import scotty.quantum.gate.*
import scotty.quantum.gate.StandardGate.{CPHASE00, CPHASE01, CSWAP, ISWAP, PSWAP, SWAP}


/** One step of a lowered circuit: either a fused block or a gate left for the simulator. */
sealed trait FusedOp:
  def qubits: IndexedSeq[Int]

  final def width: Int = qubits.length

/** A run of gates collapsed into one operator over an ascending qubit support.
  *
  * The support is held ascending, and local bit `j` of an index into [[operator]] is the value of
  * global qubit `qubits(j)`. That matches the simulator, which reads qubit `i` out of bit `i` of the
  * amplitude index (see `QuantumSimulator.nthCleared`), so the lowest-numbered qubit in the support
  * is the least significant bit of the local index.
  *
  * The operator is stored in the tightest form it admits — see [[BlockForm]] — which is what keeps
  * a multi-controlled `Z` from asking for `4^k` entries when it only needs `2^k`, and what lets the
  * kernel skip the shuffle network entirely for diagonal blocks.
  */
final case class FusedBlock(qubits: IndexedSeq[Int], operator: BlockOperator) extends FusedOp:
  def form: BlockForm = operator.form

  def dimension: Int = operator.dimension

  require(qubits.nonEmpty, "A fused block needs at least one qubit in its support.")
  require(qubits.distinct.length == width, s"A fused block's support repeats a qubit: $qubits.")
  require(
    width == 1 || qubits.lazyZip(qubits.tail).forall(_ < _),
    s"A fused block's support must be ascending: $qubits."
  )
  require(
    operator.width == width,
    s"A block over $width qubits needs an operator of the same width, not ${operator.width}."
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

      FusedBlock(support, operator.expand(qubits.map(support.indexOf).toArray, support.length))

  /** This block applied after `first`, over the union of the two supports. */
  def after(first: FusedBlock): FusedBlock =
    val support = (first.qubits ++ qubits).distinct.sorted

    FusedBlock(support, expandTo(support).operator.after(first.expandTo(support).operator))

/** A gate the pass declined to fuse, left for the simulator to apply through its own path.
  *
  * This is the escape hatch for gates whose support is wider than the fusion budget — most often a
  * multi-controlled gate spanning the whole register, as in Grover's oracle and diffusion operator.
  * The simulator applies those in one pass over the state; building any kind of operator for them,
  * even a cheap one, would be the pass making work for itself.
  */
final case class NativeOp(gate: Gate) extends FusedOp:
  lazy val qubits: IndexedSeq[Int] = gate.indices.distinct.sorted.toIndexedSeq

object FusedBlock:
  /** The widest support this pass will densify.
    *
    * A dense operator over `k` qubits is `4^k` complex entries, so the cost of getting this wrong is
    * severe rather than merely slow. Diagonal and monomial operators are `2^k` and are not subject
    * to this cap; neither is a gate wider than the fusion budget, which becomes a [[NativeOp]].
    */
  val MaxDenseWidth: Int = 10

  /** Every gate the simulator can apply, lowered to blocks of width 1 to 3, or to a [[NativeOp]].
    *
    * The cases are in the same order as `QuantumSimulator.applyGate`, and reuse the same composite
    * decompositions, so a lowered circuit is the simulator's own semantics by construction rather
    * than by a second reading of the gate definitions. The order matters: `CSWAP`, `CPHASE00` and
    * `CPHASE01` are `ControlGate`s whose `finalTarget` is not a single-qubit gate, and so must be
    * decomposed before the general controlled-gate case is reached.
    */
  def lower(gate: Gate, maxWidth: Int = GateFusion.DefaultMaxWidth): Seq[FusedOp] = gate match
    case g: SWAP     => lowerComposite(Decompositions.swap(g.index1, g.index2), maxWidth)
    case g: CSWAP    => lowerComposite(Decompositions.cswap(g.controlIndex, g.index1, g.index2), maxWidth)
    case g: ISWAP    => lowerComposite(Decompositions.iswap(g.index1, g.index2), maxWidth)
    case g: PSWAP    => lowerComposite(Decompositions.pswap(g.phi, g.index1, g.index2), maxWidth)
    case g: CPHASE00 => lowerComposite(Decompositions.cphase00(g.phi, g.controlIndex, g.targetIndex), maxWidth)
    case g: CPHASE01 => lowerComposite(Decompositions.cphase01(g.phi, g.controlIndex, g.targetIndex), maxWidth)
    case g: ControlGate =>
      Seq(if g.indices.distinct.length > maxWidth then NativeOp(g) else ofControl(g))
    case g: TargetGate    => Seq(ofTarget(g))
    case g: CompositeGate => lowerComposite(g, maxWidth)
    case g                => throw QuantumException(s"Unsupported gate: ${g.name}")

  private def lowerComposite(gate: CompositeGate, maxWidth: Int): Seq[FusedOp] =
    gate.gates.flatMap(lower(_, maxWidth))

  /** A one-qubit gate, in whichever form its matrix admits. */
  def ofTarget(gate: TargetGate): FusedBlock =
    FusedBlock(IndexedSeq(gate.index), BlockOperator.classify(1, ComplexMatrix.fromRaw(gate.matrix)))

  /** A controlled gate over its controls and its target, in the target's own form.
    *
    * A controlled gate is exactly as cheap as the gate it controls: controlling a diagonal gate
    * gives a diagonal operator, controlling a permutation gives a permutation. Only a controlled
    * dense gate needs `4^k` entries, which is why `CZ`, `CPHASE` and every multi-controlled `Z` cost
    * `2^k` here and a controlled `H` does not.
    */
  def ofControl(gate: ControlGate): FusedBlock =
    val target = gate.finalTarget
    val support = (gate.controlIndexes :+ target.index).sorted.toIndexedSeq
    val targetBit = support.indexOf(target.index)
    val controlBits = gate.controlIndexes.map(support.indexOf).toArray
    val d = 1 << support.length

    def triggered(index: Int): Boolean =
      var j = 0
      var all = true

      while all && j < controlBits.length do
        if ((index >> controlBits(j)) & 1) == 0 then all = false
        j += 1

      all

    val operator = BlockOperator.classify(1, ComplexMatrix.fromRaw(target.matrix)) match
      case diagonal: DiagonalOperator =>
        val values = new Array[Double](2 * d)
        var i = 0

        while i < d do
          if triggered(i) then
            val bit = (i >> targetBit) & 1

            values(2 * i) = diagonal.values(2 * bit)
            values(2 * i + 1) = diagonal.values(2 * bit + 1)
          else values(2 * i) = 1d
          i += 1

        DiagonalOperator(support.length, values)

      case monomial: MonomialOperator =>
        val rows = new Array[Int](d)
        val values = new Array[Double](2 * d)
        var col = 0

        while col < d do
          if triggered(col) then
            val bit = (col >> targetBit) & 1

            rows(col) = (col & ~(1 << targetBit)) | (monomial.rows(bit) << targetBit)
            values(2 * col) = monomial.values(2 * bit)
            values(2 * col + 1) = monomial.values(2 * bit + 1)
          else
            rows(col) = col
            values(2 * col) = 1d
          col += 1

        MonomialOperator(support.length, rows, values).simplified

      case dense: DenseOperator =>
        require(
          support.length <= MaxDenseWidth,
          s"${gate.name} controls a dense gate over ${support.length} qubits, past the " +
            s"dense-lowering cap of $MaxDenseWidth. A dense operator that wide is 4^k complex " +
            "entries (see FusedBlock.MaxDenseWidth)."
        )

        val others = (d - 1) & ~(1 << targetBit)
        val flat = new Array[Double](2 * d * d)
        var row = 0

        while row < d do
          var col = 0
          while col < d do
            if (row & others) == (col & others) then
              val rt = (row >> targetBit) & 1
              val ct = (col >> targetBit) & 1
              val entry = 2 * (row * d + col)

              if triggered(col) then
                flat(entry) = dense.matrix.realAt(rt, ct)
                flat(entry + 1) = dense.matrix.imagAt(rt, ct)
              else if rt == ct then flat(entry) = 1d
            col += 1
          row += 1

        DenseOperator(support.length, ComplexMatrix.fromInterleaved(d, d, flat))

    FusedBlock(support, operator)
