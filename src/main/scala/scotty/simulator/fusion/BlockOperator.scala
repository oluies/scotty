package scotty.simulator.fusion

import scotty.quantum.density.ComplexMatrix

/** Bit-shuffling shared by the operator forms and the kernels. */
object Bits:
  /** The bits of `index` at `positions`, packed down into a local index. */
  def gather(index: Int, positions: Array[Int]): Int =
    var packed = 0
    var j = 0

    while j < positions.length do
      packed |= ((index >> positions(j)) & 1) << j
      j += 1

    packed

  /** A local index spread back out over `positions`. */
  def scatter(local: Int, positions: Array[Int]): Int =
    var spread = 0
    var j = 0

    while j < positions.length do
      spread |= ((local >> j) & 1) << positions(j)
      j += 1

    spread

/** How much of a `2^k x 2^k` operator actually has to be stored, and moved.
  *
  * The three forms are a lattice under composition — a product is never cheaper than either factor,
  * and never more expensive than `Dense` — and they are not merely a storage optimisation. They are
  * three different datapaths:
  *
  *   - `Diagonal` needs no data movement at all. Every amplitude is scaled in place by one of `2^k`
  *     phases, so the kernel is a single streaming pass with no gather, no scatter and no shuffle
  *     network. In hardware this is a multiplier and a phase table.
  *   - `Monomial` (a permutation with phases) needs the shuffle network but no multiplier array:
  *     `2^k` amplitudes in, the same `2^k` amplitudes out, permuted and scaled.
  *   - `Dense` is the only form that needs a MAC array, and the only one whose storage grows as
  *     `4^k` rather than `2^k`.
  *
  * This distinction is why a dense-only engine is the wrong engine. Diagonal and permutation gates
  * are a large fraction of real circuits — `Z`, `S`, `T`, `RZ`, `PHASE`, `CZ`, `CPHASE`, every
  * multi-controlled `Z`, `X`, `Y`, `CNOT`, `CCNOT`, `SWAP`, `CSWAP` — and densifying them is pure
  * loss.
  */
enum BlockForm:
  case Diagonal, Monomial, Dense

/** A `2^k x 2^k` operator, stored in the tightest form it admits. */
sealed trait BlockOperator:
  def width: Int

  def form: BlockForm

  final def dimension: Int = 1 << width

  /** Complex entries actually stored. `2^k` for the cheap forms, `4^k` for dense. */
  def entries: Int

  def toDense: DenseOperator

  /** Every form can be read as a permutation with phases, except `Dense`. */
  def asMonomial: MonomialOperator

  /** This operator re-expressed over a wider support.
    *
    * `positions(j)` is the local bit in the wider support that corresponds to local bit `j` here;
    * `targetWidth` is the wider support's size. Expansion is linear in the new dimension for the
    * cheap forms and quadratic only for dense, which is most of the point.
    */
  def expand(positions: Array[Int], targetWidth: Int): BlockOperator

  /** `this` applied after `first`, i.e. the product `this * first`. */
  def after(first: BlockOperator): BlockOperator =
    require(
      first.width == width,
      s"Composition needs both operators over the same support, got $width and ${first.width}."
    )

    (this, first) match
      // A dense product is re-classified rather than assumed dense. Scanning 4^k entries is cheap
      // beside the 8^k multiply that produced them, and the pairs that collapse are not exotic:
      // H after H, and most Clifford cancellations, come back diagonal. Not doing this while the
      // monomial path simplifies would be inconsistent.
      case (second: DenseOperator, _) =>
        BlockOperator.classify(width, second.matrix.multiply(first.toDense.matrix))
      case (_, f: DenseOperator) =>
        BlockOperator.classify(width, toDense.matrix.multiply(f.matrix))
      case (second, f) =>
        // Both are permutations with phases, and so is the product. Doing it through the common
        // monomial reading rather than case by case is what keeps the composition rules honest:
        // there is one implementation, not nine.
        MonomialOperator.compose(second.asMonomial, f.asMonomial).simplified

/** `2^k` phases on the diagonal. */
final case class DiagonalOperator(width: Int, values: Array[Double]) extends BlockOperator:
  require(values.length == 2 * dimension, s"A diagonal operator over $width qubits needs $dimension entries.")

  val form: BlockForm = BlockForm.Diagonal

  def entries: Int = dimension

  def asMonomial: MonomialOperator =
    MonomialOperator(width, Array.tabulate(dimension)(identity), values)

  def toDense: DenseOperator =
    val d = dimension
    val flat = new Array[Double](2 * d * d)

    var i = 0
    while i < d do
      val entry = 2 * (i * d + i)

      flat(entry) = values(2 * i)
      flat(entry + 1) = values(2 * i + 1)
      i += 1

    DenseOperator(width, ComplexMatrix.fromInterleaved(d, d, flat))

  def expand(positions: Array[Int], targetWidth: Int): BlockOperator =
    val d = 1 << targetWidth
    val widened = new Array[Double](2 * d)

    var i = 0
    while i < d do
      val source = Bits.gather(i, positions)

      widened(2 * i) = values(2 * source)
      widened(2 * i + 1) = values(2 * source + 1)
      i += 1

    DiagonalOperator(targetWidth, widened)

/** A permutation with phases: column `j` maps to row `rows(j)` with coefficient `values(j)`. */
final case class MonomialOperator(width: Int, rows: Array[Int], values: Array[Double]) extends BlockOperator:
  require(rows.length == dimension, s"A monomial operator over $width qubits needs $dimension columns.")
  require(values.length == 2 * dimension, s"A monomial operator over $width qubits needs $dimension values.")

  val form: BlockForm = BlockForm.Monomial

  def entries: Int = dimension

  def asMonomial: MonomialOperator = this

  /** `Diagonal` if the permutation turns out to be the identity, otherwise unchanged. */
  def simplified: BlockOperator =
    var identity = true
    var j = 0

    while identity && j < dimension do
      if rows(j) != j then identity = false
      j += 1

    if identity then DiagonalOperator(width, values) else this

  def toDense: DenseOperator =
    val d = dimension
    val flat = new Array[Double](2 * d * d)

    var col = 0
    while col < d do
      val entry = 2 * (rows(col) * d + col)

      flat(entry) = values(2 * col)
      flat(entry + 1) = values(2 * col + 1)
      col += 1

    DenseOperator(width, ComplexMatrix.fromInterleaved(d, d, flat))

  def expand(positions: Array[Int], targetWidth: Int): BlockOperator =
    val d = 1 << targetWidth
    val spanned = positions.foldLeft(0)((mask, p) => mask | (1 << p))
    val untouched = (d - 1) & ~spanned
    val widenedRows = new Array[Int](d)
    val widenedValues = new Array[Double](2 * d)

    var col = 0
    while col < d do
      val source = Bits.gather(col, positions)

      // The untouched bits ride along unchanged; the supported ones permute as they did before.
      widenedRows(col) = (col & untouched) | Bits.scatter(rows(source), positions)
      widenedValues(2 * col) = values(2 * source)
      widenedValues(2 * col + 1) = values(2 * source + 1)
      col += 1

    MonomialOperator(targetWidth, widenedRows, widenedValues).simplified

object MonomialOperator:
  /** `second * first`, where column `j` goes through `first` and then `second`. */
  def compose(second: MonomialOperator, first: MonomialOperator): MonomialOperator =
    val d = first.dimension
    val rows = new Array[Int](d)
    val values = new Array[Double](2 * d)

    var col = 0
    while col < d do
      val intermediate = first.rows(col)
      val aRe = first.values(2 * col)
      val aIm = first.values(2 * col + 1)
      val bRe = second.values(2 * intermediate)
      val bIm = second.values(2 * intermediate + 1)

      rows(col) = second.rows(intermediate)
      values(2 * col) = bRe * aRe - bIm * aIm
      values(2 * col + 1) = bRe * aIm + bIm * aRe
      col += 1

    MonomialOperator(first.width, rows, values)

/** The general case: every one of `4^k` entries stored. */
final case class DenseOperator(width: Int, matrix: ComplexMatrix) extends BlockOperator:
  require(
    matrix.rows == dimension && matrix.cols == dimension,
    s"A dense operator over $width qubits needs a ${dimension}x$dimension matrix, " +
      s"not ${matrix.rows}x${matrix.cols}."
  )

  val form: BlockForm = BlockForm.Dense

  def entries: Int = dimension * dimension

  def toDense: DenseOperator = this

  def asMonomial: MonomialOperator =
    throw new UnsupportedOperationException("A dense operator is not a permutation with phases.")

  def expand(positions: Array[Int], targetWidth: Int): BlockOperator =
    val d = 1 << targetWidth
    val spanned = positions.foldLeft(0)((mask, p) => mask | (1 << p))
    val untouched = (d - 1) & ~spanned
    val flat = new Array[Double](2 * d * d)

    var row = 0
    while row < d do
      var col = 0
      while col < d do
        // An identity factor contributes nothing unless the untouched bits agree, so only the
        // block-diagonal entries are ever non-zero.
        if (row & untouched) == (col & untouched) then
          val r = Bits.gather(row, positions)
          val c = Bits.gather(col, positions)
          val entry = 2 * (row * d + col)

          flat(entry) = matrix.realAt(r, c)
          flat(entry + 1) = matrix.imagAt(r, c)
        col += 1
      row += 1

    DenseOperator(targetWidth, ComplexMatrix.fromInterleaved(d, d, flat))

object BlockOperator:
  /** Entries below this are treated as zero when deciding a matrix's form.
    *
    * This runs on an elementary gate's own matrix, where the zeros are exact, and again on every
    * dense product formed by [[BlockOperator.after]], where they are not. The tolerance covers both:
    * the gates whose zeros are computed rather than written, such as `RX(Math.PI)`, whose cosine is
    * 6e-17 rather than 0; and the cancellations that make a dense product cheap, such as H after H,
    * whose off-diagonal residue is around 1e-16. Dropping a term that small from a matrix whose
    * other entries are 1 is below the precision of the amplitudes it multiplies.
    */
  val Tolerance: Double = 1e-12

  /** The tightest form a matrix admits.
    *
    * Deciding this from the matrix rather than from a table of gate names is deliberate. A table
    * would have to be kept in step with the gate set by hand and would silently mislabel anything
    * user-defined; reading the structure cannot disagree with the semantics, and it classifies
    * `DefGate` and `Dagger` for free.
    */
  def classify(width: Int, matrix: ComplexMatrix, tolerance: Double = Tolerance): BlockOperator =
    val d = 1 << width
    val rows = new Array[Int](d)
    val values = new Array[Double](2 * d)
    var monomial = true
    var col = 0

    while monomial && col < d do
      var occupied = -1
      var row = 0

      while row < d do
        val re = matrix.realAt(row, col)
        val im = matrix.imagAt(row, col)

        if Math.abs(re) > tolerance || Math.abs(im) > tolerance then
          if occupied >= 0 then monomial = false
          else
            occupied = row
            values(2 * col) = re
            values(2 * col + 1) = im
        row += 1

      if occupied < 0 then monomial = false else rows(col) = occupied
      col += 1

    if monomial && rows.distinct.length == d then MonomialOperator(width, rows, values).simplified
    else DenseOperator(width, matrix)
