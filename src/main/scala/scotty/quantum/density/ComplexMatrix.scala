package scotty.quantum.density

import scotty.ErrorMessage
import scotty.quantum.QuantumContext.{Matrix, Vector as RawVector}
import scotty.quantum.math.Complex

/** An immutable dense complex matrix stored row-major as [[scotty.quantum.math.Complex]] values.
  *
  * This is the workhorse of the density-matrix layer. It is deliberately rectangular so that Kraus
  * operators changing the output dimension (the erasure channel, for instance) can be represented
  * directly. Nothing here mutates, and nothing here touches the state-vector simulator.
  */
final case class ComplexMatrix(rows: Int, cols: Int, data: Vector[Complex]):
  require(rows > 0 && cols > 0, ErrorMessage.NotSameDimension)
  require(data.length == rows * cols, ErrorMessage.NotSameDimension)

  def apply(row: Int, col: Int): Complex = data(row * cols + col)

  def isSquare: Boolean = rows == cols

  def multiply(that: ComplexMatrix): ComplexMatrix =
    require(cols == that.rows, ErrorMessage.NotSameDimension)

    ComplexMatrix(
      rows,
      that.cols,
      Vector.tabulate(rows * that.cols): idx =>
        val row = idx / that.cols
        val col = idx % that.cols
        val (re, im) = (0 until cols).foldLeft((0d, 0d)):
          case ((sumR, sumI), k) =>
            val a = apply(row, k)
            val b = that(k, col)

            (sumR + a.r * b.r - a.i * b.i, sumI + a.r * b.i + a.i * b.r)

        Complex(re, im)
    )

  def adjoint: ComplexMatrix =
    ComplexMatrix(
      cols,
      rows,
      Vector.tabulate(cols * rows): idx =>
        val c = apply(idx % rows, idx / rows)

        Complex(c.r, -c.i)
    )

  def transpose: ComplexMatrix =
    ComplexMatrix(cols, rows, Vector.tabulate(cols * rows)(idx => apply(idx % rows, idx / rows)))

  def conjugate: ComplexMatrix = ComplexMatrix(rows, cols, data.map(c => Complex(c.r, -c.i)))

  /** Kronecker product `this (x) that`. */
  def tensor(that: ComplexMatrix): ComplexMatrix =
    val newRows = rows * that.rows
    val newCols = cols * that.cols

    ComplexMatrix(
      newRows,
      newCols,
      Vector.tabulate(newRows * newCols): idx =>
        val row = idx / newCols
        val col = idx % newCols
        val a = apply(row / that.rows, col / that.cols)
        val b = that(row % that.rows, col % that.cols)

        Complex(a.r * b.r - a.i * b.i, a.r * b.i + a.i * b.r)
    )

  def plus(that: ComplexMatrix): ComplexMatrix =
    require(rows == that.rows && cols == that.cols, ErrorMessage.NotSameDimension)

    ComplexMatrix(rows, cols, data.lazyZip(that.data).map((a, b) => Complex(a.r + b.r, a.i + b.i)).toVector)

  def minus(that: ComplexMatrix): ComplexMatrix = plus(that.scale(-1d))

  def scale(factor: Double): ComplexMatrix =
    ComplexMatrix(rows, cols, data.map(c => Complex(factor * c.r, factor * c.i)))

  def scale(factor: Complex): ComplexMatrix =
    ComplexMatrix(
      rows,
      cols,
      data.map(c => Complex(factor.r * c.r - factor.i * c.i, factor.r * c.i + factor.i * c.r))
    )

  def trace: Complex =
    require(isSquare, ErrorMessage.MatrixNotSquare)

    val (re, im) = (0 until rows).foldLeft((0d, 0d)):
      case ((sumR, sumI), i) =>
        val c = apply(i, i)

        (sumR + c.r, sumI + c.i)

    Complex(re, im)

  def isHermitian(tolerance: Double = ComplexMatrix.Tolerance): Boolean =
    isSquare && (0 until rows).forall: row =>
      (0 until cols).forall: col =>
        val a = apply(row, col)
        val b = apply(col, row)

        Math.abs(a.r - b.r) <= tolerance && Math.abs(a.i + b.i) <= tolerance

  def maxAbsDifference(that: ComplexMatrix): Double =
    require(rows == that.rows && cols == that.cols, ErrorMessage.NotSameDimension)

    data.lazyZip(that.data).map((a, b) => Complex.abs(a.r - b.r, a.i - b.i)).max

  def approximatelyEquals(that: ComplexMatrix, tolerance: Double = ComplexMatrix.Tolerance): Boolean =
    rows == that.rows && cols == that.cols && maxAbsDifference(that) <= tolerance

  /** Back to the interleaved `(re, im, ...)` layout used by the state-vector simulator. */
  def toRaw: Matrix = Array.tabulate(rows, 2 * cols): (row, idx) =>
    val c = apply(row, idx / 2)

    if idx % 2 == 0 then c.r else c.i

  def toRows: Vector[Vector[Complex]] = Vector.tabulate(rows, cols)(apply)

  def toHumanString: String = toRows.map(_.map(_.toHumanString).mkString("  ")).mkString("\n")

object ComplexMatrix:
  val Tolerance: Double = 1e-9

  def zero(rows: Int, cols: Int): ComplexMatrix =
    ComplexMatrix(rows, cols, Vector.fill(rows * cols)(Complex(0)))

  def identity(dimension: Int): ComplexMatrix =
    ComplexMatrix(
      dimension,
      dimension,
      Vector.tabulate(dimension * dimension)(idx =>
        if idx / dimension == idx % dimension then Complex(1) else Complex(0)
      )
    )

  def fromRows(rows: Seq[Seq[Complex]]): ComplexMatrix =
    require(rows.nonEmpty && rows.forall(_.length == rows.head.length), ErrorMessage.NotSameDimension)

    ComplexMatrix(rows.length, rows.head.length, rows.flatten.toVector)

  /** Reads the interleaved `(re, im, ...)` layout produced by Scotty gates. */
  def fromRaw(matrix: Matrix): ComplexMatrix =
    require(matrix.nonEmpty, ErrorMessage.NotSameDimension)

    val cols = matrix(0).length / 2

    ComplexMatrix(
      matrix.length,
      cols,
      Vector.tabulate(matrix.length * cols): idx =>
        val row = matrix(idx / cols)
        val col = idx % cols

        Complex(row(2 * col), row(2 * col + 1))
    )

  /** Column vector from the interleaved state-vector layout. */
  def columnFromRaw(vector: RawVector): ComplexMatrix =
    val dimension = vector.length / 2

    ComplexMatrix(dimension, 1, Vector.tabulate(dimension)(i => Complex(vector(2 * i), vector(2 * i + 1))))

  def column(amplitudes: Seq[Complex]): ComplexMatrix =
    ComplexMatrix(amplitudes.length, 1, amplitudes.toVector)

  /** Outer product `|left><right|`. */
  def outer(left: ComplexMatrix, right: ComplexMatrix): ComplexMatrix = left.multiply(right.adjoint)

  def sum(matrices: Seq[ComplexMatrix]): ComplexMatrix =
    require(matrices.nonEmpty, ErrorMessage.NotSameDimension)

    matrices.reduceLeft(_.plus(_))
