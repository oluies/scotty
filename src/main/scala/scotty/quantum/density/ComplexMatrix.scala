package scotty.quantum.density

import scotty.ErrorMessage
import scotty.quantum.QuantumContext.{Matrix, Vector as RawVector}
import scotty.quantum.math.Complex

/** An immutable dense complex matrix, stored row-major as interleaved `(re, im)` doubles.
  *
  * This is the workhorse of the density-matrix layer. It is deliberately rectangular so that Kraus
  * operators changing the output dimension (the erasure channel, for instance) can be represented
  * directly. Nothing here mutates, and nothing here touches the state-vector simulator.
  *
  * The storage is an [[IArray]] of primitives rather than a `Vector[Complex]`. The entries are
  * read in tight loops by [[Cholesky]], [[Eigen]] and the matrix operations below, and a boxed
  * representation makes those loops about four times slower; a 6x6 positive-definiteness test
  * measures 0.181 microseconds against boxed values and 0.046 against primitives. The public API
  * still speaks in [[Complex]] — [[apply]] builds one on demand — while anything performance
  * sensitive goes through [[realAt]] and [[imagAt]] and never allocates.
  */
final class ComplexMatrix private (val rows: Int, val cols: Int, private val values: IArray[Double]):
  /** Real part of the entry at `(row, col)`, without allocating. */
  def realAt(row: Int, col: Int): Double = values(2 * (row * cols + col))

  /** Imaginary part of the entry at `(row, col)`, without allocating. */
  def imagAt(row: Int, col: Int): Double = values(2 * (row * cols + col) + 1)

  def apply(row: Int, col: Int): Complex =
    val i = 2 * (row * cols + col)

    Complex(values(i), values(i + 1))

  /** Every entry in row-major order. Convenient, but it allocates; prefer [[realAt]]/[[imagAt]] in
    * a loop.
    */
  def data: Vector[Complex] = Vector.tabulate(rows * cols)(k => Complex(values(2 * k), values(2 * k + 1)))

  def isSquare: Boolean = rows == cols

  def multiply(that: ComplexMatrix): ComplexMatrix =
    require(cols == that.rows, ErrorMessage.NotSameDimension)

    val out = new Array[Double](rows * that.cols * 2)
    var row = 0

    while row < rows do
      var col = 0

      while col < that.cols do
        var sumR = 0d
        var sumI = 0d
        var k = 0

        while k < cols do
          val ar = realAt(row, k)
          val ai = imagAt(row, k)
          val br = that.realAt(k, col)
          val bi = that.imagAt(k, col)

          sumR += ar * br - ai * bi
          sumI += ar * bi + ai * br
          k += 1

        val i = 2 * (row * that.cols + col)

        out(i) = sumR
        out(i + 1) = sumI
        col += 1

      row += 1

    ComplexMatrix.ofValues(rows, that.cols, out)

  def adjoint: ComplexMatrix =
    val out = new Array[Double](rows * cols * 2)
    var row = 0

    while row < rows do
      var col = 0

      while col < cols do
        val i = 2 * (col * rows + row)

        out(i) = realAt(row, col)
        out(i + 1) = -imagAt(row, col)
        col += 1

      row += 1

    ComplexMatrix.ofValues(cols, rows, out)

  def transpose: ComplexMatrix =
    val out = new Array[Double](rows * cols * 2)
    var row = 0

    while row < rows do
      var col = 0

      while col < cols do
        val i = 2 * (col * rows + row)

        out(i) = realAt(row, col)
        out(i + 1) = imagAt(row, col)
        col += 1

      row += 1

    ComplexMatrix.ofValues(cols, rows, out)

  def conjugate: ComplexMatrix =
    ComplexMatrix.ofValues(rows, cols, mapValues((re, im) => (re, -im)))

  /** Kronecker product `this (x) that`. */
  def tensor(that: ComplexMatrix): ComplexMatrix =
    val newRows = rows * that.rows
    val newCols = cols * that.cols
    val out = new Array[Double](newRows * newCols * 2)
    var row = 0

    while row < newRows do
      var col = 0

      while col < newCols do
        val ar = realAt(row / that.rows, col / that.cols)
        val ai = imagAt(row / that.rows, col / that.cols)
        val br = that.realAt(row % that.rows, col % that.cols)
        val bi = that.imagAt(row % that.rows, col % that.cols)
        val i = 2 * (row * newCols + col)

        out(i) = ar * br - ai * bi
        out(i + 1) = ar * bi + ai * br
        col += 1

      row += 1

    ComplexMatrix.ofValues(newRows, newCols, out)

  def plus(that: ComplexMatrix): ComplexMatrix =
    requireSameShape(that)

    ComplexMatrix.ofValues(rows, cols, zipValues(that, (a, b) => a + b))

  def minus(that: ComplexMatrix): ComplexMatrix =
    requireSameShape(that)

    ComplexMatrix.ofValues(rows, cols, zipValues(that, (a, b) => a - b))

  def scale(factor: Double): ComplexMatrix =
    ComplexMatrix.ofValues(rows, cols, mapValues((re, im) => (factor * re, factor * im)))

  def scale(factor: Complex): ComplexMatrix =
    ComplexMatrix.ofValues(
      rows,
      cols,
      mapValues((re, im) => (factor.r * re - factor.i * im, factor.r * im + factor.i * re))
    )

  def trace: Complex =
    require(isSquare, ErrorMessage.MatrixNotSquare)

    var sumR = 0d
    var sumI = 0d
    var i = 0

    while i < rows do
      sumR += realAt(i, i)
      sumI += imagAt(i, i)
      i += 1

    Complex(sumR, sumI)

  def isHermitian(tolerance: Double = ComplexMatrix.Tolerance): Boolean =
    isSquare && {
      var row = 0
      var hermitian = true

      while row < rows && hermitian do
        var col = 0

        while col < cols && hermitian do
          if Math.abs(realAt(row, col) - realAt(col, row)) > tolerance ||
            Math.abs(imagAt(row, col) + imagAt(col, row)) > tolerance
          then hermitian = false

          col += 1

        row += 1

      hermitian
    }

  def maxAbsDifference(that: ComplexMatrix): Double =
    requireSameShape(that)

    var worst = 0d
    var k = 0

    while k < rows * cols do
      val dr = values(2 * k) - that.values(2 * k)
      val di = values(2 * k + 1) - that.values(2 * k + 1)
      val magnitude = Math.sqrt(dr * dr + di * di)

      if magnitude > worst then worst = magnitude
      k += 1

    worst

  def approximatelyEquals(that: ComplexMatrix, tolerance: Double = ComplexMatrix.Tolerance): Boolean =
    rows == that.rows && cols == that.cols && maxAbsDifference(that) <= tolerance

  /** Back to the interleaved `(re, im, ...)` layout used by the state-vector simulator. */
  def toRaw: Matrix = Array.tabulate(rows, 2 * cols): (row, idx) =>
    if idx % 2 == 0 then realAt(row, idx / 2) else imagAt(row, idx / 2)

  def toRows: Vector[Vector[Complex]] = Vector.tabulate(rows, cols)(apply)

  def toHumanString: String = toRows.map(_.map(_.toHumanString).mkString("  ")).mkString("\n")

  override def toString: String = s"ComplexMatrix(${rows}x$cols)"

  override def equals(other: Any): Boolean = other match
    case that: ComplexMatrix =>
      rows == that.rows && cols == that.cols && {
        var k = 0
        var same = true

        while k < values.length && same do
          if values(k) != that.values(k) then same = false
          k += 1

        same
      }
    case _ => false

  override def hashCode: Int =
    var result = 31 * rows + cols
    var k = 0

    while k < values.length do
      result = 31 * result + java.lang.Double.hashCode(values(k))
      k += 1

    result

  private def requireSameShape(that: ComplexMatrix): Unit =
    require(rows == that.rows && cols == that.cols, ErrorMessage.NotSameDimension)

  private inline def mapValues(inline f: (Double, Double) => (Double, Double)): Array[Double] =
    val out = new Array[Double](values.length)
    var k = 0

    while k < rows * cols do
      val (re, im) = f(values(2 * k), values(2 * k + 1))

      out(2 * k) = re
      out(2 * k + 1) = im
      k += 1

    out

  private inline def zipValues(that: ComplexMatrix, inline f: (Double, Double) => Double): Array[Double] =
    val out = new Array[Double](values.length)
    var k = 0

    while k < values.length do
      out(k) = f(values(k), that.values(k))
      k += 1

    out

object ComplexMatrix:
  val Tolerance: Double = 1e-9

  /** Takes ownership of `values`; callers must not retain or mutate it afterwards. */
  private[density] def ofValues(rows: Int, cols: Int, values: Array[Double]): ComplexMatrix =
    new ComplexMatrix(rows, cols, IArray.unsafeFromArray(values))

  def apply(rows: Int, cols: Int, data: Vector[Complex]): ComplexMatrix =
    require(rows > 0 && cols > 0, ErrorMessage.NotSameDimension)
    require(data.length == rows * cols, ErrorMessage.NotSameDimension)

    val values = new Array[Double](rows * cols * 2)
    var k = 0

    while k < data.length do
      val c = data(k)

      values(2 * k) = c.r
      values(2 * k + 1) = c.i
      k += 1

    ofValues(rows, cols, values)

  /** Row-major from interleaved `(re, im)` doubles, the representation used internally. */
  def fromInterleaved(rows: Int, cols: Int, values: Array[Double]): ComplexMatrix =
    require(rows > 0 && cols > 0, ErrorMessage.NotSameDimension)
    require(values.length == rows * cols * 2, ErrorMessage.NotSameDimension)

    ofValues(rows, cols, values.clone)

  def zero(rows: Int, cols: Int): ComplexMatrix = ofValues(rows, cols, new Array[Double](rows * cols * 2))

  def identity(dimension: Int): ComplexMatrix =
    val values = new Array[Double](dimension * dimension * 2)
    var i = 0

    while i < dimension do
      values(2 * (i * dimension + i)) = 1d
      i += 1

    ofValues(dimension, dimension, values)

  def fromRows(rows: Seq[Seq[Complex]]): ComplexMatrix =
    require(rows.nonEmpty && rows.forall(_.length == rows.head.length), ErrorMessage.NotSameDimension)

    ComplexMatrix(rows.length, rows.head.length, rows.flatten.toVector)

  /** Reads the interleaved `(re, im, ...)` layout produced by Scotty gates. */
  def fromRaw(matrix: Matrix): ComplexMatrix =
    require(matrix.nonEmpty, ErrorMessage.NotSameDimension)

    val cols = matrix(0).length / 2
    val values = new Array[Double](matrix.length * cols * 2)
    var row = 0

    while row < matrix.length do
      System.arraycopy(matrix(row), 0, values, row * cols * 2, cols * 2)
      row += 1

    ofValues(matrix.length, cols, values)

  /** Column vector from the interleaved state-vector layout. */
  def columnFromRaw(vector: RawVector): ComplexMatrix =
    ofValues(vector.length / 2, 1, vector.clone)

  def column(amplitudes: Seq[Complex]): ComplexMatrix =
    ComplexMatrix(amplitudes.length, 1, amplitudes.toVector)

  /** Outer product `|left><right|`. */
  def outer(left: ComplexMatrix, right: ComplexMatrix): ComplexMatrix = left.multiply(right.adjoint)

  def sum(matrices: Seq[ComplexMatrix]): ComplexMatrix =
    require(matrices.nonEmpty, ErrorMessage.NotSameDimension)

    matrices.reduceLeft(_.plus(_))
