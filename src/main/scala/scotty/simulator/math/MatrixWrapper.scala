package scotty.simulator.math

import scotty.ErrorMessage
import scotty.quantum.QuantumContext.{Matrix, Vector}
import scotty.quantum.math.Complex
import scotty.quantum.math.MathUtils.*

/** Pure linear-algebra helpers for the interleaved complex [[Matrix]] layout.
  *
  * A matrix of dimension `n` is an `n`-element array of rows, each row holding `2 * n` doubles as
  * `(re, im, re, im, ...)`. All functions here are referentially transparent and never mutate their
  * arguments.
  */
object MatrixWrapper:
  def isUnitary(matrix: Matrix): Boolean =
    areEqual(product(conjugateTranspose(matrix), matrix), identity(matrix.length))

  def isSquare(matrix: Matrix): Boolean =
    if matrix.length == 0 then true else matrix.length == matrix(0).length / 2

  def areSameDimension(m1: Matrix, m2: Matrix): Boolean =
    if m1.length == 0 && m2.length == 0 then true
    else m1.length == m2.length && m1(0).length == m2(0).length

  def areSameDimension(matrix: Matrix, vector: Vector): Boolean =
    matrix.length == 0 || matrix.length * 2 == vector.length

  def areEqual(m1: Matrix, m2: Matrix): Boolean =
    require(isSquare(m1) && isSquare(m2), ErrorMessage.MatrixNotSquare)
    require(areSameDimension(m1, m2), ErrorMessage.NotSameDimension)

    m1.indices.forall: row =>
      m1.indices.forall: col =>
        (m1(row)(2 * col) ~= m2(row)(2 * col)) && (m1(row)(2 * col + 1) ~= m2(row)(2 * col + 1))

  def identity(dimension: Int): Matrix =
    Array.tabulate(dimension, dimension * 2): (row, idx) =>
      if idx == row * 2 then 1d else 0d

  def product(m1: Matrix, m2: Matrix): Matrix =
    require(isSquare(m1) && isSquare(m2), ErrorMessage.MatrixNotSquare)
    require(areSameDimension(m1, m2), ErrorMessage.NotSameDimension)

    val n = m1.length
    val result = Array.fill(n)(Array.fill(n * 2)(0d))

    for row <- 0 until n; col <- 0 until n do
      var sumR = 0d
      var sumI = 0d

      for k <- 0 until n do
        val (r, i) = Complex.product(m1(row)(2 * k), m1(row)(2 * k + 1), m2(k)(2 * col), m2(k)(2 * col + 1))

        sumR += r
        sumI += i

      result(row)(2 * col) = sumR
      result(row)(2 * col + 1) = sumI

    result

  def product(matrix: Matrix, vector: Vector): Vector =
    require(isSquare(matrix), ErrorMessage.MatrixNotSquare)
    require(areSameDimension(matrix, vector), ErrorMessage.NotSameDimension)

    val n = matrix.length
    val result = Array.fill(vector.length)(0d)

    for row <- 0 until n do
      var sumR = 0d
      var sumI = 0d

      for k <- 0 until n do
        val (r, i) = Complex.product(matrix(row)(2 * k), matrix(row)(2 * k + 1), vector(2 * k), vector(2 * k + 1))

        sumR += r
        sumI += i

      result(2 * row) = sumR
      result(2 * row + 1) = sumI

    result

  /** Complex conjugate transpose.
    *
    * Scotty 0.6 transposed in place and only produced the correct result for 2x2 matrices; this
    * version allocates a fresh matrix and is correct for every dimension.
    */
  def conjugateTranspose(matrix: Matrix): Matrix =
    require(isSquare(matrix), ErrorMessage.MatrixNotSquare)

    val n = matrix.length

    Array.tabulate(n, 2 * n): (row, idx) =>
      val col = idx / 2

      if idx % 2 == 0 then matrix(col)(2 * row) else -matrix(col)(2 * row + 1)
