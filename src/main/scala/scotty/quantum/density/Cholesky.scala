package scotty.quantum.density

import scotty.ErrorMessage

/** Positive-definiteness by Cholesky factorisation.
  *
  * Deciding `M >= -tol I` does not need eigenvalues: it is exactly the question of whether
  * `M + tol I` admits a Cholesky factorisation. That is a single `n^3 / 3` pass with no iteration
  * and no convergence test, against the repeated sweeps of [[Eigen]] on a `2n x 2n` real embedding,
  * and it measures about ten times faster at the sizes this layer targets.
  *
  * Use [[Eigen]] when the eigenvalues themselves are wanted, and this when only the sign of the
  * smallest one is. The factorisation runs on local scratch arrays and reads its operands through
  * [[ComplexMatrix.realAt]] and [[ComplexMatrix.imagAt]], so it allocates nothing per entry;
  * nothing visible is mutated.
  */
object Cholesky:
  /** True when `matrix + shift * I` is positive definite.
    *
    * Equivalent to `Eigen.minEigenvalue(matrix) > -shift` up to round-off, which for a Hermitian
    * matrix of this size is around `1e-15` relative. A `shift` comfortably above that keeps the two
    * tests in agreement.
    */
  def isPositiveDefinite(matrix: ComplexMatrix, shift: Double): Boolean =
    require(matrix.isSquare, ErrorMessage.MatrixNotSquare)

    factorizes(matrix, matrix, 0d, shift)

  /** `Eigen.minEigenvalue(matrix) >= -tolerance`, decided without computing any eigenvalue. */
  def isPositiveSemidefinite(matrix: ComplexMatrix, tolerance: Double = 1e-10): Boolean =
    isPositiveDefinite(matrix, tolerance)

  /** True when `left - factor * right` is positive definite, to within `relativeTolerance` of the
    * largest magnitude appearing in that difference.
    *
    * The difference is never materialised. Building it as a [[ComplexMatrix]] and handing that to
    * [[isPositiveDefinite]] gives the same answer, but allocates two matrices of boxed complex
    * numbers per call, which measures about four times the cost of the factorisation itself. This
    * is the form the semidefinite-order searches ask for millions of times.
    */
  def differenceIsPositiveDefinite(
      left: ComplexMatrix,
      right: ComplexMatrix,
      factor: Double,
      relativeTolerance: Double
  ): Boolean =
    require(left.isSquare && right.isSquare, ErrorMessage.MatrixNotSquare)
    require(left.rows == right.rows, ErrorMessage.NotSameDimension)

    factorizes(left, right, factor, relativeTolerance * largestMagnitude(left, right, factor))

  /** Largest `|left(i, j) - factor * right(i, j)|`, floored away from zero. */
  private def largestMagnitude(left: ComplexMatrix, right: ComplexMatrix, factor: Double): Double =
    val n = left.rows

    var largest = 1e-300
    var row = 0

    while row < n do
      var col = 0

      while col < n do
        val dr = left.realAt(row, col) - factor * right.realAt(row, col)
        val di = left.imagAt(row, col) - factor * right.imagAt(row, col)
        val magnitude = Math.sqrt(dr * dr + di * di)

        if magnitude > largest then largest = magnitude
        col += 1

      row += 1

    largest

  /** Cholesky of `left - factor * right + shift * I`, reporting only whether it succeeds.
    *
    * Entries of the difference are formed as they are read, each exactly once, so the only
    * allocation is the lower triangle being built.
    */
  private def factorizes(
      left: ComplexMatrix,
      right: ComplexMatrix,
      factor: Double,
      shift: Double
  ): Boolean =
    val n = left.rows
    val re = new Array[Double](n * n)
    val im = new Array[Double](n * n)

    var row = 0
    var definite = true

    while row < n && definite do
      // The pivot is the diagonal of the difference minus the squared norm of the row of L so far.
      var pivotSquared = left.realAt(row, row) - factor * right.realAt(row, row) + shift
      var k = 0

      while k < row do
        val lr = re(row * n + k)
        val li = im(row * n + k)

        pivotSquared -= lr * lr + li * li
        k += 1

      if pivotSquared <= 0d then definite = false
      else
        val pivot = Math.sqrt(pivotSquared)

        re(row * n + row) = pivot

        var below = row + 1

        while below < n do
          var sumR = left.realAt(below, row) - factor * right.realAt(below, row)
          var sumI = left.imagAt(below, row) - factor * right.imagAt(below, row)
          var j = 0

          // Subtract L(below, j) * conj(L(row, j)).
          while j < row do
            val br = re(below * n + j)
            val bi = im(below * n + j)
            val rr = re(row * n + j)
            val ri = im(row * n + j)

            sumR -= br * rr + bi * ri
            sumI -= bi * rr - br * ri
            j += 1

          re(below * n + row) = sumR / pivot
          im(below * n + row) = sumI / pivot
          below += 1

        row += 1

    definite
