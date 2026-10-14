package scotty.quantum.density

import scotty.ErrorMessage

/** Eigenvalues of Hermitian matrices by the cyclic Jacobi rotation method.
  *
  * A `d x d` Hermitian matrix `H = A + iB` is embedded in the `2d x 2d` real symmetric matrix
  * `[[A, -B], [B, A]]`, whose spectrum is that of `H` with every eigenvalue doubled. That avoids a
  * complex Jacobi implementation and is more than fast enough for the dimensions this layer targets
  * (at most 16, i.e. a 32x32 real sweep).
  *
  * The rotations run on a local scratch array; nothing visible from the outside is mutated.
  */
object Eigen:
  val Tolerance: Double = 1e-14
  val MaxSweeps: Int = 100

  /** Ascending eigenvalues of a Hermitian matrix. */
  def hermitianEigenvalues(matrix: ComplexMatrix): Vector[Double] =
    require(matrix.isSquare, ErrorMessage.MatrixNotSquare)

    val d = matrix.rows
    val embedded = Array.tabulate(2 * d, 2 * d): (row, col) =>
      val c = matrix(row % d, col % d)

      (row / d, col / d) match
        case (0, 0) => c.r
        case (1, 1) => c.r
        case (0, 1) => -c.i
        case _      => c.i

    val doubled = jacobi(embedded).sorted

    // Each eigenvalue of H appears twice in the real embedding; keep one of each pair.
    Vector.tabulate(d)(i => (doubled(2 * i) + doubled(2 * i + 1)) / 2)

  def minEigenvalue(matrix: ComplexMatrix): Double = hermitianEigenvalues(matrix).head

  def maxEigenvalue(matrix: ComplexMatrix): Double = hermitianEigenvalues(matrix).last

  /** Positive semidefiniteness up to `tolerance`, which absorbs Jacobi round-off. */
  def isPositiveSemidefinite(matrix: ComplexMatrix, tolerance: Double = 1e-10): Boolean =
    minEigenvalue(matrix) >= -tolerance

  private def jacobi(input: Array[Array[Double]]): Array[Double] =
    val n = input.length
    val a = input.map(_.clone)

    var sweep = 0

    while sweep < MaxSweeps && offDiagonalNorm(a) > Tolerance do
      for p <- 0 until n - 1; q <- p + 1 until n do
        if Math.abs(a(p)(q)) > Double.MinPositiveValue then rotate(a, p, q)

      sweep += 1

    Array.tabulate(n)(i => a(i)(i))

  private def rotate(a: Array[Array[Double]], p: Int, q: Int): Unit =
    val apq = a(p)(q)
    val app = a(p)(p)
    val aqq = a(q)(q)

    val theta = (aqq - app) / (2 * apq)
    val t =
      if theta == 0d then 1d
      else Math.signum(theta) / (Math.abs(theta) + Math.sqrt(theta * theta + 1))
    val c = 1 / Math.sqrt(t * t + 1)
    val s = t * c

    a(p)(p) = app - t * apq
    a(q)(q) = aqq + t * apq
    a(p)(q) = 0d
    a(q)(p) = 0d

    for r <- a.indices if r != p && r != q do
      val arp = a(r)(p)
      val arq = a(r)(q)

      a(r)(p) = c * arp - s * arq
      a(p)(r) = a(r)(p)
      a(r)(q) = s * arp + c * arq
      a(q)(r) = a(r)(q)

  private def offDiagonalNorm(a: Array[Array[Double]]): Double =
    val n = a.length

    (0 until n - 1).foldLeft(0d): (sum, p) =>
      sum + (p + 1 until n).foldLeft(0d)((inner, q) => inner + a(p)(q) * a(p)(q))
