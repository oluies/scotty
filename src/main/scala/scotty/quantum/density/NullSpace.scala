package scotty.quantum.density

import scotty.quantum.math.Complex

/** A basis for the kernel of a complex matrix, by Gauss-Jordan elimination with partial pivoting.
  *
  * This is what turns a fixed-point condition into something solvable. Deutsch's self-consistency
  * condition for a closed timelike curve is `rho = M[rho]` for a linear `M`, i.e. `(M - I)rho = 0`,
  * and iterating `M` does not generally converge to a solution: for the grandfather paradox the
  * iteration has period two and never settles. The kernel, on the other hand, is exact, and its
  * dimension says directly whether the solution is unique.
  *
  * The elimination runs on local scratch arrays; nothing visible is mutated.
  */
object NullSpace:
  /** Basis of `{ x : matrix x = 0 }`, as column vectors. Empty when the kernel is trivial.
    *
    * `tolerance` is relative to the largest magnitude in the matrix, so it scales with the data.
    */
  def basis(matrix: ComplexMatrix, tolerance: Double = 1e-9): Vector[Vector[Complex]] =
    val rows = matrix.rows
    val cols = matrix.cols
    val scale = matrix.data.foldLeft(1e-300)((m, c) => Math.max(m, Complex.abs(c)))
    val zero = tolerance * scale

    // Row-major scratch copy, reduced in place to row echelon form.
    val a = Array.tabulate(rows * cols)(i => matrix(i / cols, i % cols))
    val pivotOfRow = Array.fill(rows)(-1)

    var row = 0
    var col = 0

    while row < rows && col < cols do
      // Partial pivoting: the largest magnitude in this column at or below the current row.
      var best = row
      var bestMagnitude = Complex.abs(a(row * cols + col))
      var probe = row + 1

      while probe < rows do
        val magnitude = Complex.abs(a(probe * cols + col))

        if magnitude > bestMagnitude then
          best = probe
          bestMagnitude = magnitude

        probe += 1

      if bestMagnitude <= zero then
        // No usable pivot in this column, so it belongs to a free variable.
        col += 1
      else
        if best != row then swapRows(a, cols, row, best)

        divideRow(a, cols, row, a(row * cols + col))
        pivotOfRow(row) = col

        var other = 0

        while other < rows do
          if other != row then
            val factor = a(other * cols + col)

            if Complex.abs(factor) > 0d then eliminate(a, cols, other, row, factor)

          other += 1

        row += 1
        col += 1

    val pivotColumns = pivotOfRow.filter(_ >= 0).toSet
    val freeColumns = (0 until cols).filterNot(pivotColumns.contains)

    // Each free column gives one basis vector: set it to one, read the pivots off the reduced rows.
    freeColumns.toVector.map: free =>
      Vector.tabulate(cols): component =>
        if component == free then Complex(1)
        else
          pivotOfRow.indexOf(component) match
            case -1 => Complex(0)
            case r  => Complex(-a(r * cols + free).r, -a(r * cols + free).i)

  private def swapRows(a: Array[Complex], cols: Int, left: Int, right: Int): Unit =
    var c = 0

    while c < cols do
      val tmp = a(left * cols + c)

      a(left * cols + c) = a(right * cols + c)
      a(right * cols + c) = tmp
      c += 1

  private def divideRow(a: Array[Complex], cols: Int, row: Int, pivot: Complex): Unit =
    val denominator = pivot.r * pivot.r + pivot.i * pivot.i
    var c = 0

    while c < cols do
      val v = a(row * cols + c)

      a(row * cols + c) =
        Complex((v.r * pivot.r + v.i * pivot.i) / denominator, (v.i * pivot.r - v.r * pivot.i) / denominator)
      c += 1

  private def eliminate(a: Array[Complex], cols: Int, target: Int, source: Int, factor: Complex): Unit =
    var c = 0

    while c < cols do
      val t = a(target * cols + c)
      val s = a(source * cols + c)

      a(target * cols + c) =
        Complex(t.r - (factor.r * s.r - factor.i * s.i), t.i - (factor.r * s.i + factor.i * s.r))
      c += 1
