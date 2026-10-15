package scotty.quantum.density

import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random
import scotty.quantum.math.Complex

class NullSpaceSpec extends AnyFlatSpec:
  private val random = new Random(20261015L)
  private val tolerance = 1e-9

  /** Largest `|matrix x|` over the returned basis: zero when every vector is really in the kernel. */
  private def residual(matrix: ComplexMatrix, basis: Vector[Vector[Complex]]): Double =
    basis.foldLeft(0d): (worst, vector) =>
      val column = ComplexMatrix(vector.length, 1, vector)
      val image = matrix.multiply(column)

      Math.max(worst, image.data.foldLeft(0d)((m, c) => Math.max(m, Complex.abs(c))))

  private def complexEntry(): Complex = Complex(random.nextDouble() * 2 - 1, random.nextDouble() * 2 - 1)

  "basis" should "be empty for an invertible matrix" in {
    for dimension <- 1 to 6 do
      assert(NullSpace.basis(ComplexMatrix.identity(dimension)).isEmpty)
  }

  it should "span everything for the zero matrix" in {
    for dimension <- 1 to 5 do
      val basis = NullSpace.basis(ComplexMatrix.zero(dimension, dimension))

      assert(basis.length == dimension, s"dimension $dimension")
      assert(residual(ComplexMatrix.zero(dimension, dimension), basis) <= tolerance)
  }

  it should "find the one-dimensional kernel of a rank-deficient matrix" in {
    // Rows [1, 1] and [2, 2]: the kernel is spanned by (1, -1).
    val m = ComplexMatrix.fromRows(
      Seq(Seq(Complex(1), Complex(1)), Seq(Complex(2), Complex(2)))
    )
    val basis = NullSpace.basis(m)

    assert(basis.length == 1)
    assert(residual(m, basis) <= tolerance)

    val v = basis.head
    assert(Math.abs(v(0).r + v(1).r) <= tolerance, v.toString)
  }

  it should "handle a complex kernel" in {
    // Rows [1, -i] and [i, 1] are proportional, so the kernel is spanned by (i, 1).
    val m = ComplexMatrix.fromRows(
      Seq(Seq(Complex(1), Complex(0, -1)), Seq(Complex(0, 1), Complex(1)))
    )
    val basis = NullSpace.basis(m)

    assert(basis.length == 1)
    assert(residual(m, basis) <= tolerance)
  }

  it should "report the right dimension for a projector of known rank" in {
    // A rank-r diagonal projector has a kernel of dimension d - r.
    for dimension <- 2 to 6; rank <- 0 to dimension do
      val m = ComplexMatrix(
        dimension,
        dimension,
        Vector.tabulate(dimension * dimension) { k =>
          val (row, col) = (k / dimension, k % dimension)

          if row == col && row < rank then Complex(1) else Complex(0)
        }
      )
      val basis = NullSpace.basis(m)

      withClue(s"dimension $dimension, rank $rank: ") {
        assert(basis.length == dimension - rank)
        assert(residual(m, basis) <= tolerance)
      }
  }

  it should "return vectors that really are annihilated, for random low-rank matrices" in {
    for dimension <- 3 to 6; rank <- 1 until dimension do
      // Build an explicitly rank-deficient matrix as a sum of `rank` outer products.
      val m = (0 until rank)
        .map { _ =>
          val left = ComplexMatrix.column(Vector.fill(dimension)(complexEntry()))
          val right = ComplexMatrix.column(Vector.fill(dimension)(complexEntry()))

          ComplexMatrix.outer(left, right)
        }
        .reduceLeft(_.plus(_))

      val basis = NullSpace.basis(m)

      withClue(s"dimension $dimension, rank $rank: ") {
        assert(basis.length >= dimension - rank)
        assert(residual(m, basis) <= 1e-8)
      }
  }

  it should "survive a matrix whose entries span many orders of magnitude" in {
    val m = ComplexMatrix.fromRows(
      Seq(Seq(Complex(1e8), Complex(1e8)), Seq(Complex(1e-8), Complex(1e-8)))
    )
    val basis = NullSpace.basis(m)

    assert(basis.length == 1)
    // The tolerance is relative to the largest magnitude, so the residual scales with it too.
    assert(residual(m, basis) <= 1e-6 * 1e8)
  }

  it should "work for non-square matrices in both orientations" in {
    val wide = ComplexMatrix.fromRows(Seq(Seq(Complex(1), Complex(1), Complex(1))))
    val tall = ComplexMatrix.fromRows(
      Seq(Seq(Complex(1), Complex(0)), Seq(Complex(2), Complex(0)), Seq(Complex(3), Complex(0)))
    )

    assert(NullSpace.basis(wide).length == 2)
    assert(residual(wide, NullSpace.basis(wide)) <= tolerance)
    assert(NullSpace.basis(tall).length == 1)
    assert(residual(tall, NullSpace.basis(tall)) <= tolerance)
  }
