package scotty.quantum.density

import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random
import scotty.quantum.gate.StandardGate.{X, Y, Z}
import scotty.quantum.math.Complex

class CholeskySpec extends AnyFlatSpec:
  private val random = new Random(20260615L)

  /** A random Hermitian matrix with entries of order one. */
  private def hermitian(dimension: Int): ComplexMatrix =
    val raw = ComplexMatrix(
      dimension,
      dimension,
      Vector.fill(dimension * dimension)(Complex(random.nextDouble() * 2 - 1, random.nextDouble() * 2 - 1))
    )

    raw.plus(raw.adjoint).scale(0.5)

  /** A random positive semidefinite matrix of the given rank. */
  private def positive(dimension: Int, rank: Int): ComplexMatrix =
    (0 until rank)
      .map { _ =>
        val column = ComplexMatrix.column(
          Vector.fill(dimension)(Complex(random.nextDouble() * 2 - 1, random.nextDouble() * 2 - 1))
        )

        ComplexMatrix.outer(column, column)
      }
      .reduceLeft(_.plus(_))

  "isPositiveDefinite" should "accept the identity and reject its negation" in {
    for dimension <- 1 to 8 do
      assert(Cholesky.isPositiveDefinite(ComplexMatrix.identity(dimension), 0d))
      assert(!Cholesky.isPositiveDefinite(ComplexMatrix.identity(dimension).scale(-1d), 0d))
  }

  it should "reject an indefinite Pauli matrix and accept it once shifted past its spectrum" in {
    for gate <- Seq(X(0), Y(0), Z(0)) do
      val pauli = ComplexMatrix.fromRaw(gate.matrix)

      assert(!Cholesky.isPositiveDefinite(pauli, 0d))
      assert(!Cholesky.isPositiveDefinite(pauli, 0.999))
      assert(Cholesky.isPositiveDefinite(pauli, 1.001))
  }

  it should "treat a rank-deficient state as semidefinite but not definite" in {
    val bell = DensityMatrix.maximallyEntangled(2).matrix

    assert(!Cholesky.isPositiveDefinite(bell, 0d))
    assert(Cholesky.isPositiveDefinite(bell, 1e-12))
    assert(Cholesky.isPositiveSemidefinite(bell))
  }

  it should "agree with the sign of Eigen's smallest eigenvalue on random Hermitian matrices" in {
    for dimension <- 2 to 8; _ <- 1 to 40 do
      val m = hermitian(dimension)
      val smallest = Eigen.minEigenvalue(m)

      // Stay away from the boundary, where the two routines legitimately disagree on round-off.
      if Math.abs(smallest) > 1e-8 then
        withClue(s"dimension $dimension, min eigenvalue $smallest: ") {
          assert(Cholesky.isPositiveDefinite(m, 0d) == (smallest > 0d))
        }
  }

  it should "agree with Eigen on where the shift crosses the smallest eigenvalue" in {
    for dimension <- 2 to 6; _ <- 1 to 25 do
      val m = hermitian(dimension)
      val smallest = Eigen.minEigenvalue(m)
      val margin = 1e-6

      assert(Cholesky.isPositiveDefinite(m, -smallest + margin), s"dimension $dimension")
      assert(!Cholesky.isPositiveDefinite(m, -smallest - margin), s"dimension $dimension")
  }

  it should "accept random positive semidefinite matrices of every rank" in {
    for dimension <- 2 to 6; rank <- 1 to dimension; _ <- 1 to 15 do
      withClue(s"dimension $dimension, rank $rank: ") {
        assert(Cholesky.isPositiveSemidefinite(positive(dimension, rank), 1e-9))
      }
  }

  it should "reject a non-square matrix" in {
    assertThrows[IllegalArgumentException] {
      Cholesky.isPositiveDefinite(ComplexMatrix.zero(2, 3), 1d)
    }
  }

  "differenceIsPositiveDefinite" should "match building the difference explicitly" in {
    val tolerance = 1e-12

    for dimension <- 2 to 6; _ <- 1 to 40 do
      val left = hermitian(dimension)
      val right = hermitian(dimension)
      val factor = random.nextDouble() * 4 - 2

      val fused = Cholesky.differenceIsPositiveDefinite(left, right, factor, tolerance)

      val explicit = left.minus(right.scale(factor))
      val scale = explicit.data.foldLeft(1e-300)((m, c) => Math.max(m, Complex.abs(c)))
      val direct = Cholesky.isPositiveDefinite(explicit, tolerance * scale)

      withClue(s"dimension $dimension, factor $factor: ") {
        assert(fused == direct)
      }
  }

  it should "reduce to the plain test at factor zero" in {
    for dimension <- 2 to 6; _ <- 1 to 20 do
      val m = hermitian(dimension)
      val zero = ComplexMatrix.zero(dimension, dimension)
      val scale = m.data.foldLeft(1e-300)((acc, c) => Math.max(acc, Complex.abs(c)))

      assert(
        Cholesky.differenceIsPositiveDefinite(m, zero, 0d, 1e-12) ==
          Cholesky.isPositiveDefinite(m, 1e-12 * scale)
      )
  }

  it should "reject operands of different or non-square shape" in {
    assertThrows[IllegalArgumentException] {
      Cholesky.differenceIsPositiveDefinite(ComplexMatrix.identity(2), ComplexMatrix.identity(3), 1d, 1e-12)
    }
    assertThrows[IllegalArgumentException] {
      Cholesky.differenceIsPositiveDefinite(ComplexMatrix.zero(2, 3), ComplexMatrix.zero(2, 3), 1d, 1e-12)
    }
  }

  "DensityMatrix.isPositiveSemidefinite" should "still agree with Eigen after moving to Cholesky" in {
    val states = Seq(
      DensityMatrix.maximallyEntangled(2),
      DensityMatrix.maximallyMixed(Vector(2, 2)),
      DensityMatrix.basis(Vector(3), 1),
      DensityMatrix.pure(Vector(2), Vector(Complex(0.6), Complex(0, 0.8)))
    )

    for state <- states do
      assert(state.isPositiveSemidefinite())
      assert(Eigen.isPositiveSemidefinite(state.matrix))

    val indefinite = DensityMatrix(Vector(2), ComplexMatrix.fromRaw(Z(0).matrix))

    assert(!indefinite.isPositiveSemidefinite())
    assert(!Eigen.isPositiveSemidefinite(indefinite.matrix))
  }
