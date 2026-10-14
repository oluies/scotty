package scotty.quantum.density

import org.scalatest.flatspec.AnyFlatSpec
import scotty.quantum.gate.StandardGate.{X, Y, Z}
import scotty.quantum.math.Complex

class EigenSpec extends AnyFlatSpec:
  private val tolerance = 1e-10

  private def closeTo(actual: Vector[Double], expected: Vector[Double]): Boolean =
    actual.length == expected.length &&
      actual.lazyZip(expected).forall((a, e) => Math.abs(a - e) <= tolerance)

  "hermitianEigenvalues" should "return +-1 for every Pauli matrix" in {
    for gate <- Seq(X(0), Y(0), Z(0)) do
      assert(closeTo(Eigen.hermitianEigenvalues(ComplexMatrix.fromRaw(gate.matrix)), Vector(-1d, 1d)))
  }

  it should "return the diagonal of a diagonal matrix in ascending order" in {
    val diagonal = ComplexMatrix.fromRows(
      Seq(
        Seq(Complex(0.25), Complex(0), Complex(0)),
        Seq(Complex(0), Complex(-0.5), Complex(0)),
        Seq(Complex(0), Complex(0), Complex(2.5))
      )
    )

    assert(closeTo(Eigen.hermitianEigenvalues(diagonal), Vector(-0.5, 0.25, 2.5)))
  }

  it should "give a rank-one spectrum for a maximally entangled state" in {
    assert(closeTo(DensityMatrix.maximallyEntangled(2).eigenvalues, Vector(0d, 0d, 0d, 1d)))
  }

  it should "give a flat spectrum for a maximally mixed state" in {
    assert(closeTo(DensityMatrix.maximallyMixed(Vector(2, 2)).eigenvalues, Vector.fill(4)(0.25)))
  }

  it should "handle a complex Hermitian matrix with known eigenvalues" in {
    // [[1, i], [-i, 1]] has eigenvalues 0 and 2.
    val m = ComplexMatrix.fromRows(
      Seq(Seq(Complex(1), Complex(0, 1)), Seq(Complex(0, -1), Complex(1)))
    )

    assert(closeTo(Eigen.hermitianEigenvalues(m), Vector(0d, 2d)))
  }

  "isPositiveSemidefinite" should "accept states and reject indefinite operators" in {
    assert(Eigen.isPositiveSemidefinite(DensityMatrix.maximallyEntangled(2).matrix))
    assert(!Eigen.isPositiveSemidefinite(ComplexMatrix.fromRaw(Z(0).matrix)))
  }
