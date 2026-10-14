package scotty.quantum.density

import org.scalatest.flatspec.AnyFlatSpec
import scotty.quantum.gate.StandardGate.{H, X, Y, Z}
import scotty.quantum.math.Complex

class ComplexMatrixSpec extends AnyFlatSpec:
  private val tolerance = 1e-12

  private val x = ComplexMatrix.fromRaw(X(0).matrix)
  private val y = ComplexMatrix.fromRaw(Y(0).matrix)
  private val z = ComplexMatrix.fromRaw(Z(0).matrix)
  private val h = ComplexMatrix.fromRaw(H(0).matrix)

  "fromRaw" should "round-trip through toRaw" in {
    assert(ComplexMatrix.fromRaw(x.toRaw).approximatelyEquals(x, tolerance))
  }

  "multiply" should "square the Pauli matrices to the identity" in {
    for p <- Seq(x, y, z) do assert(p.multiply(p).approximatelyEquals(ComplexMatrix.identity(2), tolerance))
  }

  it should "respect the Pauli algebra XY = iZ" in {
    assert(x.multiply(y).approximatelyEquals(z.scale(Complex(0, 1)), tolerance))
  }

  it should "work for rectangular shapes" in {
    val a = ComplexMatrix.fromRows(Seq(Seq(Complex(1), Complex(2), Complex(3))))
    val b = ComplexMatrix.fromRows(Seq(Seq(Complex(1)), Seq(Complex(1)), Seq(Complex(1))))

    assert(a.multiply(b).rows == 1)
    assert(a.multiply(b).cols == 1)
    assert(a.multiply(b)(0, 0) == Complex(6))
  }

  "adjoint" should "conjugate and transpose" in {
    assert(y.adjoint.approximatelyEquals(y, tolerance))
    assert(h.adjoint.approximatelyEquals(h, tolerance))
    assert(x.multiply(x.adjoint).approximatelyEquals(ComplexMatrix.identity(2), tolerance))
  }

  it should "swap the shape of a rectangular matrix" in {
    val a = ComplexMatrix.fromRows(Seq(Seq(Complex(0, 1), Complex(2))))

    assert(a.adjoint.rows == 2 && a.adjoint.cols == 1)
    assert(a.adjoint(0, 0) == Complex(0, -1))
  }

  "tensor" should "produce the Kronecker product" in {
    val product = z.tensor(x)

    assert(product.rows == 4 && product.cols == 4)
    assert(product(0, 1) == Complex(1))
    assert(product(2, 3) == Complex(-1))
  }

  it should "be associative" in {
    val left = x.tensor(y).tensor(z)
    val right = x.tensor(y.tensor(z))

    assert(left.approximatelyEquals(right, tolerance))
  }

  "trace" should "sum the diagonal" in {
    assert(ComplexMatrix.identity(4).trace == Complex(4))
    assert(z.trace == Complex(0))
  }

  "isHermitian" should "distinguish Hermitian from non-Hermitian matrices" in {
    assert(x.isHermitian())
    assert(y.isHermitian())
    assert(!ComplexMatrix.fromRows(Seq(Seq(Complex(0), Complex(1)), Seq(Complex(0), Complex(0)))).isHermitian())
  }
