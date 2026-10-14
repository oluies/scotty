package scotty.quantum.density

import org.scalatest.flatspec.AnyFlatSpec
import scotty.quantum.gate.StandardGate.{CNOT, H}
import scotty.quantum.math.Complex
import scotty.quantum.{Circuit, Superposition}
import scotty.simulator.QuantumSimulator

class DensityMatrixSpec extends AnyFlatSpec:
  private val tolerance = 1e-12
  private val bell = DensityMatrix.maximallyEntangled(2)
  private val halfIdentity = DensityMatrix.maximallyMixed(Vector(2))

  "a maximally entangled state" should "have unit trace" in {
    assert(Math.abs(bell.realTrace - 1d) <= tolerance)
  }

  "partialTrace" should "reduce a Bell state to the maximally mixed state on either side" in {
    assert(bell.partialTrace(Seq(0)).approximatelyEquals(halfIdentity, tolerance))
    assert(bell.partialTrace(Seq(1)).approximatelyEquals(halfIdentity, tolerance))
  }

  it should "reduce the Bell state produced by the state-vector simulator too" in {
    given sim: QuantumSimulator = QuantumSimulator()

    val superposition = sim.run(Circuit(H(0), CNOT(0, 1))).asInstanceOf[Superposition]
    val rho = DensityMatrix.fromSuperposition(superposition)

    assert(rho.partialTrace(Seq(0)).approximatelyEquals(halfIdentity, 1e-10))
    assert(rho.partialTrace(Seq(1)).approximatelyEquals(halfIdentity, 1e-10))
  }

  it should "leave a product state's factors alone" in {
    val left = DensityMatrix.basis(Vector(2), 1)
    val right = halfIdentity
    val product = left.tensor(right)

    assert(product.partialTrace(Seq(1)).approximatelyEquals(left, tolerance))
    assert(product.partialTrace(Seq(0)).approximatelyEquals(right, tolerance))
  }

  it should "handle mixed dimensions" in {
    val qutrit = DensityMatrix.maximallyMixed(Vector(3))
    val product = halfIdentity.tensor(qutrit)

    assert(product.dims == Vector(2, 3))
    assert(product.partialTrace(Seq(0)).approximatelyEquals(qutrit, tolerance))
    assert(product.partialTrace(Seq(1)).approximatelyEquals(halfIdentity, tolerance))
  }

  it should "trace several subsystems at once" in {
    val target = DensityMatrix.basis(Vector(2), 1)
    val product = halfIdentity.tensor(target).tensor(halfIdentity)

    assert(product.partialTrace(Seq(0, 2)).approximatelyEquals(target, tolerance))
  }

  it should "reject out-of-range and duplicated subsystems" in {
    assertThrows[IllegalArgumentException](bell.partialTrace(Seq(2)))
    assertThrows[IllegalArgumentException](bell.partialTrace(Seq(0, 0)))
  }

  "permute" should "be the identity for the identity order" in {
    assert(bell.permute(Seq(0, 1)).approximatelyEquals(bell, tolerance))
  }

  it should "swap the factors of a product state" in {
    val product = DensityMatrix.basis(Vector(2), 0).tensor(DensityMatrix.basis(Vector(3), 2))
    val swapped = product.permute(Seq(1, 0))

    assert(swapped.dims == Vector(3, 2))
    assert(
      swapped.approximatelyEquals(
        DensityMatrix.basis(Vector(3), 2).tensor(DensityMatrix.basis(Vector(2), 0)),
        tolerance
      )
    )
  }

  it should "round-trip" in {
    val product = halfIdentity.tensor(DensityMatrix.basis(Vector(3), 1)).tensor(DensityMatrix.basis(Vector(2), 1))
    val order = Seq(2, 0, 1)
    val inverse = Seq(1, 2, 0)

    assert(product.permute(order).permute(inverse).approximatelyEquals(product, tolerance))
  }

  "fromStateVector" should "agree with an explicit outer product" in {
    val amplitudes = Vector(Complex(0.6), Complex(0, 0.8))
    val raw = Array(0.6, 0d, 0d, 0.8)

    assert(DensityMatrix.fromStateVector(Vector(2), raw).approximatelyEquals(DensityMatrix.pure(Vector(2), amplitudes), tolerance))
  }

  it should "produce a Hermitian, positive, unit-trace operator" in {
    val rho = DensityMatrix.pure(Vector(2), Vector(Complex(0.6), Complex(0, 0.8)))

    assert(rho.isHermitian())
    assert(rho.isPositiveSemidefinite())
    assert(Math.abs(rho.realTrace - 1d) <= tolerance)
  }

  "normalized" should "rescale an unnormalised operator to unit trace" in {
    val scaled = bell.scale(0.25)

    assert(Math.abs(scaled.realTrace - 0.25) <= tolerance)
    assert(Math.abs(scaled.normalized.realTrace - 1d) <= tolerance)
  }

  "the constructor" should "reject a dimension mismatch" in {
    assertThrows[IllegalArgumentException](DensityMatrix(Vector(2, 2), ComplexMatrix.identity(2)))
  }
