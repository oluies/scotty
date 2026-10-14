package scotty.quantum.density

import org.scalatest.flatspec.AnyFlatSpec
import scotty.quantum.math.Complex

class MeasurementSpec extends AnyFlatSpec:
  private val tolerance = 1e-12
  private val plus = DensityMatrix.pure(Vector(2), Vector(Complex(1 / Math.sqrt(2)), Complex(1 / Math.sqrt(2))))
  private val bell = DensityMatrix.maximallyEntangled(2)

  "project" should "return an unnormalised state and its probability" in {
    val outcome = Measurement.project(plus, DensityMatrix.basis(Vector(2), 0).matrix)

    assert(Math.abs(outcome.probability - 0.5) <= tolerance)
    assert(Math.abs(outcome.state.realTrace - 0.5) <= tolerance)
    assert(Math.abs(outcome.normalized.realTrace - 1d) <= tolerance)
  }

  it should "give probability one for a projector onto the state itself" in {
    val outcome = Measurement.project(bell, bell.matrix)

    assert(Math.abs(outcome.probability - 1d) <= tolerance)
    assert(outcome.state.approximatelyEquals(bell, tolerance))
  }

  it should "give probability zero for an orthogonal projector" in {
    val orthogonal = ComplexMatrix.outer(Channel.bellBasis(1), Channel.bellBasis(1))
    val outcome = Measurement.project(bell, orthogonal)

    assert(Math.abs(outcome.probability) <= tolerance)
  }

  "projectOnPureState" should "find the Bell state in a Bell pair with certainty" in {
    val outcome = Measurement.projectOnPureState(bell, Seq(0, 1), Channel.bellBasis(0))

    assert(Math.abs(outcome.probability - 1d) <= tolerance)
  }

  it should "give one quarter per outcome when one half of the pair is maximally mixed" in {
    val product = DensityMatrix.maximallyMixed(Vector(2)).tensor(plus)

    for index <- Channel.bellBasis.indices do
      val outcome = Measurement.projectOnPureState(product, Seq(0, 1), Channel.bellBasis(index))

      assert(Math.abs(outcome.probability - 0.25) <= tolerance)
  }

  "project on a subsystem" should "leave the untouched subsystem steered" in {
    val outcome = Measurement.project(bell, Seq(0), DensityMatrix.basis(Vector(2), 1).matrix)

    assert(Math.abs(outcome.probability - 0.5) <= tolerance)
    assert(
      outcome.normalized.approximatelyEquals(
        DensityMatrix.basis(Vector(2), 1).tensor(DensityMatrix.basis(Vector(2), 1)),
        tolerance
      )
    )
  }

  "computationalBasis" should "have probabilities summing to the trace" in {
    val outcomes = Measurement.computationalBasis(bell, 1)

    assert(outcomes.length == 2)
    assert(Math.abs(outcomes.map(_.probability).sum - 1d) <= tolerance)
    assert(outcomes.forall(o => Math.abs(o.probability - 0.5) <= tolerance))
  }

  it should "work for a qutrit subsystem" in {
    val erased = Channel.erasure(0.3)(plus)
    val outcomes = Measurement.computationalBasis(erased, 0)

    assert(outcomes.length == 3)
    assert(Math.abs(outcomes(2).probability - 0.3) <= tolerance)
    assert(Math.abs(outcomes.map(_.probability).sum - 1d) <= tolerance)
  }
