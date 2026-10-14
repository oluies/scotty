package scotty.quantum.density

import scotty.ErrorMessage

/** The unnormalised post-measurement operator together with the probability of the outcome.
  *
  * Post-selection needs both halves: the probability is the quantity that gets renormalised away by
  * a P-CTC loop, and the unnormalised operator is what feeds the next step.
  */
final case class MeasurementOutcome(state: DensityMatrix, probability: Double):
  def normalized: DensityMatrix = state.normalized

object Measurement:
  /** `rho -> P rho P^dagger` on the whole space, with `probability = Tr[P rho P^dagger]`. */
  def project(rho: DensityMatrix, projector: ComplexMatrix): MeasurementOutcome =
    require(projector.rows == rho.dimension, ErrorMessage.DensityMatrixDimensionMismatch)

    val projected = DensityMatrix(rho.dims, projector.multiply(rho.matrix).multiply(projector.adjoint))

    MeasurementOutcome(projected, projected.realTrace)

  /** The same, with the projector acting only on the listed subsystems. */
  def project(rho: DensityMatrix, subsystems: Seq[Int], projector: ComplexMatrix): MeasurementOutcome =
    val dims = subsystems.toVector.map(rho.dims)
    val projected = SubsystemOps.mapWithKraus(rho, subsystems, Vector(projector), dims, dims)

    MeasurementOutcome(projected, projected.realTrace)

  /** Projects onto a pure state given as a column vector. */
  def projectOnPureState(rho: DensityMatrix, subsystems: Seq[Int], column: ComplexMatrix): MeasurementOutcome =
    project(rho, subsystems, ComplexMatrix.outer(column, column))

  /** Every computational-basis outcome of a single subsystem, in basis order. */
  def computationalBasis(rho: DensityMatrix, subsystem: Int): Vector[MeasurementOutcome] =
    require(rho.dims.indices.contains(subsystem), ErrorMessage.SubsystemIndexOutOfRange)

    val d = rho.dims(subsystem)

    Vector.tabulate(d): i =>
      project(rho, Seq(subsystem), DensityMatrix.basis(Vector(d), i).matrix)
