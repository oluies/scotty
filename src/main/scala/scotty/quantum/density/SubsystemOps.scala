package scotty.quantum.density

import scotty.ErrorMessage

/** Shared machinery for applying operators to a chosen list of subsystems.
  *
  * The selected subsystems are permuted to the front, the operators are padded with an identity on
  * the remaining factors, and the original subsystem order is restored afterwards whenever the
  * operators preserve the number of factors.
  */
private[density] object SubsystemOps:
  /** Applies `rho -> sum_k K_k rho K_k^dagger` with each `K_k` acting on `subsystems` in order. */
  def mapWithKraus(
      rho: DensityMatrix,
      subsystems: Seq[Int],
      kraus: Seq[ComplexMatrix],
      inputDims: Vector[Int],
      outputDims: Vector[Int]
  ): DensityMatrix =
    require(kraus.nonEmpty, ErrorMessage.KrausOperatorsNotSameShape)
    require(subsystems.distinct.length == subsystems.length, ErrorMessage.SubsystemIndexesNotUnique)
    require(subsystems.forall(rho.dims.indices.contains), ErrorMessage.SubsystemIndexOutOfRange)
    require(subsystems.map(rho.dims) == inputDims, ErrorMessage.DensityMatrixDimensionMismatch)

    val targets = subsystems.toVector
    val rest = rho.dims.indices.filterNot(targets.contains).toVector
    val order = targets ++ rest
    val restDims = rest.map(rho.dims)
    val padding = ComplexMatrix.identity(Math.max(restDims.product, 1))

    val permuted = rho.permute(order)
    val padded = kraus.map(_.tensor(padding))
    val mapped = ComplexMatrix.sum(padded.map(k => k.multiply(permuted.matrix).multiply(k.adjoint)))
    val result = DensityMatrix(outputDims ++ restDims, mapped)

    if outputDims.length == targets.length then result.permute(inverseOf(order)) else result

  private def inverseOf(order: Vector[Int]): Vector[Int] =
    Vector.tabulate(order.length)(order.indexOf)
