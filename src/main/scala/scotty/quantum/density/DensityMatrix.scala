package scotty.quantum.density

import scotty.ErrorMessage
import scotty.quantum.QuantumContext.Vector as RawVector
import scotty.quantum.math.Complex
import scotty.quantum.{Superposition, State}

/** A density operator together with the tensor factorisation of the space it lives on.
  *
  * `dims` records the dimension of each subsystem in order, so `dims = Vector(2, 2, 3)` describes a
  * qubit, a qubit and a qutrit. Carrying the factorisation is what makes [[partialTrace]] and
  * [[permute]] expressible for arbitrary subsystem lists.
  *
  * Instances are not required to be normalised or positive: post-selection produces unnormalised
  * operators, and the Doeblin information of a channel is defined over unit-trace Hermitian
  * operators that need not be states.
  */
final case class DensityMatrix(dims: Vector[Int], matrix: ComplexMatrix):
  require(dims.nonEmpty && dims.forall(_ > 0), ErrorMessage.NotSameDimension)
  require(matrix.isSquare, ErrorMessage.DensityMatrixNotSquare)
  require(matrix.rows == dims.product, ErrorMessage.DensityMatrixDimensionMismatch)

  def dimension: Int = matrix.rows

  def subsystemCount: Int = dims.length

  def apply(row: Int, col: Int): Complex = matrix(row, col)

  def multiply(that: DensityMatrix): DensityMatrix =
    require(dimension == that.dimension, ErrorMessage.DensityMatrixDimensionMismatch)

    DensityMatrix(dims, matrix.multiply(that.matrix))

  def multiply(operator: ComplexMatrix): DensityMatrix = DensityMatrix(dims, matrix.multiply(operator))

  def adjoint: DensityMatrix = DensityMatrix(dims, matrix.adjoint)

  def tensor(that: DensityMatrix): DensityMatrix =
    DensityMatrix(dims ++ that.dims, matrix.tensor(that.matrix))

  def plus(that: DensityMatrix): DensityMatrix = DensityMatrix(dims, matrix.plus(that.matrix))

  def minus(that: DensityMatrix): DensityMatrix = DensityMatrix(dims, matrix.minus(that.matrix))

  def scale(factor: Double): DensityMatrix = DensityMatrix(dims, matrix.scale(factor))

  def trace: Complex = matrix.trace

  /** Real part of the trace, which is all that is meaningful for a Hermitian operator. */
  def realTrace: Double = matrix.trace.r

  def eigenvalues: Vector[Double] = Eigen.hermitianEigenvalues(matrix)

  def isHermitian(tolerance: Double = ComplexMatrix.Tolerance): Boolean = matrix.isHermitian(tolerance)

  def isPositiveSemidefinite(tolerance: Double = 1e-10): Boolean =
    Eigen.isPositiveSemidefinite(matrix, tolerance)

  def normalized: DensityMatrix =
    val t = realTrace

    require(Math.abs(t) > 0d, ErrorMessage.IncorrectQubitAmplitudes)

    scale(1 / t)

  def approximatelyEquals(that: DensityMatrix, tolerance: Double = ComplexMatrix.Tolerance): Boolean =
    dims == that.dims && matrix.approximatelyEquals(that.matrix, tolerance)

  /** Row-major strides, so `strides(i)` is the product of the dimensions after subsystem `i`. */
  lazy val strides: Vector[Int] = dims.scanRight(1)(_ * _).tail

  /** Traces out the subsystems at the given positions, keeping the rest in their original order. */
  def partialTrace(subsystems: Seq[Int]): DensityMatrix =
    require(subsystems.distinct.length == subsystems.length, ErrorMessage.SubsystemIndexesNotUnique)
    require(subsystems.forall(dims.indices.contains), ErrorMessage.SubsystemIndexOutOfRange)

    if subsystems.isEmpty then this
    else
      val traced = subsystems.toSet
      val keptPositions = dims.indices.filterNot(traced.contains).toVector

      require(keptPositions.nonEmpty, ErrorMessage.SubsystemIndexOutOfRange)

      val keptDims = keptPositions.map(dims)
      val tracedPositions = subsystems.toVector.sorted
      val tracedDims = tracedPositions.map(dims)
      val keptSize = keptDims.product
      val tracedSize = tracedDims.product

      def offset(positions: Vector[Int], subDims: Vector[Int], flat: Int): Int =
        val subStrides = subDims.scanRight(1)(_ * _).tail

        positions.indices.foldLeft(0): (acc, i) =>
          acc + (flat / subStrides(i)) % subDims(i) * strides(positions(i))

      val keptOffsets = Vector.tabulate(keptSize)(offset(keptPositions, keptDims, _))
      val tracedOffsets = Vector.tabulate(tracedSize)(offset(tracedPositions, tracedDims, _))

      val reduced = Vector.tabulate(keptSize * keptSize): idx =>
        val rowBase = keptOffsets(idx / keptSize)
        val colBase = keptOffsets(idx % keptSize)
        val (re, im) = tracedOffsets.foldLeft((0d, 0d)):
          case ((sumR, sumI), o) =>
            val c = matrix(rowBase + o, colBase + o)

            (sumR + c.r, sumI + c.i)

        Complex(re, im)

      DensityMatrix(keptDims, ComplexMatrix(keptSize, keptSize, reduced))

  /** Reorders subsystems so that new position `j` holds the subsystem previously at `order(j)`. */
  def permute(order: Seq[Int]): DensityMatrix =
    require(order.length == dims.length, ErrorMessage.SubsystemIndexOutOfRange)
    require(order.distinct.length == order.length, ErrorMessage.SubsystemIndexesNotUnique)
    require(order.forall(dims.indices.contains), ErrorMessage.SubsystemIndexOutOfRange)

    val newDims = order.toVector.map(dims)
    val newStrides = newDims.scanRight(1)(_ * _).tail
    val mapping = Vector.tabulate(dimension): newIndex =>
      newDims.indices.foldLeft(0): (acc, j) =>
        acc + (newIndex / newStrides(j)) % newDims(j) * strides(order(j))

    val permuted = Vector.tabulate(dimension * dimension): idx =>
      matrix(mapping(idx / dimension), mapping(idx % dimension))

    DensityMatrix(newDims, ComplexMatrix(dimension, dimension, permuted))

  def toHumanString: String = s"DensityMatrix(dims = ${dims.mkString(" x ")})\n${matrix.toHumanString}"

object DensityMatrix:
  def apply(dimension: Int, matrix: ComplexMatrix): DensityMatrix =
    DensityMatrix(Vector(dimension), matrix)

  def apply(matrix: ComplexMatrix): DensityMatrix = DensityMatrix(Vector(matrix.rows), matrix)

  def identity(dims: Vector[Int]): DensityMatrix =
    DensityMatrix(dims, ComplexMatrix.identity(dims.product))

  def maximallyMixed(dims: Vector[Int]): DensityMatrix =
    val d = dims.product

    DensityMatrix(dims, ComplexMatrix.identity(d).scale(1d / d))

  /** `|psi><psi|` from a column of amplitudes. */
  def pure(dims: Vector[Int], amplitudes: Seq[Complex]): DensityMatrix =
    val column = ComplexMatrix.column(amplitudes)

    DensityMatrix(dims, ComplexMatrix.outer(column, column))

  /** `|psi><psi|` from the interleaved state-vector layout used by the simulator. */
  def fromStateVector(dims: Vector[Int], vector: RawVector): DensityMatrix =
    val column = ComplexMatrix.columnFromRaw(vector)

    DensityMatrix(dims, ComplexMatrix.outer(column, column))

  /** `|psi><psi|` for a simulator superposition, treating every qubit as one subsystem. */
  def fromSuperposition(superposition: Superposition): DensityMatrix =
    fromStateVector(Vector.fill(superposition.qubitCount)(2), superposition.state)

  def fromState(state: State): DensityMatrix = state match
    case sp: Superposition => fromSuperposition(sp)
    case other => throw new IllegalArgumentException(s"Cannot build a density matrix from $other.")

  /** The standard maximally entangled state `Phi` of two `d`-dimensional systems. */
  def maximallyEntangled(d: Int): DensityMatrix =
    val amplitude = 1 / Math.sqrt(d.toDouble)
    val column = ComplexMatrix.column(
      Vector.tabulate(d * d)(i => if i / d == i % d then Complex(amplitude) else Complex(0))
    )

    DensityMatrix(Vector(d, d), ComplexMatrix.outer(column, column))

  /** `|index><index|` in the computational basis. */
  def basis(dims: Vector[Int], index: Int): DensityMatrix =
    val d = dims.product

    pure(dims, Vector.tabulate(d)(i => if i == index then Complex(1) else Complex(0)))
