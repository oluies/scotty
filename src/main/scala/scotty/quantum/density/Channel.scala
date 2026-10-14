package scotty.quantum.density

import scotty.ErrorMessage
import scotty.quantum.QuantumContext.Matrix
import scotty.quantum.gate.StandardGate.{X, Y, Z}
import scotty.quantum.gate.TargetGate
import scotty.quantum.math.Complex

/** A completely positive map given by a list of Kraus operators.
  *
  * `inputDims` and `outputDims` record how the input and output spaces factorise, which lets a
  * channel be applied to a chosen list of subsystems of a larger [[DensityMatrix]]. Kraus operators
  * may be rectangular, so a channel is allowed to change the output dimension; the qubit-to-qutrit
  * erasure channel is the motivating case.
  *
  * Trace preservation is checked, not assumed: the P-CTC construction of arXiv:2509.08965 is stated
  * for arbitrary completely positive maps, so trace-decreasing maps are legal values here.
  */
final case class Channel(inputDims: Vector[Int], outputDims: Vector[Int], kraus: Vector[ComplexMatrix]):
  require(inputDims.nonEmpty && outputDims.nonEmpty, ErrorMessage.NotSameDimension)
  require(kraus.nonEmpty, ErrorMessage.KrausOperatorsNotSameShape)
  require(
    kraus.forall(k => k.rows == outputDims.product && k.cols == inputDims.product),
    ErrorMessage.KrausOperatorsNotSameShape
  )

  def inputDim: Int = inputDims.product

  def outputDim: Int = outputDims.product

  /** `sum_k K_k^dagger K_k`, which must be the identity for a trace-preserving map. */
  def completeness: ComplexMatrix = ComplexMatrix.sum(kraus.map(k => k.adjoint.multiply(k)))

  /** Largest entrywise deviation of the completeness relation from the identity. */
  def completenessDefect: Double = completeness.maxAbsDifference(ComplexMatrix.identity(inputDim))

  def isTracePreserving(tolerance: Double = ComplexMatrix.Tolerance): Boolean =
    completenessDefect <= tolerance

  def requireTracePreserving(tolerance: Double = ComplexMatrix.Tolerance): Channel =
    require(isTracePreserving(tolerance), ErrorMessage.KrausOperatorsNotTracePreserving)

    this

  /** Applies the channel to the whole of `rho`. */
  def apply(rho: DensityMatrix): DensityMatrix =
    require(rho.dimension == inputDim, ErrorMessage.DensityMatrixDimensionMismatch)

    val mapped = ComplexMatrix.sum(kraus.map(k => k.multiply(rho.matrix).multiply(k.adjoint)))

    DensityMatrix(outputDims, mapped)

  /** Applies the channel to the listed subsystems, leaving the others untouched.
    *
    * The subsystem order is restored when the channel preserves the number of factors; otherwise the
    * channel output is returned first, followed by the untouched subsystems in their original order.
    */
  def applyTo(rho: DensityMatrix, subsystems: Seq[Int]): DensityMatrix =
    SubsystemOps.mapWithKraus(rho, subsystems, kraus, inputDims, outputDims)

  def tensor(that: Channel): Channel =
    Channel(
      inputDims ++ that.inputDims,
      outputDims ++ that.outputDims,
      for a <- kraus; b <- that.kraus yield a.tensor(b)
    )

  /** `this` after `that`. */
  def compose(that: Channel): Channel =
    require(that.outputDim == inputDim, ErrorMessage.NotSameDimension)

    Channel(that.inputDims, outputDims, for a <- kraus; b <- that.kraus yield a.multiply(b))

object Channel:
  private val Sqrt2Inverse = 1 / Math.sqrt(2)

  def identity(dims: Vector[Int]): Channel =
    Channel(dims, dims, Vector(ComplexMatrix.identity(dims.product)))

  def identity(dimension: Int): Channel = identity(Vector(dimension))

  /** A single-Kraus-operator channel, i.e. conjugation by `operator`. */
  def fromOperator(inputDims: Vector[Int], outputDims: Vector[Int], operator: ComplexMatrix): Channel =
    Channel(inputDims, outputDims, Vector(operator))

  def fromUnitary(unitary: ComplexMatrix): Channel =
    require(unitary.isSquare, ErrorMessage.MatrixNotSquare)

    Channel(Vector(unitary.rows), Vector(unitary.rows), Vector(unitary))

  /** Reads the interleaved `(re, im, ...)` matrix layout produced by Scotty gates. */
  def fromMatrix(matrix: Matrix): Channel = fromUnitary(ComplexMatrix.fromRaw(matrix))

  /** Conjugation by the matrix of any existing Scotty single-target gate, `Dagger` and `DefGate`
    * included. Control gates are not covered because Scotty never materialises their full matrix.
    */
  def fromGate(gate: TargetGate): Channel = fromMatrix(gate.matrix)

  /** `rho -> (1 - p) rho + p I/2` on a qubit, in the Pauli Kraus form. */
  def depolarizing(p: Double): Channel =
    require(p >= 0d && p <= 1d, ErrorMessage.IncorrectQubitAmplitudes)

    val weight = Math.sqrt(p / 4)

    Channel(
      Vector(2),
      Vector(2),
      Vector(
        ComplexMatrix.identity(2).scale(Math.sqrt(1 - 3 * p / 4)),
        ComplexMatrix.fromRaw(X(0).matrix).scale(weight),
        ComplexMatrix.fromRaw(Y(0).matrix).scale(weight),
        ComplexMatrix.fromRaw(Z(0).matrix).scale(weight)
      )
    )

  /** Amplitude damping with excited-state decay probability `gamma`. */
  def amplitudeDamping(gamma: Double): Channel =
    require(gamma >= 0d && gamma <= 1d, ErrorMessage.IncorrectQubitAmplitudes)

    val kept = Math.sqrt(1 - gamma)
    val lost = Math.sqrt(gamma)

    Channel(
      Vector(2),
      Vector(2),
      Vector(
        ComplexMatrix.fromRows(Seq(Seq(Complex(1), Complex(0)), Seq(Complex(0), Complex(kept)))),
        ComplexMatrix.fromRows(Seq(Seq(Complex(0), Complex(lost)), Seq(Complex(0), Complex(0))))
      )
    )

  /** `rho -> (1 - p) rho + p Z rho Z` on a qubit. */
  def dephasing(p: Double): Channel =
    require(p >= 0d && p <= 1d, ErrorMessage.IncorrectQubitAmplitudes)

    Channel(
      Vector(2),
      Vector(2),
      Vector(
        ComplexMatrix.identity(2).scale(Math.sqrt(1 - p)),
        ComplexMatrix.fromRaw(Z(0).matrix).scale(Math.sqrt(p))
      )
    )

  /** Qubit-to-qutrit erasure: with probability `p` the input is replaced by the flag state `|e>`,
    * which is basis element 2 of the three-dimensional output.
    */
  def erasure(p: Double): Channel =
    require(p >= 0d && p <= 1d, ErrorMessage.IncorrectQubitAmplitudes)

    val kept = Math.sqrt(1 - p)
    val lost = Math.sqrt(p)
    val zero = Complex(0)

    Channel(
      Vector(2),
      Vector(3),
      Vector(
        ComplexMatrix.fromRows(
          Seq(Seq(Complex(kept), zero), Seq(zero, Complex(kept)), Seq(zero, zero))
        ),
        ComplexMatrix.fromRows(Seq(Seq(zero, zero), Seq(zero, zero), Seq(Complex(lost), zero))),
        ComplexMatrix.fromRows(Seq(Seq(zero, zero), Seq(zero, zero), Seq(zero, Complex(lost))))
      )
    )

  /** The two-qubit Bell basis as an orthonormal set of columns, ordered `Phi+, Phi-, Psi+, Psi-`. */
  val bellBasis: Vector[ComplexMatrix] =
    val s = Complex(Sqrt2Inverse)
    val n = Complex(-Sqrt2Inverse)
    val z = Complex(0)

    Vector(
      ComplexMatrix.column(Vector(s, z, z, s)),
      ComplexMatrix.column(Vector(s, z, z, n)),
      ComplexMatrix.column(Vector(z, s, s, z)),
      ComplexMatrix.column(Vector(z, s, n, z))
    )
