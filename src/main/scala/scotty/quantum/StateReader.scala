package scotty.quantum

import scotty.ErrorMessage
import scotty.quantum.BlochSphereReader.{BlochSphereData, Coordinates}
import scotty.quantum.QubitProbabilityReader.QubitData
import scotty.quantum.StateProbabilityReader.StateData
import scotty.quantum.math.Complex
import scotty.quantum.math.MathUtils.*

sealed trait StateReader[T]:
  val state: State

  lazy val isCollapsed: Boolean = state.isInstanceOf[Collapsed]

  def read: Seq[T]

final case class StateProbabilityReader(state: State)(using ctx: QuantumContext)
    extends StateReader[StateData]:
  def read: Seq[StateData] = state match
    case sp: Superposition => ctx.probabilities(sp)
    case c: Collapsed      => Seq(StateData(c.toBinary, Complex(1), 1))

  def toHumanString: String = read
    .flatMap: p =>
      val prob = p.probability.toPercent

      if prob == 0 then None
      else Some(s"${p.state}: Amplitude: ${Complex.toString(p.amplitude)}, " + f"P: $prob%1.2f%%")
    .mkString("\n")

object StateProbabilityReader:
  final case class StateData(state: String, amplitude: Complex, probability: Double)

final case class QubitProbabilityReader(register: Option[QubitRegister], state: State)(using
    ctx: QuantumContext
) extends StateReader[QubitData]:
  def read: Seq[QubitData] =
    val stateData = StateProbabilityReader(state).read
    val offset = state.qubitCount - 1

    (0 until state.qubitCount).map: index =>
      QubitData(
        register.flatMap(_.values(index).label),
        index,
        stateData.foldLeft(0d): (sum, data) =>
          if data.state.charAt(offset - index) == '1' then sum + data.probability else sum
      )

  def read(label: String): Option[QubitData] = read.find(_.label.contains(label))

  def toHumanString: String = read.map(_.toString).mkString("\n")

object QubitProbabilityReader:
  def apply(register: QubitRegister, state: State)(using QuantumContext): QubitProbabilityReader =
    this(Some(register), state)

  def apply(state: State)(using QuantumContext): QubitProbabilityReader = this(None, state)

  final case class QubitData(label: Option[String], index: Int, probabilityOfOne: Double):
    val probabilityOfZero: Double = 1 - probabilityOfOne

    def toHumanString: String =
      val probZero = probabilityOfZero.toPercent
      val probOne = probabilityOfOne.toPercent

      s"${label.getOrElse(s"qubit_$index")}: " +
        f"P(0) = $probZero%1.2f%% " +
        f"P(1) = $probOne%1.2f%%"

final case class BlochSphereReader(state: State)(using ctx: QuantumContext)
    extends StateReader[BlochSphereData]:
  require(state.qubitCount == 1, ErrorMessage.BlochSphereQubitCountNotOne)

  def read: Seq[BlochSphereData] = state match
    case sp: Superposition =>
      val densityMatrix = ctx.densityMatrix(sp.state)

      val x = 2 * densityMatrix(0)(2)
      val y = 2 * densityMatrix(1)(1)
      val z = Complex.abs(densityMatrix(0)(0), densityMatrix(0)(1)) -
        Complex.abs(densityMatrix(1)(2), densityMatrix(1)(3))

      val theta = Math.acos(z)
      val phi = if theta == 0 || theta == Math.PI then 0 else Math.acos(x / Math.sin(theta))

      Seq(BlochSphereData(phi, theta, Coordinates(x, y, z)))
    case c: Collapsed =>
      val isZero = c.toBitRegister.values(0).isInstanceOf[Zero]

      Seq(BlochSphereData(0, if isZero then 0 else Math.PI, Coordinates(0, 0, if isZero then 1 else -1)))

  def toHumanString: String =
    val data = read(0)
    val phi = Math.toDegrees(data.phi)
    val theta = Math.toDegrees(data.theta)
    val x = data.coordinates.x
    val y = data.coordinates.y
    val z = data.coordinates.z

    f"phi: $phi%1.2f°, theta: $theta%1.2f°, x: $x%1.2f, y: $y%1.2f, z: $z%1.2f"

object BlochSphereReader:
  final case class Coordinates(x: Double, y: Double, z: Double)
  final case class BlochSphereData(phi: Double, theta: Double, coordinates: Coordinates)
