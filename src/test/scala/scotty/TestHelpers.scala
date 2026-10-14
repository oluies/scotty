package scotty

import org.scalactic.{Equality, TolerantNumerics}
import scotty.quantum.math.Complex
import scotty.simulator.QuantumSimulator

trait TestHelpers:
  given sim: QuantumSimulator = QuantumSimulator()

  val quarterTurn: Double = Math.PI / 2
  val thirdTurn: Double = 2 * Math.PI / 3
  val fiftyPercent: Double = Math.sqrt(2) / 2
  val Precision: Double = 1e8

  given doubleEquality: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(0.01)
  given complexEquality: Equality[Complex] = tolerantComplexEquality(0.01)

  def tolerantComplexEquality(tolerance: Double): Equality[Complex] =
    require(tolerance > 0d, s"$tolerance passed to tolerantComplexEquality must be a positive non-zero number.")

    new Equality[Complex]:
      def areEqual(a: Complex, b: Any): Boolean = b match
        case c: Complex => Math.abs(a.r - c.r) <= tolerance && Math.abs(a.i - c.i) <= tolerance
        case _          => false

      override def toString: String = s"TolerantComplexEquality($tolerance)"
