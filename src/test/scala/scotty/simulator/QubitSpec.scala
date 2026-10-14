package scotty.simulator

import org.scalatest.flatspec.AnyFlatSpec
import scotty.quantum.Qubit
import scotty.quantum.math.Complex

class QubitSpec extends AnyFlatSpec {
  val sim = QuantumSimulator()

  "Qubit" should "throw IllegalArgumentException if amplitudes are invalid" in {
    assertThrows[IllegalArgumentException] {
      Qubit(Complex(0.5), Complex(0.5))
    }
  }
}