package scotty.simulator

import org.scalatest.flatspec.AnyFlatSpec
import scotty.TestHelpers
import scotty.quantum.*
import scotty.quantum.gate.{DefGate, CompositeGate}
import scotty.quantum.math.Complex
import scotty.quantum.math.Complex.toDouble

class DefGateSpec extends AnyFlatSpec with TestHelpers {
  val twoByTwoMatrix = Array(
    Array(0d, 0d, 1d, 0d),
    Array(1d, 0d, 0d, 0d)
  )

  val nonUnitaryMatrix = Array(
    Array(1d, 0d, 0d, 0d),
    Array(0d, 0d, 2d, 0d)
  )

  val fourByFourMatrix = Array(
    Array(1d, 0d, 0d, 0d, 0d, 0d, 0d, 0d),
    Array(0d, 0d, 1d, 0d, 0d, 0d, 0d, 0d),
    Array(0d, 0d, 0d, 0d, 1d, 0d, 0d, 0d),
    Array(0d, 0d, 0d, 0d, 0d, 0d, 1d, 0d)
  )

  val rxMatrix = (params: Seq[Double]) => {
    val theta = params(0)

    Array(
      Array(Complex(Math.cos(theta / 2)), Complex(0, -Math.sin(theta / 2))),
      Array(Complex(0, -Math.sin(theta / 2)), Complex(Math.cos(theta / 2)))
    ).toDouble
  }

  "Custom gate" should "be able to work with a static matrix" in {
    def myGate(index: Int) = DefGate(twoByTwoMatrix, index)

    assert(sim.runAndMeasure(Circuit(myGate(0))).toBitRegister.values == Seq(One()))
    assert(sim.runAndMeasure(Circuit(myGate(0)).withRegister(Qubit.one)).toBitRegister.values == Seq(Zero()))
  }

  it should "be able to work with a dynamic matrix and a sequence of params" in {
    def myGate(theta: Double, index: Int) = DefGate(rxMatrix, theta, index)

    sim.run(Circuit(myGate(quarterTurn, 0))) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude === Complex(fiftyPercent, 0))
        assert(StateProbabilityReader(s).read(1).amplitude === Complex(0, -fiftyPercent))
      case _ =>
    }
  }

  it should "be able to work with a dynamic matrix and a single param" in {
    def myGate(theta: Double, index: Int) = DefGate(rxMatrix, theta, index)

    sim.run(Circuit(myGate(quarterTurn, 0))) match {
      case s: Superposition =>
        assert(StateProbabilityReader(s).read(0).amplitude === Complex(fiftyPercent, 0))
        assert(StateProbabilityReader(s).read(1).amplitude === Complex(0, -fiftyPercent))
      case _ =>
    }
  }
}