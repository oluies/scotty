package scotty.quantum.gate

import scotty.quantum.gate.TargetGate.MatrixGen
import scotty.quantum.math.Complex
import scotty.quantum.math.Complex.toDouble

object StandardGate:
  // Single qubit gates

  final case class H(index: Int) extends TargetGate:
    val params: Seq[Double] = Seq()

    val pc: Double = 1 / Math.sqrt(2)
    val nc: Double = -pc

    val matrixGen: MatrixGen = _ =>
      Array(
        Array(pc, 0d, pc, 0d),
        Array(pc, 0d, nc, 0d)
      )

  final case class I(index: Int) extends TargetGate:
    val params: Seq[Double] = Seq()

    val matrixGen: MatrixGen = _ =>
      Array(
        Array(1d, 0d, 0d, 0d),
        Array(0d, 0d, 1d, 0d)
      )

  final case class X(index: Int) extends TargetGate:
    val params: Seq[Double] = Seq()

    val matrixGen: MatrixGen = _ =>
      Array(
        Array(0d, 0d, 1d, 0d),
        Array(1d, 0d, 0d, 0d)
      )

  final case class Y(index: Int) extends TargetGate:
    val params: Seq[Double] = Seq()

    val matrixGen: MatrixGen = _ =>
      Array(
        Array(Complex(0), Complex(0, -1)),
        Array(Complex(0, 1), Complex(0))
      ).toDouble

  final case class Z(index: Int) extends TargetGate:
    val params: Seq[Double] = Seq()

    val matrixGen: MatrixGen = _ =>
      Array(
        Array(Complex(1), Complex(0)),
        Array(Complex(0), Complex(-1))
      ).toDouble

  final case class S(index: Int) extends TargetGate:
    val params: Seq[Double] = Seq()
    val matrixGen: MatrixGen = PHASE(Math.PI / 2, index).matrixGen

  final case class T(index: Int) extends TargetGate:
    val params: Seq[Double] = Seq()
    val matrixGen: MatrixGen = PHASE(Math.PI / 4, index).matrixGen

  final case class PHASE(phi: Double, index: Int) extends TargetGate:
    val params: Seq[Double] = Seq(phi)

    val matrixGen: MatrixGen = _ =>
      Array(
        Array(Complex(1), Complex(0)),
        Array(Complex(0), Complex.e(phi))
      ).toDouble

  final case class PHASE0(phi: Double, index: Int) extends TargetGate:
    val params: Seq[Double] = Seq(phi)

    val matrixGen: MatrixGen = _ =>
      Array(
        Array(Complex.e(phi), Complex(0)),
        Array(Complex(0), Complex(1))
      ).toDouble

  final case class RX(theta: Double, index: Int) extends TargetGate:
    val params: Seq[Double] = Seq(theta)

    val matrixGen: MatrixGen = _ =>
      Array(
        Array(Complex(Math.cos(theta / 2)), Complex(0, -Math.sin(theta / 2))),
        Array(Complex(0, -Math.sin(theta / 2)), Complex(Math.cos(theta / 2)))
      ).toDouble

  final case class RY(theta: Double, index: Int) extends TargetGate:
    val params: Seq[Double] = Seq(theta)

    val matrixGen: MatrixGen = _ =>
      Array(
        Array(Complex(Math.cos(theta / 2)), Complex(-Math.sin(theta / 2))),
        Array(Complex(Math.sin(theta / 2)), Complex(Math.cos(theta / 2)))
      ).toDouble

  final case class RZ(theta: Double, index: Int) extends TargetGate:
    val params: Seq[Double] = Seq(theta)

    val matrixGen: MatrixGen = _ =>
      Array(
        Array(Complex(Math.cos(theta / 2), Math.sin(-theta / 2)), Complex(0)),
        Array(Complex(0), Complex(Math.cos(theta / 2), Math.sin(theta / 2)))
      ).toDouble

  // Multi qubit gates

  final case class CNOT(controlIndex: Int, targetIndex: Int) extends ControlGate:
    lazy val target: X = X(targetIndex)

  final case class CCNOT(controlIndex: Int, controlIndex2: Int, targetIndex: Int) extends ControlGate:
    lazy val target: Controlled = Controlled(controlIndex2, X(targetIndex))

  final case class CZ(controlIndex: Int, targetIndex: Int) extends ControlGate:
    lazy val target: Z = Z(targetIndex)

  final case class SWAP(index1: Int, index2: Int) extends SwapGate

  final case class ISWAP(index1: Int, index2: Int) extends SwapGate

  final case class PSWAP(phi: Double, index1: Int, index2: Int) extends SwapGate

  final case class CSWAP(controlIndex: Int, index1: Int, index2: Int) extends ControlGate:
    lazy val target: SWAP = SWAP(index1, index2)

  final case class CPHASE(phi: Double, controlIndex: Int, targetIndex: Int) extends ControlGate:
    lazy val target: PHASE = PHASE(phi, targetIndex)

  final case class CPHASE10(phi: Double, controlIndex: Int, targetIndex: Int) extends ControlGate:
    lazy val target: PHASE0 = PHASE0(phi, targetIndex)

  final case class CPHASE00(phi: Double, controlIndex: Int, targetIndex: Int) extends ControlGate:
    lazy val target: I = I(targetIndex)

  final case class CPHASE01(phi: Double, controlIndex: Int, targetIndex: Int) extends ControlGate:
    lazy val target: I = I(targetIndex)
