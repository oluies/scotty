package scotty.quantum

import scotty.quantum.QuantumContext.Vector
import scotty.quantum.gate.Gate
import scotty.quantum.math.MathUtils

sealed trait State:
  /** Declared as a `def` so that implementations may use a `lazy val`. */
  def qubitCount: Int

  val register: QubitRegister

final case class Superposition(register: QubitRegister, state: Vector) extends State:
  lazy val qubitCount: Int =
    if state.length / 2 == 0 then 0 else (Math.log10(state.length / 2) / Math.log10(2)).toInt

  def applyGate(gate: Gate)(using ctx: QuantumContext): Unit = ctx.applyGate(state, gate)

  def combine(sp: Superposition)(using ctx: QuantumContext): Superposition =
    if state.length == 0 then sp
    else ctx.tensorProduct(register, this, sp)

  def toHumanString: String = s"Superposition(${state.toList})"

final case class Collapsed(register: QubitRegister, index: Int) extends State:
  val qubitCount: Int = register.size

  def toBinary: String = MathUtils.toPaddedBinaryString(index, register.size)

  def toBitRegister: BitRegister = BitRegister(
    MathUtils
      .toPaddedBinary(index, register.size)
      .zipWithIndex
      .map((bit, i) => register.values(i).label.fold(bit)(bit.withLabel))*
  )

  def toHumanString: String = toBitRegister.values.reverse.zipWithIndex
    .map((bit, i) => s"${bit.label.getOrElse(s"bit_$i")}: ${bit.toInt}")
    .mkString("\n")
