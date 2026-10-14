package scotty.quantum

import scotty.ErrorMessage
import scotty.quantum.gate.{CompositeGate, Gate}

final case class Circuit(register: QubitRegister, ops: Op*):
  val indices: Range = 0 until register.size

  def combine(newCircuit: Circuit): Circuit = Circuit(ops ++ newCircuit.ops*)

  def combine(newOps: Op*): Circuit = Circuit(ops ++ newOps*)

  def withRegister(newRegister: String): Circuit = Circuit(QubitRegister(newRegister), ops*)

  def withRegister(newRegister: Int): Circuit = withRegister(QubitRegister(newRegister))

  def withRegister(newRegister: QubitRegister): Circuit = Circuit(newRegister, ops*)

  def withRegister(newQubits: Qubit*): Circuit = Circuit(QubitRegister(newQubits*), ops*)

  def isValid: Boolean = register.size >= Circuit.qubitCountFromOps(ops)

  def gates: Seq[Gate] = ops.collect {
    case cg: CompositeGate => cg.gates
    case g: Gate           => Seq(g)
  }.flatten

  require(isValid, ErrorMessage.QubitCountMismatch)

object Circuit:
  val defaultState: Qubit = Qubit.zero

  def apply(ops: Op*): Circuit = this(generateRegister(ops), ops*)

  def qubitCountFromOps(ops: Seq[Op]): Int =
    if ops.isEmpty then 0 else ops.flatMap(_.indices).distinct.max + 1

  def generateRegister(n: Int): QubitRegister = QubitRegister(List.fill(n)(defaultState)*)

  def generateRegister(ops: Seq[Op]): QubitRegister =
    QubitRegister(List.fill(qubitCountFromOps(ops))(defaultState)*)
