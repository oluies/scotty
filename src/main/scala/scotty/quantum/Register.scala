package scotty.quantum

import scotty.{ErrorMessage, Labeled}

sealed trait Register[T <: Labeled[String]]:
  val values: Seq[T]

  def size: Int = values.length

  def areLabelsUnique: Boolean =
    val labels = values.flatMap(_.label)

    labels.distinct.size == labels.size

  require(areLabelsUnique, ErrorMessage.RegisterLabelsNotUnique)

final case class BitRegister(values: Bit*) extends Register[Bit]

final case class QubitRegister(values: Qubit*) extends Register[Qubit]

object QubitRegister:
  def apply(r: String): QubitRegister = this(r.toCharArray.toIndexedSeq.map(c => Qubit(Bit(c.asDigit)))*)

  def apply(r: Int): QubitRegister = this(r.toBinaryString)
