package scotty.quantum.math

import scotty.quantum.{Bit, Zero}
import scala.annotation.tailrec

object MathUtils:
  val Precision: Double = 1e6

  extension (d: Double)
    def toPercent: Double = d * 100

    /** Approximate equality within `1 / Precision`. */
    def ~=(d2: Double): Boolean = Math.abs(d - d2) < 1 / Precision

    def !~=(d2: Double): Boolean = !(d ~= d2)

  extension (i: Int) def toBinary: Seq[Bit] = toBinaryImpl(i)

  def toBinaryImpl(n: Int): Seq[Bit] =
    @tailrec
    def binary(acc: Seq[Bit], n: Int): Seq[Bit] = n match
      case 0 | 1 => Bit(n) +: acc
      case _     => binary(Bit(n % 2) +: acc, n / 2)

    binary(Seq(), n)

  def bitsToInts(bits: Seq[Bit]): List[Int] = bits.map(_.toInt).toList

  def toPaddedBinary(n: Int, qubitCount: Int): List[Bit] =
    val bits = toBinaryImpl(n)

    List.fill(qubitCount - bits.length)(Zero()) ++ bits

  def toPaddedBinaryInts(n: Int, qubitCount: Int): List[Int] = bitsToInts(toPaddedBinary(n, qubitCount))

  def toPaddedBinaryString(n: Int, qubitCount: Int): String =
    toPaddedBinaryInts(n, qubitCount).mkString("")

  def isProbabilityValid(a: Double, b: Double): Boolean =
    Math.abs(a * a + b * b - 1) < 1 / Precision
