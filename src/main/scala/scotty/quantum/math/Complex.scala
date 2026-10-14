package scotty.quantum.math

import scala.annotation.targetName
import scotty.quantum.QuantumContext.{Matrix, Vector}

/** A complex number held in double precision.
  *
  * Scotty 0.6 stored amplitudes as `Float`. The Scala 3 port widens them to `Double` so that the
  * density-matrix layer (and anything built on top of it, such as post-selection probabilities) is
  * accurate to roughly 1e-15 rather than 1e-7. All constructor overloads of the 0.6 API are kept.
  */
final case class Complex(r: Double, i: Double):
  def toHumanString: String = Complex.toString(this)

object Complex:
  /** Flattens complex values into the interleaved `(re, im, re, im, ...)` layout used by
    * [[scotty.quantum.QuantumContext.Vector]] and [[scotty.quantum.QuantumContext.Matrix]].
    */
  extension (vs: Array[Complex])
    @targetName("complexArrayToDouble")
    def toDouble: Vector = vs.flatMap(v => Array(v.r, v.i))

  extension (vs: Array[Array[Complex]])
    @targetName("complexMatrixToDouble")
    def toDouble: Matrix = vs.map(row => row.flatMap(v => Array(v.r, v.i)))

  extension (vs: List[Complex])
    @targetName("complexListToDouble")
    def toDouble: List[Double] = vs.flatMap(v => List(v.r, v.i))

  extension (vs: List[List[Complex]])
    @targetName("complexListMatrixToDouble")
    def toDouble: List[List[Double]] = vs.map(row => row.flatMap(v => List(v.r, v.i)))

  def apply(r: Double): Complex = Complex(r, 0d)

  def apply(r: Float, i: Float): Complex = Complex(r.toDouble, i.toDouble)

  def apply(r: Int, i: Int): Complex = Complex(r.toDouble, i.toDouble)

  def product(r1: Double, i1: Double, r2: Double, i2: Double): (Double, Double) =
    (r1 * r2 - i1 * i2, r1 * i2 + i1 * r2)

  def sum(c1: (Double, Double), c2: (Double, Double)): (Double, Double) =
    sum(c1._1, c1._2, c2._1, c2._2)

  def sum(r1: Double, i1: Double, r2: Double, i2: Double): (Double, Double) = (r1 + r2, i1 + i2)

  def e(phi: Double): Complex = Complex(Math.cos(phi), Math.sin(phi))

  def abs(r: Double, i: Double): Double = Math.sqrt(r * r + i * i)

  def abs(c: (Double, Double)): Double = abs(c._1, c._2)

  def abs(value: Complex): Double = abs(value.r, value.i)

  def toString(c: Complex): String =
    val r = c.r
    val i = if c.i == 0d then 0d else c.i

    f"$r%1.3f${if i >= 0 then "+" else ""}$i%1.3fi"
