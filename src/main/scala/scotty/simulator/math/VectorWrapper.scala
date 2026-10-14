package scotty.simulator.math

import scala.collection.parallel.TaskSupport
import scala.collection.parallel.immutable.ParVector
import scotty.quantum.QuantumContext.{Matrix, Vector}
import scotty.quantum.math.Complex

/** Pure vector helpers for the interleaved complex [[Vector]] layout. */
object VectorWrapper:
  def tensorProduct(v1: Vector, v2: Vector, taskSupport: Option[TaskSupport]): Vector =
    val v1Length = v1.length / 2
    val v2Length = v2.length / 2
    val newData = Array.fill(2 * v1Length * v2Length)(0d)
    val runs = ParVector.iterate(0, v1Length)(_ + 1)

    taskSupport.foreach(runs.tasksupport = _)

    runs.foreach: c1 =>
      for c2 <- 0 until v2Length do
        val (r, i) = Complex.product(v1(2 * c1), v1(2 * c1 + 1), v2(2 * c2), v2(2 * c2 + 1))

        newData(2 * c1 * v2Length + 2 * c2) = r
        newData(2 * c1 * v2Length + 2 * c2 + 1) = i

    newData

  /** Complex conjugate of a vector. Unlike Scotty 0.6 this does not mutate its argument. */
  def conjugate(v: Vector): Vector =
    Array.tabulate(v.length)(i => if i % 2 == 0 then v(i) else -v(i))

  /** Outer product `|v><v|`, kept bit-for-bit compatible with Scotty 0.6, which does not conjugate
    * the second factor. [[scotty.quantum.density.DensityMatrix.fromStateVector]] is the correct
    * `|v><v|` if you need one.
    */
  def ketBraOuterProduct(v: Vector): Matrix =
    val n = v.length / 2

    Array.tabulate(n, 2 * n): (row, idx) =>
      val col = idx / 2
      val (r, i) = Complex.product(v(2 * row), v(2 * row + 1), v(2 * col), v(2 * col + 1))

      if idx % 2 == 0 then r else i
