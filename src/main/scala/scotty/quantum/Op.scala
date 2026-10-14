package scotty.quantum

trait Op:
  lazy val qubitCount: Int = indices.length

  /** Declared as a `def` so that implementations may use a `lazy val`. */
  def indices: Seq[Int]

final case class Measure(index: Int) extends Op:
  /** Declared as a `def` so that implementations may use a `lazy val`. */
  def indices: Seq[Int] = Seq(index)
