package scotty.quantum

import scotty.quantum.ExperimentResult.StateStats
import scotty.quantum.math.MathUtils

final case class ExperimentResult(trials: List[Collapsed]):
  lazy val stateStats: StateStats =
    val qs = trials.head.qubitCount
    val stateCount = Math.pow(2, qs).toInt

    val state = (0 until stateCount).map(i => BitRegister(MathUtils.toPaddedBinary(i, qs)*))

    val results = trials
      .map(t => List.fill(stateCount)(0).updated(t.index, 1))
      .transpose
      .map(_.sum)

    StateStats(state.zip(results).toList)

object ExperimentResult:
  final case class StateStats(stats: List[(BitRegister, Int)]):
    def toHumanString: String = stats
      .map((register, count) => s"${register.values.map(_.toHumanString).mkString("")}: $count")
      .mkString("\n")

  def apply(collapsed: Collapsed): ExperimentResult = this(List(collapsed))
