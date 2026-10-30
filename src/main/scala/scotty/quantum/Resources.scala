package scotty.quantum

import scotty.quantum.gate.{CompositeGate, Controlled, ControlGate, Gate, TargetGate}

/** What a gate sequence costs, counted the way resource estimates count it.
  *
  * Quantum advantage is argued over resources, not answers: Hoefler, Häner and Troyer (CACM 2023,
  * arXiv:2307.00523) on which speedups survive a slow, error-corrected machine, and Beverland et al.
  * (arXiv:2211.07629) on the physical qubits that takes. This counts the logical side of that for
  * any circuit Scotty builds, so that how a construction grows can be measured rather than asserted.
  *
  *   - `qubits`: the register's size, or the highest index used plus one for a bare gate list;
  *   - `gates`: every gate once, a controlled gate included, whatever its depth of nesting;
  *   - `depth`: layers when each gate is placed as early as the qubits it touches allow;
  *   - `byArity` and `byName`: the mix, with a gate under `k` added controls named with `k` `C`s;
  *   - `rotations`: gates with an angle that is not a multiple of `pi / 4`. Clifford gates and `T`
  *     are exactly what an error-corrected machine applies natively; any other angle has to be
  *     approximated by a sequence of them, so these are the gates that dominate a fault-tolerant
  *     cost.
  *
  * Gates are counted as written: a [[CompositeGate]] is one gate on all its qubits, and nothing is
  * decomposed further. The counts are of Scotty's circuits, not of an optimised compilation.
  */
final case class Resources(
    qubits: Int,
    gates: Int,
    depth: Int,
    byArity: Map[Int, Int],
    byName: Map[String, Int],
    rotations: Int
):
  /** Gates on two or more qubits. */
  def multiQubit: Int = byArity.collect { case (arity, count) if arity >= 2 => count }.sum

  /** The counts side by side, as a table row reads them. */
  def summary: String =
    s"$qubits qubits, $gates gates ($multiQubit on two or more qubits), depth $depth, $rotations arbitrary-angle rotations"

object Resources:
  private val QuarterTurnEighth = Math.PI / 4

  def of(circuit: Circuit): Resources =
    of(circuit.gates).copy(qubits = circuit.register.size)

  def of(gates: Seq[Gate]): Resources =
    val ready = scala.collection.mutable.Map.empty[Int, Int].withDefaultValue(0)
    var depth = 0

    gates.foreach: gate =>
      val layer = gate.indices.map(ready).maxOption.getOrElse(0) + 1

      gate.indices.foreach(ready(_) = layer)
      depth = Math.max(depth, layer)

    Resources(
      qubits = gates.flatMap(_.indices).maxOption.fold(0)(_ + 1),
      gates = gates.size,
      depth = depth,
      byArity = gates.groupMapReduce(_.indices.size)(_ => 1)(_ + _),
      byName = gates.groupMapReduce(label)(_ => 1)(_ + _),
      rotations = gates.count(isArbitraryRotation)
    )

  /** The gate's name, with a `C` for each control [[Controlled]] adds. */
  def label(gate: Gate): String = gate match
    case Controlled(_, target) => "C" + label(target)
    case other                 => other.name

  /** Whether a gate carries an angle that is not a multiple of `pi / 4`. */
  def isArbitraryRotation(gate: Gate): Boolean = angles(gate).exists: phi =>
    val eighths = phi / QuarterTurnEighth

    Math.abs(eighths - Math.rint(eighths)) > 1e-9

  private def angles(gate: Gate): Seq[Double] = gate match
    case t: TargetGate      => t.params
    case c: ControlGate     => angles(c.target) ++ fieldAngles(c)
    case CompositeGate(gs*) => gs.flatMap(angles)
    case _                  => Seq()

  // Controlled gates such as CPHASE keep their angle in a field of their own rather than in a
  // target's params, so read it from the case class.
  private def fieldAngles(gate: Gate): Seq[Double] = gate match
    case p: Product => p.productIterator.collect { case d: Double => d }.toSeq
    case _          => Seq()
