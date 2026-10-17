package scotty.simulator.fusion

import scala.collection.parallel.TaskSupport
import scala.collection.parallel.mutable.ParArray
import scotty.quantum.QuantumContext.Vector
import scotty.quantum.{Circuit, Superposition}
import scotty.simulator.QuantumSimulator

/** Applies a fused circuit to a state vector, in parallel.
  *
  * The arithmetic is in [[FusedKernel]], which has no parallel-collections dependency so that it can
  * be linked by Scala.js. This object is the JVM scheduling layer, plus the fallback to the
  * simulator for the gates the pass declined to fuse.
  */
object FusedRunner:
  def run(
      circuit: Circuit,
      maxWidth: Int = GateFusion.DefaultMaxWidth,
      flopBudget: Double = GateFusion.DefaultFlopBudget
  ): Superposition =
    val simulator = QuantumSimulator()
    val state = simulator.registerToState(circuit.register)

    applyAll(state, GateFusion.fuse(circuit, maxWidth, flopBudget), simulator)
    Superposition(circuit.register, state)

  def applyAll(state: Vector, fused: FusedCircuit, simulator: QuantumSimulator): Unit =
    fused.ops.foreach:
      case native: NativeOp  => simulator.applyGate(state, native.gate)
      case block: FusedBlock => applyBlock(state, fused.qubitCount, block, simulator.taskSupport)

  def applyBlock(
      state: Vector,
      qubitCount: Int,
      block: FusedBlock,
      taskSupport: Option[TaskSupport] = None
  ): Unit =
    val qubits = block.qubits.toArray
    val d = block.dimension

    block.operator match
      // A diagonal block has no slice structure at all: every amplitude is scaled in place, so the
      // work is chunked over the state itself rather than over gathers.
      case diagonal: DiagonalOperator =>
        val amplitudes = state.length / 2

        chunked(amplitudes, taskSupport): (first, last) =>
          FusedKernel.applyDiagonal(state, diagonal.values, qubits, first, last)

      case monomial: MonomialOperator =>
        val strides = FusedKernel.stridesOf(block)

        chunked(1 << (qubitCount - block.width), taskSupport): (first, last) =>
          FusedKernel.applyMonomial(
            state,
            monomial.rows,
            monomial.values,
            strides,
            qubits,
            d,
            first,
            last,
            new Array[Double](d),
            new Array[Double](d)
          )

      case dense: DenseOperator =>
        val operator = FusedKernel.flatten(dense)
        val strides = FusedKernel.stridesOf(block)

        chunked(1 << (qubitCount - block.width), taskSupport): (first, last) =>
          FusedKernel.applyDense(
            state,
            operator,
            strides,
            qubits,
            d,
            first,
            last,
            new Array[Double](d),
            new Array[Double](d)
          )

  /** Splits `total` units of work into chunks and runs them in parallel.
    *
    * Chunks rather than individual units so that per-chunk scratch is allocated once, and so the
    * inner loops stay long enough to be worth entering.
    */
  private def chunked(total: Int, taskSupport: Option[TaskSupport])(body: (Int, Int) => Unit): Unit =
    if total <= 0 then ()
    else
      val count = Math.min(total, 8 * Runtime.getRuntime.availableProcessors)
      val size = (total + count - 1) / count
      val chunks = ParArray.iterate(0, count)(_ + 1)

      taskSupport.foreach(chunks.tasksupport = _)

      chunks.foreach(chunk => body(chunk * size, Math.min(total, (chunk + 1) * size)))
