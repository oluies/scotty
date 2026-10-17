package scotty.simulator.fusion

import scala.collection.parallel.TaskSupport
import scala.collection.parallel.mutable.ParArray
import scotty.quantum.QuantumContext.Vector
import scotty.quantum.{Circuit, Superposition}
import scotty.simulator.QuantumSimulator

/** Applies fused blocks to a state vector, in parallel over slices.
  *
  * The arithmetic itself is in [[FusedKernel]], which has no parallel-collections dependency so that
  * it can be linked by Scala.js. This object is the JVM scheduling layer and nothing else.
  */
object FusedRunner:
  /** Runs a circuit through the fusion pass and returns the resulting superposition.
    *
    * `Measure` ops are ignored, as they are by `Circuit.gates`; this is the unitary path only.
    */
  def run(circuit: Circuit, maxWidth: Int = GateFusion.DefaultMaxWidth): Superposition =
    val simulator = QuantumSimulator()
    val state = simulator.registerToState(circuit.register)

    applyAll(state, GateFusion.fuse(circuit, maxWidth), simulator.taskSupport)
    Superposition(circuit.register, state)

  def applyAll(state: Vector, fused: FusedCircuit, taskSupport: Option[TaskSupport] = None): Unit =
    fused.blocks.foreach(applyBlock(state, fused.qubitCount, _, taskSupport))

  def applyBlock(
      state: Vector,
      qubitCount: Int,
      block: FusedBlock,
      taskSupport: Option[TaskSupport] = None
  ): Unit =
    val d = block.dimension
    val operator = FusedKernel.flatten(block)
    val strides = FusedKernel.stridesOf(block)
    val qubits = block.qubits.toArray
    val sliceCount = 1 << (qubitCount - block.width)
    val chunkCount = Math.min(sliceCount, 8 * Runtime.getRuntime.availableProcessors)
    val chunkSize = (sliceCount + chunkCount - 1) / chunkCount
    val chunks = ParArray.iterate(0, chunkCount)(_ + 1)

    taskSupport.foreach(chunks.tasksupport = _)

    chunks.foreach: chunk =>
      FusedKernel.applySlices(
        state,
        operator,
        strides,
        qubits,
        d,
        chunk * chunkSize,
        Math.min(sliceCount, (chunk + 1) * chunkSize),
        new Array[Double](d),
        new Array[Double](d)
      )
