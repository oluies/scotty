package scotty.simulator.fusion

import scotty.quantum.*
import scotty.algorithm.Grover
import scotty.quantum.gate.StandardGate.{CNOT, H, RZ}
import scotty.simulator.QuantumSimulator

/** Measures what the fusion pass buys on the JVM, and prints the roofline figures next to it.
  *
  * Run with `sbt 'Test/runMain scotty.simulator.fusion.FusionBench'`. The numbers are the input to
  * the hardware question: if fusion alone closes most of the gap in software, a dedicated engine has
  * much less to prove.
  */
object FusionBench:
  val Qubits: Int = 22
  val GroverQubits: Int = 14
  val Layers: Int = 6
  val Warmups: Int = 3
  val Runs: Int = 7

  def main(args: Array[String]): Unit =
    val workload = args.headOption.filterNot(_.forall(_.isDigit)).getOrElse("brickwork")
    val numbers = args.filter(_.forall(_.isDigit)).map(_.toInt)
    val qubits = numbers.headOption.getOrElse(if workload == "grover" then GroverQubits else Qubits)
    val groverQubits = qubits
    val circuit = workload match
      // Grover at its optimal iteration count is thousands of layers at this width, so the
      // iteration count is capped: the point is the gate mix and the all-qubit layers, not the
      // search. Pass it explicitly as the second number.
      // Grover's oracle spans every qubit, so its blocks stay native above the fusion budget; the
      // default width is its own because 22 qubits of Grover at the optimal iteration count is
      // tens of thousands of gates.
      case "grover" => Grover.circuit(groverQubits, Seq((1 << groverQubits) - 2), numbers.lift(1).getOrElse(4))
      case _        => brickwork(qubits, Layers)

    println(s"workload: $workload")
    val simulator = QuantumSimulator()
    val gates = circuit.gates.length
    val elementary = circuit.gates.flatMap(FusedBlock.lower(_)).length

    println(s"$qubits qubits, $gates gates, $elementary lowered blocks")
    println(s"state: ${(Math.pow(2, qubits.toDouble) * 16 / (1024 * 1024)).round} MiB in double precision")
    println()
    println(
      f"${"width"}%5s ${"blocks"}%7s ${"flop/byte"}%10s ${"flops"}%8s ${"traffic"}%8s " +
        f"${"operator"}%9s ${"ms"}%8s ${"gate/s"}%8s"
    )

    val unfused = FusionCost.of(
      FusedCircuit(qubits, GateFusion.fuseOps(circuit.gates.flatMap(FusedBlock.lower(_, 1)), qubits, 1))
    )
    val baseline = time(Warmups, Runs)(simulator.run(circuit))

    println(
      f"${"none"}%5s ${elementary}%7d ${unfused.arithmeticIntensity}%10.2f ${1.0}%7.1fx ${1.0}%7.1fx " +
        f"${"-"}%9s $baseline%8.1f ${gates * 1000 / baseline}%8.0f"
    )

    (2 to 6).foreach: width =>
      val fused = GateFusion.fuse(circuit, width)
      val cost = fused.cost
      val operator = fused.blocks.map(b => b.operator.entries.toLong * 16).maxOption.getOrElse(0L)
      val ms = time(Warmups, Runs)(FusedRunner.run(circuit, width))

      println(
        f"$width%5d ${fused.blocks.length}%7d ${cost.arithmeticIntensity}%10.2f " +
          f"${cost.flops / unfused.flops}%7.1fx ${cost.bytes / unfused.bytes}%7.1fx " +
          f"${humanBytes(operator)}%9s $ms%8.1f ${gates * 1000 / ms}%8.0f"
      )

    println()
    println(s"form mix at width 6: ${GateFusion.fuse(circuit, 6).forms}, " +
      s"${GateFusion.fuse(circuit, 6).natives.length} native")

    println()
    println("flops/traffic are relative to the unfused lowering; operator is the largest fused")
    println("operator, which is what has to fit in an on-chip scratchpad.")
    println()
    println("Fusion does not make the work smaller. It trades memory traffic for arithmetic, and is")
    println("only a win where arithmetic is cheaper than bandwidth -- which on a CPU it is not.")

  def time(warmups: Int, runs: Int)(body: => Any): Double =
    (0 until warmups).foreach(_ => body)

    val elapsed = (0 until runs).map: _ =>
      val started = System.nanoTime()

      body
      (System.nanoTime() - started) / 1e6

    elapsed.min

  def humanBytes(bytes: Long): String =
    if bytes >= 1024 * 1024 then s"${bytes / (1024 * 1024)} MiB"
    else if bytes >= 1024 then s"${bytes / 1024} KiB"
    else s"$bytes B"

  /** The same nearest-neighbour brickwork the spec uses, with a rotation layer so the operators are
    * dense rather than permutations.
    */
  def brickwork(qubitCount: Int, layers: Int): Circuit =
    val ops = (0 until layers).flatMap: layer =>
      (0 until qubitCount).map(H(_)) ++
        (0 until qubitCount).map(q => RZ(0.1 + 0.01 * q, q)) ++
        (layer % 2 until qubitCount - 1 by 2).map(q => CNOT(q, q + 1))

    Circuit(Circuit.generateRegister(qubitCount), ops*)
