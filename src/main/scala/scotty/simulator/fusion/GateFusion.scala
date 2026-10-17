package scotty.simulator.fusion

import scotty.quantum.Circuit

/** A circuit lowered to fused blocks and native gates, ready to be applied or handed to a backend. */
final case class FusedCircuit(qubitCount: Int, ops: Vector[FusedOp]):
  def blocks: Vector[FusedBlock] = ops.collect { case b: FusedBlock => b }

  def natives: Vector[NativeOp] = ops.collect { case n: NativeOp => n }

  def widths: Seq[Int] = ops.map(_.width)

  /** How many blocks sit in each form. The mix an engine would have to serve. */
  def forms: Map[BlockForm, Int] = blocks.groupMapReduce(_.form)(_ => 1)(_ + _)

  def cost: FusionCost = FusionCost.of(this)

  require(
    ops.forall(_.qubits.forall(_ < qubitCount)),
    s"An op's support runs past the $qubitCount qubits of the register."
  )

/** The greedy, form-aware gate-fusion pass.
  *
  * Gates are lowered to blocks of width 1 to 3 — each in the tightest [[BlockForm]] its matrix
  * admits — and then accumulated left to right: each new block is composed into the one being built
  * as long as the union of their supports stays within `maxWidth` qubits, and otherwise the
  * accumulated block is emitted and a new one started. The pass reorders nothing, so it cannot
  * change the circuit's semantics, which is what makes it checkable against the unfused simulator.
  *
  * It still leaves money on the table: a gate that does not commute with the block being built, but
  * does commute with everything between it and a later block, has to wait. Commutation-aware and
  * look-ahead policies are worth trying now that the form lattice is in place, because the thing
  * they would be optimising has changed — it is the form mix, not the block count.
  */
object GateFusion:
  /** Five qubits is the usual software sweet spot: a 32x32 dense operator is 16 KiB in double
    * precision, small enough to stay in cache while the state streams past it.
    */
  val DefaultMaxWidth: Int = 5

  /** How much extra arithmetic a fusion may introduce, as a multiple of the un-fused cost.
    *
    * Fusion does not reduce the work; it converts memory traffic into arithmetic. Whether that is a
    * good trade depends entirely on the machine, so it is a parameter rather than a constant. The
    * default is unbounded, which is pure width-greedy fusion and minimises traffic. Set it to 1.0 to
    * forbid any flop inflation, which is roughly what a CPU wants; set it to 2 or 4 to buy less
    * traffic with more arithmetic, which is what a wide multiplier array wants.
    */
  val DefaultFlopBudget: Double = Double.PositiveInfinity

  def fuse(
      circuit: Circuit,
      maxWidth: Int = DefaultMaxWidth,
      flopBudget: Double = DefaultFlopBudget
  ): FusedCircuit =
    val lowered = circuit.gates.flatMap(FusedBlock.lower(_, maxWidth))

    FusedCircuit(circuit.register.size, fuseOps(lowered, circuit.register.size, maxWidth, flopBudget))

  def fuseOps(
      lowered: Seq[FusedOp],
      qubitCount: Int,
      maxWidth: Int,
      flopBudget: Double = DefaultFlopBudget
  ): Vector[FusedOp] =
    require(maxWidth >= 1, s"maxWidth has to be at least 1, not $maxWidth.")

    val fused = Vector.newBuilder[FusedOp]
    var pending: Option[FusedBlock] = None

    def flush(): Unit =
      pending.foreach(fused += _)
      pending = None

    lowered.foreach:
      // A gate the pass declined to lower cannot be composed with anything.
      case native: NativeOp =>
        flush()
        fused += native

      case block: FusedBlock =>
        pending match
          // A block wider than the budget cannot be fused into anything, but it still has to be
          // applied, so it goes out on its own.
          case _ if block.width > maxWidth =>
            flush()
            fused += block

          case None => pending = Some(block)

          case Some(accumulated) =>
            val union = (accumulated.qubits ++ block.qubits).distinct

            if union.length > maxWidth then
              flush()
              pending = Some(block)
            else
              val combined = block.after(accumulated)
              val separate =
                FusionCost.flopsOf(accumulated, qubitCount) + FusionCost.flopsOf(block, qubitCount)

              if FusionCost.flopsOf(combined, qubitCount) <= flopBudget * separate then
                pending = Some(combined)
              else
                flush()
                pending = Some(block)

    flush()
    fused.result()

/** A roofline-style account of what applying a fused circuit costs.
  *
  * This is the number the hardware question turns on. A multiplier array only earns its area if the
  * kernel it runs has enough arithmetic per byte of memory traffic to keep it fed; state-vector
  * simulation of one-qubit gates does not, and fusion is what changes that — by spending arithmetic,
  * not by saving it.
  *
  * The three forms cost very differently, which is the point of distinguishing them:
  *
  *   - a `Dense` block of width `k` does `2^(n-k)` complex matrix-vector products, so
  *     `2^(n-k) * (8 * 4^k - 2 * 2^k)` flops, counting a complex multiply as six and a complex add
  *     as two, and moves `2^(n-k) * 2 * 2^k` amplitudes plus one load of its `4^k` entries;
  *   - a `Diagonal` or `Monomial` block scales each amplitude exactly once, so `6 * 2^n` flops
  *     regardless of `k`, and moves the state once plus its `2^k` entries.
  *
  * A [[NativeOp]] is charged at the unfused one-qubit cost, `14 * 2^n` flops over one pass of the
  * state. That is the simulator's worst case rather than its actual cost — a multi-controlled `Z`
  * does far less — so the model is pessimistic about the gates it declined to fuse.
  */
final case class FusionCost(ops: Int, flops: Double, bytes: Double, operatorBytes: Double):
  /** Flops per byte of amplitude and operator traffic. */
  def arithmeticIntensity: Double = if bytes == 0d then 0d else flops / bytes

object FusionCost:
  /** One amplitude is a double-precision complex number. */
  val BytesPerAmplitude: Int = 16

  /** A row index in a monomial operator. */
  val BytesPerIndex: Int = 4

  def flopsOf(op: FusedOp, qubitCount: Int): Double =
    val amplitudes = Math.pow(2d, qubitCount.toDouble)

    op match
      case _: NativeOp => 14d * amplitudes
      case block: FusedBlock =>
        block.form match
          case BlockForm.Dense =>
            val d = block.dimension.toDouble

            amplitudes / d * (8d * d * d - 2d * d)
          case _ => 6d * amplitudes

  def of(fused: FusedCircuit, bytesPerAmplitude: Int = BytesPerAmplitude): FusionCost =
    val amplitudes = Math.pow(2d, fused.qubitCount.toDouble)

    fused.ops.foldLeft(FusionCost(0, 0d, 0d, 0d)): (total, op) =>
      val stateBytes = amplitudes * 2d * bytesPerAmplitude
      val operator = op match
        case _: NativeOp => 0d
        case block: FusedBlock =>
          val d = block.dimension.toDouble

          block.form match
            case BlockForm.Dense    => d * d * bytesPerAmplitude
            case BlockForm.Monomial => d * (bytesPerAmplitude + BytesPerIndex)
            case BlockForm.Diagonal => d * bytesPerAmplitude

      FusionCost(
        ops = total.ops + 1,
        flops = total.flops + flopsOf(op, fused.qubitCount),
        bytes = total.bytes + stateBytes + operator,
        operatorBytes = total.operatorBytes + operator
      )
