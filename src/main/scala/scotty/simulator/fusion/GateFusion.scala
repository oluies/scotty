package scotty.simulator.fusion

import scotty.quantum.Circuit

/** A circuit lowered to dense blocks, ready to be applied to a state or handed to a backend. */
final case class FusedCircuit(qubitCount: Int, blocks: Vector[FusedBlock]):
  def widths: Seq[Int] = blocks.map(_.width)

  def cost: FusionCost = FusionCost.of(this)

  require(
    blocks.forall(_.qubits.forall(_ < qubitCount)),
    s"A block's support runs past the $qubitCount qubits of the register."
  )

/** The greedy gate-fusion pass.
  *
  * Gates are lowered to dense blocks of width 1 to 3 and then accumulated left to right: each new
  * block is multiplied into the one being built as long as the union of their supports stays within
  * `maxWidth` qubits, and otherwise the accumulated block is emitted and a new one started. This is
  * the simplest policy that works and it is deliberately the first thing here — it reorders nothing,
  * so it cannot change the circuit's semantics, which makes it straightforward to check against the
  * unfused simulator.
  *
  * It also leaves money on the table. A gate that does not commute with the block being built, but
  * does commute with everything between it and a later block, has to wait; a commutation-aware or
  * look-ahead policy would place it better. Those are worth trying only once the greedy numbers are
  * measured, which is what [[FusionCost]] is for.
  */
object GateFusion:
  /** Five qubits is the usual software sweet spot: a 32x32 operator is 16 KiB in double precision,
    * small enough to stay in cache while the state streams past it.
    */
  val DefaultMaxWidth: Int = 5

  def fuse(circuit: Circuit, maxWidth: Int = DefaultMaxWidth): FusedCircuit =
    FusedCircuit(circuit.register.size, fuseBlocks(circuit.gates.flatMap(FusedBlock.lower), maxWidth))

  def fuseBlocks(elementary: Seq[FusedBlock], maxWidth: Int): Vector[FusedBlock] =
    require(maxWidth >= 1, s"maxWidth has to be at least 1, not $maxWidth.")

    val fused = Vector.newBuilder[FusedBlock]
    var pending: Option[FusedBlock] = None

    elementary.foreach: block =>
      pending match
        // A lowered block wider than the budget cannot be fused into anything, but it still has to
        // be applied, so it goes out on its own.
        case _ if block.width > maxWidth =>
          pending.foreach(fused += _)
          fused += block
          pending = None

        case None => pending = Some(block)

        case Some(accumulated) =>
          if (accumulated.qubits ++ block.qubits).distinct.length <= maxWidth then
            pending = Some(block.after(accumulated))
          else
            fused += accumulated
            pending = Some(block)

    pending.foreach(fused += _)
    fused.result()

/** A roofline-style account of what applying a fused circuit costs.
  *
  * This is the number the hardware question turns on. A multiplier array only earns its area if the
  * kernel it runs has enough arithmetic per byte of memory traffic to keep it fed; state-vector
  * simulation of one-qubit gates does not, and fusion is what fixes that. The model counts, for each
  * block of width `k` over an `n`-qubit state:
  *
  *   - `2^(n-k)` slices, each a `2^k`-dimensional complex matrix-vector product, so
  *     `2^(n-k) * (8 * 4^k - 2 * 2^k)` flops, counting a complex multiply as six and a complex add
  *     as two;
  *   - `2^(n-k) * 2 * 2^k` amplitudes of traffic, read and written once each, plus one load of the
  *     `4^k`-entry operator.
  *
  * The operator-load term is what stops the ratio from growing without limit: past roughly
  * `k = n/2` the operator is larger than the state and is no longer amortised over anything. In
  * practice the binding constraint is tighter still — the operator has to fit in whatever register
  * file or scratchpad feeds the multipliers, and building it costs `O(8^k)` per gate fused.
  */
final case class FusionCost(blocks: Int, flops: Double, bytes: Double, operatorBytes: Double):
  /** Flops per byte of amplitude and operator traffic. */
  def arithmeticIntensity: Double = if bytes == 0d then 0d else flops / bytes

object FusionCost:
  /** One amplitude is a double-precision complex number. */
  val BytesPerAmplitude: Int = 16

  def of(fused: FusedCircuit, bytesPerAmplitude: Int = BytesPerAmplitude): FusionCost =
    fused.blocks.foldLeft(FusionCost(0, 0d, 0d, 0d)): (total, block) =>
      val d = block.dimension.toDouble
      val slices = Math.pow(2d, (fused.qubitCount - block.width).toDouble)
      val operator = d * d * bytesPerAmplitude

      FusionCost(
        blocks = total.blocks + 1,
        flops = total.flops + slices * (8d * d * d - 2d * d),
        bytes = total.bytes + slices * 2d * d * bytesPerAmplitude + operator,
        operatorBytes = total.operatorBytes + operator
      )
