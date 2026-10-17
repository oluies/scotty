package scotty.simulator.fusion

import scotty.quantum.QuantumContext.Vector

/** The fused-block inner loops, with no dependency on parallel collections or on the simulator.
  *
  * This is deliberately a separate object. [[FusedRunner]] uses `scala.collection.parallel`, which
  * is JVM-only, so anything that imports it cannot be linked by Scala.js — and the browser is a real
  * target here, since `pctc` already ships a Scala.js build. Everything that touches amplitudes
  * lives in this file, and lowering goes through `scotty.quantum.gate.Decompositions` rather than
  * the simulator, so the whole fusion path is linkable.
  *
  * The three loops are also the hardware specification, in the sense that matters, and they are
  * three different machines:
  *
  *   - [[applyDiagonal]] is a single streaming pass. No gather, no scatter, no shuffle network: one
  *     complex multiply per amplitude against a `2^k` phase table.
  *   - [[applyMonomial]] gathers `2^k` amplitudes, permutes and scales them, and scatters them back.
  *     It needs the shuffle network but no multiplier array.
  *   - [[applyDense]] is the only one that needs a MAC array.
  */
object FusedKernel:
  /** Scales each amplitude in `[firstIndex, lastIndex)` by the phase its supported bits select.
    *
    * The cheapest form by a wide margin, and the one a dense-only engine would throw away: a
    * multi-controlled `Z` over the whole register costs one complex multiply per amplitude here.
    */
  def applyDiagonal(
      state: Vector,
      values: Array[Double],
      qubits: Array[Int],
      firstIndex: Int,
      lastIndex: Int
  ): Unit =
    var index = firstIndex

    while index < lastIndex do
      val local = Bits.gather(index, qubits)
      val dRe = values(2 * local)
      val dIm = values(2 * local + 1)
      val i = 2 * index
      val re = state(i)
      val im = state(i + 1)

      state(i) = dRe * re - dIm * im
      state(i + 1) = dRe * im + dIm * re
      index += 1

  /** Permutes and scales each slice's `2^k` amplitudes. */
  def applyMonomial(
      state: Vector,
      rows: Array[Int],
      values: Array[Double],
      strides: Array[Int],
      qubits: Array[Int],
      dimension: Int,
      firstSlice: Int,
      lastSlice: Int,
      scratchRe: Array[Double],
      scratchIm: Array[Double]
  ): Unit =
    var slice = firstSlice

    while slice < lastSlice do
      val base = baseOf(slice, qubits)

      var col = 0
      while col < dimension do
        val i = 2 * (base + strides(col))

        scratchRe(col) = state(i)
        scratchIm(col) = state(i + 1)
        col += 1

      col = 0
      while col < dimension do
        val vRe = values(2 * col)
        val vIm = values(2 * col + 1)
        val i = 2 * (base + strides(rows(col)))

        state(i) = vRe * scratchRe(col) - vIm * scratchIm(col)
        state(i + 1) = vRe * scratchIm(col) + vIm * scratchRe(col)
        col += 1

      slice += 1

  /** Applies a dense operator to every slice in `[firstSlice, lastSlice)`.
    *
    * `scratchRe` and `scratchIm` are the gather buffers, `2^k` wide, supplied by the caller so they
    * can be allocated once per thread or per chunk. Allocating them per slice costs more than the
    * arithmetic fusion saves — at width 4 on 22 qubits there are a quarter of a million slices per
    * block.
    */
  def applyDense(
      state: Vector,
      operator: Array[Double],
      strides: Array[Int],
      qubits: Array[Int],
      dimension: Int,
      firstSlice: Int,
      lastSlice: Int,
      scratchRe: Array[Double],
      scratchIm: Array[Double]
  ): Unit =
    var slice = firstSlice

    while slice < lastSlice do
      val base = baseOf(slice, qubits)

      var local = 0
      while local < dimension do
        val i = 2 * (base + strides(local))

        scratchRe(local) = state(i)
        scratchIm(local) = state(i + 1)
        local += 1

      var row = 0
      var entry = 0
      while row < dimension do
        var accRe = 0d
        var accIm = 0d
        var col = 0

        while col < dimension do
          val mRe = operator(entry)
          val mIm = operator(entry + 1)

          accRe += mRe * scratchRe(col) - mIm * scratchIm(col)
          accIm += mRe * scratchIm(col) + mIm * scratchRe(col)
          entry += 2
          col += 1

        val i = 2 * (base + strides(row))

        state(i) = accRe
        state(i + 1) = accIm
        row += 1

      slice += 1

  /** A dense operator flattened into an interleaved array, so the inner loop walks it with one
    * running index instead of recomputing `2 * (row * cols + col)` per entry.
    */
  def flatten(operator: DenseOperator): Array[Double] =
    val d = operator.dimension
    val flat = new Array[Double](2 * d * d)

    var row = 0
    while row < d do
      var col = 0
      while col < d do
        val entry = 2 * (row * d + col)

        flat(entry) = operator.matrix.realAt(row, col)
        flat(entry + 1) = operator.matrix.imagAt(row, col)
        col += 1
      row += 1

    flat

  /** The strides at which a slice's amplitudes sit, relative to the slice's base address. In
    * hardware these are the constants programmed into the address generator once per block.
    */
  def stridesOf(block: FusedBlock): Array[Int] =
    val qubits = block.qubits.toArray

    Array.tabulate(block.dimension)(Bits.scatter(_, qubits))

  /** Applies one block to a whole state, single-threaded. The Scala.js entry point. */
  def applyBlock(state: Vector, qubitCount: Int, block: FusedBlock): Unit =
    val qubits = block.qubits.toArray
    val d = block.dimension

    block.operator match
      case diagonal: DiagonalOperator =>
        applyDiagonal(state, diagonal.values, qubits, 0, state.length / 2)

      case monomial: MonomialOperator =>
        applyMonomial(
          state,
          monomial.rows,
          monomial.values,
          stridesOf(block),
          qubits,
          d,
          0,
          1 << (qubitCount - block.width),
          new Array[Double](d),
          new Array[Double](d)
        )

      case dense: DenseOperator =>
        applyDense(
          state,
          flatten(dense),
          stridesOf(block),
          qubits,
          d,
          0,
          1 << (qubitCount - block.width),
          new Array[Double](d),
          new Array[Double](d)
        )

  /** The base amplitude index of a slice: `slice` with a zero bit inserted at each supported qubit.
    *
    * This is `QuantumSimulator.nthCleared` applied once per qubit in the support. Doing it in
    * ascending order is what makes it correct — each insertion shifts the positions above it up by
    * one, and the next qubit's position is already expressed in the shifted numbering.
    */
  def baseOf(slice: Int, qubits: Array[Int]): Int =
    var index = slice
    var j = 0

    while j < qubits.length do
      val mask = (1 << qubits(j)) - 1

      index = (index & mask) | ((index & ~mask) << 1)
      j += 1

    index

  /** A local index spread back out over the supported qubits' bit positions. */
  def offsetOf(local: Int, qubits: Array[Int]): Int = Bits.scatter(local, qubits)
