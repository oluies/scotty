package scotty.simulator.fusion

import scotty.quantum.QuantumContext.Vector

/** The fused-block inner loop, with no dependency on parallel collections.
  *
  * This is deliberately a separate object. [[FusedRunner]] uses `scala.collection.parallel`, which
  * is JVM-only, so anything that imports it cannot be linked by Scala.js — and the browser is a real
  * target here, since `pctc` already ships a Scala.js build. Everything that actually touches
  * amplitudes lives in this file so that a cross-built or single-threaded caller can use the fusion
  * path without pulling parallel collections in.
  *
  * It is also the hardware specification, in the sense that matters: compute a slice's base address,
  * gather `2^k` amplitudes at fixed strides, multiply by the operator, scatter back. An RTL
  * implementation is this loop with the address arithmetic in a generator and the matvec in a MAC
  * array.
  */
object FusedKernel:
  /** Applies one block to every slice in `[firstSlice, lastSlice)`.
    *
    * `scratchRe` and `scratchIm` are the gather buffers, `2^k` wide, supplied by the caller so they
    * can be allocated once per thread or per chunk. Allocating them per slice costs more than the
    * arithmetic fusion saves — at width 4 on 22 qubits there are a quarter of a million slices per
    * block.
    */
  def applySlices(
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

  /** The operator flattened into an interleaved array, so the inner loop walks it with one running
    * index instead of recomputing `2 * (row * cols + col)` per entry.
    */
  def flatten(block: FusedBlock): Array[Double] =
    val d = block.dimension
    val flat = new Array[Double](2 * d * d)

    var row = 0
    while row < d do
      var col = 0
      while col < d do
        val i = 2 * (row * d + col)

        flat(i) = block.operator.realAt(row, col)
        flat(i + 1) = block.operator.imagAt(row, col)
        col += 1
      row += 1

    flat

  /** The strides at which a slice's amplitudes sit, relative to the slice's base address. In
    * hardware these are the constants programmed into the address generator once per block.
    */
  def stridesOf(block: FusedBlock): Array[Int] =
    val qubits = block.qubits.toArray

    Array.tabulate(block.dimension)(offsetOf(_, qubits))

  /** Applies a block to a whole state, single-threaded. The Scala.js entry point. */
  def applyBlock(state: Vector, qubitCount: Int, block: FusedBlock): Unit =
    val d = block.dimension

    applySlices(
      state,
      flatten(block),
      stridesOf(block),
      block.qubits.toArray,
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
  def offsetOf(local: Int, qubits: Array[Int]): Int =
    var offset = 0
    var j = 0

    while j < qubits.length do
      offset |= ((local >> j) & 1) << qubits(j)
      j += 1

    offset
