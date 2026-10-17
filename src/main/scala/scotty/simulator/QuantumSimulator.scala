package scotty.simulator

import scala.collection.parallel.mutable.ParArray
import scala.collection.parallel.{ExecutionContextTaskSupport, ParIterable}
import scala.concurrent.ExecutionContext
import scala.util.Random
import scotty.quantum.*
import scotty.quantum.QuantumContext.*
import scotty.quantum.StateProbabilityReader.StateData
import scotty.quantum.gate.*
import scotty.quantum.gate.StandardGate.{CPHASE00, CPHASE01, CSWAP, ISWAP, PSWAP, SWAP}
import scotty.quantum.math.{Complex, MathUtils}
import scotty.simulator.math.{MatrixWrapper, VectorWrapper}

final case class QuantumSimulator(ec: Option[ExecutionContext], random: Random) extends QuantumContext:
  val taskSupport: Option[ExecutionContextTaskSupport] = ec.map(new ExecutionContextTaskSupport(_))

  def probabilities(sp: Superposition): Seq[StateData] =
    parIndices(sp.state.length).foldLeft(Vector.empty[StateData]): (ps, i) =>
      val r = sp.state(2 * i)
      val im = sp.state(2 * i + 1)
      val p = Complex.abs(Complex.product(r, im, r, im))

      if p > 0 then ps :+ StateData(MathUtils.toPaddedBinaryString(i, sp.qubitCount), Complex(r, im), p)
      else ps

  def measure(register: QubitRegister, state: QuantumContext.Vector): Collapsed =
    val initProbData = (0, 0d, None: Option[Int])
    val rnd = random.nextDouble()

    val result = parIndices(state.length).foldLeft(initProbData): (probData, stateIndex) =>
      val abs = Complex.abs(state(2 * stateIndex), state(2 * stateIndex + 1))
      val totalProb = probData._2 + abs * abs

      probData match
        case (index, _, None)    => (index + 1, totalProb, if rnd <= totalProb then Some(index) else None)
        case (index, _, indexOp) => (index + 1, totalProb, indexOp)

    Collapsed(register, result._3.get)

  def run(circuit: Circuit): State =
    val state = registerToState(circuit.register)
    val shouldMeasure = circuit.ops.exists(_.isInstanceOf[Measure])
    val iterator = parIndices(state.length / 2)

    circuit.gates.foreach(applyGate(iterator, state, _))

    if shouldMeasure then measure(circuit.register, state)
    else Superposition(circuit.register, state)

  def applyGate(state: QuantumContext.Vector, gate: Gate): Unit =
    applyGate(parIndices(state.length / 2), state, gate)

  def applyGate(iterator: ParIterable[Int], state: QuantumContext.Vector, gate: Gate): Unit = gate match
    case g: SWAP  => applyComposite(iterator, state, QuantumSimulator.swap(g.index1, g.index2))
    case g: CSWAP => applyComposite(iterator, state, QuantumSimulator.cswap(g.controlIndex, g.index1, g.index2))
    case g: ISWAP => applyComposite(iterator, state, QuantumSimulator.iswap(g.index1, g.index2))
    case g: PSWAP => applyComposite(iterator, state, QuantumSimulator.pswap(g.phi, g.index1, g.index2))
    case g: CPHASE00 =>
      applyComposite(iterator, state, QuantumSimulator.cphase00(g.phi, g.controlIndex, g.targetIndex))
    case g: CPHASE01 =>
      applyComposite(iterator, state, QuantumSimulator.cphase01(g.phi, g.controlIndex, g.targetIndex))
    case control: ControlGate => applyControlGate(iterator, state, control)
    case target: TargetGate   => applyTargetGate(iterator, state, target.index, target.matrix)
    case composite: CompositeGate => applyComposite(iterator, state, composite)
    case g                        => throw QuantumContext.QuantumException(s"Unsupported gate: ${g.name}")

  def applyComposite(iterator: ParIterable[Int], state: QuantumContext.Vector, gg: CompositeGate): Unit =
    gg.gates.foreach(applyGate(iterator, state, _))

  def applyTargetGate(
      iterator: ParIterable[Int],
      state: QuantumContext.Vector,
      index: Int,
      matrix: Matrix
  ): Unit =
    iterator.foreach: i =>
      val clearedBit = nthCleared(i, index)
      val oneIndex = 2 * clearedBit
      val zeroIndex = 2 * (clearedBit | (1 << index))

      val zeroState = (state(oneIndex), state(oneIndex + 1))
      val oneState = (state(zeroIndex), state(zeroIndex + 1))

      val newZeroState = Complex.sum(
        Complex.product(matrix(0)(0), matrix(0)(1), zeroState._1, zeroState._2),
        Complex.product(matrix(0)(2), matrix(0)(3), oneState._1, oneState._2)
      )

      val newOneState = Complex.sum(
        Complex.product(matrix(1)(0), matrix(1)(1), zeroState._1, zeroState._2),
        Complex.product(matrix(1)(2), matrix(1)(3), oneState._1, oneState._2)
      )

      state(oneIndex) = newZeroState._1
      state(oneIndex + 1) = newZeroState._2

      state(zeroIndex) = newOneState._1
      state(zeroIndex + 1) = newOneState._2

  def applyControlGate(
      iterator: ParIterable[Int],
      state: QuantumContext.Vector,
      control: ControlGate
  ): Unit =
    val m = control.finalTarget.matrix
    val targetIndex = control.finalTarget.index

    iterator.foreach: i =>
      val clearedBit = nthCleared(i, targetIndex)
      val zeroIndex = 2 * clearedBit
      val oneIndex = 2 * (clearedBit | (1 << targetIndex))

      val zeroControlsTrigger = control.controlIndexes.forall(idx => ((1 << idx) & (zeroIndex / 2)) > 0)
      val oneControlsTrigger = control.controlIndexes.forall(idx => ((1 << idx) & (oneIndex / 2)) > 0)

      if zeroControlsTrigger || oneControlsTrigger then
        val zeroState = (state(zeroIndex), state(zeroIndex + 1))
        val oneState = (state(oneIndex), state(oneIndex + 1))

        if zeroControlsTrigger then
          val newZeroState = Complex.sum(
            Complex.product(m(0)(0), m(0)(1), zeroState._1, zeroState._2),
            Complex.product(m(0)(2), m(0)(3), oneState._1, oneState._2)
          )

          state(zeroIndex) = newZeroState._1
          state(zeroIndex + 1) = newZeroState._2

        if oneControlsTrigger then
          val newOneState = Complex.sum(
            Complex.product(m(1)(0), m(1)(1), zeroState._1, zeroState._2),
            Complex.product(m(1)(2), m(1)(3), oneState._1, oneState._2)
          )

          state(oneIndex) = newOneState._1
          state(oneIndex + 1) = newOneState._2

  def nthCleared(n: Int, target: Int): Int =
    val mask = (1 << target) - 1

    (n & mask) | ((n & ~mask) << 1)

  def runExperiment(circuit: Circuit, trialsCount: Int): ExperimentResult =
    ExperimentResult(
      (0 until trialsCount).map { _ =>
        run(circuit) match
          case sp: Superposition => measure(circuit.register, sp.state)
          case c: Collapsed      => c
      }.toList
    )

  def parIndices(stateLength: Int): ParArray[Int] =
    val is = ParArray.iterate(0, stateLength / 2)(_ + 1)

    taskSupport.foreach(is.tasksupport = _)

    is

  def registerToState(register: QubitRegister): QuantumContext.Vector =
    if register.values.isEmpty then Array()
    else if register.values.forall(_ == Qubit.zero) then
      val state = Array.fill(2 * Math.pow(2, register.size).toInt)(0d)
      state(0) = 1d
      state
    else if register.values.forall(_ == Qubit.one) then
      val state = Array.fill(2 * Math.pow(2, register.size).toInt)(0d)
      state(state.length - 2) = 1d
      state
    else
      register.values
        .map(q => Array(q.a.r, q.a.i, q.b.r, q.b.i))
        .reduceLeft((state, q) => VectorWrapper.tensorProduct(state, q, taskSupport))

  def tensorProduct(register: QubitRegister, sp1: Superposition, sp2: Superposition): Superposition =
    Superposition(register, VectorWrapper.tensorProduct(sp1.state, sp2.state, taskSupport))

  def densityMatrix(vector: QuantumContext.Vector): Matrix = VectorWrapper.ketBraOuterProduct(vector)

  def isUnitary(g: TargetGate): Boolean = MatrixWrapper.isUnitary(g.matrix)

object QuantumSimulator:
  // Kept as aliases so existing callers still compile. The definitions live in
  // scotty.quantum.gate.Decompositions, which has no parallel-collections dependency and so can be
  // reached from code that has to link under Scala.js.
  val swap: (Int, Int) => CompositeGate = Decompositions.swap
  val cswap: (Int, Int, Int) => CompositeGate = Decompositions.cswap
  val iswap: (Int, Int) => CompositeGate = Decompositions.iswap
  val pswap: (Double, Int, Int) => CompositeGate = Decompositions.pswap
  val cphase00: (Double, Int, Int) => CompositeGate = Decompositions.cphase00
  val cphase01: (Double, Int, Int) => CompositeGate = Decompositions.cphase01

  def apply(): QuantumSimulator = QuantumSimulator(None, new Random())

  def apply(random: Random): QuantumSimulator = QuantumSimulator(None, random)

  def apply(ec: ExecutionContext): QuantumSimulator = QuantumSimulator(Some(ec), new Random())

  def apply(ec: ExecutionContext, random: Random): QuantumSimulator = QuantumSimulator(Some(ec), random)
