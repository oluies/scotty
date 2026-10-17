package scotty.simulator.fusion

import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random
import scotty.TestHelpers
import scotty.quantum.*
import scotty.quantum.gate.*
import scotty.algorithm.{Grover, Qft, Shor}
import scotty.quantum.gate.StandardGate.*

/** The fusion pass is only useful if it is exactly the simulator, so most of this file is the same
  * assertion over and over: fuse a circuit at some width, apply it, and compare the resulting state
  * amplitude by amplitude against the unfused simulator.
  */
class GateFusionSpec extends AnyFlatSpec with TestHelpers {
  val Tolerance: Double = 1e-9

  "Scotty's bit convention" should "put qubit 0 in the least significant bit of the amplitude index" in {
    // Pinned here because the fusion pass has to agree with it: local bit j of a block's operator is
    // global qubit qubits(j), so the lowest-numbered qubit of a support is its least significant
    // bit. If this ever flips, every dense operator built below is transposed in the wrong basis.
    assert(sim.runAndMeasure(Circuit(X(0), I(1))).toBinary == "01")
    assert(sim.runAndMeasure(Circuit(X(1), I(0))).toBinary == "10")
  }

  "A fused circuit" should "reproduce the simulator on a one-qubit gate" in {
    forEveryWidth(Circuit(H(0)))
  }

  it should "reproduce the simulator on a controlled gate" in {
    forEveryWidth(Circuit(H(0), CNOT(0, 1)))
  }

  it should "reproduce the simulator on a doubly controlled gate" in {
    forEveryWidth(Circuit(H(0), H(1), H(2), CCNOT(0, 1, 2)))
  }

  it should "reproduce the simulator on gates whose controls are not adjacent to their target" in {
    forEveryWidth(Circuit(H(0), H(4), CCNOT(0, 4, 2), CNOT(4, 1)))
  }

  it should "reproduce the simulator on the swap family" in {
    forEveryWidth(Circuit(H(0), X(2), SWAP(0, 1), ISWAP(1, 2), PSWAP(thirdTurn, 0, 2), CSWAP(2, 0, 1)))
  }

  it should "reproduce the simulator on the phase family" in {
    forEveryWidth(
      Circuit(
        H(0),
        H(1),
        PHASE(quarterTurn, 0),
        PHASE0(thirdTurn, 1),
        CPHASE(quarterTurn, 0, 1),
        CPHASE00(thirdTurn, 0, 1),
        CPHASE01(quarterTurn, 1, 0),
        CPHASE10(thirdTurn, 0, 1)
      )
    )
  }

  it should "reproduce the simulator on rotations, daggers and nested controls" in {
    forEveryWidth(
      Circuit(
        RX(thirdTurn, 0),
        RY(quarterTurn, 1),
        RZ(thirdTurn, 2),
        Dagger(T(0)),
        Controlled(0, Controlled(1, H(2)))
      )
    )
  }

  it should "reproduce the simulator on teleportation, which uses a custom register" in {
    val msg = Qubit(scotty.quantum.math.Complex(0.8), scotty.quantum.math.Complex(0.6), "message")
    val register = QubitRegister(msg, Qubit.zero("here"), Qubit.zero("there"))
    val circuit = Circuit(H(1), CNOT(1, 2), CNOT(0, 1), H(0), CNOT(1, 2), Controlled(0, Z(2)))
      .withRegister(register)

    forEveryWidth(circuit)
  }

  it should "reproduce the simulator on the quantum Fourier transform" in {
    forEveryWidth(Circuit(Circuit.generateRegister(4), (X(0) +: Qft.gates((0 until 4).toIndexedSeq))*))
  }

  it should "reproduce the simulator on Grover's search" in {
    forEveryWidth(Grover.circuit(4, 11))
  }

  it should "reproduce the simulator on Shor's period finding for 15" in {
    forEveryWidth(Shor.periodCircuit(7, 4))
  }

  // The vectors above compare amplitudes, which only shows two implementations agree. These two
  // check the answer, which is what makes Grover and Shor worth carrying as difftest payloads: a
  // fusion bug that preserved norm and phase relationships would still be caught here.
  it should "still find Grover's marked state at every fusion width" in {
    val marked = 21

    (1 to 6).foreach: width =>
      val state = FusedRunner.run(Grover.circuit(5, marked), width).state
      val found = state(2 * marked) * state(2 * marked) + state(2 * marked + 1) * state(2 * marked + 1)

      withClue(s"maxWidth $width: ") {
        assert(found > 0.99, f"only $found%.4f on the marked state")
      }
  }

  it should "still factor 15 at every fusion width" in {
    val countingQubits = 4
    val q = 1 << countingQubits

    (1 to 6).foreach: width =>
      val state = FusedRunner.run(Shor.periodCircuit(7, countingQubits), width).state
      val marginal = Array.fill(q)(0d)

      (0 until state.length / 2).foreach: i =>
        marginal(i & (q - 1)) += state(2 * i) * state(2 * i) + state(2 * i + 1) * state(2 * i + 1)

      val factorisations = (0 until q).filter(marginal(_) > 0.01).flatMap(Shor.factor(7, _, q)).distinct

      withClue(s"maxWidth $width: ") {
        assert(factorisations == Seq((3, 5)))
      }
  }

  it should "reproduce the simulator on randomised circuits" in {
    val random = new Random(20261017L)

    for trial <- 0 until 24 do
      val circuit = randomCircuit(qubitCount = 5, gateCount = 18, random)

      withClue(s"trial $trial: ") {
        forEveryWidth(circuit)
      }
  }

  it should "leave a composite gate's decomposition intact when the budget is one qubit" in {
    // maxWidth 1 cannot fuse a CNOT at all, so its width-2 block has to be emitted on its own
    // rather than dropped or split.
    val fused = GateFusion.fuse(Circuit(H(0), CNOT(0, 1)), maxWidth = 1)

    assert(fused.widths == Seq(1, 2))
    forEveryWidth(Circuit(H(0), CNOT(0, 1)))
  }

  "The dense-lowering cap" should "refuse a controlled gate wider than it can represent" in {
    // Grover's oracle and diffusion both contain a controlled Z over every qubit. The unfused
    // simulator applies those in one pass, because they are diagonal; the greedy dense pass would
    // have to build a 2^k x 2^k operator, which at 12 qubits is already 128 GiB. It has to say so
    // rather than try.
    val thrown = intercept[IllegalArgumentException] {
      GateFusion.fuse(Grover.circuit(12, 7), maxWidth = 5)
    }

    assert(thrown.getMessage.contains("dense-lowering cap"))
  }

  it should "handle Grover up to the cap" in {
    assert(GateFusion.fuse(Grover.circuit(10, 7), maxWidth = 5).blocks.nonEmpty)
  }

  "The fusion pass" should "collapse a run of gates on the same qubits into one block" in {
    val circuit = Circuit(H(0), X(0), Y(0), Z(0), S(0), T(0))

    assert(circuit.gates.length == 6)
    assert(GateFusion.fuse(circuit, maxWidth = 1).blocks.length == 1)
  }

  it should "start a new block when the support would exceed the budget" in {
    val circuit = Circuit(H(0), H(1), H(2), H(3))

    assert(GateFusion.fuse(circuit, maxWidth = 2).widths == Seq(2, 2))
    assert(GateFusion.fuse(circuit, maxWidth = 4).widths == Seq(4))
  }

  it should "reduce the block count on a circuit with locality" in {
    val circuit = randomCircuit(qubitCount = 6, gateCount = 40, new Random(7L))
    val elementary = circuit.gates.flatMap(FusedBlock.lower).length

    assert(GateFusion.fuse(circuit, maxWidth = 4).blocks.length < elementary)
  }

  "The cost model" should "raise arithmetic intensity as the fusion width grows" in {
    val circuit = brickwork(qubitCount = 20, layers = 4)
    val intensities = (1 to 6).map(w => GateFusion.fuse(circuit, w).cost.arithmeticIntensity)

    assert(
      intensities.lazyZip(intensities.tail).forall(_ < _),
      s"intensity should be monotone in the fusion width, got $intensities"
    )
  }

  it should "report an unfused one-qubit gate at roughly the figure the roofline predicts" in {
    // 28 flops against 64 bytes of traffic per amplitude pair: the number that says a wide
    // multiplier array would starve on this kernel. The register has to be large enough for the
    // one-off operator load to amortise, which is the whole point of the model.
    val cost = GateFusion.fuse(Circuit(Circuit.generateRegister(20), H(0)), maxWidth = 1).cost

    assert(Math.abs(cost.arithmeticIntensity - 0.4375) < 0.01, s"got ${cost.arithmeticIntensity}")
  }

  it should "fall back off the roofline once the operator is larger than the state" in {
    // Past roughly k = n/2 the fused operator stops being amortised over anything, which is what
    // bounds the useful fusion width from above even before storage does.
    val circuit = brickwork(qubitCount = 8, layers = 4)
    val intensities = (1 to 8).map(w => GateFusion.fuse(circuit, w).cost.arithmeticIntensity)

    assert(intensities.max > intensities.last, s"expected an interior optimum, got $intensities")
  }

  "Address generation" should "insert a zero bit at every supported qubit" in {
    assert(FusedKernel.baseOf(0, Array(0, 1)) == 0)
    assert(FusedKernel.baseOf(1, Array(0, 1)) == 4)
    assert(FusedKernel.baseOf(1, Array(0, 2)) == 2)
    assert(FusedKernel.baseOf(3, Array(1)) == 5)
  }

  it should "spread a local index back over the supported qubits" in {
    assert(FusedKernel.offsetOf(0, Array(0, 2)) == 0)
    assert(FusedKernel.offsetOf(1, Array(0, 2)) == 1)
    assert(FusedKernel.offsetOf(2, Array(0, 2)) == 4)
    assert(FusedKernel.offsetOf(3, Array(0, 2)) == 5)
  }

  def forEveryWidth(circuit: Circuit): Unit =
    (1 to 6).foreach(assertMatchesSimulator(circuit, _))

  def assertMatchesSimulator(circuit: Circuit, maxWidth: Int): Unit =
    val expected = sim.run(circuit) match
      case s: Superposition => s.state
      case other            => fail(s"expected a superposition, got $other")

    val actual = FusedRunner.run(circuit, maxWidth).state

    assert(actual.length == expected.length, s"maxWidth $maxWidth changed the state's length")

    expected.indices.foreach: i =>
      withClue(
        s"maxWidth $maxWidth, amplitude ${i / 2} ${if i % 2 == 0 then "re" else "im"}: "
      ) {
        assert(Math.abs(actual(i) - expected(i)) < Tolerance)
      }

  /** A nearest-neighbour brickwork circuit: enough locality that fusion actually has something to
    * do, and regular enough that the resulting block widths are predictable.
    */
  def brickwork(qubitCount: Int, layers: Int): Circuit =
    val ops = (0 until layers).flatMap: layer =>
      (0 until qubitCount).map(H(_)) ++
        (layer % 2 until qubitCount - 1 by 2).map(q => CNOT(q, q + 1))

    Circuit(Circuit.generateRegister(qubitCount), ops*)

  /** A circuit covering every lowering path in [[FusedBlock.lower]]. */
  def randomCircuit(qubitCount: Int, gateCount: Int, random: Random): Circuit =
    def pick(n: Int): Seq[Int] = random.shuffle((0 until qubitCount).toList).take(n)
    def angle(): Double = (random.nextDouble() - 0.5) * 2 * Math.PI

    val ops = (0 until gateCount).map { _ =>
      val one = pick(1).head
      val two = pick(2)
      val ascending = two.sorted
      val three = pick(3)

      random.nextInt(18) match
        case 0  => H(one)
        case 1  => X(one)
        case 2  => Y(one)
        case 3  => Z(one)
        case 4  => T(one)
        case 5  => Dagger(S(one))
        case 6  => RX(angle(), one)
        case 7  => RY(angle(), one)
        case 8  => RZ(angle(), one)
        case 9  => PHASE(angle(), one)
        case 10 => CNOT(two.head, two.last)
        case 11 => CZ(two.head, two.last)
        case 12 => CPHASE(angle(), two.head, two.last)
        case 13 => CPHASE00(angle(), two.head, two.last)
        case 14 => SWAP(ascending.head, ascending.last)
        case 15 => ISWAP(ascending.head, ascending.last)
        case 16 => CCNOT(three.head, three(1), three.last)
        case _  => CSWAP(three.head, three(1).min(three.last), three(1).max(three.last))
    }

    Circuit(Circuit.generateRegister(qubitCount), ops*)
}
