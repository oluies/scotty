package scotty.quantum.density

import org.scalatest.flatspec.AnyFlatSpec
import scotty.quantum.gate.StandardGate.{H, RY, S, X, Y, Z}
import scotty.quantum.gate.{Dagger, DefGate}
import scotty.quantum.math.Complex

class ChannelSpec extends AnyFlatSpec:
  private val tolerance = 1e-12
  private val parameters = (0 to 10).map(_ / 10d)
  private val plus = DensityMatrix.pure(Vector(2), Vector(Complex(1 / Math.sqrt(2)), Complex(1 / Math.sqrt(2))))
  private val one = DensityMatrix.basis(Vector(2), 1)
  private val half = DensityMatrix.maximallyMixed(Vector(2))

  private def channels(p: Double): Seq[(String, Channel)] = Seq(
    "identity" -> Channel.identity(2),
    "depolarizing" -> Channel.depolarizing(p),
    "amplitudeDamping" -> Channel.amplitudeDamping(p),
    "dephasing" -> Channel.dephasing(p),
    "erasure" -> Channel.erasure(p)
  )

  "every standard channel" should "satisfy the Kraus completeness relation" in {
    for p <- parameters; (name, channel) <- channels(p) do
      withClue(s"$name($p) defect ${channel.completenessDefect}: ") {
        assert(channel.isTracePreserving(tolerance))
      }
  }

  it should "preserve the trace of a state" in {
    for p <- parameters; (name, channel) <- channels(p); rho <- Seq(plus, one, half) do
      withClue(s"$name($p): ") {
        assert(Math.abs(channel(rho).realTrace - 1d) <= 1e-10)
      }
  }

  it should "map states to states" in {
    for p <- parameters; (name, channel) <- channels(p); rho <- Seq(plus, one, half) do
      withClue(s"$name($p): ") {
        assert(channel(rho).isHermitian(1e-10))
        assert(channel(rho).isPositiveSemidefinite())
      }
  }

  "depolarizing" should "interpolate between the input and the maximally mixed state" in {
    for p <- parameters do
      val expected = plus.scale(1 - p).plus(half.scale(p))

      assert(Channel.depolarizing(p)(plus).approximatelyEquals(expected, 1e-12))
  }

  "amplitudeDamping" should "fix the ground state and decay the excited state" in {
    val ground = DensityMatrix.basis(Vector(2), 0)

    for p <- parameters do
      assert(Channel.amplitudeDamping(p)(ground).approximatelyEquals(ground, tolerance))

      val expected = ground.scale(p).plus(one.scale(1 - p))

      assert(Channel.amplitudeDamping(p)(one).approximatelyEquals(expected, tolerance))
  }

  "dephasing" should "fix the computational basis and damp the coherences" in {
    for p <- parameters do
      assert(Channel.dephasing(p)(one).approximatelyEquals(one, tolerance))
      assert(Math.abs(Channel.dephasing(p)(plus)(0, 1).r - (1 - 2 * p) / 2) <= tolerance)
  }

  "erasure" should "send a qubit to a qutrit and flag the erasure" in {
    for p <- parameters do
      val output = Channel.erasure(p)(plus)

      assert(output.dims == Vector(3))
      assert(Math.abs(output(2, 2).r - p) <= tolerance)
      assert(Math.abs(output(0, 0).r - (1 - p) / 2) <= tolerance)
  }

  "fromGate" should "equal conjugation by the gate matrix" in {
    val gates = Seq(H(0), X(0), Y(0), Z(0), S(0), RY(0.73, 0), Dagger(S(0)), Dagger(RY(0.73, 0)))

    for gate <- gates; rho <- Seq(plus, one, half) do
      val u = ComplexMatrix.fromRaw(gate.matrix)
      val expected = DensityMatrix(Vector(2), u.multiply(rho.matrix).multiply(u.adjoint))

      withClue(s"${gate.name}: ") {
        assert(Channel.fromGate(gate)(rho).approximatelyEquals(expected, tolerance))
      }
  }

  it should "work for a user-defined gate" in {
    val gate = DefGate(
      Array(Array(Complex(0), Complex(1)), Array(Complex(1), Complex(0))).map(_.flatMap(c => Array(c.r, c.i))),
      0
    )

    assert(Channel.fromGate(gate)(one).approximatelyEquals(DensityMatrix.basis(Vector(2), 0), tolerance))
  }

  "applyTo" should "touch only the selected subsystem" in {
    val rho = one.tensor(plus)
    val flipped = Channel.fromGate(X(0)).applyTo(rho, Seq(0))

    assert(flipped.dims == Vector(2, 2))
    assert(flipped.approximatelyEquals(DensityMatrix.basis(Vector(2), 0).tensor(plus), tolerance))
  }

  it should "act on a non-contiguous subsystem list" in {
    val rho = one.tensor(plus).tensor(one)
    val flipped = Channel.fromGate(X(0)).applyTo(rho, Seq(2))

    assert(flipped.approximatelyEquals(one.tensor(plus).tensor(DensityMatrix.basis(Vector(2), 0)), tolerance))
  }

  it should "change the dimension of the targeted subsystem" in {
    val rho = one.tensor(plus)
    val erased = Channel.erasure(1d).applyTo(rho, Seq(1))

    assert(erased.dims == Vector(2, 3))
    assert(erased.approximatelyEquals(one.tensor(DensityMatrix.basis(Vector(3), 2)), tolerance))
  }

  it should "half-depolarize the second half of a Bell pair into a known state" in {
    val bell = DensityMatrix.maximallyEntangled(2)
    val fully = Channel.depolarizing(1d).applyTo(bell, Seq(1))

    assert(fully.approximatelyEquals(half.tensor(half), 1e-12))
  }

  "tensor" should "combine two channels" in {
    val combined = Channel.fromGate(X(0)).tensor(Channel.depolarizing(1d))
    val out = combined(one.tensor(plus))

    assert(out.approximatelyEquals(DensityMatrix.basis(Vector(2), 0).tensor(half), 1e-12))
    assert(combined.isTracePreserving(tolerance))
  }

  "compose" should "chain two channels" in {
    val twice = Channel.fromGate(X(0)).compose(Channel.fromGate(X(0)))

    assert(twice(plus).approximatelyEquals(plus, tolerance))
  }

  "a non-trace-preserving map" should "be representable but reported as such" in {
    val halfKraus = Channel(Vector(2), Vector(2), Vector(ComplexMatrix.identity(2).scale(Math.sqrt(0.5))))

    assert(!halfKraus.isTracePreserving())
    assert(Math.abs(halfKraus(plus).realTrace - 0.5) <= tolerance)
    assertThrows[IllegalArgumentException](halfKraus.requireTracePreserving())
  }
