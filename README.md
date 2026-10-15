# Scotty: Quantum Computing in Scala

[![CI](https://github.com/entangled-xyz/scotty/actions/workflows/ci.yml/badge.svg)](https://github.com/entangled-xyz/scotty/actions/workflows/ci.yml) [![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)

*"Whatever you say, sir. Thy will be done."*—Montgomery Scott

Scotty is a quantum computing framework for Scala developers. It comes with a quantum computer simulator that can be used for writing hybrid programs out-of-the-box.

Most quantum frameworks and simulators are written either in quantum-specific languages (like Q# and QISKit) or Python. Scotty is one of the first attempts at building a cross-platform quantum framework on top of the JVM.

It was built with three principles in mind:

- **Write once, run anywhere**: experiment with quantum code and run it with Scotty. Export Scotty circuits (coming soon) to Quil or OpenQASM and run it on other simulators or real quantum computers.
- **Expandability**: provide a high-level set of abstractions that can be expanded on different architectures.
- **No PhD required**: it should be easy to get started and everything should work intuitively out-of-the-box.

Here is an example of a quantum teleportation algorithm written in Scotty to give you an idea of what a typical piece of code looks like:

```scala
def entangle(q1: Int, q2: Int) = Circuit(H(q1), CNOT(q1, q2))

val msg = Qubit(Complex(0.8), Complex(0.6), "message")
val here = Qubit.zero("here")
val there = Qubit.zero("there")

val register = QubitRegister(msg, here, there)

val circuit = entangle(1, 2)
  .combine(CNOT(0, 1), H(0))
  .combine(CNOT(1, 2), Controlled(0, Z(2)))
  .withRegister(register)

given sim: QuantumSimulator = QuantumSimulator()

assert(
  QubitProbabilityReader(register, sim.run(circuit))
    .read("there")
    .fold(false)(_.probabilityOfOne ~= msg.probabilityOfOne))
```

Here we just setup a quantum circuit with a custom register of qubits, ran it in the quantum simulator, and then peeked at the *superposition* probability of a "there" qubit.

This example is kept honest by `src/test/scala/scotty/ReadmeExampleSpec.scala`.

## Building

Scotty targets **Scala 3.3 LTS**. It is built and tested on the latest JDK (27 at the time of
writing) and emits bytecode for **JDK 21 or newer** via `-release:21`, so it stays usable on the
current LTS. CI covers JDK 21, 25 and 27. Two build definitions are kept in sync; either works.

```bash
sbt test                      # compile and run the suite
sbt publishLocal publishM2    # install into ~/.ivy2/local and ~/.m2/repository

./mill scotty.test            # the same via Mill
./mill scotty.publishM2Local
```

`publishLocal`/`publishM2` is what downstream projects (such as the P-CTC simulation in `../pctc`) depend on.

## Density matrices and quantum channels

`scotty.quantum.density` is an additive layer. It does not change the behaviour of the state-vector
simulator, it has no dependencies of its own, and it is built on the same `scotty.quantum.math.Complex`
number used everywhere else. Everything in it is immutable.

### `ComplexMatrix`

A dense, rectangular, row-major complex matrix — the representation Kraus operators live in.

```scala
import scotty.quantum.density.*
import scotty.quantum.math.Complex

val h = ComplexMatrix.fromRaw(H(0).matrix)         // read a Scotty gate matrix
val x = ComplexMatrix.fromRows(Seq(
  Seq(Complex(0), Complex(1)),
  Seq(Complex(1), Complex(0))))

h.multiply(x)                                       // matrix product
h.adjoint                                           // conjugate transpose
h.tensor(x)                                         // Kronecker product
h.trace                                             // Complex
h.plus(x) ; h.minus(x) ; h.scale(0.5)
h.isHermitian() ; h.approximatelyEquals(x, 1e-12)
h.toRaw                                             // back to Scotty's interleaved layout
```

| Constructor | Meaning |
| --- | --- |
| `ComplexMatrix(rows, cols, data)` | row-major `Vector[Complex]` |
| `ComplexMatrix.fromRows(rows)` | from a `Seq[Seq[Complex]]` |
| `ComplexMatrix.fromRaw(matrix)` | from Scotty's interleaved `(re, im, …)` matrix |
| `ComplexMatrix.column(amplitudes)` / `columnFromRaw(vector)` | a column vector |
| `ComplexMatrix.identity(d)` / `zero(rows, cols)` | constants |
| `ComplexMatrix.outer(left, right)` | `|left><right|` |

### `DensityMatrix`

A square operator plus the tensor factorisation of the space it lives on. Carrying `dims` is what
makes a partial trace over an arbitrary subsystem list expressible.

```scala
val bell = DensityMatrix.maximallyEntangled(2)      // dims = Vector(2, 2)

bell.partialTrace(Seq(0))                           // == DensityMatrix.maximallyMixed(Vector(2))
bell.permute(Seq(1, 0))                             // reorder subsystems
bell.eigenvalues                                    // Vector(0.0, 0.0, 0.0, 1.0), ascending
bell.realTrace                                      // 1.0
bell.multiply(other) ; bell.adjoint ; bell.tensor(other)
bell.scale(0.25).normalized                         // unnormalised operators are legal
bell.isHermitian() ; bell.isPositiveSemidefinite()
```

Instances are deliberately **not** required to be normalised or positive: post-selection produces
unnormalised operators, and the Doeblin information of a channel is defined over unit-trace Hermitian
operators that are not states.

| Constructor | Meaning |
| --- | --- |
| `DensityMatrix(dims, matrix)` | explicit |
| `DensityMatrix.fromStateVector(dims, vector)` | `|psi><psi|` from the interleaved layout |
| `DensityMatrix.fromSuperposition(sp)` / `fromState(state)` | straight from a simulator result |
| `DensityMatrix.pure(dims, amplitudes)` | `|psi><psi|` from `Seq[Complex]` |
| `DensityMatrix.basis(dims, i)` | `|i><i|` |
| `DensityMatrix.identity(dims)` / `maximallyMixed(dims)` | constants |
| `DensityMatrix.maximallyEntangled(d)` | the standard `Phi` on `Vector(d, d)` |

Hermitian eigenvalues come from `scotty.quantum.density.Eigen`, a cyclic Jacobi routine run on the
real symmetric `2d x 2d` embedding `[[Re H, -Im H], [Im H, Re H]]`. That is accurate to roughly
`1e-14` and fast enough for the dimensions this layer targets (at most 16). `Eigen` also exposes
`minEigenvalue`, `maxEigenvalue` and `isPositiveSemidefinite`.

### `Channel`

A completely positive map as a list of Kraus operators, with the input and output factorisations
recorded so a channel can be applied to a chosen list of subsystems. Kraus operators may be
rectangular, so a channel is allowed to change the output dimension.

```scala
val depolarizing = Channel.depolarizing(0.2)        // rho -> 0.8 rho + 0.2 I/2
val damping      = Channel.amplitudeDamping(0.3)
val dephasing    = Channel.dephasing(0.1)           // rho -> 0.9 rho + 0.1 Z rho Z
val erasure      = Channel.erasure(0.4)             // qubit -> qutrit, |e> is basis element 2
val hadamard     = Channel.fromGate(H(0))           // any Scotty TargetGate

depolarizing.completeness                            // sum_k K_k^dagger K_k
depolarizing.completenessDefect                      // largest deviation from the identity
depolarizing.isTracePreserving()                     // true
depolarizing.requireTracePreserving()                // throws if it is not

depolarizing(rho)                                    // apply to the whole operator
depolarizing.applyTo(rho, Seq(1))                    // apply to chosen subsystems
damping.tensor(dephasing)                            // parallel composition
damping.compose(dephasing)                           // damping after dephasing
```

`applyTo` permutes the selected subsystems to the front, pads the Kraus operators with an identity
on the rest, and restores the original subsystem order afterwards. When the channel changes the
number of factors the output is returned first instead, followed by the untouched subsystems in
their original order.

Trace preservation is checked, never assumed: trace-decreasing completely positive maps are legal
values of `Channel`, which is what the P-CTC construction of
[arXiv:2509.08965](https://arxiv.org/abs/2509.08965) needs.

`Channel.bellBasis` is the two-qubit Bell basis as four orthonormal columns, ordered
`Phi+, Phi-, Psi+, Psi-`.

### `Measurement`

```scala
val outcome = Measurement.projectOnPureState(rho, Seq(0, 1), Channel.bellBasis(0))

outcome.probability                                  // Tr[P rho P]
outcome.state                                        // P rho P, left unnormalised
outcome.normalized                                   // ... and normalised, if you want it

Measurement.project(rho, projector)                  // whole space
Measurement.project(rho, Seq(0), projector)          // chosen subsystems
Measurement.computationalBasis(rho, subsystem)       // every basis outcome, in order
```

Returning the unnormalised post-measurement operator *and* the probability is what post-selection
needs: the probability is the quantity a P-CTC loop renormalises away, and the unnormalised operator
is what feeds the next step.

## Notes on the Scala 3 port

The gate, circuit, register and reader API is unchanged in shape. Four things did change:

1. **Amplitudes are `Double`, not `Float`.** `Complex` now holds two `Double`s and
   `QuantumContext.Vector`/`Matrix` are `Array[Double]`/`Array[Array[Double]]`. Single precision caps
   accuracy at about `1e-7`, which is not enough for the density-matrix layer. Code that passed
   `Array[Float]` literals (`Array(1f, 0f)`) into `DefGate` needs `Array(1d, 0d)`; the
   `Complex` conversion helpers are `.toDouble` where they used to be `.toFloat`, and
   `One.floatValue`/`Zero.floatValue` are now `One.rawValue`/`Zero.rawValue`.
2. **Implicit parameters are `using` parameters.** `implicit val sim = QuantumSimulator()` still
   works as a `given`; only call sites that applied the implicit list explicitly need `(using sim)`.
3. **`Op.indices`, `State.qubitCount` and `ControlGate.target` are `def`s**, because Scala 3 will not
   let a `lazy val` override a non-lazy `val`. Implementations are unaffected.
4. **Two long-standing bugs were fixed.** `MatrixWrapper.conjugateTranspose` transposed in place and
   only produced the correct result for 2x2 matrices; it is now pure and correct at every dimension.
   `MatrixWrapper.areSameDimension(matrix, vector)` compared a vector to itself and so always
   returned `true`. `VectorWrapper.ketBraOuterProduct` keeps its original (non-conjugating)
   behaviour, because `BlochSphereReader` depends on it; use
   `DensityMatrix.fromStateVector` for a true `|psi><psi|`.

Deprecated or removed Scala 2 constructs replaced along the way: `implicit class` to `extension`,
`(a, b).zipped` to `lazyZip`, `xs: _*` to `xs*`, `import p._` to `import p.*`, and ScalaTest's
`FlatSpec` to `flatspec.AnyFlatSpec`.

## Getting Started

To learn more about installing and using Scotty please [check out the official docs](https://www.entangled.xyz/scotty/).

## Contributing

Contributions are super welcome! Take a look at the current issues and if you'd like to help please submit a pull request with some tests covering your implementation.

## License

Scotty is available under the Apache 2.0 License.
