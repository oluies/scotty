# Scotty: Quantum Computing in Scala

[![CI](https://github.com/oluies/scotty/actions/workflows/ci.yml/badge.svg?branch=pctc)](https://github.com/oluies/scotty/actions/workflows/ci.yml) [![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)

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

Scotty targets **Scala 3.9 LTS**. It is built and tested on the latest JDK (27 at the time of
writing) and emits bytecode for **JDK 21 or newer** via `-release:21`. Scala 3.9 itself needs JDK 17
or newer. CI covers JDK 21, 25 and 27. Two build definitions are kept in sync; either works.

```bash
sbt test                      # compile and run the suite
sbt publishLocal publishM2    # install into ~/.ivy2/local and ~/.m2/repository

./mill scotty.test            # the same via Mill
./mill scotty.publishM2Local
```

`publishLocal`/`publishM2` is what downstream projects depend on — see
[Downstream projects](#downstream-projects).

### Releases

Releases are published to GitHub Packages as `io.github.oluies::scotty`, not to Maven Central:
`xyz.entangled` there belongs to the original project, whose last release is 0.6.0. Pushing a `v*`
tag runs `.github/workflows/release.yml`, which tests, publishes and creates the GitHub release.

```scala
libraryDependencies += "io.github.oluies" %% "scotty" % "0.7.0"
resolvers += "GitHub Packages" at "https://maven.pkg.github.com/oluies/scotty"
// GitHub Packages needs a token to read, even for a public package: a personal access token with
// read:packages, here taken from the environment. Added only when it is set, so a build without it
// still loads and fails only at resolving Scotty.
credentials ++= sys.env
  .get("GITHUB_TOKEN")
  .map(token => Credentials("GitHub Package Registry", "maven.pkg.github.com", "_", token))
  .toSeq
```

Or, without a token, build the tag from source: `git checkout v0.7.0 && sbt publishLocal`.

## Density matrices and quantum channels

`scotty.quantum.density` is an additive layer. It does not change the behaviour of the state-vector
simulator, it has no dependencies of its own, and it is built on the same `scotty.quantum.math.Complex`
number used everywhere else. Everything in it is immutable.

### `ComplexMatrix`

A dense, rectangular, row-major complex matrix — the representation Kraus operators live in.

Entries are stored as interleaved `(re, im)` primitives in an `IArray[Double]`, not as
`Vector[Complex]`. The public API still speaks in `Complex`, and `apply(row, col)` builds one on
demand, but anything in a loop should read `realAt(row, col)` and `imagAt(row, col)`, which allocate
nothing. `Cholesky` and `Eigen` do. Measured against the boxed representation it is worth about
1.6x on a 6x6 positive-definiteness test and 2.5x on a Kronecker product, which is most of the gap
to hand-written primitive code.

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
| `ComplexMatrix.fromInterleaved(rows, cols, values)` | row-major interleaved `(re, im)` doubles |
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

### `Cholesky`

When only the *sign* of the smallest eigenvalue is wanted, `Eigen` is the wrong tool: `M >= -tol I`
is exactly the question of whether `M + tol I` factorises, which is one `n^3 / 3` pass with no
iteration. `scotty.quantum.density.Cholesky` answers it directly and measures about ten times
faster than the Jacobi route at these sizes.

```scala
Cholesky.isPositiveDefinite(m, shift)            // is m + shift * I positive definite?
Cholesky.isPositiveSemidefinite(m, tolerance)    // minEigenvalue(m) >= -tolerance
Cholesky.differenceIsPositiveDefinite(a, b, factor, relativeTolerance)
```

The last one decides `a - factor * b > 0` without ever materialising the difference, with the shift
taken as `relativeTolerance` times the largest magnitude in it. Building the difference as a
`ComplexMatrix` and passing it to `isPositiveDefinite` gives the same answer but allocates two
matrices of boxed complex numbers, which measures about four times the cost of the factorisation
itself. `DensityMatrix.isPositiveSemidefinite` goes through `Cholesky`; `CholeskySpec` pins both
entry points against `Eigen` on randomised Hermitian and positive semidefinite matrices.

### `NullSpace`

```scala
NullSpace.basis(matrix, tolerance)   // a basis for { x : matrix x = 0 }, as column vectors
```

Gauss-Jordan elimination with partial pivoting, for any shape, with the zero threshold taken
relative to the largest magnitude in the matrix. This is what turns a fixed-point condition into
something solvable: a self-consistency requirement `rho = M[rho]` for linear `M` is the kernel of
`M - I`, and the dimension of that kernel says whether the solution is unique. Iterating `M` is not
a substitute — it need not converge at all.

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

`Channel.apply` has a hardware implementation checked against this one: `ChannelEngine` in
[brokkr](https://oluies.github.io/brokkr/) computes `rho -> sum_k K_k rho K_k^dagger` in Chisel, and
is verified against Scotty over four channels and 24 cases — including `erasure`, whose rectangular
`3 x 2` operators are why the engine carries separate input and output dimensions. Trace preservation
and Hermiticity are asserted there too, since they hold whatever the reference says.

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

## Algorithms

`scotty.algorithm` holds worked implementations, each with tests that check the *answer* rather than
agreement with another implementation. They double as verification vectors: a bug that preserved
norm and phase relationships would still be caught by a search that stops finding its target.

```scala
// The quantum Fourier transform, least-significant qubit first. Checked against an explicitly
// constructed DFT matrix on every basis state, not against another circuit.
Qft.gates((0 until 4).toIndexedSeq)
Qft.inverseGates((0 until 4).toIndexedSeq)

// Grover's search. Marked states are integers read with qubit 0 as the least significant bit.
val circuit = Grover.circuit(qubitCount = 5, marked = 21)   // optimal iteration count
Grover.iterations(5)                                        // floor(pi/4 * sqrt(N/M)) = 4
sim.runAndMeasure(circuit).toBinary                         // "10101" with probability > 0.999

// Shor's period finding for N = 15, with a hand-compiled modular exponentiation.
val periods = Shor.periodCircuit(a = 7, countingQubits = 4)
Shor.period(a = 7, y = 4, q = 16)                           // Some(4)
Shor.factor(a = 7, y = 4, q = 16)                           // Some((3, 5))
```

Shor is restricted to 15 on purpose. A general modular exponentiation for an `m`-bit modulus needs
on the order of `m^3` gates and `2m + 3` qubits, which puts even 21 out of reach of anything holding
`2^n` amplitudes; every Shor implementation that actually runs compiles the arithmetic for one
modulus. Modulo 15 that compilation is pleasant: multiplication by 2 is a cyclic rotation of the four
work bits because `2^4 = 1 (mod 15)`, and bitwise negation is multiplication by `-1`, so every base
is a rotation optionally followed by a negation — `CSWAP`s and `CNOT`s, nothing more.

### Tests

Eighteen cases across three specs. They are written to check the **answer**, not agreement with
another implementation, which is the property that makes them useful as verification vectors: a
diff against a second simulator only tells you two things agree, whereas a search that stops
finding its target tells you something is wrong.

```bash
sbt 'testOnly scotty.algorithm.*'
```

| spec | what it pins down |
|---|---|
| `QftSpec` | The transform matches an **explicitly constructed DFT matrix** on every basis state for 1 to 4 qubits — amplitude by amplitude, real and imaginary parts, to `1e-9`. This is what fixes the qubit-ordering convention; checking a QFT against another circuit would not. Also: the inverse undoes it, and `|0>` maps to a uniform superposition. |
| `GroverSpec` | Amplitude concentrates on the marked state for 2 to 6 qubits (`> 0.94`, the analytic floor over that range), exactly on two qubits, and better than `0.99` by five. Two marked states are found at once. Every *unmarked* state is left below `0.001` — the check that catches an oracle that amplifies indiscriminately. The iteration count is `floor(pi/4 * sqrt(N/M))`, and the oracle flips the marked amplitude's sign and nothing else. |
| `ShorSpec` | The compiled multiplier has the period classical arithmetic says it does, **for all six bases** it covers — not just the textbook `a = 7`. The period-finding circuit puts over 99% of its weight on multiples of `2^c / r`, with each peak within `0.01` of `1/r`, and the distribution normalises. Continued fractions recover the period; a zero measurement correctly yields nothing. The classical tail factors 15 into `(3, 5)`, and an end-to-end run takes every outcome the circuit actually produces with real weight and requires that the factorisations they yield are exactly `(3, 5)`. |

Both algorithms are also re-run through the fusion pass in `GateFusionSpec`, at every fusion width
from 1 to 6:

- `should reproduce the simulator on Grover's search` and the same for Shor and the QFT — the
  amplitude-level comparison against the unfused simulator;
- `should still find Grover's marked state at every fusion width` and
  `should still factor 15 at every fusion width` — the answer-level checks. A fusion bug that
  preserved norm and phase relationships but broke the algorithm would pass the first pair and fail
  this one;
- Grover also supplies the two structural cases: that a multi-controlled `Z` lowers to `2^k` entries
  rather than `4^k`, and that its all-qubit controlled `Z` is left native rather than fused.

`ReadmeExampleSpec` runs the snippets above, so the documented API cannot drift out of compiling and
the figures quoted in this file cannot drift out of being true.

## Gate fusion

`scotty.simulator.fusion` collapses runs of gates that touch the same qubits into a single operator.
It exists because applying a one-qubit gate reads an amplitude pair, multiplies it by a 2x2 matrix
and writes it back — about 28 flops against 64 bytes of traffic, which is far too little arithmetic
per byte to keep any wide multiplier array busy.

```scala
val fused = GateFusion.fuse(circuit, maxWidth = 4)

fused.forms                     // Map(Dense -> 26, Diagonal -> 18, Monomial -> 21)
fused.cost.arithmeticIntensity  // flops per byte of traffic, under the model below
FusedRunner.run(circuit, 4)     // apply it, in parallel over slices
```

Operators are stored in the tightest of three forms, a lattice closed under composition. These are
not three storage optimisations so much as three different machines:

| form | stores | flops/amplitude | what it needs |
|---|---|---|---|
| `Diagonal` | `2^k` | 6 | a multiplier and a phase table. No gather, scatter or shuffle at all |
| `Monomial` | `2^k` | 6 | a shuffle network, but no multiplier array |
| `Dense` | `4^k` | `8*2^k - 2` | a multiply-accumulate array — the only form that does |

The form is decided by reading the matrix rather than from a table of gate names, so it cannot
disagree with the semantics and classifies `Dagger` and `DefGate` for free. Controlled gates inherit
their target's form, which is why `CZ`, `CPHASE` and every multi-controlled `Z` cost `2^k` rather
than `4^k`. Gates wider than the fusion budget are not lowered at all; they become a `NativeOp` the
simulator applies through its own path, which is what keeps Grover's all-qubit controlled `Z` from
asking for a `4^k` operator.

`FusedBlock.lower` mirrors `QuantumSimulator.applyGate` case for case and reuses the same composite
decompositions from `scotty.quantum.gate.Decompositions`, so a lowered circuit is the simulator's
semantics by construction rather than by a second reading of the gate definitions. `GateFusionSpec`
checks that amplitude by amplitude across the gate set, randomised circuits, and the algorithms above.

### What it costs

Fusion does not make the work smaller. It **converts memory traffic into arithmetic**: fusing `k`
one-qubit gates on `k` distinct qubits divides traffic by `k` and multiplies dense flops by
`8*2^k / 14k`. Whether that is a good trade is a property of the machine, not of the circuit, so
`flopBudget` is a parameter — unbounded minimises traffic, `1.0` forbids any flop inflation.

Measured on an Apple M5 Pro (18 cores, JDK 27), best of seven after three warmups:

```
# nearest-neighbour brickwork, 22 qubits, 327 gates, 64 MiB state
width  blocks  flop/byte    flops  traffic  operator       ms   gate/s
 none     327       0.34     1.0x     1.0x         -    344.2      950
    3     148       0.76     1.0x     0.5x     1 KiB    260.2     1257
    6      65       6.49     3.8x     0.2x    64 KiB    544.5      601

# Grover, 14 qubits, 4 iterations, 254 gates
 none     254       0.32     1.0x     1.0x         -     22.9    11081
    4      63       2.08     1.8x     0.3x     4 KiB      8.2    31012
```

So fusion is worth about 1.3x on the brickwork circuit and 2.8x on Grover, both at modest widths,
and becomes a loss past width 5 where the added arithmetic outweighs the traffic saved. Reproduce
with:

```bash
sbt 'Test/runMain scotty.simulator.fusion.FusionBench 22'
sbt 'Test/runMain scotty.simulator.fusion.FusionBench grover 14 4'
```

Everything that touches amplitudes is in `FusedKernel`, which depends on neither the simulator nor
`scala.collection.parallel`, so the fusion path can be linked by Scala.js. `FusedRunner` is the JVM
scheduling layer on top of it. The state-vector simulator itself remains JVM-only.

The pass was written to answer a hardware question — whether state-vector simulation can be made
tensor-engine-shaped — which is written up at
[oluies.github.io/brokkr](https://oluies.github.io/brokkr/). The short answer is that the shape
transfers but the economics do not: fusion is an exchange rate, and a dense-only pass trades badly
enough to lose to the specialised 2x2 kernel at every width. The form lattice above is what reverses
that. The assessment then ends somewhere else again — a density matrix reaches 683 flop/byte at ten
qubits where the best fused state vector reaches 15.9, so the engine it actually builds implements
`Channel.apply` rather than `applyGate`.

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

The port first landed on Scala 3.3 LTS and moved to 3.9 LTS when that line
[superseded it](https://www.scala-lang.org/news/3.9/) in September 2026.

## Downstream projects

Two projects are built on this fork, and both are the reason parts of it exist.

- **[pctc](https://github.com/oluies/pctc)** — [run it in the browser](https://oluies.github.io/pctc/)
  — a numerical model of the retrocausal-capacity framework of
  [arXiv:2509.08965](https://arxiv.org/abs/2509.08965), built on the density-matrix layer above. It
  is what that layer was written for, and each piece traces to something it needs: `Channel` admits
  trace-decreasing maps and rectangular Kraus operators because the construction is stated for
  arbitrary completely positive maps; `Measurement` returns the unnormalised post-measurement
  operator alongside its probability because post-selection renormalises by exactly that probability;
  `Cholesky` decides `a - factor * b > 0` without materialising the difference, which is the inner
  loop of an operator-ordering bisection; and `NullSpace` turns Deutsch's self-consistency condition
  `rho = M[rho]` into the kernel of `M - I`.
- **[brokkr](https://oluies.github.io/brokkr/)** —
  [oluies/brokkr](https://github.com/oluies/brokkr) — a design assessment of whether a CUTE-style
  Chisel tensor engine can accelerate quantum simulation, with the RTL to go with it. The gate-fusion
  pass above was written to answer its first question; its conclusion is that the density-matrix layer
  is the better target, so its engine is a hardware `Channel.apply` verified against this
  implementation.

Scotty is the golden model for both. Neither depends on the other.

## Getting Started

To learn more about installing and using Scotty please [check out the official docs](https://www.entangled.xyz/scotty/).

## Contributing

Contributions are super welcome! Take a look at the current issues and if you'd like to help please submit a pull request with some tests covering your implementation.

## License

Scotty is available under the Apache 2.0 License.
