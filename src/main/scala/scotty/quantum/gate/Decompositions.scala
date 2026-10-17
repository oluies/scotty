package scotty.quantum.gate

import scotty.quantum.gate.StandardGate.{CNOT, CCNOT, CPHASE, CPHASE10, PHASE, S, SWAP, X}

/** The composite decompositions of the gates the simulator does not apply directly.
  *
  * These live here, in the gate layer, rather than on `QuantumSimulator`'s companion, for two
  * reasons. They are gate-level facts with no dependence on any backend; and `QuantumSimulator`
  * imports `scala.collection.parallel`, which is JVM-only, so anything that reaches for the
  * decompositions through it cannot be linked by Scala.js — including the fusion pass, whose whole
  * point is to be backend-agnostic.
  *
  * Having exactly one definition of each also keeps the fusion pass honest: it lowers a circuit by
  * reusing these, so its semantics are the simulator's by construction rather than by a second
  * reading of the gate definitions.
  */
object Decompositions:
  val swap: (Int, Int) => CompositeGate =
    (i0, i1) => CompositeGate(CNOT(i0, i1), CNOT(i1, i0), CNOT(i0, i1))

  val cswap: (Int, Int, Int) => CompositeGate =
    (i0, i1, i2) => CompositeGate(CNOT(i2, i1), CCNOT(i0, i1, i2), CNOT(i2, i1))

  val iswap: (Int, Int) => CompositeGate =
    (i0, i1) => CompositeGate(SWAP(i0, i1), S(i0), S(i1), CPHASE(Math.PI / 2, i0, i1))

  val pswap: (Double, Int, Int) => CompositeGate =
    (phi, i0, i1) => CompositeGate(SWAP(i0, i1), PHASE(phi, i0), PHASE(phi, i1), CPHASE(phi, i0, i1))

  val cphase00: (Double, Int, Int) => CompositeGate =
    (phi, i0, i1) => CompositeGate(X(i0), CPHASE10(phi, i0, i1), X(i0))

  val cphase01: (Double, Int, Int) => CompositeGate =
    (phi, i0, i1) => CompositeGate(X(i0), CPHASE(phi, i0, i1), X(i0))
