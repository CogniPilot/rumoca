# Loop-constructor identity review control

Diagnostic supplement to the frozen `builtin-identity` candidate. This closes
its missing `cont`/`done` exercise; it does not change the producer or adopt
the toolchain. SPEC_0033 §§2a/4/6 governs; SPEC_0037 §6 (DRAFT) identifies the
trusted translation boundary. The independent review at2026-09-08T13:31:34Z
accepted the controls with the scope clarification recorded below.

## Source and first divergence

The Rust fixture has source functions named `cont` and `done`, both identity
functions, and a loop whose Boolean state allows either an immediate exit or
one continuation followed by exit. Every call returns its original `u32`.
Calling those source functions ensures both names are real dependencies.

The predecessor Aeneas binary refuses both name registrations (exit1):
`LoopResult/@Continue` conflicts with the source `cont`, and
`LoopResult/@Break` conflicts with source `done`. Its partial generated Lean
also rejects `cont again1` as `Result Bool` instead of `ControlFlow Bool Unit`,
and `done ()` as `Unit` instead of the source function's `U32` input. This
predecessor is not a silently accepted wrong program. Its partial output is
inspected only as diagnostic evidence, never admitted.

The frozen candidate's generated body has both fully qualified constructors,
`_root_.Aeneas.Std.ControlFlow.cont` and `.done`, while ordinary source calls
remain `cont false` and `done value`. The same Charon output is consumed by
both binaries; neither the Rust source nor generated Lean is renamed or edited.
The existing map repair is therefore exercised at its actual responsible owner.

## Verified checks

`run-evidence.json` binds exact commands, working directories, expected and
observed exits, terminal output, source/tool/LLBC/Lean hashes and development
history. Observation chunks are identifiers, not command timings.

| Check | Outcome |
|---|---|
| Native fixture; strict Clippy; rustfmt | Pass without suppressions |
| Candidate generation and generated Lean | Pass |
| `continuation_is_not_a_source_call` | Exact continuation constructor and next state; no axioms |
| `exit_is_not_a_source_call` | Exact exit constructor and unit result; no axioms |
| `loop_exits` | Terminates for both Boolean inputs |
| `source_payload_is_preserved` | Returns the original value for every Boolean and U32 input |
| Actual Rust payload mutation | Native test and unchanged payload theorem fail |
| Mutant generation and generated Lean | Pass; a type-correct program can still be wrong |
| `MutationWitness.wrong_payload` | Proves zero result after the continuation on the mutant; refuses on clean candidate |

The two loop-composition laws and wrong-answer witness use exactly Lean's
`propext`, `Classical.choice`, and `Quot.sound`, through the existing
`partial_fixpoint` loop semantics. Every theorem guards its exact axiom set.
The proof input is the freshly compiled, source-extracted module, not an
authored replacement loop. Existing compiled Aeneas primitives remain trusted;
the receipt binds their artifacts but does not claim to rebuild that library.

`wrong-payload.patch` changes the Rust `done` implementation to return zero.
The native test reports `left: 0, right: 1`. The unchanged payload theorem
fails at its final `rfl`, and its axiom guard rejects the resulting `sorryAx`.
The wrong-answer witness passes only on the mutant; it fails at `rfl` on the
clean candidate. This mutation tests the source-value contract, not every
possible corruption of the constructor map.

## Boundaries

These are two branches of one actual loop, not a proof of all loops or of
identifier hygiene generally. The finite Boolean domain makes termination
explicit without testing arbitrary counters. The U32 payload is universally
quantified. No Rust branding, construction invariant, production IR, live
tool pin, or proof admission is changed. UnitDerivative remains1/18.
The existing pilot's loop functions were not re-emitted or rechecked under
this candidate and are outside this supplement. The proof script establishes
the fixture's loop/body connection by unfolding the generated definitions;
the theorem statements specify results, not a general loop-lowering relation.

Full upstream backends, canonical tool adoption and whole DAE Lean admission
remain outside this supplement. The production five-test UnitDerivative
check passed separately at13:03Z; it is not a proof of this translator.
No quick/full success is claimed here. Maintained sources and proofs live
here; generated diagnostic artifacts are under the directory bound in the
receipt and are also included in the durable checkpoint noted in the roadmap.
