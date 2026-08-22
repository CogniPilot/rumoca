# SPEC_0037: Verification Architecture for Checkers and Semantics

## Status
DRAFT

## Summary

Verification effort is spent on three things: making invalid IR
unrepresentable, checking transformations against witnesses, and proving the
checkers and the semantics. A machine-checked theorem about the whole compiler
is a non-goal.

## Governing Theorem

For every model M, if the compiler accepts M and emits artifact A, then
simulating A agrees with the meaning of M under the reference semantics (§5).
Every mechanism in this spec exists to make some fragment of that statement
checkable; machine-checking the whole of it is a non-goal (§4), and §7 records
which fragments hold today.

"Agrees" reads in three layers, and a correctness claim MUST name the layer it
makes.

| Layer | What agreement means | Where it is discharged |
|---|---|---|
| Symbolic system | A runs exactly the equations, events, and discrete updates M denotes: same unknowns, same conditions, same update order. The standard is exactness, so any symbolic difference is a defect | Checkers, witnesses, and the differential harnesses |
| Numeric evaluation | A evaluates those expressions in IEEE 754 arithmetic at the precision it declares, so agreement holds per artifact and per declared precision rather than over the reals | Assumed by name in every claim that reaches numbers (§6) |
| Solver approximation | Nothing. The trajectory a step controller produces from A is not a compiler claim | Outside the theorem |

Refusal preserves soundness. The theorem quantifies only over models the
compiler accepts, so refusing M can never falsify it. Any gate here may
therefore refuse, and a checker that cannot classify its input is required to
(§2). The one failure the theorem admits is an accepted M whose artifact
disagrees.

## Specification

### 1. Division of Labor

Three mechanisms cover three kinds of obligation. Each is the cheapest one for
its own kind, and none is asked to do another's work.

| Obligation | Mechanism | Owner |
|---|---|---|
| IR well-formedness | Correct by construction in the Rust type system | [SPEC_0036](SPEC_0036_VALID_BY_CONSTRUCTION_IR.md), catalog [SPEC_0043](SPEC_0043_CONSTRUCTION_CATALOG.md) |
| Transformation correctness | Translation validation: untrusted producer, witness, trusted checker | producing phase; [SPEC_0039](SPEC_0039_PROOF_CARRYING_SPARSITY.md) is the in-repo precedent |
| Checker and semantics correctness | Machine-checked proof | this spec |

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| A representable invalid state is a defect, not a validation target | `rumoca-ir-*` | Unrepresentable costs no proof |
| Validators do not define successful phase states | phase APIs | Validity is established at construction |
| Fail-closed `Result` belongs at trust boundaries, not at every call | phase and crate entry points | A `Result` per call site is proof surface |
| An optimizer is untrusted and emits a witness | transformation phases | Search stays fast and unproven |
| The checker, not the producer, enters the trusted base | checker modules | Only small code is provable at this cost |
| A witness is a type with a private constructor, not a boolean | witness owners | Possession is the evidence |
| A transformation with no witness runs outside verified mode | transformation phases | Unproven work stays labelled |
| Provers verify checkers and semantics, never whole passes | this spec | Per-pass theorems do not repay their cost |

### 2. Checker Discipline

These rules are normative for every checker that enters the trusted base. One
checker body must serve both bounded model checking and a later Rust-to-Lean
functional translation (Aeneas-style) without being rewritten for either.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| A checker is a pure function of its arguments | checker core | A proof needs a value, not an environment |
| A checker is total over its declared domain | checker core | A panic is an unproven case |
| The declared domain is written down and enforced at the boundary | checker entry | Totality means nothing without a domain |
| No `unsafe` | checker core | Neither tool models it |
| No trait objects and no closures in the core | checker core | Higher-order terms are what both tools handle worst |
| Data structures are concrete and bounded | checker core | Bounded model checking needs a finite footprint |
| Recursion is structural over an owned value | checker core | Termination has to be visible |
| A checker is substantially smaller than the producer it checks | checker modules | Size is the whole argument for trusting it |
| A checker's acceptance is a witness type, not a boolean | checker API | A bare `true` composes with anything |

**Fail-open composition is prohibited.** Absence of a refusal is never
permission. A guard set concluding "no predicate objected, therefore allow" is
a defect even when every predicate is individually correct, because the
composition grants what none of them proved. A checker MUST derive acceptance
from a positive, exhaustive case analysis of its input, and every case it does
not handle MUST refuse by name.

### 3. The Verification Ladder

A checker climbs one rung at a time. Each rung states its entry criterion, and
a checker MUST NOT be described as verified above the rung it has reached.

| Rung | Entry criterion | What it establishes |
|---|---|---|
| L1 differential coverage | The question is stated a second time, independently, and both statements run over an enumerated corpus | Disagreements are classified by direction, and the unsafe direction asserts |
| L2 witness gating | The checker refuses in production and no consumer proceeds without its acceptance | A rejected artifact cannot reach a later phase |
| L3 Kani bounded proof | An admissible manifest entry (§3a) over a symbolic domain | The property holds for every input inside a declared bound |
| L4 Lean unbounded proof | The checker is inside the §2 subset and its specification is stated in the selected assistant | The property holds for every input in the domain |

The first instance is the loop-compaction store-deletion checker in
`rumoca-phase-dae`, at L1. Dataflow liveness states the "is this stored value
ever read again" question once, an enumerated program corpus answers it by
execution, and the syntactic deletion predicates are measured against both.
Disagreement in the delete-a-live-store direction asserts; the opposite
direction is a missed deletion and is recorded.

#### 3a. Kani Harness Admissibility

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Every required harness has a checked-in manifest entry | `infra/verification/kani-proofs.json` | CI runs the manifest, not a directory scan |
| An entry names its production kernel, symbolic inputs, enumeration barrier, counterexample meaning, assumptions, and bounds | manifest schema | An unexplained bound is not evidence |
| A harness over a small finite table, or without symbolic input, is inadmissible | verification review | Exhaustive ordinary tests are cheaper and stronger there |
| `cargo xtask verify kani` verifies one harness at a time | verify command | The driver's interleaved output cannot bind a result block to its harness |
| Unwinding checks stay enabled and an incomplete proof fails the command | verify command | Timeout, cancellation, and unwinding failure are not evidence |
| The result records verifier version, harness, declared bound, elapsed time, and outcome | verify command | A proof claim is reproducible or it is nothing |
| No shell substitutes a property-test fallback when the verifier is absent | toolchain pins | A green fallback is validation evidence, never proof evidence |
| Removing or renaming a required harness updates its owning spec and manifest | change author | A silently dropped proof is a regression |

A manifest entry states a bounded property of one kernel. It never implies a
claim about the compiler.

### 4. Prover Targets

| Priority | Target | Why here |
|---|---|---|
| 1 | Kani on each checker as it lands | Bounded proof is cheap enough to be routine, and it runs on the Rust that ships |
| 2 | Lean semantics of GALEC | Smallest language in the system, certification-facing, and its traps are enumerated in [SPEC_0042](SPEC_0042_GALEC_LANGUAGE_CATALOG.md) |
| 3 | Lean semantics of the `rumoca-reference` slice | The definitional interpreter is already written to be transliterable (§5) |
| Non-goal | A machine-checked theorem for the whole compiler | Stated below |

The whole-compiler proof is a non-goal, not a deferred goal. Its top-level
statement would have to refine the behaviors the Modelica Language
Specification assigns to a source model, and that specification is prose with
tracked silences ([SPEC_0022](SPEC_0022_MLS_COMPILER_COMPLIANCE.md)), so there
is no formal object to refine toward. Under this architecture the proof surface
is the checkers plus the semantics, and it stays small on purpose.

### 5. Definitional Semantics

`rumoca-reference` ([SPEC_0041 §4](SPEC_0041_CRATE_OWNERSHIP_CATALOG.md#4-layering-ownership-catalog-spec_0029-12))
is the definitional semantics of the Modelica discrete/event core: the
statement of meaning the differential harnesses compare against, and the object
a Lean transcription would transcribe.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| The reference is the definition; a disagreement means one side is wrong | `rumoca-reference` | It is the side auditable by reading |
| Its `[dependencies]` section names no `rumoca-*` crate | `rumoca-reference/Cargo.toml` | A reference linking the compiler would agree with it by construction |
| Its `[dependencies]` section is otherwise an allowlist | architecture gate | Whatever it imports joins the semantics' trusted base |
| Compiler dependencies are dev-only, for the differential harness | `rumoca-reference/Cargo.toml` | Nothing a consumer links pulls the compiler in |
| Making it faster, cleverer, or more general is a defect | `rumoca-reference` | Obviousness is the property being bought |
| A model outside the covered slice is refused by name | `rumoca_reference::simulate::RefError` | An oracle that guesses lets the compiler agree for the wrong reason |
| Differential agreement is validation evidence, not proof evidence | this spec | Two implementations can be wrong together |

The refused set is the boundary a Lean transcription inherits: the slice the
semantics defines is exactly the set of models `RefError` does not name, so
widening the slice changes what the definition claims. Growing the
dependency allowlist is a deliberate enlargement of the trusted base, and fails
the architecture gate until it is recorded there.

### 6. Trusted Computing Base

Every verification claim names the rows of this table it rests on.

| Component | Required treatment | Why |
|---|---|---|
| Proof-assistant or verifier kernel | Pinned version | It checks everything else |
| Definitional semantics | Reviewed, auditable, dependency-gated (§5) | It defines what correct means |
| Checkers | §2 discipline, then proof | They stand in for the producers |
| Witness formats | Versioned, decoded fail-closed | A misread witness proves nothing |
| Rust toolchain and linker | Pinned and trusted | Source to binary stays unproven |
| Target and runtime semantics | Stated contract | It fixes what is observed |
| Numerics and hardware floating point | Stated model or assumption | It bounds every trajectory claim |
| External functions | Typed contract or refusal | Arbitrary code is not proven |

### 7. Status

This section is the inventory, and a row moves only with the evidence it names.

| Item | Status |
|---|---|
| `advance_event_iteration_lanes` event-history advance | Proven by one Kani harness under a declared unwind bound |
| Kani driver, manifest schema, pinned verifier, CI job | Scaffolding, exercised by that single manifest entry |
| Loop-compaction store deletion | L1: differential corpus, no witness gating |
| DAE wire round trip | Property-tested, not proven |
| Translation validation | SPEC_0039 defines a witness; no phase yet ships a producer and checker pair under it |
| Definitional semantics | Slice 1 implemented and differentially validated, not transcribed |
| Lean | Not started: no assistant selected, nothing transcribed |
| Whole-compiler theorem | Non-goal (§4) |

Anything not named above is unverified. DRAFT status means work has started
under this spec, not that any claim in it holds.

## Promotion Criteria

| Requirement | Evidence |
|---|---|
| Proof assistant selected | Recorded evaluation and maintainer vote |
| One IR semantics implemented | Executable formal definition |
| Checker discipline gated | Mechanical enforcement of §2 over trusted-base checkers |
| One checker proven | A reproducible bounded proof, then an unbounded one |
| Trusted computing base reviewed | Published assumptions per claim |
| Proof CI bounded | Reproducible runtime and resource report |

## References

- [SPEC_0007](SPEC_0007_IR_PIPELINE.md), production phase contracts.
- [SPEC_0022](SPEC_0022_MLS_COMPILER_COMPLIANCE.md), MLS contract index and its recorded silences.
- [SPEC_0033 §6a](SPEC_0033_DEVELOPMENT_PROCESS.md#6a-two-tier-verification-cadence), OMC/MSL cadence, which produces validation evidence.
- [SPEC_0036](SPEC_0036_VALID_BY_CONSTRUCTION_IR.md), construction rules for the IR side.
- [SPEC_0039](SPEC_0039_PROOF_CARRYING_SPARSITY.md), the in-repo translation-validation precedent.
- [SPEC_0041](SPEC_0041_CRATE_OWNERSHIP_CATALOG.md), ownership row for the reference-semantics crate.
- [SPEC_0042](SPEC_0042_GALEC_LANGUAGE_CATALOG.md), GALEC traps a semantics has to discharge.
- [CompCert semantic preservation](https://compcert.org/man/manual001.html)
- [CakeML verified compiler](https://cakeml.org/)
- [Aeneas: Rust verification by functional translation](https://dl.acm.org/doi/10.1145/3547647)
- [Kani Rust verifier](https://model-checking.github.io/kani/)
