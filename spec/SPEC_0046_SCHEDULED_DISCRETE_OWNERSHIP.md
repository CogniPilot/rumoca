# SPEC_0046: Scheduled Discrete Ownership

## Status
DRAFT

## Summary

Scheduled and clocked discrete execution rests on one total lazy `next`
relation and ONE STATIC composition root per Solve event system, reused by every
runtime attempt, whose child occurrence owners settle under a single whole-event
commit; coupled residual cycles are typed rejections, never an invented order.

## Specification

**Sections.** 1 governance · 2 total next · 3 plan and attempt · 4 first scope ·
5 execution strata · 6 event attempt · 7 identities · 8 algorithm transactions ·
9 activation proof · 10 counters · 11 compactness · 12 state, gates,
alternatives · 13 reversal gates.

### 1. Governance, Scope, And Acceptance-Time Amendment Map

This DRAFT proposes the amendments below and claims none today. On acceptance
the voted series amends, atomically and clause by clause, each row verified
against its source: SPEC_0040 **DAE-C07** (typed `sample`/`previous` identities),
**DAE-C17** (event-generating occurrence relations), **DAE-C21** (model-event
transaction), **SOLVE-C11** (event-timing partition), **SOLVE-C22** (compact
event-iteration plan), **SOLVE-C47**, **SOLVE-C48**, **SOLVE-C49** (guarded-
assignment activation, branch lowering, and first-true-arm-otherwise-hold),
**SOLVE-C55** (`EventTransactionProgram`), and **SOLVE-C57**; SPEC_0022
**SIM-010**; and the SPEC_0043 §4 C57 EXTRACTION rows (the whole-clock plan, producer,
proof-and-lowering route, admitted-producer lowering, transaction-exclusion,
SOLVE-C28 boundary, and unowned-row rows) together with its §5 C57 EVIDENCE rows
(the false-narrower-guard HOLD case, reverse-ordered B.1b exchange, mixed
B.1b/B.1c chain, compact-tensor traversal, linear-growth, event-transaction
exclusion, `hold`/`sample` boundary, and unowned-row cases). The HOLD tests and
the transaction-exclusion rows cannot survive the atomic amendment.

SOLVE-C57's **EXCHANGE / HOLD-FALLBACK split cannot survive acceptance**: its
hold-fallback member preserves "no active branch means hold the current target"
as a STORAGE read, which SDO-001 replaces with one total lazy relation. The
split is deleted, not narrowed.

This spec also OWNS a gap no active row covers: whole-event publication has no
specified owner today, and §6 becomes it.

Governed: scheduled and clocked discrete ownership — the total-next relation,
the instant plan and attempt, activation strata, discrete identities, algorithm
transactions, activation proofs, phase counters, and discrete compactness. Not
governed: the Solve grammar, type algebra, profiles, and identity ladder
([SPEC_0045](SPEC_0045_SOLVE_EXECUTABLE_VOCABULARY_AND_PROFILES.md)); target
refinement and prepared products
([SPEC_0048](SPEC_0048_TARGET_REFINEMENT_AND_PREPARED_PRODUCTS.md)).
SPEC_0048 alone owns target-refinement direction. This spec changes C55/C57's
internal scheduled semantics only; it cannot introduce direct DAE →
`SolveAlgorithmBlock`, Solve → GALEC, or parallel production lowering.

Provenance: the accepted ownership decisions consolidated by this
specification and its normative catalog.

### 2. The Total Next Relation

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SDO-001 | **`next = active ? lazy(first-selected RHS, else held-entry) : held-entry`.** ONE compact total relation per PRODUCER COMPLETE RESULT TUPLE or range, with issued projections — never one phi per coordinate and never one per target, which would split a correlated aggregate or an algorithm transaction. Defined at every instant, with no storage-read fallback. | construction | Producers are the unit, not targets |
| SDO-002 | An ordinary same-instant read consumes `next`. ONLY an explicit `pre`, `previous`, or `sample(u)` consumes its named history lane. | construction | One read rule, one exception set |
| SDO-003 | Laziness is semantic, not an optimization: when a target is inactive or its selected arm is not taken, the calls, assertions, folds, and tensor kernels under it execute ZERO times. | construction, runtime | Inactive work is not skipped work |
| SDO-004 | SDO-001 REPLACES SOLVE-C57's EXCHANGE/HOLD-FALLBACK split. No member kind distinguishes a totality-proved producer from an admitted remainder. | construction | One relation needs no member kinds |

### 3. Instant Plan And Attempt

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SDO-010 | `EventInstantExecutionPlan` is an opaque STATIC composition root issued ONCE per Solve semantic root and event system, REUSED by every `EventAttempt` coordinate. Issuing a plan per runtime instant is FORBIDDEN: plan count and IR size must not grow with simulated duration. | `rumoca-ir-solve` | Static count, unbounded instants |
| SDO-011 | Its compact body is stored ONCE, independent of how many consumers read it; a consumer loads the issued definition and never re-lowers, inlines, duplicates, or memoizes the producer graph. | construction | Size grows with producers, not readers |
| SDO-012 | `EventAttempt` is the RUNTIME coordinate and private work state for one instant, and issues no static structure. EXACTLY ONE plan admits and owns each attempt and its outer commit. PER-OCCURRENCE identity is `InvocationOwnerId` (SDO-050) — a child owner, never a plan. | runtime | Occurrences are owners, not plans |

### 4. First Scope And The Rejection Boundary

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SDO-020 | The first scope is PROVED-ACYCLIC scheduled equation owners plus ordinary Appendix-B iteration. | construction | Honest scope beats a silent gap |
| SDO-021 | A legal coupled or nonlinear B.1b residual SCC is a TYPED REJECTION at its owning source spans; no topological order is ever invented among simultaneous equations. | construction | An invented order is a wrong answer |
| SDO-022 | B.1c assignment cycles remain ILLEGAL, unchanged. | construction | Already illegal in MLS |
| SDO-023 | SIM-010 stays `Partial` until a compact `ResidualSccOwner` carries its simultaneous tuple, solver contract, activation, rollback, and backend refinement — the preregistered successor to SDO-021. | roadmap | A rejection with a named successor |

### 5. Execution Strata

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SDO-030 | Capture precedes execution over FOUR distinct history lanes: `LeftLimit`, `SampledLeftLimit`, initial iterative-`pre`, and clock `Previous`. No unified buffer. | construction | Lanes advance differently |
| SDO-031 | Active synchronous base partitions execute ONCE. INDEPENDENT base clocks are PERMUTATION-INVARIANT: no order among them is invented, and none may be observed (MLS §16.5.1.1). | runtime | Independence is not an ordering |
| SDO-032 | Unclocked round 1 follows, where Boolean `sample(start, interval)` owners run once; ordinary Appendix-B iteration continues from there. | runtime | One first pass, then iterate |
| SDO-033 | Scheduled total-next results stay CURRENT across every later Appendix-B pass and NEVER rerun. Only iterative `pre(z/m)` advances between rounds. Witness: scheduled `m = pre(m) + 1` then unclocked `n = pre(m)` must cascade correctly. | runtime | Rerunning a scheduled owner double-counts |
| SDO-036 | A condition-triggered UNCLOCKED algorithm may activate at Appendix-B round `k >= 2`. It consumes the CURRENT scheduled and iterative definitions, and exposes its final tuple to later iterative members. | runtime | Late activation is ordinary iteration |
| SDO-037 | Such an algorithm CANNOT feed a once-only owner: that requires a joint owner or a typed rejection, never a rerun of the once-only owner. | construction | Once-only means once |
| SDO-038 | After convergence the POST-SETTLE SUFFIX runs — actions and outputs consume the SETTLED tuple — and then the one outer commit (SDO-040) publishes. | runtime | Settle, then act, then commit |
| SDO-034 | Both coincident directions are pinned: Boolean code reading `hold(clockVar)` sees THIS tick's newly solved clock value; a Clock partition sampling a Boolean-updated variable sees its CAPTURED LEFT LIMIT. | construction | The asymmetry is the semantics |
| SDO-035 | `ScheduledActivationId` and `ClockId` are DISJOINT semantic types; neither converts to the other. | `rumoca-ir-solve` | Conflation loses an advancement rule |

### 6. Event Attempt: Candidate And Commit

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SDO-040 | An event instant is one whole-event CANDIDATE/COMMIT relation: every target, history, and action outcome settles in PRIVATE work state, and success publishes atomically. | runtime | Half an event is not an event |
| SDO-041 | Abort restores ALL of: `Y`, `P`, evaluator, delay, and cache state; relation and condition memory; random and impure state; integrator invalidation and restart state; FMI lifecycle, termination, and next-event state; schedule consumption; histories; and observable ledgers. A partial restore is a defect. | runtime | Rollback is total or absent |
| SDO-042 | An outcome is exactly `Publish { ordered_staged_effects, terminate? }` or `Abort { fatal_failure }`. `Publish` commits and emits its staged effects once, in ISSUED and SOURCE order within each ordered owner, retaining multiplicity; `Abort` restores and carries no staged fields. | runtime | Abort has no effects to describe |
| SDO-043 | A FATAL failure emits its specified failure ONCE and SUPPRESSES every earlier staged warning, terminate, and status effect of that attempt. | runtime | A failed attempt reports one thing |
| SDO-044 | `assert`, `terminate`, and status effects are STAGED until commit, and a retry duplicates NO effect. | runtime | Retries must not double-report |
| SDO-046 | Two orderings are DISTINCT and neither implies the other: staged effects are ORDERED within each ordered owner (SDO-042), while INDEPENDENT base partitions remain permutation-invariant (SDO-031). Effect order inside an owner is observable; partition order between independent clocks is not. | runtime | Order within, invariance between |
| SDO-045 | A transactional impure, random, or external effect is admitted only when snapshot-and-replayed; a NON-ROLLBACKABLE effect REJECTS before the attempt begins. | construction | Reject early, not mid-commit |

### 7. Discrete Identities

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SDO-050 | `InvocationOwnerId` is per SOURCE CALL OCCURRENCE plus activation, domain, and profile — never per function body. Two same-body calls at one instant are TWO invocation and effect owners sharing ONE immutable relation body. | construction | Occurrences differ; bodies are shared |
| SDO-051 | Identity is issued STATICALLY and executed once per active coordinate. Per-tick identity issuance grows the IR with simulated duration and is FORBIDDEN. | construction | IR size cannot track wall time |
| SDO-052 | `EventActionId` and its output projection identity are construction-issued, never derived from position or name. | construction | Authority follows issuance (SEV-040) |
| SDO-053 | Event coordinate, entry and history lanes, relation-side probes, the refresh closure, and the consumption token are DISTINCT issued identities. | construction | Distinct roles, distinct identities |

### 8. Algorithm Transactions

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SDO-060 | An algorithm transaction is ONE outer producer whose COMPLETE FINAL target tuple is one value of the SDO-001 form. Statement intermediates are visible ONLY within its source-ordered atomic section. | construction | `x := a; x := b(x)` must yield `b(a)` |
| SDO-061 | A cross-owner cycle treats the transaction as ONE OPAQUE BLOCK: reject it, or solve it jointly in a future slice. Splitting or interleaving its statements is prohibited. | construction | Interleaving fabricates a schedule |

### 9. Activation Proof

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SDO-070 | Activation proofs are SYMBOLIC and COMPACT. Hyperperiod expansion tables and per-tick activation bitsets are prohibited. | construction | O(hyperperiod) is unbounded |
| SDO-071 | A cyclic edge set is FALSE when the JOINT SATISFIABILITY — the INTERSECTION — of its edge activations is empty. Such a cycle ACCEPTS on an exact lattice proof; a jointly active cycle REJECTS at its owning spans. | construction | A union is never the joint condition |
| SDO-072 | Schedules are NORMALIZED EXACT AFFINE relations fixed after initialization. No epsilon comparison, no repeated-`f64` drift, and no near-instant merge: two equal-lattice occurrences stay DISTINCT. | construction | Drift silently merges instants |
| SDO-073 | A tunable parameter change REISSUES the activation certificate; a stale certificate is never reused across the change. | construction | Tunables move the lattice |
| SDO-074 | Coprime periods stay O(owners + compact edges + rank); no cost tracks the period product. | construction | Coprimality must not explode |
| SDO-075 | Session admission CONSUMES every normalized periodic schedule into one opaque checked schedule carrying its exact lattice, its absolute anchor resolved once against the session start, and a proof that every tick through the finite admitted horizon fits exact tick-index arithmetic. Failure to prove that bound is a typed pre-execution error; runtime tick issuance is infallible over the admitted domain, and absence means only "no tick in this horizon." Exactly ONE semantic tick-membership relation is issued and consumed by every semantic scheduler, activation, history, transaction, and visibility user. No consumer re-derives a lattice from `f64`, substitutes float arithmetic after an exact failure, treats an error as nonmembership, or compares semantic membership with another tolerance. A distinct numerical-arrival policy may compare an approximate located root or integrator coordinate with an already-issued exact tick, but it never defines schedule membership. | session admission and runtime | Prove once, execute exactly and cheaply |

### 10. Counters And Phases

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SDO-080 | `sample(0, T)` activates only AFTER Modelica initialization completes. | runtime | Initialization is not tick zero |
| SDO-081 | Activation counters report a THREE-WAY split: Modelica initialization, the estimator initialization arm, and runtime ticks. | evidence | One total hides phase defects |

### 11. Discrete Compactness

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SDO-090 | Root, wire, preparation, AND stored bodies stay O(compact bodies + owners + edges + rank/ranges). No tensor-coordinate node is constructed at any of those layers. | construction | Four layers, one bound |
| SDO-091 | Inherent model state, input, and output tensor PAYLOAD storage scales with source extent legally. What SDO-090 prohibits is extent-DERIVED METADATA in the semantic graph, wire, preparation, or stored body. Selected-branch work and transient payload scale at execution. | runtime | Payload is data, metadata is structure |

### 12. Current State, Gates, And Rejected Alternatives

**None of §2–§11 is implemented.** The migration-period runtime proofs
(uniformity, row-filter) exist in the working tree with named deletion edges,
and the one-tick and settle restorations are landed behavior.
`SolveRuntimeSnapshot` ALREADY captures the static refresh cache, evaluator
random and impure state, and delay state: the defect is the ABSENCE of one
enclosing `EventAttempt` that invokes it on EVERY failure path, plus the absent
observable ledgers. Everything else is `Absent`. Row-level state, the preregistered gates including the RDD2 estimator
discriminator, and the defeated alternatives are
[SPEC_0047 §7](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#7-scheduled-discrete-ownership-spec_0046);
each row there names the `SDO` rule it covers.

### 13. Reversal Gates

| ID | Reopening | Requires |
|----|-----------|----------|
| SDO-110 | EXCHANGE/HOLD-FALLBACK over total next (SDO-001) | A same-instant read whose storage-fallback semantics are observationally distinguishable from `next` AND sound — the split's own unsoundness is what defeated it |
| SDO-111 | A universal execution order over the activation-aware proof | A proof that one total order preserves every permutation-invariant observable of SDO-031, for INDEPENDENT base clocks MLS leaves unordered |
| SDO-112 | One re-lowered body over opaque child composition | A product whose child owners cannot compose, plus evidence the giant body does not grow with consumer count (SDO-011) |
| SDO-113 | A journal over the private-arena attempt | Observational-rollback equivalence on every SDO-041 category, plus ≤2% prepare-time regression on the named canary |
| SDO-114 | One history buffer over four distinct lanes | A proof that `LeftLimit`, `SampledLeftLimit`, iterative-`pre`, and `Previous` advance identically at every coincident instant |
| SDO-115 | Admitting coupled residual SCCs (SDO-021) | A compact `ResidualSccOwner` carrying simultaneous tuple, solver contract, activation, rollback, and backend refinement, plus parity against an independent solver oracle |
| SDO-116 | A runtime row cache | A design escaping BOTH horns of the dichotomy that defeated it: a cache serving a stale replay, or one re-executing effects. One horn is not sufficient |

## References

- [SPEC_0043](SPEC_0043_CONSTRUCTION_CATALOG.md) — the normative construction
  catalog for the event-strata and ownership rules consolidated here.
- [SPEC_0045](SPEC_0045_SOLVE_EXECUTABLE_VOCABULARY_AND_PROFILES.md),
  [SPEC_0048](SPEC_0048_TARGET_REFINEMENT_AND_PREPARED_PRODUCTS.md) — the
  grammar, identity ladder, profiles, and product refinement this spec composes
  with.
- [SPEC_0047](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md) §7 — shared
  evidence annex for §12.
- MLS §16.5.1.1 (independent base clocks), Appendix B (iteration).
