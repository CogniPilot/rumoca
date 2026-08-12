# SPEC_0046: Scheduled Discrete Ownership

## Status
DRAFT

## Summary

Scheduled and clocked discrete execution has one owner per activation, one
whole-event candidate/commit relation, and construction-issued identities;
coupled residual cycles are typed rejections, never an invented order.

## Specification

**Sections.** 1 governance · 2 first scope · 3 execution strata · 4 event
attempt · 5 identities · 6 algorithm transactions · 7 activation proof ·
8 counters and phases · 9 state, gates, alternatives · 10 reversal gates.

### 1. Governance, Scope, And Acceptance-Time Amendment Map

This DRAFT proposes the amendments below and claims none today. On acceptance
it amends SPEC_0007's C57 rows and the SOLVE-C32–C38 range, audited clause by
clause under the governance agreement, and the substantive rules of SPEC_0031
that presume a runtime-ordered discrete pass. It also OWNS a gap no active row
covers: whole-event publication has no specified owner today, and §4 becomes it.

Governed: scheduled and clocked discrete ownership — activation strata, the
event candidate/commit relation, discrete identities, algorithm transactions,
activation proofs, and phase counters. Not governed: the Solve grammar, type
algebra, profiles, and identity ladder
([SPEC_0045](SPEC_0045_SOLVE_EXECUTABLE_VOCABULARY_AND_PROFILES.md)); target
refinement and prepared products
([SPEC_0048](SPEC_0048_TARGET_REFINEMENT_AND_PREPARED_PRODUCTS.md)). This spec
composes with them: an `EventInstantExecutionPlan` references compact child
owners by ID and range, its identities extend the SEV-040 ladder, and its
arithmetic resolves under the SEV-024 profile contracts.

Provenance: `dev/2026-08-11-core-structure-decisions.md` §13.1–§13.3 and the
coordination mailbox rulings of 2026-08-12 (12:40, 20:57, 20:58, 22:45, 23:32,
00:04, 00:08, 00:16, 00:24, 00:32, 00:35).

### 2. First Scope And The Rejection Boundary

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SDO-001 | The first scope is PROVED-ACYCLIC scheduled equation owners plus ordinary Appendix-B iteration. | construction | Honest scope beats a silent gap |
| SDO-002 | A legal coupled or nonlinear B.1b residual SCC is a TYPED REJECTION at its owning source spans. Construction NEVER invents a topological order among simultaneous equations. | construction | An invented order is a wrong answer |
| SDO-003 | B.1c assignment cycles remain ILLEGAL, unchanged by this spec. | construction | Already illegal in MLS |
| SDO-004 | SIM-010 stays `Partial` until a compact `ResidualSccOwner` exists carrying its simultaneous tuple, solver contract, activation, rollback, and backend refinement. That owner is the preregistered successor to SDO-002, not an excuse to relax it. | roadmap | A rejection with a named successor |

### 3. Execution Strata

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SDO-010 | Capture precedes execution and owns FOUR distinct history lanes: `LeftLimit`, `SampledLeftLimit`, initial iterative-`pre`, and clock `Previous`. No unified buffer. | construction | Lanes advance differently |
| SDO-011 | Active synchronous base partitions execute ONCE. INDEPENDENT base clocks are PERMUTATION-INVARIANT: no order is invented among them, and none may be observed (MLS §16.5.1.1). | runtime | Independence is not an ordering |
| SDO-012 | Unclocked round 1 follows, where Boolean `sample(start, interval)` owners run once; ordinary Appendix-B iteration continues from there. | runtime | One first pass, then iterate |
| SDO-013 | Actions are STAGED during the strata and published exactly once. | runtime | Publication is not a side effect |
| SDO-014 | Both coincident directions are pinned: Boolean code reading `hold(clockVar)` sees THIS tick's newly solved clock value; a Clock partition sampling a Boolean-updated variable sees its CAPTURED LEFT LIMIT. | construction | The asymmetry is the semantics |
| SDO-015 | `ScheduledActivationId` and `ClockId` are DISJOINT semantic types; neither converts to the other. | `rumoca-ir-solve` | Conflation loses an advancement rule |

### 4. Event Attempt: Candidate And Commit

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SDO-020 | An event instant is one whole-event CANDIDATE/COMMIT relation: every target, history, and action outcome settles in PRIVATE work state, and success publishes atomically. | runtime | Half an event is not an event |
| SDO-021 | Any failure restores ALL of: `Y`, `P`, evaluator/delay/cache state, transaction execution validity, schedule consumption, and histories. A partial restore is a defect, not a degraded mode. | runtime | Rollback is total or absent |
| SDO-022 | **Effect boundary.** `assert`, `terminate`, and status effects are STAGED until commit; a failed attempt emits none of them. | runtime | Retries must not double-report |
| SDO-023 | A transactional impure, random, or external-state effect is admitted only when snapshot-and-replayed; a NON-ROLLBACKABLE effect REJECTS before the attempt begins. | construction | Reject early, not mid-commit |

### 5. Discrete Identities

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SDO-030 | Construction issues one activation and pre-mode CAPABILITY per transaction; a mixed-mode transaction is UNCONSTRUCTIBLE. | construction | Mixed modes are unrepresentable |
| SDO-031 | `EventActionId` and its output projection identity are construction-issued, never derived from position or name. | construction | Authority follows issuance (SEV-040) |
| SDO-032 | ONE STATIC `InvocationOwnerId` per step body, EXECUTED once per active periodic coordinate. Per-tick identity issuance grows the IR with simulated duration and is FORBIDDEN. | construction | IR size cannot track wall time |
| SDO-033 | Event coordinate, entry and history lanes, relation-side probes, the refresh closure, and the consumption token are DISTINCT issued identities. | construction | Distinct roles, distinct identities |

### 6. Algorithm Transactions

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SDO-040 | An algorithm transaction is ONE outer producer exposing the COMPLETE FINAL target tuple. Statement intermediates are visible ONLY within its source-ordered atomic section. | construction | `x := a; x := b(x)` must yield `b(a)` |
| SDO-041 | A cross-owner cycle treats the transaction as ONE OPAQUE BLOCK: reject it, or solve it jointly in a future slice. Splitting or interleaving its statements is prohibited. | construction | Interleaving fabricates a schedule |

### 7. Activation Proof

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SDO-050 | Activation proofs are SYMBOLIC and COMPACT. Hyperperiod expansion tables and per-tick activation bitsets are prohibited. | construction | O(hyperperiod) is unbounded |
| SDO-051 | A phase-shifted cycle whose union of activations is provably empty ACCEPTS on an exact lattice and activation proof; a JOINTLY ACTIVE cycle REJECTS at its owning spans. | construction | False cycles are not real cycles |
| SDO-052 | Coprime periods stay O(owners + compact edges + rank); no proof cost scales with the period product. | construction | Coprimality must not explode |

### 8. Counters And Phases

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SDO-060 | `sample(0, T)` activates only AFTER Modelica initialization completes; it does not fire during initialization. | runtime | Initialization is not tick zero |
| SDO-061 | Activation counters are reported as a THREE-WAY split: Modelica initialization, the estimator initialization arm, and runtime ticks. One total hides the phase defects. | evidence | Three phases, three counts |

### 9. Current State, Gates, And Rejected Alternatives

**None of §2–§8 is implemented.** The migration-period runtime proofs
(uniformity, row-filter) exist in the working tree with named deletion edges;
the one-tick and settle restorations are landed behavior; everything else is
`Absent`. Row-level state, the preregistered gates including the RDD2 estimator
discriminator, and the defeated alternatives are
[SPEC_0047 §7](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#7-scheduled-discrete-ownership-spec_0046);
every row there names the SDO rule it covers.

### 10. Reversal Gates

| ID | Reopening | Requires |
|----|-----------|----------|
| SDO-110 | Admitting coupled residual SCCs (SDO-002) | A compact `ResidualSccOwner` carrying simultaneous tuple, solver contract, activation, rollback, and backend refinement — plus parity against an independent solver oracle |
| SDO-111 | A runtime row cache (rejected) | A design escaping BOTH horns of the dichotomy that defeated it: a cache that can serve a stale replay, or one that re-executes effects. Showing only one horn is not sufficient |
| SDO-112 | Combined first-pass ordering (rejected) | A proof that the combined order is observationally equivalent to the strata of §3 for INDEPENDENT base clocks, which SDO-011 makes permutation-invariant |

## References

- `dev/2026-08-11-core-structure-decisions.md` §13.1–§13.3 — the ratified
  event-strata and ownership record this spec consolidates.
- [SPEC_0045](SPEC_0045_SOLVE_EXECUTABLE_VOCABULARY_AND_PROFILES.md),
  [SPEC_0048](SPEC_0048_TARGET_REFINEMENT_AND_PREPARED_PRODUCTS.md) — the
  grammar, identity ladder, profiles, and product refinement this spec composes
  with.
- [SPEC_0047](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md) §7 — shared
  evidence annex; binding force lives in the SDO rules above, which §9 points
  at their state, gate, and alternative rows there.
- MLS §16.5.1.1 (independent base clocks), Appendix B (iteration).
