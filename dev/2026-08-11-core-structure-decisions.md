# Core Compiler Structure — Proposed-Final Decisions

**Date:** 2026-08-11
**Status:** Claude's finalized negotiation position (user-directed). Becomes
binding only via the mailbox negotiation with Codex and the resulting spec
amendments (governing rules into ACCEPTED parents, catalogs into annexes).
**Question answered:** is this the optimal shape and procedure for a formally
verified, concise, maintainable, efficient, modern Modelica compiler — and
where it is not, what is the final shape new code should land on from today.

## 0. What is settled and defended (no negotiation sought)

These carry a week of adversarial evidence; changing them requires
counter-evidence, not preference:

- **Staged IR pipeline** AST → Flat → DAE → Solve with opaque phase proofs and
  valid-by-construction roots; DAE is the canonical MLS Appendix-B RUNTIME
  semantic anchor. (Codex wording correction accepted: source-language
  semantics and frontend refinement are defined and proved at the
  AST/resolve/type/instance/Flat stages; DAE is not literally the only place
  language semantics exist.)
- **Construction-issued authority** (the SOLVE-C56 doctrine): no runtime
  rediscovery, no body/name/span/hash/pointer identity, wire decodes only by
  replaying checked constructors. Every serious defect found this week (dead
  rotational dynamics, clocked one-tick lag, forgeable sparsity patterns,
  wire smuggling) was a violation of exactly this doctrine.
- **Tensor-native, range-preserving IR** (SPEC_0032): no expand-then-recover.
- **Codegen split**: Rust owns semantic context and domain decisions;
  MiniJinja spells target syntax; targets are manifest-declared products with
  fail-closed unsupported paths. First-class support partials (landed) are
  part of this shape.
- **FMI 3 ME as the sole solver-facing semantic protocol**, conditional per
  Codex's accepted correction: the in-process trait must stay isomorphic to
  the standard state machine, statically devirtualizable, and must never
  block the compact batched directional/dependency access an implicit solver
  needs — it is a projection of the checked kernel, not the compiler's
  canonical executable IR.
- **eFMI dataflow (amended per Codex counterposition 3, accepted):** shared
  typed executable regions precede BOTH GALEC and Production C. Phase Solve
  issues the one set of typed program owners; GALEC admissibility/projection
  REFERENCES those owners and adds only GALEC lifecycle/language
  correlations; `SolveAlgorithmBlock` binds storage/effects/arithmetic
  profile as a checked executable PROJECTION over the same regions — never a
  second lowered body; `.alg` and Production C render their respective
  checked projections. This supersedes the earlier
  `DAE → AC → re-lowered SolveAlgorithmBlock` reading of the 2026-08-08 plan
  and closes GAL-038's duplicate-lowering class structurally.
- **Independent differential oracles** (eval-galec vs eval-solve vs generated
  C) as release gates for the flight path.
- **Process**: spec-first for new invariants; red-proof-first fixes;
  two-stream adversarial review; green-or-revert slices; the live mailbox.

## 1. One executable vocabulary (the LinearOp end-state)

**Decision (proposed final):** the typed program vocabulary becomes the ONLY
canonical Solve program form. `LinearOp` is demoted to a private,
backend-boundary lowering detail and is deleted from the canonical IR and the
wire when the migration ladder below completes. No new semantic feature may be
added to `LinearOp`; new semantics land as typed operations only, effective
immediately.

Rationale: dual vocabularies are the largest standing incoherence risk; the
f64-register / Boolean-as-0.0 encoding is the proof-hostile surface SPEC_0037
will otherwise have to model twice; the migration plan (2026-08-08) already
names this end-state — this decision only makes it enforceable now.

Migration ladder (each rung spec-first, certificate-carrying, canary-gated):
1. Freeze `LinearOp` semantically (LANDED: variant-count assertion in the
   architecture hardening test, frozen at 50). Rename `LinearOp` -> `ScalarOp`
   as a dedicated post-release mechanical commit (user finding: "LinearOp"
   reads as linear-operator/LTI to control engineers while the enum is a
   straight-line scalar instruction set, and the file even contains
   `LinearSolveComponent` where "linear" means linear algebra — one word, two
   meanings). `ScalarOp` aligns with SPEC_0007's mandated
   ScalarProgram/ScalarProgramBlock terminology. The rename is wire-neutral
   (serde tags by variant name) and template-neutral (kind_name() returns
   variant names); it is a Rust-identifier-only sweep (~5k refs) plus the
   linear_op.rs -> scalar_op.rs file move and SPEC_0040 prose sync.
2. Close the typed gap families (see evidence inventory below) as typed ops
   with construction proofs: tables, random draws, remaining store/output
   range forms, any fold/conditional legacy forms without typed equivalents.
3. Move residual/derivative production to typed programs; backends COMPILE OR
   INTERPRET the typed structured regions DIRECTLY (Codex correction
   accepted: no stored second program anywhere; a target may derive a
   BORROWED scalar projection only where its ABI genuinely requires scalar
   observations; cranelift emits bounded loops/calls from Map/Fold/tensor
   intrinsics; the interpreter dispatches the same owners). During migration
   `LinearOp`/`ScalarOp` is an ADAPTER only — never a permanent private
   backend IR replicated across backends.
3b. Proof authorities (structural_pattern, refresh) re-anchor their
   derivations on typed programs (no second stored semantic root).
4. Wire cutover: SOLVE schema serializes typed programs only; no row forms on
   the wire, no reconstruction into stored rows, no compatibility readers.
5. Delete the legacy scalar lowering path (`lower/scalar` families) once
   `typed_functions` + the C57 owner cover its inputs; delete `LinearOp`.

**EVIDENCE (consumer inventory & gap families):** landed — see Appendix A.
Decisive facts: ~60-65k coupled lines but typed coverage already 31/31 in
both evaluator and JIT; 24/50 variants lack typed equivalents with the
schedule set by storage classes + result boundary + block-level JVP; the wire
cutover is one affordable big-bang; the freeze is enforced in the
architecture test as of today.

## 2. One discrete-execution ownership story

**Decision (proposed final):** exactly three executable discrete owners, all
fed by one structural authority, with written seam rules:

- **Authority:** `CausalDiscretePlan` (structural) issues order, rank,
  intermediate definition identities, and activation-totality facts for
  everything it admits; it is the only order/dependency oracle.
- **Owner A — event transactions (C21/C55):** model-algorithm shaped mixed
  targets; atomic commit; reaching definitions substituted at construction.
  To gain (from the P0 review): assert-atomicity inside the transaction.
- **Owner B — clock-partition transactions (C57):** equation-shaped clocked
  B.1b/B.1c; producers lowered once; consumers load issued intermediates.
  MECHANISM (decision text aligned with the accepted candidate per Codex
  19:55.2 — the earlier "hold-fallback keeps storage semantics" wording was
  contradictory and is withdrawn): every guarded producer issues one TOTAL
  lazy next-value definition, `next = active && guard ? rhs : held_entry`;
  ALL ordinary same-instant reads consume `next`; only explicit
  history/sample boundaries read entry storage. Hold semantics are thus the
  `held_entry` branch OF the phi, not a separate storage-read path.
  `PeriodicClockId` is activation metadata; coincident `when sample(...)`
  periods get one event-instant causal solution. Carve-outs stand: C28
  sample boundaries and Owner-A targets.
- **Owner C — MLS event iteration:** the Appendix-B fixed point over
  unclocked discrete rows; clocked lanes excluded per SIM-010 (first
  iteration solving governs re-iteration only, not intra-partition exchange).

Seam rules to write into the parent spec: a coordinate has exactly one owner
per instant; reads across owners are left-limit unless the authority issued a
same-instant edge; runtime holds/locks may only implement owner semantics,
never define them. The one-tick lag existed because a seam rule was implicit.

**EVIDENCE (ownership map & seam list):** landed — see Appendix B. Decisive
facts: the proved causal order currently has zero consumers (GALEC rebuilds
its own and uses it; Solve snapshot-evaluates); same-instant exchange exists
only in model-event transactions; eight located seams, two of them new
pre-C57 code-defect leads (coupled-path lock inconsistency; overloaded
iteration counter).

## 3. Conciseness by deletion (the kill-list)

**Decision (proposed final):** deletion is sequenced work with cutover proofs,
never compatibility. Ordered list:
1. Second GALEC function lowerer (`phase-galec/lower/user_functions.rs`) —
   deleted at M3 when the shared typed lowerer feeds GALEC admissibility
   (SPEC_0041 §4 one-lowerer rule becomes satisfiable).
2. Legacy scalar function path (`lower/scalar/functions.rs`, 4.3k lines) —
   deleted at vocabulary ladder rung 5.
3. Rust-side MLIR/WGSL renderers — replaced by typed views + templates per
   SPEC_0029 §12, or the §12 rule is renegotiated honestly; the current state
   (rule says one thing, code does another, no CI) is the only unacceptable
   option.
4. `c-ode` target demoted from user-visible once FMI 2/3 readiness rises
   (SPEC_0038's own rule).
5. Orphan/deferred surfaces: dead `target_symbols` (deleted), deferred spec
   crates audit (0028-class), exec backends that lack a product contract.
Crate-count target: no numeric fetish, but every crate must name its owner
spec row and its consumer; anything that cannot is a deletion candidate.

## 4. Certificates as stage contract (proof strategy)

**Decision (amended per Codex counterposition 2, accepted):** proof strategy
follows SPEC_0037's own selection table rather than a universal certificate
badge. UNIVERSAL for every checked owner: a decidable well-formedness
predicate and an explicit construction/refinement obligation. SELECTIVE: a
separate certificate only where the producer is substantially more complex
than a small checker (search-heavy resolution/structural algorithms);
translation validation for optimization/codegen; direct functional proof for
small constructors/evaluators; Kani ONLY for genuinely symbolic bounded
kernels, never a default. Certificates are ephemeral phase evidence — never
duplicate mutable IR fields; execution plans are semantic data. Retrofit
order by proof value: refresh/dependency authority, pattern authority
(landed), event transactions, typed call table, wire replay completeness.
`rumoca-reference` differential coverage becomes a CI lane for the discrete
core (W1/W2), not an aspiration.

**Naming glossary (USER-RATIFIED 2026-08-11):** legacy enum `LinearOp` ->
`ScalarOp` (names the limitation; migration adapter only; DELETED at ladder
end; wire/template-neutral rename). The FINAL sole canonical vocabulary:
`SolveOperation` -> `SolveOp` — everything becomes `SolveOp`.
Tensor-nativeness lives in the VALUE/TYPE layer (every value a typed tensor,
rank-0 included — TensorShape/exact domains); the operation set also carries
control/effect ops (Branch/Invoke/Limit/Signal) that are not tensor
operations, so the vocabulary name is `SolveOp` with the value-computing
subset keeping `Tensor*` variant names. `TensorOp` as the whole-vocabulary
name was considered and set aside as over-claiming. User approved this end
state explicitly ("everything becomes SolveOp; ScalarOp deleted").

## 5. Performance architecture under the 56.5 ms floor

**Decision (proposed final):** the remaining path to 10x is architectural and
lands in this order, each measured on a clean committed tree:
1. Codex's guarded whole-clock causal owner + exact refresh/remainder
   certificates (event work partitioned by proof, not by sweep).
2. Typed-owner directional/JVP completion so derivative/root tuples stop
   re-deriving through legacy folds (the 27.9 + 28.6 ms floor is attacked
   here, nowhere else).
3. Source-issued semantic linear-algebra intrinsics — `SolveSPD` first, via
   spec row + typed op + per-backend kernel; never name/body recognition.
4. Checked native region boundaries so SSA live ranges are bounded (the
   giant-SSA experiment's lesson made structural).
No optimization that cannot state its certificate obligation enters the tree.

## 6. Constitutional fix (spec governance)

**Decision (proposed final):** one rule change to SPEC_0000: a REFERENCE
annex may hold only rows whose governing MUST lives in an ACCEPTED/DRAFT
parent sentence; the parent sentence counts against the parent's budget; an
annex row without a parent anchor is void. This closes the budget-escape
loophole without losing the annex mechanism. Numbering, statuses, pending
markers (with resolvable referents), and the drift-guard tests stay as
landed this week.

## 7. What new code must assume from today (freeze rules)

Until the negotiation concludes, all new code lands on the proposed-final
shape — this is the "less likely to change" guarantee the user asked for:
- no new `LinearOp` variants; new semantics are typed ops;
- new discrete behavior names its owner (A/B/C) and consumes
  `CausalDiscretePlan` facts, never re-derives order;
- new checked owners ship certificates + manifest assumptions;
- new targets use manifest partials/`shared_as`, no Rust target dispatch;
- deletion candidates receive no feature work, only cutover work.

## 8. Codex counter-agenda (18:56 EDT) — incorporation status

Codex agrees in principle with items 1-5 subject to consumer-proof (the
pending evidence appendices are exactly that proof), and adds eight items.
Claude's incorporation:

- **ACCEPTED into item 1** (strengthens it): the canonical program preserves
  Map/Fold/AffineStencil/tensor ops and exact domains NATIVELY; demotion of
  `LinearOp` must never route a compact owner through scalar SSA; the final
  backend lowering is a checked lazy target view, never a second stored
  semantic root. The vocabulary decision is made on deletion cost, tensor/
  domain expressivity, AD/JVP ownership, event/effect semantics, target
  admissibility, and backend profiles — the appendix inventory supplies these
  axes, not IR age.
- **ACCEPTED as new item 1b — four-identity separation, now with the ratified
  concrete design (Codex 19:15 identity review):** root-branded
  `SemanticValueId` (typed, span-free, effect-free value relation),
  `OccurrenceId` (every exact span/use), `InvocationOwnerId` (callee, ordered
  args, activation/dominance/clock/domain coordinate, parent, captures,
  effects/profile), `OutputProjectionId` (owner + result-or-assertion
  ordinal/type). Pattern receipts bind the compute owner and exact
  input/output projections. Wire replays operations and REISSUES all handles
  — no raw ordinal is ever a claim. CSE/hash-consing permitted only on
  `SemanticValueId`, key O(arity) over canonical operand ids; it never merges
  occurrences nor authorizes execution reuse/hoisting; calls, effects,
  temporal/event owners, runtime-coordinate invocations stay distinct even
  when value-structurally equal. Codex's supplied negative-test list
  (cross-table rebinding, reordered call/fold projections, domain/profile/
  NaN payloads, wire swaps, pattern-receipt swaps) is the acceptance
  evidence. LIVE P0 DRIVING THIS: `SolvePureCallTable::matches_site` accepts
  a same-ABI site from another table and EXECUTES THE WRONG BODY; plus
  projection-insertion-order call ownership, adjacent-ordinal fold
  correlation, and owner-unbound pattern receipts — all queued as the
  identity-hardening slice, red-proof first.
- **ACCEPTED into item 4 (hardens it):** one opaque branded replayable
  aggregate per phase; public child deserialization, `Default`, mutable proof
  fields, and validate-after-assembly are disallowed for future formal
  claims; certificates are reissued from canonical constructor inputs or
  checked against exact branded owners — never caller assertions. (This is
  the SPEC_0036 end-state made a negotiation invariant; SolveProblem/
  flat::Model retrofits get sequenced in the migration order.)
- **LEADING CANDIDATE for item 2's mechanism:** Codex's global scheduled-
  event causal solution for coincident `when sample` owners + separate MLS
  synchronous-clock base/subpartition topology, with `PeriodicClockId` as
  activation metadata and guarded producers issuing a TOTAL LAZY NEXT-VALUE
  PHI (ordinary current reads see the phi; only explicit history/sample
  boundaries see entry storage). This is a stronger unifying mechanism than
  the bare three-owner sketch; adopted as the working design pending the
  seam-map appendix, with my seam rules retained as its acceptance
  conditions.
- **ACCEPTED into item 5:** backend selection is a refinement proof — checked
  target-profile root, identical arithmetic/effect/status semantics,
  compile-before-hot-path `NativeRequired`, zero hidden interpreter calls.
- **ACCEPTED as new item 9 — upstream compact families:** Source/Instance IR
  becomes compact-family native; the SPEC_0032 per-element Instance authority
  mandate is amended before implementation credit (consistent with the review
  finding that Flat still owns O(N) scalar rows).
- **ACCEPTED into item 3:** GALEC/C consume a checked subset/projection of
  the one executable root; the second DAE→GALEC lowerer and template-time
  semantic repair die at cutover, no fallback retained.
- **ACCEPTED as item 10 — acceptance canaries:** million-point compact
  families staying O(owner+ranges) before final text emission; exact event
  and call-occurrence counterexamples; wire mutation/rebinding rejection;
  complete interpreter/native parity; current RDD2 `NativeRequired`
  timing/counters. Architecture claims are accepted only against these.
- **AGREED document shape:** this file evolves into the joint architecture
  decision record with Codex's required sections (current-state map,
  invariants, alternatives/tradeoffs, accepted decisions, spec amendments,
  crate dependency graph, migration/deletion order, evidence gates) during
  the negotiation session.
- **AGREED sequencing:** negotiation convenes after the post-release hostile
  review; it blocks the NEXT broad implementation batch, not the current
  bounded workers, which finish or revert into small signed checkpoints.

## Negotiation state

Posted to the mailbox 18:55 EDT as the six-item agenda; this document is the
fleshed-out position. Codex counter-positions pending. Agreed items graduate
to spec amendments; contested items get an evidence task, not a stalemate.

## Appendix B — Discrete-execution ownership map and seam list (evidence)

Gathered read-only from the current tree (agent survey, 2026-08-11 19:10 EDT).
Framing facts:

1. **Six owners of "what a discrete/clocked value is at instant t"**, unified
   by no single artifact. SOLVE-C57 is the accepted, pre-implementation
   unification; every seam below is either claimed by C57 or carved out of it.
2. **The causal DAG is proved but not executed.**
   `CausalDiscretePlan::discrete_real_order()` (phase-structural
   causal_discrete.rs:73) has ZERO consumers; both consumers use only
   `discrete_real_definition()`. GALEC independently rebuilds an equivalent
   order (clocked_assignments.rs:368 read expansion; clock_schedule.rs:132
   topological emit) and USES it; Solve/runtime ignores order and evaluates
   all producers against a frozen event-entry snapshot
   (discrete_rows.rs:533-534 freeze, :557-559 evaluate, :562-574 write-back).
   This asymmetry is the root of the one-tick-lag class.
3. **Same-instant value exchange is implemented in exactly one place**:
   model-event transactions' reaching-definition substitution
   (model_events.rs:511-515). Everything equation-shaped lacks it.
4. Orientation IS already unified (one plan, two consumers, same error
   semantics) — keep.

Seams (each a location where two owners can disagree at one instant):

| # | Seam | Anchors |
|---|---|---|
| 1 | Atomic snapshot vs proved causal order (canonical lag) | discrete_rows.rs:533-574 vs causal_discrete.rs:73 |
| 2 | GALEC causal-ordered DoStep vs Solve snapshot on the same model | clocked_assignments.rs:49 + clock_schedule.rs:132 vs discrete_rows.rs:533 |
| 3 | Transaction (sequential) vs guarded/scalar rows (snapshot) at one tick | model_events.rs:511-515 vs discrete_rows.rs:557-559; carved out at SPEC_0040:107, unenforced |
| 4 | `condition_clock_owner` over-claims: And(clock, predicate) returns the clock without totality | events.rs:1539-1543 -> locks at guarded_assignments.rs:30, discrete_rows.rs:208/753 |
| 5 | **NEW DEFECT LEAD:** coupled-event Newton path drops the first-iteration clock lock for scalar/structured rows (guarded rows correctly gated) | coupled_event.rs:156-157, :182-184 vs discrete_rows.rs:208/753 |
| 6 | **NEW DEFECT LEAD:** `event_iteration` overloaded (Appendix-B pass vs intra-pass settle index); `Fixed`-mode pre source switches meaning at settle_iteration==1 | discrete_rows.rs:456; solve_ops.rs:327 |
| 7 | B.1c discrete-value has NO causal proof (plan collects DiscreteReal only); GALEC rejects B.1c cycles, Solve silently snapshots them | causal_discrete.rs:230; clocked_assignments.rs:1084 |
| 8 | Event-settled clock remainder vs base event refresh: per-clock semantic agreement of clock-ownership sources unproved (lengths checked only) | refresh_execution.rs:122-144; refresh.rs:1837; solve_runtime.rs:477-483 |

**Consequences adopted into item 2:** (a) the issued order becomes the single
consumed artifact — C57's plan input is exactly the currently-dead
`discrete_real_order()` extended per-clock and to B.1c under the shared
dependency proof; (b) Codex's total lazy next-value phi is the mechanism that
makes the order executable without Gauss-Jacobi resweeps; (c) seams 4-6 are
pre-C57 code defects and enter the red-proof queue now (lock consistency on
the coupled path; un-overloading the iteration counter; totality evidence
before clock-owner locking); (d) fixture 4 of galec_equivalence is today the
only multi-clock cross-generator equivalence gate — the acceptance canaries
(item 10) must add the reverse-ordered chain and mixed B.1b/B.1c cases.

## Appendix A — LinearOp vs typed_program consumer inventory (evidence)

Gathered read-only (agent survey, 2026-08-11 19:20 EDT). Counts are ±10%.

**Headline:** LinearOp = 50 variants, ~5,000 refs, 13 crates, ~60-65k coupled
lines (~12k of it already-typed code sitting beside it). SolveOperation
(typed) = 31 variants, ~415 refs, 9 crates. Typed coverage is already 31/31
in BOTH the eval-solve typed evaluator and the cranelift typed JIT. The
`LinearOpSliceKind` enum (ir-solve/src/visitor.rs:16) exhaustively names the
seven IR slice sites and is the migration checklist.

**Consumer difficulty map:** eval-solve interpreter ~16k/High (it is the
differential oracle — migrates LAST); cranelift ~10k/High-but-replacement
(typed JIT exists beside it); ir-solve authorities ~13.6k/High
(structural_pattern.rs + refresh.rs derive PROOFS by walking rows); phase-
solve ~15k (ad.rs High; lower/scalar 9.8k is rung-5 DELETION, not port);
phase-codegen ~5.5k + 10 templates dispatching on all 50 kind-names as
strings (mlir.mlir.jinja alone consumes raw serde shapes, 63 refs — and
exec-mlir has ZERO production LinearOp refs, so one template is the whole
MLIR coupling); exec-wasm 1.2k/Low (supports ~12 scalar variants, rejects
everything else — cheapest first cutover); solver/diffsol/rk45 prod-Low with
~1.2k test-fixture refs of churn.

**The bridge today:** typed owners are islands behind an f64-scalar
marshalling airlock (`PureCall`/`PureCallDirectional` -> eval_pure_call_payload,
eval-solve lib.rs:4152) — on the hot path for every call. The working
dual-representation precedent for rung 3 is `EventTransactionProgram` +
`legacy_owners` coverage proof (validated ir-solve lib.rs:1321): reuse that
shape for residual/derivative blocks, do not invent a new cutover form.

**Gap families (no typed equivalent — ~24 of 50 variants):**
(a) solver-boundary storage: no time/solver-Y/AD-seed storage classes — THE
root blocker, close first; (b) table ops (4, host-effect op; slope op must
land with AD); (c) random ops (6; the impure three carry call_site identity —
the concrete test case for the four-identity separation, NOT pure value
relations); (d) linear algebra: no typed solve — exactly where the SolveSPD
intrinsic lands; (e) output/store projection (4 ops + TensorOutputMap
entanglement) — the sleeper: a result-boundary redesign, not one op;
(f) legacy fold/conditional forms — typed is semantically AHEAD except the
GuardedFunctionFold activation guard; (g) block-level AD: ad.rs (3.5k, JVP
over rows) + reverse.rs have no typed equivalent — typed directional covers
one owner at a time. Items (a)+(e)+(g) are the schedule, not tables/randoms.
(h) typed is strictly richer on the pure-value axis (Convert, Diagonal,
Reduce, SelectElement, ProjectElementDynamic have no LinearOp form).

**Wire consequence:** executable SolveProblem content is ~100% LinearOp rows;
LinearOp derives Serialize/Deserialize so variant names ARE the wire. Rung 4
is therefore one big-bang serialization rewrite — affordable (hard version
reject, one golden fixture) — and is "extend the replay discipline typed
programs already have (constructor inputs only, derived facts re-issued) to
the whole problem."

**Sequencing adopted into item 1:**
- Rung 1 freeze is enforceable TODAY: variant-count assertion added to the
  architecture hardening test (landed with this document).
- Rung 2's schedule = (a) storage classes, (e) result boundary, (g) block
  JVP; tables/randoms are bounded mechanical work.
- Rung 3 reuses the EventTransaction legacy-owners coverage-proof shape.
- Backend cutover order: exec-wasm -> exec-mlir (one template) -> cranelift
  -> eval-solve interpreter LAST (it grades everything else).

**Pre-negotiation decisions (Claude positions, answering the survey):**
1. The 4,925-line SolveMethod/effect subtree is KEPT: it is not speculative —
   it is the M3 `SolveAlgorithmBlock` vocabulary built deliberately ahead per
   the 2026-08-08 plan (Milestone 2 increments), already in the typed
   vocabulary, and its first consumer is the M3 root. Guard: it ships no
   further increments until the vocabulary decision is ratified, so
   unification cannot orphan new work.
2. Codex's "no second stored semantic root" amendment BINDS the proof
   authorities: structural_pattern.rs and refresh.rs re-anchor their
   derivations on typed programs as part of rung 3 (added to the ladder as
   rung 3b). LinearOp-as-private-backend-lowering survives only below the
   proof layer.

## 9. Convergence round 2 (Codex 19:18/19:20/19:23) — incorporation

**19:23 formal-architecture corrections — all eight accepted, two with
nuance:**
1. Semantic execution plans (CausalDiscretePlan, RefreshPlan) ARE
   runtime-needed semantic data and may themselves serve as checker
   inputs/witnesses; no duplicate certificates for them. (Refines item 4.)
2. **NEW LIVE P0 — queued:** `RefreshPlan::causal_solution_certified` is a
   public serialized Boolean; validation checks shape but never rederives the
   claim; runtime branches on it to omit staged projection — one flipped wire
   bit skips required work. Fix: derive the execution-mode discriminant from
   canonical owners during construction/replay; a caller-carried proof flag
   never controls execution. (Refresh machinery — proposed owner: Codex's
   per-clock partition slice; Claude co-signs the red-proof.)
3. "One canonical semantic root per stage" replaces "one aggregate per
   phase"; root-bound derived artifacts legal, rederived or
   checker-validated, never parallel semantic owners.
4. FMI: accepted with its own spec work — an internal checked
   model-execution kernel with an `OdeConvertible` refinement to strict ME
   plus an explicit DAE/projection capability until FMI-LS-DAE executes
   (manifold projection is the counterexample to strict-ME universality);
   `FmiComponent(SolveProblem)` and `MeModelArtifact(SolveModel)` converge
   into one checked projection. Fallback if strict ME stays universal:
   documented reduced coverage, not claimed optimality. Needs the SPEC_0038
   amendment before implementation credit.
5. The structural phase output gets a NAME and dataflow: one opaque
   `PreparedDae`/`StructuralResult` carries the single issued plan into both
   projections — closing the fact (Appendix B) that Solve and GALEC today
   call `derive` independently and nobody consumes the order. Authority
   without dataflow is not authority.
6. Two distinct roots over one vocabulary confirmed; program bodies shared,
   never copied.
7. Invocation identities stay compact: an invocation owner references a
   compact activation/domain owner — never one ID/table row per tensor or
   fold coordinate; coordinates are runtime inputs or final scalar views.
8. §7 freeze rule narrowed: freeze only choices that do NOT conflict with
   active accepted specs; where they do (compact Instance ownership vs
   SPEC_0032), the spec amendment lands first.

**19:20 construction-procedure invariant — accepted:** nested executable
regions are TRANSACTIONAL BY CONSTRUCTION: a branch/loop/method/arm builds in
an isolated child arena under a fresh region brand, receives only explicit
typed captures, returns a sealed typed result tuple + effect summary, and
attaches to the parent only after all local checks pass; region-local IDs
cannot escape. Commit-late child construction replaces mutate-then-rollback,
ID truncation, and poison flags; it structurally eliminates the
swallowed-arm/orphan-scope/ABA class we patched with typed errors this week.
Root-level linear construction stays sequential.

**19:18 root split — adopted as the joint canonical dataflow (names to
ratify):**
compact TypedInstanced -> compact Flat -> DAE -> PreparedDae/StructuralPlan
-> immutable complete SolveProblem (typed program owners + schedules/plans
+ identities + correlations) -> SolveInstance (ALL mutable runtime state)
-> checked AlgorithmCode/AlgorithmBlock projection -> checked FmiComponent
projection -> TargetProgram(profile/capabilities) -> passive renderer/native
adapter.
Key adoptions: (a) SolveInstance separates every mutable runtime value from
the immutable problem root — current `SolveModel` (compiler artifacts +
mutable tables + presentation rows) is NOT the canonical root and is
restructured along this line; (b) `TargetProgram` is a checked boundary that
BORROWS canonical owners and closes operation/effect, arithmetic,
shape/domain, loop, storage, alias/ABI, external-capability and
resource-budget obligations before rendering — it never copies or lowers
semantic bodies; (c) cutover is owner-family-at-a-time with per-operation
reference/native parity proofs — atomic replacement rejected (consistent
with the ladder; supersedes any big-bang reading except the wire, which
remains single-cutover); (d) the spec-first list (compact Instance families,
compact root families, opaque TypedInstancedTree, SPEC_0007 terminology
replacement, C55 migration-projection removal at final cutover, TargetProgram
boundary, SPEC_0036 milestone promotion) enters the amendment queue;
(e) size discipline: regular-family IR/wire/common code is O(source owners +
rank + sparse heterogeneous exceptions); genuinely irregular matching may use
a bounded EPHEMERAL scalar view with a checked domain-row bijection — never a
stored parallel owner or recollapse; (f) crate-direction consequences
adopted: phase-solve stops delegating refresh-semantics construction to
eval-solve; phase-codegen drops eval-solve/phase-dae semantic deps; exec-mlir
drops phase-codegen; concrete solvers depend only on the generic solver/ME
protocol. Minor record note: Claude's script-measured legacy variant count is
50 (frozen by test); Codex's pass says ~53 — reconcile the counting method in
the joint record before ratification.

## 10. Claude counter-positions — items REOPENED after user process correction

The user correctly observed that convergence-by-acceptance does not find the
optimum. The following previously-accepted items are reopened with genuine
alternatives and tradeoffs. Ratification of each now requires the negotiation
to answer the stated question, not either side's preference.

### 10.1 REOPENED: direct typed dispatch in EVERY backend (was 19:12.1, accepted)

COUNTER-CASE — oracle monoculture. If the interpreter (the differential
oracle every backend is graded against) dispatches the same typed regions
through the same region-walking semantics as the JIT, a region-semantics bug
passes parity everywhere because both sides inherit it. The repo's own
doctrine already recognizes this failure mode: eval-galec must not delegate
to eval-solve "otherwise the two sides could share the same defect."
ALTERNATIVES:
  (a) Codex's position — all backends direct-dispatch; independence comes
      only from eval-galec and generated-C legs. Cheapest, purest
      no-second-program story; weakest oracle.
  (b) The interpreter keeps a DELIBERATELY independent evaluation strategy
      (e.g., the bounded ephemeral scalar view under its checked bijection)
      as the reference semantics; production backends direct-dispatch.
      Stronger differential power; costs one sanctioned scalar-view user and
      a perf-irrelevant slower oracle.
  (c) Two interpreters: a direct-dispatch production interpreter plus a tiny
      reference evaluator grown from rumoca-reference (SPEC_0037 W1's
      executable semantics), making the ORACLE the formally-anchored artifact.
      Most aligned with the verification story; most work.
QUESTION TO ANSWER: which artifact is the reference semantics of record —
the production interpreter, or a deliberately independent evaluator?
Claude's position: (c) as end-state, (b) as the migration-period stance;
(a) is rejected as weakening the differential mesh that caught this week's
defects.

### 10.2 REOPENED: selective certificates, default-off (was 19:12.2/19:23.1, accepted)

COUNTER-CASE — burden of proof under schedule pressure. "Certificate only
when the producer is substantially more complex than a small checker" is a
judgment call that every author under deadline will decide in their own
favor; this repo's own history (aspiration-as-fact spec drift, caller-
supplied certificates, the causal_solution_certified Boolean) shows exactly
how default-off obligations erode. SPEC_0037's strategy table is right about
WHICH mechanism fits which producer; the open question is the DEFAULT.
ALTERNATIVES:
  (a) Codex: default-off, judgment-based (accepted too hastily).
  (b) Default-ON with recorded opt-out: a new checked owner ships a
      certificate UNLESS a recorded review note names the applicable
      SPEC_0037 row (direct proof / translation validation) and why the
      checker would not be smaller. The artifact of the decision exists
      either way; drift requires a paper trail.
  (c) Universal certificates (Claude's original; withdrawn — genuinely
      over-heavy for small constructors).
Claude's position: (b). The cost is one paragraph per owner; the benefit is
that the erosion mode this codebase has demonstrably suffered becomes
auditable.

### 10.3 REOPENED: shared regions precede GALEC and Production C (was 19:12.3, accepted)

COUNTER-CASE — the eFMI authority question. eFMI's assurance story makes
checked Algorithm Code the authoritative controller artifact an auditor
reviews, with Production Code proven to implement IT (the 2026-08-08 plan
rejected independent DAE->C precisely to keep that containment provable, and
GAL-004 pins admissibility over the UNTOUCHED DAE). Codex's regions-first
shape makes the typed regions the semantic parent and AC a correlated
projection — better for GAL-038's duplicate-lowering class, but it inverts
which artifact is authoritative for flight review, and it must not let
admissibility decisions depend on solve-lowering products.
ALTERNATIVES:
  (a) Regions-first (Codex): one lowering, AC is a projection+correlations.
      Question it must answer: is "PC implements AC" still a construction-
      level proof when both are projections of a third artifact — and does
      GAL-004's untouched-DAE admissibility survive?
  (b) AC-first (the current plan): AC authoritative, SolveAlgorithmBlock
      lowered FROM checked AC. Keeps the eFMI containment story literal;
      keeps one semantic re-lowering (the thing GAL-038 flags).
  (c) HYBRID (Claude's position): pure-function bodies are shared typed
      owners issued once (already decided and landed — that part is not
      reopened); the CONTROLLER LIFECYCLE/statement structure remains
      AC-authoritative, and PC's body is the checked projection of AC whose
      expressions/calls REFERENCE the shared owners. Containment stays
      literal ("PC implements AC"), duplicate FUNCTION lowering still dies,
      and admissibility still reads the untouched DAE.
QUESTION TO ANSWER: what does the eFMI auditor certify against, and can (a)
prove containment as strongly as (c)? This one is flight-relevant and should
be settled with the eFMI text open, not by architectural taste.

### 10.4 REOPENED: transactional child arenas (was 19:20, accepted)

COUNTER-CASE — cost and interning. Arena-per-region construction adds
allocation churn on a prepare path already measured at ~5.2 s, and isolated
child arenas break value interning across region boundaries: SemanticValueId
CSE either stops at the boundary (losing sharing exactly where folds repeat
subexpressions) or needs a two-level intern with its own correctness story.
Wire replay of nested arenas also multiplies replay surface.
ALTERNATIVES:
  (a) Codex: full child arenas, commit-late attach. Strongest isolation;
      costs above.
  (b) JOURNALED parent arena: single arena + region checkpoint; failed
      region construction rolls back by truncation under a proof that no ID
      issued after the checkpoint escaped (escape-freedom is checkable at
      commit because captures/results are explicit). Same
      cannot-attach-partial guarantee, no interning boundary, no per-region
      allocation, single-arena wire replay.
  (c) Status quo hardened (typed errors; landed this week) — known-fragile,
      migration stopgap only.
Claude's position: (b), with (a) as fallback if the escape-freedom proof for
truncation turns out weaker than brand isolation in practice. Decide on a
measured prototype of both on the fold-heavy RDD2 lowering, not on elegance.

### 10.5 REOPENED (narrow): internal kernel + OdeConvertible (was 19:23.4, accepted)

COUNTER-CASE — SPEC_0038 §"internal boundary" exists precisely to kill a
second, richer, private model API; an internal kernel with ME-as-refinement
re-institutionalizes one. The manifold-projection counterexample is real,
but the standards-track answer already exists in-tree: FMI-LS-DAE (layered,
spec'd, unregistered pending execution).
ALTERNATIVE: accelerate FMI-LS-DAE to executable status and keep strict ME
as the sole protocol, with the projection capability expressed through the
layered standard rather than a private kernel. Claude's position: prefer
this; accept Codex's internal kernel only if FMI-LS-DAE is shown structurally
insufficient for the constrained-DAE capabilities an implicit solver needs.

### 10.6 Negotiation norm (proposed, both agents)

No item is RATIFIED until the record shows: at least two genuinely distinct
alternatives, the tradeoff each rejects, the question the decision answers,
and the evidence (measured where measurable) that discriminates. Acceptance
messages without an alternatives section are position statements, not
ratifications. This applies retroactively to §8/§9: items there marked
"accepted" that lack an alternatives record revert to "position converged,
ratification pending the norm."

## 11. Round 3 (Codex 19:55/20:00/20:05) and SPEC codification plan

### 11.1 Negotiation method — MERGED NORM (supersedes §10.6)

Codex's 20:05 arrived at the same correction independently; the merged
ratification record per major decision requires: (1) >=2 credible
alternatives incl. retain/simplify-current; (2) the strongest case for each,
no straw men; (3) a concrete semantic/performance/proof/maintenance
counterexample defeating each rejected option; (4) implementation +
migration/deletion cost incl. temporary proof surface and branch churn;
(5) a falsifiable experiment or invariant that would REVERSE the preference;
(6) decision only after both agents state independent positions and resolve
disagreement. Codex's 11-item mandatory contested set is adopted as the
session agenda (staged depth; one vocabulary vs small tensor IR + target
IRs; complete SolveProblem vs kernel+projections; value/occurrence split;
interning scope; compact Instance + irregular escape hatch; plan/certificate
location; internal kernel vs strict ME vs direct capability API;
TargetProgram vs backend roots; shared GALEC/C region vs checked
translation; owner-family cutover vs shadow dual execution vs one-time
cutover). Both sides' standing positions are LIVE EVIDENCE, not answers.

### 11.2 New contested items (both positions on record)

**RENAME `LinearOp`->`ScalarOp` — now CONTESTED (was user-ratified).**
Codex case (19:55.3): a ~5k-reference rename of a frozen, deletion-scheduled
enum buys no reduction in final proof surface and costs review/blame/
conflict churn across an active multi-agent tree; freeze under the existing
name, delete owner-family-wise; rename only the surviving vocabulary
(`SolveOperation`->`SolveOp`). Claude case: the migration window is
weeks-plus during which every discussion, review, and new-contributor read
mis-primes on "linear"; the rename is wire/template-neutral, mechanical, and
one `--ignore-rev` line removes the blame cost; names teach during exactly
the period that matters. Compromise available: skip the enum rename, do the
`SolveOp` rename now, and put a doc-comment banner on `LinearOp` ("legacy
scalar instruction set, frozen, deletion-scheduled; 'linear' = straight-line,
not linear algebra"). DECISION RETURNED TO THE USER (they ratified the
rename before Codex's cost argument was on record).

**Dual-root migration precedent — CONTESTED.** Codex (19:55.4): do NOT copy
the stored `EventTransactionProgram + legacy_owners` pattern to
residual/derivative roots; differential legacy/new comparison belongs in a
test harness or ephemeral checker input; production issues ONE canonical
owner plus at most a non-serialized root-bound adapter during a cutover;
old producer and consumer die in the same rung. Claude: the stored pattern
is the one cutover that empirically worked (C55), and coverage proofs must
survive wire replay during the window. Synthesis position now favored by
Claude: coverage proof YES, but as construction-issued EPHEMERAL evidence
consumed by the differential gate — never serialized fields on the root.
Ratify under the merged norm with the falsifiable question: can the C55-class
coverage guarantee be reproduced without a stored field (prototype on one
owner family)?

**Accepted from 19:55 without contest:** reference-semantics-first
sequencing (the typed eval-solve reference evaluator + differential gate
exist before ANY backend cutover lands; interacts with, but does not decide,
the reopened oracle-independence question §10.1); freeze-rule wording
("ships certificates" -> "carries a refinement obligation/evidence-manifest
row with the mechanism selected per SPEC_0037"); the eager root-local
interning design as the LEADING CANDIDATE for the CSE item (requires the
SPEC_0007/0036/0043 single-arena amendments BEFORE implementation; no size
thresholds; occurrences retain ordered child-occurrence edges).

### 11.3 Typed-scope checkpoint blockers (Codex 20:00) — acceptance list v3

Four release blockers folded into the running slice's acceptance criteria,
each with a required red fixture: (1) reachable empty-target conditionals
must preserve condition/call/assertion effects (fixture: pure g(x) with
assertion, `if g(x) then end if`); (2) `nested_calls` registers calls from
ALL statement-owned expression roots incl. overwritten assignment RHSs, via
the shared batched traversal (fixture: overwritten assignment whose RHS call
asserts); (3) reverse demand resolves definition -> AUTHORITATIVE AGGREGATE
OWNER (plain/group/fold) and lowers/memoizes the whole owner once (fixture:
reverse-demanded correlated tuple); (4) per-update definition provenance on
fold coercions (fixture: two differently-located Integer-to-Real updates).

### 11.4 SPEC codification plan (user directive)

Once items ratify under the merged norm, they land as follows (SPEC_0000
rules: implemented+stable -> ACCEPTED text; pre-implementation design ->
DRAFT; catalogs -> REFERENCE annex rows anchored to a parent MUST):

- NEW **SPEC_0045 (DRAFT): Solve Executable Vocabulary and Identity** — the
  SolveOp end-state, ScalarOp freeze/adapter/deletion ladder, the
  four-identity model (SemanticValueId/OccurrenceId/InvocationOwnerId/
  OutputProjectionId), interning policy, occurrence edges, wire
  reissued-handles rule. Rows to SPEC_0040; construction rows to SPEC_0043.
- NEW **SPEC_0046 (DRAFT): Discrete Execution Ownership** — the three-owner
  story, PreparedDae/StructuralResult named dataflow, the total lazy
  next-value mechanism, seam rules, SIM-010 relationship; C57 rows fold in
  or cross-reference.
- AMEND **SPEC_0007** (ACCEPTED): canonical dataflow diagram gains
  PreparedDae + SolveInstance + TargetProgram stages when ratified;
  ScalarProgram terminology transition; row-range updates. Budget-managed.
- AMEND **SPEC_0032**: compact Instance families (Codex item 9/19:18) —
  removes the per-element Instance authority mandate; irregular-array
  bounded-ephemeral-view escape hatch with checked bijection.
- AMEND **SPEC_0036/0043**: one-canonical-root-per-stage wording;
  transactional-region or journaled-checkpoint construction (whichever wins
  §10.4); SolveInstance separation rows.
- AMEND **SPEC_0037**: evidence-mechanism selection made normative
  (obligation row universal, mechanism per table); plan-as-witness rule.
- AMEND **SPEC_0038**: the FMI resolution (strict-ME + accelerated
  FMI-LS-DAE, or internal kernel + OdeConvertible — whichever wins §10.5);
  FmiComponent/MeModelArtifact convergence.
- AMEND **SPEC_0000**: the annex constitutional fix (§6) + the merged
  ratification-record norm as the architecture-decision process.
- The joint architecture decision record itself stays in dev/ as the
  rationale ledger; specs carry only the binding rules per SPEC_0000 budgets.

## 12. The three alternatives passes (Codex 20:15/20:20/20:25) — dispositions

FACTUAL CORRECTIONS ACCEPTED:
- §9's linear diagram was semantically wrong: SolveInstance (mutable) cannot
  sit upstream of AC/FMI/target construction — the flow is a FAN-OUT of
  sibling checked projections from immutable roots. Diagram to be redrawn in
  the joint record.
- Current SolveModel is already documented/implemented immutable with mutable
  state in SolveRuntime; its real defects are public-field/default/validate
  assembly, incomplete correlation, and split authority. The 19:18
  "SolveInstance separation" motivation is thereby corrected; the contested
  root question becomes: seal/evolve SolveModel (Codex provisional
  preference: no third root for better nouns) vs new roots vs rename-at-
  cutover.
- "Formally verified" language corrected: brands/private fields are
  valid-by-construction ENFORCEMENT; the formal claim begins with executable
  semantics + refinement/checker theorems + Rust/formal correspondence per
  SPEC_0037.
- §6's annex constitutional fix largely exists in SPEC_0000 §§144-176; the
  defect is enforcement/status drift — repair anchors/status + the gate, do
  not duplicate prose. (My §6 narrows accordingly.)
- The decisions doc is now TRACKED (committed c3db795f), resolving 20:20.3.
- The method/effect scaffold landed hardened (690ad951); Codex's landing
  conditions (transactional failure safety, opaque lifecycle root, first
  consumer, corresponding deletion) become the M3 ACCEPTANCE CRITERIA rather
  than a shelving question.

CONVERGENCES (independent passes met my reopened positions):
- GALEC hybrid (20:15.6 ≈ §10.3): AC stays the auditor-visible lifecycle
  root; shared pure expression/function lowering + exact correlations;
  profile-specific SolveAlgorithmBlock for C. To be ratified with PC->AC
  traceability + code-size evidence against regions-first and AC-first.
- FMI (20:15.7 ≈ §10.5): strict ME public, private prepared kernel inside
  the component, constrained DAE waits on FMI-LS-DAE; the deviation
  experiment is specified before any richer public API.

NEW/UPDATED CONTESTED MATRIX ITEMS (for the joint session):
1. Vocabulary internal shape (does not reopen the user's one-vocabulary
   ratification): monolithic SolveOp enum vs FACTORED single typed-program
   model (pure ValueOp + structured regions/terminators + root-specific
   EffectOp) vs permanent tensor IR + target IRs. Codex prefers factored;
   Claude leans factored as well IF wire/replay and the freeze test can pin
   the union of factors as one vocabulary (the ratified end-state is "one
   canonical vocabulary", which a factored model still satisfies).
2. Identity synthesis v2 (20:25.1): ValueDefinitionId / PureTermId /
   OccurrenceId / InvocationOwnerId / opaque OutputProjectionId; PureTerm
   sharing authorizes STORAGE only; eager interning gated on measured
   memory/compile benefit. Supersedes the v1 four-ID wording as the leading
   candidate.
3. Discrete architecture v2 (20:25.8 + 20:15.3): DiscreteOwnershipPlan
   proving exclusivity/visibility over three typed subplans (DAE-C21
   algorithm order; structural equation causality; event fixed point) with
   explicit cross-owner edge kinds SameInstant/LeftLimit/History and ONE
   composition theorem — no falsely universal order oracle. Supersedes the
   bare "CausalDiscretePlan is the only oracle" wording.
4. Profile binding (20:15.2): "same bodies, profile bound later" is not free
   (Binary32/64 in value types; GAL-024/SOLVE-C33 operation-level
   differences). Three alternatives on record.
5. Root packaging (20:25.4): one opaque package per correlated identity
   universe (SolvePackage with subroot views) vs literal one-root-per-stage.
6. TargetProgram (20:15.5): per-product checked projections sharing
   capability/profile checkers; generic TargetProgram only after three
   products show materially identical invariant data.
7. Backend migration order (20:15.9): vertical owner-family migration
   (constructor/wire -> typed reference evaluator -> dependency/JVP ->
   Cranelift -> oracle/C -> delete family) CONTESTS the cheapest-backend-
   first order in Appendix A; resolve with the falsifiable question of which
   ordering retires risk earlier on the RDD2 canary.
8. Flat->DAE materialization (20:25.2): explicit roots vs streaming collapse,
   25% RSS/transition-code threshold to reverse.
9. KernelIR (20:25.5): direct SolveOp lowering vs digest-bound derived
   KernelIR — allowed only if two real backends repeat a nontrivial
   legalization defect; never a second canonical wire.
10. Certificate persistence (20:25.6): ephemeral by default; immutable
    content-addressed sidecars bound to root digests permitted for
    assurance profiles; never caller-authored semantic fields, never
    runtime-controlling.
11. MiniJinja for MLIR/WGSL (20:20.2): typed emitter + translation
    validation vs passive template — measured comparison; SPEC_0029 amended
    to whichever wins.
12. Compact Instance authority (20:25.7): checked Atom + homogeneous Family
    sum with proved-homogeneous partitions and atom fallback; million-point
    O(owner+rank+exceptions) acceptance.
13. NativeRequired (20:15.8): complete owner-closure CompiledImage before
    execution, fail closed; declared hybrid profile only for migration.
14. Frontend closure (20:25.9): private typed phase chain
    (InstancedModel -> typecheck by value -> TypedInstanceModel -> Flat
    constructor) precedes compact-family or formal-verification credit.

PREREQUISITE GATE (20:20.5, agreed): the three identity/replay closures
(cross-table pure-call rebind; serialized refresh Boolean; sparse-pattern
wire decode) precede broad architecture implementation. They are the first
post-release slices together with the 20:00 blockers.

## 13. SPEC_0045/0046 drafting constraints (from the 21:18/00:35 exchange)

Codex's two-DRAFT synthesis is the drafting basis, with governance accepted
(self-contained DRAFTs; nothing originates in catalogs; one voted series
combining accepted-surface extraction + DRAFTs; full C32-C38 audit with
TypedProgram-enforced clauses re-anchored). Claude's stricter gates, pending
Codex counter-round:
1. Wire serialization is deterministic and interning-invariant; the PureTerm
   A/B decides memory/compile cost only, never byte form (protects the eFMI
   checksum web and certificate sidecars).
2. The one-owner invariant is machine-enforced at both layers: brand
   lifetimes at compile time, root digest in capability reissue at replay.
   Preregistered red: tampered cross-root reissue fails by digest.
3. SPEC_0046's composition theorem carries an explicit iteration-indexing
   rule for cross-subrelation reads at coincident instants. Preregistered
   red: equation producer + algorithm consumer + fixed point converging at
   iteration >=2.
4. Additional preregistered reds: identity double-reissue collision;
   cross-member-kind coincident read (hold-fallback member read by another
   clock's exchange member).
5. Experiment reversal gates: package-vs-literal-roots reverses only on an
   invariant that cannot be stated root-locally; arena-vs-journal reverses
   on >2% prepare-time regression on the RDD2 canary or any escaped-handle
   red.

### 13.1 Counter-round resolution (00:50)

Codex's 21:27/21:36 counter-round resolved against §13, item by item:
1. Wire gate stands as byte-form determinism; mechanism is canonical
   first-use term records, NOT mandatory in-memory interning. Added machine
   red: decode -> re-encode -> byte-identical (verifier-recomputable
   canonical order).
2. Root digest CONCEDED as in-memory authority; replaced by the strictly
   stronger unrepresentability design (generative brands, root-local
   namespace-typed ordinals, one outer replay closure with no syntax for
   foreign roots). Digests retain authority only at the outer cross-root
   projection and external sidecars. Codex's three machine reds adopted.
3. SPEC_0046 composition = MLS-derived strata: lane capture -> once-only
   first-pass scheduled/clock subplans -> unclocked z/m/when fixed point
   (pre_iter advances between rounds; condition-triggered algorithm
   transactions live INSIDE the relation) -> post-settle actions, single
   commit. Edge legality: once->iterative legal; iterative->post-settle
   legal; iterative->once OPEN on one ruling — MLS-legal clocked
   `sample(u)` of an unclocked discrete variable at a coincident tick:
   RULED (Codex 21:47, MLS 16.5.1): `sample(u)` is a LEFT-LIMIT lane
   captured at clock-partition entry (clocked partitions evaluate first
   and once; iteration only over the unclocked partition afterward), so
   the example dissolves and joint-owner-or-reject stands unweakened for
   genuine SameInstant iterative->once edges. Four distinct history
   capabilities (event-entry Pre, iterative pre_iter, clock Previous,
   SampledLeftLimit); no unified buffer. Added preregistered fixtures:
   coincident-change retention; noClock(x) vs sample(hold(x)).
4. Identity reds tightened: duplicate DEFINITION rejects; multiple uses of
   one definition converge to one handle. Member-kind vocabulary removed
   from test prose; guarded-total-next coincident-schedule fixture.
5. Reversal gates final: roots reversal requires two production consumers;
   journal invalidation requires ID aliasing (observational-rollback
   invariant is the comparator); >2% prepare regression is a preregistered
   statistical discriminator, not an automatic correctness reversal.
Governance final: 19/20 spec count; two-commit voted PR (PROPOSED ->
DRAFT + atomic accepted-surface extraction); SPEC_0000 has no
DRAFT->ACCEPTED path today — whether to add one is scoped into the same
voted series.

## Appendix C: DCO repair hash map (rebase --signoff, tree-identical, 00:55)

```
489f7fd3 -> 74973f6d  Implement tensor-native Solve owners and event transactions	74973f6d
522613a6 -> d191377c  Execute checked event transactions atomically	d191377c
6499d8d0 -> d2935e36  Specify checked continuous refresh ownership	d2935e36
7c4de0c7 -> 3ca62c3f  Execute continuous calls through checked directional owners	3ca62c3f
0e1d7d15 -> 12c7e6f5  Select exact typed region captures	12c7e6f5
ae636cce -> 3a42cb32  Issue canonical continuous refresh schedules	3a42cb32
8c0e28d1 -> 09b864ea  Document rejected Cranelift optimization experiments	09b864ea
e5ddf351 -> 64644ca0  Issue exact causal refresh remainders	64644ca0
72d3d318 -> ce868f1b  Document RDD2 performance handoff	ce868f1b
32c9cb90 -> 5c7d7fde  Update RDD2 agent handoff	5c7d7fde
e72f9dd3 -> 91db17f5  Reuse event-settled clock refresh owners	91db17f5
95315052 -> 3c1ad654  Store compact refresh row selections	3c1ad654
7a24727c -> 7acba844  Refresh RDD2 agent handoff	7acba844
9c1af21e -> 9fbf56e4  Preserve affine refresh dependencies	9fbf56e4
07e9d5df -> ff0a1786  Rebuild Solve artifacts on wire replay	ff0a1786
c63cff21 -> d5f1904a  Reject typed pure-call ops in the WASM emitter	d5f1904a
106fb650 -> f0097adf  Fix zero-trip fold arity, algorithm activation panic, exec-mlir tests	f0097adf
b1e86183 -> 9822c05e  Key typed-function lowering by issued definition identity	9822c05e
690ad951 -> bef79023  Harden the typed method/effect scaffold	bef79023
50fcecef -> ad85fca2  Close in-process structural-pattern authority	ad85fca2
3df0b4ac -> 43eeab93  GALEC C codegen: single-source symbols, checked limits, partials	43eeab93
31458892 -> 4e8fa8ec  Wire the three-leg GALEC differential oracle	4e8fa8ec
305b211b -> 394cea34  Sync the contract registry with the SPEC_0022 catalog	394cea34
d14d7e9b -> 6d7c372d  Require assumptions in the Kani proof manifest (schema 3)	6d7c372d
24b8838d -> 3d873d5e  Enforce the scalar-op vocabulary freeze; file-size markers	3d873d5e
c3db795f -> 86435a8b  Spec truthing, C57 reconciliation, decision and evidence records	86435a8b
9a7d52fd -> d40c6567  Post the batch release to the coordination mailbox	d40c6567
18c8354a -> ecb5d795  Disposition the three architecture alternatives passes	ecb5d795
87f43c85 -> e4e46148  Reject orphaned construction; admit zero-trip loop domains	e4e46148
60dff295 -> fe53875d  Acknowledge the stop-ship repair and position dispositions in the mailbox	fe53875d
a150cf34 -> cb68cf6a  Record the three-angle post-checkpoint audit verdict (Codex ledger row)	cb68cf6a
6c2a2b58 -> ff732d3b  Accept the checkpoint-audit dispositions; sequence the corrected series	ff732d3b
cdb8a0df -> ef42d406  Counterpositions and stricter gates for the SPEC_0045/0046 drafts	ef42d406
6d798c28 -> 95d33729  Counter the draft architecture gates	95d33729
1cb868c6 -> eaee351d  Resolve scheduled-event iteration strata	eaee351d
7639c05b -> 9bc2b6b9  Resolve SPEC_0045/0046 counter-round to drafting constraints	9bc2b6b9
```

### 13.2 Event-strata P0 constraints (Codex live audit, verified 01:15)

Three verified current-runtime counterexamples bind the SPEC_0046 draft and
the C57 vertical slice (anchors verified in-tree before acceptance):
1. Boolean `sample(start,interval)` is conflated with synchronous Clock
   ownership (phase-dae calls.rs owns it "as a periodic clock";
   pre_params.rs excludes clock-owned runs from z/m fixed-point
   advancement). Slice commit 1: split PeriodicEventActivationId from
   ClockPartitionId (construction reclassification + wire schema bump),
   red-proved by scheduled m=pre(m)+1 with unclocked n=pre(m) — Appendix-B
   requires n=1, conflation leaves n=0. Both IDs reuse exact lattice
   arithmetic.
2. First-pass umbrella = orchestration plan referencing compact child
   owners with total-next intermediates — never an evaluate-once
   old-storage snapshot, never a monolithic re-lowered body (the
   discrete_rows.rs same-snapshot 1s/2s case is the standing red).
3. driver.rs replaces the ENTIRE `pre` snapshot with post-pass live
   storage at the coincident boundary — an MLS 3.7 §8.5 citation applied
   to every lane when it licenses only event-iterated ones. Requirement:
   LANE-PARTITIONED pre advancement (only pre_iter advances between
   passes; LeftLimit / SampledLeftLimit / clock Previous retain their
   generation). Red fixture pins a NON-iterated lane observed across the
   coincident boundary.
Ownership gaps: (a) unclocked condition-triggered algorithm transactions
must be constructible as iterative-relation members (stratum-3 membership;
same slice as the ID split); (b) FMI post-settle assertion conversion must
precede any history/canonical commit (EventTransactionProgram/C55
commit-once; the red doubles as the observational-rollback fixture for the
arena-vs-journal experiment).
EventInstantExecutionPlan (opaque coverage root over typed subrelations,
compact child owners by ID/range) accepted as the converged composition
shape.

### 13.3 Expansion-boundary sharpening (00:06/00:12 stops; ratified
proposed amendment and implementation freeze, 02:05 — NOT active spec law:
SPEC_0032 still authorizes the shared eval-solve scalar fallback until the
scheduled amendment removing that license and establishing the final
template/target execution as the sole materialization boundary is voted in
the SPEC_0045/0046 series)

The scalar-projection license in earlier drafts ("scalar-only targets are
a sanctioned projection point at the view layer") is WITHDRAWN — two
successive repair attempts grew from it, one enumerating elements in the
template view, one in eval-solve. Binding rule: the expansion boundary is
the FINAL Jinja render. A scalar-only target meeting a compact tensor op
has exactly two outcomes: (1) its final template renders a compact
loop/instruction sequence directly from the op's checked O(1)/O(rank)
metadata (render-boundary Jinja helpers allowed), or (2) checked target
admission rejects the op before rendering. No IR, eval, query, evaluator,
or native-preparation path materializes per-element scalar graphs from
compact ops; the existing eval-solve scalarizer is transition debt.
Acceptance evidence: million-element case with O(1)/O(rank) compiler
metadata and preparation — emitted loops acceptable, materialized
million-op vectors nowhere.

### 13.4 Gate definition (ratified 02:20)

Two reported states, never merged:
1. ORDINARY GATE (corrected 03:05 per SPEC_0025 §4 — `cargo test
   --workspace == 0` is ONE subgate, not the definition of green): the
   complete applicable accepted matrix — fmt --check; all-target
   all-feature clippy with warnings denied; workspace tests; docs; the
   MSL gate; ModelicaTest parity; pinned modelica_models compatibility
   for compiler/simulator semantic changes. All subgates reach zero. Triage labels (REGRESSED / PRE-EXISTING / ABSENT-AT-BASE)
   route evidence; they never waive it. Ratchets, baselines, allowlists,
   and size limits are POLICY surfaces: a failing guard is repaired at the
   source or escalated as a voted policy change — never edited under a
   named-commit justification.
2. TOPIC COUNTEREXAMPLE INVENTORY (adopted implementation requirement,
   PENDING — no manifest, harness, CI job, or milestone gate exists yet):
   deliberately-authored future counterexamples live in a topic harness
   for PLACEMENT only — the harness runs in CI reporting-only and prints
   its inventory with linked roadmap rows every run; the corresponding
   milestone gates mechanically on inventory-zero. ADMISSION RULE: a test
   that was ever ordinary/current may NOT move into the inventory; only
   counterexamples authored and preregistered for a future milestone
   qualify. (Note: the 02:20 GALEC-trio placement is VOID — the 02:50
   classification proved those tests formerly green, hence ordinary
   regressions, hence ordinary-gate reds to fix.) Stop-ship status is retained; inventory membership is
   never semantic green nor permission to advance to performance work.
The checkpoint is explicitly non-green until state (1) is zero.
