# Clock-partition transaction design (SOLVE-C57)

Date: 2026-08-11
Branch: `msl-trace-parity-50`
Status: ACCEPTED DESIGN, PRE-IMPLEMENTATION
Owner rows: SPEC_0040 `SOLVE-C57`, SPEC_0043 §4 clock-partition rows, SPEC_0043 §5
clock-partition transition cases, SPEC_0007 Stage 4 governing sentence.

This is the artifact named by the `pending:` marker on every `SOLVE-C57`
catalog row. It records what the owner is, what it deliberately does **not**
claim, which review obligations it discharges, and the regressions that must be
red before the implementation lands.

## 1. Problem

Inside one clock tick, a clocked partition is solved once (MLS App B: "a clock
partition is solved once per tick, in the first event iteration of that tick").
That single solution is a *causal* solution: a producer that reads another
producer's target on the same tick must read this tick's value. Today's Solve
lowering evaluates the partition against an atomic event-entry snapshot, so an
unconditional B.1b chain `a = f(t); b = g(a)` written in reverse row order
serves `b` a stale `a`. The 14:25 patch repaired that by recursively inlining
the producer expression into every consumer; the 14:32 review rejected that
approach as post-DAE expansion (it duplicates the producer graph and any
compact tensor/fold work once per consumer) and as a construction gap (the
recursion guard silently fell back to old storage, which invents left-limit
semantics the DAE never proved). The whole patch was reverted at 14:21/14:25.

## 2. Accepted design

One branded, whole-clock **`ClockPartitionTransactionProgram`**: the
equation-shaped member of the `EventTransactionProgram` (SOLVE-C55) family,
constructed before any target or tensor coordinate is enumerated.

| Element | Rule |
|---|---|
| Plan input | The structural causal plan issues, for one clock, the complete ordered producer list with each producer's typed rank and intermediate-definition identity. Rank and identity are *issued values*; no consumer recovers them by searching an order list or by matching targets, names, spans, or provenance (kills the `order_by_causal_rank` / `position` O(n²) recovery the 14:32 review flagged). |
| One route | Unconditional and guarded clocked producers pass through the same proof and lowering route. A partition where only guarded producers reach the owner is unconstructible — two routes are exactly what left unconditional B.1b chains stale. |
| One lowering | Each admitted producer lowers exactly once into one compact SSA/typed region. A consumer reference is an in-bounds load of the issued intermediate: never a re-lowering, inline, duplication, or runtime memoization. Program size is linear in producers, independent of consumer count and of tensor extent. |
| Commit | Intermediates become visible to later producers without committing unrelated targets; the complete final-target tuple commits atomically after the last transition. |
| Consumers | Interpreter, Cranelift/WASM, GALEC, and Production C consume this one owner; storage and loop choices happen only at final emission. |

### 2.1 Two construction-issued member kinds

Membership is decided once, at construction, and is never repaired later.

**Exchange member.** Admitted only under exact per-tick activation totality: the
owning clock alone is total; `And(clock, non-clock predicate)` and every other
narrower guard is total only with a construction proof over that tick. Its
target is written on every tick, so a same-tick consumer loads its issued
intermediate. A directed cycle among exchange members is unconstructible.
Discrete-value (B.1c) producers join exchange membership only under the same
dependency/cycle proof that admits discrete Real (B.1b) producers.

**Hold-fallback member.** Every guarded producer whose narrower guard carries no
totality proof — the ordinary `And(clock, predicate)` shape. This is the
carve-out that keeps C57 consistent with the accepted rows: the producer is
still a member of the partition (it is ordered, lowered once, and executed by
the owner), but its target keeps the checked hold semantics that SOLVE-C07,
SOLVE-C10, SOLVE-C47, and SOLVE-C49 already specify, and a same-tick read of
that target is a storage read under the SOLVE-C22 history view, not an
intermediate load.

Rejection at the exact producer span is reserved for a row that *claims*
exchange membership without its totality proof. C57 does not reject
`And(clock, predicate)` guards, and it never demotes an exchange member to hold
(or promotes a hold-fallback member to exchange) as a repair.

### 2.2 Carve-outs (what C57 does not own)

| Carve-out | Rule | Quoted authority |
|---|---|---|
| Event transactions | A target owned by a DAE-C21 / SOLVE-C55 model-event transaction is excluded from C57 admission. The transaction is the owner; C57 producers consume its committed final values and never claim, re-derive, or re-order a transaction target. | DAE-C21: "downstream executable phases consume the transaction owner and MUST NOT reconstruct it from projected equations, target adjacency, shared call ids, provenance, or expression shape" |
| `sample` / state boundaries | A `sample(u)` source keeps its event-entry left-limit lane and a state derivative terminates the chain; exchange lowering does not convert either into a same-tick intermediate. The accepted `y = hold(h); s = sample(y); h = 2*s` chain therefore stays legal and acyclic. | SOLVE-C28: "Explicit `sample(u)` ownership and state derivatives end that same-instant path" |
| Hold semantics | See §2.1. | SOLVE-C07: "No active branch means hold the current target"; SOLVE-C10: "A B.1c definition computes its first active value or leaves its target unchanged" |
| Unowned rows | A row the SPEC_0043 §4 causal plan leaves unowned (ambiguous, cyclic, self-reading, or type/shape-incompatible) has no C57 membership. | SPEC_0043 §4: "ambiguous, cyclic, self-reading, or type/shape-incompatible rows remain unowned" |

### 2.3 Disposition of unowned rows (today's named behavior)

An unowned row is not a silent coverage hole and not a C57 failure. Today's
checked behavior, which C57 preserves verbatim:

- `rumoca-phase-structural` returns `CausalDiscreteError::NonComputable { span }`
  (`crates/rumoca-phase-structural/src/causal_discrete.rs`).
- `rumoca-phase-solve` maps it to `LowerError::non_computable("coupled discrete
  Real residual is not an explicit computable definition", span)`
  (`crates/rumoca-phase-solve/src/lower/events.rs`).
- `rumoca-phase-galec` maps it to the `coupled-discrete-real-equation`
  unsupported-target error
  (`crates/rumoca-phase-galec/src/lower/clocked_assignments.rs`).

**Future work (not claimed by C57):** a compact coupled-owner representation
that carries such a block into the partition as one checked simultaneous owner
(one block, one solve, no scalar expansion). Until that owner exists, the typed
span-bearing rejection above *is* the specified behavior. A future amendment
adds the coupled owner as a third member kind; it does not weaken §2.1.

## 3. Review obligations discharged

### From Codex 14:32 (read-only review of the reverted clock patch)

| Obligation | Where it is answered |
|---|---|
| Discrete-value definitions were admitted without the shared dependency/cycle proof; a self- or mutually-reading same-clock definition could acquire unproved left-limit semantics | §2.1 exchange membership: B.1c joins only under the B.1b dependency/cycle proof; cycles are unconstructible |
| The re-entry path silently fell back to old storage | §2.1: re-entry is a typed contract error; hold semantics are a construction-issued member kind, never a runtime recovery |
| Recursive producer inlining into every consumer is post-DAE expansion | §2: one lowering per producer, consumers load the issued intermediate, size linear in producers |
| `order_by_causal_rank` recovers rank with `position` (O(n²), forgeable) | §2 plan input: rank and intermediate identity are issued values |
| The owner belongs beside/extends `EventTransactionProgram`, while equation causality stays distinct from algorithm statement order | §2: equation-shaped member of the SOLVE-C55 family; the two owners stay separate and neither is reconstructed from the other |

### From Codex 14:55 (construction obligations for the replacement owner)

| Obligation | Where it is answered |
|---|---|
| Exact activation totality — the old `condition_clock_owner` also returned the clock for `And(clock, nonclock_predicate)`, which is not a definition on every tick | §2.1: totality is the exchange-admission proof; without it the producer is a hold-fallback member, and a false claim of exchange rejects at the producer span |
| One proof/lowering route spanning unconditional and guarded B.1b/B.1c producers | §2 "One route" |
| Regressions: false narrower guard, reverse-ordered unconditional B.1b, mixed B.1b/B.1c chains and cycles, compact tensors, linear IR growth | §4 |

### From Codex 16:20 (per-clock refresh partition ownership)

| Obligation | Where it is answered |
|---|---|
| Per-clock ownership must be construction-issued and consumed without graph inspection; transaction dependencies come from compact typed `inputs()` storage ranges, not from a global union that happens to cover them | C57 issues one owner per clock whose dependencies are its own compact input ranges; the base event owner keeps only always-event consumers. The per-clock refresh partition slice (Codex-owned) and this owner must agree on one clock-ownership source; construction rejects missing or alignment-conflicting clock ownership |
| The compact affine dependency certificate for a selected structured node is derived once, never by enumerating coordinates or unioning a whole `ComputeBlock` | §2: no coordinate enumeration before final emission, in the owner or in its dependency certificate |

## 4. Regression list (must be red before the implementation, green after)

Pinned as SPEC_0043 §5 rows.

1. **False narrower guard.** An `And(clock, predicate)` producer whose predicate
   is false on a tick is constructed as a hold-fallback member: its target holds
   (SOLVE-C07/C10) and a same-tick reader observes the left limit, not a
   same-tick value. A row that claims exchange membership without a totality
   proof is rejected at that producer's span.
2. **Reverse-ordered unconditional B.1b.** Same-tick exchange holds independently
   of source, row, and storage order. This is the exact shape that stayed stale
   when only guarded producers were routed through the owner.
3. **Mixed B.1b/B.1c chain and cycle.** Both value kinds are admitted under one
   dependency proof; every directed cycle among exchange members fails
   construction with a typed error.
4. **Compact tensor.** A tensor-valued producer and its consumers traverse the
   owner with no coordinate enumeration.
5. **Linear growth.** Program size is linear in producer count; no producer is
   duplicated per consumer.
6. **Event-transaction exclusion.** A clocked target owned by a DAE-C21
   transaction is not admitted as a C57 member, and a C57 producer reading it
   consumes the transaction's committed value.
7. **`sample` boundary.** `y = hold(h); s = sample(y); h = 2*s` stays accepted,
   `s` keeps its event-entry left-limit lane, and exchange lowering does not
   turn the sampled read into a same-tick intermediate load. The proved
   instantaneous-feedback rejection (`s = hold(h); h = 2*s` with no explicit
   `sample`) is unchanged.
8. **Unowned row disposition.** A self-reading or ambiguous clocked B.1b row
   still reports `LowerError::non_computable` in Solve and
   `coupled-discrete-real-equation` in GALEC at its own span — not a C57
   coverage failure and not a silent hold.

## 5. Interaction map

| Row | Relationship to C57 |
|---|---|
| SOLVE-C07 | Unchanged. Supplies the hold semantics of a hold-fallback member. |
| SOLVE-C10 | Unchanged. Supplies "first active value or leaves its target unchanged" for B.1c hold-fallback members. |
| SOLVE-C22 | Unchanged. Supplies the history view a same-tick read of a held target observes: continuous left limits and clocked `previous()` remain at event entry, and clocked equations execute only on the first pass. |
| SOLVE-C28 | Unchanged and quoted. Its `sample`/state-derivative boundary terminates a same-instant producer path inside the partition exactly as outside it; C57 adds no new feedback rejection. |
| SOLVE-C47 | Unchanged. Only an `Always` or exact owning-`ClockId` branch resolves before scalar-program construction; a hold-fallback member's dynamic branches stay dynamic. C57 must not use exchange membership to resolve a branch C47 leaves dynamic. |
| SOLVE-C49 | Unchanged. The `GuardedAssignmentProgram` remains the executable owner of a correlated guarded family; C57 orders and admits, it does not re-derive the family's arm/hold execution. |
| DAE-C21 / SOLVE-C55 | Sibling owner. Algorithm statement order versus derived equation causality; targets are partitioned between them, never shared, and neither is reconstructed from the other. |
| SOLVE-C51 / C56 | Pure-call owners and continuous refresh owners are consumed by reference; C57 introduces no second call table and no second refresh owner. |
| SPEC_0043 §4 causal plan | Supplies membership candidates. Rows it leaves unowned are dispositioned per §2.3. |

## 6. Implementation order

1. Spec reconciliation (this artifact plus the SPEC_0007/0040/0043/0022 rows) — done.
2. Codex's per-clock refresh partition slice (16:20) lands first; it establishes
   the exact per-clock ownership C57 consumes.
3. Structural: issue the ordered producer list with rank and intermediate
   identity, including the totality proof and the member-kind decision.
4. Solve: `ClockPartitionTransactionProgram` construction plus checked wire replay.
5. Backends: interpreter, Cranelift/WASM, GALEC, Production C consume the owner;
   legacy per-producer executable projections are suppressed only under the
   SOLVE-C55 coverage-equality proof.
