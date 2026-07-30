# SPEC_0036: Valid-by-Construction Compiler IR

## Status
DRAFT

## Summary

Compiler IR makes invalid stage values unrepresentable. `Dae::construct` and
future `flat::Model::construct` use sequential semantic-owner closures over one
private aggregate without weaker or duplicate storage.

## Specification

### Scope

This stays `DRAFT` until AST proofs, `flat::Model`, `Dae`, and `SolveProblem`
hide invariant fields/root validators. Solve sparsity follows
[SPEC_0039](SPEC_0039_PROOF_CARRYING_SPARSITY.md).

Milestone acceptance rows, reservation owners, canonical arenas, equation
contracts, and enforcement evidence are catalogued in
[SPEC_0043](SPEC_0043_CONSTRUCTION_CATALOG.md). Every row there is normative by
reference from the section that links it.

### DAE Milestone Acceptance

Acceptance rows and the `rumoca-ir-dae` LOC review triggers are
[SPEC_0043 §1](SPEC_0043_CONSTRUCTION_CATALOG.md#1-dae-milestone-acceptance-and-review-triggers).

### One Aggregate Owns Construction

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Aggregate owns all arenas/systems, including `ExpressionArena` | `Dae` | One authority |
| Sequential owner closures | `Dae::construct` | Shared boundary |
| Handles are borrowed, branded, nonserializable capabilities | DAE API | No owned IR |
| Success returns immutable `Dae`; failure exposes none | `Dae::construct` | No partial root |
| Finalization is O(1), excluding freezing | `Dae::construct` | No rescan |

Data-owning builders, partial roots, unchecked insertion, and
finalized mutation are prohibited. Producers own analysis; insertion checks
supplied proofs and local integrity.

### Solve Aggregate and Discrete Definitions

`SolveProblem::construct` is the one Solve construction authority; its per-owner
rules are
[SPEC_0043 §6](SPEC_0043_CONSTRUCTION_CATALOG.md#6-solve-aggregate-and-discrete-definition-catalog-spec_0036-solve-aggregate).

For target `x`, the first true `(a_k, v_k)` gives `x' = v_k`; otherwise
`x' = x`. Activations are shared per iteration; inactive values are skipped.

### Flat Aggregate Construction

`flat::Model::construct` is the one Flat construction authority; its per-owner
rules are
[SPEC_0043 §7](SPEC_0043_CONSTRUCTION_CATALOG.md#7-flat-aggregate-construction-catalog-spec_0036-flat-aggregate).

Declarations retain exact spans. Per SPEC_0032,
`InstanceOverlay::component_families` remains a non-authoritative descriptor;
per-element instance entries own Instance semantics. `flat::Model` owns only
the flattened structured families, whose scalar views/counts derive.
Drafts, public invariant fields, repair, compatibility, unchecked insertion,
finalized mutation, and alternate constructors are prohibited.

### Storage and Forward References

Storage shape and the entries permitted to reserve are
[SPEC_0043 §2](SPEC_0043_CONSTRUCTION_CATALOG.md#2-reservation-owner-catalog-spec_0036-storage-and-forward-references).

Only catalogued entries reserve. Private linear authority and an O(1) unfilled
counter reach zero before success; all else inserts complete values in proven
order. Local checks/counters are required. Global trackers, parallel identity
maps, persistent seals, root validation/repair, and unchecked paths are
prohibited. Brands affect no finalized equality/order/display/wire data.
Acyclic functions construct in dependency order.

Construction is O(nodes + operands + total rank); insertion is amortized O(1)
plus operand/rank work. Views borrow, derived indexes build once, proof
transitions do not deep-clone IR.

### Canonical Arenas, Systems, and Environments

The aggregate owns exactly the arenas, systems, and environments listed in
[SPEC_0043 §3](SPEC_0043_CONSTRUCTION_CATALOG.md#3-canonical-arenas-systems-and-environments),
each with its required storage.

### Type and Variable Identity

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Effective Flat `TypeId` keys DAE types; `DefId` is provenance | `TypeArena` | Instances may differ |
| Nonprimitives use typed IDs | Value grammar | No name identity |
| Fields/enums use owner-local typed ordinals; enums are one-based | `TypeArena` | Unique MLS order |
| Operator records retain canonical bases | `TypeArena` | Explicit compatibility |
| External lifecycle uses typed functions | Function construction | No raw `DefId` |
| Variable identity differs from display; roles are typed and causality-orthogonal | `VariableArena` | No text identity |
| Proven parameter-variable families become calculated parameters atomically | ToDAE analysis/construction | One computable owner |
| Calculated-parameter bindings require finite shape and acyclic dependency proofs | ToDAE analysis | Reject unsafe promotion |
| Element type includes shape; shape products use checked multiplication | All constructors | Overflow fails |
| Attributes are checked on attachment | Variable construction | No drift |

Coordinates are primitive/enumeration rectangular values; function values may
include checked aggregates/external objects. Finite, inspectable proofs
monomorphize function extents before DAE construction. Loops preserve source
order as compact finite-domain transitions over typed carried values.
Unresolved, cyclic, overflowing, or zero-step domains fail at their owner; no
guessed extents/literal unrolling.
Partition ordinals are layout, never semantic identity.

### Expressions and Equations

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| `expr.at(provenance).operation(...)` creates every node | `ExpressionArena` API | Provenance mandatory |
| Parallel provenance stores exact identity/range/origin; text stays in `SourceMap` | DAE root | No duplicated text |
| Variadic children use packed buffers | Expression arena | Compact dense storage |
| Coordinates carry use spans; declarations carry declaration spans | Owning arenas | Distinct occurrences |
| Operands use active-build typed IDs | Expression API | No cross-build use |
| Nodes derive type, shape, variability, domain | Expression API | O(1) checks |
| Composite variability is operand maximum; coordinate owners supply it | DAE aggregate | No tree walk |
| Nested domains name their checked lexical parent | Domain arena | Explicit scope tree |
| Domain merging requires ancestry; comprehensions consume locals and retain parents | Expression API | Typed capture |
| Source temporal/flow calls are absent | Expression grammar | Closed boundary |
| Role conversion returns typed expression IDs | Expression API | Compile-time roles |
| Equations accept role-specific IDs | Equation systems | No generic forgery |
| Optional-lhs equations are prohibited | Final DAE | Role-defined form |

Per-system equation contracts are
[SPEC_0043 §4](SPEC_0043_CONSTRUCTION_CATALOG.md#4-equation-contract-catalog-spec_0036-expressions-and-equations).

Each non-input `m` has exactly one B.1c definition owner. A source
`when`/`elsewhen` chain becomes one atomic, source-priority-ordered conditional
definition of the branch target set; independent `when` owners cannot define
the same target, as required by SPEC_0022 EQN-020. Event-only updates of `m`
are therefore B.1c definitions, not a second generic event-action assignment
path.

The B.1c topology includes every current-`m` dependency reachable through the
value, branch guard, trigger condition, condition DAG, and relation
expression. `pre(m)` is a dependency leaf. The producer supplies stable
topological owner order, and the linear construction capability independently
checks that every reachable current-`m` dependency has already been issued in
that owner group before consuming the target. No exclusivity claim, event
iteration, or final graph scan repairs an invalid order.

Structured families own compact domains, checked bodies, typed scalar views,
and constructor-derived row counts; `rumoca-eval-dae` owns evaluation/lazy projection.

### Conditions, Events, Clocks, and Temporal State

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| `RelationId` and non-root `ConditionId` identify one relation and B.1 `c` | Condition system | Separate policy |
| Conditions compose relations/discrete operands | Condition expressions | Boolean composition |
| Relation/condition counts are independent; continuous relations intern once | `ConditionSystem` | Sharing is legal |
| Root activation is closed/typed; synthetic surfaces have root IDs | `RootArena` | No index identity |
| Coincident time events retain IDs | `EventSystem` | Preserve semantics |
| Actions own trigger, branch guard, action, provenance | `EventSystem` | Preserve edge and branch semantics |
| Clocks are typed; variables have one owner; exact `ClockLattice` is authoritative | `ClockSystem` | No rounded identity |
| Pre pairs with current `z`/`m` | `TemporalSystem` | Explicit coordinates |
| Solve may slot typed history coordinates | Solve lowering | Slots are not parameters |
| Previous retains its clock; terminal/delay coordinates are typed | `TemporalSystem` | No generated names |

Only continuously monitored closed activations receive roots. Clock-domain
environments filter typed capabilities without owning expressions. Delay
construction proves primitive shape, scalar-Real timing, and
`0 < delayTime <= delayMax` where applicable; Boolean claims/text-derived
runtime identity are prohibited.

### Transformations

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Passes consume/return immutable `Dae`; multi-object changes use closed operations | Structural/DAE API | Atomic changes |
| Operations are aggregate-bound, consuming, non-cloneable | DAE API | No replay |
| Inputs are closure-scoped to one DAE | DAE API | Lifetime ownership |
| Changed expressions use add operations | DAE API | Preserve checks |
| Repartitioning preserves provenance | Variable transform | Stable source |
| Structured owners cannot disappear silently | Structural transforms | Explicit loss |
| Mutable partition callbacks are prohibited | Public DAE API | No bypass |
| Changed contracts use named stage types | Phase boundaries | Visible semantics |
| Backend projections are immutable views | Compile/codegen | No mutation |

A transformation context is a lightweight replacement-aggregate capability,
not another IR. It may ownership-transfer unchanged immutable arenas; changed
objects use initial construction's checked adds. Persistent root seals,
change/receipt registries, generation tokens, and post-rewrite validation are prohibited.

### Serialization

Only DAE wire v12 exists; v11-and-earlier/pre-versioned payloads, adapters,
migration readers, and dual writes are prohibited.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Current wire records are private and deny unknown fields | DAE serde | Wire is not IR |
| Required collections are explicit, including empty | Private wire types | Omission is not ambiguity |
| Decode calls the same provenance-requiring operations | DAE serde | Deserialization is construction |
| Provenance serializes identity/range/origin only | DAE wire | Source text stays canonical |
| IDs project to deterministic wire-local ordinals; arenas to ordered arrays | DAE serde | Process IDs never leak |
| Non-dense key duplicates fail before insertion | DAE serde | Maps cannot hide malformed input |
| Derived counts and indexes are absent from wire | Private wire types | Caches cannot be forged |
| Invariant-bearing children have no fieldwise `Deserialize` | IR serde | Bytes cannot bypass checks |

Across `flat::Model`, `Dae`, and `SolveProblem`, a root may implement custom
`Deserialize` only by decoding private current-version records through checked
construction. Children cannot implement or derive fieldwise `Deserialize`.

### Other IR Boundaries

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Parse preserves recovered syntax | Parse AST | Diagnostics |
| Success returns opaque proofs | AST phases | Completed proof |
| Partial work gets no proof | AST phases | No forgery |
| `ParsedTree → ResolvedTree` has one mint | Resolve phase | One authority |
| `ResolvedTree → InstancedTree` has one mint | Instantiate phase | One authority |
| `InstancedTree → TypedInstancedTree` has one mint | Typecheck phase | One authority |
| `TypedInstancedTree → flat::Model` consumes by value and has one mint | Flatten phase | Closed proof chain |
| Proof fields/constructors are private; no `DerefMut`/mutable overlay | Owning phase | No forgery |
| Invariant fields private | `flat::Model`/`Dae`/`SolveProblem` | No bypass |
| Public root `validate()` prohibited | `flat::Model`/`Dae`/`SolveProblem` | Construction proves |
| Unchecked builders prohibited | `flat::Model`/`Dae`/`SolveProblem` | No weaker value |
| Sparsity patterns derived, not claimed | Solve construction | No unsafe under-approximation |

Instantiation applies modifications and builds its overlay; post-instance
typechecking sees those results; flattening expands checked connections. No raw
tree/overlay enters flattening. Consuming a proof transfers its unique phase
capability, not necessarily its immutable payload; payload sharing is allowed
when it cannot forge or mutate a proof.

### Refinement Obligations

Each phase defines a deterministic relation `R_phase(input, output)`. Opaque
proofs enforce order; the following obligations establish semantic correctness:

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Isolate deterministic transitions from I/O/diagnostics | Phase algorithms | Proof-ready relation |
| Preserve semantic identity, ordering, provenance, and supported behavior | `R_phase` | Refinement |
| Fail unsupported input at its first owner with typed provenance | `R_phase` | No false success |
| Use explicit producer-owned cross-stage ID maps | Phase transition | No ordinal assumptions |

Runtime proof capabilities are erased and add no serialized receipts or
duplicate IR. Compile-fail tests cover proof forgery/cross-stage use; property,
differential, and refinement tests exercise each relation.

### Enforcement

The evidence each guarantee requires is
[SPEC_0043 §5](SPEC_0043_CONSTRUCTION_CATALOG.md#5-enforcement-evidence-catalog).
Tests may privately audit the complete aggregate. Production audits, public
validation, superseded fallbacks, and compatibility are prohibited.

### Backend Capability Restoration

Capability deletion is not completion.

Structural, event, clock, temporal, algorithm, aggregate,
tensor, external-call, symbolic-export, FMI 2/3 ME/CS, eFMI, native, and Wasm
capabilities consume checked `Dae`/`SolveProblem` facts and retain equivalent end-to-end
evidence. Missing lowering fails at its first owner. Pending targets stay
undiscoverable; no stubs, alternate semantic paths, target aliases, silent
defaults, old-shape adapters, or compatibility readers.

## References

- [SPEC_0007](SPEC_0007_IR_PIPELINE.md) — stage contracts
- [SPEC_0021](SPEC_0021_CODE_COMPLEXITY.md) — complexity and deterministic storage
- [SPEC_0022](SPEC_0022_MLS_COMPILER_COMPLIANCE.md) — MLS contract index
- [SPEC_0029](SPEC_0029_CRATE_BOUNDARIES.md) — crate and rewrite ownership
- [SPEC_0032](SPEC_0032_RANGE_PRESERVING_TENSORS.md) — structured families
- [SPEC_0039](SPEC_0039_PROOF_CARRYING_SPARSITY.md) — sound dependency patterns
- [SPEC_0043](SPEC_0043_CONSTRUCTION_CATALOG.md) — construction catalogs
- [SPEC_0037](archive/deferred/SPEC_0037_FORMALLY_VERIFIED_COMPILER.md) —
  deferred formal-verification architecture
- [MLS Appendix B](https://specification.modelica.org/maint/3.6/modelica-dae-representation.html)
