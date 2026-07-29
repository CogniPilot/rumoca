# SPEC_0036: Valid-by-Construction Compiler IR

## Status
DRAFT

## Summary

Compiler-generated IR makes invalid stage values unrepresentable. DAE
construction uses sequential semantic-owner closures through `Dae::construct`
over one private aggregate, without weaker roots or duplicate storage.

## Specification

### Scope

DAE, AST phase proofs, Flat, and Solve must all satisfy this contract. The spec
stays `DRAFT` while any stage exposes public invariant fields or root
validators. Solve sparsity follows [SPEC_0039](SPEC_0039_PROOF_CARRYING_SPARSITY.md).

### DAE Milestone Acceptance

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Fail at the responsible add operation | ToDAE/wire decode | Earliest boundary |
| Failures carry typed errors and provenance | DAE API | Actionable defects |
| Every source semantic owner is consumed exactly once | ToDAE construction | Prevent silent omission |
| Unsupported semantics fail with typed provenance | First owning phase | Reject before simulation |
| Missing/failed semantics never become fallback values | All phase boundaries | Plausible wrong output is unsafe |
| Delete impossible-state checks and fallbacks | All consumers | Guarantees replace checks |
| Constructor checks replace validators | DAE cutover | One owner |
| Delete superseded DAE and wire atomically | DAE cutover | No compatibility |
| Report repository production LOC before/after | PR metrics | Total effect |
| DAE-related production LOC is net-negative | PR metrics | Demonstrates savings |
| Checked-DAE production is at most 11,000 LOC | `rumoca-ir-dae` | Bounds ceremony |

### One Aggregate Owns Construction

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| One aggregate owns every arena/system | `Dae` | One authority |
| Aggregate owns `ExpressionArena` | `Dae` | Canonical expressions |
| Producers use sequential owner closures | `Dae::construct` | Shared boundary |
| Handles borrow aggregate storage | DAE API | No duplicate IR |
| Handles are branded/phase capabilities only | DAE API | No data ownership |
| Build capabilities are non-serializable | DAE API | Build state stays private |
| Success returns immutable `Dae` | `Dae::construct` | No invalidation |
| Failure exposes no DAE | `Dae::construct` | No partial root |
| Finalization is O(1), excluding freezing | `Dae::construct` | No rescan |

`DaeDraft`, separate roots, data-owning builders, partial roots, unchecked
insertion, and finalized mutation are prohibited. Tree analysis stays in the
producer; DAE insertion checks local integrity and supplied proofs.

### Storage and Forward References

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Dense ID arenas build with `Vec` and `u32` IDs | DAE aggregate | Direct indexing |
| Dense arenas freeze to boxed slices | Final `Dae` | Compact storage |
| `IndexMap` serves non-dense semantic keys/order | DAE aggregate | Keyed lookup |
| Producers supply semantic order | Producing phase | Explicit order |
| Secondary indexes are derived | DAE aggregate | Unforgeable caches |
| Functions may reserve recursive and loop-transition slots | `FunctionArena` | Recursion/iteration |
| Variables may reserve header slots | `VariableArena` | Forward attributes |
| Conditions may reserve identity slots | `ConditionSystem` | Condition/runtime cycle |
| B.1c keeps an incremental topology capability | Discrete system | Ordered assignment |
| Other objects insert complete values | Owning arena/system | Ordered dependencies |

Only the listed domains may reserve. Their private linear definition authority
and O(1) unfilled count must reach zero before success. All other objects insert
complete values in producer-proved order. Global completion trackers, parallel
identity maps, seals, validation passes, and unchecked paths are prohibited.
Brands are build-only and never affect finalized equality, order, display, or
wire data.

### Canonical Arenas, Systems, and Environments

| Name | Responsibility | Required storage |
|---|---|---|
| `TypeArena` | Effective types | Dense entries + `TypeId` lookup |
| `FunctionArena` | Functions | Dense entries |
| `VariableArena` | Variables and role views | Dense entries |
| `ExpressionArena` | Immutable expressions | Parallel node/provenance/type vectors |
| `RelationArena` | Primitive relations | Dense entries |
| `RootArena` | Monitored surfaces | Dense entries |
| `ConditionSystem` | Relations, conditions, activation | Arenas + indexes |
| `EventSystem` | Events, schedules, actions | Arenas + indexes |
| `ClockSystem` | Clocks, lattice, ownership | Arenas + indexes |
| `TemporalSystem` | History, terminal, delays | Arenas + indexes |

### Type and Variable Identity

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Effective Flat `TypeId` keys DAE types | `TypeArena` | Instances may differ |
| `DefId` is provenance only | `TypeArena` | Not effective type |
| Nonprimitives use typed IDs | Value grammar | No name identity |
| Fields use owner-local typed ordinals | `TypeArena` | Unique ownership |
| Enum literals use owner-local one-based order | `TypeArena` | MLS order |
| Operator records retain canonical bases | `TypeArena` | Explicit compatibility |
| External lifecycle uses typed functions | Function construction | No raw `DefId` |
| Variable identity differs from display | `VariableArena` | No text identity |
| Roles use distinct typed IDs | `VariableArena` | Compile-time roles |
| Role and causality are orthogonal | `VariableArena` | Distinct semantics |
| Proven parameter-variable families become calculated parameters atomically | ToDAE analysis/construction | One computable owner |
| Calculated-parameter bindings require finite shape and acyclic dependency proofs | ToDAE analysis | Reject unsafe promotion |
| Element type includes shape | Variables/expressions | Local compatibility |
| Attributes are checked on attachment | Variable construction | No drift |

Coordinates use primitive/enumeration rectangular values; function values may
also contain checked aggregates and external objects. Function array extents
are concretely monomorphized from finite, inspectable shape proofs before DAE
construction. Function loops retain source order as compact finite-domain
transitions over typed carried values. Unresolved, cyclic, overflowing, or
zero-step domains fail at their owner; guessed extents and literal unrolling
are prohibited. Partition ordinals remain layout, never semantic identity.

### Expressions and Equations

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Aggregate creates expression nodes | `ExpressionArena` API | Known ownership |
| Every expression node has exact provenance | Parallel provenance vector | Lossless source traceability |
| Variadic children use packed buffers | Expression arena | Compact dense storage |
| Source text exists only in `SourceMap` | DAE root | Avoids duplicated text |
| `expr.at(provenance).operation(...)` inserts nodes | Expression owner scope | Provenance cannot be omitted |
| Coordinate nodes carry use-site spans | Expression arena | References retain occurrences |
| Declarations carry declaration spans | Type/function/variable arenas | Identity and use stay distinct |
| Operands use active-build typed IDs | Expression API | No cross-build use |
| Nodes derive type, shape, variability, domain | Expression API | O(1) checks |
| Composite variability is operand maximum | Expression API | No tree walk |
| Coordinate owners supply variability | DAE aggregate | One source |
| Nested domains name their checked lexical parent | Domain arena | Explicit scope tree |
| Expression domain merging requires an ancestry relation | Expression API | Reject unrelated binders |
| A comprehension consumes its local domain and retains its parent scope | Expression API | Nested capture stays typed |
| Source temporal/flow calls are absent | Expression grammar | Closed boundary |
| Role conversion returns typed expression IDs | Expression API | Compile-time roles |
| Equations accept role-specific IDs | Equation systems | No generic forgery |
| Optional-lhs equations are prohibited | Final DAE | Role-defined form |

| Equation contract | Owner/Where | Brief Justification |
|---|---|---|
| Continuous equations own checked residual IDs | Continuous system | B.1a has one form |
| Initialization uses initialization-specific IDs | Initialization system | Runtime rules differ |
| Discrete Real equations own Real residual IDs | Discrete system | B.1b may be coupled |
| B.1c updates own typed `m` targets and values | Discrete system | Assignment shape is explicit |
| Every non-input B.1c target has one definition | Discrete system | Missing or duplicate is impossible |
| Input `m` capabilities are read-only | Variable/discrete systems | Inputs cannot be assigned |
| B.1c dependencies use issued-order capabilities | Discrete system | Topology is incremental |
| Reinitialization owns typed state/value updates | Event system | State resets are explicit |
| Reinit branches preserve ordering and exclusivity | Event system | Multiple legal branches remain expressible |
| Caller-supplied scalar counts are prohibited | Equation domains | Counts are derived |

Structured families own compact domains, checked bodies, typed scalar views,
and constructor-derived row counts. Evaluation and lazy projection belong to
`rumoca-eval-dae`.

### Conditions, Events, Clocks, and Temporal State

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| `RelationId` identifies one relation | `RelationArena` | Policy is separate |
| `ConditionId` identifies one B.1 `c` | `ConditionSystem` | Not a root |
| Conditions compose relations/discrete operands | Condition expressions | Boolean composition |
| Relation/condition counts are independent | `ConditionSystem` | Sharing is legal |
| Continuous relations are interned once | `RelationArena` | No duplication |
| Root activation is closed and typed | `RootArena` | Monitored only |
| Synthetic surfaces have typed root IDs | `RootArena` | No index identity |
| Coincident time events retain IDs | `EventSystem` | Preserve semantics |
| Actions own trigger, branch guard, action, provenance | `EventSystem` | Preserve edge and branch semantics |
| Clocks have typed identity | `ClockSystem` | No textual identity |
| Clocked variables have one owner | `ClockSystem` | MLS association |
| Exact `ClockLattice` is authoritative | `ClockSystem` | No rounding identity |
| Pre pairs with current `z`/`m` | `TemporalSystem` | Explicit coordinates |
| Solve may slot typed history coordinates | Solve lowering | Slots are not parameters |
| Previous retains owning clock | `TemporalSystem` | Tick-local history |
| Terminal/delay coordinates are typed | `TemporalSystem` | No generated names |

Only continuously monitored closed activations receive roots. Clock-domain
environments filter typed capabilities without owning expressions. Delay
construction proves primitive shape, scalar-Real timing, and
`0 < delayTime <= delayMax` where applicable; Boolean claims and text-derived
runtime identity are prohibited.

### Transformations

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Passes consume/return immutable `Dae` | Structural phases | No partial mutation |
| Multi-object changes use closed operations | DAE API | Atomic changes |
| Operations are aggregate-bound, consuming, non-cloneable | DAE API | No replay |
| Inputs are closure-scoped to one DAE | DAE API | Lifetime ownership |
| Changed expressions use add operations | DAE API | Preserve checks |
| Repartitioning preserves provenance | Variable transform | Stable source |
| Structured owners cannot disappear silently | Structural transforms | Explicit loss |
| Mutable partition callbacks are prohibited | Public DAE API | No bypass |
| Changed contracts use named stage types | Phase boundaries | Visible semantics |
| Backend projections are immutable views | Compile/codegen | No mutation |

A transformation context is a lightweight capability over the replacement DAE
aggregate, not a second IR. It may preserve unchanged immutable arenas by
ownership transfer, but changed objects enter through the same checked add
operations as initial construction. Persistent root seals, change registries,
receipt stores, generation tokens, and post-rewrite validation are prohibited.

### Serialization

Schema version 11 is the only supported DAE wire version. Version 10,
pre-versioned payloads, adapters, migration readers, and dual writes are
prohibited.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Wire records are private and current-version only | DAE serde | Wire shape is not compiler IR |
| Records use `deny_unknown_fields` | Private wire types | Unknown data fails |
| Required collections are explicit, including empty | Private wire types | Omission is not ambiguity |
| Decode calls the same provenance-requiring operations | DAE serde | Deserialization is construction |
| Provenance serializes identity/range/origin only | DAE wire | Source text stays canonical |
| IDs project to deterministic wire-local ordinals | DAE serde | Process IDs never leak |
| Arenas project as ordered arrays | DAE serde | Dense identity stays native |
| Non-dense key duplicates fail before insertion | DAE serde | Maps cannot hide malformed input |
| Derived counts and indexes are absent from wire | Private wire types | Caches cannot be forged |
| Invariant-bearing children do not derive `Deserialize` | DAE IR | Bytes cannot bypass checks |

### Other IR Boundaries

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Recovered syntax remains representable | Parse AST | Diagnostics |
| Successful phases return opaque proofs | AST phases | Completed proof |
| Partial work receives no completed proof | AST phases | No forgery |
| Flattening accepts a typed instance | Flatten phase | Resolved input |
| Invariant fields are private | Flat/DAE/Solve | No bypass |
| Public root `validate()` is prohibited | Flat/DAE/Solve | Construction proves |
| Unchecked builders are prohibited | Flat/DAE/Solve | No weaker value |
| Sparsity patterns are derived, not claimed | Solve construction | No unsafe under-approximation |

### Enforcement

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Compile-fail tests cover private construction | IR API tests | No invalid assembly |
| Negative wire fixtures cover invariants | DAE serde tests | No forged DAE |
| Property tests compare private audits | DAE tests | Constructor defects |
| Shared/compound relations are tested | DAE/Solve tests | B.1 cardinality |
| Transform tests preserve families | DAE/structural tests | Consistent views |
| Consumers have no malformed-DAE branches | Repository review | Guarantees replace checks |
| LOC limits are CI/review metrics | DAE cutover | Bounded complexity |

Private test-only audits may inspect the complete aggregate. No production
audit, public validation pass, superseded fallback, or compatibility layer is
permitted.

### Backend Capability Restoration

Capability deletion is not completion. Structural, event, clock, temporal,
algorithm, aggregate, tensor, external-call, symbolic-export, FMI 2/3 ME/CS,
eFMI, native, and Wasm capabilities must consume checked DAE/Solve facts and
retain equivalent end-to-end evidence. Missing lowering fails at its first
owner. Pending targets stay undiscoverable; stubs, alternate semantic paths,
old-shape adapters, target aliases, silent defaults, and compatibility readers
are prohibited.

## References

- [SPEC_0007](SPEC_0007_IR_PIPELINE.md) — stage contracts
- [SPEC_0021](SPEC_0021_CODE_COMPLEXITY.md) — complexity and deterministic storage
- [SPEC_0022](SPEC_0022_MLS_COMPILER_COMPLIANCE.md) — MLS contract index
- [SPEC_0029](SPEC_0029_CRATE_BOUNDARIES.md) — crate and rewrite ownership
- [SPEC_0032](SPEC_0032_RANGE_PRESERVING_TENSORS.md) — structured families
- [SPEC_0039](SPEC_0039_PROOF_CARRYING_SPARSITY.md) — sound dependency patterns
- [SPEC_0037](archive/deferred/SPEC_0037_FORMALLY_VERIFIED_COMPILER.md) —
  deferred formal-verification architecture
- [MLS Appendix B](https://specification.modelica.org/maint/3.6/modelica-dae-representation.html)
