# SPEC_0036: Valid-by-Construction Compiler IR

## Status
DRAFT

## Summary

Compiler-generated IR makes invalid stage values unrepresentable. DAE
construction uses sequential semantic-owner closures through `Dae::construct`
over one private aggregate, without weaker roots or duplicate storage.

## Specification

### Rollout

| Milestone | Scope | Status |
|---|---|---|
| DAE | Aggregate, children, wire, transformations | This branch |
| AST | Parsed, resolved, typed, instanced proofs | This branch |
| Flat | Instantiated symbolic model | This branch |
| Solve | Layouts, tensors, programs and derived sparsity | This branch |

Current public IR fields and Flat/Solve validators remain noncompliant. This
spec stays `DRAFT` until all milestones are implemented.

Proof-carrying sparsity is part of the Solve milestone and follows
[SPEC_0039](SPEC_0039_PROOF_CARRYING_SPARSITY.md). A sparsity annotation is not
an unchecked optimization hint: patterns admitted to finalized Solve artifacts
are derived by construction from checked programs and compact domains.

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

LOC metrics compare the merge base with the completed branch using the same
method. Production Rust is repository-wide; tests and generated code are
reported separately. Coexisting superseded and checked DAE fails this milestone.

### One Aggregate Owns Construction

`Dae::construct` creates one aggregate, lends its producer a branded capability,
and returns immutable `Dae` only on success.

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

`DaeDraft`, separate roots, data-owning builder chains, root-shaped partial
values, unchecked insertion, and finalized mutation are prohibited. Phase
capabilities may consume themselves but always borrow the same aggregate.

Every `add_*` checks applicable type, shape, variability, clock, domain, role,
ownership, and key contracts before insertion, then returns a branded typed ID.
Tree analysis stays in the producer; DAE checks local integrity and proofs.

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

Types, relations, roots, events, clocks and ownerships, temporal coordinates,
delays, and equations are inserted complete in producer-proved dependency
order. A function loop-transition slot exists only because the body transition
reads the preceding iteration's carried values; its linear authority is
consumed when the compact fold and its result expressions are inserted.
Reservation may be added to another domain only after an MLS-valid cycle is
demonstrated and this spec is amended.

An allowed forward arena may keep private slots and an O(1) local unfilled
count; definition authority is linear and branded. Final DAE has no unfilled
slot. Global `RemainingDefinitions`, parallel identity/definition maps,
data-owning reservation stages, seals, completion wrappers, validation passes,
validation switches, and unchecked paths are prohibited.

Build references carry one private session brand; finalized storage keeps only
typed IDs. The brand never affects equality, order, display, or wire data.

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

An arena owns typed identity and order; a system owns related arenas and
contracts. An environment is a read-only filtered query and owns no identity.
Facts attached to existing IDs are annotations.

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
| Element type includes shape | Variables/expressions | Local compatibility |
| Attributes are checked on attachment | Variable construction | No drift |

Coordinates contain permitted primitive/enumeration rectangular values.
Function values may also contain records, operator records, and external
objects. Aggregates never use synthetic array shapes. Enum expressions retain
type and literal ID; textual literal sidecars are prohibited.

Variable-size function arrays are monomorphized before DAE construction. The
producing phase derives one reachable specialization for each distinct concrete
input signature and supplies an inspectable shape certificate containing the
declared symbolic axes, call-site equalities, resolved parameter/result/local
extents, and selected specialization identity. Dependent extents are accepted
only when their source shape expressions evaluate exactly from already proven
axes. Unresolved axes, inconsistent equalities, and unsupported shape
expressions fail at their source owner; zero sentinels, guessed extents, and
single-signature fallback are prohibited. DAE function signatures contain only
concrete extents.

Function control flow stores source-order statements and a checked denotation.
A `for` statement owns one compact domain and a simultaneous transition over
its loop-carried values. Transition parameters and results use owner-local
typed identities; construction derives the carried set, checks every update,
and consumes the loop authority. Literal-domain unrolling and mutable
expression nodes are prohibited.

ToDAE supplies a finite-domain proof for every function loop. A bound is
statically known only when integer expression evaluation reaches literals
through function values that have source defaults and no assignment anywhere
in the function body. Runtime-dependent, cyclic, overflowing, or zero-step
domains fail at the loop owner; construction never guesses a bound or silently
unrolls a loop.

Partition ordinals are layout, not identity. Metadata stores typed canonical or
runtime facts. Descriptions, partial status, balance, ancestry, backend fields,
and structural reports stay separate. Eliminated-value starts use typed stable
source identity, never text.

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
| Source temporal/flow calls are absent | Expression grammar | Closed boundary |
| Role conversion returns typed expression IDs | Expression API | Compile-time roles |
| Equations accept role-specific IDs | Equation systems | No generic forgery |
| Optional-lhs equations are prohibited | Final DAE | Role-defined form |

Typed coordinates represent parameters, time, states, derivatives, algebraics,
discrete values, pre-values, conditions, delay outputs, and terminal state.
Source `der`, `pre`, `edge`, `change`, `sample`, `previous`, `initial`,
`reinit`, `assert`, and `terminate` never survive as generic DAE calls.

Literal, coordinate, operator, call, index, range, comprehension, and array
nodes each use their own source occurrence. Compiler-generated nodes use a
typed `DaeGeneration` and the nearest semantically responsible source span;
dummy provenance is prohibited. Equation `equal(lhs, rhs)` creates its
synthetic residual with the equation owner's span and generated classification.

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

Structured families own compact domains, checked bodies, and body-view mode.
`BinderSubstitution` substitutes checked binders and contributes every scalar
body element per domain point. `RowMajorProjection` requires each aggregate
body shape to equal the domain extents and contributes one scalar element per
body per point. Constructors derive row counts without double multiplication.
Scalar views retain typed family identity. Evaluation and lazy projection
belong to `rumoca-eval-dae`, not the IR crate.

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

Only relations whose closed activation requires continuous monitoring receive
roots. Discrete-only, literal-`noEvent`, and assertion-only relations do not;
synthetic event-generating numeric surfaces do.

Clock-domain expression environments filter capabilities without owning
expressions. Domain exclusion and unknown identity are distinct errors.
Runtime identity is never recovered from text or unrelated equations.

Delay sources are legal primitive scalar/array values. Two-argument delay owns
a strictly-positive scalar-Real parameter expression. Three-argument delay owns
unclocked scalar-Real `delayTime` and strictly-positive parameter `delayMax`,
requiring runtime `0 < delayTime <= delayMax`. Positive evidence stores the
expression and finite evaluated value using an O(1) witness check; Boolean
claims and tree walks are prohibited.

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

The DAE cutover may establish a smaller reliable core before every backend is
rewritten, but capability deletion is not completion. A backend removed during
the cutover remains a required restoration item until it consumes checked DAE
and Solve facts directly.

The same rule applies to compiler and runtime semantics. The reliable core must
reject a missing lowering at the first responsible phase; it must not erase the
construct, substitute a default, or produce a partially computable Solve
program. The following cutover gaps are required work, not optional features:

| Capability | Required checked replacement | Required evidence |
|---|---|---|
| High-index DAE reduction | Provenance-preserving Pantelides/dummy-derivative transformation that constructs a new checked DAE | Index-reduction unit cases and representative MSL mechanics |
| Alias elimination, tearing, and BLT preparation | Checked structural transformations with explicit preservation/loss receipts for every semantic owner | Structural equivalence and simulation parity |
| Relations, roots, conditions, and event actions | Typed DAE-to-Solve event partition including `reinit`, `assert`, and `terminate` | Event ordering, coincident-event, and MSL event simulations |
| Clocked equations and history | Typed Solve clocks for `sample`, `previous`, clock ownership, and exact lattices | Clock lattice, tick ordering, and clocked MSL simulations |
| Delay and terminal semantics | Typed history buffers and terminal coordinates with checked bounds | Delay/terminal unit tests and simulations |
| Coupled discrete Real equations and discrete-value updates | Computable B.1b/B.1c Solve partitions with explicit topology | Mixed continuous/discrete and event-iteration tests |
| External functions, tables, and random sources | Typed runtime operations with declared host/runtime capabilities | Import/call/table/random tests with deterministic failure modes |
| Initial algorithms and full algorithm statements | Semantic-owner construction for assignment, control flow, calls, and multi-output statements | Scalar, array, record, and initialization algorithm tests |
| Aggregate and multi-output expressions | Typed record/tuple/field and function-output projection operations | Record, tuple, array, and function regression suites |
| Dynamic array selection and remaining tensor forms | Checked index/domain operations or explicit target capability rejection | Tensor scaling, dynamic-index, and stencil tests |

| Capability | Required replacement | Required evidence |
|---|---|---|
| CasADi MX and SX | Typed Solve programs and checked declarations | Render, import, evaluate, differentiate |
| JAX | Typed Solve programs and checked declarations | Render, import, `jit`, `grad`, evaluate |
| SymPy | Checked expressions/systems or typed Solve programs | Render, import, symbolic solve, evaluate |
| SymForce | Checked expressions/systems or typed Solve programs | Render, import, residual/Jacobian evaluation |
| ONNX | Typed Solve program graph | Model checker and runtime evaluation |
| Julia ModelingToolkit | Checked expressions/systems or typed Solve programs | Julia load, structural simplify, evaluate |
| FMI 2/3 ME/CS in native, packaged, and Wasm forms | One checked kernel and FMI lifecycle; in-process Diffsol is an FMI 3 ME host | XML/WIT validation and cross-form lifecycle/trace parity; implementation design is tracked by non-active deferred SPEC_0038 |
| eFMI Algorithm Code and Production Code | Checked Solve/GALEC projection with no alternate semantic path | Schema/checksum validation, generated-C checks, and traceability evidence |

Pending backends are absent from target discovery and fail as unsupported.
Superseded templates, old-shape projections, target aliases, semantic stubs, and
silent defaults are prohibited even as temporary restoration mechanisms. The
checklist is complete only when each target is restored with equivalent
end-to-end behavior or an accepted spec explicitly changes its capability.

## Rationale

Correct construction must remove code, not create lifecycle ceremony. One
aggregate, local insertion, dense arenas, and justified forward slots preserve
invariants without parallel representations. `DaeDraft` and data-owning stage
builders recreate a weaker DAE and are rejected.

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

Before proposal, SPEC_0007 and SPEC_0029 require coordinated terminology and
construction-boundary amendments.
