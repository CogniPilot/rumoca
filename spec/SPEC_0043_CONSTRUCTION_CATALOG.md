# SPEC_0043: Valid-by-Construction Catalog

## Status
REFERENCE

## Summary

Lookup catalog of DAE milestone evidence, reservation owners, canonical arenas,
equation contracts, Solve/Flat aggregate rows, and enforcement tests referenced
by [SPEC_0036](SPEC_0036_VALID_BY_CONSTRUCTION_IR.md).

## How To Use This Catalog

This annex holds no rules of its own. Every row below is a SPEC_0036 obligation
or a SPEC_0036 evidence requirement and is **normative by reference from
SPEC_0036**; the owning section in SPEC_0036 states the governing requirement
and links here. Adding, moving, or removing a row is a spec change.

## Specification

### 1. DAE Milestone Acceptance and Review Triggers

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Adds fail with typed errors/provenance | ToDAE/wire decode | Earliest boundary |
| Consume each source-semantic owner once | ToDAE construction | Prevent omission |
| Unsupported/missing semantics fail with typed provenance, never default | First owner | Prevent wrong output |
| Delete impossible-state checks/fallbacks | All consumers | Trust constructors |
| Constructor checks replace validators | DAE cutover | One owner |
| Delete superseded DAE and wire atomically | DAE cutover | No compatibility |
| Report before/after repository LOC; DAE production is net-negative | PR metrics | Demonstrate savings |
| Core above 11,000 LOC requires a module review | `rumoca-ir-dae`, excluding `model/wire*` | Bounds core ceremony |
| Wire above 3,250 LOC requires a module review | `rumoca-ir-dae::model::wire` | Bounds replay ceremony |
| Total above 14,250 LOC requires a module review | `rumoca-ir-dae` | Bounds aggregate ceremony |

These thresholds are review triggers, not acceptance ceilings. A fresh
module-level report must inventory code above a threshold, remove demonstrated
duplication and obsolete ceremony, and explain the semantics, construction
evidence, or readability carried by the remainder. Necessary explicit code may
remain above a threshold when that report finds no bloat. Audit-hostile
metaprogramming, code golfing, test deletion, and capability deletion are not
valid LOC reductions.

#### 1a. Measurement Convention

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Count physical lines of `crates/rumoca-ir-dae/src/**/*.rs` | trigger gate | One mechanical number |
| Exclude a `tests/` or `generated/` path segment, `tests.rs`, and `*_tests.rs` | trigger gate | Matches the repository production-source rule |
| Wire is `model/wire.rs` plus `model/wire/`; core is the rest; total is their sum | trigger gate | Three triggers partition one crate |

#### 1b. Exceedance Acknowledgment Ledger

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| An exceeded trigger MUST carry a ledger row naming its ceiling and reduction owner | this section | Exceedance is acknowledged, never silent |
| The ceiling MUST be the measured value rounded up to the next 250 lines | ledger row | A derived number, not chosen headroom |
| Crossing a 250-line step in either direction MUST update the row in the same change | ledger row | The number moves both ways |
| A trigger back at or below its threshold MUST lose its ledger row | ledger row | Retired debt must not linger |
| Acknowledging debt does not discharge the module review the trigger demands | this section | A ledger note is not a review |

| Trigger | Threshold | Acknowledged ceiling | Reduction owner |
|---|---|---|---|
| `dae-core-loc` | 11,000 | 13,750 | SPEC_0036 cutover: ranked non-wire reductions |
| `dae-wire-loc` | 3,250 | 4,000 | SPEC_0036 cutover: operation-shaped wire |
| `dae-total-loc` | 14,250 | 17,500 | Both items above; total follows their sum |

**Why:** the triggers were unenforced and all three were exceeded in silence.
The gate makes exceedance loud without blocking a landing: any measured value is
legal once its ledger row records it, and only crossing a 250-line step forces
the row to be rewritten. Measured, checked, and stated as an acceptance contract
in `crates/rumoca/tests/dae_loc_trigger_test.rs`.

### 2. Reservation Owner Catalog (SPEC_0036 §Storage and Forward References)

Only the entries listed here may reserve a slot before its complete value
exists. Every other object inserts complete values in proven order.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Dense arenas use `Vec`/`u32` IDs and freeze to boxed slices | DAE aggregate | Compact indexing |
| `IndexMap` owns non-dense keys/order; secondary indexes derive | DAE aggregate | Unforgeable lookup |
| Producers order semantics | Producing phase | Explicit order |
| DAE call-header reservation | `FunctionArena` | Proven recursive SCC only |
| DAE loops may reserve transition slots | `FunctionArena` | Checked finite iteration |
| Variables may reserve header slots | `VariableArena` | Forward attributes |
| Conditions may reserve identity slots | `ConditionSystem` | Condition/runtime cycle |
| B.1c keeps an incremental topology capability | Discrete system | Ordered assignment |
| Flat functions may reserve call headers | `flat::Model::construct` | Proven recursive SCC only |
| Flat variables may reserve headers | `flat::Model::construct` | Forward attributes/bindings/references only |
| Other objects insert complete values | Owning arena/system | Ordered dependencies |

### 3. Canonical Arenas, Systems, and Environments

| Name | Responsibility | Required storage |
|---|---|---|
| `TypeArena` | Effective types | Dense entries + `TypeId` lookup |
| `FunctionArena` | Functions | Dense entries |
| `VariableArena` | Variables/role views | Dense entries |
| `ExpressionArena` | Immutable expressions | Parallel node/provenance/type vectors |
| `RelationArena` | Primitive relations | Dense entries |
| `RootArena` | Monitored surfaces | Dense entries |
| `ConditionSystem` | Relations/conditions/activation | Arenas + indexes |
| `EventSystem` | Events/schedules/actions | Arenas + indexes |
| `ClockSystem` | Clocks/lattice/ownership | Arenas + indexes |
| `TemporalSystem` | History/terminal/delays | Arenas + indexes |

### 4. Equation Contract Catalog (SPEC_0036 §Expressions and Equations)

| Equation contract | Owner/Where | Brief Justification |
|---|---|---|
| Continuous equations own checked residual IDs | Continuous system | B.1a has one form |
| Initialization uses initialization-specific IDs | Initialization system | Runtime rules differ |
| Discrete initial values from initial algorithms or explicit `m = value` / `pre(m) = value` equations own a typed scalar target and one settled value | Initialization system | MLS §8.6 assigns, never solves |
| One discrete coordinate has at most one initial value | Initialization system | Duplicate is impossible |
| A discrete initial value reads only `time`, parameters, and constants | Initialization system | Nothing else is settled there |
| Discrete Real equations own activated Real residual IDs | Discrete system | B.1b may be coupled; trigger/guard ownership is explicit |
| B.1c updates own typed `m` targets and values | Discrete system | Assignment shape is explicit |
| Runtime binding equations contribute event owners by expression occurrence | Event analysis | MLS §4.4 binding syntax and equation-section syntax have the same MLS §8.5 event surface |
| Every non-input B.1c target has one definition | Discrete system | Missing or duplicate is impossible |
| Input `m` capabilities are read-only | Variable/discrete systems | Inputs cannot be assigned |
| B.1c dependencies use issued-order capabilities | Discrete system | Topology is incremental |
| A structured B.1c owner owns one checked domain/view and typed target/value bodies | Discrete system | Preserve the authoritative family |
| Structured B.1c construction derives scalar count and proves exact target coverage | Discrete system | Partial, duplicate, overlapping, and caller-count definitions are impossible |
| Reinitialization owns typed state/value updates | Event system | State resets are explicit |
| Reinit branches preserve ordering and exclusivity | Event system | Multiple legal branches remain expressible |
| Caller-supplied scalar counts are prohibited | Equation domains | Counts are derived |

### 5. Enforcement Evidence Catalog

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Compile-fail tests cover private construction | IR API tests | No invalid assembly |
| Negative wire fixtures cover invariants | DAE serde tests | No forged DAE |
| Property tests compare private audits | IR tests | Constructor defects |
| Shared/compound relations are tested | DAE/Solve tests | B.1 cardinality |
| Transform tests preserve families | DAE/structural tests | Consistent views |
| Consumers have no malformed-DAE branches | Repository review | Guarantees replace checks |
| LOC thresholds trigger documented review | DAE cutover | Bounded complexity |
| §1 triggers are measured and checked against the §1b ledger | `dae_loc_trigger_test.rs` | Unacknowledged growth fails |

Tests may privately audit the complete aggregate. Production audits, public
validation, superseded fallbacks, and compatibility are prohibited.

### 6. Solve Aggregate and Discrete Definition Catalog (SPEC_0036 §Solve Aggregate)

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| `SolveProblem::construct` lends branded scopes over private invariant fields | `rumoca-ir-solve` | One construction authority |
| `initial()` creates a typed activation, never a generic P load | Activation arena | Phase state cannot become a value |
| A B.1c definition owns a target and either `Always` or ordered activated branches | Discrete system | Preserve source priority |
| No active branch holds the current target | Discrete system | Hold cannot be omitted |
| Each typed owner derives pre, observation, and clock policy | Solve construction | Parallel metadata cannot disagree |
| B.1b residuals, B.1c definitions, reinit, and condition memory are distinct owners | Solve construction | Tags cannot conflate semantics |
| Definitions, branches, generated edges, and holds retain exact typed provenance | Construction scopes | No dummy source claims |
| Dense vectors, packed branches, and `u32` IDs freeze without rescanning | Solve aggregate | Linear construction |
| Wire decode replays the same owner operations | Solve serialization | Bytes cannot forge definitions |
| Old schemas, raw insertion, validators, defaults, and adapters are absent | Solve boundary | No weaker path survives |
| Tests use production construction and cannot bypass provenance or activation | Solve tests | Evidence exercises the boundary |
| Structured B.1c owners derive compact compute and target maps plus row/pre/observation/clock policy | Solve construction | Per-scalar metadata cannot become a parallel owner |
| Structured B.1c wire decode replays the same checked owner operation | DAE/Solve serialization | Bytes cannot forge a family or its target coverage |

### 7. Flat Aggregate Construction Catalog (SPEC_0036 §Flat Aggregate)

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| `flat::Model` owns private class-free grammar and equation families | `rumoca-ir-flat` | No classes/duplicate owners |
| Sequential scopes freeze the aggregate | `flat::Model::construct` | Checked ownership |
| Core expression shape is generic over its stage-owned reference payload | `rumoca-core` | One structural grammar |
| Generic expression shape has no default or untyped Flat payload | Core/Flat boundary | No unresolved mode |
| Flat owns total value, function, function-value, binder, enum, intrinsic, and generated targets | `FlatReferenceTarget` | Exhaustive reference kinds |
| Target views and nested function/domain IDs are branded | `flat::Model::construct` | No cross-owner IDs |
| Every reference occurrence carries its exact use span | Flat expression grammar | Exact diagnostics |
| Source `DefId` identifies declaration provenance only | Flat target entries | Instances need distinct identity |
| Names and source paths are display/protocol data only | Flat root views | No textual lookup |
| Flat wire decode replays root construction operations | Flat root wire | Constructor-enforced invariants |
| Flat display/serialization resolves targets through root-owned projections | Flat root views | IDs never leak |
| Synthetic instance `DefId` allocation and late reference repair are absent | Flatten/Flat boundary | No identity adapters |
| SPEC_0029 helper ownership changes with the implementing cutover | Same atomic change | Specs remain consistent |
| Every node requires source/generated provenance | `flat::Model::construct` | No dummy provenance |

## References

- [SPEC_0036](SPEC_0036_VALID_BY_CONSTRUCTION_IR.md) — owning construction
  rules and the obligations each catalog row serves.
- [SPEC_0007](SPEC_0007_IR_PIPELINE.md) — stage contracts.
- [SPEC_0022](SPEC_0022_MLS_COMPILER_COMPLIANCE.md) — MLS contract index.
