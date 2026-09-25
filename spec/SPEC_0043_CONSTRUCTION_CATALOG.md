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
| `dae-core-loc` | 11,000 | 18,000 | Retain checked conditional/tensor/loop-region and initialization parameter identity; retire downstream recovery during Solve Algorithm Block cutover |
| `dae-wire-loc` | 3,250 | 5,500 | Operation-shaped replay; consolidate correlation replay after construction coverage lands |
| `dae-total-loc` | 14,250 | 23,500 | Both items above; total follows their sum |

**Why:** the triggers were unenforced and all three were exceeded in silence.
The gate makes exceedance loud without blocking a landing: any measured value is
legal once its ledger row records it, and only crossing a 250-line step forces
the row to be rewritten. Measured, checked, and stated as an acceptance contract
in `crates/rumoca/tests/suite_gates/dae_loc_trigger_test.rs`.

**2026-09-12 scalar-owner index review:** production is 17,347 core + 5,431
wire = 22,778 lines. The [module review](../docs/dev-guide/src/tooling/multibody-coverage.md#scalar-row-owner-lookup)
accounts for 39 core lines: checked per-owner row ends replace repeated scans.
The index is compact, immutable, and absent from wire; no scalar owner is added.

**2026-09-12 initialization-owner review:** production is 17,308 core + 5,431
wire = 22,739 lines. The [module inventory](../docs/dev-guide/src/tooling/multibody-coverage.md#initialization-owner-module-review)
accounts for 145 core and 24 wire lines since the derivative-owner review.
It retains distinct initialization semantics and checked replay, shares the
wire value record, and finds no obsolete reader or duplicate parameter binding.

**2026-08-20 module review:** measured production source is 16,577 core +
5,292 wire = 21,869 lines. The step crossing comes from the checked
iteration-local SSA member of `FunctionFold`: core construction now owns its
local-only role, dominance, nonescape, and disjointness from the carried tuple,
while wire replay owns the matching exact identities. The former loop methods
were moved intact from the 2,000-line `model.rs` into `model/function_loops.rs`;
that split changes no aggregate LOC. Review found no second loop-region
abstraction, downstream recovery, or obsolete compatibility path to delete.
The remaining increment is the
construction and replay evidence required to avoid cloning multiply-used loop
expressions, so it remains under the updated derived ceilings.

**2026-09-11 module review:** measured production source is 17,163 core +
5,407 wire = 22,570 lines. Checked function-derivative links account for the
new construction and replay code. The
[module inventory and review](../docs/dev-guide/src/tooling/multibody-coverage.md#dae-module-review)
records the 502-line increase from the branch HEAD, retained obligations,
and removal of redundant field lookups. The ledger changes only to the
derived 250-line steps; the review triggers and totality-debt ceilings remain
unchanged.

**2026-09-25 evaluable-parameter review:** measured production source moved
from 23,218 to 23,358 total lines with the checked `evaluable` attribute
(construction, finish-time dependency check, and wire replay). The derived
rule rounds the measured value up to the next 250-line step, so the
`dae-total-loc` ceiling moves from 23,250 to 23,500; the review triggers,
core and wire ceilings, and totality-debt ceilings are unchanged.

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
| Exact authored component identities retain their variable and scalar selection through the equality closure used for transferred initial rows. Structural preservation inventories every source-fixed continuous Real declaration independently of its state or algebraic role. Solve emits each initial equation through a transferred row or its remaining-coordinate inventory, including default starts. Each transferred row identifies its source scalar; coverage of one component cannot discharge the remaining declaration's initial equations. Dynamic or nonlinear dependencies prove no such identity | Structural initial-value transfer and Solve initial-equation inventory | Fixed equations belong to exact quantities, including array elements |
| Structural index reduction retains every differentiated manifold in the separate initialization solve together with all source-fixed initial conditions. A source-fixed state whose start is proved independent of initialization unknowns is a given input with no projection or update writer; dependent starts and transferred initial values remain equations in the same solve. Checked initialization construction derives targets and unknown inventories from one disjoint projection plan, and root assembly requires every retained manifold row. After this simultaneous solve settles, initialization and its event boundary may certify the manifold through read-only state access; only continuous-time correction may move states | Structural index-reduction admission and Solve manifold construction | Initial guesses cannot replace fixed equations; later correction cannot overwrite their settled values |
| Discrete initial values from initial algorithms or explicit `m = value` / `pre(m) = value` equations own a typed scalar target and one settled value | Initialization system | MLS §8.6 assigns, never solves |
| One discrete coordinate has at most one initial value | Initialization system | Duplicate is impossible |
| A discrete initial value reads only `time`, parameters, and constants | Initialization system | Nothing else is settled there |
| A variable's `evaluable` attribute attaches only to a fixed, non-tunable parameter whose binding has at most parameter variability; when every declaration is complete, each evaluable binding reads only constants and other evaluable parameters, through pure calls. ToDAE sets it only for parameters without `Evaluate=false` whose bindings evaluate from constants and other such parameters and that are `final`, `Evaluate=true`, or dependent on such a parameter (MLS §4.5, §18.6); structural reconstruction and wire replay repeat the check | DAE variable construction and wire replay | STRUCT-T10(a) folds a value only under a checked license, never a tunable or runtime-dependent one |
| Discrete Real equations own activated Real residual IDs | Discrete system | B.1b may be coupled; trigger/guard ownership is explicit |
| B.1c updates own typed `m` targets and values | Discrete system | Assignment shape is explicit |
| Runtime binding equations contribute event owners by expression occurrence | Event analysis | MLS §4.4 binding syntax and equation-section syntax have the same MLS §8.5 event surface |
| A dynamic scalar `div`/`mod`/`rem` quotient is admitted only with scalar operands and a divisor of at most parameter variability that is rejected when statically zero or non-finite, and constructs with exactly one recorded runtime owner; a model owner atomically issues the quotient, its six-node canonical continuous boundary indicator, exact relation, generated `Always` activation, and root, while a function-body owner names the exact open function and is event-free per MLS §3.7.2; the owner registry is canonical in quotient-expression order on every recording path; structural transformation and checked wire decode re-issue the one staged checked owner operation from typed identities at the exact claimed source stream positions, omit only construction-derived artifacts, and fail closed on any missing, duplicate, foreign, out-of-order, or partially replayed owner | DAE runtime-quotient construction, structural replay, and checked wire replay | A dynamic discontinuity cannot lose or duplicate its event surface, forge function event-freedom, become statically admitted through reconstruction, or expose owner or stream order that depends on replay timing |
| Whole-record equality derives leaf owners only from exact record/field identities and equal complete layouts; each array field remains one tensor equation | Flat-to-DAE record-equation analysis | Aggregate routing cannot depend on rendered names or source scalarization |
| A row-major scalar family recovers its whole tensor only when every member is an exact full-rank integer projection of one identical base, in canonical coordinate order, and the base dimensions equal the claimed extents | DAE expression construction | Source scalarization cannot force backend scalarization, while reordered, partial, duplicate, or mixed-base families cannot forge aggregate identity |
| Straight-line function-loop scratch elimination substitutes only scalar call-locals whose definitions dominate their uses, whose expression dependencies remain unchanged through every substituted use, whose name is not assigned again in the substituted suffix, whose final values are not read after the loop, and whose substitution has at most one use. A multiply-used nonescaping local instead becomes an explicit iteration-local SSA definition in the compact fold transition: construction clears its enclosing value on entry, requires a dominating per-iteration definition before every read, records it as region-owned but not loop-carried, and restores the enclosing reaching definition on exit. Unresolved self-reads remain explicit loop-carried transitions. | Flat-to-DAE function-loop analysis and DAE function-fold construction | Compact loop owners cannot erase loop-carried state, replace a snapshot with a later live tensor read, bypass a sequential redefinition, erase escaping values, or turn shared per-iteration work into a cloned expression tree |
| A function loop with call-scoped actions and no carried values constructs one zero-carried compact fold over the checked domain | Flat-to-DAE function-loop analysis | Scratch substitution may expose assertion-only loops without erasing or scalarizing their per-point actions |
| In a function with certified early-return output seeds, invariant-guard pushdown may use those exact seeded outputs as the false-arm values of compact loops; an ordinary unseeded output must already have a whole definition | Flat-to-DAE function-loop normalization | Guard pushdown preserves both the returned path and the non-return loop path without inventing an output value |
| An ordered unit-step first-match loop compacts only from a zero seed, an exact `found == 0` guard, and an update to the current binder; its tensor reduction appends an out-of-range sentinel before `min` and maps the sentinel back to zero | Flat-to-DAE function-loop normalization | The compact form is total for empty/no-match domains and preserves the source loop's first-match semantics without scalar expansion |
| A direct `x := x + term`, `x := x - term`, or `x := x * term` loop over a binder-dependent range compacts to the corresponding `sum` or `product` comprehension joined with its exact seed | Flat-to-DAE function-loop normalization | Additive and multiplicative identities preserve empty-domain behavior while dependent scalar iteration never enters DAE IR |
| Conditional target certificates inside a compact function domain resolve the selected nested branch before collection and union monotonically across all domain points | Flat-to-DAE function-loop definedness analysis | A later point or an outer guard cannot erase a tensor update owned by an earlier point or nested branch |
| A `GuardedFunctionFold` names a previously defined scalar activation materialized outside its guarded path and checked initial/capture ranges; inherited activation is consumed once by its owning fold, while nested folds own only guards introduced in their update. A guarded nested function-fold output additionally owns exact polarity, a nonempty in-bounds result range, and the corresponding parent-carried range. Reuse identity includes the activation path/function context and, for nested output, parent-carried versions. | Solve function-fold construction and checked wire replay | Lazy backend execution cannot recursively guard an activation definition, read an uninitialized result, cross branch ownership, duplicate an inherited guard at every nested fold, or reuse a result under a different activation path |
| A multi-target function conditional constructs its ordered branch correlation and all joined definitions in one atomic operation; target uniqueness, branch-vector completeness, expression ownership, current reaching definitions, and value-type compatibility are checked before any definition advances | DAE function-body construction | No partial join can escape, and a backend receives proof-bearing shared control flow instead of reverse-engineering it from independently projected conditional expressions |
| A model-event algorithm transaction constructs atomically with its exact source statement order, activation/clock topology, mixed discrete Real/discrete-value target tuple, current/`pre` reaching definitions, aggregate types, and issued pure-call occurrences; every statement path must define a compatible next environment and every final target must have unique complete coverage before the derived B.1b/B.1c views become visible | DAE model-event construction and checked wire replay | A partial transaction, stale reaching definition, split target class, forged call occurrence, or backend-reconstructed statement schedule cannot escape construction |
| A compact tensor transpose separately checks nonzero row/column extents, nonzero trailing element width, arithmetic lane count, total source/result range, and overflow; rank-two uses element width one and forward AD changes only lanes | Solve scalar-program construction and checked wire replay | Higher-rank MLS transpose cannot be scalarized, truncated, or confused with primal/tangent storage |
| Solve scalar lowering stores a literal-only scalar (literals, constants, `+`, `-`, `*`, `/`, and the `zeros`, `ones`, `fill`, `identity`, `outerProduct`, `skew`, and `transpose` generators) as one literal, reuses the operand of `x * 1`, `1 * x`, `x / 1`, `x - (+0)`, and `-(-x)`, and drops each literal store or unary op no later op reads, repeating until none remains, with liveness taken from the register-flow source checks. Every such fold is the IEEE value of the same operations in source order. `x + 0` is not folded (`-0 + 0` is `+0`), transcendental builtins are not evaluated at translation, and a product with a literal zero factor is dropped only where the structural incidence proof (SPEC_0032) drops the same term, because `0 * x` is NaN for a non-finite `x` | Solve scalar-program construction | Literal arithmetic costs no runtime work, and no fold changes IEEE results beyond what the incidence proof already licenses |
| A `FunctionConditionalProgram` constructs only from a nonempty ordered condition list, an equal number of branch result regions, one fallback region, exact capture and result tuple types/shapes, and branch programs whose outputs completely define that tuple; its enclosing op proves capture/result ranges and full call/activation reuse identity | Solve function-conditional construction and checked wire replay | Malformed or partially correlated branches are unrepresentable, and lazy native control flow cannot read an undefined capture or return a partial tuple |
| A `ScalarProgramBlock` function-conditional table issues one id per exact semantic call identity, stores each checked body once, and accepts references only with complete in-bounds compact capture/result ranges matching that owner's typed shapes and arithmetic profile | Solve scalar-program-block construction and checked wire replay | A scalar projection cannot clone a body, forge cross-row reuse, enumerate a tensor ABI, or invoke one owner with different captures/results |
| One multi-output continuous aggregate-call group constructs only from algebraic residuals whose typed `Field`/`Index` projection chains terminate at the same pure DAE call `ExprId`; construction derives every scalar width and explicit output ordinal, rejects duplicate/non-algebraic members, and retains per-output dependency and assignment-isolation proofs | DAE-to-Solve continuous scalar-program construction | A grouped program restores one exact source call occurrence without graph matching, scalar expansion, name inference, or accidental reuse of a distinct/impure call |
| A prepared lazy-row plan derives separate interpreter-trace and native-specialization capabilities while visiting the checked row once: any `FunctionConditional` or `GuardedFunctionFold` preserves interpreter laziness but makes trace-derived native replacement unconstructible; only scalar-`Select` rows can issue that replacement | Solve evaluator preparation | Runtime observation cannot override aggregate branch ownership, manufacture hundreds of scalar guard outputs, or turn a compact owner into a pruned reconstructed graph |
| A clock-owned guarded assignment derives its reachable source-priority branch suffix from typed DAE conditions; the first `Always` or exact owning-`ClockId` branch supplies the initial value and makes hold plus every lower-priority branch absent from the constructed program | DAE-to-Solve discrete construction | An unconditional clock-tick update cannot carry an unreachable old-value load or scalar true-condition selection into Solve IR, while unclocked and different-clock activation remains intact |
| A guarded-assignment activation owner constructs from the exact optional clock, trigger `ConditionId`, guard `ConditionId`, trigger-memory slot, expected polarity, and materialized register before its branch value is lowered; context, fold, conditional, assertion, and cache identities retain that owner | DAE-to-Solve discrete scalar-program construction | Branch-local aggregate work cannot lose its activation, reuse across a different event path, or require post-expansion trace recovery |
| A `GuardedAssignmentProgram` constructs atomically from aggregate DAE updates with one exact clock/pre policy and identical ordered activation topology; construction derives branch reachability, aggregate widths, compact result ranges, and affine target ranges, proves complete nonoverlapping simultaneous coverage and every arm/fallback result shape, and retains selected-arm activation ownership without constructing scalar target/value coordinates | DAE-to-Solve discrete construction and checked wire replay | A correlated clock/event update cannot become per-coordinate `Select` graphs, execute an inactive tensor algorithm, forge a partial result tuple, overlap storage, or require runtime reconstruction of its state-transition relation |
| A checked scalar-program owner derives its immutable register/output execution certificate once, validates each issued function-conditional body once by owner identity, and exposes the certificate only together with the aggregate it proves; wire replay reconstructs the same certificate before construction succeeds | Solve scalar-program and guarded-assignment construction | Evaluators and final backends cannot repeatedly clone/replay nested control-flow validation, trust forged register capacities, or separate a proof from its exact compact owner |
| A model-level pure-call table issues one id for one exact DAE call occurrence/context, constructs its compact typed capture/result/predicate ABI and finite acyclic owner graph atomically, and accepts every residual/root/action/value reference only as an in-bounds projection of that owner; construction also derives the complete coordinate-invalidation inventory used by an invocation certificate | Solve model and pure-call construction plus checked wire replay | A call body or assertion path cannot be duplicated, forged, recursively cyclic, or reused after any time/state/history/table/arithmetic input changes, and final backends receive one tensor-native helper relation rather than expanded scalar graphs |
| An `EventTransactionProgram` constructs only from one DAE-issued model-event transaction, a canonical nonempty activation-clock set, one exact member clock per target, exact activation/history lanes, ordered typed statement regions, compact intermediate/final ranges, and in-bounds projections of issued pure-call owners; construction proves each transition's input definitions, complete final target coverage, nonoverlap, exact clock-set coverage, a target-result prefix followed only by checked Boolean assertion predicates, a nonempty exact action-index set per predicate, lazy held fallback for every inactive clock region, commit atomicity, and exact settled/invalidation inventory before exposing the owner. Each target also owns one aligned construction-issued producer projection; whole-model replay proves that it equals one complete canonical storage run on its member clock, that claims do not overlap, and that any scalar or guarded executable producer is covered as one complete program before suppression. The issued clock partition contains the transaction once and activates it on the union of its member clocks, so coincident clocks cannot invoke it twice. | DAE-to-Solve event construction and checked wire replay | Runtime and generated targets cannot split one controller tick, execute a clock owner twice, eagerly execute an inactive clock region, discard or independently recompute a call assertion, suppress only part of a producer, observe a partial commit, forge invocation reuse, or rediscover transaction identity from scalar programs, bodies, spans, or target matching |
| A whole-clock causal transition/partition plan (pending: SPEC_0046, pre-implementation) issues, for one clock, the complete ordered producer list with each producer's typed rank, its intermediate-definition identity, and its member kind; ranks, identities, and member kinds are issued values that consumers read directly and never recover by searching an order list, comparing positions, or matching targets, names, spans, or provenance. A discrete-value producer is admitted to exchange membership only under the same dependency/cycle proof that admits a discrete Real producer, and a directed cycle among exchange members fails plan construction. | Structural causal-plan construction | Positional rank recovery is quadratic and forgeable, and a discrete-value definition admitted without the shared proof lets a same-clock self- or mutually-reading definition acquire left-limit semantics the DAE never proved |
| A clock-partition producer (pending: SPEC_0046, pre-implementation) is issued exactly one member kind at construction. An EXCHANGE member requires exact per-tick activation totality: the owning clock alone is total, and `And(clock, non-clock predicate)` or any other narrower guard is total only under a construction proof over that tick. Every other guarded producer is issued as a HOLD-FALLBACK member that keeps the checked SOLVE-C07/C10/C47/C49 hold semantics, whose target a same-tick reader observes through the SOLVE-C22 event-entry history view. Construction rejects, at that producer's span, only a row claiming exchange membership without its totality proof, and it never re-issues a member kind to repair a later failure. | `ClockPartitionTransactionProgram` construction | Returning the owning clock for a narrower conjunction treats a producer that does not define its target on some tick as an always-fresh definition, while rejecting that conjunction would contradict the accepted hold rows |
| One proof and lowering route (pending: SPEC_0046, pre-implementation) spans unconditional and guarded clocked producers alike, and both member kinds travel it; a partition in which only guarded producers pass through the owner while unconditional producers retain an independent path is unconstructible | `ClockPartitionTransactionProgram` construction | Two routes are what left unconditional B.1b chains stale under atomic snapshot evaluation |
| Each admitted producer (pending: SPEC_0046, pre-implementation) lowers exactly once into one compact SSA/typed region. A consumer reference to an exchange member is an in-bounds load of its issued intermediate; a consumer reference to a hold-fallback target is that target's checked storage read. Construction rejects a second lowering of one producer, a load of an unissued or undominated intermediate, and any re-entry, reporting coverage, ordering, cycle, and re-entry failures as typed contract errors rather than selecting storage semantics at runtime. Intermediate visibility does not commit unrelated targets, and the complete final-target tuple commits atomically | `ClockPartitionTransactionProgram` construction and checked wire replay | Recursive consumer inlining and runtime memoization duplicate producer graphs and compact tensor work, and a recursion guard that silently reads old storage invents unproved semantics |
| A target owned by a DAE-C21/SOLVE-C55 model-event transaction is excluded from clock-partition admission (pending: SPEC_0046, pre-implementation); the transaction is that target's owner, C57 producers consume its committed final values, and construction rejects any partition member that claims, re-derives, or re-orders a transaction target | `ClockPartitionTransactionProgram` construction | DAE-C21 already requires downstream executable phases to consume the transaction owner; a second equation-shaped owner for the same target would split one controller tick |
| A SOLVE-C28 boundary ends a same-instant producer path inside the partition (pending: SPEC_0046, pre-implementation): an explicit `sample(u)` source keeps its event-entry left-limit lane and a state derivative terminates the chain, so neither becomes a same-tick intermediate and neither creates a cycle among exchange members | `ClockPartitionTransactionProgram` construction | Same-tick exchange must not rewrite the sampled left limit MLS §16.5.1 defines, nor turn a legal feed-forward `hold`/`sample` pipeline into proved feedback |
| A clocked row the causal plan leaves unowned has no clock-partition membership (pending: SPEC_0046, pre-implementation) and keeps its existing checked disposition: `CausalDiscreteError::NonComputable` at that row's span, surfaced as `LowerError::non_computable` by Solve and as `coupled-discrete-real-equation` by GALEC. A compact coupled-owner member kind that solves such a block inside the partition is future work and requires a spec amendment | Structural causal-plan and `ClockPartitionTransactionProgram` construction | An unowned row is a named span-bearing rejection today, not a silent hold and not a coverage hole the new owner may quietly absorb |
| A typed conditional region constructs from one scalar Boolean condition, exact typed captures, two complete same-typed result tuples, and prior issued call owners; a typed finite-loop region additionally constructs from one valid compact domain, exact Integer binders, same-typed initial/transition carried tuple, and invariant captures. Region-local reads require dominating definitions, every output is stored once, and wire replay recursively rebuilds the same interfaces before exposing the parent. | Shared typed-program construction and checked wire replay | Lazy branches and finite loops cannot return partial/mismatched values, read hidden state, form recursive owners, unroll in IR, or smuggle scalar bytecode behind a typed operation |
| Every typed tensor-algebra operation derives its result type from checked operand types plus the minimum compact axis/domain metadata, rejects incompatible ranks/extents or arithmetic profiles before issuing a destination, and wire replay calls the same constructor | Shared typed-program construction and checked wire replay | Tensor algebra cannot acquire forged result shapes, scalar coordinate lists, or a second backend-specific semantic lowering |
| One branded causal-discrete plan orients executable discrete Real residuals by unique-target forced elimination after excluding causal-algebraic rows; current reads are distinct from `pre`, and ambiguous, cyclic, self-reading, or type/shape-incompatible rows remain unowned | Structural construction | Solve and GALEC cannot assign different meanings to one B.1b equation |
| The branded causal-definition owner issues an event-held declaration fact only after exact whole-aggregate or complete scalar coverage is oriented acyclically and its defining expression DAG contains no continuous coordinate, delay, impure call, unresolved function-local coordinate, or algebraic dependency outside the already issued event-held set | Structural and Solve variable-declaration construction | A continuous-looking alias can carry a compact, formally auditable piecewise-constant domain without names, traces, scalar graph expansion, or forged wire metadata |
| Causal-definition ordering builds readiness from distinct candidate dependencies once and emits the earliest ready entry in the constructed candidate list; dependency updates cannot rescan all candidates. Dependencies outside that candidate inventory retain their existing closure obligations, and cycles remain unowned | Structural causal-definition construction | A dependency queue preserves exact deterministic ordering without quadratic repeated searches or changing elimination proofs |
| Every non-input B.1c target has one definition | Discrete system | Missing or duplicate is impossible |
| Input `m` capabilities are read-only | Variable/discrete systems | Inputs cannot be assigned |
| B.1c dependencies use issued-order capabilities | Discrete system | Topology is incremental |
| A structured B.1c owner owns one checked domain/view and typed target/value bodies | Discrete system | Preserve the authoritative family |
| Structured B.1c construction derives scalar count and proves exact target coverage | Discrete system | Partial, duplicate, overlapping, and caller-count definitions are impossible |
| Reinitialization owns typed state/value updates | Event system | State resets are explicit |
| Reinit branches preserve ordering and exclusivity | Event system | Multiple legal branches remain expressible |
| A representable call-scoped function assertion constructs one call-specialized guarded root and one row-aligned guarded assertion action; root/action metadata are appended together or not at all | Solve event construction | Pure scalar programs retain exact call activation while initialization and root location enforce the assertion |
| A cached scalar function-fold result is keyed by its exact call plus the carried values and domain points of every active fold its checked initial/update expression graph reads | Solve scalar construction | Equal keys prove equal relevant loop environments; unrelated active loops cannot cause redundant re-expansion and a loop dependency cannot reuse a value from the wrong iteration |
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
| A false narrower guard case proves an `And(clock, predicate)` producer that is false on a tick is issued as a hold-fallback member whose target holds and whose same-tick reader observes the left limit, while a row claiming exchange membership without a totality proof is rejected at its own span (pending: SPEC_0046, pre-implementation) | Clock-partition transition tests | Activation totality must be evidence, not assumption, and over-rejecting the conjunction would contradict SOLVE-C07/C10/C47/C49 |
| A reverse-ordered unconditional B.1b chain case proves same-tick value exchange independent of source, row, and storage order (pending: SPEC_0046, pre-implementation) | Clock-partition transition tests | This exact shape stayed stale when only guarded producers were routed through the owner |
| Mixed B.1b/B.1c chain and cycle cases prove both value kinds are admitted under one dependency proof and that every directed cycle among exchange members fails construction with a typed error (pending: SPEC_0046, pre-implementation) | Clock-partition transition tests | Cross-role cycles are the unproved left-limit path |
| A compact-tensor case proves a tensor-valued producer and its consumers traverse the owner without coordinate enumeration (pending: SPEC_0046, pre-implementation) | Clock-partition transition tests | SPEC_0032 tensor-native survives the new owner |
| A growth case proves program size is linear in producer count, with no producer duplicated per consumer (pending: SPEC_0046, pre-implementation) | Clock-partition transition tests | Consumer-side inlining is observable only as size growth |
| An event-transaction case proves a clocked target owned by a DAE-C21 transaction is not admitted as a partition member and that a partition producer reading it consumes the transaction's committed value (pending: SPEC_0046, pre-implementation) | Clock-partition transition tests | One controller tick must not gain a second equation-shaped owner |
| A boundary case proves `y = hold(h); s = sample(y); h = 2*s` stays accepted with `s` reading its event-entry left-limit lane, while the explicit-`sample`-free instantaneous feedback `s = hold(h); h = 2*s` keeps its SOLVE-C28 rejection (pending: SPEC_0046, pre-implementation) | Clock-partition transition tests | Same-tick exchange must move neither side of the accepted MLS §16.5.1 boundary |
| An unowned-row case proves a self-reading or ambiguous clocked B.1b row still reports `LowerError::non_computable` in Solve and `coupled-discrete-real-equation` in GALEC at its own span (pending: SPEC_0046, pre-implementation) | Clock-partition transition tests | The disposition of a row outside the causal plan is a named rejection, not a new coverage failure |

Tests may privately audit the complete aggregate. Production audits, public
validation, superseded fallbacks, and compatibility are prohibited.

### 6. Solve Aggregate and Discrete Definition Catalog (SPEC_0036 §Solve Aggregate)

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| `SolveProblem::construct` lends branded scopes over private invariant fields | `rumoca-ir-solve` | One construction authority |
| `initial()` creates a typed activation, never a generic P load | Activation arena | Phase state cannot become a value |
| A B.1c definition owns a target and either `Always` or ordered activated branches | Discrete system | Preserve source priority |
| No active branch holds the current target | Discrete system | Hold cannot be omitted |
| Each typed owner derives pre, observation, clock, and `RuntimeAssignmentRole` policy; observation construction also certifies whether any selected scalar row reads Y storage and therefore needs algebraic/discrete iteration. `RelationFree` may consume a frozen relation-memory result, while relation evaluation, transitive dependence on a relation-evaluating owner, and writes to relation memory construct `RelationEvaluating`. Construction issues `post_commit_assignment_runtime_rows` only as a bounded, unique, owner-order projection through `RelationFree` runtime-assignment rows reachable from typed root-relation-memory targets without crossing a `RelationEvaluating` row | Solve construction | Parallel metadata cannot disagree, runtime cannot rediscover observation coupling from bytecode, or authorize relation-evaluating post-commit replay |
| Each lowered root-relation output derives exactly one row-aligned `RootRelationRefreshRole`; `Frozen` is issued only with a proof that its typed dependency closure contains no continuous algebraic coordinate, `AlgebraicDependent` only with a proof that one exists, and unsupported dependency forms fail construction | Solve event construction | Refresh ownership remains aligned with `root_conditions` and `root_relation_memory_targets` and is never recovered by runtime inspection |
| Each typed discrete owner derives an integrator-history effect from the finalized dependency graph; only a proved non-continuous target constructs `Preserve` | Solve construction | Unknown, cyclic, ambiguous, unsupported, and state-affecting updates fail closed to `Restart` |
| B.1b residuals, B.1c definitions, reinit, and condition memory are distinct owners | Solve construction | Tags cannot conflate semantics |
| An unclocked scalar model relation with a checked root derives one dedicated current-truth P buffer from its exact DAE expression identity; ordinary expression consumers read it, root programs evaluate the original operands, and condition-edge history and whole Boolean assignments have separate storage. Repeated uses share the same issued truth owner; no model names or observed signs select ownership | DAE-to-Solve layout, expression, and event construction | Compound conditions retain the located side without corrupting edge history |
| Definitions, branches, generated edges, and holds retain exact typed provenance | Construction scopes | No dummy source claims |
| Dense vectors, packed branches, and `u32` IDs freeze without rescanning | Solve aggregate | Linear construction |
| Pure-call construction and wire replay borrow the already-issued owner prefix through constant-time ID lookup; nested regions and dependency, value-projection, and affinity derivation reuse that view without materializing previous-owner interface arrays. Generative program brands, exact owner identity, and acyclic availability remain mandatory | Solve typed call construction | Interface size must not multiply by the number of later owners |
| Root and nested pure-call registration share one table-scoped lookup keyed by the exact DAE-issued call-owner identity. Only completed owners are reusable, after the active-function recursion check; distinct call identities never merge through names, spans, or body equality. Each caller retains its actual argument tuple, activation, ordered result projections, and every invocation's assertion predicates. Finishing a table discards its registration scope | DAE-to-Solve pure-call construction | Repeated demand for an identical nested source owner must not expand an acyclic call graph into an invocation tree or authorize reuse across caller input coordinates |
| Each prepared refresh row owns the selected isolator for its exact target | Solve evaluation construction | Runtime cannot rediscover another target shape |
| Assignment construction proves unique destination ranges for each source prefix with a compact producer index; every nested prefix lookup uses the same index and excludes later producers. Range overlap remains a refusal to issue an unversioned assignment certificate, while register gaps and nonmonotone destination order remain valid. Tensor extents must not expand into per-register ownership entries | Solve assignment construction | Shared source DAG walks must not repeatedly scan whole operation prefixes or scalarize ownership proofs |
| An additive assignment certificate derives a finite nonzero constant target coefficient and a bounded weighted projection of target-independent source registers; shared subexpressions remain shared, the original evaluation prefix remains required, and wire replay matches the exact derived selection. Final backends materialize arithmetic from the issued projection, while evaluators never recover it from a runtime residual or old target value | Solve assignment construction | Independent offsets avoid cancellation; shared DAGs stay bounded |
| An affine tensor projection binds one offset/coefficient rule to each selected canonical source operation, preserving compact tensor ranges and source DAG sharing. Independent cuts reuse original register ranges; bilinear rules require an independent operand and division an independent denominator. Execution adapters materialize only the issued relation after the unchanged source prefix, never substitute a target into a call or assertion, and decline zero/nonfinite coefficients. Wire replay rederives source producer identities, rules, output identity, and independent cuts | Solve assignment construction and final execution adapters | Tensor isolation preserves evaluation effects, bounded structure, and singular implicit equations |
| A pure-call value-projection certificate derives exact output-to-input coordinate maps only from a total checked body of copies, finite constants, static projections, fills, transposes, and nested certified calls with no assertion predicates. Isolation consumes the issued certificate and retains the call's evaluation prefix; arithmetic, runtime indexing, control flow, and assertions remain unproved. Wire replay rederives and matches the certificate, and a dependency relation alone never proves value equality | Solve pure-call and assignment construction | Exact forwarding can expose an isolator without erasing calls or changing nonlinear branch selection |
| A prepared scalar row may certify that its complete solver-Y gradient is invariant under solver-Y and time at a fixed parameter snapshot | Solve scalar construction | Runtime may reuse the exact gradient only while the parameter snapshot is bitwise identical; nonlinear or time-varying gradients remain uncached |
| A retained sparse projection workspace binds one immutable matrix pattern, its symbolic factorization, and numeric storage. Numeric factors become usable only after successful checked factorization of the current bitwise-exact conditioned coefficients; mutation revokes that state before refactorization. Pattern changes reconstruct the owner, failed factors never solve a right-hand side, and scratch reuse preserves kernel, parallelism, and fallback policies | Component numerical projection | Storage reuse cannot authorize stale or partially factored equations |
| An affine elimination layout constructs only from complete, unique block-local row and coordinate permutations whose selected causal diagonals exist. Dependencies on later causal coordinates issue guards, each paired with the causal position holding it and the later causal position solving its column; every solve checks their fresh unconditioned coefficients before factor reuse. A nonzero guard, unless its holding step is already a tear, promotes the step solving its column to a tear before elimination; a weak or zero pivot promotes its own step in place during elimination (a new recovery column, a unit recovery row for its coordinate, and its row appended to the reduced system), which is exact because no earlier step depends on a promoted coordinate except through an exactly zero guard. Promotion proceeds up to `projection_policy::torn_promotion_capacity` of the issued tear count, and then declines reduction; a nonfinite coefficient or a singular reduced system also declines. Its immutable sparse dependencies drive reduced factorization and full-coordinate recovery. Promotions are re-derived on every factorization and never carried between calls, and cached reduced factors are usable only for the exact current conditioned coefficients, guard outcomes, and layout. The linked kernel and every generated C component share the admission rule `projection_policy::affine_elimination_capacity`, the guard table, and the capacity. A promoted solve passes the same scaled residual certificate as every affine path (`ALGEBRAIC_REFRESH_TOLERANCE`). | Solve structural artifacts and numerical projection | Conditional triangularity and numerical nonsingularity require fresh evidence |
| Manifold projection binds each checked sparsity color to exact logical outputs of its canonical directional programs before execution. A selected tensor program executes once per color and scatters only its issued output placements; unrelated programs are not required. Admission proves unique, complete, repeatable output ownership. Residual certification, numerical failure propagation, and atomic state correction remain unchanged | Solve structural artifacts and final projection adapters | State constraints must consume the same tensor output and coloring proofs as algebraic blocks, without runtime regrouping or repeated unrelated Jacobian work |
| A continuous refresh owner may certify a projection block affine only by deriving every selected residual's degree in that block's exact unknown inventory. Outside-block coordinates are coefficients; compact typed-call owners issue and replay input-interaction summaries, and unsupported operations remain unproved. This certificate permits unrestricted Newton corrections for that block but proves neither nonsingularity, parameter-static Jacobians, complete causal seed coverage, nor nonlinear branch equivalence | Solve call and refresh construction | State-dependent coefficients must not obscure linear blocks or authorize unrelated solver shortcuts |
| A causal refresh row constructs `ParameterStatic` only by recursively accepting pure scalar/tensor/fold/conditional owners, exact P ranges proved immutable during continuous-time mode (including the dedicated homotopy endpoint after its initialization owner pins lambda to one), and Y ranges owned by itself or earlier `ParameterStatic` rows; time, state/dynamic Y, every other runtime P coordinate, seeds, mutable tables, and impure state reject construction | Solve refresh-plan construction | Runtime can reuse tensor-native invariant values under an identical bitwise parameter snapshot without operation rescans, coordinate expansion, stale dynamic inputs, or continuous root searches over initialization-only continuation state |
| A function-fold program derives its finite domain, carried tuple types, initial definitions, update definitions, result projections, and exact provenance in one construction; no scalar iteration list is accepted or stored | Solve program construction | A backend receives the original bounded loop and cannot depend on post-expansion graph recovery |
| The staged executor accepts a checked value-stage schedule only when every numerical block coordinate has a compiler-issued causal seed; the schedule begins with the complete seed sweep in certified order, then reapplies each block's local seeds immediately before that BLT stage; otherwise execution uses the complete preserved projection, and a runtime-unavailable isolator also restores the incoming coordinate before that fallback | Solve evaluation construction and runtime artifact check | Complete seed coverage makes block-local branch selection construction-owned; missing, reordered, or deleted warm-start witnesses can select a different nonlinear solution, while treating a runtime-singular isolator as a semantic failure would discard the preserved implicit equation |
| Every continuous refresh execution owner is issued from canonical row/logical-output projections with one purpose, ordered compact target ranges, typed call sites, BLT stages, and a complete coordinate dependency certificate; exact assignment programs and schedules are frozen as part of construction | Solve construction and wire replay | Runtime preparation cannot clone/filter residual programs, rediscover isolators, or invent schedule identity |
| Construction issues coverage and ordered-remainder relations only between compatible refresh owners at an identical complete coordinate; the relation names exact stage identities and preserves the uncovered owner's checked BLT order | Solve construction and wire replay | Derivative-settled root/value work may be omitted without pointer identity, row reconstruction, body equality, hashing, or tracing |
| A typed pure-call directional owner is admitted only when the closed typed-operation vocabulary has a checked tangent relation for every reachable operation: Real payloads carry exact primal/tangent pairs, discrete/control values contribute no variable tangent, conditions remain primal, lazy regions differentiate only the selected path, compact domains remain loops, and nested calls refer to earlier issued owners | Solve pure-call construction and wire replay | Continuous AD cannot fall back to a cloned scalar function/fold graph or expand tensor coordinates |
| Wire decode replays the same owner operations | Solve serialization | Bytes cannot forge definitions |
| Old schemas, raw insertion, validators, defaults, and adapters are absent | Solve boundary | No weaker path survives |
| Tests use production construction and cannot bypass provenance or activation | Solve tests | Evidence exercises the boundary |
| Structured B.1c owners derive compact compute and target maps plus row/pre/observation/clock policy | Solve construction | Per-scalar metadata cannot become a parallel owner |
| Structured B.1c wire decode replays the same checked owner operation | DAE/Solve serialization | Bytes cannot forge a family or its target coverage |
| Canonical typed variable-storage runs derive the complete compact event-iteration plan; every non-input `z`/`m` run has exactly one scalar, structured, or hold owner, external inputs are explicitly excluded, value kind and clock participation are derived rather than accepted, and wire decode replays the same owner operations | Solve construction and serialization | Deleting or relabeling parallel fields cannot shrink or forge the Appendix-B convergence domain |
| A complete event pass reads ordinary unclocked discrete history, continuous event-entry history, and clocked `previous()` from distinct owned lanes; only ordinary unclocked `z`/`m` lanes advance atomically between passes, and successful return requires exact typed current/history equality for every fixed-point run | Solve runtime | Mixed temporal expressions cannot collapse to one row-wide snapshot policy, and runtime success witnesses convergence |

### 6a. Continuous Refresh Construction

Continuous refresh optimizations require constructor-issued facts bound to the
canonical residual and unknown inventories; wire replay rederives those facts,
and consumers may use only the property proved by the certificate.

Logical output bindings borrow the checked scalar-program block that proves
their complete cardinality and ordering. Consumers iterate this issued pairing
of source program, local output, and logical index without asserting a second
independently advanced iterator's length. Wire replay checks the same pairing.

A finalized scalar-program block may share its immutable backing allocation.
Cloning shares the complete programs, provenance, output identities, and
execution certificates together; wire replay constructs a fresh checked owner.
Allocation identity may bind a derived execution view to this live source,
but never replaces semantic identity in canonical data or serialization.

A scalar view of one canonical scalar-program node retains that exact owner
and its output identities. Canonical expressions remain authoritative: cached
projection metadata cannot substitute an older source. Replacing that node
without rederiving its metadata is a typed error. An optional prepared
application requires the current scalar view to retain the issued source owner;
materialized views without that relation decline preparation before execution.

A complete projection-Jacobian application derives ordered colors, solver-Y
seed indices, and column-major output placements from the checked block and
pattern. It retains the exact scalar-program owner used during derivation;
native preparation must consume that same owner. Runtime may select the
complete forward application only when every selected primal program lacks
reverse-gradient support, preserving mixed forward/reverse policy. An exclusive
workspace may retain seed, program-output, and matrix storage. Each invocation
uses fresh coordinates and the same ordered complete programs, checks native
status, and publishes the matrix only after success. Failed calls cannot leave
active seeds for the next call. Projection reuse retains no value across
invocations; independently certified closed-input pure-call reuse retains its
own contract. Initialization and parameter-sensitivity seed domains are unchanged.

A tangent-lane program constructs only from a checked forward JVP program and
a lane count below `MAX_TENSOR_LANES`: operations that read no seed-dependent
register run once, seed-dependent scalar operations run once per lane, and
interleaved dual aggregates (including the two single-lane idioms that pack
and split them) widen to one primal and one lane per tangent. Seeds are
element-major and outputs lane-major; each lane equals the one-direction
program operation for operation, and any other seed-dependent vocabulary
declines. A torn block's tangent plan constructs from its checked
`BlockTearing` and the solver-Y JVP rows: each causal step names its row, its
target, its coefficient source (a lane seeded on the target alone), and the
tear columns its reads reach; each reduced residual row names its tangent
source; a row without a widened program, or a causal row without a
coefficient in its target, declines the plan.
Evaluation follows the sweep order and declines at a vanished coefficient. A
block's colored tangent plan constructs from its issued colored Jacobian
application: each distinct program runs once with one lane per color that
calls it, and each placement takes the value that color's call would write.
Plans are derived views, rebuilt by each consumer from the same construction,
never canonical IR.

A torn sweep's runs construct from its checked causal steps and the certified
isolators of their rows (`torn_sweep_runs`): consecutive causal steps of one
residual program form one run when one chain program answers them in order
(`target_isolation_chain_program`): prefixes do not decrease, and neither a
step's isolated value nor an operation of its prefix that can fail on its
operand values (a singular dense solve, a raising pure call, a table query, a
random-generator op, or a fold or conditional holding one) depends on the
target of an earlier step of the run. The prefix may read that target
elsewhere (each residual output reads its own target) because those registers
neither reach the value nor decide an error. A run evaluates its row prefix
once and writes exactly the per-step isolators' values; the linked kernel and
the generated C consume the same runs, and an evaluation failure inside a run
falls back to its steps one by one.

A projection block's residual split constructs from the block's checked row
programs and its unknown set (`y_indices`), once per (block, program). The
block-unknown cone of a program is the set of register versions written by an
operation that reads a block unknown (a `LoadY` or a Y tensor range naming
one, directly or inside a fold or conditional body) or reads a register
version already in the cone; a `Select`, a conditional, or a guarded fold
whose condition or activation is in the cone is in it, and a pure call, fold,
or conditional runs whole, so it is in the cone when any input is, whatever
its output summaries say. An operation is invariant when it is outside the
cone, every register version it reads was written by an invariant operation,
every register it writes is written once in the program, and it has no
non-repeatable effect (a random or impure operation); output stores are never
invariant. The invariant part is the invariant operations in program order,
and its live-out registers are those a remaining operation reads. The
dependent part is the remaining operations in program order, entered with the
live-out registers preset. A block projection call changes only the block's
unknowns at fixed parameters, time, and external tables, so every invariant
operation computes the same bits at every point of the call: the invariant
part evaluated once per call, followed at each pass by the dependent part
over its live-out values, is bit-identical to evaluating the whole program at
that pass. The split serves the residual passes of affine blocks of more
than one row; only values invariant under every unknown of the block hoist,
so any pass that moves the unknowns reads the same live-out values. A one-row
block settles by its singleton assignment first, and a block the affine
certificate does not cover evaluates its own torn sweep and Newton programs,
so neither splits; an evaluator that runs a program lazily (skipping
operations its selected branches do not reach) keeps that program unsplit,
because the invariant part evaluates every invariant operation. A compiled
backend runs the invariant part as a program storing the live-out registers
as its outputs and the dependent part as a program loading them from its
seed vector, both standalone programs of the same operations. Jacobian and
tangent-lane programs are outside the split: their register identities
differ from the row programs', and their seed-invariant operations already
run once per call. The certificate records the invariant positions and
live-out registers of each (block, program); its checker re-derives both
from the program and the unknown set and rejects any difference, a
dependent write to a live-out register, and a read neither part defines.
Failures keep their order: the invariant part runs before the call's first
program evaluation and reports nothing; when it fails, the call discards its
values and evaluates the unsplit programs, and a dependent part that fails
or declines at a pass hands that pass to its unsplit program, so the same
operation raises the same error at the same pass as without the split.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Within one structural-artifact construction, applications of one immutable source share one operation-invariance proof; source replacement or specialization derives fresh evidence for its exact owner. The existing register-source checker must prove every read uses a seed-invariant register version; seed loads and any operation with non-repeatable effects cannot establish reuse. Every write replaces the destination version's evidence. | Projection-Jacobian construction | Preserves failure behavior without multiplying source-wide proof work by the block count |
| Native preparation may retain certified result ranges after their first complete, ordered execution and reuse those exact values in subsequent colors. Mixed primal/tangent tensor operations remain complete operations; no scalar graph is reconstructed. | Prepared projection application | Preserves tensor ownership and arithmetic order |
| Reuse is confined to one non-reentrant call at identical Y, P, time, external tables, and execution mode. The first failure aborts publication; later calls execute their first occurrence afresh. | Native projection workspace | Preserves validation, coordinate freshness, and failure semantics |
| A block residual split hoists an operation only outside the block-unknown cone, when each register it writes is written once and it has no non-repeatable effect; the dependent part enters with the hoisted live-out registers preset. The invariant part runs once per block projection call, before the first program evaluation and without reporting; any failure discards it and the call evaluates the unsplit programs. | Block residual construction and checker (`rumoca-ir-solve`), evaluation (`rumoca-eval-solve`), linked kernel, compiled residual path, and generated C | Identical operands give identical bits within a call that changes only the block's unknowns; errors surface where the unsplit evaluation raises them |

A separate block-Jacobian kernel may restrict numerical AD to the unknown
coordinates issued by that projection block. Its construction retains the
canonical primal and unrestricted directional owners, selected logical outputs,
and exact seed domain. Coordinates outside that domain have zero tangents;
their primal evaluations and validation effects remain ordered. Tensor loads
split only at seed-domain boundaries and remain compact ranges. Native execution
may consume this kernel only for its owning block and issued colors; canonical
source replacement invalidates it. Initialization, general directional calls,
and parameter sensitivity retain their unrestricted seed domains. Wire replay
reconstructs the specialization through the same numerical AD producer.

AD register packing may join exact contiguous or repeated register-version
runs into tensor concatenations. Packing preserves the ordered input tuple;
its compact structure follows run boundaries rather than tensor scalar extent.

An algebraic block's residual selection may group logical outputs of the same
canonical program when construction proves unique, complete, repeatable source
ownership. One residual evaluation executes each issued program once at the
unchanged coordinate and scatters its complete selected outputs in block order.
Distinct source programs remain distinct. Grouping preserves every executed
operation, assertion, numerical failure, and atomic projection update; it
neither caches values between coordinates nor discovers groups during execution.

Within one scalar program and its exact lowering context, construction may
reuse a pure unary arithmetic result for the same operator and immutable
operand register. Numerical AD applies the same rule to its emitted primal and
tangent arithmetic. The first evaluation remains in order; operand evaluation,
calls, assertions, lazy regions, and compact tensor owners remain unchanged.
This local register relation neither identifies distinct function occurrences
nor authorizes reuse between invocations, coordinates, or activation contexts.
Final native constant tracking follows the same version boundary. A scalar or
tensor destination write removes earlier facts over that complete range;
in-place arithmetic derives any new fact from the input versions before the
write. Runtime indices cannot be specialized using a prior register value.

A checked pure-call owner may issue a closed-input coordinate: its entire
readable environment consists of its typed inputs, constants, initialized
method locals, and earlier closed-input call owners. Structured regions retain
the same restriction. At a fixed owner and arithmetic profile, bitwise equality
of the complete input tuple proves identical ordered results and assertion
predicates. The mechanically derived directional owner issues a separate
coordinate including every tangent input. Native execution may retain one
successful result per issued coordinate, comparing all input cells without
hashing or approximate equality. Misses invalidate the entry before evaluation;
failures never publish it, and consumers still check returned assertion
predicates on every invocation. Cache storage belongs to that compiled owner,
cannot be accessed concurrently, and outlives every native reference to it.
This relation is a projection of the complete evaluation coordinate, not an
identity inferred from matching function bodies or observed model behavior.

An additive isolator derives a finite nonzero constant target coefficient and
a compact weighted selection of target-independent source registers. Shared
source subexpressions stay shared. Evaluation retains the original source
prefix before computing the isolated value from that selection; a correction
formed by subtracting the residual from the old target value is not an exact
causal assignment. Wire replay rederives the coefficient and register selection.

An affine tensor isolator may instead issue a source-bound projection of the
residual's compact operation DAG. Each selected source operation has one checked
offset/coefficient rule; independent subexpressions refer to their already
evaluated source ranges. Products require a target-independent operand and
division requires a target-independent denominator. Tensor extents and shared
subexpressions remain compact. Only final execution adapters materialize the
paired arithmetic, after the unchanged original prefix; calls and assertions
are never re-executed with a substituted target. Runtime zero or nonfinite
coefficients retain the preserved implicit projection. Wire replay rederives
the complete source selection and rules.

### 6b. Relation and Initialization Construction

Solve lowering gives each unclocked scalar model relation with a checked root
one dedicated current-truth buffer keyed by its exact source expression.
DAE condition lowering consumes the expression's constructed variability:
parameter-only comparisons become literal discrete conditions, without
continuous crossing roots or relation buffers (MLS §3.8.3/§8.5). This preserves
parameter bindings when they are reevaluated during initialization.
Expression consumers read that buffer; root evaluation retains the original
relation operands. The buffer is distinct from condition-edge history and from
any whole B.1c Boolean result. Clocked, structured, suppressed, and call-scoped
relations retain their own checked activation and storage contracts. A
`noEvent` operand has a literal-evaluation context, including its descendants
and expression caches, even when it shares an expression with a monitored root.
The corresponding construction row is in [SPEC_0043 §6](SPEC_0043_CONSTRUCTION_CATALOG.md).

Each initialization homotopy step settles the numeric initialization equations,
covered algebraic equations, relation truth, and ordinary Boolean definitions
together before advancing lambda. Ordinary `pre` values remain fixed during
that solve. A rejected step restores both numeric and discrete coordinates to
the last accepted step; exhausting discrete iteration is a typed failure.

A scalar discrete coordinate determined by an initial algorithm assignment or
by an explicit initial equation `m = value` / `pre(m) = value` has one typed
initial-value owner. Recognition only selects this owner; construction derives
the target role and scalar type, proves that the value reads only
initialization-settled coordinates, and rejects a second owner for the same
target. The claimed Flat row therefore cannot also enter the numeric
initialization residual system.

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

### 8. FMI Component Construction Catalog

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| `FmiComponent::construct` consumes one checked `SolveModel` into a private `Arc` and matching phase-solve FMI inputs satisfying SPEC_0044 §8 | `rumoca-ir-solve::fmi` | Metadata cannot detach from its executable kernel or duplicate FMI semantic ownership |
| Source identity, tensor extent, scalar names, storage role/type/run, attributes, state count, and value-reference bounds are checked before construction returns | `FmiComponent::construct` | FMI adapters receive no malformed parallel metadata |
| The checked component is non-cloneable. Runtime borrows its checked views; codegen may consume it only into a nonconstructible checked codegen view that retains the FMI metadata and clones the read-only `Arc<SolveModel>` solely for `'static` lazy-render objects. Rendering obtains `SolveArtifacts` only from that same `SolveModel`, never from a second argument. No API returns an owned bare Solve root or permits foreign metadata pairing | FMI component boundary | One shared kernel supports bounded lazy rendering without an ownership escape or a second aggregate |
| FMI 2 scalar variables and FMI 3 tensor variables derive from the same checked storage runs | Version adapters | Version projection cannot repeat equation lowering |
| The consuming `FmiCCodegenView` contains either the event-free proof or the parameter-assertion proof of SPEC_0044 ME-EVENT-003, plus the ordered parameter-initialization proof of ME-PARAM-001. It admits only the implemented Real public ABI. Whole-inventory encoding requires those proofs; pure-call owners and initial values come from the same retained model. The unrestricted correlated view remains non-serializable | FMI C profile construction | Assertion support cannot bypass admission, pair foreign bodies or initial values with metadata, or discard validation |
| Unsupported source types fail in `rumoca-phase-solve::fmi` before target rendering | FMI lowering | No plausible default representation |

### 9. Solve Algorithm Block Construction Catalog (pending: 2026-08-08 plan, M3-4)

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| `SolveAlgorithmBlock::construct` lends branded scopes over private declaration, method, program, action, call, and correlation arenas | `rumoca-ir-solve` | One controller execution authority |
| Construction consumes one checked Algorithm Code package identity and one explicit arithmetic profile | `rumoca-phase-solve` | Semantics and target arithmetic cannot drift |
| Real types carry their checked representation; Integer carries its domain; Boolean remains Boolean. Rounding is not part of a value type and is not carried on the profile either, since one admitted mode discriminates nothing (SPEC_0047 4.20) | Shared typed program construction | No numeric erasure |
| Slots derive exact scalar/aggregate type, storage class, mutability, shape, and source identity | Algorithm-block storage construction | Parallel layout metadata cannot disagree |
| Method locals construct inside lexical scopes and cannot become persistent/interface slots | Method construction | Scratch lifetime stays bounded |
| Startup, Recalibrate, and DoStep are each issued exactly once before root completion | Lifecycle construction | Partial lifecycle is unrepresentable |
| Register insertion proves dominance and exact operand/result types before committing an operation | Typed program construction | Invalid bytecode cannot escape |
| Aggregate copy, fill, constructor, projection, selection, and call-transfer operations prove shape, domain, and alias policy atomically | Aggregate program construction | Tensor relationships stay compact |
| Calls prove exact arity, result cardinality, evaluation order, bounded acyclic reachability, and a complete ABI plan | Call construction | Templates do not invent call mechanics |
| Branches and loops own their lexical scopes; loops require finite checked bounds and explicit step semantics | Action construction | Execution and storage remain bounded |
| Limits, signals, closures, escape sets, and failure atomicity are explicit action effects | Method construction | Failure behavior is never implicit |
| Every issued owner carries exact Algorithm Code correlation and provenance; the mapping is injective and complete at root close | Correlation construction | Production code remains auditable |
| Wire decode replays the same current-version construction operations; old schemas and unchecked child deserialization are absent | Solve serialization | Bytes cannot forge execution |
| Tests use production construction and compare the independent GALEC evaluator with Solve execution | Differential evidence | Shared lowering defects stay observable |

### 10. Function Derivative Ownership Proposal

This extension is being implemented for MLS §12.7.1 structural differentiation.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Resolve derivative targets and restricted inputs in the declaring function's lexical scope | Resolve | Annotation spelling is not identity |
| Retain source-priority derivative references and positional input roles through record-input decomposition | Flatten | Excluding a record excludes its complete decomposed input |
| Construct each derivative link from exact function identities and checked primal/tangent signatures; wire replay repeats construction | DAE | A malformed derivative ABI cannot enter structural analysis |
| A higher-order link requires its constructor-issued predecessor and appends tangents only for that predecessor's last derivative group; selection retains source priority and the exact chain context | DAE / Structural | MLS §12.7.1 permits higher-order annotations only on compiler-differentiated calls |
| A differentiated call retains its earlier source call and selected link. Construction and wire replay require the matching predecessor, identical original argument prefix, and checked tangent ABI. Rebuilding a call preserves this origin; an ordinary call of a derivative function acquires no differentiation history. | DAE / Structural | Higher-order selection must survive separate structural reduction rounds |
| Select the first applicable source derivative before differentiating the primal body; carry a selected link into reconstruction | Structural | Selection and emission consume the same source contract |
| `zeroDerivative` requires an invariant actual argument; `noDerivative` omits the tangent under the source function's documented domain assumptions | Structural | Omitted tangents do not assert constant values |
| Keep the original call for values and retained position constraints, and preserve compact tensor shapes in derivative arguments/results | Structural | Derivative metadata cannot replace primal semantics |
| Reject unsupported derivative orders or type profiles when required, with source provenance; do not silently discard a selected link | DAE / Structural | Unsupported metadata cannot produce a false proof |

## References

- [SPEC_0036](SPEC_0036_VALID_BY_CONSTRUCTION_IR.md) — owning construction
  rules and the obligations each catalog row serves.
- [SPEC_0007](SPEC_0007_IR_PIPELINE.md) — stage contracts.
- [SPEC_0022](SPEC_0022_MLS_COMPILER_COMPLIANCE.md) — MLS contract index.
