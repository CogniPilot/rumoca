# SPEC_0043: Valid-by-Construction Catalog

## Status
REFERENCE

## Summary

Lookup catalog of DAE milestone evidence, reservation owners, canonical arenas,
equation contracts, Solve/Flat aggregate rows, and enforcement tests referenced
by [SPEC_0036](SPEC_0036_VALID_BY_CONSTRUCTION_IR.md).

## How To Use This Catalog

This annex holds no rules of its own. Rows linked from DRAFT SPEC_0036 are
tracked review obligations now and become normative by reference when
SPEC_0036 is accepted. Its owning section states each requirement and links
here. Rows separately bound by ACCEPTED SPEC_0032 retain that parent's
current authority. Adding, moving, or removing a row is a spec change.

## Specification

### 1. DAE Milestone Acceptance and Review Triggers

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Adds fail with typed errors/provenance | ToDAE/wire decode | Earliest boundary |
| Consume each source-semantic owner once | ToDAE construction | Prevent omission |
| Canonical Flat-to-DAE construction issues the immutable DAE and its balance/ordered structural-selection evidence together from the same analyzed prepared Flat snapshot, only after all current construction checks succeed; the result has no public constructor or wire decoder, and compiler consumers do not reanalyze that input for its evidence. The standalone evidence query preserves its existing analyze-only domain. | `rumoca-phase-dae::construction`, sealed `DaeConstructionResult`, and compile-session consumption | Sharing an issued result cannot omit checks, pair evidence with a foreign root, widen a diagnostic mode, change a refusal or scalarize a tensor. |
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
| `dae-core-loc` | 11,000 | 21,250 | Retain checked conditional/tensor/loop-region, selected coordinate ownership and initialization parameter identity; retire downstream recovery during Solve Algorithm Block cutover |
| `dae-wire-loc` | 3,250 | 6,500 | Operation-shaped replay; consolidate correlation replay after construction coverage lands |
| `dae-total-loc` | 14,250 | 27,750 | Both items above; total follows their sum |

**Why:** the triggers were unenforced and all three were exceeded in silence.
The gate makes exceedance loud without blocking a landing: any measured value is
legal once its ledger row records it, and only crossing a 250-line step forces
the row to be rewritten. Measured, checked, and stated as an acceptance contract
in `crates/rumoca/tests/suite_gates/dae_loc_trigger_test.rs`.

**2026-10-13 selected-coordinate review:** production measures 21,193 core +
6,393 wire = 27,586 lines under §1a. Checked array selection, exact source
ownership, ordered coverage and replay retain one declaration with independent
element events. Shared target projection replaces rendered-name recovery;
variable operations now live in their ordinary model module. The §1b rounding
rule derives ceilings 21,250 / 6,500 / 27,750. Review triggers and totality-debt
ceilings are unchanged; remaining code carries construction and replay evidence
rather than parallel unchecked paths.

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

**2026-09-28 event-clock review:** measured production source moved to
18,079 core and 23,566 total lines with the MLS §16.5.2 shifted event clock
(its checked constructor, view, wire replay, and the event-clock branch of the
clocked value conversion proof, DAE-C23) and the Boolean `min`/`max` type rule
(ARR-040). The derived rule rounds each measured value up to the next
250-line step, so the `dae-core-loc` ceiling moves from 18,000 to 18,250 and
the `dae-total-loc` ceiling from 23,500 to 23,750; the review triggers, the
wire ceiling, and the totality-debt ceilings are unchanged.

**2026-10-02 warning-assertion review:** measured production source moved to
18,372 core, 5,511 wire, and 23,883 total lines with the warning-level
assertion event action and function assertion level (DAE-C27), the observed
discrete owners (SPEC_0022 EXPR-012), and the dgesv status (DAE-C26). The
derived rule rounds each measured value up to the next 250-line step, so the
`dae-core-loc` ceiling moves to 18,500, `dae-wire-loc` to 5,750, and
`dae-total-loc` to 24,000; the review triggers and totality-debt ceilings are
unchanged.

**2026-10-03 native-body review:** measured production source moved to
18,491 core, 5,511 wire, and 24,002 total lines with the proven native body
binding of an external interface (DAE-C30) and its view. The derived rule
rounds the total up to the next 250-line step, so the `dae-total-loc` ceiling
moves to 24,250; the review triggers, core and wire ceilings, and
totality-debt ceilings are unchanged.

**2026-10-03 terminal-print review:** measured production source moved to
18,521 core, 5,518 wire, and 24,039 total lines with the terminal print event
action (DAE-C31), its view, and its wire replay. The derived rule rounds the
core up to the next 250-line step, so the `dae-core-loc` ceiling moves to
18,750; the review triggers, wire and total ceilings, and totality-debt
ceilings are unchanged.

**2026-10-07 fold-continuation and nested-record review:** measured production
source moved to 18,718 core, 5,570 wire, and 24,288 total lines with the
continuation predicate of a compact function fold that ends a bounded
`while` loop (SPEC_0022 ALG-018), its checks and wire replay, and the
declared field and element types a nested record or record-array column is
assembled at (SPEC_0022 FUNC-045). No duplicate view or replay code was
found to retire. The derived rule rounds the total up to the next 250-line
step, so the `dae-total-loc` ceiling moves to 24,500; the review triggers,
core and wire ceilings, and totality-debt ceilings are unchanged.

**2026-10-07 enumeration-declaration review:** measured production source moved to 18,684 core, 5,572 wire, and 24,256 total lines with the enumeration declaration attribute of an enumeration variable (DAE-C32) and its view and wire replay. The derived rule rounds the total up to the next 250-line step, so the `dae-total-loc` ceiling moves to 24,500; the review triggers, core and wire ceilings, and totality-debt ceilings are unchanged.

**2026-10-11 compact quotient-boundary review:** production source at `ab034993b` measures 18,803 core, 5,759 wire, and 24,562 total lines (tests/generated excluded by §1a). The existing quotient batch and staged replay now retain an exact structured-domain boundary instead of forcing a binder-dependent relation into the scalar condition system; wire records omit and reconstruct its issued root at the same stream position. The §1b rounding rule yields ceilings 19,000 / 6,000 / 24,750. Thresholds and totality-debt ceilings stay unchanged; execution qualification remains pending.


**2026-10-11 workshop conditional-owner review:** production at `4aaaf7872` measures 19,686 core, 6,037 wire, and 25,723 total lines. The review covers checked model-arm construction/replay, quotient correlation, closed-owner proof transfer, and fixed operand views. The derived acknowledgment steps are 19,750 / 6,250 / 25,750; review triggers and totality-debt ceilings are unchanged. Final combined execution gates and maintainer review remain pending.

**2026-10-12 combined workshop review:** production at `d3f804491` measures
19,937 core, 6,037 wire and 25,974 total lines. The review
covers closed model-arm derivative proof transfer, constructor-issued
clock facts and the shared read-only selected-value traversal. Solve's duplicate
synchronous-condition owner analysis is removed. Derived acknowledgment steps
are 20,000 / 6,250 / 26,000; thresholds and totality-debt
ceilings remain unchanged. Final combined execution gates and maintainer review
remain pending.

**2026-10-12 compact-step review:** measured production source moved to
19,458 core, 5,882 wire, and 25,340 total lines with the binder-reading fold
continuation and run-time loop bounds (SOLVE-C39), multi-output calls in
`when` equations as one event call, pure statement conditionals issued as
if-expression assignments, initial equations that initialize foreign library
state (FLAT-C05), function-definition read checking split into its own owner,
and the `size`/`ndims` shape-read classification. The derived rule rounds each
measured value up to the next 250-line step, so the `dae-core-loc` ceiling
moves to 19,500, `dae-wire-loc` to 6,000, and `dae-total-loc` to 25,500; the
review triggers and totality-debt ceilings are unchanged.

**2026-10-13 scoped-region and branch-replay review:** measured production source
moved to 20,690 core, 6,347 wire, and 27,037 total lines with the checked
function-region and scoped-conditional views (function regions, region views,
assigned-read checking, scoped conditionals), the corresponding scoped-branch
wire replay, and the type-arena storage they read. The previous function
conditional owner was removed from the core. The derived rule rounds each
measured value up to the next 250-line step, so the `dae-core-loc` ceiling moves
to 20,750, `dae-wire-loc` to 6,500, and `dae-total-loc` to 27,250; the review
triggers and totality-debt ceilings are unchanged. The module review remains a
maintainer obligation.

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
| Pure-call owners may reserve a recursive group | `SolvePureCallTableBuilder` | Proven recursive SCC only (SOLVE-C62) |
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
| An initial relation between two discrete coordinates (SPEC_0040 DAE-C28) settles through its identity component: exactly one determined member gives every other member its initial value; none or two is refused at construction | Initialization system | No order is guessed or iterated |
| A discrete initial value reads only `time`, parameters, constants, inputs, states, and algebraics; never a derivative, `pre`, delay, or discrete coordinate. Solve applies it as an initialization update after the projection and proves at construction that its reads, expanded through matched algebraic definitions, derivative rows, and substituted bindings exactly as an initialization row is, reach no discrete or `pre` value other than a settled one; a read that does is rejected (SPEC_0022 EQN-040). A discrete coordinate is settled when its own initial definition reads, through the same expansion, no projection unknown, no algebraic, and no unsettled discrete; the set is the least fixed point of that rule. The settle loop applies every update before each projection pass, so the projection and every definition read a settled discrete, and its `pre` storage, as a known value | DAE initialization construction and Solve initialization lowering (`initial_projection::prove_initial_definition_reads`) | MLS §8.6 solves one simultaneous system; the projection settles discrete-free reads independently of the target, so one application after it is that solution and the settle loop needs no further pass |
| Every initialization projection unknown carries one Newton scale of its storage kind, issued at construction: a solver coordinate its solver variable scale, a `fixed = false` parameter its declared `nominal` (evaluated once, finite and positive) or, without one, the magnitude of its start guess when the projection begins (1 when that guess is zero or not finite); the Rust runtime and the C profile translate the same table | Initialization system | MLS §4.8.1 `nominal` exists for solver scaling and §8.6 makes `start` the guess; a fixed floor of 1 lets one step move a 1e-5 s time constant off its branch |
| A variable's `evaluable` attribute attaches only to a fixed, non-tunable parameter whose binding has at most parameter variability; when every declaration is complete, each evaluable binding reads only constants and other evaluable parameters, through pure calls. ToDAE sets it only for parameters without `Evaluate=false` whose bindings evaluate from constants and other such parameters and that are `final`, `Evaluate=true`, or dependent on such a parameter (MLS §4.5, §18.6); structural reconstruction and wire replay repeat the check | DAE variable construction and wire replay | STRUCT-T10(a) folds a value only under a checked license, never a tunable or runtime-dependent one |
| A model-level expression of constant variability that unconditionally evaluates a call of a pure Modelica function, so every argument is a compile-time constant (a call with no arguments included: literals, constants, and parameters already replaced by their STRUCT-T10(a) values; a read of any other parameter has parameter variability and a read of a variable more, so a call reading an ordinary tunable parameter or a variable never folds, MLS §4.4.4), is evaluated once at construction by the checked DAE evaluator that settles STRUCT-T10(a) values and replaced by its literal value. The expressions searched are the continuous owners and every parameter binding, which initialization evaluates unconditionally; folding a constant call inside a tunable parameter's binding keeps the binding's other reads, so the parameter stays settable. A record-valued call folds through the field or output its reader selects. The fold follows the STRUCT-T02 quotient and precedes matching. A call the evaluator cannot settle (a foreign body, an unsupported operation, a read of anything but constants) stays a call, and a call inside a conditional branch is not evaluated; a call whose evaluation fails (an assertion, a non-finite or out-of-range value) is ES015, naming the call, because it fails at every instant the model runs it. The same fold applies inside a pure function body to the closed call of an unconditional top-level assignment, one of constant variability that reads none of the function's inputs, locals or loop values, so the fit or table a Medium package constant denotes is evaluated once at construction instead of at every invocation; a function-body call the evaluator cannot settle or whose evaluation fails stays a call, because the body may never run it | Structural constant-value folding (`rumoca-phase-structural::dae_transform::constant_values`) | A constant call has one value for the whole run, and its literal exposes to the SPEC_0032 zero-coefficient proof the coefficients the call hid |
| A continuous algebraic Real declaration whose own residual owner (a scalar residual or a single-body row-major family) states `v - L = 0` for the whole declaration, with `L` of literal structure, is read as `L` by every other expression, and the rounds repeat until no such read remains. The owner and the declaration stay, so `v` keeps its declaration, causality, and observability and remains computed, and a declaration two owners bind is left unchanged. Only continuous algebraic declarations qualify (MLS §4.5): a parameter with a literal binding keeps its P slot and is read at run time, so a set of it takes effect, and a discrete variable, assigned in a `when` clause or read through `pre`, is never propagated; constants and evaluable parameters reach expressions only through STRUCT-T10(a) | Structural constant-value folding and the SPEC_0032 zero-coefficient proof (`rumoca_eval_dae::literal_bindings`) | The owner proves `v = L` at every instant, including initialization, so solutions correspond one to one with the source |
| A causal step of an issued tearing carries a construction proof that its isolated coefficient is bounded away from zero: a unit target (up to sign and a nonzero literal output scale), a nonzero literal coefficient, a nonzero constant sum of weights, or a tensor projection with no product factor. A coefficient computed from run-time values has no proof. A block whose structural tearing isolates without a proof is torn again with only proven exact isolators as causal candidates, and that tearing replaces the issued one when it needs no more tears than promoting the unproven steps would; a step still without a proof is promoted to a tear. Executors trust the recorded proof: the forward tangent of a row whose other terms do not read the target equals that nonzero constant, so the runtime, tangent-lane, and generated C torn tangents carry no check for a vanishing coefficient (a debug build asserts it, `causal_coefficient_is_finite`); a non-finite coefficient, which run-time values produce as a zero tangent times an infinite partial, declines the torn solve and counts as a projection fallback | Solve tearing construction (`rumoca-phase-solve::proven_tearing`, `rumoca_eval_solve::refresh_plan::causal_proofs`) | A causal step divides by its coefficient on every call; only a coefficient bounded away from zero at construction keeps that division, and the reduced Jacobian, finite at every visited point |
| Discrete Real equations own activated Real residual IDs | Discrete system | B.1b may be coupled; trigger/guard ownership is explicit |
| B.1c updates own typed `m` targets and values | Discrete system | Assignment shape is explicit |
| Runtime binding equations contribute event owners by expression occurrence | Event analysis | MLS §4.4 binding syntax and equation-section syntax have the same MLS §8.5 event surface |
| A scalar `div`/`mod`/`rem` with both operands of at most parameter variability retains its pure expression and compact binder scope; statically zero or non-finite divisors remain rejected, and other invalid divisors retain the ordinary evaluation fault | Canonical quotient admission | Time-invariant values need no integration event |
| A dynamic scalar `div`/`mod`/`rem` quotient has scalar operands and constructs with exactly one recorded runtime owner; model admission requires a divisor of at most parameter variability; a statically zero or non-finite divisor is rejected for either owner kind; a model owner atomically issues the quotient and its six-node canonical continuous boundary indicator, with either the exact scalar relation/generated `Always` activation/root or one compact structured root over the quotient's constructor-derived domain, while a function-body owner names the exact open function and is event-free per MLS §3.7.2 independently of operand variability; the owner registry is canonical in quotient-expression order on every recording path; structural transformation and checked wire decode re-issue the one staged checked owner operation from typed identities at the exact claimed source stream positions, omit only construction-derived artifacts, and fail closed on any missing, duplicate, foreign, out-of-order, or partially replayed owner | DAE runtime-quotient construction, structural replay, and checked wire replay | A dynamic discontinuity cannot lose or duplicate its event surface, forge function event-freedom, become statically admitted through reconstruction, or expose owner or stream order that depends on replay timing |
| Whole-record equality derives leaf owners only from exact record/field identities and equal complete layouts; each array field remains one tensor equation | Flat-to-DAE record-equation analysis | Aggregate routing cannot depend on rendered names or source scalarization |
| A row-major scalar family recovers its whole tensor only when every member is an exact full-rank integer projection of one identical base, in canonical coordinate order, and the base dimensions equal the claimed extents | DAE expression construction | Source scalarization cannot force backend scalarization, while reordered, partial, duplicate, or mixed-base families cannot forge aggregate identity |
| Straight-line function-loop scratch elimination substitutes only scalar call-locals whose definitions dominate their uses, whose expression dependencies remain unchanged through every substituted use, whose name is not assigned again in the substituted suffix, whose final values are not read after the loop, and whose substitution has at most one use. A multiply-used nonescaping local instead becomes an explicit iteration-local SSA definition in the compact fold transition: construction clears its enclosing value on entry, requires a dominating per-iteration definition before every read, records it as region-owned but not loop-carried, and restores the enclosing reaching definition on exit. Unresolved self-reads remain explicit loop-carried transitions. | Flat-to-DAE function-loop analysis and DAE function-fold construction | Compact loop owners cannot erase loop-carried state, replace a snapshot with a later live tensor read, bypass a sequential redefinition, erase escaping values, or turn shared per-iteration work into a cloned expression tree |
| Declared function-local scratch is iteration-local when canonical path-sensitive source/plan analysis proves no incoming read, no suffix escape and no enclosing backedge read. Each actual planned partial assignment or partial multi-output receiver update counts as a read of its base until a preceding whole definition removes that path from the same ordered incoming-value flow; partial writes are therefore permitted only after that whole-definition proof. A skipped guard skips both its write and reads; unconditional whole-write dominance is not additionally required. Generated selection Booleans retain their separate lexical dominance certificate | Flat-to-DAE function loop classification | Empty and inactive domains do not require an invented initial value for nonescaping scratch |
| A source function For with a prepared, validated zero-cardinality domain has no reached iteration, carried update or action. Phase construction omits that empty loop before issuing seeds or constructing its unreachable nested transitions; earlier source/name/type validation and all reads after the loop remain checked | Flat-to-DAE function loop construction | Empty loops cannot require an invented initial value solely to construct a body that never executes |
| An exhaustive conditional whose arms write the same original target with identical symbolic axes across the same binder region retains that common write even when the enclosing runtime path is not exact. Its certainty remains that enclosing path's certainty; only an exhaustive join on an exact parent path may promote the common write to an unconditional binder-region proof. A missing arm, different axes or incomplete domain cannot supply total output coverage | Flat-to-DAE compact fold definedness | Nested runtime conditionals cannot erase a common write or manufacture an unconditional one |
| An element write's dead seed is assigned once, where the sequence or compact loop that holds the write begins; every write inside it, including a write in a conditional branch, updates the value in scope, so a compact loop carries what its earlier iterations wrote. An already-analyzed seed for an iteration-local target belongs after that loop's local-state clearing and active-iteration selection, once per reached iteration; enclosing seed collection excludes it. Zero-cardinality loops establish no definedness, including a planned total array definition | Flat-to-DAE function construction | A seed can never overwrite a carried value, so a guarded element write cannot silently discard earlier iterations' writes |
| Scalar-store substitution retains a complete conditional RHS assignment until its selected-read owner has been constructed. `StoreUnobserved` and `LoopLocalStoreUnobserved` certify liveness, not transfer of arm-local assigned-state checks into an embedded expression. Straight-line, dominated-loop-local and loop-prefix substitution use one admission rule; without a checked selected-owner transfer they preserve the original ordered assignment for the canonical normalization below. This retention creates neither an initial value nor eager arm reads | Flat-to-DAE compact function scalar substitution (MLS §§3.6.5, 11.1) | Deleting an unobserved temporary cannot discard the source occurrence that owns selected mutable reads |
| A partial function assignment retains a separate receipt for its existing whole update base only when the canonical source readable-value checker proves that exact mutable target wholly readable on the selected pre-write path. Body and Fold consume that receipt after lowering the RHS and before constructing the tensor update. Absent whole-base proof does not authorize promotion of element coverage, manufacture a seed, or bypass the existing undefined-value checker. | Flat-to-DAE assignment planning and Body/Fold tensor-update construction; MLS §11.2.1.1 | A conditional whole assignment remains the source of a later selected partial update; partial element definitions cannot forge whole-value authority or reorder an inactive read ahead of the RHS |
| A generated Boolean snapshot whose RHS is a lazy conditional uses the same ordered conditional-RHS source normalizer and selected statement construction as an ordinary complete assignment. One private source/plan pair retains its original target declaration, capture/fact certificate and lexical occurrence; leaf markers assign that same target on exactly the chosen arm, including the original false fallback. Each leaf stores only the existing checker-issued eager mutable-read receipt for its reached source path; both Body and Fold consume it before lowering that leaf value. No eager receipt may include an inactive arm, and no fabricated source declaration identity reifies a generated target | Flat-to-DAE generated snapshot planning and statement construction (MLS §§3.6.5, 11.2.6) | Snapshot activation cannot discard selected assigned-state checks or evaluate a skipped predicate |
| A function assignment whose complete RHS is a conditional expression is normalized after original return reachability, conditional extent checking and checked loop compaction, but before snapshot and definedness planning, into the same ordered statement conditional, with the original target and subscripts assigned only in each selected arm. Each later condition belongs to the preceding condition's false continuation through the existing single-condition selected statement owner; its provenance is the actual later source condition. Conditions retain first-true order, and each reached arm retains RHS evaluation before target-index evaluation. Original extent validation and bound/compact-domain derivation precede the rewrite; this selected-read normalization cannot replace their conditional-expression input or supply new bound facts. Common element-type compatibility, numeric promotion and target conversion remain obligations of canonical checked construction, with the same mixed-type source refusals required before and after normalization; extent validation alone proves none of these. This transformation issues no definedness itself. Arm-local reads use the existing selected statement's definedness and ordered assigned-state owner, never an eager certificate hoisted from all arms. Embedded conditional expressions and comprehension bodies retain their existing expression-owner obligations | Flat-to-DAE function-source normalization | A proven lazy arm cannot lose its checked mutable read, and an inactive arm or target index cannot become an eager fault |
| A compact fold retains whole carried-entry reads proved on its exact selected source path by the same definedness checker and consumes their existing ordered assigned-state checks before reading the initial tuple. Iteration-local and empty-domain values issue no entry read; learning rounds grant no receipt, and a selected ordinary or nested loop consumes its receipt only inside its checked taken region. Ordinary and compact-fold assignments retain the same eager-read certificate from the canonical current-path definedness checker. A multiple-output call retains that same certificate on its argument tuple, independently of its receiver list, and consumes it before lowering arguments in either an ordinary body or a compact fold. All arguments read the old state before any ordered receiver write, including when every receiver is omitted. Only its whole mutable-value proof may issue the existing ordered assigned-state check before the reached RHS or target index; literal total coverage and guarded total coverage use that same checker. Lazy arms and comprehension bodies retain their own selected execution owner. An absent mutable aggregate with no preceding write is refused by source definedness, including a read after an empty loop | Flat-to-DAE function definedness and construction | A branch-owned aggregate cannot lose its proven selected-path read, an absent source value cannot become a default, and an inactive read cannot gain an eager action |
| Every runtime function conditional plan retains one eager-read receipt slot per actual source condition, issued by the same current-path checker after the condition's existing readability check. Ordinary bodies, bodies nested inside fold branches and fold conditions consume those exact source-ordered slots through the existing assigned-state issuer before lowering the condition. The shared eager-read visitor excludes lazy arms and comprehension bodies; neither target presence nor branch-result totality alone authorizes a read. Source/plan/receipt condition counts agree exactly, and static or unreachable plans retain their existing validation order | Flat-to-DAE conditional definedness planning and Body/Fold construction | Complete source definitions split across complementary normalized guards keep their canonical conditional state check at the reached condition, without seeds, unconditional promotion or prefetched lazy reads |
| A guarded branch join skips its source-owned coverage proof only when the unconditional target coverage is total. A partial unconditional element inventory and the complete or partial coverage of the same guarded branch coexist in the existing definedness owner. Reads prefer a valid selected-path proof, otherwise check actual unconditional element coverage before refusing the missing guarded definition; neither target presence nor a generated aggregate seed grants whole-value definedness | Flat-to-DAE guarded function definedness joins and reads | A complete selected child producer cannot be erased by an empty unconditional element inventory, and a known unconditional element remains readable without admitting an undefined whole aggregate |
| A compact child fold whose complete source body has one unchanged invariant selection resolves through the existing selected path and joins with its skipped path. An enclosing binder is fixed during that child execution and does not itself veto the selection; own or nested binders, body-written selections and unsupported statement shapes remain excluded by the canonical invariant-selection check. A generated selection is consumed as its certified capture, never re-evaluated from mutable source operands. The selected path grants coverage only under that guard, and joining preserves undefinedness when the guard or an enclosing domain skips the writes | Flat-to-DAE selected function-fold definedness construction | Distributing an already captured parent selection through a child body cannot erase complete guarded row coverage or invent entry truth |
| A successful selected-fold definedness resolution privately issues an entry capability for that exact source loop. Only certified generated selection captures can issue the entry capability. When a carried entry is conditional, construction consumes it with the same invariant-selection query and creates a checked selected branch before requiring conditional carried entries and starting the child fold. The skipped branch performs neither the entry read nor the child transition and joins through the existing conditional owner. Own or nested binder dependence and selections changed by child writes cannot issue this capability; no unconditional assigned state or seed is inferred from it | Flat-to-DAE selected child-fold construction | A carried entry defined only on the captured selected path is read on that path, preserving inactive and zero-iteration semantics and strict canonical fold initialization |
| Entering a nested function fold privately issues relative coverage for the exact selected parent entry region. Child path certainty concerns executions of that entry, not all executions of the enclosing fold. A completed binder-free child write defines its elements only when its outer-coordinate coverage contains the complete issued entry region; a partial parent predicate or a write still indexed by parent binders is lifted with the original parent certainty. The qualifier is construction-local, never serialized or accepted as an independent proof, and cannot establish a value after an inactive or possibly empty parent | Flat-to-DAE function-fold definedness construction | Complete per-iteration scratch may be read on its selected parent path without granting whole-aggregate coverage to partial parent writes or skipped iterations |
| A function fold retains a target whose canonical conditional join retains a partial assigned-state owner. Such a target is not reclassified as iteration-local solely because its current coverage is conditional; entry, update, selected-path reads and final publication continue through the existing checked owners without claiming unconditional definedness | Flat-to-DAE function-fold target selection | Closing a nested fold cannot discard the joined producer and restore an absent value before a reached enclosing read |
| A generated branch-selection Boolean carries a lexical scope certificate checked over the final normalized function body: its definition is the statement at the source conditional's position, it reads only generated Booleans already in scope, it is not defined twice in one scope or issued twice for one source occurrence, and every read lies after it in the sequence that holds it. A definition inside a loop body is therefore that innermost loop's iteration-local value: the loop's fold defines it on every iteration, no enclosing loop carries it, nested loops read it as an invariant capture, and branch-only definedness proofs guarded by it are forgotten at the end of each iteration that reseeds it. Captures are introduced only after compact loop normalization, so no store-deleting or substituting rewrite reasons about a definition whose reads it cannot see | Flat-to-DAE function normalization, loop analysis, and DAE function-fold construction | A per-iteration selection can neither leak its value or definedness into another iteration nor lose a dependency that only the capture reads |
| Incoming-read and whole-definition dominance analysis correlates normalized core statements with their actual checked statement plans in source order. A generated Boolean plan owns its read before its write even when the core occurrence is an empty marker. Body, suffix and enclosing back-edge sequences use the same incoming-value flow and definite-definition rules. Iteration-local decisions may be issued during an immutable source/plan traversal and consumed during a later mutable traversal only with exact structural position, source occurrence, lowering depth, target inventory and complete decision consumption checks; they are private, ephemeral and confer no value or seed | Flat-to-DAE function-fold local classification | A marker cannot hide a reached incoming read, while a whole definition before the same capture remains valid local scratch |
| Ordered incoming-value flow preserves the checked selection correlation of normalized exhaustive definitions using the existing `PathSet` of paths that still need the incoming value. A whole assignment removes only its reached undefined paths; conditional joins retain every skipped path, and compact child loops retain the zero-iteration path and invalidate body-written or shadowed facts through the canonical loop-entry transfer. Planned queries borrow the actual function fact scope and generated capture certificates; pre-plan queries use an untyped scope without capture authority through the same walker. Missing arms or changed mutable conditions never inherit an exhaustive immutable selection | Flat-to-DAE function-fold local classification and incoming-value flow | Splitting exhaustive tuple/whole assignments around a loop must not create an absent initial value obligation or grant a definition on an unwritten path |
| The canonical undefined-path set retains the alternative Boolean decisions of `if` expressions and conjunction/disjunction predicates when entering an actual branch or its fall-through, and at the actual statement that issues a certified generated Boolean capture. Capture transfer first separates the existing truth alternatives through that same path owner, then assigns the captured value through the unchanged fact transfer; it never re-evaluates that value after a source operand changes. Each alternative is issued from the same borrowed expression by the existing guard-fact transfer, in first-true order, and the existing bounded path union may collapse only to a conservative hull. A mutable write still invalidates the facts about its prior value; a captured Boolean remains its issued value, not a re-evaluated source predicate | Flat-to-DAE `guard_facts::PathSet` branch-entry construction | Complementary guarded whole definitions remove every active undefined path without inventing a definition on skipped paths or reusing a changed condition |
| A Boolean selection arm retains an exclusion only when the same borrowed source expression is exactly the opposite conjunction of Boolean reference values, recognized by the existing canonical Boolean shape owners. A bounded path hull and arm join retain only exclusions common to every corresponding issued arm. Existing Boolean narrowing consumes a clause only when its other terms hold; a changed operand invalidates the whole clause, including across loop entry and capture reseeding. Unsupported expressions, calls and partial element references issue no exclusion, and no assignment manufactures a source value | Flat-to-DAE guard-fact selection construction, joins and mutation | A capture's false conjunction or true disjunction preserves its exact impossible operand combination through the sixteen-path conservative hull without increasing that bound or treating a mutable operand as its earlier value |
| A declared function local written only on selected paths of a compact loop, whose resolved value is not defined, guarded-total, or seeded at loop exit, remains an iteration-local assignment target. Removing it from the carried targets must issue that same source local as an iteration local; it does not grant an entry value or a definition on any path. Original read-definedness checks run before this ownership classification and its enclosing definition is restored on leaving the loop | Flat-to-DAE function-loop definedness analysis | A valid branch-only scratch assignment retains a target owner without inventing a value on the path that skips it |
| A conditional join inside a function fold requires a partial target's assigned state only when that same construction-issued fold carries the target as an update. A branch-only iteration local is not an exit update and its unused partial state creates no read obligation; its actual source reads retain the existing definedness and exact-region checks. Lowering obtains carried membership from the issued fold owner with the branded target, not a reconstructed name inventory | DAE function-fold construction and Flat-to-DAE conditional lowering | Skipping a branch that writes an unused scratch value is not a read of that undefined value |
| A dead seed issued for function-loop scratch that one guard both writes and reads gives the compact transition a carried slot only; it is no MLS §12.4.4 definition. Once the region that issued it ends, a value no statement there writes outside that guard is defined only under the guard, so a later read under an implied guard is admitted and any other read is refused as a read of a value only some branches define. A guard that varies across the enclosing loop proves nothing past it. A guarded scratch seed is issued only under a guard that reads no value the function writes; a guard over a written value is left to the value facts | Flat-to-DAE function-loop definedness analysis | A value written only on a selected path can never be read through its seed on the path that did not select it |
| Function-loop definedness is proven from one generic iteration, never per domain point: each binder stands for its range, narrowed on a path only by guards that select exactly a binder range (a settled bound, or a dependent membership `L(p) <= b and b <= L(p) + w` with `b` re-based on its offset from `L`; a captured branch selection counts as its definition when that reads only binders and immutable values); a learning round finds what every iteration certainly defines by its end and a judging round checks every read against it. An element read is admitted when its image is defined before the loop, was written earlier in the same iteration on every path (including, element by element, writes an enclosing loop's current iteration made before this loop began, which hold at every binder value of this loop), or was written by an earlier iteration (an exact binder shift towards the start of the iteration order), each through the shared `rumoca-core` index sets; a loop defines the image of each write it certainly makes. A subscript whose image has no exact box description defines nothing and is read only when a superset of it is defined. An array that some paths of a binder-range selection leave undefined and every other path writes element by element joins with no element proven by the join; the loop still defines the image of each write certain on its branch | Flat-to-DAE function-loop definedness analysis | The proof's cost follows the loop body and the number of index boxes, never a domain's point count or an array's extent |
| A function value that a conditional defines on only some paths is tracked with the value facts every undefined path proves (the `guard_facts` facts of the row below: Integer intervals, Boolean and literal selections, and Real intervals proven NaN-free; a write forgets every fact that mentions its target, the body of a loop being resolved sees only facts no iteration changes, and after a loop its head invariant holds). Entering a branch conjoins its condition with those facts on each path; a branch whose facts are contradictory on every undefined path has the value defined, with the coverage every defining path proves, and a join keeps the facts of each undefined path apart (up to sixteen paths, merged where they hold the same Boolean values, then their hull), as are the facts of the current point, and every later conditional advances them by its branches' own statements, the fall-through by its negated conditions; a loop whose body branch the path excludes under conditions no pass changes writes nothing on that path, and a captured generated Boolean holds its definition from its capture statement; a Boolean assigned a conjunction (or a one-branch `if a then b else false`) also proves its operands wherever it holds (a longer `if ... elseif ... else false` chain is a disjunction of conjunctions and proves only what every branch proves, with the earlier conditions false), and where it fails proves the remaining operand false (true for `or`) when the other operand is known on the path to hold the non-deciding value, until a write changes a value an operand names. Lowering joins the defining paths over a dead entry seed that no surviving path reads. A branch value of an if-expression is read under its condition, so a value proven under a guard that condition implies is defined there. A later single-branch conditional under an equivalent guard extends the guarded proof with what it writes. A loop binder shadows a value of its name for the loop body (MLS §11.2.2) and is always defined there. A declared value no statement defines before a loop is undefined at a read on the first pass (MLS §12.4.4 gives a local no start value): the read is admitted only where the value is defined by this iteration's earlier statements, or where an earlier pass certainly defined it and the path of the read excludes the first pass of that loop (`if i > 1 then ... x ... end if; x := i`); every other read is a typed refusal. A loop whose body is one conditional without an else over a condition the loop does not change is resolved as that conditional around the loop | Flat-to-DAE function definedness analysis | Facts are kept per path, widened only at a loop head; a read the facts do not prove defined stays refused |
| A compact function fold receives a continuation exactly when its body is one conditional without an else (optionally preceded by the generated capture of its condition) whose condition reads no binder of that loop and calls no function. Such a body changes nothing on a pass its condition rejects, and the condition then reads the same state on every later pass, so ending the fold there yields the same values and raises no error the remaining passes would not; the conditional is kept, so the continuation is only the exit. The DAE constructor checks the continuation is a scalar Boolean read before the body's first statement and reads no binder of its own loop | Flat-to-DAE function construction | A bounded `while` and any loop of the same shape stop at the pass MLS stops at, without a separate while owner or a source rewrite |
| A compact dependent domain or a `while` pass bound is bounded by Integer intervals whose endpoints are proven separately, so a half-bounded fact still bounds an envelope. The function value facts (`guard_facts`, the one fact owner) are one source of such intervals, alongside settled values and binder bounds. A fact's subject is a scalar or an array element named by literal subscripts. Facts come from: a relation between a subject and a bounded expression on the path of a branch, and its negation on later branches of the same `if` (a Real fact only where the relation's outcome excludes a NaN operand: one that holds other than `<>`, or a `<>` that fails); the interval of the value an assignment stores (if-expression arms read under their conditions, `integer`/`floor`, half-bounded `min`/`max`/`div`); the literal arms a local is selected among (a Boolean's truth values, a numeric `if c then 1.0 else 0.0`), each with the facts of its path, so a relation with a literal admits only matching arms; the index of a completed element access, bounded by the extent (MLS §10.5), except inside an `and`/`or` operand or an if-expression arm, which may be skipped (MLS §3.3); and a product bounded above, which bounds each Integer factor whose cofactor is proven at least 1. A fact is forgotten when any statement writes a value it mentions (including the path facts of the arms the write itself selects among, which describe the old value), a binder shadows facts about its name, and paths that rejoin keep only what every path proves. A loop head holds an invariant found by iterating the facts before the loop joined with the facts after its body, widening a moving bound to the next threshold (an Integer literal or array extent the body names) and dropping it after a fixed number of rounds; a tighter head from one more pass is kept only when it is itself proven invariant; a body that can `break` keeps only facts about values it never writes, and only a loop with no `break` exits with its condition proven false. Guard facts never select a branch or fold a value | Flat-to-DAE function-loop normalization | A runtime range or `while` proven bounded by its facts stays one compact loop over a finite envelope, without a scalar expansion or a fact read after the value it bounds has changed |
| A function loop with call-scoped actions and no carried values constructs one zero-carried compact fold over the checked domain | Flat-to-DAE function-loop analysis | Scratch substitution may expose assertion-only loops without erasing or scalarizing their per-point actions |
| A guard moved into a compact loop by branch-selection capture (SPEC_0022 ALG-019) leaves every false-arm value to the definedness certificate: in a function with certified early-return output seeds those exact seeded outputs are the false-arm values, and an ordinary unseeded output with no definition on the unselected path is refused by its conditional owner. The branch-snapshot normalizer retains the actual lexical selection for every emitted loop. Its private decisions bind in exact source preorder to the same immutable normalized loop bodies before planning; count or source-shape disagreement is refused. A loop selected by this owner resolves its carried-entry reads on that selection, including a body containing a later generated capture; a generated capture marker is never ignored to recover a selection. An unselected loop may still use the existing checked invariant-body selection. Loop compaction owns no separate predicate pushdown | Flat-to-DAE function-loop normalization | One owner moves guards into loops, so a selection is evaluated once at its source point and no output value is invented on the false path |
| The same private captured selection that owns a compact loop entry may retain read-only incoming function values alongside its carried-entry reads. Candidates are actual output/local declarations read by the existing ordered source/plan incoming-value flow and absent from the canonical whole-body write inventory. The existing whole-value readable checker must prove each candidate on that exact selected entry before body analysis; no element-only or selected expression-arm fact suffices. Construction consumes the same assigned-read receipts inside the selected region before beginning the fold, including when no carried target itself needs that region. Missing whole proof leaves the existing reached-read refusal intact. Unselected loops gain no capture receipt, and no inactive arm is evaluated to collect or consume a receipt. | Flat-to-DAE selected loop planning and Body/Fold construction | MLS §11.2 ordered execution and §12.4.4 reaching definitions permit an unchanged, already defined enclosing value to be captured; lazy index selection cannot lose its original definition or gain an invented entry value |
| An ordered unit-step first-match loop compacts only from a zero seed, an exact `found == 0` guard, and an update to the current binder; its tensor reduction appends an out-of-range sentinel before `min` and maps the sentinel back to zero | Flat-to-DAE function-loop normalization | The compact form is total for empty/no-match domains and preserves the source loop's first-match semantics without scalar expansion |
| A direct `x := x + term`, `x := x - term`, or `x := x * term` loop over a binder-dependent range compacts to the corresponding `sum` or `product` comprehension joined with its exact seed | Flat-to-DAE function-loop normalization | Additive and multiplicative identities preserve empty-domain behavior while dependent scalar iteration never enters DAE IR |
| Conditional target certificates inside a compact function domain resolve the selected nested branch before collection and union monotonically across all domain points | Flat-to-DAE function-loop definedness analysis | A later point or an outer guard cannot erase a tensor update owned by an earlier point or nested branch |
| A `GuardedFunctionFold` names a previously defined scalar activation materialized outside its guarded path and checked initial/capture ranges; inherited activation is consumed once by its owning fold, while nested folds own only guards introduced in their update. A guarded nested function-fold output additionally owns exact polarity, a nonempty in-bounds result range, and the corresponding parent-carried range. Reuse identity includes the activation path/function context and, for nested output, parent-carried versions. | Solve function-fold construction and checked wire replay | Lazy backend execution cannot recursively guard an activation definition, read an uninitialized result, cross branch ownership, duplicate an inherited guard at every nested fold, or reuse a result under a different activation path |
| A multi-target function conditional constructs its ordered branch correlation and all joined definitions in one atomic operation; target uniqueness, branch-vector completeness, expression ownership, current reaching definitions, and value-type compatibility are checked before any definition advances | DAE function-body construction | No partial join can escape, and a backend receives proof-bearing shared control flow instead of reverse-engineering it from independently projected conditional expressions |
| A model-event algorithm transaction constructs atomically with its exact source statement order, activation/clock topology, mixed discrete Real/discrete-value target tuple, current/`pre` reaching definitions, aggregate types, and issued pure-call occurrences; every statement path must define a compatible next environment and every final target must have unique complete coverage before the derived B.1b/B.1c views become visible | DAE model-event construction and checked wire replay | A partial transaction, stale reaching definition, split target class, forged call occurrence, or backend-reconstructed statement schedule cannot escape construction |
| A compact tensor transpose separately checks nonzero row/column extents, nonzero trailing element width, arithmetic lane count, total source/result range, and overflow; rank-two uses element width one and forward AD changes only lanes | Solve scalar-program construction and checked wire replay | Higher-rank MLS transpose cannot be scalarized, truncated, or confused with primal/tangent storage |
| Solve scalar lowering stores a literal-only scalar (literals, constants, `+`, `-`, `*`, `/`, and the `zeros`, `ones`, `fill`, `identity`, `outerProduct`, `skew`, and `transpose` generators) as one literal, reuses the operand of `x * 1`, `1 * x`, `x / 1`, `x - (+0)`, and `-(-x)`, and drops each literal store or unary op no later op reads, repeating until none remains, with liveness taken from the register-flow source checks. Every such fold is the IEEE value of the same operations in source order. `x + 0` is not folded (`-0 + 0` is `+0`), transcendental builtins are not evaluated at translation, and a product with a literal zero factor is dropped only where the structural incidence proof (SPEC_0032) drops the same term, because `0 * x` is NaN for a non-finite `x`. A constant factor or divisor of at most 16 lanes is read lane by lane only when every lane is an exact `0`, `1`, or `-1`, since each such lane folds or is dropped; any other constant factor stays a packed tensor op. A sum or difference with a small constant operand is read lane by lane, since each lane costs one scalar op either way and packing the other operand would store lanes a product term already dropped. The trade-off was measured on `Loops.Fourbar1` with the kernel lane's minimal isolators: unpacking every small constant operand gives 67,900 C lines at 642,036 instructions per derivative evaluation, keeping every one packed gives 63,999 lines at 651,869 (+1.5%), and this rule gives 67,026 lines at 642,266 (+0.04%), the smallest text within 0.5% of the fastest that stores no dropped lane | Solve scalar-program construction | Literal arithmetic costs no runtime work, no fold changes IEEE results beyond what the incidence proof already licenses, and unpacking never costs more text than it saves |
| A `FunctionConditionalProgram` constructs only from a nonempty ordered condition list, an equal number of branch result regions, one fallback region, exact capture and result tuple types/shapes, and branch programs whose outputs completely define that tuple; its enclosing op proves capture/result ranges and full call/activation reuse identity | Solve function-conditional construction and checked wire replay | Malformed or partially correlated branches are unrepresentable, and lazy native control flow cannot read an undefined capture or return a partial tuple |
| A `ScalarProgramBlock` function-conditional table issues one id per exact semantic call identity, stores each checked body once, and accepts references only with complete in-bounds compact capture/result ranges matching that owner's typed shapes and arithmetic profile | Solve scalar-program-block construction and checked wire replay | A scalar projection cannot clone a body, forge cross-row reuse, enumerate a tensor ABI, or invoke one owner with different captures/results |
| One multi-output continuous aggregate-call group constructs only from algebraic residuals whose typed `Field`/`Index` projection chains terminate at the same pure DAE call `ExprId`; construction derives every scalar width and explicit output ordinal, rejects duplicate/non-algebraic members, and retains per-output dependency and assignment-isolation proofs | DAE-to-Solve continuous scalar-program construction | A grouped program restores one exact source call occurrence without graph matching, scalar expansion, name inference, or accidental reuse of a distinct/impure call |
| A prepared lazy-row plan binds each operand and output store to its source-point register version through the canonical immutable register-range inventory. Destination writes issue distinct versions, including partially overwritten tensor ranges; native operations and operand views remain compact. Preparation derives reusable version storage once. Interpreter demand, cached traces, and native specialization consume that same version ownership; no capability uses a final numeric-register definition for an earlier read or store. Any `FunctionConditional` or `GuardedFunctionFold` preserves interpreter laziness but makes trace-derived native replacement unconstructible; only scalar-`Select` rows can issue that replacement | Solve evaluator preparation | Later whole-range execution cannot clobber an earlier demanded lane version; inactive address reads remain inactive, original faults retain provenance, and runtime observation cannot override aggregate branch ownership, manufacture hundreds of scalar guard outputs, or turn a compact owner into a pruned reconstructed graph |
| A clock-owned guarded assignment derives its reachable source-priority branch suffix from typed DAE conditions; the first `Always` or exact owning-`ClockId` branch supplies the initial value and makes hold plus every lower-priority branch absent from the constructed program | DAE-to-Solve discrete construction | An unconditional clock-tick update cannot carry an unreachable old-value load or scalar true-condition selection into Solve IR, while unclocked and different-clock activation remains intact |
| A guarded-assignment activation owner constructs from the exact optional clock, trigger `ConditionId`, guard `ConditionId`, trigger-memory slot, expected polarity, and materialized register before its branch value is lowered; context, fold, conditional, assertion, and cache identities retain that owner | DAE-to-Solve discrete scalar-program construction | Branch-local aggregate work cannot lose its activation, reuse across a different event path, or require post-expansion trace recovery |
| A `GuardedAssignmentProgram` constructs atomically from aggregate DAE updates with one exact clock/pre policy and identical ordered activation topology; construction derives branch reachability, aggregate widths, compact result ranges, and affine target ranges, proves complete nonoverlapping simultaneous coverage and every arm/fallback result shape, and retains selected-arm activation ownership without constructing scalar target/value coordinates | DAE-to-Solve discrete construction and checked wire replay | A correlated clock/event update cannot become per-coordinate `Select` graphs, execute an inactive tensor algorithm, forge a partial result tuple, overlap storage, or require runtime reconstruction of its state-transition relation |
| A checked scalar-program owner derives its immutable register/output execution certificate once, validates each issued function-conditional body once by owner identity, and exposes the certificate only together with the aggregate it proves; wire replay reconstructs the same certificate before construction succeeds | Solve scalar-program and guarded-assignment construction | Evaluators and final backends cannot repeatedly clone/replay nested control-flow validation, trust forged register capacities, or separate a proof from its exact compact owner |
| A model-level pure-call table issues one id for one exact DAE call occurrence/context, constructs its compact typed capture/result/predicate ABI and finite owner graph, acyclic outside SOLVE-C62 recursive groups, atomically, and accepts every residual/root/action/value reference only as an in-bounds projection of that owner; construction also derives the complete coordinate-invalidation inventory used by an invocation certificate | Solve model and pure-call construction plus checked wire replay | A call body or assertion path cannot be duplicated, forged, recursively cyclic, or reused after any time/state/history/table/arithmetic input changes, and final backends receive one tensor-native helper relation rather than expanded scalar graphs |
| An `EventTransactionProgram` constructs only from one DAE-issued model-event transaction, a canonical nonempty activation-clock set, one exact member clock per target, exact activation/history lanes, ordered typed statement regions, compact intermediate/final ranges, and in-bounds projections of issued pure-call owners; construction proves each transition's input definitions, complete final target coverage, nonoverlap, exact clock-set coverage, a target-result prefix followed only by checked Boolean assertion predicates, a nonempty exact action-index set per predicate, lazy held fallback for every inactive clock region, commit atomicity, and exact settled/invalidation inventory before exposing the owner. Each target also owns one aligned construction-issued producer projection; whole-model replay proves that it equals one complete canonical storage run on its member clock, that claims do not overlap, and that any scalar or guarded executable producer is covered as one complete program before suppression. The issued clock partition contains the transaction once and activates it on the union of its member clocks, so coincident clocks cannot invoke it twice. | DAE-to-Solve event construction and checked wire replay | Runtime and generated targets cannot split one controller tick, execute a clock owner twice, eagerly execute an inactive clock region, discard or independently recompute a call assertion, suppress only part of a producer, observe a partial commit, forge invocation reuse, or rediscover transaction identity from scalar programs, bodies, spans, or target matching |
| A whole-clock causal transition/partition plan (pending: SPEC_0046, pre-implementation) issues, for one clock, the complete ordered producer list with each producer's typed rank, its intermediate-definition identity, and its member kind; ranks, identities, and member kinds are issued values that consumers read directly and never recover by searching an order list, comparing positions, or matching targets, names, spans, or provenance. A discrete-value producer is admitted to exchange membership only under the same dependency/cycle proof that admits a discrete Real producer, and a directed cycle among exchange members fails plan construction. | Structural causal-plan construction | Positional rank recovery is quadratic and forgeable, and a discrete-value definition admitted without the shared proof lets a same-clock self- or mutually-reading definition acquire left-limit semantics the DAE never proved |
| A clock-partition producer (pending: SPEC_0046, pre-implementation) is issued exactly one member kind at construction. An EXCHANGE member requires exact per-tick activation totality: the owning clock alone is total, and `And(clock, non-clock predicate)` or any other narrower guard is total only under a construction proof over that tick. Every other guarded producer is issued as a HOLD-FALLBACK member that keeps the checked SOLVE-C07/C10/C47/C49 hold semantics, whose target a same-tick reader observes through the SOLVE-C22 event-entry history view. Construction rejects, at that producer's span, only a row claiming exchange membership without its totality proof, and it never re-issues a member kind to repair a later failure. | `ClockPartitionTransactionProgram` construction | Returning the owning clock for a narrower conjunction treats a producer that does not define its target on some tick as an always-fresh definition, while rejecting that conjunction would contradict the accepted hold rows |
| One proof and lowering route (pending: SPEC_0046, pre-implementation) spans unconditional and guarded clocked producers alike, and both member kinds travel it; a partition in which only guarded producers pass through the owner while unconditional producers retain an independent path is unconstructible | `ClockPartitionTransactionProgram` construction | Two routes are what left unconditional B.1b chains stale under atomic snapshot evaluation |
| Each admitted producer (pending: SPEC_0046, pre-implementation) lowers exactly once into one compact SSA/typed region. A consumer reference to an exchange member is an in-bounds load of its issued intermediate; a consumer reference to a hold-fallback target is that target's checked storage read. Construction rejects a second lowering of one producer, a load of an unissued or undominated intermediate, and any re-entry, reporting coverage, ordering, cycle, and re-entry failures as typed contract errors rather than selecting storage semantics at runtime. Intermediate visibility does not commit unrelated targets, and the complete final-target tuple commits atomically | `ClockPartitionTransactionProgram` construction and checked wire replay | Recursive consumer inlining and runtime memoization duplicate producer graphs and compact tensor work, and a recursion guard that silently reads old storage invents unproved semantics |
| A target owned by a DAE-C21/SOLVE-C55 model-event transaction is excluded from clock-partition admission (pending: SPEC_0046, pre-implementation); the transaction is that target's owner, C57 producers consume its committed final values, and construction rejects any partition member that claims, re-derives, or re-orders a transaction target | `ClockPartitionTransactionProgram` construction | DAE-C21 already requires downstream executable phases to consume the transaction owner; a second equation-shaped owner for the same target would split one controller tick |
| A SOLVE-C28 boundary ends a same-instant producer path inside the partition (pending: SPEC_0046, pre-implementation): an explicit `sample(u)` source keeps its event-entry left-limit lane and a state derivative terminates the chain, so neither becomes a same-tick intermediate and neither creates a cycle among exchange members | `ClockPartitionTransactionProgram` construction | Same-tick exchange must not rewrite the sampled left limit MLS §16.5.1 defines, nor turn a legal feed-forward `hold`/`sample` pipeline into proved feedback |
| A clocked row the causal plan leaves unowned has no clock-partition membership (pending: SPEC_0046, pre-implementation) and keeps its existing checked disposition: `CausalDiscreteError::NonComputable` at that row's span, surfaced as `LowerError::non_computable` by Solve and as `coupled-discrete-real-equation` by GALEC. A compact coupled-owner member kind that solves such a block inside the partition is future work and requires a spec amendment | Structural causal-plan and `ClockPartitionTransactionProgram` construction | An unowned row is a named span-bearing rejection today, not a silent hold and not a coverage hole the new owner may quietly absorb |
| A typed conditional region constructs from one scalar Boolean condition, exact typed captures, two complete same-typed result tuples, and prior issued call owners; a typed finite-loop region additionally constructs from one valid compact domain, exact Integer binders, same-typed initial/transition carried tuple, and invariant captures. Region-local reads require dominating definitions, every output is stored once, and wire replay recursively rebuilds the same interfaces before exposing the parent. | Shared typed-program construction and checked wire replay | Lazy branches and finite loops cannot return partial/mismatched values, read hidden state, form recursive owners, unroll in IR, or smuggle scalar bytecode behind a typed operation |
| A typed internal assigned-state product carries one checked payload type and an absent-or-present semantic value/range, separately from ordinary typed registers and call interfaces. Regions publish every internal state once, including explicit absence; only the source-owned ordered assignedness check can issue a normal payload projection, using the existing checked assertion interface, canonical value name and use/return provenance. After that ordered check issues an ordinary payload under its exact definition identity, later region capture retains it through the existing completed-definition capture path even when the conditional/value still owns its internal assigned state. Only a definition without a completed payload follows the state-only capture path; another definition of the same value cannot substitute for it. Nested products retain the selected path without replay, and wire decoding calls the same branded constructors. | Shared typed-program construction and checked wire replay | Partial function definitions cannot fabricate a numeric value, weaken complete ordinary tuple coverage, lose first-fault effects, or expand compact tensor state into scalar operation graphs |
| The canonical conditional assigned-state input query may prune an exact branded conditional/value pair before visiting that owner's selection conditions or retained arm payloads. The shared expression operand traversal consumes this same query and exact state predicate, including nested conditional states beneath indexing and field projections; it does not implement a second state walker. Its default and constructor fact queries remain unpruned. Typed region capture discovery may select a pruning boundary only from an already issued state in its actual enclosing assigned-state environment; that same state crosses the checked region interface. Every unbound owner retains its selection conditions and assigned source inputs, and the ordered `RequireAssigned` action remains the only payload admission. Query pruning issues no value, assignedness fact, or new state owner. | DAE conditional-state traversal and Solve typed-region capture discovery | A newer local state can retain its transported predecessor without reopening private sibling definitions or re-evaluating old mutable selections, while missing captures and unassigned paths remain strict |
| A source-owned assigned-read guard retains its checked presence predicate, original value/use provenance and error action but cannot issue a model crossing root; scheduling distinguishes its typed source kind from an authored assertion, without inspecting message text. Authored error-level assertion roots and unsupported-backend refusals remain unchanged | Solve call-check scheduling and checked typed consumers | A partial function read cannot fabricate model events or lose its ordered fatal check |
| Every typed tensor-algebra operation derives its result type from checked operand types plus the minimum compact axis/domain metadata, rejects incompatible ranks/extents or arithmetic profiles before issuing a destination, and wire replay calls the same constructor | Shared typed-program construction and checked wire replay | Tensor algebra cannot acquire forged result shapes, scalar coordinate lists, or a second backend-specific semantic lowering |
| One branded causal-discrete plan orients executable discrete Real residuals by unique-target forced elimination after excluding causal-algebraic rows; a target is a whole coordinate or one literal-subscript scalar element of an array coordinate, a whole definition covers every element, and any element definition leaves the whole coordinate taken; current reads are distinct from `pre`, and ambiguous, cyclic, self-reading, or type/shape-incompatible rows remain unowned | Structural construction | Solve and GALEC cannot assign different meanings to one B.1b equation |
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

Constant-call and STRUCT-T10(a) replacements consume the same evaluator's exact
typed source runs. Integer and Boolean payloads never pass through a numerical
Real projection. A source Repeat reconstructs as a checked `fill` of its exact
scalar and original declared dimensions; authored Literal payloads reconstruct
as literal arrays and are never coalesced by equality. Reconstruction does not
enumerate a Repeat's extent. The current reconstruction profile admits one
Repeat or one Literal run. A mixed-run result retains its original source
expression after the original evaluation and first-fault checks complete; the
fold does not densify it to gain representability. Real declarations retain
the language's Integer-to-Real conversion, separately from exact Integer
payloads, and preserve authored IEEE signed-zero bits.

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
| Native root register storage may use one private constant virtual-range-to-physical-range layout per exact checked program. The canonical register-version owner supplies original destinations and operand selections. Overlapping writes and complete read-selection hulls form indivisible contiguous components; each component stays live from its first write through every original read and write. Only components whose last operation strictly precedes another's first write may share physical storage. Offline largest-first placement may reuse gaps determined by these exact closed operation intervals; placement order does not change the intervals or permit same-operation sharing. Mutable versions, indexed/strided ranges, captures and original same-operation aliasing retain their exact virtual relationships; nested conditional namespaces remain separately checked above the physical root extent. Capacity construction retains each immutable complete program with its issued layout; private source-ordered stage bindings emit that retained program, including through outlining, and require exact program consumption. Address consumers retain original virtual admission and apply the same layout before checked byte addressing. No operation, output order, fault or source identity changes. | Native root arena construction and address emission | Exact ranges retain values while dead storage can be reused |
| Native typed frame construction keeps actual linear-memory ranges, offsets, cursors and scratch high-water marks checked in the original u32 address domain. Hypothetical unshared storage accounting sums sequential regions and call frames in checked u64 and is report-only; it cannot impose an address limit or issue storage authority. The same accounting type crosses CallLayout and public scratch reports without truncation. Accounting overflow and actual address overflow remain typed refusals. | Native typed frame layout and scratch reporting | Hypothetical storage totals do not address memory |
| A completed function fold output is a reaching definition in its enclosing scope. Typed capture discovery uses the same canonical lexical scope relation as ordinary SSA definitions; enclosing outputs resolve once as typed captures, region-owned outputs retain their dependencies, and disjoint or foreign definitions remain refused | DAE-to-Solve typed function capture construction | Transition-local values cannot become captures of a sibling region |
| An iteration's numeric frame retains its canonical fold body scope. A missing local SSA expression or completed fold tuple may borrow an already-issued value from an ancestor frame only when that scope classifies its exact reaching definition as Enclosing. Both readers use the same lexical lookup. Lookup stops at the first function-entry frame, including recursive invocations; Region, Disjoint, foreign and non-definition expressions cannot borrow | Canonical DAE numeric function capture reads | Fresh iteration caches preserve exact typed lexical captures without reusing sibling or foreign invocation state |
| Numeric evaluation retains each successful ordered loop's completed typed carried tuple in its exact source frame. Output projections consume that tuple without replaying the transition DAG outside its branch statements; function invocations and enclosing iterations keep fresh frames, active backedge reads retain the active fold stack, and failed execution publishes no tuple | Canonical DAE numeric function evaluation | Preserves exact Integer/Boolean/Real values, source action order and undefined-read refusal |
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
| Assertion-observation construction may reuse the canonical register-flow proof through a private-field witness borrowing the exact immutable operation slice. Only the existing register-flow constructor issues that witness; invocation admission accepts its table, index, and actions but no replacement program. Standalone invocation construction issues the same witness and invokes the same admission body. Slice validation creates it only upon an allowed observation; every site, action, message, source, and operation-index check remains unchanged | Solve assertion-observation construction | Repeated observations share exact-program proof without admitting foreign slices or incomplete invocations |
| Immutable pure-call site equality may establish equality of shared `Eq` interface slices by exact shared-storage identity; independently reconstructed slices retain full structural equality. Owner identities, directional interfaces, projections, and affinity remain part of equality. Assertion-observation matching may test exact predicate identity before the complete site interface, but cannot omit any matching or validation obligation | Solve pure-call interface and assertion-observation construction | Shared storage avoids repeated equality walks without accepting foreign interfaces |
| Root and nested pure-call registration share one table-scoped lookup keyed by the exact DAE-issued call-owner identity. Only completed owners are reusable, including the members of a SOLVE-C62 recursive group, which is issued before any owner whose body enters it; re-entering an owner still under construction is refused; distinct call identities never merge through names, spans, or body equality. Each caller retains its actual argument tuple, activation, ordered result projections, and every invocation's assertion predicates. Completed callee assertion interfaces and their message-output bindings share immutable storage; nested output-offset rebasing constructs a distinct binding inventory, and sharing never memoizes caller values or execution. Finishing a table discards its registration scope | DAE-to-Solve pure-call construction | Repeated demand for an identical nested source owner must not expand an acyclic call graph into an invocation tree or authorize reuse across caller input coordinates |
| Inside one function-body lowering scope, each nested call owner's assertion-predicate slot holds one value. A conditional region publishes, for each slot of a call its arms reach and the scope has not yet evaluated, the predicate of the arm that executed or the unselected (vacuous) value; the region's call values stay inside the region. A later evaluation of that same call owner in the enclosing scope is the same pure call in the same environment, executed on every path, so its predicate replaces the published one: it is exact on every path and the branch value it replaces equals it whenever that branch executed (MLS §8.3.7, §12.4.4) | DAE-to-Solve function-body lowering | A call shared by a nested conditional and a later unconditional read (the `regFun3` branches of the `Modelica.Fluid.Pipes.BaseClasses.WallFriction` `pressureLoss_m_flow_staticHead` functions) keeps one assertion per invocation instead of being refused as a duplicate predicate definition |
| A function loop lowered as a fold carries the assertion slots of every call its iterations reach after its carried targets: each predicate slot enters at the satisfied value and leaves as the conjunction of every iteration's predicate, and each message value slot leaves as the value of the first iteration whose predicate failed, because the first failing assertion ends the function (MLS §8.3.7, §11.2.8.1). The first condition of a correlated conditional runs in the enclosing scope before either arm, so the slots of the calls it reaches are published from that scope and the arms publish only the slots still pending. A compact map body that reaches an asserting call remains a typed refusal | DAE-to-Solve function-body lowering | `Modelica.Media.Water.IF97` property functions call asserting functions inside loops and in branch conditions; refusing them blocked every IF97 medium |
| Each prepared refresh row owns the selected isolator for its exact target | Solve evaluation construction | Runtime cannot rediscover another target shape |
| Assignment construction proves unique destination ranges for each source prefix with a compact producer index; every nested prefix lookup uses the same index and excludes later producers. Range overlap remains a refusal to issue an unversioned assignment certificate, while register gaps and nonmonotone destination order remain valid. Tensor extents must not expand into per-register ownership entries | Solve assignment construction | Shared source DAG walks must not repeatedly scan whole operation prefixes or scalarize ownership proofs |
| Read-only Y-dependency queries may reuse one complete canonical register inventory across exact prefixes of an immutable source only when the compact producer owner proves unique destination ranges for the whole source and the canonical dependency walk succeeds. Producer, candidate, and shape queries still read the exact requested prefix. Any overwrite or refused suffix keeps the original prefix analysis; at most one fallback prefix stays resident per source. Register-only dependency walks validate output reads through the same operation owner without retaining unused output inventories. | Solve refresh construction and read-only footprint queries | Source-version identity and fail-closed refusals remain exact without rebuilding or retaining one wide register table per output store |
| An additive assignment certificate derives a finite nonzero constant target coefficient and a bounded weighted projection of target-independent source registers; shared subexpressions remain shared, the original evaluation prefix remains required, and wire replay matches the exact derived selection. Final backends materialize arithmetic from the issued projection, while evaluators never recover it from a runtime residual or old target value | Solve assignment construction | Independent offsets avoid cancellation; shared DAGs stay bounded |
| A reciprocal assignment certificate is issued only when the target enters its residual once, as the denominator of a quotient whose numerator is a nonzero finite literal, beside a target-independent remainder; the literal is proven nonzero at construction, so the isolated value `-(s * c) / remainder` is the unique solution whenever the remainder is nonzero and finite and evaluators refuse it otherwise. Wire replay rederives the quotient, literal, and remainder | Solve assignment construction | A target starting at the pole of its quotient must not seed a Newton step there |
| An affine tensor projection binds one offset/coefficient rule to each selected canonical source operation, preserving compact tensor ranges and source DAG sharing. Independent cuts reuse original register ranges; bilinear rules require an independent operand and division an independent denominator. Execution adapters materialize only the issued relation after the unchanged source prefix, never substitute a target into a call or assertion, and decline zero/nonfinite coefficients. Wire replay rederives source producer identities, rules, output identity, and independent cuts | Solve assignment construction and final execution adapters | Tensor isolation preserves evaluation effects, bounded structure, and singular implicit equations |
| A pure-call value-projection certificate derives exact output-to-input coordinate maps only from a total checked body of copies, finite constants, static projections, fills, transposes, and nested certified calls with no assertion predicates. Isolation consumes the issued certificate and retains the call's evaluation prefix; arithmetic, runtime indexing, control flow, and assertions remain unproved. Wire replay rederives and matches the certificate, and a dependency relation alone never proves value equality | Solve pure-call and assignment construction | Exact forwarding can expose an isolator without erasing calls or changing nonlinear branch selection |
| An exact-assignment program is built by one `ExactAssignmentProgramBuilder` that takes the source prefix once, fixes the first unused register at that point, and reserves the registers of every operation appended through it, so isolating n assignments after one prefix costs n appends and never rescans the program; the free `materialize_target_assignment` is one builder over one assignment | `rumoca-ir-solve::refresh::materialization` | A wide call owner's refresh stays linear in its rows (a 407040-row call program spent 5 minutes rescanning its prefix per row) |
| A prepared scalar row may certify that its complete solver-Y gradient is invariant under solver-Y and time at a fixed parameter snapshot | Solve scalar construction | Runtime may reuse the exact gradient only while the parameter snapshot is bitwise identical; nonlinear or time-varying gradients remain uncached |
| An affine projection block formed row by row from reverse gradients retains its origin linearization (Jacobian, row and variable scales, row magnitudes, derived-scale flags) and reuses it only while the union of its certified rows' parameter snapshots is bitwise identical; every uncertified row is refilled from its reverse gradient with its scale, magnitude, and flag, and every fallback row scale that reads a coordinate outside the block is formed again, so the reuse is bitwise the formation at the current point. Debug builds form the reused linearization afresh and require equality | Component numerical projection | A switching circuit's affine block stays fixed between events except for state-dependent coefficient rows; re-forming it at every projection repeats work whose inputs did not change |
| A retained sparse projection workspace binds one immutable matrix pattern, its symbolic factorization, and numeric storage. Numeric factors become usable only after successful checked factorization of the current bitwise-exact conditioned coefficients; mutation revokes that state before refactorization. Pattern changes reconstruct the owner, failed factors never solve a right-hand side, and scratch reuse preserves kernel, parallelism, and fallback policies | Component numerical projection | Storage reuse cannot authorize stale or partially factored equations |
| An affine elimination layout constructs only from complete, unique block-local row and coordinate permutations whose selected causal diagonals exist. Dependencies on later causal coordinates issue guards, each paired with the causal position holding it and the later causal position solving its column; every solve checks their fresh unconditioned coefficients before factor reuse. A nonzero guard, unless its holding step is already a tear, promotes the step solving its column to a tear before elimination; a weak or zero pivot promotes its own step in place during elimination (a new recovery column, a unit recovery row for its coordinate, and its row appended to the reduced system), which is exact because no earlier step depends on a promoted coordinate except through an exactly zero guard. Promotion proceeds up to `projection_policy::torn_promotion_capacity` of the issued tear count, and then declines reduction; a nonfinite coefficient or a singular reduced system also declines. Its immutable sparse dependencies drive reduced factorization and full-coordinate recovery. Promotions are re-derived on every factorization and never carried between calls, and cached reduced factors are usable only for the exact current conditioned coefficients, guard outcomes, and layout. The linked kernel and every generated C component share the admission rule `projection_policy::affine_elimination_capacity`, the guard table, and the capacity; it admits a sparse-candidate block and a small dense block its issued tearing reduces (fewer tears than unknowns), so a torn affine block of any size eliminates along its causal order on every call. A promoted solve passes the same scaled residual certificate as every affine path (`ALGEBRAIC_REFRESH_TOLERANCE`). | Solve structural artifacts and numerical projection | Conditional triangularity and numerical nonsingularity require fresh evidence |
| Manifold projection binds each checked sparsity color to exact logical outputs of its canonical directional programs before execution. A selected tensor program executes once per color and scatters only its issued output placements; unrelated programs are not required. Admission proves unique, complete, repeatable output ownership. Residual certification, numerical failure propagation, and atomic state correction remain unchanged | Solve structural artifacts and final projection adapters | State constraints must consume the same tensor output and coloring proofs as algebraic blocks, without runtime regrouping or repeated unrelated Jacobian work |
| A continuous refresh owner may certify a projection block affine only by deriving every selected residual's degree in that block's exact unknown inventory. Selected outputs of the same canonical node/program share one ordered degree walk within the block; each store reads its original register state, and an unsupported later suffix does not erase an earlier store's proof. Different unknown inventories remain separate proofs. Outside-block coordinates are coefficients; compact typed-call owners issue and replay input-interaction summaries, and unsupported operations remain unproved. This certificate permits unrestricted Newton corrections for that block but proves neither nonsingularity, parameter-static Jacobians, complete causal seed coverage, nor nonlinear branch equivalence | Solve call and refresh construction | State-dependent coefficients must not obscure linear blocks or authorize unrelated solver shortcuts |
| A causal refresh row constructs `ParameterStatic` only by recursively accepting pure scalar/tensor/fold/conditional owners, exact P ranges proved immutable during continuous-time mode (including the dedicated homotopy endpoint after its initialization owner pins lambda to one), and Y ranges owned by itself or earlier `ParameterStatic` rows; time, state/dynamic Y, every other runtime P coordinate, seeds, mutable tables, and impure state reject construction | Solve refresh-plan construction | Runtime can reuse tensor-native invariant values under an identical bitwise parameter snapshot without operation rescans, coordinate expansion, stale dynamic inputs, or continuous root searches over initialization-only continuation state |
| A function-fold program derives its finite domain, carried tuple types, initial definitions, update definitions, result projections, and exact provenance in one construction; no scalar iteration list is accepted or stored | Solve program construction | A backend receives the original bounded loop and cannot depend on post-expansion graph recovery |
| The staged executor accepts a checked value-stage schedule only when every numerical block coordinate has a compiler-issued causal seed; the schedule begins with the complete seed sweep in certified order, then reapplies, immediately before each BLT stage, the block's local seeds whose isolation coefficient is a construction constant (`TargetAssignmentShape::constant_coefficient`; a coefficient that is a solver value, such as `frame_a.R.T[1,1]` isolating `R_rel.T[1,1]`, can vanish and would make the seed singular on every call, so its target keeps the value the complete sweep left); otherwise execution uses the complete preserved projection, and a runtime-unavailable isolator also restores the incoming coordinate before that fallback | Solve evaluation construction and runtime artifact check | Complete seed coverage makes block-local branch selection construction-owned; missing, reordered, or deleted warm-start witnesses can select a different nonlinear solution, while treating a runtime-singular isolator as a semantic failure would discard the preserved implicit equation |
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
| Native checked-assignment WASM executes `LoadIndexedRegister` with its canonical dimensions, strided packed source and index operands. Its register-flow certificate proves the source range; runtime axes must be finite exact signed Integers within their one-based extents before the affine source address is accessed. | Solve construction and native WASM emission | Preserves compact checked indexing without clamps, candidate expansion or invalid reads |
| A function slice whose start reads a loop index or run-time Integer is admitted only with a construction-constant step and extent, proved by the same `exact_range_distance` that sizes it in the function shape proof and in its lexical binder scope; it lowers as one one-iterator comprehension index map over that extent, consumed by the compact gather of the base; the loop-index and run-time terms of its start are read at each point, never folded or expanded into scalar element reads | DAE function loop compaction and Solve typed gather | One distance proof sizes and lowers a readable image-kernel window, so the shape and the view cannot disagree |
| Typed native input lanes (SOLVE-C69) are classified once from the canonical external-input, Integer/Boolean parameter and constant storage runs; their per-stage exact Integer bindings (lane-bound call argument cells, lane-published Integer outputs, and the call result cells whose Real view is unread) are issued with the schedule and consumed unchanged by the WASM emitter, which writes the views into its private P copy in its entry prologue, checks every unbound Integer view, and never recovers a binding from emitted code | Solve construction and native WASM emission | One owner decides where an Integer crosses the Real register boundary, so no backend rounds it silently |
| Gather-only programs use the checked transaction ABI. Failed conversion or bounds checks return nonzero status, leave P unchanged and prevent whole-program Y publication; inactive conditional regions perform none of their gathers. | Native assignment WASM profile | Retains checked failure, branch laziness and atomic publication |
| Model address faults carry their registered source-program span and emitted kernel/program/operation/region coordinates in a separate diagnostic inventory; they never receive a manufactured function owner. Typed helper statuses remain unchanged, with model faults allocated after them. | Native WASM fault catalog and artifact API | Keeps diagnostics source-bound without inventing semantic identities |
| A typed pure-call WASM operation is an adapter of its `rumoca-eval-solve` typed definition and never a second semantics: `Scale` (same numeric element type for tensor and scalar, row-major multiplication by the fixed scalar cell, reusing checked Integer multiplication and domain faults without a Real conversion), `Cross` (Real three-vectors, `lhs[a]*rhs[b] - lhs[c]*rhs[d]` per component), `Reduce` (a left fold from the first element, Boolean minimum and maximum as `and` and `or`, Integer overflow faulting at the step that overflows), `Diagonal`, `Concatenate` (row-major: for each index of the leading axes, each operand block in operand order) and `SelectElement` (the fallback when any one-based coordinate lies outside its axis) produce the evaluator's cells in its element and operation order; an operation the evaluator refuses for a value faults with the operation's source-bound status and publishes no output | Typed-call WASM emission | Native and interpreted pure calls return bit-identical tuples; differential tests in `rumoca-exec-wasm` `tests/suite_typed_calls/aggregate_ops.rs` |

A pure-call assertion predicate output owns its exact source severity. Each
converted message output names an earlier predicate output of that same owner
by a checked backward distance. Current wire replay refuses a missing role,
a zero/out-of-range distance, or a distance naming an ordinary result.
Directional construction relocates this association across primal/tangent
output pairs; severity is retained once on the Boolean predicate. This checked
interface conveys no assertion-stop or native-admission authority by itself.

The typed assertion vocabulary issues a check capability only from
the exact enclosing owner output interface; nested regions borrow that same
interface and replay reissues the capability. Message work is either
`NoCaptures` (empty source/destination association and callback operations) or
`Captures` (a nonempty checked region with exact role-derived output count,
types and output associations). Ordinary value calls report a diagnostic
failure and never publish an incomplete value tuple. The diagnostic payload
grants no C25 observation or model-projection authority. Source emission and
C25 admission use the coherent SPEC_0036 cutover: effect-output publication,
child forwarding and immutable invocation/action membership are checked
together. Unimplemented native and directional adapters decline. A typed
directional owner may retain an uncaptured assertion check when its condition
uses the exact primal registers and its checked predicate association is
relocated through the canonical primal/tangent output ABI. The check executes
once at the authored position; fatal failure returns no ordinary or tangent
results and its diagnostic names the original source predicate and span.
Warnings continue. A captured-message directional check is admitted only when
its complete checked message region has a tangent relation in the supported
vocabulary. Its captures and results use the canonical primal/tangent ABI;
the checked region runs only at the original failed check, never on a successful
check or before its authored position. Diagnostic captures retain only the
original message outputs; tangent lanes do not become authored diagnostic
values. Unsupported message operations decline the whole directional owner.
Selected regions and compact folds retain that same check
and control order. A nested call with an issued directional owner retains its
source-issued assertion forwarding: construction relocates both the exact
child predicate and enclosing predicate through their canonical directional
output ABIs and reissues the association with the existing branded call and
assertion constructor. The child check executes once at its authored position;
a stopped child retains its original predicate, span, severity and invocation,
and publishes no incomplete enclosing primal or tangent tuple. Selected
regions remain lazy, and a foreign owner or mismatched association cannot
acquire forwarding authority. Wire replay derives the same checked directional
association from the checked primal owner. Captured checks and assertion-bearing
maps remain outside this directional profile; scalar-bodied map AD evaluates
separate primal and tangent maps, so it must not duplicate assertion effects.

The supported assertion-flow metadata derives once at pure-call owner
finalization and wire replay from the canonical checked body. Every predicate
role must have exactly one structural check at its issued severity or forwarded child predicate.
Lazy selected regions and compact fold continuation/transition regions retain
their canonical region ancestry; repeating one compact site does not introduce
another source. Distinct sites naming one predicate, including sites in disjoint
arms, decline issuance. Every reachable site must have stopped-execution coverage;
calls require the exact child's derived flow plus complete source-issued forwarding.
A complete predicate store must use that checked condition, the forwarded child
predicate, or a typed true
constant. Every message store must use the corresponding check/call destination,
and those destinations have no other consumers. A nested site's complete root
publication must instead store literal typed true and canonical inactive message
cells. Region-local message destinations cannot escape into a region result or
another operation. The one IR inactive-cell constructor preserves the exact
scalar type: Boolean false, format-correct Real zero, or Integer zero clamped to
that checked domain. Inactive cells convey no failed-capture authority. Zero
iterations and unselected arms therefore need no fabricated result captures.
An assertion-free owner issues
an empty flow only from its checked body and
transitively checked child flows; a result-only interface alone proves nothing.
One invariant-branded emitted-call handle binds the exact operation, child
interface and ordered destination registers. Its canonical call operation owns
unique child-to-parent predicate forwarding with exact issued severity and message
role type/count associations. Current wire requires the forwarding field and
replays each association through the same constructor; missing, forged or
duplicate associations fail closed. Ordinary value-only call APIs delegate to
the same emitter. Warning checks use the same issued flow and continue after
recording the violation. A SOLVE-C62 recursive group issues an empty flow only
after its constructor proves that its members and every earlier callee are
transitively assertion-free; pending group interfaces share that empty-flow
obligation until the group is issued atomically. This grants no assertion
publication for recursive owners. Nested message checks and unsupported
shapes retain ordinary role-only ownership without publication proof. Pure map
and message regions require transitively assertion-free checked bodies; they
introduce no assertion publication. The opaque descriptive flow identifies
canonical check/call operation positions and structural region ancestry;
it never grants model/action membership or native/C25 admission by itself.

Assigned-read predicates refine that same flow from the constructor's exact
presence/check association, only at error severity without message captures.
The subtype is private and omitted from wire data. Forwarded predicates inherit
it only through the actual child's derived flow and issued forwarding relation;
region collection retains the original selected source. Authored Boolean checks
and warning checks do not acquire it. Replaying the canonical program and call
table rederives the subtype; detached metadata still proves no model/action or
native normal-call membership.

The normal typed native adapter consumes the actual immutable call table and
owner to refine a complete capture-free fatal assigned-read interface. Every
predicate must have the same owner's derived assigned-read subtype; an authored
or warning predicate keeps the ordinary interface refusal. The emitted check
executes before its checked payload projection and publishes no result on
failure. Existing call status forwarding retains the child's owner, operation,
region and source span. This refinement neither creates observations nor
authorizes omission of a model's event action inventory.

The model-level refinement covers every original action using the existing
checked invocation projector on calls in the actual issued native value-stage
kernels. Provisional classification is private and grants no returned schedule
until complete coverage succeeds. The actual call table's existing immutable
lineage remains attached to the schedule, and retained preparation refuses a
foreign table even when its replayed contents compare equal. Finalized FMI
construction reissues this source association before creating the immutable
root. Standalone problem construction still refuses event actions.

Compact refresh and assignment artifact construction may consume the finalized
problem and actual immutable model call table before artifacts finish the model.
This construction-only source context delegates to the same native checker,
original action coverage and existing table lineage. Calls are validated first;
foreign cached tables refuse. Problem-only construction has no such context and
remains strict. Canonical model decoding reissues the context from its decoded
table before artifact derivation; no wire flag transports it, and alternate
charts may not substitute a primary table for their own.

The existing call-reference checker exposes a construction-time membership
query before refresh construction. It traverses the same actual inventories;
the finalized validator retains its complete shape/refresh check after owners
exist. Neither query alone nor a partial draft proves a finalized stage.

### 6a. Continuous Refresh Construction

Continuous refresh optimizations require constructor-issued facts bound to the
canonical residual and unknown inventories; wire replay rederives those facts,
and consumers may use only the property proved by the certificate.

Logical output bindings borrow the checked scalar-program block that proves
their complete cardinality and ordering. Consumers iterate this issued pairing
of source program, local output, and logical index without asserting a second
independently advanced iterator's length. Wire replay checks the same pairing.

An exact ranged assignment derives only from a canonical `StoreOutputRange`
and its source-validated Direct isolators, preserving selected output ordinals
and row order. Construction proves the value-register progression and exact
target span; scalar stores are never reassembled into a range. A scalar-program
block issues an affine output-span projection only for one canonical ranged
store whose complete logical mapping agrees with that span. Wire replay
reconstructs both facts. Generated exact assignments write and check each
value in canonical order, preserving the first non-finite fault and its written
prefix, including the failing value; successful execution writes every target.

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
source; a row without a JVP program, or a causal row without a coefficient in
its target, declines the plan. The plan takes the multi-lane form (one lane
per tear and one coefficient lane) when that lane count is below
`MAX_TENSOR_LANES` and every program widens; otherwise it takes the
directional form, which evaluates every step's coefficient first and then one
tear column at a time through the one-direction JVP programs, equal to the
multi-lane form bit for bit. Consecutive causal steps of one program whose
outputs depend on no other member's target (the program's exact per-output
seed dependencies) form a group that one evaluation answers, its coefficient
lane or direction seeding every member target; consecutive reduced rows of one
program share one evaluation. Each member then reads exactly the values its
own evaluation would give. Evaluation follows the sweep order and declines
at a vanished or non-finite coefficient. The plan is the only source of a
torn block's tear Jacobian in the linked kernel and the generated C: a
declined plan, a non-finite entry, or a reduced row or column that is exactly
zero declines the torn solve to the dense block Newton, never to a difference
quotient. A
block's colored tangent plan constructs from its issued colored Jacobian
application: each distinct program runs once with one lane per color that
calls it, and each placement takes the value that color's call would write.
Blocks whose applications share one source construction (one block per scalar
row of a wide tensor operation) share one widening of each source program per
lane count, and one prepared evaluation of it: the widening and its register
tables are built once per source program, never once per block.
Plans are derived views, rebuilt by each consumer from the same construction,
never canonical IR.

Algorithmic differentiation is the only Jacobian source. Every Jacobian,
tangent, and sensitivity that Solve construction, the linked kernel, and the
generated C form comes from a forward-mode JVP program or its reverse: the
torn tear Jacobian from the block's tangent plan, block Jacobians from the
solver-Y JVP rows and their colored application, the settled initialization
Jacobian from the initialization residual's JVP along the settled view's
tangent (the initialization update rows' JVP and the seed projection of the
algebraic plan; a block Jacobian over chosen initialization rows projects only
the algebraic blocks those rows read, a cone issued once per initialization
row from the structural patterns when the runtime is built, so a block outside that set that is singular at the point, such as an
undetermined enthalpy at a startless zero mass, never refuses it, while a
singular block inside the set still refuses the derivative; the values those
rows read come from the same cone, reconstructed on the evaluation-local view,
so a block outside it that is undefined at the seed, such as `h` of `H = m*h`
at that zero mass before the block projecting `m` has run, never fails their
evaluation, and the complete residual is read only after every projection
block has had one pass; the generated C reconstructs the complete view for
every block and refuses such a seed), the coupled event Newton from the discrete event
rows' JVPs (scalar rows, runtime assignments, guarded programs, and
structured maps, lowered as discrete Solve artifacts), and the `--inspect
jacobian` probe from the state JVP. No path differences a residual. The
continuous and initialization residual JVPs are lowered with the model, so a
row without one fails construction; the update and event rows' JVPs are
optional artifacts, and a solve that would read a missing one reports the
missing derivative. A pure callee whose body admits no directional owner
fails the JVP lowering, and Solve lowering refuses every external C or
Fortran callee because the runtime executes none, so no callee needs a
difference quotient; native table operators carry their exact slopes. A
`derivative` annotation feeds structural index reduction, while the JVP
differentiates the callee body. `architecture_hardening_test`'s
`ad_only_jacobians` scan fails when a source or code template names a
difference quotient outside its listed AD verification batteries.

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

With compiled execution, a residual pass over a block's rows calls each
compiled entry once for all the rows it serves: every row takes the entry its
own evaluation takes (the active split's dependent program for a one-output
program the split holds, else its compiled residual program), so each value
is the bits of that row's own call. Which entry serves which row depends only
on the rows and the active split, so it is formed once and reused by every
later pass over the same rows under the same split. A pass with a row that has
no scalar view, under an interpreted split, or whose compiled entry declines
is evaluated row by row as before.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Within one structural-artifact construction, applications of one immutable source share one operation-invariance proof; source replacement or specialization derives fresh evidence for its exact owner. The existing register-source checker must prove every read uses a seed-invariant register version; seed loads and any operation with non-repeatable effects cannot establish reuse. Every write replaces the destination version's evidence. | Projection-Jacobian construction | Preserves failure behavior without multiplying source-wide proof work by the block count |
| Native preparation may retain certified result ranges after their first complete, ordered execution and reuse those exact values in subsequent colors. Mixed primal/tangent tensor operations remain complete operations; no scalar graph is reconstructed. | Prepared projection application | Preserves tensor ownership and arithmetic order |
| Reuse is confined to one non-reentrant call at identical Y, P, time, external tables, and execution mode. The first failure aborts publication; later calls execute their first occurrence afresh. | Native projection workspace | Preserves validation, coordinate freshness, and failure semantics |
| An ordered sequence of assignment programs that one call executes in order (an exact refresh assignment schedule), and a single program (a torn run's chain program, `target_isolation_chain_program`, replaced by its one segment when that has no more operations), is issued as shared-value segments: consecutive fusible programs are concatenated in one register file, each value computed once and later occurrences read the first by register, a copy names the value it copies, and a load of a slot an earlier program of the segment stored reads the stored register. Native ranged-store programs remain unchanged segments: their canonical range owns the lanes and scalar sharing cannot expand it. Such a program closes the preceding scalar segment and begins a fresh symbolic incoming-slot context for subsequent scalar sharing. Classification runs once per other program: a program is fusible when its register flow validates, every register is written once, and every operation is a solver-slot load, a copy, an output store, or a renamable value (constants, time and parameter loads, unary, binary, compare and select operations, dot and matrix products, tensor arithmetic, cross, transpose, concatenation, fill and identity, parameter tensor loads, and calls whose results are values only). A value-only call is shared like any value: its owner issues a closed-input coordinate (below), so identical operand terms prove identical results, and every row reading one call coordinate of a refresh reads one evaluation per refresh with no value retained between refreshes; an unread call is kept, since its numerical failure remains observable. Every other operation makes its program a segment of its own, executed unchanged: random and impure-random draws, calls whose results include assertion predicates, and folds, conditional regions, table lookups, seed loads, and indexed loads, whose effects are those of their bodies or inputs. Variability (MLS §4.4.4, §4.5) follows from the segment boundaries: a shared value has the variability of its most variable input, and a segment keeps no value between calls, so a value of constants and parameter slots alone is computed once per call and one reading a solver slot or time once per evaluation; a discrete value, a previous value (`pre`), a delayed value, and relation memory are read from their memory slots at every call like any parameter-slot load, and an event boundary lies between calls, so no shared value crosses one. A segment is closed when the registers it uses plus those the next program may add would exceed the single policy constant `SHARED_VALUE_REGISTER_CAP`, except that a program whose call operand terms match a call the open segment holds joins it, since a call costs its body, which no register count bounds; a closed segment keeps only the registers it uses, renumbered in order: an operation whose value key records the offsets between its operands (a range or unrenamed operation) keeps every register between its lowest and highest read, so those offsets and every contiguous range are preserved, and a scalar-source operation keeps only its reads, so compaction costs the operands rather than their distance; the construction records, for each value a later segment recomputes because of that cap, the segment and the operation kind. The checker first proves a complete ordered partition of the original programs. Every native source program must be its own unchanged segment: complete canonical operation bytes (including IEEE literal bits, input/output identities, typed sites and nested regions), exact ordered targets, output cardinality, and valid register flow prove the identity; serialization errors refuse. All operations execute against the same source-owned call table and frame context. Each complete scalar interval between those identity barriers is checked for arbitrary incoming slots by the original symbolic proof; their ordered composition preserves outputs, final slots and first faults without enumerating native lanes. Within an interval the checker evaluates the original programs and the segments symbolically over one hash-consed term table (each value its interned operation shape and its operand terms; each load the current symbolic slot; each copy its source term), where an operation's shape and operand terms are interned once and each of its outputs is a term over that one id, so the cost of a wide tensor operation is its width, and a register read set is collected in one validation pass and requires every output, in order, and every final slot to be the identical term, so each value is the same operation tree on the same inputs and the same bits; it runs at every construction, where a schedule whose segments fail it is a construction error and a single program that fails it keeps its unshared operations and is counted (`shared_value_proof_failures`, recorded on MSL worker rows), and the construction tests include one per variability class. One construction serves the runtime interpreter, the JIT, and the C renderer, each executing the segments in order and committing a segment's outputs before the next begins; each issued schedule has its own segments; the runtime resolves each issued schedule to exactly one executor when it is built (a compiled schedule the backend admits, otherwise an interpreter copy proved and prepared at that construction, and every schedule when there is no backend), so an admitted schedule owns no interpreter copy and nothing is prepared while the runtime runs | Shared-value construction and checker (`rumoca-ir-solve::linear_op::shared_values`), schedule segments (`rumoca-ir-solve::refresh::shared_schedule`), assignment-schedule execution in `rumoca-solver`, `rumoca-exec-cranelift`, and generated C | A repeated side-effect-free operation on identical terms has identical bits, so computing it once changes no value; segment commits keep every executor's order of stores, and an effect runs exactly as often as before |
| Register-keyed construction state of a shared-value sequence is dense, and the outputs of a wide operation are one consecutive block of terms owned by its operation key, so deriving and checking a call program costs a few words per register and no hash entry per lane | `rumoca-ir-solve::linear_op::shared_values` | A 4-million-register call program (FAST at 480x848) must not cost hundreds of bytes per register in its construction proof |
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
During initialization, expression consumers evaluate the original relation at
the current simultaneous solve iterate (MLS §8.6). Solve reserves its existing
initial-mode flag for every buffered relation and constructs a selected-arm
program: the literal relation in initialization, only its truth buffer after
initialization. The inactive literal arm is not evaluated. Runtime-assignment
effect construction excludes a comparison only under the exact cleared
initial-mode flag load; other guards, else arms, nested regions, and opaque
call effects cannot issue a relation-free proof. Root construction
certifies that the initial-mode flag is in-bounds hidden P storage, disjoint
from source storage, other runtime memory, and every equation writer. Wire
replay reconstructs this ownership proof before accepting effect certificates.
Root evaluation
retains the original relation operands. The buffer is distinct from condition-edge history and from
any whole B.1c Boolean result. Clocked, structured, suppressed, and call-scoped
relations retain their own checked activation and storage contracts. A
`noEvent` operand has a literal-evaluation context, including its descendants
and expression caches, even when it shares an expression with a monitored root.
The corresponding construction row is in [SPEC_0043 §6](SPEC_0043_CONSTRUCTION_CATALOG.md).

Initialization algebraic read cones derive from the canonical primal programs'
value and control dependencies, including comparison operands in retained
regions. Jacobian sparsity is derivative authority and cannot issue this value
ordering: a comparison has zero derivative while still reading its operands.
The cone closes only over requested algebraic producers, leaving unrelated
singular blocks outside it and preserving the original selected-arm evaluation.
The execution-mode lane is not a scheduled-event owner. Every initialization
boundary clears that lane and performs the MLS §8.6 event iteration at the same
start time; a right-limit continuation requires an independently issued event.

Each initialization homotopy step settles the numeric initialization equations,
covered algebraic equations, relation truth, and ordinary Boolean definitions
together before advancing lambda. Ordinary `pre` values remain fixed during
that solve. A rejected step restores both numeric and discrete coordinates to
the last accepted step; exhausting discrete iteration is a typed failure.
The continuation runs only around the iteratively solved blocks the
continuation parameter reaches (MLS 3.7 §3.7.4.2: the iteration loop from the
first BLT block with `homotopy` to the last nonlinear block that depends on
it). `InitialContinuationCoverage` derives that set once, when the runtime is
constructed, from the issued refresh stages: an exact assignment has one value
at every lambda, so it only carries lambda to its target; a projection block
is steered when a row or seed reads lambda or such a value, and its unknowns
then carry lambda on; an initialization projection row is steered on the same
reads. With no steered block, lambda stays at one and every `homotopy` reads
`actual`, the operator's trivial form, so a `simplified` operand that leaves an
exactly assigned value undefined (the Limiter's `simplified = u` under a
division in `Modelica.Blocks.Examples.TotalHarmonicDistortion`) is never
evaluated.

A scalar discrete coordinate determined by an initial algorithm assignment or
by an explicit initial equation `m = value` / `pre(m) = value` has one typed
initial-value owner. Recognition only selects this owner; construction derives
the target role and scalar type, proves that the value reads only coordinates
the initialization system settles (parameters, constants, `time`, inputs,
states, and algebraics), and rejects a second owner for the same target. Solve
proves that the continuous reads are settled independently of every discrete
value before it orders the update after the projection. The claimed Flat row therefore cannot also enter the numeric
initialization residual system.

### 6c. Compact Algebraic Families

A regular `for` family of continuous algebraic equations (an image kernel such
as `gray[y,x] = (r+g+b)/3`, `dx[y,x] = (gray[y,x+2]-gray[y,x])/2`, or a
windowed `sum`) keeps one owner from Flat to the runtime, so compile cost and
memory follow the authored source, not the domain size (SPEC_0032 §1). Each
stage below is a separate construction; a family that one stage cannot certify
keeps its scalar form at that stage and every later one, which only costs
time, never a value.

| Rule | Owner/Where | Construction proof |
|---|---|---|
| Event producers enter selected-array ownership only from the checked causal Real definition's element selection or the checked discrete-value target's scalar selection; declaration rank and width are not selection evidence. Whole-array producers retain their whole-declaration claim, including the exact producer projection consumed by a model-event transaction. A selected array's event-iteration producer partitions cover its whole declaration storage exactly once in canonical scalar order. Construction rejects empty, overlapping, missing, out-of-range, nested, unclaimed, or clock-incompatible partitions; each leaf replays the existing checked scalar or guarded producer range, and wire decoding replays the same proof. Current and pre bindings remain one complete typed declaration run | Solve event-iteration construction | Independent element triggers cannot become one priority chain or leave convergence lanes unowned, and a whole-array transaction cannot lose its exact producer |
| A family whose captured template is its only body keeps full bodies only at its corner cells (base and one neighbor per binder, through nested loops) and issues placeholder rows for every other cell, recording its owner in `flat::FamilyInteriors`: a regular state-derivative body, a proven regular parameter-variability assignment, or a continuous algebraic body. A continuous algebraic body defines only continuous-time `Real` declarations of its own class occurrence that no `der` equation reads; its nested loop ranges do not read enclosing binders; and its template reads only arithmetic, references, literals, arrays, ranges, unfiltered comprehensions, and smooth builtins (no relation, `if`, user call, record field, or operator with event, memory, clock, or derivative semantics). | `rumoca-phase-flatten` (`continuous_algebraic`, `equations::cheapen`) | The owner is decided from the still-symbolic body and occurrence-scoped declaration evidence before any cell is expanded; a family whose template cannot own its expanded rows is a flatten error, never a silent placeholder |
| A family flattened as continuous algebraic is a continuous structured owner lowered from its template; derived-parameter promotion admits only `FamilyInteriors::ParameterAssignment`. A consumer of the scalar equation view (flat-IR template rendering) rejects the family at its span (`EC007`); the DAE reads it only through its template. | `rumoca-phase-dae` template selection and family analysis | The cheapening reason is read from the family, never re-inferred from row contents; the template's call shapes pass the same certification as a selected materialized template |
| A continuous algebraic family whose rows are matched to the scalars of one algebraic variable lowers to one synthesized pure-call owner keyed by the DAE family (SOLVE-C70) and one residual program `target - call(inputs)` stored as a range, when its admission proof holds: one body `target[binders] - rhs` in the binder-substitution view; the target subscripts are the binders in declaration order over the target's whole extents, so domain order is the target's row-major order; `rhs` is a scalar of the target's scalar type; and `rhs` reads no coordinate of the target, no derivative or non-variable coordinate, and no function call. Constants `rhs` reads with literal bindings are folded into the body. The owner body is one typed map over the family domain evaluating `rhs` at the binder values. A family the proof refuses (a recurrence reading its own target among them) keeps its scalar rows by that refusal, and so does a family whose owner body has no directional relation (a product reduction among them), since the residual is differentiated through that relation. | `rumoca-phase-solve` continuous family lowering, `typed_functions::family_owner` | The proof is checked on the DAE family before any point is enumerated; the call's result order is the target's row-major order, so the residual is elementwise and its IR size follows the source |
| The residual program of an admitted family exposes one direct assignment per output: output `k` is `y[target + k] - call[k]`, and the call reads no target slot by the admission proof. Refresh certifies those rows with the same rules as any multi-output call program (§6a) and executes the owner once per refresh evaluation through the closed-input coordinate. The causal order releases ready rows source by source, so a family's rows stay adjacent and commit as one exact program. | `rumoca-ir-solve::refresh`, `rumoca-eval-solve::refresh_plan` | No new refresh unit or backend kernel: the owner is an ordinary SOLVE-C51 owner |

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
| A synthesized record-constructor call is built only through `RecordConstructorRef::call`; a whole-record copy of a decomposed record input is one assignment per field (arrays whole) and builds no constructor call | `rumoca-core`, Flatten record-parameter lowering (`expand_record_copies`) | No constructor call without resolved function metadata; copies need none |

### 8. FMI Component Construction Catalog

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Fixed guest construction captures the canonical first-party source closure, current lock and WIT as immutable build inputs, builds it once with the issuing pinned compiler and an empty attempt target, and retains the actual Cargo-issued core plus decoded import/allocation inventory. No raw core, artifact path, arbitrary build inventory, callback or digest constructor exists | `rumoca-exec-wasm::component_build` fixed source build owner | Only the actual prescribed receiver body can consume a linked slot |
| Final native FMI-LS construction consumes the simulation facade's private original library/capsule pair. It retains the actual emitted module and fixed source-issued receiver, generates the capsule-data provider internally, proves unique providers and exact five-pointer entry wiring, unshared public memory and real allocator/data/table/stack relocations, model-private arena isolation and initialization order, then checks the complete WIT world. The private fresh-root receiver replays the sole canonical capsule reader once, derives its original schedule/layout/typed cells/fault inventory and retains the same entry gate. Equal independent replays, foreign metadata, altered bodies/data/providers and separately composed imports cannot issue the final product. Receiver checks do not claim imported-body introspection; the complete graph constructor is the explicit trusted boundary | `rumoca-sim::native_fmi_ls` and private execution receiver | Source, compute, memory and lifecycle authority must be established together |
| Initial-value Repeat/Literal runs are issued by numeric source evaluation, scalar broadcast, and checked builtin fill before extent-sized allocation, then bound through exact declaration storage runs. The Solve initialization wrapper is their sole stored owner: checked source-span writes carry overrides, aliases and hidden slots; dense runtime buffers are derived read-only views and cannot mutate the owner. Wire replay requires the current schema and reconstructs the complete ordered partition and capacity; model wire and C-profile admission retain finite initialization checks over the issued runs. Singleton alias classes issue no transfer writes, so a fixed tensor start retains its source partition. Compact C projection borrows the same runs, copies exact IEEE bits, and writes every storage cell; an empty column initializes only its allocated dummy. Arbitrary authored or computed literal data retains its source data cost; no repeated run is inferred from equal dense values | Numeric initial-value evaluation (`rumoca-eval-dae`), Solve runtime-value construction (`rumoca-phase-solve::model_values`), checked initialization wrapper (`rumoca-ir-solve::initial_values`), FMI C projection | Native fill/default starts stay compact without a drifting parallel buffer or recovered tensor pattern |
| Input initialization explicitly selects eval-owned `RequireRuntimeDriver` (the default), `HostDrivenStart`, or the FMI-packaging-only `FmiExportDefaults`. A binding precedes a start in every mode. An unbound input in the host mode evaluates its actual checked start through the same numeric source owner; absent a start it still requires every actual runtime value. Structural state-selection trials and stored runtime values consume the same policy, while their override map contains only actual caller parameter/input data. Finite actual overrides take precedence per scalar without replacing unchanged source spans; missing values and nonfinite starts/overrides refuse. Native/GPU preparation selects host mode through one correlated adapter; normal FMI packaging explicitly selects export defaults through that same adapter; headless simulation stays strict. No mode rewrites starts into bindings or recovers runs from a seed map | NumericEvaluator input construction, phase-solve state selection/runtime-value construction, sim host preparation | MLS §4.4.2.1 keeps binding equations distinct from the §4.9.6 start attribute; pre-write host defaults cannot supply a missing headless runtime driver |
| FMI export default issuance retains the source declaration-start facet unchanged and selects actual supplied values, otherwise the selected binding/start, otherwise a bounds-aware MLS type fallback. Exact typed source runs, actual supplied-origin intervals and exact bound witnesses are retained before numerical projection. Real fallback may have absent unbounded endpoints but its chosen value must be finite; infeasible bounds refuse. Explicit infinite metadata bounds remain unsupported. Authored values acquire no new blanket bounds check | eval-dae input initialization and phase-solve conversion | Effective component defaults do not invent source authorship |
| The existing checked FMI inventory retains each effective default with its original declaration identity, kind, extent, storage agreement and complete origin partition. Fresh construction and current evaluation-owned replay use the same checks; source/fallback witnesses cannot detach from values. Publication completeness is derived from actual input initialization authority and original storage agreement, never P bits or a caller flag. Export refinements require completeness; ordinary host components may remain valid without it | FmiComponent constructor and deployment admission | One checked authority supplies XML, cells, numeric initialization and Reset |
| `FmiComponent::construct` consumes one checked `SolveModel` into a private `Arc` and matching phase-solve FMI inputs satisfying SPEC_0044 §8 | `rumoca-ir-solve::fmi` | Metadata cannot detach from its executable kernel or duplicate FMI semantic ownership |
| Source identity, tensor extent, scalar names, storage role/type/run, attributes, state count, and value-reference bounds are checked before construction returns | `FmiComponent::construct` | FMI adapters receive no malformed parallel metadata |
| Declaration name and checked dimensions own scalar naming; scalar names are derived lazily through the same neutral name/shape renderer used by DAE. Numeric attributes retain source-issued repetition/literal runs through `SolveInitialValues`, with exact logical extent checked against the declaration and storage run. Neither construction nor current component wire expands broadcasts or stores one scalar name per slot. FMI 2 derives scalar names/values at its XML boundary; FMI 3 projects the same checked values according to its attribute rules. Repetition comes from evaluation/broadcast ownership, never equality-based recovery from dense values | FMI metadata construction and version projections | Declaration-sized metadata retains scalar identity, source order, exact value bits, and all extent checks |
| `SolveLayout::input_scalar_names` is a name table holding one name and shape per input declaration (`SolveScalarNames`); scalar `k` is named by the shared name/shape renderer, `position` inverts it, and the wire form lists declarations, never one string per scalar | `rumoca-ir-solve` layout, `rumoca-phase-solve` layout construction | An image-sized input costs one entry, not one string per pixel |
| An entry records a declared `input`/`output` prefix (SPEC_0040 DAE-C24) only where it differs from the exported causality; a declared causality equal to the exported one is rejected (`RedundantDeclaredCausality`) | `FmiComponent::construct` | The SPEC_0044 §8 annotation renders only where it adds information |
| The checked component is non-cloneable. Runtime borrows its checked views; codegen may consume it only into a nonconstructible checked codegen view that retains the FMI metadata and clones the read-only `Arc<SolveModel>` solely for `'static` lazy-render objects. Rendering obtains `SolveArtifacts` only from that same `SolveModel`, never from a second argument. No API returns an owned bare Solve root or permits foreign metadata pairing | FMI component boundary | One shared kernel supports bounded lazy rendering without an ownership escape or a second aggregate |
| Lazy FMI render objects retain the checked codegen view through one shared inventory handle: every lazy buffer and variable field clones that handle, never the view, so an issued lazy value holds exactly one reference to the view beyond the caller's | `rumoca-phase-codegen::solve_lazy::fmi` | Reference ownership is fixed when the lazy value is constructed, independent of how many buffers or variables a template reads |
| FMI 2 scalar variables and FMI 3 tensor variables derive from the same checked storage runs | Version adapters | Version projection cannot repeat equation lowering |
| The consuming `FmiCCodegenView` contains either the event-free proof or the parameter-assertion proof of SPEC_0044 ME-EVENT-003, plus the ordered parameter-initialization proof of ME-PARAM-001. It admits only the implemented Real public ABI. Whole-inventory encoding requires those proofs; pure-call owners and initial values come from the same retained model. The unrestricted correlated view remains non-serializable | FMI C profile construction | Assertion support cannot bypass admission, pair foreign bodies or initial values with metadata, or discard validation |
| Unsupported source types fail in `rumoca-phase-solve::fmi` before target rendering | FMI lowering | No plausible default representation |
| `FmiComponent::construct` attaches `RootLocationPlan::STANDARD`; its `RootBracket` opens with the schedule `tolerance * 2^(n + minmax_slack)`, `n` the bisection count, halves it per evaluation, and admits only trials within the minmax radius of the midpoint, so the evaluation bound is fixed when the bracket is constructed; its tie-break window `min(low + tolerance, upper)` is a function of the located bracket alone (SPEC_0040 SOLVE-C64) | `rumoca-ir-solve::fmi::root_location` | Termination is proven at construction, not checked per indicator |

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


### 6d. Compact Visible Projection Construction

These rows implement SPEC_0032 §7. The owners are
`SolveLayout::{variable_declarations,variable_storage_runs}`, `VarLayout`,
`SolveVariableMeta`, and the sole evaluation-owned complete model reader; no finalized metadata
Deserialize or additional variable catalog is introduced.

| Construction obligation | Owner/Where | Brief Justification |
|---|---|---|
| Issue each visible selection from the original dense declaration ordinal and its same-root storage run; correlate its original VarLayout allocation at construction | SolveModel construction | Foreign roots cannot transfer identity |
| Check role, type, time domain, exact shape product, span provenance, canonical byte offset, logical extent and selection prefix arithmetic before issuing views | Visible projection insertion | Invalid ranges expose no model |
| Preserve distinct aliases and original declaration order; duplicate selections or changed source-slot equations are typed construction failures | Visible selection insertion | Reporting cannot reorder source |
| Retain one source metadata row per original declaration, referencing existing typed identity rather than rendered names | Declaration source metadata | No parallel identity authority |
| Point views derive their name, span, slot and metadata from that exact selection and allocation; no independent consumer may reconstruct a scalar interpreter | Shared point producer | Reports and execution agree |
| Storage selection has no scalar program inventory; explicit scalar backend admission materializes through the same point producer | Scalar backend boundary | Native preparation stays compact |
| Replay the new current SolveModel schema with the existing sole phase reader, correlate all selections/source references, then derive existing artifacts | Phase model replay | No alternate wire checker |
| Refuse old visible_names/visible_value_rows/variable_meta fields and old model schema; no default or legacy reader fills missing report authority | Current wire reader/writer | One canonical representation |
| Test paired 2/65536 metadata growth, demanded first/middle/last points, aliases/order, foreign role/type/extent/span/byte/source references, unnamed reports and native no-materialization | Focused construction controls | Measure the actual owner |

The existing catalog permits program-only unnamed numerical construction. This
owner adds no global named-coverage requirement; source-backed report
selections must correlate wherever supplied. Arbitrary manually authored
expression observations require an explicit checked expression member of this
same visible owner and their original source span/output obligations, rather
than a parallel legacy scalar block fallback.

### 11. Exact Declaration Initialization Catalog

Moved from SPEC_0036 section "Exact Declaration Initialization"; rows below track that DRAFT section and bind when SPEC_0036 is accepted.

Native typed input initialization MUST borrow this retained source owner through
the original correlated FMI inventory. It MUST preflight every source run against
the issued input descriptors before writing any typed cell. Literal payloads are
borrowed and Repeat payloads initialize actual cells without a dense intermediate
payload. A missing declaration start MUST leave input presence absent until a
legal host write, unless the same component issued a distinct checked FMI
effective instantiation default under SPEC_0043 §8. Source absence remains
Missing; an export default does not author a Modelica start or binding. HostDrivenStart preparation reserves the existing numerical P storage for an absent external input without evaluating a source value or issuing typed presence. RequireRuntimeDriver stays strict, and any reached source initialization that reads that absent input refuses before a value is issued. Storage reservation is not a declaration default or an input driver. Only storage roles and kinds admitted by the issued native input
inventory participate; other declarations cannot manufacture input descriptors.

Real external inputs use the same compact P-interval presence owner as exact
Integer and Boolean inputs. Only original source declaration starts, checked effective FMI instantiation
defaults, or complete checked host input writes mark those intervals. Raw coordinate synchronization
never marks input presence. A Float64 host batch validates every original Real
ExternalInput allocation, extent, finite payload and invocation gate before any
numeric P write or presence change. Getter and invocation preflight require
actual coverage. Original-source reset restores this same presence owner with
the original issued payload. Missing Real starts remain absent in ordinary host
preparation; an exported component restores its issued effective default without
changing the missing source facet.

A declaration's typed start is issued from its checked DAE expression, before
any numerical solver view. Integer and Enumeration payloads retain exact
`i64`; Boolean payloads retain `bool`. Source fill and scalar broadcast retain
Repeat extents. Literal source arrays retain their source element cost.
Metadata and native cells consume the same checked stored initialization owner:
the source start, or the distinct effective default issued for FMI instantiation.
Neither may reconstruct typed values or supplied presence from a Float64 guess.

Exact initialization follows every reached source dependency through the
complete checked source execution owner, including parameter bindings and index
expressions. A call result projection cannot establish that its earlier
assertions executed. Unsupported operations remain typed refusals at their
original reached source position. Parameter and
constant selection uses the canonical binding-before-start policy; an input
uses its binding before the host-driven start. Missing source values remain
absent rather than acquiring a new default.

Function-valued initialization MUST execute the existing checked DAE evaluator's
complete source invocation, including ordered statements, assertions, selected
branches, and folds. Its internal payloads retain source Integer and Boolean
types; Float64 is an explicit numerical view at a caller boundary. The numerical
and exact initialization APIs MUST share that execution owner, expression caches,
argument frames, default-binding selection, and compact run operations. A native
body consumes and produces its declared exact scalar types without an Integer
round trip through Float64. Source Repeat structure survives function results and
parameter bindings; equal computed values do not authorize recoalescing.

Integer numerical attributes MUST refuse a source bound without a checked exact
projection in their deployed numerical storage profile. An Int64 declaration
does not authorize rendering a rounded Float64 min/max as an exact Integer bound.

### 12. Function Assertion Cutover Catalog

Moved from SPEC_0036 section "Source-Ordered Function Assertion Cutover"; rows below track that DRAFT section and bind when SPEC_0036 is accepted.

The canonical typed evaluator and compiled shared C kernel implement this
cutover. ALG-015 remains the source-language
requirement throughout.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| An assertion check executes at its authored statement, before later statements or loop iterations | Typed function lowering | Failed assertions stop algorithms |
| The existing issued predicate-output role owns severity; existing assertion/message metadata remains authoritative | Solve pure-call construction | No parallel assertion registry |
| A branded check capability names an issued assertion output in its exact construction context | TypedProgramBuilder | Numeric ordinals cannot forge assertions |
| A warning records its predicate and continues; a failed error returns a private stopped outcome immediately | Canonical typed evaluator | Preserve severity and first fault |
| Message captures evaluate only for a violated assertion, at that statement | Typed function lowering | Messages cannot introduce inactive faults |
| A stopped outcome contains only initialized predicate/message observations and its exact source invocation | Canonical typed evaluator | No invented value or tangent results |
| Child-to-lifted assertion forwarding is issued with the call, including lazy regions and compact loops | Typed function construction | Preserve nested origin without rediscovery |
| C25 observation consumes a checked projection of that same invocation without demanding missing results | Solve root/action construction | Preserve root location and action reporting |
| Ordinary value calls expose failure; a stopped outcome never becomes a successful complete tuple | Evaluation boundary | No partial success |
| Observation authority binds the immutable call table, exact source programs and ordered actions together | Solve construction | Replaced or foreign programs cannot reuse proof |
| Wire replay reconstructs every capability, forwarding relation and observation projection; old readers/versions fail closed | Solve serialization | No legacy assertion semantics |
| Unimplemented execution adapters explicitly refuse checked assertion-stop programs | Native/codegen adapters | Interpreter repair cannot widen native admission |

The cutover must update SOLVE-C25/C51/C52 and the current Solve schema together
with the coherent source, typed interpreter and C25 consumer implementation.
Schema 76 adds source-positioned checks and checked predicate-only observation
projections to the severity and message associations carried by schema 75.
Nested normal diagnostic outputs contain only canonical inactive cells; the
private invocation observer retains actual failed predicates and captures.
Observation execution depends conservatively on the complete actual-input
footprint, including later failure paths. An opcode-only
change, a public allow flag, a digest substitute for invocation membership,
or an observation returning fabricated result leaves is prohibited.

Direct native/WASM execution retains the existing status-returning three-pointer
typed-call ABI. A separate private observation carrier occupies lifetime-pinned
scratch storage outside ordinary result publication and operation-local reuse.
Its checked capacity derives from the exact finite region and issued call graph:
sequential effects add, selected branches take the maximum, and bounded folds
account for every possible transition and continuation execution. A source-site
count is not an execution bound. Unsupported effects, recursion, or capacity
overflow cause a typed admission refusal; observations are never silently lost.

Each emitted observation retains its exact source owner, operation, selected
region/iteration path, invocation arguments, and initialized typed message bits.
Failed-only message execution precedes publication of that assertion observation;
an earlier message fault wins. Warning observations preserve execution order;
fatal observations stop before any subsequent operation. Child observations are
forwarded through the issued predicate/message association before returning the
child status or reusing its frame. Forwarding preserves the original source and
actual invocation while projecting only the checked parent diagnostic role.
Neither a stopped call nor a numerical fault publishes an ordinary result tuple.

Standalone numerical execution retains its immutable `SolveExecutionSource`;
`event_program` issues a `RetainedEventProgram` containing the original scalar
program, shared call table and ordered action bindings. Its invocation view
reborrows those bindings without repeating the checker. This source grants no
FMI component authority; a correlated FMI root shares its original allocation
when producing the same execution-source view. The implementations are in
`rumoca-ir-solve/src/assertion_observation/execution_source.rs` and
`rumoca-exec-cranelift/src/checked_observation.rs`.

A checked native observation adapter retains the exact sealed invocation by
borrowing it or consuming it into the compiled product. The invocation continues
to borrow its immutable table, source program and ordered actions together;
owning it does not clone or reconstruct authority. Returned observation actions
borrow the retained invocation in that compiled product, so they cannot outlive
it. One retained product may execute repeatedly with each actual input snapshot;
execution does not rebuild the proof or compile another module. Detached carrier
bytes, statuses, digests and descriptive source metadata grant no observation
authority. A guarded invocation resets its own
carrier before input validation; invalid buffer guards grant no carrier-read
authority. FMI remains the transaction owner for warning staging, fatal latching,
logging, commit/rollback and reset without replay. The kernel performs no logger
callback or publication policy. Unqualified adapters continue to refuse admission.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| An opaque retained assertion root shares only the actual FMI runtime view's immutable root allocation; no public `Arc`, owned bare model, cursor or binding constructor escapes | `FmiRuntimeView::retain_assertion_root` | Preserve exact source ownership |
| The retained runtime root also shares that component's original immutable FMI metadata allocation; schedule-only start-time resolution preserves that inventory, and no copied or reconstructed inventory can authorize typed value references | Retained component inputs | Runtime type, causality and storage checks use the actual correlated inventory |
| Canonical start-time resolution precedes invocation issuance; changed roots receive new allocations and cannot reuse the former root's proof | `RetainedAssertionRoot` | Instance schedules bind the actual execution root |
| The sealed retained root records its actual finite canonical resolution coordinate. Read-only access permits consuming adoption to compare the instance configuration against that original anchor without another resolution or a caller-claimed coordinate | Retained instance anchor | A resolved Boolean cannot prove start-time correlation |
| A retained root resolves once; each new instance retains the component's original root before resolving its own start time, and already resolved roots refuse another resolution | Instance root resolution | Absolute schedules cannot silently retain another instance's origin |
| Retained selectors name only that root's actual source blocks and ordered actions | Retained invocation construction | Detached metadata cannot claim program membership |
| The existing invocation checker issues projection bindings once; subsequent views reborrow original actions, text, programs and private bindings without rebuilding proof | Retained invocation views | No repeated validation or source replay |
| Projection membership compares the private issuing binding allocation and original action, never a temporary view address | `owns_projection` | Independently issued proofs remain distinct |
| Nested scalar-region selection explicitly refuses until its complete constructor-issued cursor is proved | Retained source selection | Partial cursor support cannot widen admission |
| Retained native schedule inputs select only the resolved root's construction-issued complete schedule, layout and call table; missing issuance and the original typed schedule refusal remain distinct errors | Retained native inputs | Detached schedules cannot claim source membership |
| A preparation-only retained native capability selects the original root's already-issued complete stateless schedule through that same owner; it permits compilation and pre-initialization storage preparation but grants no execution lease, outcome or assertion-observation authority | Retained native preparation | Compile once without inventing an instance start time |
| Activating prepared native code consumes the instance's canonically resolved root through the existing native schedule issuer and checks the exact issuing model allocation, original metadata allocation and original schedule owner; unresolved, foreign or resolution-changed owners refuse before resolved transport or execution admission | Prepared native activation | Equal or reconstructed source data cannot transfer code authority |
| Shared prepared native code retains its preparation capability, complete compiled module, private arena and non-reentrant entry gate once; each resolved execution product owns its exact schedule inputs and shares only that original code allocation. Resource buffers remain separate. Backend bytes do not independently grant FMI deployment or observation admission | Direct WASM preparation | Compile once without transferring detached proof |
| Preparation and resolved transport share one private resource buffer owner, with the same batch, source-start, codec, presence and checkpoint operations; preparation grants no entry lease, outcome or published outputs | Native pre-initialization storage | Pre-init writes do not grant execution |
| Activation consumes that actual prepared storage and binds through the existing resolved-root capability; it moves the same buffers, typed presence and private resource brand. Refusal returns the same opaque storage without changing its values or ownership | Prepared storage activation | Failed initialization cannot discard accepted pre-init writes |
| Original native reset storage is issued only from prepared code's actual compiler-owned unresolved root defaults and original checked FMI metadata. The same private buffer owner retains one immutable, same-resource original snapshot after exact source initialization and before host writes. Caller coordinates, ordinary snapshots, current values, resolved code and raw storage constructors cannot mint this authority | Native source pristine | Reset defaults cannot be an adopted host mutation |
| Consuming reset restores that original snapshot through the existing resource brand and non-reentrant gate checks, then moves the same storage and code into preparation-only state. Busy, absent original authority and foreign activation refuse without changing or dropping the actual resource. A subsequent activation consumes a freshly once-resolved original root, without retaining the old execution product or granting entry before activation | Native reset to preparation | Reset changes neither typed-store ownership nor compiled-code identity |
| Native resource transport privately shares its exact retained compiled product allocation, owns separate Y/P/scratch/lane storage, and records typed input presence; floating-point start metadata cannot initialize Integer or Boolean lanes | Retained native transport | Preserve actual input bits and resource isolation |
| A typed input batch validates its complete original descriptor range before any write; input presence records actual written intervals and completeness reads the original typed declaration/storage runs without per-coordinate flags | Native input transport | Array extent is payload, not metadata |
| One product-local non-reentrant entry lease guards its shared private arena before buffer access; overlapping or nested entry refuses, and every ordinary return or error releases the lease | Native kernel entry | Separate public buffers do not isolate a shared private arena |
| A transport snapshot captures only actual coordinates, typed cells and presence, carries its private resource brand, and restores only that resource outside an active invocation; it does not reconstruct source or grant success | Native transaction checkpoint | Existing FMI rollback restores the single typed state owner |
| A resolved transport continuation snapshot additionally borrows ownership by privately retaining that actual resolved product allocation. Restore checks Busy and resource brand first, then requires that same product; preparation rejects resolved continuations. A preparation snapshot contains no resolved product or output authority. No public field, replay decoder, digest, epoch or caller-supplied product may manufacture this binding | Native continuation snapshot scope | Reset cannot replay old-anchor output presence into a newly activated resource |
| Transport buffers and an entry lease grant neither kernel-success nor FMI publication authority; a concrete checked execution adapter must retain the lease through actual execution and validation, and existing FMI transactions own publication | Native execution adapter | Arbitrary callbacks or decoded statuses cannot create success |
| A native execution outcome has a private constructor reached only by the retained product's actual instantiated entry, its original status inventory and typed output descriptor validation; it retains the entry lease through consumption, while existing FMI transactions retain publication authority | Retained native execution | No detached status or callback can mint success |
| Native typed reads pair actual cells with original input/output descriptors; unwritten inputs and outputs without a consumed checked outcome refuse, and rollback restores that presence with the cells | Typed native getters | Zero-filled backing is not an accepted value |

The retained capability API lives in
`rumoca-ir-solve/src/assertion_observation/retained.rs`; it changes no Solve wire
data or native execution admission by itself.

### 13. Function Assigned State Catalog

Moved from SPEC_0036 section "Executed-Path Function Assigned State"; rows below track that DRAFT section and bind when SPEC_0036 is accepted.

Partial function state crosses DAE-to-Solve only through a separate, checked
internal assigned-state product. Its canonical typed-program owner retains one
payload type and an absent-or-present state per semantic value/range. Absent
state has no numeric typed value. Conditional regions publish complete tagged
state products independently of their complete ordinary result tuples; nested
states transfer the selected payload and assignedness without source replay.
The source-owned `RequireAssigned` action checks that exact state in lexical
order using the existing checked assertion/effect interface before issuing a
normal payload projection. Its message and provenance derive from the canonical
function value and source use/return. A selected earlier assertion fault wins.

An eager record-field read may require its mutable declared root only when the
existing resolved record-constructor projection inventory identifies that root
and the current-path definedness checker proves the complete root readable.
Field-only coverage, an expression role, or a rendered path prefix cannot issue
a whole-record assigned-state check. Immutable function inputs retain their
existing parameter reads.

An unbound protected record local whose original component-use inventory proves
field writes without any whole write or whole read may retain its fields as
separate locals through the existing resolved record field-local construction.
Only source-written fields acquire definitions; unread siblings need no value.
The original constructor identities, field extents, source order and declared
field defaults remain authoritative. A whole-copy read is a whole read, and a
returned record, a whole declaration binding or a whole-record consumer cannot
use this field-only admission to discard required fields or a prior root value.


That executed-path guard is not an authored Modelica assertion or a model
relation: its typed assigned-value source kind cannot issue a crossing root.
Its checked predicate, source error action, first-fault ordering and payload
projection remain intact. Authored error-level assertions retain their existing
call-scoped root/action contract. A backend that cannot execute the guard through
its normal checked-call fault path continues to refuse admission.

The existing assigned-state constructor derives a private, nonserialized
assigned-read predicate refinement only for an error-level, capture-free check
of its exact issued presence condition. The canonical assertion-flow checker
retains that refinement through its original region and child forwarding
relations. Wire replay reissues it through the same constructors. A Boolean
predicate role, severity, message text, detached flow or public call-site
description cannot establish this source kind. Native admission additionally
requires the actual immutable call table and original model action association;
this descriptive refinement alone cannot excuse model event actions or issue
normal-call execution authority.

A normal typed native call may refine its actual immutable table owner to the
capture-free fatal assigned-read profile when every predicate has that owner's
constructor-derived subtype. It executes the original checked condition at its
source operation, returns the existing source-bound fatal status on absence,
and transfers no result tuple. Child calls preserve their original source fault
and stop before parent result transfer. This value-call refinement creates no
observation carrier, warning publication, or model event-action exemption.
The existing checked observation profile remains distinct.

A model-level native assigned-read refinement borrows the actual canonical
problem, immutable call table and ordered action inventory together. A private
provisional refinement may permit schedule construction only when the original
actions name constructor-derived fatal assigned-read predicates. Before issuing
the schedule, the existing checked invocation projector must cover every
original action from calls retained in the actual native value-stage kernels.
An effect-only, foreign, authored or unbound action refuses. The schedule retains
the existing immutable table lineage; finalized FMI construction reissues it
from the actual model before freezing its root. Preparation checks that same
table owner. Problem-only schedule construction remains strict, and neither
wire flags nor descriptive call sites may bypass event-action refusal.

The finalized problem and its actual immutable call table are available before
mechanical artifacts complete the Solve model. Construction may pass this same
borrowed source pair to the existing native checker before compact refresh and
assignment artifact preparation. It validates call-site membership before
issuance and retains the same complete original action coverage and table
lineage. It neither copies a model/table nor issues execution authority. The
problem-only constructors stay strict. Canonical model replay supplies its
newly reconstructed table before deriving artifacts; a cached schedule belonging
to another table refuses. An alternate chart cannot borrow the primary table
in place of its own source inventory.

The construction-time call membership query uses the same canonical validator
traversal as finalized problem validation, but claims only call-reference
membership while refresh owners are being built. The original finalized-stage
validator still checks the complete shape/refresh contract after issuance;
construction cannot bypass or substitute the membership query for that proof.

The constructor brands internal state handles and checks dominating writes,
region capture/publication types, and complete state publication. Internal
states are excluded from ordinary pure-call argument/result types. Current-wire
replay uses the same constructor and cannot supply projection authority.
Interpreters and compiled consumers preserve the same selected state and check;
a backend unable to implement it refuses admission. Runtime backing storage may
be initialized, but an absent payload is never read or exposed as a numeric
value. Compact tensors retain one state flag per semantic range, not per-lane
operation graphs. No dummy value, inferred Boolean proof, branch replay, or
owner-only cache across invocation environments can replace this owner.

Directional construction transfers the same selected assigned state for primal
and tangent payloads. A real payload's tangent is absent exactly when its primal
is absent; a construction-known zero tangent is a present value only on the
present path. Expanded presence checks retain the original checked predicate
and source provenance and certify every projected primal/tangent state. Native
transport copies presence first and accesses payload backing storage only on a
present path. Neither transport nor differentiation creates missing-value or
assertion-effect authority.

A fold may capture such a state as an immutable internal invariant using the
same checked state-input interface in its transition and continuation regions.
The fold's ordinary carried tuple remains complete; this interface cannot admit
an absent carried numeric value or publish optional carried outputs. Constructor
replay checks the exact capture types and both region interfaces. Selected
iterations consume the captured state without recreating the enclosing branch
or executing a missing payload. Per-iteration local partial state follows the
same ordered `RequireAssigned` contract.

### 14. Linked Native FMI-LS Construction Catalog

Moved from SPEC_0036 section "Linked Native FMI-LS Construction"; rows below track that DRAFT section and bind when SPEC_0036 is accepted.

The trusted compiler/linker boundary for a fixed guest and its direct emitted
model is catalogued in [SPEC_0043 §8](SPEC_0043_CONSTRUCTION_CATALOG.md#8-fmi-component-construction-catalog).

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| A fixed native guest core issues only from the canonical retained-source build, with actual Cargo artifact and complete core validation | `rumoca-exec-wasm` fixed guest build owner | Caller bytes, files, paths, hashes and feature flags cannot establish source identity |
| Final graph construction consumes one private source-correlated library/capsule product and one issued fixed core | `rumoca-sim::native_fmi_ls` | Encoding and model emission retain their existing owners without a dependency cycle |
| Static receiver authority follows the checked complete provider/alias/allocation graph, not an import signature | Trusted whole-graph constructor | A receiver cannot inspect the callee behind its function import |
| Standalone or separately linked guest imports grant no admission; every call retains the existing lease/outcome/fault and typed-cell checks | Private native receiver and existing execution owner | Direct linkage changes platform plumbing, never success or publication semantics |

**DRAFT receiver boundary:** the isolated receiver source remains
unqualified and is not an accepted exception to SPEC_0029. The
receiver cannot inspect an imported callee; altered-provider refusal belongs to
the trusted complete-graph constructor, not a guest-supplied flag or descriptor.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Fixed slot replay alone constructs the receiver's paired component and private static code variant | Private `linked_guest` module | No caller root, bytes, token or callback crosses this constructor. |
| ME initialization and restart check the actual prepared or active code entry before mutation | Existing shared ME resource owner | A platform cfg cannot admit another product. |
| Static calls use actual private work buffers and the existing lease/status/typed-output gate | `NativeInvocationLease` | Direct linkage preserves transaction publication and first fault. |
| Publication token data is copied only from the same paired graph's retained build request | Sim graph issuer and private receiver | Tokens correlate publication, never source or execution authority. |
| The paired issuer generates the sole capsule/token data library internally, with actual dylink allocation and memory-base-relative initialization; the complete graph contains only that data, the consumed linkable model, and the source-built fixed guest | `rumoca-sim::native_fmi_ls` | No independently selectable data/body/provider may acquire a final product. Reserved receiver exports in the fixed core are refused before linking; the pinned linker validates actual provider signatures and relocations without stubs or libdl. |
| The final product retains the exact prepared publication binding and validates the complete component against the fixed build owner's captured canonical WIT and actual platform imports | Same paired issuer and existing WIT checker | Publication identity cannot grant source authority; a structural core graph alone cannot issue an FMI component. |

### 15. Serialization Catalog

Moved from SPEC_0036 section "Serialization"; rows below track that DRAFT section and bind when SPEC_0036 is accepted.

Only the current wire identified by `rumoca_ir_dae::DAE_SCHEMA_VERSION` exists;
older/pre-versioned payloads, adapters, migration readers, and dual writes are
prohibited. The code constant is authoritative.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Current wire records are private and deny unknown fields | DAE serde | Wire is not IR |
| Finite floating-point values retain their exact bits through current-wire serialization and decoding | IR numeric transport | Replay cannot change coefficients or literals |
| Operands travel on the operation that consumes them, never in a positional side table | DAE serde | One reading order |
| Facts a construction operation produces are absent and re-issued by replay | Private wire types | Results cannot be forged |
| Required collections are explicit, including empty | Private wire types | Omission is not ambiguity |
| Decode calls the same provenance-requiring operations | DAE serde | Deserialization is construction |
| Provenance serializes identity/range/origin only | DAE wire | Source text stays canonical |
| IDs project to deterministic wire-local ordinals; arenas to ordered arrays | DAE serde | Process IDs never leak |
| Non-dense key duplicates fail before insertion | DAE serde | Maps cannot hide malformed input |
| Derived counts and indexes are absent from wire | Private wire types | Caches cannot be forged |
| Invariant-bearing children have no fieldwise `Deserialize` | IR serde | Bytes cannot bypass checks |

`flat::Model`, `Dae`, and `SolveProblem` deserialize only private current-version
records through checked construction; children prohibit fieldwise `Deserialize`.

### 16. Other IR Boundaries Catalog

Moved from SPEC_0036 section "Other IR Boundaries"; rows below track that DRAFT section and bind when SPEC_0036 is accepted.

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

### 17. Range-Preserving Ownership And Domains Catalog

Moved from SPEC_0032 section "1. Ownership And Domains"; rows and rules below are normative by reference from that section.

Structured families include source `for` equations, whole-array equations,
slices, comprehensions, boundary ranges, and connection-generated array
equations that are naturally ranged. Domain payloads must not serialize one
entry per scalar iteration except inside an explicitly materialized scalar view.
Stage-specific structured-equation ids remain stage-owned and must be mapped
explicitly when identity crosses phase boundaries.

The runtime variable layout retains one original declaration base slot and its
checked shape/span. It does not retain redundant rendered element-name slots.
Phase lowering binds this run from the actual branded DAE variable identity;
names remain display and external lookup keys, never storage-proof identity.
One demanded scalar query derives its row-major slot through the same checked
shape/base owner. Full scalar-name inventories are lazy derived views requested
only by explicit scalar consumers. Current-wire replay retains the same compact
base/shape input and existing constructor checks; serialization must not recover
runs by compressing a scalar binding table.

Solver Y storage counts derive from checked state, algebraic and output
extents, correlated with the same problem's variable layout. Names, reverse
name maps and per-declaration scalar-index vectors are not stored solver
authority. Solver name/index/range queries borrow this same checked problem;
they cannot pair a layout with foreign declaration storage. Construction checks
exact role-prefix coverage and original declaration shape/span correlation
without enumerating tensor elements. Explicit full reports may materialize a
requested scalar-name view at their boundary, never on every execution tick.
Program-only numerical constructions may retain their existing unnamed Y
coordinates. Correlation checks every named allocation that exists; it does
not invent names or require a global named catalog. A consumer requesting a
complete named report fails with a typed error when a requested allocation
is unnamed. Diagnostic-only fallback labels remain display, not authority.
Removing a superseded wire field changes the current Solve schema; the sole
current-version replay path rejects previous schemas and does not retain an
alternate legacy name-map reader.

A compact structural singleton family retains the exact matching-family product
that the structural incidence/matching check admitted. Its immutable handle
issues scalar row queries through the matching owner's canonical candidate
query; it does not copy a second affine mapping or accept public geometry as a
proof. This proves only the structural incidence and matching relation. Solve
construction must separately bind its output placement and declaration storage
to the actual Solve layout before issuing projection or target coverage.

A declaration-run certificate for that family retains its actual checked DAE
family and domain identities and one declaration base per body. The existing
matching check must establish, for every selected scalar candidate, the same
declaration and the exact scalar displacement from that base. Candidate steps
remain owned by the matching product. Rebranding retained certificates is
restricted to the prepared DAE root that issued them; equal spans, row offsets,
or foreign declarations do not transfer this proof. Such a certificate still
does not establish Solve storage coverage or combine independent singleton
blocks into one nonlinear block.

Solve lowering retains an admitted singleton matching family as one ordered
projection stage. Its per-body declaration storage bindings are checked against
the canonical variable layout and the matching owner's affine scalar bounds,
using the existing tensor output-map contract. Individual singleton blocks are
derived views at an explicit scalar consumer boundary, in source point/body
order. Missing contextual matching certification may use the original scalar
view; an invalid admitted storage binding or arithmetic overflow is a typed
contract failure, not an optimization decline. This local storage binding does
not replace the enclosing Solve source/output/target coverage proof.
The stage owner fixes, at construction, a sorted row-segment table and a
logical-block table, so row-to-target and block-to-stage queries are binary
searches and never walk stages.

A full-declaration paired row-run certificate is issued only from that retained
matching family and its checked declaration storage. It retains the actual DAE
family/domain and proves one body, the exact source row range, zero declaration
scalar base, and the canonical domain-order scalar displacement over the whole
storage run. Equality with the canonical dense tensor output map is an exact
order proof, not an interval-hull or independent row/target-set proof. Its scalar
pairs and singleton stages remain derived views of the matching owner. A source,
row range, declaration, or count mismatch cannot transfer the certificate;
other admitted affine images retain their original scalar boundary until an
exact paired compact certificate is implemented. This local certificate alone
does not authorize enclosing Solve coverage or an emitted program's stores.

The final Solve projection inventory stores an ordered explicit block or a
full-declaration singleton run. A run retains its exact canonical scalar-source
node/program selector, complete output placement, and declaration storage
claim. Root construction and current-wire replay bind those inputs to the
actual compute source and layout, and reuse the existing native ranged
assignment isolator and dependency checker to establish the correlated
row/target relation. Independent row and target sets, enclosing interval bounds,
equal source bodies, and spans cannot establish that relation. An unsupported
isolator retains the existing explicit representation; an invalid admitted
source, placement, or storage claim is refused.

For a fused scalar program, that binding selects one complete original ranged
store through the program's checked output span and compute-source cursor.
Native issuance consumes all of that program's paired store targets together:
the existing segmented isolator checks independence over the complete owned
target inventory, preserves the original prefix once, and retains source store
order through compact value-output placements. Different declaration order
does not permit exchanging store/target pairs or duplicating the shared prefix.

Implicit targets derived from a ranged projection share that projection's
immutable owner, rather than retaining a second per-row map. Their wire marker
resolves against the actual decoded projection inventory before contextual root
construction. Legacy scalar blocks and target slices are explicitly requested
derived views and are never serialized. Normal lowering, tearing repair,
coverage construction, and native preparation consume the ordered stages
without requesting those views. A ranged stage remains the original sequence
of independent singleton SCCs; it neither authorizes a coupled Newton block nor
permits repeating the whole-family value kernel once per scalar output.

A stateless root containing admitted declaration runs retains a compact
continuous refresh recipe after checking its actual paired source/store and
root target inventory. The existing complete native derivation independently
checks derived outputs, typed inputs, and complete work-target coverage, and
retains either its executable receipt or exact refusal on that same owner.
A backend refusal does not reject otherwise computable Modelica or erase the
canonical compact source owner. This recipe conservatively selects the complete
source program for each consumer; it does not fabricate scalar row catalogs,
selections, or simultaneous-block indices. An admitted native schedule executes
the shared fused prefix and stores once in original source order. Scalar staged
adapters explicitly refuse this recipe rather than treating its absent scalar
catalog as an empty successful refresh; an actual scalar executor requests its
derived boundary view explicitly. Neither an unsupported adapter nor wire
replay silently constructs canonical per-output catalogs. Wire data carries
recipe inputs, never an admission flag: contextual replay binds the decoded
canonical projection and reissues native admission or refusal before root use.

A checked stateless assignment decomposition may own continuous structural
artifacts through that same source/projection receipt instead of per-singleton
Newton structures or JVP programs. Admission requires the complete checked
assignment decomposition, no states or retained derivative/manifold obligations,
and no genuine explicit coupled block;
initialization artifacts retain their independent obligations. This capability
retains the exact immutable scalar-program owners and canonical projection
owner. Equal replayed bodies, cardinalities, or source spans cannot substitute
for those owners. Scalar Newton/JVP consumers refuse this capability before
materializing their preparation; absence of a JVP program is not a derivative
proof. Wire reconstruction reissues the capability from the actual decoded
source owner and checked refresh disposition. Native-refused and genuine
coupled systems retain their existing structural-artifact construction.

Solve's matching-query owner retains an admitted matched family once alongside
explicit matches outside that family. A demanded row uses the same issued
`matched_declaration` recipe; algebraic-range eligibility consumes that recipe's
checked declaration roles. It does not recover a correspondence from equal
intervals or retain a second per-point matching map. This does not remove the
structural producer's existing scalar matching inventory or its proof scan.

The algebraic source-definition index retains such a certified full-declaration
family once, including its actual body/domain/scalar-view recipe. A demanded
scalar definition derives its domain tuple through the canonical domain ordinal
query; unused points create neither index entries nor tuple copies. Explicit
and derivative definitions retain their original source and duplicate-definition
checks. Missing, non-full, or unsupported paired certification retains the
original scalar indexing boundary without guessing an inverse affine map.

A checked borrowed Solve compute-source view runs the existing local node-shape
and output-cursor checks before exposing any placement callback. Placements
retain their actual node and represent contiguous ranges, borrowed explicit
indices, or the canonical affine domain/output map, without enumerating domain
points. Materialized scalar inventories derive from this same traversal and
cursor owner. The view borrows the actual aggregate; it is not an independently
transportable whole-source proof and certifies neither enclosing layout nor
root output/target coverage. Retained native inventories bind these views
through their exact immutable Solve root, not copied nodes or equal geometry.
An early-stopping consumer cannot bypass the view's full local preflight.

Scalar program output placement has one immutable owner: dense local placement,
ordered per-program ranges (including genuinely explicit program entries), or
an explicit stored-output sequence. Construction checks each program entry's
cardinality against its source stores and checks range endpoints and aggregate
counts before freezing. Duplicate outputs retain the earliest source-store
lookup; tangent programs retain their local output identities. A legacy slice
is only an on-demand derived view and grants no additional placement authority.
Count, cursor, ordinal, ranged-store, and compact source queries do not construct
that slice. The current Solve wire carries the canonical placement input and
replays the same constructor; caches and proof indexes are not serialized and
superseded wire fields are refused. Whole-family lowering appends the actual
issued program's checked range rather than enumerating its output points.

An event-generating relation inside a structured family is itself a compact
family over the same checked domain. DAE owns continuously monitored state
relations as compact root families, and Solve derives one root program for each
domain point in canonical order. A structured event kind without its own
compact owner MUST be rejected at the exact relation; lowering MUST NOT retain
the conditional value while silently omitting its event boundary.

Instantiation may compact an array of structured components: it resolves one
template domain point and derives the others by reindexing instance paths
(`rumoca-phase-instantiate/src/array_expansion/`). This is an instantiation
optimization, not an ownership change — Instance IR's authoritative
representation of the array is still the per-element `components`/`classes`
entries, and every later phase reads those.

**REQUIRED:** a compacted array must produce byte-identical per-element
entries — including `InstanceId` allocation order, on which flat variable
identity depends (SPEC_0001) — to element-by-element expansion. The declared
extents of every expanded array, compacted or not, are recorded once in
`InstanceOverlay::array_parent_dims`; that is the only array-level record
Instance IR keeps, and the per-element entries remain the array's
representation.

**PROHIBITED:** recording *in Instance IR* that an array was compacted.
Instantiation kept a `component_families` descriptor for a possible future
reader and acquired none; while it existed a diagnostic read it and reported
whether the compiler had compacted the array rather than what the model
declared. A compaction record in the IR is by construction a second owner for
state the per-element entries already own, and it makes the two paths
distinguishable to consumers, which this section forbids. A future phase that
wants the compact domain re-derives it from the declared extents.

This prohibition is on IR content, not on observability as such: a counter or
other out-of-band signal that no consumer can read is still permitted, and one
is needed. Compaction currently has no liveness witness — the differential
tests prove only that the compact and scalar overlays agree, which stays
trivially true if the homogeneity gate silently stops compacting anything, so
the optimization could regress to element-by-element expansion undetected. The
follow-up is a non-IR compaction counter that the instantiate tests can assert
on.

Compaction is refused whenever any per-element rewrite in
`prepare_element_declaration` would fire: a non-`each` `start`, an array-level
binding, or any non-`each` modification that indexes per element — which
includes every nested class modification, even an index-independent one. The
gate is one-sided by design: refusing costs only instantiation time, while
compacting a genuinely per-element array would produce a wrong model.

### 18. Solve Tensor Nodes Catalog

Moved from SPEC_0032 section "4. Solve Tensor Nodes"; rows and rules below are normative by reference from that section.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| `ComputeNode::Map` is elementwise | Solve IR | Pointwise tensor semantics |
| `ComputeNode::AffineStencil` is neighborhood access | Solve IR | Affine offset semantics |
| Runtime tensor access/update stays aggregate-native | Solve IR typed programs | Compact affine indexing instead of extent-sized scalar selection |
| Solve grouping is semantic | `rumoca-phase-solve` | Backends do not redefine IR |
| Scalar fallback uses shared scalarization | `rumoca-eval-solve` | One ordering implementation |
| Structured B.1c uses compact map and target map | Solve IR | Preserve the authoritative discrete family |
| A derivative family's output map is the affine image of its matched slots | `rumoca-phase-solve` | A strided grid interior stays one node |
| A native backend executes a `Map`/`AffineStencil` node as one loop kernel over its compact domain, driven by the same point plan that generates the node's scalar view | `rumoca-eval-solve::AffineKernelPlan`, native backends | Compile cost independent of domain size; the kernel and the view cannot disagree |
| A single-body structured initialization family whose points share one affine program lowers to one `Map` node over its consecutive residual rows; its Jacobian is that node's tensor JVP | `rumoca-phase-solve` initialization lowering | The initialization system stays compact through Solve and the runtime |
| A scalar Jacobian artifact of a block is the scalar view of the block's tensor JVP: forward AD runs once per tensor node's base program | `rumoca-phase-solve` artifacts | Under the solver-y and parameter seed every AD rule is independent of load indices and constant values, so the view equals per-row AD while its cost follows the node, not its domain |

`Map` represents canonical DAE residual families that are elementwise over a
compact domain, including `der(u) = w` after DAE canonicalization. `AffineStencil`
comes from structured DAE domains plus affine operand proofs; Solve lowering
must not rediscover stencils by scanning anonymous scalar rows. Backends may
fuse or split generated kernels as target-local codegen, but the reported
kernel inventory must match the generated work.

A derivative family's output slots are read from the structural matching and
need not be contiguous: the interior of a grid writes a strided sub-range of
its state array, interleaved with edge rows. Lowering fits one affine output
map from the base point and one step per binder and checks it at every point;
a family with no such map keeps its scalar rows. The derivative block proves
that every state slot is written exactly once, not that pieces are adjacent.

An affine tensor node's point plan is constructed once from its domain, output
map, base program, and stride metadata. Construction combines each operation's
strides per binder and proves over the whole domain that every load and output
index lies in `[0, max]` and every strided constant is finite (each bound is
affine or monotone in every binder ordinal, so its extremes lie on the domain
box's corners). The scalar view is the plan evaluated at each point in canonical
order; a native kernel evaluates the same plan in a loop nest with the view's
integer index arithmetic and IEEE constant operations in the same order, so its
results are bitwise identical to the view. The kernel takes its proven maxima as
its length requirements and checks no bound per point. A node whose base program
the kernel path does not support keeps the per-row path, chosen when the block
is compiled.

An initialization family is certified like a structured B.1c family: it
becomes a `Map` node only when one base program with affine load and constant
strides reproduces every point's compiled program exactly. The node writes the
family's rows at the residual positions the per-point rows held, so projection,
incidence, and scalar consumers see the same rows. Any other family keeps its
scalar rows.

Structured B.1c lowering uses `ComputeNode::Map` (or a stronger proven tensor
node) together with a compact affine target map. Discrete row role, pre mode,
observation policy, and clock owner derive from the structured owner. A runtime
that needs scalar programs requests the shared scalar view from
`rumoca-eval-solve`; `rumoca-phase-solve` does not scalarize the owner.

A runtime tensor index, slice, or update is a first-class shape-checked
aggregate operation. Its dimensions, index operands, one-based Modelica index
conversion, row-major affine address, result shape, and out-of-range behavior
are constructed once. Phase lowering and canonical Solve construction do not
enumerate coordinates into scalar registers, repeated operations, or select
chains. A scalar consumer receives a projection of this aggregate owner; it
does not become the owner. Evaluators and native backends execute the compact
operation directly. Text backends may materialize target-language coordinate
operations only while rendering the final checked template view. Native checked-assignment WASM gather and fault rows live in [SPEC_0043 §6](SPEC_0043_CONSTRUCTION_CATALOG.md).

Native typed-input storage and stage binding retain the source-storage runs and
register-version ranges specified by [SOLVE-C69](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md).
Their metadata follows source operations and actual range refinements rather than
external-input scalar cardinality. The original exact Integer and Boolean
conversion, refusal, and ordered fault obligations apply to every represented
cell, including cells read only through an indexed or family operation.

### 19. Compact Visible Projections Catalog

Moved from SPEC_0032 section "7. Compact Visible Projections"; rows and rules below are normative by reference from that section.

The current-only SolveModel schema 4 owns `VisibleProjection` in place of the
three scalar visible inventories. The original producer is
`rumoca-phase-solve/src/model_values.rs::RuntimeVectorBuilder::visible_projections`;
`rumoca-eval-solve::model_wire::deserialize_solve_model` is the sole complete
reader and shares artifact derivation with fresh phase construction. Construction
obligations are in SPEC_0043 §6d; source-equation reporting obligations are in
SPEC_0040's compact visible-projection section.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Replace the three scalar visible inventories with an ordered compact selection of existing original declaration ordinals and checked logical extents | SolveModel visible projection | One source declaration identity |
| Each selection borrows role, value kind, time domain and allocation from that same SolveProblem declaration and storage run; name, shape and span come from its correlated original VarLayout declaration | Solve construction | Names never establish identity |
| Retain reporting attributes once with the original declaration source metadata; do not introduce another name/identity catalog or borrow finalized FMI metadata upward | Solve declaration source metadata | One attribute owner |
| Preserve original declaration order and every visible alias declaration, even when values or numerical slots agree | Visible selection producer | Equality cannot erase reports |
| Storage-backed views read the original checked Y/P range directly; demand one scalar load/store view only at an explicit scalar backend boundary | Shared visible point producer | No canonical scalar programs |
| Complete report requests may materialize names, metadata and values; filtered reports demand only selected points | Reporting/backend boundary | Allocation follows actual demand |
| Native FMI aggregate getters use original checked metadata and native storage without creating visible scalar names, programs or plan entries | Native ME construction/getters | Tensor access remains compact |
| Program-only unnamed numerical roots retain numerical admission; a requested complete named report requires actual report authority or typed refusal | Report construction | No fabricated names or defaults |
| Current SolveModel wire removes all three old fields and their reader/writer together; compact selection and original source metadata are the sole replay inputs | Phase-owned model replay | No legacy shadow inventory |

Source-backed attributes include exact type/variability/time domain, source span,
unit, authored start/min/max/nominal source text, fixed and description. Their
report projection does not author a numeric start or turn Missing into zero.
Structural coordinate and phasor facts retain their existing source-equation
proofs under the SPEC_0040 rows. Genuine authored scalar maps retain
source payload cost; tensor runs are never recovered by comparing values.

The selection stores the existing declaration ordinal, not a name or new
variable ID. During lowering, the original DAE declaration issues the
correlation to its existing VarLayout binding and storage run together. The
declaration source metadata retains that binding selector once; it is a reference
to the existing name/shape owner, not a second name catalog, and cannot prove
storage identity by string equality. Replay checks this reference's allocation
against the selected original storage run. Aliases may share an allocation only
where that original source correlation explicitly issues the shared allocation.

The current transition is SolveModel schema 3 to 4: delete
`visible_names`, `visible_value_rows` and `variable_meta` from `SolveModel`,
`SolveModelWireRef`, `SolveModelWire` and correlation views, replacing them with
the compact selection and original declaration source metadata. Current FMI
component replay continues to call that same phase reader for its nested model;
no second component or model parser is introduced. New finalized owners do not
implement Deserialize.

A manually authored expression member encodes optional metadata explicitly.
Absent metadata remains absent; the member's checked names, expression rows and
source spans grant expression observation only, never original-declaration or
FMI metadata authority. Source-declaration members require their actual source
attributes and the original typed declaration/allocation correlation.

## References

- [SPEC_0036](SPEC_0036_VALID_BY_CONSTRUCTION_IR.md) — owning construction
  rules and the obligations each catalog row serves.
- [SPEC_0007](SPEC_0007_IR_PIPELINE.md) — stage contracts.
- [SPEC_0022](SPEC_0022_MLS_COMPILER_COMPLIANCE.md) — MLS contract index.
