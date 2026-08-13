# Deep branch review: `msl-trace-parity-50`

Date: 2026-07-27  
Base: local `main`  
HEAD reviewed: `60e1d556` (`Improve Modelica compiler and runtime parity`)  
Branch distance: one commit ahead of `main`

## Executive verdict

**Not fully spec-compliant and not yet merge-ready as a claim of 50% OMC trace
parity.**

The branch contains substantial, real improvements. The frontend and structural
pipeline are much stronger after the fixes made during this review, focused
compiler/runtime suites pass, and several silent-wrong-answer paths from the
independent review have been corrected. However:

1. Corrected trace parity is **214/566 = 37.81%**, not 50%. The gap is 69 models
   under the repository's high-or-minor predicate and 96 models under strict
   high agreement.
2. Expandable-connector augmentation and causality deduction are not
   implemented; they now fail closed, and their contract statuses have been
   corrected to `Deferred`.
3. MLS §6.4/§6.5 interface and plug compatibility remain partial. The relevant
   contract statuses have been corrected to `Partial`.
4. FUNC-029 record-cast checking is not implemented. A false constructor rule
   was removed and the contract is now accurately `Deferred`.
5. SPEC_0032's structural benchmark is now capable of checking compactness, but
   the current branch remains red on native Solve ownership/materialization.
6. The complete post-elimination rewrite/factor/reconstruct/scalarize/
   condensation transaction currently lives in `rumoca-sim`, although
   SPEC_0007 assigns structural transformations to the structural phase.
7. The MSL/OMC artifacts predate the reviewed dirty tree. They are useful
   historical evidence, not a fresh result for the fixes in this review.
8. The branch is one commit ahead, but the reviewed worktree is very large:
   `main...HEAD` is 739 files and +81,707 net lines, while the final fixes are
   uncommitted. That is a serious auditability and bisectability problem under
   SPEC_0025/SPEC_0033.

The right merge claim is therefore: **major correctness and verification
hardening, with explicit remaining MLS and tensor-native gaps**. It is not
complete MLS compliance and not 50% trace parity.

## Scope and method

The review fanned out to independent Sol 5.6 xhigh agents across these tracks:

- project-spec and phase-ownership compliance;
- parser, source preservation, and diagnostic behavior;
- lookup, `ScopeId`/`DefId`, imports, inheritance, and redeclaration;
- MLS types, functions, records, arrays, and interface compatibility;
- connections, streams, expandable connectors, events, and clocks;
- DAE construction, index reduction, incidence, and elimination;
- solver/runtime correctness and memory behavior;
- tensor/array-native ownership and backend consumption;
- CI gates, contract metadata, and evidence integrity;
- MSL simulation failures and the path to 50% OMC trace parity.

Candidate findings were not accepted by vote. They were re-read against the
code, reproduced when practical, and then challenged by a separate reviewer.
Several initially plausible findings were rejected or materially narrowed.
Fixes were likewise challenged; two fixes were changed after adversarial review.

The mandatory routing specs were reviewed: SPEC_0007, SPEC_0008, SPEC_0021,
SPEC_0022, SPEC_0025, SPEC_0029, SPEC_0032, and SPEC_0033. MLS checks used the
project's current MLS 3.7 reference, with focused comparison to the stable 3.6
wording where applicable.

## Evidence and provenance

`target/msl/results/msl_quality_current.json` records commit
`a966d9e8e781ff7154d25a7cf4e74eb2b14ac4df`. The committed branch contents were
byte-identical to that artifact's source state when first reviewed, but the
worktree fixes in this review are not represented by it. No final full MSL/OMC
sweep has been run on the dirty tree.

The artifact reports 190 high + 27 minor = 217 agreements. Three high-band
clock entries have stale/unusable OMC provenance and cannot count as current
trace comparisons. The defensible count is therefore:

| Metric | Count | Percent of 566 |
|---|---:|---:|
| High agreement with usable OMC provenance | 187 | 33.04% |
| Minor agreement | 27 | 4.77% |
| **High + minor trace parity** | **214** | **37.81%** |
| 50% target | 283 | 50.00% |
| **Remaining gap** | **69** | 12.19% |
| Strict high-only gap | 96 | 16.96% |

The 217 figure remains a useful description of the artifact's own counters,
but it must not be presented as the current independently validated parity
number.

## Findings and dispositions

### R1 — Expandable connectors fail closed; MLS §9.1.3 augmentation is absent

Severity: high capability/compliance gap  
Status: confirmed, intentionally not papered over

`rumoca-phase-flatten/src/connections/mod.rs` now builds a structured
`ConnectionEndpointIndex` with `IndexMap<ComponentPath, InstanceId>`. It
distinguishes exact declared members from undeclared children under an
expandable owner without suffix checks, dotted-string splitting, or repeated
full-overlay scans.

That fixes the dangerous behavior: an undeclared expandable member is no longer
silently dropped. It now produces EF020 before connection-set construction.
Declared primitive members and empty expandable-bus connections have positive
regressions so the guard does not over-reject those cases.

This is still not the MLS feature. Legal one-sided member synthesis, member
union, and causality deduction are not performed. The former CONN-012 and
CONN-021 tests only proved the compiler rejected the unsupported boundary; they
did not prove the contracts. Their statuses were therefore corrected from
`Implemented` to `Deferred`, their false contract-case mappings were removed,
and the fail-closed tests were retained as ordinary regressions.

Required completion:

1. augment the instance/flat connector member union before set construction;
2. preserve declaration identity and dimensions while synthesizing peers;
3. run §9.1.3 input/output causality deduction over the augmented set;
4. reject duplicate input sources and source-free inputs for their actual MLS
   reasons, rather than because augmentation is unsupported.

### R2 — MLS §6.4/§6.5 compatibility is useful but still partial

Severity: high semantic gap for redeclarations  
Status: confirmed and contract metadata corrected

`rumoca-phase-instantiate/src/plug_compat.rs` now checks substantially more than
the old name-vector comparator:

- specialized class kind;
- operator-record and ExternalObject identity;
- expandable-connector and purity flags;
- primitive base, rank, known dimensions, variability, causality, connection
  prefix, condition presence, and inner/outer;
- function input/output leading-prefix rules and defaults;
- some additional-component default-connectability cases.

It is not an exact implementation of §6.4/§6.5:

- transitively non-replaceable is a shallow local walk and explicitly does not
  follow extends targets;
- enum identity/literal compatibility and several recursively structured
  interfaces remain unproved when type metadata is unresolved;
- extra-element handling is deliberately relaxed for MSL sibling idioms;
- default-connectability is approximated primarily as “extra input has a
  binding,” not the full MLS definition;
- inherited-member collection is depth-limited.

TYPE-023 was already `Deferred`. TYPE-002, TYPE-003, and TYPE-022 were still
advertised as fully implemented; this review corrected them to `Partial` and
removed them from the implemented-contract constant. Their focused cases remain
valuable tests of the implemented subset.

### R3 — Tensor-native gate is honest now, but the branch is still red

Severity: high SPEC_0032 efficiency/ownership gap  
Status: confirmed

The prior independent review was right that a timing-exponent-only ratchet could
pass scalarization. That specific gate defect has been fixed:
`rumoca-tensor-scaling` now checks aggregate DAE row count, compact family
domain, scalar-view ownership, non-materialization, and native Solve
`Map`/`AffineStencil` nodes in addition to timing.

Current focused measurements remain red:

- whole-array first-order scales with an observed exponent near 1.91 in the
  reviewed run and does not produce the required Solve `Map`;
- cascaded first-order has an exponent near 1.04 and is marked
  non-materialized, but it retains a parallel O(N) placeholder-equation
  inventory instead of compact ownership.

Thus “the benchmark passes” and “the compiler is array-native” are both false.
The important improvement is that the gate now says so.

The residual design problem is ownership: several paths retain both a scalar
view and structured-family metadata, and many backends still scalarize tensor
nodes. Native structures exist and have real producers, so the independent
review's stronger claim that `MatMul` is dead/unreachable was rejected. The
ordinary residual-equation path nevertheless misses important opportunities,
and `SparseCandidate`/dense paths still use naive kernels.

### R4 — Structural causal restoration is in the simulation crate

Severity: medium-high project-spec violation  
Status: confirmed; local correctness fix retained, ownership move deferred

The gear regression exposed a real efficiency defect: elimination restored a
single-use generated causal helper, making the projection block 5×5 instead of
4×4. The final fix counts exact semantic occurrences in the authoritative
live, post-elimination DAE and retains:

- source-observable state/algebraic/output owners; or
- generated computations with more than one downstream consumer.

Single-use generated computations stay inlined. Dead substitution inventory is
not counted as a consumer. Repeated occurrences in one live expression still
count because they represent repeated downstream computation. Counting uses
semantic fingerprints plus exact equality, and an `IndexMap` preserves
deterministic candidate order. It does not infer meaning from generated
variable names.

The API fragments are in `rumoca-phase-structural`, but the simulation crate
owns the complete sequence of whole-DAE rewriting, factoring, reconstruction,
scalarization, and condensation in
`rumoca-sim/src/solve_lowering/structural_lowering.rs`. SPEC_0007 assigns those
structural transformations to the structural phase. Moving the complete
transaction behind one structural-phase API is still required.

### R5 — FUNC-029 was a false implementation claim

Severity: high MLS test-integrity issue; medium current capability gap  
Status: fixed metadata and false rejection; true feature deferred

The old project wording said that a target record with a conditional component
cannot have an ordinary constructor. The test used an enclosing non-constant
parameter and was invalid for that reason. It was also not a record cast.

MLS §12.6.1 applies to `R(m)`, where `m` is a model/block/connector instance,
and rejects a conditional component in the source corresponding to a required
field of `R`. It does not ban all ordinary constructors for records that contain
a statically present conditional field.

The review therefore:

- corrected SPEC_0022 and the contract catalog wording;
- changed FUNC-029 from `Implemented` to `Deferred`;
- removed it from the implemented-contract list and false case mapping;
- removed the blanket constructor rejection and reserved EF018 for a future
  real record-cast check;
- added a positive ordinary-constructor regression;
- implemented the independent §5.3.1 check as INST-013/ER130 using `ScopeId`,
  `DefId`, and declaration variability;
- corrected short-class modifier resolution to use separate target and value
  scopes per MLS §4.6.1, including the encapsulated exception.

A true record-cast classifier and lowering/check remain open.

### R6 — Generated-code clock semantics now fail loud at the capability boundary

Severity: potentially high wrong-code risk  
Status: confirmed and fixed for current continuous-only targets

The initial review found `Clock() -> 0`, `previous(x) -> x`, and related
continuous-context stubs in generated-code templates. The accepted fix puts the
semantic guard before template execution:

- generic expression rendering rejects surviving `Clock`, `previous`, `hold`,
  `firstTick`, `noClock`, and sub/super/shift/back-sample calls;
- clocks require an explicit `clocks = true` target capability rather than
  absence being treated as support;
- event feature analysis detects `pre`, `edge`, `change`, and `reinit` in
  expressions;
- CasADi MX/SX and Julia MTK explicitly declare events/runtime-events/clocks
  unsupported.

Templates may retain implementation stubs, but current production admission
and rendering paths cannot silently use them for a model carrying those
semantics. A future target that advertises clock support must provide schedule
lowering rather than relaxing this guard.

### R7 — Diffsol session construction intentionally leaks

Severity: medium runtime efficiency/reliability  
Status: confirmed

`rumoca-solver-diffsol/src/session.rs` uses
`Box::leak(Box::new(problem))`. Each constructed session permanently leaks its
problem. A one-shot CLI may hide this, but servers, optimization loops, repeated
simulations, and notebooks do not. This needs an owned/self-referential-safe
session design or an upstream API that does not require manufacturing a
`'static` borrow.

### R8 — Tensor kernels are selected but not yet optimized

Severity: medium performance  
Status: confirmed and narrowed

`MatMul` and `LinSolve` have real IR producers and prepared consumers. The
independent claim that they were wholly unreachable was rejected. However,
`SmallDense`, `Dense`, and `SparseCandidate` still converge on naive work in
important paths, with no mature sparse factorization/BLAS reuse. All `MatMul`
modes use the same triple loop. Dense and sparse-candidate `LinSolve` lower to
dense augmented Gaussian elimination, and a scalarized n-output solve may
factor the same n×n system independently for every output, yielding O(n^4)
work. This is especially relevant after structural fixes expose larger machine
and MultiBody systems.

### R9 — Verification provenance is incomplete

Severity: high process/evidence  
Status: confirmed

- No fresh full MSL/OMC sweep covers the final dirty tree.
- The historical quality artifact includes three stale clock comparisons.
- The worktree is large and uncommitted.
- The all-features workspace run was stopped at load 47, then failed on ENOSPC
  during a reduced-load rerun. Neither is a valid test result.

No parity increase should be claimed until the final tree is committed and a
fresh `msl-parity --all-omc-targets` result is attached.

### R10 — Structural lowering performs repeated whole-DAE cloning

Severity: medium-high performance/memory  
Status: confirmed

The normal structural funnel performs multiple whole-DAE deep copies around
rewrite, scalarization, factor/reconstruct, and condensation. Array-shaped BLT
handling also clones, scalarizes, and sorts a DAE before discarding the
resulting blocks. This is directly relevant to the 28 timeout/memory misses and
must be profiled before treating them as solver-only failures.

### R11 — Scalarized tensor provenance is insufficiently structured

Severity: medium SPEC_0032/identity gap  
Status: confirmed and partially narrowed

`ProjectionVariableIndex` now uses `ComponentPath` rather than rendered strings
for aggregate lookup. That removes dotted-string key reconstruction. However,
`ScalarProgramBlock` still lacks an explicit parent tensor/family identity plus
index tuple, and not every path segment carries resolved `DefId` identity.
Structured scalar provenance remains required for deterministic
reconstruction, diagnostics, and backend ownership.

### R12 — DAE validity is still enforced too late and too expensively

Severity: high compiler-architecture/performance  
Status: confirmed; DAE-only replacement in progress

The production `Dae` remains a publicly mutable aggregate and still relies on
large Appendix-B/reference validation walks. The first checked replacement
also reproduced part of that mistake: final root assembly rescanned clocks,
events, and ownership, while `DaeFunctionCatalog` walked raw shared function
statements, expressions, and the complete call graph from its constructor.
That would require retaining an optional unchecked/validation mode for speed
and is not valid-by-construction IR.

The accepted design is narrower:

- private fields and role-specific enums exclude invalid alternatives;
- typed catalog insertion performs only the local work needed to create one
  item, normally O(1);
- construction-session-bound capabilities prevent equal numeric ordinals from
  different staged DAE constructions being mixed;
- acyclic dependencies are issued in topological order, so a derived clock can
  refer only to an already-issued clock;
- missing definitions are counted incrementally and finalization is O(1);
- semantic source-tree walks remain in the producing AST/typecheck/phase-dae
  phase and produce proof-bearing inputs;
- final `Dae` assembly is infallible and has no validation feature switch.

The relation/condition issuance cycle and relation truth-type mismatch have
been fixed in the checked foundation. Clock/event/temporal membership checks
have moved to incremental insertion, and the redundant final runtime scan was
removed. Condition, clock, root, and time-event dependencies now share one
construction-only session identity. The linear stage types are non-cloneable,
the identity is erased before finalized catalogs are stored, and a regression
proves that equal clock ordinals from separate sessions cannot mix.
State/discrete target capabilities are now issued directly by the proved
variable catalog rather than rechecking role types after final root
construction. The checked delay input accepts the MLS Real, Integer, and
Boolean source types while rejecting String, and expression composition no
longer rejects valid children merely because generated or inlined provenance
names a different source file.
Rejected duplicate clock associations no longer mutate the builder,
pre-coordinate membership lookup is O(1), and temporal history now has to use
the variable's actual owning clock.

An adversarial construction attack found that the single session identity is
not itself excessive machinery: it is one allocation-free private token per
staged DAE and is erased before finalization. The real remaining defect is
premature erasure. The accepted fix is a top-level session-bound construction
wrapper stripped in O(1) only by the owning stage; finalized expression trees
remain session-free. That wrapper is now implemented through relation,
condition, event, clock, temporal, and equation insertion, with
foreign-session regressions. Equation bundles are created only inside a
consuming stage closure. Per-catalog seal objects are prohibited: catalogs are
deterministic `IndexMap` owners, while the few Rust sealed traits merely close
generic role sets at compile time and carry no runtime state. Runtime identities
still need an explicit reserve-then-define stage. B.1c assignments are now
issued in dependency order without a final graph scan, and reinitializations
now have one nonempty ordered owner per state.

The B.1c adversarial follow-up also split current input `m` reads from solved
definitions. The variable catalog issues read-only input capabilities, refuses
definition targets for inputs, derives the canonical non-input target order
once, and tracks the next required definition incrementally. Missing,
out-of-order, duplicate, and input-target definitions therefore fail before an
equation stage exists; final completion is an O(1) outstanding-count check.

The follow-up catalog-lifecycle review also removed redundant
`CompletedFunctionCatalog` and `CompletedVariableCatalog` relay wrappers.
Function definitions now consume directly into variable reservation, and
variable definitions consume directly into the first DAE stage. Consequently,
there is no public seam at which callers can manually pair a function catalog,
variable catalog, and construction session.

The session boundary now covers every dependent coordinate, not only runtime
objects. Construction expressions accept session-bound parameter, state,
derivative, algebraic, discrete, pre, delay, previous, terminal, condition,
and relation references. Their outer capability also carries a closed
`General`, `Unclocked`, or owning-`ClockId` domain. Composition checks both
facts in O(1), and the authoritative owner strips the wrapper without walking
the finalized expression tree. Equal ordinals from different DAE sessions and
same-session expressions from different clock partitions are both rejected
before catalog lookup.

A second adversarial pass found that the runtime stage order itself was
incorrect. Relation and condition identities must be reserved early, but their
bodies cannot be defined until pre/previous/delay/terminal/time-event/clock-tick
identities exist. It also confirmed that bare runtime IDs remain forgeable,
terminal lacks one session-bound singleton capability, clock lineage is too
coarse for legal sub-then-supersampling of event clocks, clocked factories lack
an active owning clock, and delay typing/variability is incomplete. The specs
were corrected for event-free `abs`/`sign`, strict-positive MLS 3.7 delay time,
monitored-only relation roots, and the split identity/definition stages.

The replacement function IR no longer walks a raw shared tree and is split
within complexity limits, but adversarial construction attacks found
cross-function local capture, detachable branch assignment summaries, and
uninitialized local/output reads. Those proof-breaking defects are being fixed
before broader completeness work on structural value types, semantic array
operators, one-call multi-output invocation, compact for-loops, closed assertion
levels, derived range shapes, and the external-function ABI.

This finding is not closed: the checked representation is not yet the
production `Dae`; its value-type grammar still lacks enumeration, record,
operator-record, and external-object identity; checked function bodies still
need the remaining semantic array/operator and ABI forms; schema 11 is not yet
the only wire format; and the legacy validators/feature flag remain. No
valid-by-construction compliance claim is warranted until that cutover deletes
more production code than it adds. In particular, a wire adapter that projects
back to the mutable schema-10 `Dae` would preserve the defect and is rejected.

The schema adversarial review also rejected implementing schema 11 before the
type gap is closed. A new wire cannot encode enumeration literals through the
legacy `IndexMap<String, i64>` side table or encode records as synthetic array
shapes. The revised design begins with a real ordered type-identity catalog,
then functions and variables. Schema-11 arrays project those canonical
identities to typed wire-local ordinals and decoding remints them through the
same staged constructors. Version 10, pre-versioned payloads, and compatibility
adapters must be deleted in the same production cutover rather than retained
beside the new decoder.

Producer and consumer cutover reviews found three further blockers that pass
adversarial scrutiny:

- initialization activation needed a typed checked condition/event scope so
  source `initial()` cannot survive as an expression. The implemented closed
  activation stores separate initialization and runtime Boolean bodies. An
  adversarial counterexample, `initial() and b`, disproved the first bodyless
  initialization variant and caused that design to be replaced;
- checked equation factories are not yet projected through one active clock,
  so the expression-domain proof used by clock definitions does not currently
  protect clocked equation owners;
- structural passes still mutate legacy partitions and sidecars. Their
  replacement must be a consuming, closure-scoped context borrowed from one
  finalized DAE, with locally checked operations that cannot escape or be
  replayed against another root. A persistent root seal, change catalog, or
  whole-root commit scan would reproduce the construction code smell.

The consumer audit also rejected mechanically copying the legacy
`DaeMetadata` bag. Interface/overconstrained balance accounting is an analysis
product, output causality is orthogonal to Appendix-B role and must not create
a second output partition, relation-root indexes must become `RootId`, and
parameter/B.1c/BLT/root/scalar-view orders must remain distinct typed
orderings. Canonical runtime facts that survive elimination still need typed
owners; rendered variable names are not acceptable keys.

Delay source admission is now a closed local type decision for the currently
representable Real, Integer, and Boolean cases, with String rejected before
identity reservation and again before definition. Enumeration remains blocked
on the type catalog. The timing contract is intentionally not claimed yet:
checked expressions need an incrementally derived variability class, and delay
timing needs closed two-argument positive-parameter and three-argument
runtime-bounded variants. A naked producer Boolean or a constructor tree walk
was rejected.

## Independent compiler-state review: adversarial disposition

`dev/2026-07-27-compiler-state-review.md` was reviewed against the current
working tree. Its historical reproductions remain useful, but its headline
metrics and several source claims predate substantial uncommitted fixes.

| Independent finding | Current disposition | Required action |
|---|---|---|
| DAE codegen silently emits cheapened placeholder residuals | Silent export path fixed; incomplete DAE ownership confirmed | Remove placeholder equations at Flat-to-DAE construction and retain one authoritative structured family |
| Component-modification `redeclare` is ignored | Fixed in source with component-form positive and negative tests | Re-run the focused and MSL gates |
| Index-3 constraints drift | Power rule and internal projection fixed; portable DAE defect confirmed | Structural DAE-to-DAE transformation must retain a complete equivalent constraint formulation without a runtime-only sidecar |
| Enumeration parameter binding/type is lost | Binding ordinal fixed; enum type identity still lost | Preserve stable enumeration type identity in the DAE variable/type catalog |
| Algorithm-section `assert` is dropped | Fixed in source; unsupported forms fail loud | Re-run focused algorithm/event tests |
| Tensor gate measures timing only | Fixed; gate now detects compactness and native operators | Remove the still-detected O(N) placeholder rows |
| Range subscripts, residual MatMul, regular stencil metadata | Fixed in source | Re-run tensor and derivative batteries |
| Connection arrays lack compact authoritative owners | Confirmed | Create structured owners during connection equation generation |
| Temporal semantics remain in Solve fallbacks | Confirmed dead semantic-owner duplication | Delete the downstream `edge`/`change`/`sample`/`previous` fallback branches after the DAE gate is proven |
| Rendered scalar names remain semantic identity | Confirmed | Replace with typed variable plus structured-index coordinates at the earliest producer |
| Parser drops top-level `final`, `break connect`, and descriptions/annotations | Confirmed | Preserve them during parser-to-AST conversion; later phases cannot recover discarded syntax |
| `size(A)`, `promote`, vector subscripts, and `floor`/`ceil` signatures are incomplete/inconsistent | Confirmed | Fix shared typed builtin signatures and AST-to-Flat array typing before lowering |
| Function plug compatibility is textual/partial | Confirmed | Compare one resolved structural function-signature type during instantiation |
| Remote MSL baseline silently falls back, ModelicaTest is narrow, external smoke stops at `t=0` | Confirmed verification gaps | Require explicit offline fallback and add nonzero-time, broader semantic gates |

The review's 217/566 parity and detailed ES010 counts are historical evidence,
not current metrics: the working tree has changed semantically. A fresh full
sweep is required before the branch claims a current trace-parity number.

## Simulation failure analysis

Using corrected parity, there are 352 exclusive misses:

| Exclusive bucket | Count | Primary owner |
|---|---:|---|
| ES010 structural singularity after elimination | 136 | structural |
| Harness-excluded / not attempted | 56 | measurement policy |
| EX002 projection/event-update convergence | 32 | sim/solve |
| Timeout or memory limit | 28 | structural/solve performance |
| EX001 numerical solver failure | 26 | solver/runtime |
| EL0xx unsupported Solve lowering | 25 | solve |
| Trace deviation/non-comparable | 24 | numerics/events/evidence |
| ES014 structural contract failure | 13 | structural |
| Other compile-phase failure | 12 | mixed |
| **Total** | **352** | |

The 24 trace/non-comparable cases split into 15 deviations, five comparator
skips, three stale comparisons, and one successful simulation with no usable
trace. Four of the five skips are stochastic; `Inverse_sine` is skipped because
it has no comparable samples.

EX002 splits into:

| EX002 subtype | Count |
|---|---:|
| initialization/homotopy | 11 |
| event iteration/update | 10 |
| boundary/manifold projection | 7 |
| target isolation | 4 |

These EX002 subtypes were classified from the existing `sim_error` text, not
from structured failure fields. They are useful planning buckets but should be
made machine-readable before they become a gate.

The denominator includes 13 partial models. There are four stochastic models,
not five. These matter when estimating the reachable ceiling but do not justify
changing the denominator without a specified metric change.

### What to focus on next

1. **ES010 rotating-machine structural families.** The exact three-pattern
   union for `fixed.flange.tau`, `spacePhasor`, and
   `electroMagneticConverter` is 51 misses within the rotating-machine family.
   Adding complex re/im as a fourth pattern raises that union to 56 within the
   family and 63 globally. The cohorts overlap, so none of these figures can be
   added or claimed as a proven +69.
2. **EX002 initialization/event/manifold cohorts.** Eleven
   initialization/homotopy and seven boundary/manifold failures are likely more
   coherent than the whole bucket.
3. **EL0xx grouped by unsupported operation.** Twenty-five models are a
   high-signal compiler backlog and usually cheaper to prove than numerical
   failures.
4. **Timeout/memory profiling after structural fixes.** Successful structural
   lowering may expose a different hot path.
5. **Harness exclusions as observability, not instant yield.** The 56 excluded
   models mostly move into another failure bucket when forced. Fix zero-sized
   input classification so metrics are honest, but do not book those models as
   parity wins.
6. **Obtain OMC references for misses.** Current reference generation is biased
   toward models Rumoca already simulates. `--all-omc-targets` is needed to know
   which of the 352 are actually winnable.

## Independent review: adversarial disposition

The independent review
`dev/2026-07-27-compiler-state-review.md` was treated as a hypothesis set.

### Confirmed and fixed

- **D1 placeholder DAE payload:** confirmed. Placeholder/scalar-view ownership
  and DAE target guards were hardened; codegen must fail rather than emit
  dropped family bodies.
- **D2 component-modification redeclare:** confirmed. Source redeclare flags are
  consumed, resolved replacement identity is preserved, explicit unresolved
  replacements fail, and instance-local package constants are tested.
- **D3 index reduction and power spelling:** confirmed. Exponent
  differentiation, manifold projection, demotion balance checks, and the gear
  causal-factor regression were addressed.
- **D4 enum parameter binding:** confirmed and fixed through enum binding/type
  propagation regressions.
- **D5 algorithm assert:** confirmed and fixed through explicit algorithm
  assertion lowering.
- repeated exponentiation truncation, ranges, `end`, `fill`/`size`, dynamic
  `if`, inherited ambiguity, operator-record scope fixtures, function output
  ordering, and stale contract mappings were also corrected.
- short-class modifier values now use the enclosing instance `ScopeId` while
  modifier targets retain the class/base scope, including the MLS §4.6.1
  encapsulated exception.
- clock/event target admission and generic rendering now reject unsupported
  temporal semantics instead of allowing numeric/pass-through stubs.
- the SPEC_0032 number collision was fixed by promoting development process to
  SPEC_0033 and updating the index/routes.

### Confirmed but narrowed

- **Redeclare:** the dominant component package path is fixed, but this is not a
  proof that every nested modifier/redeclare form is complete.
- **Tensor ownership:** the old gate weakness was real and fixed; the current
  compiler still fails the now-honest structural criteria.
- **Temporal operators:** several ownership/gate defects were fixed, but it was
  too broad to say every temporal operator was omitted or silently wrong.
- **Function compatibility:** trailing outputs are legal under the leading
  prefix rule; only interleaving/missing constrained outputs are rejected.
- **Harness exclusions:** real measurement defect, not a direct parity gain.
- **Rotating-machine yield:** fingerprints overlap; additive estimates were
  rejected.

### Rejected

- 217 as the defensible current parity count;
- five stochastic and roughly eighteen partial models;
- 349 misses after correcting stale comparisons;
- “MatMul is entirely unreachable”;
- “redeclare is wholly ignored” after the component path fix;
- “every if-without-else corrupts valid Modelica” (the repro was invalid input);
- “all clock/temporal constructs are omitted”;
- “EF020 is dead or unwired”: equation generation constructs the endpoint index
  and rejects unsupported augmentation before connection-set validation;
- adding package counts or fingerprint cohorts as if disjoint;
- changing declaration order to a sorted map where no canonical sort is
  specified. `IndexMap` is used where first-seen order is semantic; `BTreeMap`
  is retained only where output explicitly requires lexical order.

## Fixes made during this review

### Structured identity and deterministic lookup

- Fixed `UnresolvedMemberSegment` hashing so `HashMap` lookup through
  `Borrow<str>` hashes the same identity as the stored key.
- Added explicit `InheritedMember::{Unique, Ambiguous}` state; lookup stops on
  ambiguity instead of selecting a base or falling through.
- Added INST-013/ER130 using resolved declaration and scope identity.
- Resolved short-class modifier targets and values in distinct `ScopeId`
  environments; removed the former same-spelling `x=x` heuristic and retained
  the encapsulated short-class exception.
- Kept scoped package-member handling on `Reference`/`ComponentPath`; no dotted
  string split or suffix match was introduced.

### Redeclaration and type compatibility

- Wired component source redeclare flags into nested package override handling.
- Deferred forwarding redeclarations to instance-local resolution while keeping
  explicit unresolved replacements fail-loud.
- Validated forwarded replacement targets and preserved replacement modifier
  arguments rather than forwarding only class identity.
- Added positive per-instance package constant tests and negative compatibility
  tests.
- Corrected TYPE-002/003/022 status to `Partial`.

### Arrays, functions, records, and clocks

- Fixed range construction to use indexed values rather than accumulated
  floating error.
- Corrected repeated exponentiation, `end`, shape/rank, enum, record-field,
  `fill`, `size`, `matmul`, conditional equation, and output-order handling.
- Corrected `Clock` constructor typing so dynamic-clock models reach the
  intended DAE diagnostic rather than a Boolean fallback mismatch.
- Made clock support opt-in at target validation, detected event builtins in
  expression analysis, marked continuous-only targets explicitly, and rejected
  surviving clock operators in generic rendering.
- Removed the false FUNC-029 constructor rejection and corrected its status.

### Connections

- Added expandable type identity to instances.
- Added a one-pass ordered endpoint index keyed by `ComponentPath`.
- Reject unsupported augmentation at the correct flatten boundary with EF020.
- Added regressions for undeclared virtual members, declared members, and empty
  expandable buses.
- Corrected CONN-012/021 status to `Deferred`.

### Structural and numerical correctness

- Added checked demotion balance arithmetic and grouped demotion verification.
- Added constrained two-step derivative demotion with a no-repromotion guard
  and an explicit expression-reference proof tying the newly eligible
  successor to the first demotion.
- Reverted an unsupported “group before pair” scheduling change after a second
  reviewer disproved the original gear diagnosis.
- Factored retained causal computations by exact live-DAE semantic occurrence
  count, excluding dead substitution inventory and restoring the 4×4 gear
  block.
- Replaced rendered aggregate-path projection keys with ordered
  `IndexMap<ComponentPath, _>` identity.
- Added manifold/projection and event/runtime hardening identified by the
  independent review.

### Verification integrity

- Corrected stale contract-case names for sample lowering and TYPE-019.
- Made the tensor ratchet check structural ownership, not only timing exponent.
- Repaired spec numbering/indexing and several phase/error references.
- Removed generated root HTML and addressed file-size/history gate failures.

## Adversarial review of fixes

The fixes were challenged in three ways:

1. **Counterexample fixtures.** Positive and negative tests were paired where a
   broad guard could over-reject: declared/undeclared expandable members,
   unique/ambiguous inherited members, legal trailing/illegal interleaved
   outputs, and ordinary constructor/record-cast distinction.
2. **Identity audit.** New semantic matching was checked for rendered-name
   reconstruction, suffix matching, or dotted splitting. The new scoped-member,
   inherited-member, enclosing-reference, and expandable-owner paths use
   `DefId`, `ScopeId`, `Reference`, or `ComponentPath`.
3. **Independent diagnosis challenge.** The first gear reviewer blamed
   pair/group scheduling. Instrumentation showed that path did not commit. A
   second reviewer found the retained single-use generated helper. The
   unsupported ordering change was reverted and the consumer-count fix retained.

One connector fix was also narrowed after review: the first guard rejected any
missing scalar expansion under an expandable connector. The final
`ConnectionEndpointIndex` distinguishes exact declarations first, preventing
false EF020 errors for declared primitive members and empty bus-to-bus
connections.

The ER130 fix was likewise changed after a compiler regression. Treating every
resolved parent declaration as an illegal capture incorrectly rejected the
MLS-sanctioned short-class idiom
`replaceable model Load = Resistor(R = R)`. The final implementation models the
short class's modifier environment directly with parent `ScopeId`, while
long-form class bodies and encapsulated short classes retain their own scope.
This follows MLS §4.6.1 rather than exempting a spelling or declaration kind.

## Verification

Passed:

- `cargo fmt --all -- --check`;
- `git diff --check`;
- checked-DAE focused suite: 103 tests;
- checked-DAE strict clippy with warnings, excessive nesting, argument-count,
  and function/file-length gates denied;
- core phase libraries: 1,353 tests;
- `rumoca` pipeline integration: 92 tests;
- DAE/structural/Solve/runtime libraries: 2,499 tests;
- function contracts: 42 tests;
- connection contracts: 35 tests;
- instantiation contracts: 62 tests before the added parameter regression, then
  the focused INST-013 suite;
- gear loop regression: 4×4 projection and trajectory;
- contract manifest consistency after stale-case corrections.

Workspace-wide all-features testing is **incomplete**. The first run was stopped
after nested/native work drove system load to 47 despite a 16-job Cargo cap. A
six-job rerun failed with ENOSPC while linking. The immediate 125 GB rustc
incremental cache was cleared, recovering 122 GB; no source or MSL result was
removed. The ENOSPC result is environmental, not a test failure, but the gate
has not passed.

## Merge conditions

At minimum:

1. complete a resource-capped workspace test and exact clippy gates;
2. run the now-structural tensor ratchet and retain its red result as an explicit
   blocker or fix it;
3. move structural transformation ownership behind the structural-phase API or
   obtain an explicit SPEC_0007 change;
4. commit/split the work into reviewable units;
5. run a fresh full MSL sweep and all-OMC-target reference generation;
6. update this report with artifact hashes and corrected parity;
7. do not mark partial/deferred MLS contracts implemented merely because the
   compiler fails closed.

Expandable connector synthesis, full §6.4/§6.5 compatibility, and true record
cast checking may be follow-up work only if the merge claim explicitly excludes
those capabilities. They are incompatible with a blanket “MLS compliant”
claim.
