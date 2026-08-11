# RDD2 Tensor-Native Simulation and eFMU Roadmap

Updated: 2026-08-11

## Objective

Complete the RDD2 workflow end to end:

1. preserve array/tensor structure through compiler-owned IR and lower it only
   at the final target boundary;
2. make both optical-flow and GPS waypoint missions correct in pure Modelica
   simulation with Rumoca and comparable to OMC;
3. make warmed mission simulation at least 10x faster than real time;
4. converge GALEC/Production C on the same DAE-owned checked aggregate and
   `SolveAlgorithmBlock` lowering rather than maintaining an independent
   semantic lowering path; and
5. build `cerebri_rdd2`, generate its eFMUs, and demonstrate equivalent
   firmware simulations completing the same missions.

This file is the working progress ledger. Normative design rules remain in the
accepted specs, especially SPEC 0007, 0032, 0033, 0036, and their reference
catalogs. Any new invariant is added to the governing spec before code relies
on it.

## Current completion snapshot

Overall completion is approximately **51%**. This is a weighted acceptance
estimate, not a count of changed lines or checked boxes:

- Phase 1, lazy guarded-fold correctness and ownership: **100%**.
- Phase 2, tensor-native hot-path architecture and performance: **about 84%**.
  The checked model-level table has collapsed 10,374 call occurrences to 326
  issued owners, and all typed scalar, tensor, `Conditional`, `Fold`, and `Map`
  operations now have final-boundary Cranelift lowering. Native/interpreter
  parity, release-mode attribution, further owner optimization, and the 10x
  runtime gate remain open.
- Phase 3, pure-Modelica mission qualification: **about 25%**. Short optical
  traces agree closely with matched OMC DASSL, but neither full 45 s optical
  nor full GPS route is qualified.
- Phase 4, shared GALEC/Production-C and firmware/eFMU qualification:
  **about 5%**. The shared contracts exist, but the end-to-end migration,
  builds, simulations, and trace parity remain open.
- Phase 5, final verification and cleanup: **not started as a closing gate**;
  recurring reviews are active throughout the work.

The correct-by-construction and range-preserving IR requirements are gates on
every percentage above. A speed result obtained by scalarizing before final
emission, inferring equal graphs, bypassing checked construction/wire replay,
or changing mission semantics earns no roadmap credit.

## Roadmap operating cadence

- Update this ledger immediately after every material implementation,
  correctness result, benchmark/profile, discovered blocker, and review.
- Run focused tests and the fixed Tier 1 canary after every capability change;
  record commands, outcomes, and the canary delta here before calling the
  change complete, as required by SPEC 0033.
- Perform a code/design review after every three material changes, after every
  measured runtime shift of at least 10%, after every Solve IR/schema contract
  change, and before advancing to another phase.
- Every review reads the complete relevant diff and checks spec ownership,
  correct-by-construction invariants, tensor-native representation, negative
  tests, generated-code behavior, and before/after profile evidence.
- A failed review reopens the owning roadmap item. Findings are fixed at the
  first divergent layer and recorded in the review log before optimization
  continues.
- Run the final repository/PR review only after the mission, parity,
  performance, and firmware acceptance evidence is complete; it does not
  replace the recurring reviews above.

## Non-negotiable design constraints

- No phase may expand an array, tensor, structured domain, or function fold
  into a repeated scalar expression graph and later attempt to recover it.
- Solve IR retains checked shapes, domains, affine addressing, typed clock
  ownership, and explicit loop-carried state.
- Target-specific scalar instructions, loop unrolling, SIMD selection, and
  target-language coordinate spelling occur only at final emission/rendering.
- Optimizations consume compiler-issued certificates. They do not infer
  semantic ownership from model names, row positions, or post-expansion graph
  patterns.
- Invalid aggregate shapes, ranges, guards, result projections, or reuse
  identities fail checked construction or wire replay before execution.
- DAE owns Modelica semantics once. GALEC is a checked admissible projection of
  that semantic subset, and Production C consumes its checked
  `SolveAlgorithmBlock` refinement as required by GAL-008/024/026/037; neither
  path independently reconstructs shapes, schedules, branch ownership, calls,
  or aggregate dataflow.
- Pure Modelica and generated firmware/eFMU runs must agree on mission
  behavior; completion alone is not parity evidence.

### Construction and verification obligations

- Every aggregate-bearing IR node has one checked constructor that proves its
  shape, domain, range, guard, ownership, and projection invariants; public
  consumers receive the checked form and cannot assemble a partially valid
  aggregate.
- Every shared body has a compiler-issued, collision-checked identity. Runtime
  and backend code may consume that identity but may not reconstruct semantic
  equivalence from cloned operations, scalar coordinates, source names, or
  pointer coincidence.
- Aggregate reads, writes, captures, and results cross phase boundaries as
  compact checked ranges or typed views. A `Vec` of scalar operations is not a
  valid intermediate encoding of a tensor, record, assignment group, or
  function call frame.
- Interpreters, AD, dependency analysis, serialization/wire replay, and every
  final backend consume the same checked owner contract. Backend-specific
  lowering may choose loops, SIMD, or scalar target instructions, but it may
  not change branch, clock, simultaneous-assignment, or state semantics.
- Each new constructor has positive and negative construction tests; each
  owner/reuse contract has collision and replay tests; each final lowering has
  interpreter/native parity coverage. Reviews must identify the exact test or
  proof for every claimed invariant.
- Formal-verification readiness is reviewed as a data-flow property: explicit
  inputs, outputs, ranges, guards, state transitions, and ownership, with no
  hidden semantic inference in caches, templates, or runtime graph scans.

## Acceptance evidence

| Requirement | Evidence required |
|---|---|
| Tensor-native compiler | IR/construction tests proving compact owners survive phase lowering; searches and code review find no new phase-level coordinate enumeration or recollapse pass |
| Correct guarded algorithms | Focused interpreter/Cranelift tests plus generated-symbol/perf evidence that inactive estimator branches do not execute |
| 10x faster than real time | Repeated warmed full-mission benchmark with wall time <= simulated duration / 10, including exact command, binary profile, and machine context |
| Optical-flow mission | Full 45 s Rumoca trace completes with finite outputs and mission criteria; OMC trace comparison records named channels and tolerances |
| GPS mission | Full Rumoca trace completes with finite outputs and mission criteria; OMC trace comparison records named channels and tolerances |
| Firmware/eFMU | Reproducible `cerebri_rdd2` build and eFMU generation commands; firmware simulations complete both routes and compare against the pure-Modelica traces |
| GALEC/Production C convergence | One DAE-owned semantic action/aggregate contract feeds checked GALEC and its `SolveAlgorithmBlock` executable refinement; generated C templates only spell checked operations; structural/code-size/runtime evidence finds no independent scalar lowering or graph recollapse |
| Repository quality | Focused suites, required canary delta, broader workspace/PR gates, clean review findings, and removal of temporary probes |

## Baseline and current measurements

- Model: `Vehicles.Rdd2.Test.WaypointMission`
- Source root: `/home/jgoppert/git/modelica_models`
- Benchmark slice: 0.5 simulated seconds, output `dt=0.005`, RK-like host.
- Earlier warmed runtime: approximately 0.620 s.
- Current best before guarded-fold work: 0.512339476 s.
- Current best after guarded-fold work: 0.465261289 s (1.0747x real time), a
  9.19% wall-time reduction from the preceding best. This remains 9.31x slower
  than the target wall time of 0.05 s for this slice.
- Schema-50 initially regressed to 2.245526220 s best. Final-backend-only
  inlining of fixed folds through eight points reduced that to 1.642736249 s;
  it did not expand Solve IR. Correct semantic arm ownership then exposed two
  unconditional inactive 6x6 GPS/mocap Cholesky folds in the optical-flow
  action schedule. The strict-prefix/conditional-owner fix reduces the warmed
  result to 0.505802826 s best and preserves 101 points at final time 0.5.
- The current post-review result averages `0.397493436 s` and bests
  `0.396933454 s` over three timed runs after one warmup. That is 1.26x real
  time and remains 7.94x slower than the 0.05 s wall-time acceptance
  threshold. It is therefore a correctness and work-elimination win, not
  completion of the performance objective.
- Solve schema 57 cuts the continuous RDD2 calls over to their issued compact
  typed owners and mechanically derives a checked aggregate directional
  relation from the sole primal owner. Interpreter and Cranelift regressions
  cover compact matrix multiplication/reduction, lazy conditional folds,
  singular-value AD guards, and the scalar-program JVP ABI. The reviewed 0.5 s
  waypoint slice averages `0.208543818 s` and bests `0.204968503 s` over three
  timed runs after one warmup (101 points, exact final time), a 48.4% reduction
  from the schema-56 `0.396933454 s` best. A 60-iteration `perf` run averages
  `0.205572696 s`; it identifies primal `LinearAlgebra.solveSPD` owner 30 as
  22.99% self time, followed by typed owner 22 at 5.11% and assignment schedule
  3 at 3.82%. The result is 2.44x real time and remains 4.10x slower than the
  10x-real-time target. Artifacts:
  `/tmp/rdd2-schema57-cutover-bench.json`,
  `/tmp/rdd2-schema57-cutover-runtime.perf.data`, and
  `/tmp/rdd2-schema57-cutover-runtime.perf.report`.
- The 2026-08-11 typed-fold storage review proved that every compact carried
  tuple was copied into a transition frame and back on every domain point,
  while functional tensor updates copied their complete aggregate even when
  checked register liveness proved the input consumed. Final Cranelift storage
  now forwards read-only inputs and final outputs, aliases a functional update
  only at its exact last use, and carries fold tuples through two ownership-
  swapping frames. A focused storage-plan test proves a one-element update
  forwards input to output without mutating the read-only input. The 0.5 s
  waypoint slice improved from `0.5918915 s` best before this review to
  `0.45828918 s` best (`22.6%`) with 101 points and exact final time. Artifact:
  `/tmp/rdd2-fold-frame-release-v1.json`.
- The post-review release rebuild, including the input-only alias restriction
  found during review, averages `0.458440113 s` and bests `0.457804617 s` over
  five warmed iterations. It retains 101 points and exact final time `0.5`, so
  the reviewed lowering has no measurable regression from the pre-review
  artifact. Focused Phase Solve is 85/85; the combined evaluator, Cranelift,
  solver, formatting, and documentation-test command exits successfully.
  Artifact: `/tmp/rdd2-fold-frame-reviewed-release-v1.json`.
- Solve schema 56 now proves exact event-transaction replacement before the
  runtime cutover: every compact target is aligned with its compiler-issued
  legacy scalar/structured/guarded producer, every producer program is covered
  as a complete execution unit, every predicate covers a nonempty set of exact
  legacy action indices, and event-plan ownership points back to the aggregate
  transaction. The runtime evaluates the target/predicate tuple once on the
  first clock pass, commits the complete tuple only after all predicates pass,
  holds it on later passes, and skips only the checked legacy inventory.
  The reviewed release 0.5 s optical slice averages `0.399150423 s` and bests
  `0.397781312 s` over five timed runs after one warmup (101 points, exact final
  time), versus the prior `0.458440113/0.457804617 s` reviewed baseline: a
  13.1% best-time improvement. A post-refactor three-run audit improves the
  average/best slightly to `0.397493436/0.396933454 s`. Dynamic counts are
  `transaction 351=102` and
  `step 342=102` for 101 ticks, down from `step=304`; nested `predict 110=502`
  remains an internal correlation issue. Artifacts:
  `/tmp/rdd2-solve-c56.json`,
  `/tmp/rdd2-c56-event-transaction-call-count.{json,stderr}`, and
  `/tmp/rdd2-c56-event-transaction-runtime.perf.data`.
- The post-cutover runtime-only 30-run profile averages `0.405530348 s` and
  bests `0.404333654 s` with 12,807 samples. Assignment schedules own 65.48%
  inclusive, led by schedule 3 at 29.98%; schedules 1/5/4/2/6 own another
  11.11/7.35/6.98/5.92/2.93%. The largest self symbols are compact fold 65
  (8.85%), typed owner 30 (5.69%), schedule 1 (5.02%), fold 72 (4.84%),
  schedule 5 (4.67%), fold 63 (4.63%), schedule 4 (4.26%), and schedule 3
  (4.13%). This makes the next bottleneck the checked refresh-execution
  schedule boundary, not the Kalman transaction. Artifacts:
  `/tmp/rdd2-c56-event-transaction-runtime.perf.data` and
  `/tmp/rdd2-c56-event-transaction-runtime-{self,inclusive}.txt`.
- A construction-attribution run maps the hottest schedule to the 83-row
  continuous plant refresh owner: 1,315 direct operations become 6,272
  recursive operations because its `bodyAngularVelocityRate[1:3]` row still
  enters a 95-call legacy `FunctionFoldProgram` tree (65 unique fold programs,
  3,348 unique fold operations). The typed model call table already owns
  `LinearAlgebra.solveSPD` and `RigidBody.bodyAngularVelocityRate`; however,
  continuous scalar lowering explicitly rejects typed calls because the
  typed-call JVP relation is not constructed, and `ad.rs` fails closed with
  `typed pure-call JVP owner has not been constructed`. This is the first
  divergent representation layer. SOLVE-C56 now requires directional
  evaluation over the sole tensor-native typed owner and a compiler-issued
  refresh schedule/remainder relation; translating it to another scalar fold
  body or recognizing equal runtime schedules is forbidden. Artifact:
  `/tmp/rdd2-c56-refresh-attribution.{json,stderr}`.
- A source-level matrix-chain experiment changed the continuous log-linear
  controller from `J * K * x` to `J * (K * x)` and replaced
  `J * diagonal(gain) * error` with `J * (gain .* error)`. Best time was
  `0.457612514 s`, indistinguishable from the `0.45828918 s` control. Because
  it changed floating-point association without a reproducible runtime win,
  the model edit was reverted. Artifact:
  `/tmp/rdd2-matvec-association-release-v1.json`.
- SOLVE-C44 now certifies parameter-static refresh owners recursively across
  compact tensors, folds, and lazy conditional regions and keys reuse by the
  exact recursively collected parameter slots. The warmed optical slice is
  0.503993813 s best versus the 0.505802826 s control (about 0.36%), with the
  same 101 points/final time. The dominant guidance and plant schedules are
  correctly still dynamic because their checked programs read continuous
  navigation and rigid-body state; static caching is therefore retained as a
  correctness/architecture capability but is not credited as the next speedup.
- SOLVE-C45 groups typed field/index projections only when they retain the
  exact compiler-issued pure aggregate-call `ExprId`. The RDD2 `outerLoop`
  call now lowers from three programs totaling 1,659 direct operations to one
  706-operation, eight-output program (57.4% less direct IR at that call).
  The warmed optical slice averages 0.458260692 s and bests 0.457198481 s,
  versus the 0.505952444/0.505802826 s pre-group control: a reproducible 9.6%
  runtime reduction with 101 points and exact final time 0.5. This is 1.09x
  real time and still about 9.14x slower than the 0.05 s acceptance boundary.
- SOLVE-C46 removes runtime trace-derived replacement for rows that already
  contain a compiler-owned lazy conditional/fold. Enabling the direct owner
  exposed missing upstream guarded-assignment ownership and temporarily
  regressed the warmed slice from 0.4572 s to about 1.044 s; that intermediate
  state is correctness evidence, not an accepted performance result.
- SOLVE-C47 resolves an exact clock-owned `Always`/owning-`ClockId` branch
  before scalar lowering. Discrete RHS program 4 (native residual program 68)
  fell from 1,272 to 453 direct operations, 276 to 3 `Select`s, and 282 to 9
  parameter loads while retaining all 273 simultaneous outputs. Whole-run
  time remained about 1.04 s because another clocked planning group still
  lowered inactive branch-local folds eagerly.
- SOLVE-C48 now gives each dynamic guarded branch an exact typed activation
  owner before lowering its value. Program 64 has no unguarded direct folds;
  its fold operations are guarded, and the warmed slice recovers to
  0.498069861 s best (30-run perf average 0.504535273 s, best 0.503887378 s).
  Review remains open: 103 scalar target rows rebuild correlated activation
  and value structure, leaving 30 guarded folds, 758 `Select`s, 441 parameter
  loads, and 6,215 direct operations in that program. The next fix must retain
  one compact checked guarded-assignment result/range owner rather than
  optimizing this expanded form.
- SOLVE-C49 schema 51 now constructs one private-field
  `GuardedAssignmentProgram` with mandatory provenance, one shared compact
  lazy program, checked nonempty/nonoverlapping Y/P target ranges, exact output
  coverage, and replayed construction on deserialization. Phase lowering no
  longer enumerates target coordinates and derives the reachable branch suffix
  once. The rejected reusable `DiscreteScalarView` has been removed: refresh,
  dependency, event, and invalidation planning retain one compact prepared
  owner; the interpreter iterates coordinates only at the final storage write,
  coupled Newton only at its unavoidable scalar solver ABI, Cranelift only at
  final native lowering, and a private codegen adapter only while constructing
  the final template render context. Focused suites pass: IR 137/137, Phase
  Solve 73/73, evaluator 122/122, solver 285/285, Diffsol 107/107, RK45 34/34,
  codegen 99/99, and Cranelift 50/50. The first Tier-1 canary attempt stopped
  before model execution because `rumoca-sim` compiled its RK45-only native
  adapter in a Diffsol-only feature graph under `-D dead-code`; the module is
  now feature-gated by `solver-rk45`, and the exact optimized artifact build
  passes. The fixed 20-model Tier-1 canary rerun is green (20/20 AST and flat,
  11/20 Solve compilation, 8/20 configured simulation completion); its focused
  scope correctly skips baseline promotion. The release benchmark and
  post-measurement review remain before C49 is accepted.
- The first schema-51 release benchmark averages 0.351423530 s and bests
  0.350756488 s for the 0.5 s optical slice (101 points, exact final time), a
  23.2% best-time improvement over the schema-50 C49 prototype and 1.425x real
  time. The compact runtime boundary is therefore a real performance win, but
  remains 7.02x slower than the 0.05 s acceptance threshold and triggers the
  required post-shift profile/review.
- The matching OMC mature-backend control proves the model is expensive but
  does not explain Rumoca's remaining gap. OMC frontend/backend/sim-code/template
  work took 1.317/0.785/0.472/3.954 s, then GCC `-Os` spent 2,419.546 s compiling
  the generated 42 MiB function unit; total wall time was 40m32.98s with
  10.1 GiB peak RSS. Its DASSL executable needs 6.573 s for 0.5 s. Its best
  fixed-step GBODE/Runge-Kutta control needs 1.30177 s internally (1.33 s wall),
  of which 0.98510 s is 500 time-event handling and 0.22829 s is simulation.
  Rumoca's 0.350756488 s best is therefore 3.71x faster than OMC's best measured
  fixed-step total and 18.7x faster than its DASSL total, but it is still 7.02x
  slower than Rumoca's own 10x-real-time requirement.
- The first authoritative short-horizon trace comparison exposed a real
  correctness defect hidden by the schema-51 speed number: the clock-owned
  B.1c estimator Boolean owner reached guarded lowering without a branch plan,
  so its default metadata emitted 13 current-value self-holds. After moving
  reachability planning into the sole guarded-owner construction boundary,
  estimator initialization/validity and prediction/optical-flow acceptance
  transition at the same task instants as OMC. The repository comparator's
  mean-channel bounded normalized L1 falls from `2.541e-2` to `3.248e-3` over
  280 common variables and 501 aligned points. The remaining 8% comparator
  score on the two acceptance flags is OMC's duplicate left/right event sample
  versus Rumoca's single right-limit sample; their value transitions are both
  0 at 0 and 1 at 5 ms. Artifacts:
  `/tmp/rdd2-optical-rumoca-vs-omc-fixed.html` and the paired `*.trace.json`
  files.
- Correct execution invalidates the prior `0.350756488 s` performance result:
  it measured a no-op Boolean result path. The corrected hot benchmark has
  compile `1.431 s`, prepare `7.677 s`, average `0.458785 s`, and best
  `0.458356 s` for 0.5 s simulated (1.091x real time), so the remaining gap is
  9.17x to the 0.05 s target. This corrected baseline is authoritative.
- Schema-45 correctness baseline: average 0.464903497 s, best 0.464504963 s
  (1.0764x real time), so the higher-rank transpose refinement introduces no
  regression and slightly improves the prior best within run-to-run noise.
- A runtime-only 30-iteration capture (8 s perf delay) measures generated JIT
  execution rather than preparation: residual programs 60--62 and 98--100
  together own about 18.7% of samples; fold kernels 111, 113, and 120 own
  15.08%. The largest individual symbols are fold 113 (7.20%), assignment
  scheduling (5.57%), residual 126 (5.09%), and residuals 98--100
  (4.33--4.56% each).
- Runtime work per 0.5 s slice: 4555 derivative callbacks, 1154 root
  callbacks, and 500 periodic event updates. An instrumented debug run spends
  172.748 ms in derivative callbacks, 994.808 ms in root callbacks, and
  1673.380 ms in event callbacks (1673.050 ms below event update). These
  absolute debug times are not release benchmarks, but the attribution proves
  that root/event refresh work is the next runtime boundary to inspect.
- The Solve model has 17 continuous root expressions. They are call-scoped
  Modelica assertion predicates and ordinary relation predicates, not Kalman
  nonlinear solves. Two assertion-root programs contain 674 and 700 direct
  operations because the function-call frame was rebuilt for each assertion
  projection. The 99-row figure observed during
  profiling is their algebraic dependency closure, not 99 nonlinear root
  solves.
- Schema-49/50 lowering removes the dominant estimator graph expansion:
  scalar entries into `MultiSensorInvariant.predict` covariance field 5 fall
  from 4,950 to zero and scalar `LinearAlgebra.symmetrize` calls fall from
  4,950 to zero. The selected conditional bodies now contain 28 aggregate
  covariance projections and 83 aggregate `symmetrize` calls.
- Compact conditional result projection reduces scalar `StoreOutput` nodes
  from 24,339 to 3,249 and introduces 2,713 checked `StoreOutputRange` nodes;
  the emitted Solve JSON falls from about 88 MiB to about 85 MiB. This is IR
  evidence only until the release benchmark and trace gate pass.
- SOLVE-C54/schema 53 stores one typed effective time domain per Solve
  declaration. The branded causal-definition traversal certifies a complete
  algebraic/output declaration as event-held only when its exact acyclic
  definition depends solely on static/event coordinates or earlier issued
  event-held declarations. It creates no scalar metadata owner and joins the
  classification with the existing dependency walk instead of adding a second
  graph pass. A fresh RDD2 trace classifies
  `avionics.navigation.accelerationWorldEnu_m_s2[3]` as
  `event-discontinuous`; the repository OMC-DASSL comparison improves from
  `1.190e-3` to `7.009e-4` mean-channel bounded normalized L1. Artifacts:
  `/tmp/rdd2-rumoca-optical-domain.trace.json` and
  `/tmp/rdd2-optical-rumoca-vs-omc-dassl-domain-xtask.html`.

## Work phases

### Phase 1 — Restore and certify lazy guarded folds

- [x] Prove with `perf` that disabled estimator correction kernels execute in
  the optical-flow route.
- [x] Add a normative lazy guarded-fold contract to the Solve and construction
  catalogs.
- [x] Emit control flow only at the final Cranelift boundary while retaining
  compact `FunctionFoldProgram` ownership.
- [x] Include guard polarity and parent-carried versions in nested-fold reuse
  identity.
- [x] Add active/inactive native execution coverage.
- [x] Run focused evaluator, IR, Cranelift, solver, and RK45 suites. IR is
  122/123 with only the pre-existing schema-31 golden stale against schema 45;
  evaluator is 115/115, Cranelift 44/44, Phase Solve 64/64, solver 284/284,
  and RK45 34/34.
- [x] Benchmark and use Solve IR plus uniquely named JIT symbols to prove the
  inactive 6x6 GPS/mocap SPD kernels leave the optical-flow hot path. Program
  17 now contains `GuardedFunctionFold` for the sole direct 6x6 owner and no
  direct unguarded 6x6 owner; warmed runtime falls 69.2% from the inline-eight
  control.

Review checkpoint R1:

- Review the spec row, checked constructor, interpreter behavior, Cranelift
  dominance/reuse safety, false-arm preservation, and active/inactive tests.
- Reject the change if it adds scalar expansion, unchecked branch metadata, or
  a backend-only semantic discrepancy.
- Record findings and resolutions in the review log below.

### Phase 2 — Remove remaining hot-path waste

- [x] Re-record `perf` after Phase 1 with hot iterations dominating preparation.
- [x] Specify and construct one checked `FunctionConditionalProgram` for an
  ordered, correlated multi-target function assignment (SOLVE-C42, schema 46).
- [x] Implement lazy checked evaluation, dependency/sparsity propagation,
  primal-only branch selection in forward AD, and final Cranelift control flow.
- [x] Finish Phase Solve emission with an exact nonzero capture ABI and prove
  the RDD2 estimator conditional is represented once per exact call frame with
  no per-target `Select` graphs or recursively rebuilt prior statements.
- [ ] Replace final-lowering duplication of a checked conditional region in
  every residual row with compiler-owned helper identity. Preserve lazy CFG
  execution and exact call-frame captures; do not infer or recollapse owners
  from expanded operations.
- [x] Preserve one issued in-memory owner across independent continuous
  residual projections and reject one owner id with two checked bodies at
  construction/wire replay. This is an incremental schema-47 prerequisite,
  not completion of the block-level owner table or compact range ABI.
- [x] Replace the current scalar-slot capture/result ABI with checked compact
  ranges so records and tensors cross conditional helper boundaries without
  phase-level coordinate enumeration.
  Schema 48 is in progress: `StoreOutputRange` now carries one checked affine
  register projection through IR, AD, evaluator, sparsity, template planning,
  and retained final lowering. The first Phase Solve cutover correctly failed
  on RDD2 because the preceding `pack_record_field` had already scalar-packed
  the aggregate with `Move`s and AD exposed its irregular alias layout. The
  cutover was withdrawn rather than repacking or recollapsing in AD. Replace
  that earlier packer with a compact register view before enabling ranges for
  real conditional results; compact captures and the owner table also remain.
  Schema 49 replaces aggregate conditional captures with semantic source
  segments and one checked `LoadFunctionConditionalCaptureRange`; a tensor or
  record field contributes its width to the ABI without contributing one
  source-catalog entry or load op per coordinate. Nested record-valued DAE
  conditionals now own one lazy result program, and branch results cross the
  ABI through checked `StoreOutputRange` operations. Schema 50 retains vector
  division and cross products as `TensorBinary(Div)` and `TensorCross` through
  forward AD rather than repacking irregular dual registers.
- [ ] Replace recursively replayed/deep-cloned conditional validation with a
  checked, compact construction certificate whose runtime consumers do not
  rediscover validity; specify the invariant before implementation.
- [x] Fix the schema-46 native null-call crash using a focused conditional with
  nested-fold reproduction and symbol/relocation evidence.
- [x] Fix the interpreter/native estimator regression beginning at the 5 ms
  task tick, proving branch selection, correlated record projection, and
  sequential state ownership are correct before accepting a speed result.
  The 0.5 s native run completes 101/101 points, and focused lazy tensor-arm
  assertion evaluation proves inactive/active branch behavior.
- [ ] Attribute derivative, root, and each typed-clock event cost separately.
- [x] Keep guarded assignment ownership compact through dependency, refresh,
  event, and invalidation planning; restrict coordinate traversal to the final
  storage/native/Newton/text-render target boundary (SOLVE-C49, schema 51).
- [ ] Optimize the largest measured compact owner first.
- [ ] Prefer stronger tensor kernels (`MatMul`, tensor binary, affine stencil,
  compact fold) or final-emission specialization over graph rewriting.
- [ ] Evaluate Cranelift SIMD support for fixed-width tensor kernels at the
  final boundary; retain a checked scalar target lowering when SIMD shape or
  target support is unavailable.
- [ ] Eliminate redundant certified refreshes and copies only with dependency
  and temporal-ownership proof.
- [x] Execute one checked mixed B.1b/B.1c `EventTransactionProgram` per active
  first clock pass, suppress only its exact construction-issued legacy
  producer/action projections, and commit its complete aggregate target tuple
  atomically. Solve IR 161/161 and Phase Solve 86/86 pass; runtime and native
  regressions prove first-pass hold and one aggregate payload invocation.
- [x] Give typed functional updates checked last-use storage forwarding and
  carry compact fold tuples across native loop backedges by swapping owned
  frames instead of copying aggregate payloads. Retain read-only input and
  final-output separation unless the storage proof permits exact aliasing.
- [ ] Move the remaining continuous assignment schedules and their legacy
  `FunctionFoldProgram` helpers onto the same checked typed Solve owner and
  compact tensor-loop lowering used by pure calls. Issue exact refresh-stage
  identity/remainder proofs before sharing work across derivative, root, or
  event coordinates; do not compare the repeated 89-row schedules or fold
  bodies structurally.
- [x] Specify SOLVE-C56: compiler-issued continuous refresh owners, complete
  coordinate invalidation, exact construction-issued coverage/remainder
  relations, construction-time assignment schedules, and directional
  evaluation through the sole typed pure-call owner's mechanically derived
  checked compact relation, without an independently lowered derivative body or
  tensor-coordinate expansion.
- [ ] Construct and wire-replay the SOLVE-C56 owner/relation artifacts; remove
  runtime `Arc::ptr_eq`, reconstructed `RefreshStageIdentity`, pointer-keyed
  schedule identity, and runtime exact-assignment program materialization.
- [ ] Give every reachable typed operation a checked compact directional rule,
  cut continuous scalar lowering over to issued typed calls, and prove
  interpreter/native primal/JVP parity including lazy branch and fold cases.
  The RDD2-reachable no-`Map` vocabulary, continuous cutover, and parity tests
  are complete in schema 57; compact `Map` and the remaining deliberately
  rejected nonlinear aggregate vocabulary keep this item open.
- [ ] Repeat until the full warmed mission is >=10x faster than real time.

Review checkpoint R2 (repeat after every three material performance changes,
and whenever a change moves runtime by >=10%):

- Inspect perf before/after data, generated code size, preparation time, and
  semantic tests.
- Verify the speedup comes from less necessary work or stronger final lowering,
  not relaxed convergence, skipped events, altered task periods, or stale
  values.
- Inspect all new loops for compact-domain ownership and all caches for a
  complete compiler-issued identity.
- Revert changes that increase complexity without a reproducible mission win.

### Phase 3 — Pure Modelica mission qualification

- [ ] Run the complete 45 s optical-flow mission in Rumoca.
- [ ] Run the complete GPS/global waypoint mission in Rumoca.
- [ ] Capture mission success criteria, estimator acceptance flags,
  navigation error, final state, finite-value checks, and runtime.
- [ ] Run matching OMC simulations from the same model sources and parameters.
- [ ] Compare traces with repository tooling; classify and fix the first
  divergent compiler/runtime layer for every mismatch.
- [x] Add a focused regression for the first estimator event issue found: a
  clock-owned conditional B.1c Boolean branch must emit its branch value and
  cannot degenerate into a self-hold. Phase Solve is 74/74.
- [x] Issue compiler-owned step-hold metadata for complete causal
  algebraic/output aliases and verify the repository comparator consumes it
  without a name/value/trace heuristic.

Review checkpoint R3:

- Review model parameters and routes for exact equivalence.
- Review time/event alignment before comparing numeric channels.
- Confirm no model-specific compiler branches, tolerance loosening, or hidden
  exclusions were introduced.
- Record exact commands and artifact locations.

### Phase 4 — `cerebri_rdd2` and eFMU qualification

Detailed implementation ledger: [GALEC to Solve IR Production Code Plan](2026-08-08-galec-to-solve-ir-production-code-plan.md).
Its completed shared-causal-plan work and typed-program foundation are inputs
to this phase; its pending `SolveAlgorithmBlock` and Production-C migration
milestones are mandatory RDD2 gates, not a separate follow-up project.

- [ ] Inspect the current firmware build and eFMU workflow; document exact
  revisions and commands.
- [ ] Close the existing SPEC-0034 implementation gap: Production C must
  consume a checked float32-profile `SolveAlgorithmBlock` derived from the
  checked Algorithm Code package (GAL-008/024), not render an independently
  lowered GALEC statement view directly.
- [ ] Factor DAE-owned aggregate action, call, guard, clock, shape, range, and
  provenance facts into one checked construction path consumed by numerical
  Solve lowering and the GALEC admissible projection. Target profiles may
  refine/reject that owner but may not reconstruct its semantics.
- [ ] Build all RDD2 firmware targets required by both missions.
- [ ] Generate eFMUs through Rumoca's checked GALEC/eFMI path.
- [ ] Audit DAE -> `AlgorithmCodePackage` lowering for premature array/range
  scalarization and replace independent lowering with the shared DAE owner.
  SOLVE-C49 is not credited as a generated-C optimization until the checked
  `SolveAlgorithmBlock` refinement and generated runtime prove the connection.
- [ ] Benchmark generated Production Code C before/after on the RDD2 estimator
  and controller task methods, including code size and worst/average task time.
- [ ] Validate generated artifacts and target capability contracts.
- [ ] Simulate optical-flow and GPS firmware/eFMU compositions faster than
  real time.
- [ ] Compare mission outcomes and selected traces with pure Modelica and OMC.

Review checkpoint R4:

- Review the full DAE -> Algorithm Code -> `SolveAlgorithmBlock` -> C chain
  for single-source semantics and reject duplicate schedule/shape/guard/call
  lowering, scalar target catalogs, and post-expansion graph recovery.
- Review generated Algorithm Code identity, tensor calling conventions,
  persistent/local storage, task periods, interface mappings, and numerical
  profiles.
- Confirm templates only render checked IR and contain no semantic inference.
- Confirm firmware parity covers mission completion, not merely successful
  compilation or startup.

### Phase 5 — Final verification and cleanup

- [ ] Remove temporary timing/debug probes or promote generally useful,
  opt-in profiling facilities with tests and documentation.
- [ ] Run formatting, focused tests, MSL canary, required workspace gates, and
  any PR gates selected by SPEC 0025.
- [ ] Audit every objective requirement against authoritative artifacts.
- [ ] Perform a final code review across all touched crates and specs.
- [ ] Record residual limitations explicitly; do not mark complete with an
  unverified mission, parity route, performance target, or firmware artifact.

Review checkpoint R5:

- Read the complete diff, not only the final commits.
- Check crate ownership and dependency direction against SPEC 0029.
- Check construction and wire-replay invariants against SPEC 0036/0043.
- Check every tensor/fold path against SPEC 0032 and SOLVE-C23/C39/C40.
- Check tests include negative construction cases and behavioral parity.

## Review log

| Date | Checkpoint | Findings | Resolution/evidence |
|---|---|---|---|
| 2026-08-10 | Pre-R1 profiling review | Event scheduling was not the dominant cost. Several inactive 3x3 SPD/Cholesky folds were eagerly called in the optical-flow estimator branch. Existing JIT names merged unrelated kernels. | Added globally unique fold symbols for profiling; specified SOLVE-C40; implementing lazy final-emission control flow with checked guard identity. |
| 2026-08-10 | R1 guarded-fold review (open) | Required activation and register ranges are checked; inactive evaluator/interpreter/Cranelift paths preserve the exact initial tuple; AD preserves the primal guard; native control flow surrounds the compact kernel. Review found deferred guard conditions could contain folds and recursively guard themselves, and inherited guards were redundantly owned by nested folds. | Guard conditions now materialize once outside their own guarded path; suppression propagates through compact-fold builders; an outer fold consumes inherited activation once and nested folds own only locally introduced guards. RDD2 cold compile and short simulation pass. Specs SOLVE-C40 and construction catalog row 128 record these invariants. Focused Cranelift 44/44 and IR construction coverage pass; remaining suites and perf proof keep R1 open. |
| 2026-08-10 | R1/R2 runtime-only profile review | Native execution, not solver/root infrastructure, dominates. Six near-identical 1,064--1,299-op residual owners account for about 18.7%; three compact 9-point estimator folds account for 15.08%. The eager checked-program compile path evaluates ordinary conditional arms before trace specialization, so post-expansion specialization cannot be the long-term optimization. | Keep DAE's checked atomic multi-target conditional as the source owner and specify a lazy, typed Solve conditional region before implementation. Do not add CSE or graph recollapse. Runtime-only artifact: `/tmp/rdd2-schema44-runtime-only.data`. R1 remains open pending phase-solve regressions and inactive-kernel proof. |
| 2026-08-10 | R1 tensor regression review | The linear-solve failure expected six scalar `Move` operations even though lowering now owns the matrix and RHS as two compact `TensorLoad`s. The rank-three transpose failure was real: the compact op conflated untouched trailing tensor width with AD lanes and accepted only rank two. | Revised the linear-solve test to require two exact compact loads, no repacking, and numerical solution `[3,2]`. Added SOLVE-C41, independent checked `element_width`/`lanes`, schema 45, negative construction coverage, and a compiled rank-three native-loop test. Phase Solve is 64/64. |
| 2026-08-10 | R2 conditional-owner implementation review (open) | The first backend pass omitted recursive fold discovery and input-buffer requirements inside lazy regions; AD also needed a construction-level distinction between primal condition outputs and dual result outputs. The first RDD2 cold compile then exposed parent activation indices copied into a region-local empty activation stack. | Recursive fold discovery and all-arm input proofs now traverse the checked owner. AD uses an explicit `StoreOutputMode::{Primal,Derivative,Dual}` and doubles the compact capture/result ABI without differentiating branch predicates. Region-local function frames now own activation base zero. IR/evaluator/Phase Solve checks pass; native focused tests and a successful RDD2 compile/runtime measurement keep this review open. |
| 2026-08-10 | R2 schema-46 integration review (open) | Record-valued conditional targets exposed a scalar-only projection assumption; after correcting the field-major checked result ABI, the real RDD2 program exposed two deeper blockers: native execution jumps through a null address during/after assignment-schedule finalization, and interpreter execution becomes pathological at the estimator's first 5 ms tick. The first emission pass also recompiles preceding `working` definitions inside child regions instead of capturing the exact parent value once. | Record targets now use a deterministic checked field-major width/projection ABI. No runtime result is accepted yet. Add a conditional-with-nested-fold native regression, repair final-symbol ownership at its first divergent layer, and replace recursive prior-statement rebuilding with an exact compiler-issued capture ABI. Artifacts: `/tmp/rdd2-schema46-crash.data` and `/tmp/rdd2-schema46-interp-hang.data`. |
| 2026-08-10 | R2 preparation-profile review (open) | Schema-46 preparation spends most sampled time below `PreparedScalarProgramBlock::new` repeatedly deriving register flow and replaying `FunctionConditionalProgram::validate`. Validation reconstructs, deeply clones, and deeply compares nested conditional/fold regions, so statement nesting creates recursive work before simulation. | Treat validity as construction-owned evidence. Specify and implement a compact certificate/metadata path that is checked once at construction or wire replay and consumed directly by runtime analyses. Do not add memoized graph recollapse or bypass validation. Re-profile preparation and runtime separately after the fix. |
| 2026-08-10 | R2 native crash root-cause review | GDB showed the JIT returning through address zero with a corrupted stack, while a simple conditional-to-fold test passed. The first divergent layer was final Cranelift lowering: a conditional region reused its parent row's register-tape pointer even though region registers have an isolated namespace and may exceed the parent tape's checked size. | `lower_function_conditional_region` now allocates a tape from the exact checked region only when final tensor lowering requires it. A regression uses sparse region registers `r100+`, a selected compact fold, and a parent tensor op that forces a small parent tape. Cranelift is 46/46; the release zero-time RDD2 reproduction exits successfully. No tensor expansion or validator bypass was added. |
| 2026-08-10 | R2 generated-owner profile review (open) | The adjacent 0.004 s native control completes but needs 14.894 s hot time, proving the pathology precedes the 5 ms estimator activation. The perf map contains repeated residual/assignment functions around 3.3 MiB and a 7.8 MiB Jacobian row. The first Phase Solve emission recursively rebuilds preceding estimator `working` definitions in every conditional region instead of capturing the already computed exact value. | Implement SOLVE-C42's exact nonzero capture ABI: compute prior statement owners once in the parent, load only compiler-proven external values with `LoadFunctionConditionalCapture`, and retain one conditional owner per exact call frame. Artifact: `/tmp/rdd2-schema46-runtime-004.data`; JIT map: `/tmp/perf-3622551.map`. |
| 2026-08-10 | R2 exact-capture scheduled review (open) | Exact call-frame captures removed recursive prior-statement rebuilding and the independent native region tape remains correct. Cranelift previously loaded the union of all captures before testing a guard; loads now occur only in the condition or selected result block. Review also found the ABI still enumerates record/tensor values into scalar capture and result slots, and residual batch lowering creates a fresh row context that inlines the same checked region repeatedly. | Phase Solve 66/66 and Cranelift 46/46 pass; formatting and `git diff --check` pass. Preparation fell 47.901 to 6.460 s, backend build 43.830 to 4.303 s, and first-use hot time 14.894 to 1.078 s for the 0.004 s diagnostic. The block-local load refinement was runtime-neutral. Keep R2 open; specify compact range ABI and checked native helper ownership before relying on either invariant. |
| 2026-08-10 | R2 runtime-only exact-capture profile (open) | Warm 0.5 s execution regressed to about 0.67 s versus the schema-45 0.4645 s baseline. Generated native work still dominates: `rumoca_residual_row_0` is about 19.6%, repeated ~337 KiB residual batches own much of the remainder, and checked conditional regions are recursively inlined into each fresh row lowering context. | Do not apply post-expansion CSE. Establish a compiler-issued conditional owner/helper ABI, compile its lazy CFG once, and call it from rows using exact checked captures and compact result ranges. Runtime artifact: `/tmp/rdd2-schema46-capture-runtime-hot.data`; JIT map: `/tmp/perf-3630398.map`. |
| 2026-08-10 | R2 schema-47 issued-owner review (open) | Phase Solve can now give independent residual projections the same exact call-frame owner without rebuilding its regions, and native residual-block lowering evaluates an explicitly owned result once. Review found that pointer identity alone was unsafe for hand-constructed programs and that a forged wire could assign one id to different bodies. It also confirmed this is not yet SOLVE-C43's out-of-line table: bodies and scalar capture/result slots are still serialized inline. | Added explicit nonzero `FunctionConditionalOwnerId`, retained it through AD, limited result reuse to issued owners, and made `ScalarProgramBlock` reject conflicting bodies for one id. A focused two-row phase test proves `Arc::ptr_eq`; a negative IR test proves collision rejection. Phase Solve 66/66, Cranelift 46/46, and Solve IR 125/126 pass, with only the pre-existing schema-31 golden stale against schema 47. Specs now define SOLVE-C43 and its construction row; the milestone remains open until the table/range contract is implemented. |
| 2026-08-10 | R2 schema-47 runtime review (open) | The main 214-row residual now compiles as one owner-aware batch, but the warmed 0.5 s best is 0.674595202 s: only about 2.4% better than the immediate 0.690111729 s control and still 45% slower than schema 45. Runtime perf remains dominated by another 1.76 MiB residual owner (17.55%), repeated ~337 KiB residual batches (about 11.9% for the top two), and estimator fold kernels. | The seven shared continuous conditional owners are real but not the dominant remaining work. Do not broaden cache inference. Give every generated residual program a unique profile id, attribute the hot 1.76 MiB owner to its exact Solve block/source, then move the next optimization to that first semantic owner. Artifact: `/tmp/rdd2-schema47-owner-hot.data`; map: `/tmp/perf-3636268.map`. |
| 2026-08-10 | R2 hot-owner attribution | Unique native symbols and source-program inspection identify `rumoca_residual_program_76_row_0` as discrete RHS program 4: the grouped estimator clock update. Its checked source owns 273 scalar outputs and one conditional; learned specialization emits 547 scalar stores (values plus guard certificates), with 1,327 direct and 88,762 recursive operations across 325 unique compact fold bodies. It owns about 19.7% of runtime samples and is not the visible-value block or a nonlinear root solve. | Fix the first representation boundary: specify a compact checked discrete update/call-frame result owner whose typed ranges map to simultaneous state targets and whose guard certificate is compact. Interpreter and final backends consume that owner directly; do not materialize hundreds of `StoreOutput`s, clone conditional bodies, or recollapse them afterward. Attribution artifacts: `/tmp/rdd2-schema47-owner-attribution.log`, `/tmp/rdd2-schema47-owner-attribution.json`, `/tmp/rdd2-schema47-unique2-hot.data`, and `/tmp/perf-3640165.map`. |
| 2026-08-10 | R2 selected-region input-cache review (rejected) | A final-boundary experiment reused each exact `Y`/`P` load within one selected conditional CFG region. Cranelift 46/46 passed and non-perf best moved only 0.49% (0.674595202 to 0.671299442 s), while the dominant generated function grew 2.18% from `0x1aea40` to `0x1b8080` bytes; runtime-only perf average moved only about 0.29%. Longer SSA live ranges likely increased register pressure/spills. | Removed the experiment. Do not trade loads for a larger scalar SSA graph. Proceed with SOLVE-C43's compact owner/range ABI so phase output and final code become structurally smaller. Evidence: `/tmp/rdd2-schema47-region-load-cache-bench.json`, `/tmp/rdd2-schema47-region-load-cache.data`, `/tmp/rdd2-schema47-region-load-cache-perf.json`, and `/tmp/perf-3648868.map`. |
| 2026-08-10 | R2 schema-48 compact-result review (open) | Phase Solve previously emitted one `StoreOutput` per coordinate for every function-conditional result, despite already owning exact record/tensor starts and widths. The final backend then emitted scalar stack traffic in both directions. The first cutover exposed an earlier violation: `pack_record_field` had already emitted coordinate `Move`s, and forward AD correctly could not prove that the aliased dual outputs were one affine range. | Added checked affine `StoreOutputRange { start, count, stride }`, bumped Solve schema to 48, and carried it through register-flow proof, wire construction, output identity, interpreter/evaluator, sparsity, AD, scalar adapters, template planning, and Cranelift retained tape-copy lowering. Positive strided, negative construction/wire, focused AD, and tensor-backed native parity pass. Withdrawing the Phase cutover keeps RDD2 buildable and avoids an AD repack/recollapse workaround. Phase Solve 66/66, evaluator 116/116, Cranelift 46/46, and Solve IR 127/128 pass; the sole IR failure is the committed schema-31 golden. Next replace the first scalar packer with a compact register view, then enable the range without fallback. |
| 2026-08-10 | R2 schema-48 final-backend review | The target-neutral template plan carried the checked range, but nine textual templates had no `StoreOutputRange` branch and MLIR/WGSL scalar fallback paths counted only scalar stores. Those targets either rejected the new capability or, in MLIR's raw template path, could omit its outputs. | Added final-render-only strided projection to C, Rust, CUDA, JAX, CasADi, FMI 2/3, FMI-LS WASM, and fixed-Rust templates; added explicit MLIR range emission with sparse output mappings and WGSL ordinal projection without modifying the compact IR. A shared regression proves the plan remains one range and C, Rust, MLIR, and WGSL render the correct `r0,r2` projection. The complete codegen library suite passes 99/99. This review finding is closed; schema-48 remains open on first-layer compact packing and real RDD2 cutover. |
| 2026-08-10 | R2 schema-49 compact-capture construction review (open) | A profile found 4,950 scalar entries into `MultiSensorInvariant.predict` covariance field 5 and 4,950 scalar `symmetrize` calls: 22 conditional contexts each re-entered a 225-element tensor. Semantic capture ranges eliminate that catalog/load expansion. Review then found the outer Cranelift row planner still recognized only scalar `StoreOutput`, although nested conditional lowering already handled `StoreOutputRange`; WASM also lacked exhaustive schema-48/49 handling. | Added checked `LoadFunctionConditionalCaptureRange`, compact semantic definition/record-field segments, width-based ABI merging, direct tensor concatenation only when several semantic ranges require one call frame, interleaved AD range mapping, checked evaluator/sparsity/native consumers, and forged empty/out-of-frame wire rejection. Cranelift now projects output ranges only at its final native row-plan boundary; WASM expands result stores only at final emission and explicitly rejects its still-unsupported conditional ABI. Tensor phase lowering, AD, interpreter, and native parity regressions pass. Affected suites: Solve IR 131/132 (only schema-31 golden stale), evaluator 116/116, Phase Solve 68/68, Cranelift 47/47, codegen 99/99, WASM 2/2; formatting and diff checks pass. Keep review open until the RDD2 IR profile proves the 4,950 re-entries collapse and the fixed canary confirms no semantic regression. |
| 2026-08-10 | R2 schema-49 nested-value/result-range review (open) | The first real profile still showed all 4,950 covariance re-entries. DAE inspection proved the first divergence was not the capture catalog: `MultiSensorInvariant.step` carries `predict(...)` inside a record-valued expression `Conditional`, while only multi-target function statement conditionals had lazy lowering. The generic expression packer therefore scalarized the record before capture. Review also found the new helper exceeded SPEC-0021's preferred size and duplicated checked-program construction. | Added lazy aggregate DAE-conditional lowering inside function scope, sharing one checked construction helper with statement conditionals. Refactored region construction/emission/cache responsibilities below the 100-line limit and added a focused array-valued conditional-call regression. Real counters now show covariance field-5 scalar entries 4,950 to zero and scalar `symmetrize` 4,950 to zero. Result ranges cut scalar stores 24,339 to 3,249. Phase Solve is 69/69 at this checkpoint; `cargo clippy` is unavailable in the active Nix toolchain, so no clippy pass is claimed. Fixed canary and release runtime keep the review open. |
| 2026-08-10 | R2 schema-50 AD/tensor-family review (open) | Enabling real result ranges correctly made forward AD reject two non-affine branch outputs. The first was a vector/scalar division emitted as three scalar `Div` graphs; the second was `cross(a,b)` emitted as scalar multiply/subtract operations followed by three `Move`s. Repacking those derived registers would preserve the expansion and violate the tensor-native contract. | Extended checked `TensorBinary` to division with exact interleaved quotient and zero-denominator semantics, and added first-class `TensorCross` with one/two-lane primal/AD semantics. Both remain one owner through evaluator, sparsity, dependency analysis, wire/schema validation, Cranelift, and final WGSL rendering; unsupported WASM lowering fails explicitly. Focused phase and AD tests pass, schema is 50, and `cargo check --workspace --all-targets` passes. A debug 0.01 s RDD2 run now completes past AD/native preparation with 11 points and final time 0.01; its 103.7 s hot time includes debug native/interpreter validation and is not accepted as a performance result. Release benchmark, native parity tests, negative constructor coverage, and canary keep this review open. |
| 2026-08-10 | R1/R2 lazy assertion-owner review | Runtime-only perf showed two 36-point/38-carried solveSPD owners consuming about 53% after small child folds were final-inlined. Call-action guard materialization erased strict outer prefixes; aggregate `FunctionConditional` regions also hoisted assertions without their short-circuit arm path. A first prefix repair correctly panicked because temporarily shortened paths violated saved function-frame activation boundaries. | Materialize each guard under exactly its strict outer prefix while temporarily rebasing and then restoring checked frame boundaries. Give every conditional condition/result/fallback region the exact semantic short-circuit path (prior arms false, selected arm true), retained for hoisted assertions while the compact conditional remains its runtime owner. The assertion fold now owns activation and returns its checked `all_safe=true` identity without traversal when inactive. Focused tensor-arm inactive/active root behavior passes; Phase Solve 72/72 and Cranelift 49/49 pass. RDD2 program 17 has one guarded and zero unguarded direct 6x6 folds. No graph CSE, expansion, recollapse, model-name inference, or backend semantic inference was added. |
| 2026-08-10 | R2 post-laziness performance review (open) | The fixed warmed best is 0.505802826 s versus 1.642736249 s immediately before semantic arm ownership, a 69.2% reduction with identical point count/final time. Fresh delayed perf no longer shows the former 6x6 leaders. Multiple non-unique assignment-schedule symbols now total about 30.3%; residual batch 90 is 8.83%, fold 207 is 7.24%, and residual program 71 is 7.14%. | Accept the semantic fix and bounded final fold specialization. Next issue globally unique assignment-schedule symbols and map the top schedule/fold/residual owners to exact checked Solve blocks before changing code. Runtime artifact: `/tmp/rdd2-schema50-conditional-guards-runtime.data`; flat report: `/tmp/rdd2-schema50-conditional-guards-runtime-flat.txt`; JIT map: `/tmp/perf-3725920.map`. |
| 2026-08-10 | R2 SOLVE-C44 parameter-static certificate review | The prior classifier rejected every tensor/fold/conditional owner, but extending it also exposed that lazy conditional parameter dependencies were absent from cache-key construction and that seed-bearing tensor loads cannot be static. The real RDD2 guidance owner is continuous by model contract, not a missed clock-held cache candidate. | Added normative SOLVE-C44/construction rows, an exhaustive fail-closed recursive classifier, exact conditional-region parameter collection, seed rejection, compact tensor/fold/conditional tests, and bitwise snapshot invalidation coverage. Evaluator 121/121 and solver 284/284 pass. The 0.36% benchmark shift is noise-scale, so no dominant work is skipped or relabeled static. Artifact: `/tmp/rdd2-schema50-parameter-static-refresh.json`. |
| 2026-08-10 | R2 SOLVE-C45 exact aggregate-call projection review | Phase Solve compiled record fields with independent scalar compilers and therefore replayed one source `outerLoop` call three times. Grouping by reconstructed bodies, arguments, names, spans, or post-expansion graphs would violate semantic ownership and be collision-prone. Review also found that assignment isolation initially recognized only scalar `StoreOutput`, which made the new range output incomplete at that consumer. | Added normative SOLVE-C45/construction rows and group only consecutive, pure, typed Field/Index projections rooted at the same exact compiler-issued call `ExprId`; a separate identical call stays separate in the negative identity test. One checked multi-output program carries affine output ranges. Assignment isolation now consumes `StoreOutputRange` through a lazy scalar view at its final scalar target boundary, with no intermediate `Vec` or graph reconstruction. Phase Solve 73/73, evaluator 121/121, Cranelift 49/49, and solver 284/284 pass. RDD2 proof: one `Control.Multirotor.LogLinear.outerLoop` group, 8 outputs, 706 direct ops versus 1,659 before; warmed runtime improves 9.6%. Runtime artifact: `/tmp/rdd2-schema50-call-group-runtime.data`; owner maps: `/tmp/rdd2-schema50-call-group-profile-ir.log` and `/tmp/rdd2-schema50-call-group-kernel-map.log`. |
| 2026-08-10 | R2 SOLVE-C46 direct-owner review | A reference-evaluator trace had become a second lowering authority: it expanded a checked lazy row, observed one execution path, appended scalar guard outputs, and supplied a pruned native replacement. Removing that reconstruction initially regressed 0.4572 s to about 1.044 s because it exposed eager branch-local work elsewhere. | Added normative SOLVE-C46/construction rows and disable trace-derived native replacement whenever the row already owns `FunctionConditional` or `GuardedFunctionFold`; the interpreter may still trace lazily for evaluation. Evaluator 122/122, Cranelift 49/49, and solver 284/284 pass. Keep the direct compiler owner and fix missing ownership upstream; do not accept the intermediate runtime or restore post-expansion recollapse. |
| 2026-08-10 | R2 SOLVE-C47 typed unconditional-branch review | Discrete RHS program 4 was incorrectly paying a hold load plus one constant-true `Select` for every projection even though its schedule already proved the exact owning clock active. The 273-output estimator update was program 68, not a visible-output or nonlinear-root program. | Added normative SOLVE-C47/construction rows and resolve only `Always` or the exact owning `ClockId` before scalar construction. Program 68 now has 453 direct operations, 3 `Select`s, and 9 parameter loads versus 1,272/276/282, with identical output count. The remaining ~1.04 s runtime was traced to program 64's inactive planning folds, so C47 is retained as a structural correction but not credited as a complete speed fix. |
| 2026-08-10 | R2 SOLVE-C48 guarded-value owner review (open) | Exact branch activation was constructed before each `Select`, but the value was lowered outside that semantic context, so nested planning/estimator folds ran even when the assignment branch was inactive. The first implementation guards those folds and restores runtime to about 0.50 s. Review then found two incomplete obligations: compact conditionals still need an explicit guarded execution contract, and 103 scalar targets independently rebuild correlated branch structure, producing 6,215 direct operations and 758 `Select`s in program 64. | Retain the exact optional clock, trigger `ConditionId`, guard `ConditionId`, and trigger-memory slot as one hashable activation owner; reconstruct it only under its strict prefix for deferred actions. Phase Solve 73/73 passes and program 64 contains 30 guarded, zero unguarded direct folds. Do not close C48 or accept the scalar rebuilding. Specify a checked compact guarded-assignment group/result-range owner next, make compact conditionals consume activation explicitly, add focused collision/wire/native tests, then repeat all suites and perf. Runtime artifact: `/tmp/rdd2-schema50-solve-c48-guarded-owner-runtime.data`. |
| 2026-08-10 | R2 SOLVE-C49 compact guarded-assignment prototype review (open) | Aggregate value lowering now happens once per correlated guarded family and produces one lazy conditional plus one compact output range. Phase Solve 73/73, evaluator 122/122, Cranelift 49/49, and solver 284/284 pass. Review found that this is only half of the required contract: `DiscreteRows::push_group` still expands each aggregate target into scalar `DiscreteTarget` metadata; the implementation reuses `FunctionConditionalProgram` although the catalog names a `GuardedAssignmentProgram`; branch reachability/control topology is rediscovered in two lowering modules; and an impossible empty-group path emits a source-free diagnostic. | Keep C49 open. Add a checked Solve-IR guarded update owner with ordered compact source/target ranges, construction and wire-replay rejection for width/overlap/identity errors, and one runtime final adapter. Derive reachable branches and fallback exactly once while constructing the owner, remove duplicate semantic rescans and source-free production diagnostics, then add interpreter/native active/inactive parity and rerun the fixed Tier 1 canary. No scalar target vector or graph recollapse is accepted as the source representation. Runtime artifact: `/tmp/rdd2-schema50-solve-c49-compact-guard-runtime.data`. |
| 2026-08-10 | R2 SOLVE-C49 schema-51 construction/runtime review (open) | The Solve owner is now private, checked, compact, source-backed, and wire-replayed; phase lowering has no coordinate loop, target overlap is proved with interval arithmetic, and active/inactive interpreter plus native execution agree. Review found the shared `DiscreteScalarView` constructs one metadata row and target entry per coordinate before refresh/dependency/runtime planning. That violates SOLVE-C49 even though it does not clone the expression body. | Keep schema 51 unaccepted for performance. Split compact planning from final execution: plans must retain one guarded program plus checked target ranges and range-level policies; only the final interpreter/native write or textual renderer may iterate a range. Add plan-shape and invalidation tests, then rerun evaluator, solver, Diffsol, RK45, codegen, the fixed Tier 1 canary, and review again. Current focused evidence: IR 137/137, Phase Solve 73/73, solver 285/285, Cranelift 50/50; Diffsol is 106/107 with one malformed zero-unknown fixture corrected and full rerun pending. |
| 2026-08-10 | R2 SOLVE-C49 schema-51 compact-planning follow-up (closed) | The prior reusable scalar view crossed the planning boundary and was therefore rejected. The replacement retains one shared checked operation body, exact output count, required inputs/registers, clock policy, and compact target ranges. Refresh closure consumes the compact program directly; no runtime target catalog exists. Range traversal is confined to final storage writes, the coupled-Newton scalar ABI, final Cranelift lowering, and the private final-template render adapter. Review also found an unrelated feature-boundary defect: `rumoca-sim::native_execution` was compiled without its only RK45 consumer in the Diffsol-only MSL graph. | Deleted `DiscreteScalarView` rather than recollapsing it, added direct compact planning and plan-shape coverage, and ran IR 137/137, Phase Solve 73/73, evaluator 122/122, solver 285/285, Diffsol 107/107, RK45 34/34, codegen 99/99, and Cranelift 50/50. Gate the native adapter with `solver-rk45`; the exact `msl-fast` artifact build passes. The fixed Tier-1 canary rerun passes: 20/20 AST/flat, 11/20 Solve, and 8/20 simulation completion, with baseline ratcheting correctly skipped for the focused target file. Release best improves 23.2% to 0.350756488 s with exact point/final-time preservation. C49 is accepted; R2 remains open on the 10x target. |
| 2026-08-10 | R2 schema-51 post-shift performance review (open) | Runtime-only perf confirms the compact guarded owner is no longer a leader. Residual batch 90 is 9.17%, fold 245 is 8.64%, and assignment schedules 4/2/5/3/1 total 22.26%. Schedules 2 and 4 are construction-identical 27-program/100-output guidance closures (1,489 direct, 9,066 recursive operations) but have distinct runtime plan instances; larger schedules repeat the same exact `outerLoop` owner. The 8 s delayed capture includes about 0.65 s of preparation, visible as dependency/Cranelift samples, so those are not runtime optimization targets. Source review also finds `discrete_rows.rs` at 1,020 lines, inside SPEC-0021's warning band. | Do not content-hash/CSE the equal scalar programs or cache by observed graph equality. Specify compiler-issued refresh-stage/body identity plus exact time/state/parameter/discrete invalidation before sharing execution across consumers. Split guarded runtime responsibilities enough to return the touched file below the warning threshold. Runtime artifact: `/tmp/rdd2-schema51-c49-compact-planning-runtime.data`; JIT map: `/tmp/perf-3823559.map`. |
| 2026-08-10 | R3 first Rumoca/OMC trace review (fixed first divergence; numeric review open) | The MSL comparator found five near-unit deviations: estimator/navigation validity and estimator prediction/optical-flow acceptance stayed false in Rumoca. DAE retained the correct `Clock(1)` B.1c values, but structured guarded lowering bypassed `plan_guarded_targets`; default `dynamic_branch_count=0` and `fallback_branch=None` therefore constructed hold-only programs. | Put reachability/fallback planning in `lower_guarded_targets`, the sole constructor boundary used by every guarded DAE route. Added a clock-owned B.1c regression that requires `Const(true)` and forbids a target `LoadP` self-hold. Phase Solve 74/74. Corrected trace has exact initialized/valid right limits and exact acceptance transition at 5 ms; comparator mean-channel score improves `2.541e-2 -> 3.248e-3`. Remaining acceleration divergence after 65 ms and event-side alignment require full-route adjudication. |
| 2026-08-10 | R2 corrected-estimator performance review (open) | Correcting the self-hold activates 18,818 recursive operations in guarded programs 9/10 that the broken fast run omitted. Hot best is `0.458355511 s`, not `0.350756488 s`. Delayed perf again attributes necessary generated execution: residual batches 90/79/89/52 total 22.67%; schedules 1/5/4/2/3/6 total 23.83%; fold 263 is 5.73%. Schedule owners 2 and 4 remain the same exact 27-program/100-output closure. | Reject the broken speed result. Preserve the correctness fix. Finish SOLVE-C43's actual block owner table (the current embedded `Arc` plus id still serializes bodies) and specify issued refresh-stage identity with exact external input/output/invalidation ranges before attempting same-coordinate reuse. Runtime artifact: `/tmp/rdd2-correct-estimator-runtime-hot.data`; trace plot: `/tmp/rdd2-optical-rumoca-vs-omc-fixed.html`. |
| 2026-08-10 | R2 schema-51 module-size follow-up (closed) | Guarded-assignment evaluation, optional native compilation, checked target-range writes, and relation-memory overrides formed one coherent responsibility inside the 1,020-line discrete-row module. Moving only those methods avoids an arbitrary file split and preserves the compact owner through the final write adapter. | Added `solve_runtime/guarded_assignments.rs` (213 lines); `discrete_rows.rs` is now 802 lines, below SPEC-0021's warning band. Workspace-aware formatting and `git diff --check` pass; `cargo test -p rumoca-solver --lib` passes 285/285. No representation, execution, or cache behavior changed. |
| 2026-08-10 | R4 GALEC/Production C architecture review (open) | The roadmap initially treated GALEC tensor cleanup as a later audit. Source inspection found the current `galec-production` dispatch constructs `AlgorithmCodePackage` directly from DAE and `AlgorithmCodeTemplateRenderer` feeds that GALEC statement view to the shared C template. No implemented `SolveAlgorithmBlock` refinement appears on this path, despite GAL-008/024 requiring it and GAL-026/037 requiring array-native checked operations through final rendering. | Promote convergence to an explicit objective and acceptance gate. DAE must own semantic action/aggregate facts once; GALEC is an admissible checked projection; Production C consumes the float32-profile `SolveAlgorithmBlock` executable refinement. Add structural, differential, code-size, and task-runtime proof before eFMU completion. This finding remains open and SOLVE-C49 is not credited as a Production C improvement yet. |

## Measurement log

| Date | Revision/worktree state | Scenario | Result | Profile conclusion |
|---|---|---|---|---|
| 2026-08-10 | dirty integration branch, before guarded folds | Optical flow, 0.5 s | best 0.512339476 s; 0.9759x real time | Compact fold kernels dominate; inactive 3x3 estimator corrections execute eagerly. |
| 2026-08-10 | schema 44 guarded folds, dirty integration branch | Optical flow, 0.5 s, 5 timed after 2 warmups | avg 0.465718363 s, best 0.465261289 s; best 1.0747x real time | Correct lazy fold ownership removes about 9.19% wall time, but residual batches still own 31.86% of whole-process perf samples and three 9-point/11-carried fold kernels own 10.32%; target not met. A 30-run perf capture is `/tmp/rdd2-schema44-guarded-hot.data`. |
| 2026-08-10 | schema 44 guarded folds plus globally unique residual symbols, dirty integration branch | Optical flow, 0.5 s, 30 timed after 2 warmups; perf collection delayed 8 s | avg under perf 0.479132 s; prior non-perf best remains 0.465261289 s | Runtime-only symbols prove generated work dominates: residuals 60--62 and 98--100 total about 18.7%, folds 111/113/120 total 15.08%, assignment schedule 5.57%. Capture: `/tmp/rdd2-schema44-runtime-only.data`. |
| 2026-08-10 | schema 45 higher-rank compact transpose, dirty integration branch | Optical flow, 0.5 s, 5 timed after 2 warmups | avg 0.464903497 s, best 0.464504963 s; best 1.0764x real time | No performance regression from separating trailing `element_width` from AD lanes; next measured target remains lazy correlated conditional execution. Cache: `/tmp/rumoca-rdd2-schema45`. |
| 2026-08-10 | schema 46 first correlated-conditional cold compile | Optical flow, 0.5 s, profiling enabled | Compile reached `Estimation.MultiSensorInvariant.step` and exposed a region-local activation-stack ownership bug before native preparation; no runtime number accepted. | Failure was deterministic and fixed at the semantic fork: copied function frames now use the conditional region's local activation base. Cache remains `/tmp/rumoca-rdd2-schema46`; rerun required. |
| 2026-08-10 | schema 46 record-aware conditional owner, dirty integration branch | Optical flow, 0.001--0.01 s diagnostic slices | Native execution exits 139 through address zero. With native execution disabled, 0.004 s completes but 0.005 s exceeds 20 s; no performance number accepted. | The regression starts exactly at the first estimator task activation. Native and interpreter failures must be isolated independently before a mission benchmark. Native artifact: `/tmp/rdd2-schema46-crash.data`; interpreter artifact: `/tmp/rdd2-schema46-interp-hang.data`. |
| 2026-08-10 | schema 47 unique-symbol attribution, dirty integration branch | Optical flow, 0.01 s diagnostic, debug binary, one timed run | Correctness-only run completed; debug real-time factor is not a performance result. Program 76 has 1,327 direct ops, 547 outputs, 88,762 recursive ops, and was the prior release perf leader at 19.67%. | Discrete RHS program 4 is the exact semantic owner. Its source has 273 outputs and one conditional; the specialized output count includes its runtime guard certificate. Optimize the compact discrete update contract, not visible projections or generic graph CSE. |
| 2026-08-10 | schema 46 independent conditional-region tape, dirty integration branch | Optical flow, 0.0 s native diagnostic | Completes successfully; prepare 50.828 s, backend build 46.772 s, hot 0.0000165 s. | Native stack corruption is fixed. Preparation remains grossly inflated by repeated validation and enormous recursively embedded functions. |
| 2026-08-10 | schema 46 pre-capture-ABI control, dirty integration branch | Optical flow, 0.004 s native, `dt=0.001` | prepare 47.901 s; hot 14.894 s; 0.000269x real time. | Pathology exists before the estimator's 5 ms task tick. Delayed perf/JIT evidence shows repeated ~3.3 MiB residual/assignment owners and a ~7.8 MiB Jacobian owner caused by recursive statement rebuilding. |
| 2026-08-10 | schema 46 exact call-frame capture ABI, dirty integration branch | Optical flow, 0.004 s native first use, `dt=0.001` | prepare 6.460 s; backend build 4.303 s; hot 1.078 s. Warm best after preparation is 0.002445609 s (1.636x real time). | Exact captures improve preparation 7.4x, backend build 10.2x, and first-use hot execution 13.8x by eliminating recursively rebuilt prior statements. This is a structural win but not the mission target. |
| 2026-08-10 | schema 46 exact captures plus block-local native capture loads, dirty integration branch | Optical flow, 0.5 s, warmed | best 0.671060833 s; 0.745x real time | Lazy capture loads are semantically correct but runtime-neutral versus 0.669602236 s immediately before the refinement. The schema-45 0.464504963 s baseline remains faster; repeated native conditional-region inlining is now the first hot structural owner. |
| 2026-08-10 | schema 47 issued exact call-frame owners, dirty integration branch | Optical flow, 0.5 s, 5 timed after 2 warmups | avg 0.674973500 s, best 0.674595202 s; best 0.7412x real time | One owner-aware 214-row residual batch removes cross-row selected-body recomputation but is only a small improvement over the 0.690111729 s immediate control. The first perf capture after the change records 20 iterations at 0.679165736 s average under perf; generated residual work remains dominant. |
| 2026-08-10 | schema 48 compact output-range capability with Phase Solve cutover withdrawn, dirty integration branch | Optical flow, 0.5 s, 5 timed after 2 warmups | compile 0.965 s; prepare 6.648 s; avg 0.673670725 s, best 0.673274167 s; best 0.7426x real time | The restored scalar Phase Solve path completes and is runtime-equivalent to schema 47. The checked range capability is present but intentionally unused by RDD2 until aggregate packing owns a compact register view; no invalid AD repack or graph recollapse was retained. Cache: `/tmp/rumoca-rdd2-schema48-stable`. |
| 2026-08-10 | schema 48 targeted IR attribution before compact captures | Optical flow, zero-time compile with `RUMOCA_PROFILE_IR=1` | 4,950 scalar `predict` record-field-5 entries, 4,950 scalar `symmetrize` calls, versus 43 aggregate `symmetrize` calls | The 225-element covariance is expanded at capture entry in 22 conditional contexts. This is the fixed schema-49 acceptance counter; post-change profiling must remove the scalar call families rather than recollapse their emitted graph. |
| 2026-08-10 | schema 49 lazy aggregate conditional plus result ranges | Optical flow Solve-IR compile with `RUMOCA_PROFILE_IR=1` | covariance field-5 scalar entries 0, aggregate entries 28; scalar `symmetrize` 0, aggregate 83; scalar stores 3,249, range stores 2,713; JSON about 85 MiB | The 4,950-entry expansion is removed at the record-valued DAE conditional. Release preparation then correctly rejected irregular forward-AD projections, exposing vector division and cross product as the next first divergent tensor families. Artifacts: `/tmp/rdd2-schema49-lazy-range.solve.json` and `/tmp/rdd2-schema49-lazy-range-profile.log`. |
| 2026-08-10 | schema 50 compact division/cross, debug correctness diagnostic | Optical flow, 0.01 s, one debug run | completes 11 points at final time 0.01; prepare 6.102 s; debug hot 103.719 s | Both prior AD affine-range failures are gone. Debug hot time is dominated by validation instrumentation and is not comparable to release baselines. Release rebuild/benchmark is required before any speed conclusion. Cache: `/tmp/rumoca-rdd2-schema50-tensor-native-debug`. |
| 2026-08-10 | schema 50 aggregate-expression owner, before final fold specialization | Optical flow, 0.5 s, warmed | avg 2.220422894 s, best 2.219396889 s | Issuing the exact aggregate expression owner is a SOLVE-C43 prerequisite but only a 1.2% runtime change; it is retained for semantic identity, not credited as the performance fix. Artifact: `/tmp/rdd2-schema50-expression-owner.json`. |
| 2026-08-10 | schema 50 bounded final fold specialization | Optical flow, 0.5 s, warmed | avg 1.643826943 s, best 1.642736249 s | Final Cranelift emission inlines fixed folds through eight points without altering compact Solve IR. Child 5-point call/capture overhead disappears and wall time improves 26.0%; outer 36-point solveSPD folds then own about 53% of runtime. Artifacts: `/tmp/rdd2-schema50-inline8.json` and `/tmp/rdd2-schema50-inline8-runtime.data`. |
| 2026-08-10 | schema 50 strict-prefix and conditional-arm assertion ownership | Optical flow, 0.5 s, five timed after one warmup | avg 0.505952444 s, best 0.505802826 s; best 0.9885x real time; 101 points, final time 0.5 | Inactive GPS/mocap 6x6 solveSPD work is now skipped by one checked guarded owner. Runtime improves 69.2% from the immediate control and the old fold leaders disappear. Fresh 30-run delayed perf averages 0.510887640 s; assignment schedules are now the largest unattributed family (~30.3%). Artifacts: `/tmp/rdd2-schema50-conditional-guards.json`, `/tmp/rdd2-conditional-guards.solve.json`, and `/tmp/rdd2-schema50-conditional-guards-runtime.data`. |
| 2026-08-10 | schema 50 SOLVE-C44 compact parameter-static refresh certificate | Optical flow, 0.5 s, five timed after one warmup | avg 0.504198378 s, best 0.503993813 s; best 0.9921x real time; 101 points, final time 0.5 | Exact compact certification and cache-key construction pass, but the ~0.36% best-time change is noise-scale. Source review confirms the leading outer-loop guidance is deliberately algebraic/continuous between integral ticks and the rigid-body solve reads continuous state. Preserve the capability; optimize necessary dynamic tensor work next. Artifact: `/tmp/rdd2-schema50-parameter-static-refresh.json`. |
| 2026-08-10 | schema 50 SOLVE-C45 exact aggregate-call projection group | Optical flow, 0.5 s, five timed after one warmup | avg 0.458260692 s, best 0.457198481 s; best 1.0936x real time; 101 points, final time 0.5 | Exact source-call ownership removes two duplicated continuous programs without CSE or recollapse. A 30-run runtime-only capture averages 0.462787537 s under perf. Residual batch 86 is 8.34%, discrete estimator-clock program 68 is 7.01%, rigid-body fold 207 is 6.72%, and assignment schedules 1/5/4/2 total 19.82%. Program 68's old trace specialization exposed 547 outputs (273 values plus 274 scalar guard certificates) and recursively owned 89,075 operations; source inspection proves it is discrete RHS program 4, not a visible-output or nonlinear-root program. Artifacts: `/tmp/rdd2-schema50-call-group-runtime.data`, `/tmp/rdd2-schema50-call-group-profile-ir.log`, and `/tmp/rdd2-schema50-call-group-kernel-map.log`. |
| 2026-08-10 | schema 50 SOLVE-C46 direct checked owners, before guarded-value ownership | Optical flow, 0.5 s, warmed | about 1.044 s; 101 points, final time 0.5 | Removing trace-derived recollapse exposed real eager work. Perf attributed roughly 35% to planning trig/fold kernels reached from program 64. This intermediate regression is rejected as a performance state but retained as proof that runtime tracing had hidden an upstream ownership defect. |
| 2026-08-10 | schema 50 SOLVE-C47 exact clock-unconditional branch | Optical flow, 0.5 s, warmed | about 1.036 s; 101 points, final time 0.5 | Program 68 shrinks to 453 direct ops, 3 `Select`s, and 9 parameter loads, but program 64 still owns 15 unguarded planning folds. Structural result accepted; whole-run performance remains rejected. Artifact: `/tmp/rdd2-schema50-solve-c47-direct-owner-runtime.data`. |
| 2026-08-10 | schema 50 SOLVE-C48 exact guarded branch-value owner, first cut | Optical flow, 0.5 s; five timed after one warmup, then 30 under delayed perf | non-perf avg 0.498809212 s, best 0.498069861 s; perf avg 0.504535273 s, best 0.503887378 s; 101 points, final time 0.5 | Eager program-64 folds become guarded and the ~1.04 s regression is mostly removed. The remaining program is still scalar-expanded: 6,215 direct ops, 30 guarded folds, 758 `Select`s, and 441 parameter loads for 103 outputs. Runtime leaders are batches 70/86, program 68, fold 247, and assignment schedules 1/4/5/2/3. C48 remains open pending a compact correlated guarded-assignment/range owner and explicit conditional activation. Artifact: `/tmp/rdd2-schema50-solve-c48-guarded-owner-runtime.data`. |
| 2026-08-10 | schema 50 SOLVE-C49 compact guarded-assignment prototype | Optical flow, 0.5 s; five timed after one warmup, then 30 under delayed perf | non-perf avg 0.457408871 s, best 0.456610952 s; perf avg 0.463584603 s, best 0.462321008 s; 101 points, final time 0.5 | The representative guarded family is two direct operations and the 273-output estimator program is 195 direct operations, with lazy selected-region execution. Performance returns to the SOLVE-C45 level without trace recollapse, but is only about 1.095x real time and remains roughly 9.13x slower than the 0.05 s target. The leading measured owners are residual batch 90 (9.37%), fold 263 (8.04%), estimator program 72 (7.73%), and assignment schedules 1/5/4/2 (22.25% combined). Schedules repeat large exact guidance programs, and residual batches 90/52 repeat the same outer-loop dependency family; call-site attribution and validity-keyed reuse proof are required after the compact target-range contract. Artifact: `/tmp/rdd2-schema50-solve-c49-compact-guard-runtime.data`. |
| 2026-08-10 | schema 51 SOLVE-C49 compact planning/runtime boundary | Optical flow, 0.5 s; five timed after one warmup | compile 20.339859789 s; prepare 7.379146104 s; avg 0.351423530 s, best 0.350756488 s; 101 points, exact final time | Removing the reusable scalar planning catalog improves best wall time 23.2% from 0.456610952 s. The result is 1.425x real time but remains 7.02x slower than the 0.05 s target. This exceeds the 10% review trigger: capture fresh delayed perf and review the compact-owner diff before choosing the next optimization. |
| 2026-08-10 | schema 51 runtime-only perf and exact schedule attribution | Optical flow, 0.5 s; 30 timed after two warmups; perf enabled after 8 s; pinned core | avg 0.354678606 s, best 0.354172786 s under perf; 13,538 samples, zero lost | Residual batch 90 9.17%; fold 245 8.64%; schedules 4/2/5/3/1 total 22.26%. Schedules 2 and 4 have the exact same 27 programs, 100 outputs, names, and 1,489 direct/9,066 recursive operations, but distinct plan instances. Any execution reuse requires issued semantic identity and complete coordinate/invalidation proof, not graph comparison. Artifact: `/tmp/rdd2-schema51-c49-compact-planning-runtime.data`. |
| 2026-08-10 | OMC a96aa1a mature backend, GCC `-Os` generated-code control | Same optical model and 0.5 s horizon; 100 intervals | frontend 1.317 s; backend 0.785 s; sim-code 0.472 s; templates 3.954 s; C compile 2,419.546 s; total 2,432.694 s; peak RSS 10,553,412 KiB; DASSL 6.573 s; fixed GBODE/Runge-Kutta best internal 1.30177 s / 1.33 s wall | The source model is demanding, but OMC's 42 MiB/8,787-line generated function unit and 40-minute C compile are a severe scalar code-generation pathology. Even its optimized executable spends ~76% of fixed-step time handling 500 clock events. Rumoca is already 3.71x faster end-to-end than this best OMC runtime control; the remaining 7.02x gap is against Rumoca's independent target and must be removed through compact owner execution, not by weakening the target. Artifacts: `/tmp/rdd2-omc-optical`, `benchmark.mos`, `optical_05`, and `optical_05_gbode_{1,2,3}.csv`. |
| 2026-08-10 | First authoritative optical trace comparison, before/after guarded-plan fix | Rumoca and OMC, 0.0--0.5 s, repository `cargo xtask repo msl -- plot-compare`, 280 common variables / 501 aligned points | Before: mean-channel bounded normalized L1 `2.541e-2`, estimator Boolean outputs permanently false. After/recheck: `3.248e-3`; initialized/valid are true at the t=0 right limit and prediction/optical-flow acceptance transition at 5 ms on both traces. Worst numeric channels are estimator/navigation vertical acceleration (`9.789e-2` bounded L1), followed by accepted-flag event alignment (`7.407e-2`). | First divergence was Phase Solve construction, not the model or Kalman algorithm. OMC stores duplicate event left/right samples while Rumoca emits one right-limit sample, explaining the remaining flag event-mismatch score. Acceleration agrees through 60 ms then diverges modestly after the first large correction; full mission and tolerance/event-alignment review remain open. Artifacts: `/tmp/rdd2-optical-rumoca-vs-omc-fixed.html` and `/tmp/rdd2-optical-rumoca-vs-omc-fixed-recheck.html`. |
| 2026-08-10 | Corrected estimator hot benchmark and runtime perf | Optical flow, 0.5 s; five timed after one warmup, then 30 timed after two warmups with perf enabled after 8 s | compile `1.431167 s`; prepare `7.676724 s`; hot avg `0.458785 s`, best `0.458356 s`; perf avg `0.462098 s`, best `0.461414 s`; 101 points | The prior schema-51 best was invalid because Boolean function outputs were held. Correct runtime is 1.091x real time and 9.17x slower than the 10x target. Runtime leaders: residual 90 8.41%, residual 79 6.44%, schedule 1 5.79%, fold 263 5.73%, schedules 5/4 4.51/4.44%, residual 89 4.12%, residual 52 3.70%. Artifact: `/tmp/rdd2-correct-estimator-runtime-hot.data`. |
| 2026-08-10 | SOLVE-C50 construction-certificate review | Focused checked-construction, evaluator, Phase Solve, Cranelift, and release RDD2 preparation/runtime | Solve IR 137/137, evaluator 122/122, Phase Solve 74/74, Cranelift 50/50; prepare `7.501854 s`; hot avg `0.462322 s`, best `0.460603 s` | Stored scalar and guarded-assignment programs now derive immutable register-capacity certificates during checked construction. Conditional validation is shared by issued owner id inside the enclosing checked table, and register proof borrows operations instead of deeply cloning them. This removes about 2.3% of preparation time and is runtime-neutral, as expected. Wire replay reconstructs the proof; no validation bypass, expression hashing, CSE, or recollapse was introduced. The remaining C43 milestone is the actual out-of-line call-frame owner table rather than an embedded `Arc` plus id. |
| 2026-08-10 | Corrected estimator post-C50 delayed perf | Optical flow, 0.5 s; 30 timed after two warmups, perf enabled after 10 s | perf avg `0.465973 s`, best `0.465550 s`; 101 points | Runtime remains unchanged within profiler noise. Leaders are residual 90 9.17%, residual 79 7.53%, fold 263 7.06%, schedules 1/4/5 6.13/5.32/5.13%, residual 89 4.57%, schedules 2/3 4.55/3.36%, folds 270/261 4.15/4.00%, and residual 52 3.65%. Schedules 2 and 4 have the same 27-program/100-output shape, but equality of shape does not prove equality of invocation coordinates or permit reuse. Artifacts: `/tmp/rdd2-c50-runtime-hot.data`, `/tmp/rdd2-c50-runtime-hot-flat.txt`, `/tmp/rdd2-c50-runtime-hot.json`, and `/tmp/rdd2-c50-runtime-hot.stderr`. |
| 2026-08-10 | Root/event callback attribution review | Optical flow, 0.5 s, one tracing-enabled debug run | derivatives 4555 / 172.748 ms; roots 1154 / 994.808 ms; events 500 / 1673.380 ms, including 1673.050 ms update; total evaluator time 2840.936 ms | The estimator is deterministic and does not request nonlinear root solving. The model owns 17 continuous roots; the expensive ones are function-scoped assertions whose 674/700-operation call frames are independently embedded. Root and event callbacks repeatedly execute large exact algebraic closures. Next instrument semantic refresh-stage coordinates, then move assertion predicates and result projections behind one checked function-call owner. Do not cache based on matching scalar plans. Artifact: `/tmp/rdd2-refresh-trace.stderr`; Solve IR: `/tmp/rdd2-optical-owned.solve.json`. |
| 2026-08-10 | R2 pure-call ownership design review | `ScalarCompiler::function_call`, call-scoped assertion lowering, Solve event blocks, typed program vocabulary, SOLVE-C25/C43/C50 | The first expansion boundary is explicit: Solve has no ordinary call op, so every result projection inlines a callee and every assertion root/action forks another compiler over the enclosing call path. Block-local conditional ownership cannot share value and predicate projections across residual/root/action blocks. | Added SOLVE-C51 and its construction row: one model-level checked pure-call table owns compact typed captures, value results, and reached assertion predicates; all consumers use issued projections under an exact coordinate/invalidation certificate; final backends emit one helper. Updated C25 to forbid rebuilding call arguments/body for root/action projections. Implementation must extend the existing typed Solve vocabulary and replace the legacy inlining boundary, not add a parallel cache or graph matcher. |
| 2026-08-10 | R2 pure-call construction review | First shared typed-Solve `SolvePureCallTable`, atomic `SolveOperation::Call`, nested wire replay, negative construction tests | The initial owner interface correctly kept each aggregate as one register and returned value plus assertion predicate atomically, but review found two proof gaps: provenance alone cannot distinguish repeated call occurrences/contexts, and wire replay did not compare interface slot types or forbid reading an output before its definition. | Added opaque nonzero compiler-issued `SolvePureCallIdentity` with table-wide uniqueness, exact input/output type replay, one-definition output proof, output-read rejection, topological prior-owner-only calls, and table-aware nested-call replay. One call operation owns all aggregate arguments and ordered value/predicate destinations; there is no scalar coordinate list or body matching. Solve IR is 142/142 plus 3/3 doctests; focused strict clippy is clean after allowing only the broader branch's already known `linear_op` nesting/length and two pre-existing needless-borrow findings. Full strict clippy still reports those pre-existing broader-branch findings and is not claimed green. |
| 2026-08-10 | R2 schema-52 pure-call integration review | Mandatory `SolveModel::pure_calls`, shared unary/binary intrinsic vocabulary, construction dominance, model/wire fixtures | Review found that a call body could declare non-interface persistent storage and thereby hide a semantic capture, and runtime/static aggregate coordinates lacked one explicit shared-vocabulary boundary statement. | Schema 52 requires the checked table on every model. Output and method-local loads require a dominating definition; call bodies may declare only explicit input/output slots plus method locals, so every capture is visible in the typed ABI. SOLVE-C36 now fixes constant projection/slice coordinates as zero-based IR values and dynamic selection registers as one-based Modelica values converted once by the compact operation. Solve IR passes 149/149 plus 3/3 doctests after the added hidden-capture and structured-region rejection coverage. |
| 2026-08-10 | R2 compact typed-evaluator review | `rumoca-eval-solve` execution of the shared typed program and model-level call table | The evaluator boundary needed exact binary32 rounding, Integer-domain failures, row-major slice/index behavior, and nested call transfer without turning an aggregate register into an operation graph. Review also checked that value plus assertion outputs come from one invocation rather than separate body evaluation. | Added one `TypedValue` per typed register/slot, with compact shape plus boundary-local payload. The evaluator executes aggregate operations directly, recursively invokes only prior issued owners, transfers every ordered result/predicate atomically, rounds binary32 at each operation, and rejects invalid conversions/domain overflow. Focused tests cover nested aggregate/result/assertion tuples, one-based runtime selection and fallback, binary32 rounding, and domain failure; full evaluator is 128/128. Focused clippy is clean with allowances only for documented broader-branch pre-existing nesting/length/argument/borrow/iterator findings; strict full-package clippy still reports those existing files and is not claimed green. |
| 2026-08-10 | R2 SOLVE-C52 structured-region review | Checked `Conditional` and `Fold` typed operations, recursive wire replay, compact evaluator execution | A pure-function owner cannot preserve Modelica laziness with an eager select, and unrolling a function fold would recreate the forbidden scalar graph before the final backend. Review required exact region captures/results, complete branch definitions, bounded compact iteration domains, and recursive checked replay. | Added typed structured conditional regions that execute only the selected checked arm and compact folds whose transition owns the carried tuple and invariant captures. Construction rejects incomplete branches and invalid domains before committing an operation; wire deserialization recursively replays the same constructors. The evaluator traverses `StructuredIndexDomain::index_tuple_iter()` without materializing domain tuples. Full Solve IR is 151/151 plus 3/3 doctests, evaluator 129/129, Phase Solve 74/74, formatting and diff checks pass, and no legacy scalar body or expansion/recollapse bridge was introduced. |
| 2026-08-10 | R2 SOLVE-C53 tensor-algebra vocabulary review | Typed scalar/tensor scale, first-two-axis transpose, vector/matrix multiplication, and aggregate reduction | Reusing elementwise binary operations would require broadcasting or matrix products to be expanded into coordinate operations before the final backend. Review required result shapes to derive solely from operand types, invalid inner extents to fail before destination issuance, and evaluator arithmetic to honor the declared binary32/binary64/Integer profile. | Added four compact checked operations and recursive wire replay. Matrix products cover vector-vector, matrix-vector, vector-matrix, and matrix-matrix shape relations; transpose retains all trailing axes. Evaluator payload loops remain at the execution boundary and use the same checked scalar arithmetic. Positive shape/wire/numeric tests and a negative no-commit inner-extent test pass. Full Solve IR is 151/151 plus doctests and evaluator 129/129. The remaining DAE aggregate families—concatenation, identity/generators, diagonal/outer/skew/cross, and update—must join this same vocabulary before the Phase call cutover. |
| 2026-08-10 | Exact legacy call-entry identity review | `ScalarContextFrame`, deferred assertion frames, scalar/aggregate/record call entry points | The legacy transition path identified a function frame only by function plus actual arguments, which merges two distinct source call occurrences and is insufficient to issue SOLVE-C51 identities. | Every call entry now carries its exact source `ExprId` through the semantic context, deferred assertion recipe, and active-frame match. The existing Phase Solve suite remains 74/74. This is identity plumbing for replacement by the typed owner, not a body cache or graph-equivalence inference. |
| 2026-08-10 | First single-source DAE-to-typed lowering canary | One checked aggregate Modelica function computing `transpose(matrix) * vector` | The shared lowerer must prove it can consume DAE function parameters and result definitions directly without passing through `LinearOp` or enumerating the matrix. This canary is deliberately test-gated until structured statements, records, nested calls/assertions, and the complete tensor vocabulary are supported; it is not yet the production Phase cutover. | Added the initial `lower/typed_functions.rs` implementation and a real checked-DAE fixture. One exact call becomes one owner with two aggregate inputs, one aggregate result, and exactly five operations: two loads, one compact transpose, one compact matrix multiply, and one store. There are no `ProjectElement` or `ConstructAggregate` operations. The focused Phase test and non-test package check pass. Next extend this same implementation rather than adding a GALEC-specific function lowerer. |
| 2026-08-10 | MSL trace comparator recheck | `cargo xtask repo msl -- plot-compare` with the retained corrected Rumoca and OMC optical traces | The repository comparator, not a separate script, must remain the sole parity authority. | Recheck covers 280 common variables and 501 aligned points through 0.5 s. Mean-channel bounded normalized L1 remains `3.248e-3`; the worst numeric channel remains vertical estimator/navigation acceleration at `9.789e-2`, and the two accepted flags remain `7.407e-2` because of OMC's duplicate event-side samples. Artifact: `/tmp/rdd2-optical-rumoca-vs-omc-xtask-recheck.html`. |
| 2026-08-10 | Recursive typed call-owner canary review | Exact nested DAE call with one unconditional function assertion | A nested call must be registered before its caller, invoked once, and return all selected results plus every reached assertion predicate as one ordered tuple. Review rejected propagating the assertion through another scalar lowering or identifying calls from equal bodies/arguments. | The test-gated single-source lowerer now recursively registers each exact nested `ExprId` under its parent occurrence. The outer typed body has exactly one `SolveOperation::Call`; the inner result and predicate evaluate atomically to `(7.0, true)` and the predicate is forwarded through the outer owner without rebuilding the inner body. Unsupported conditionals/folds still fail closed. Two focused Phase canaries pass; production cutover remains blocked on exact guarded predicate activation, record leaves, remaining tensor families, and the model-level identity/coordinate registry. |
| 2026-08-10 | MSL short-horizon/right-limit trace adjudication | Repository `plot-compare --compare-until` sweep at 5, 10, 20, 40, 60, 65, 70, and 100 ms, followed by direct inspection of retained samples | The apparent first acceleration failure at 65 ms could be either wrong estimator execution, event-side alignment, or numerical trajectory separation. The central comparator already selects the last numerically coincident sample, so OMC duplicate left limits do not explain its numeric score. | Agreement improves through 60 ms (mean channel score `2.449e-3`). At the 65 ms post-event right limit, Rumoca/OMC vertical estimator acceleration is `132.049788/127.670396`; corresponding vertical plant position is `0.099425959/0.099442285`, and OMC's estimator position right limit `-0.018947777` is close to Rumoca `-0.018893035`. The short-window near-unit bounded score is caused by robust normalization seeing one large endpoint against a nearly constant prior range, not a wrong left-limit comparison. This is a modest post-liftoff numerical/integration difference rather than the earlier held-estimator compiler bug. Retain the full-horizon `9.789e-2` channel score and compare matched solver/step settings before changing estimator semantics. Artifacts: `/tmp/rdd2-optical-parity-0.005.html` through `/tmp/rdd2-optical-parity-0.100.html`. |
| 2026-08-10 | Matched OMC DASSL control and right-limit review | Existing OMC DASSL CSV converted to the repository trace schema, then compared with `cargo xtask repo msl -- plot-compare` | The retained OMC JSON was a fixed GBODE trajectory, so its 65 ms value was not a matched control for Rumoca's accepted RK-like trajectory. | At 65 ms Rumoca and OMC DASSL agree on vertical estimator acceleration to `0.01915 m/s2` (`0.014%`), plant position to `7.8e-8 m`, estimator position to `2.4e-7 m`, velocity to `9.6e-5 m/s`, and thrust to `2.4e-4 N`. The no-metadata aggregate comparison improves to `2.146e-3`; GBODE divergence is a solver-method effect, not evidence for an estimator rewrite. Artifact: `/tmp/rdd2-optical-rumoca-vs-omc-dassl-xtask.html`. |
| 2026-08-10 | Compact DAE function-fold wire replay review | Fresh RDD2 DAE JSON replay through `rumoca-sim-worker`, plus focused and full `rumoca-ir-dae` tests | Production `LinearAlgebra.solve`/`solveSPD` contain nested compact folds, while assertion validation contains zero-carried folds whose transitions emit no generated parameter/output expressions. The wire reader rejected nesting and then stalled on the zero-width transition. | Replay now recursively flattens the statement tree but reconstructs every fold through `begin_nested_loop`/`finish_nested_loop`; zero-carried transitions advance only when they are the next checked operation and therefore need no invented wire marker. Positive nested and assertion-only round trips plus forged identity reuse coverage pass; full DAE IR was 140/140 before the added assertion-only canary, whose focused test also passes. No array or expression graph is expanded. |
| 2026-08-10 | Metadata-bearing DASSL trace comparison | Fresh 101-point RK-like Rumoca result with 1,096 compiler-issued variable metadata records, compared against OMC DASSL using repository `plot-compare` | The retained Rumoca trace had no metadata, so the comparator linearly interpolated estimator clock-held fields. Fresh metadata correctly classifies `estimator.estimate.*` as event-discrete, but the algebraic `avionics.navigation.*` aliases are still labeled continuous-time solely from declaration role. | Mean channel bounded normalized L1 improves from `2.146e-3` to `1.190e-3`. The remaining worst acceleration score (`1.007e-1`) belongs to the misclassified algebraic alias, not the correctly classified estimator field. Specify and issue a construction-owned effective time-domain certificate for visible algebraic aliases; do not add name-, value-, or comparator-inference heuristics. Artifact: `/tmp/rdd2-optical-rumoca-vs-omc-dassl-meta-xtask.html`. |
| 2026-08-10 | SOLVE-C54 effective-time-domain review | Spec/catalog ownership, branded causal definitions, Solve declaration construction/wire replay, scalar metadata boundary, and fresh RDD2/OMC-DASSL trace comparison | The first implementation was semantically correct but walked each causal definition a second time solely for time-domain classification. Review also required incompatible wire labels to fail instead of being trusted as report metadata. | Classification now joins coordinate-domain and algebraic-dependency facts during the existing causal dependency traversal; transitive issuance follows the same acyclic order and complete scalar coverage reuses already-present scalar residual owners without expanding aggregates. `SolveVariableDeclaration::event_discontinuous` accepts only Real algebraic/output storage, and root validation rejects forged role/domain combinations. Focused structural, Solve IR, Phase Solve, and comparator tests pass. Fresh comparison covers 280 variables/501 aligned points and improves mean-channel score to `7.009e-4`; worst remaining channels are continuous plant/IMU acceleration (`6.003e-2`). |
| 2026-08-10 | R2 Cranelift conditional-helper profile review (open) | The corrected 273-output estimator update was residual program 8. Its 377 direct operations recursively embedded about 535,925 operations, exceeded Cranelift's function-size limit, and silently fell back to the interpreter. The checked block-local `FunctionConditionalOwnerId` existed, but final lowering still duplicated its body in the caller instead of emitting the helper required by SOLVE-C43/C51. | Cranelift now emits each issued conditional owner once as a lazy helper and calls it with compact capture/result ranges; unowned hand-built programs retain the local path. Full Cranelift coverage passes 50/50 and the size-limit fallback is gone. Warm 0.5 s optical-flow runtime improves from the immediate broken `2.84055 s` average to `0.69623 s` (`4.1x`), but remains slower than the corrected `0.45836 s` baseline and far from the `0.05 s` target. A checked contiguous capture range now aliases the parent tape without scalar copying, yielding only about 1%; perf attributes about 38% inclusive time to the estimator owner and about 36% to assignment schedules. This is an incremental final-backend repair, not completion of the Phase Solve owner-table cutover. Artifacts: `/tmp/rdd2-conditional-helper-bench.json`, `/tmp/rdd2-conditional-zero-copy-bench.json`, and `/tmp/rdd2-conditional-helper.perf.data`. |
| 2026-08-10 | R2 real RDD2 typed-call lowering review (open) | The fresh 6.2 MiB optical-flow DAE artifact was used as an exact `Estimation.MultiSensorInvariant.step` call-graph canary. Each successive fail-closed boundary identified a missing compact source operation: record leaves, correlated branch tuples, identity, element/slice update, constant and runtime projection, concatenation/generators, numeric promotion, contiguous slice, nested aggregate construction, min/max, finite folds, lazy generic conditionals, compile-time `size`, and diagonal construction. | The complete real step graph now constructs one checked typed pure-call table without enumerating tensor coordinates or loop domains. Covariance remains a 15x15 register; Cholesky uses nested `Fold` regions; `J[1:3,1:3] := Jl` is one `UpdateSlice`; runtime `A[i,i]` is one bounds-checked dynamic projection; five-way estimator correction and nested value conditionals return value plus assertion-predicate tuples lazily, with true identities on inactive arms. Converting the external canary into permanent tests found and fixed one sequential fold bug: a carried target must shadow its pre-loop definition (`sum := sum + i` now yields 6, not 3). Full focused suites pass: Solve IR 153/153, evaluator 130/130, and Phase Solve 80/80. Production attachment, native typed-owner codegen, trace parity, and runtime gates remain open. |
| 2026-08-10 | R2 production pure-call attachment and tensor-boundary review (open) | Phase Solve now registers exact root and nested DAE calls during construction, issues one checked model-level owner/site ABI, emits one clock-owned `LinearOp::PureCall` with compact typed input/result ranges, and evaluates that site through the shared typed table. The first real RDD2 production canary failed closed at missing tensor vocabulary rather than invoking the legacy inliner. Review of the first two failures found `cross` and mixed `[:, 1]` affine projection/update were still expressible only by scalar-coordinate logic. | Added one checked typed `Cross` operation plus boundary-local execution, and one checked rank-reducing `ProjectView`/`UpdateView` whose compact axes retain spans and one-based runtime indices. Builder-derived shape/bounds checks and recursive wire replay pass; focused construction and numeric tests prove one operation per source tensor relation. The zero-duration RDD2 canary now constructs every continuous dynamics owner and reaches the clocked `Planning.Bezier.prepareWaypointPlan` owner. Its next fail-closed boundary is a binder-dependent `[8,3]` conditional comprehension over the exact waypoint domain, proving the next missing owner is a structured typed `Map`, not a request to enumerate eight rows. Before closing this review: add `Map` construction/replay/evaluator and negative tests, audit the complete IR-schema diff and version gate, rerun full IR/evaluator/Phase suites, then continue the canary. Native typed-owner, GALEC/Production C, trace, and runtime gates remain open. |
| 2026-08-10 | R2 issued pure-call production cutover review (open) | The complete RDD2 production graph now lowers to a model-level checked typed pure-call table. The unconditional multi-output equation path still requested each projection separately, issuing duplicate owners, and the evaluator re-executed one issued owner for each projection. | Construction now requests all selected results in one call-frame operation. Runtime caches the complete value/predicate tuple by exact compiler-issued owner identity and reuses it for later projections; it does not compare bodies or recollapse graphs. A nested multi-result regression proves one `SolveOperation::Call` and numerical result `9.0`. Full suites pass: DAE IR 142, Phase DAE 213, and Phase Solve 83 tests. |
| 2026-08-10 | R2 owner-collapse and preparation profile review (open) | A zero-duration RDD2 canary previously carried 10,374 pure-call owners and spent about 52.7 s preparing the model, including about 39.0 s in Solve lowering. | Exact issued multi-result ownership collapses the table to 326 owners (`31.8x` fewer). Preparation is 7.066 s and Solve lowering 3.133 s; compile is 2.068 s and backend construction 3.579 s. This validates the construction-layer fix without expression expansion, CSE, or graph recovery. Artifacts: `/tmp/rdd2-dae-call-owner-t0-v2.json` and `/tmp/rdd2-dae-call-owner-t0-v2.stderr`. |
| 2026-08-10 | R2 post-cutover runtime perf review (open) | The production cutover routes typed pure calls through the compact evaluator because Cranelift explicitly has no native helper attached. A 0.5 s mission slice takes 13.071--15.467 s hot time (`0.036x` realtime), versus the acceptance limit of 0.05 s. Perf is led by `EvalFrame::run` (21.68%), allocator/free paths, typed elementwise collection, conditionals, matrix multiply, and value cloning; root solving is not the dominant cost. | The next long-term fix is one native Cranelift helper per compiler-issued `SolvePureCallOwner`, compiled once in topological owner order and called through compact typed input/output ranges. Structured regions and tensor algebra must remain compact and lower to runtime loops only at this final backend. A host callback into the Rust evaluator is rejected because it preserves the allocation/interpreter bottleneck. Artifacts: `/tmp/rdd2-issued-owner-perf.json` and `/tmp/rdd2-issued-owner-runtime.perf.data`. |
| 2026-08-10 | R2 native typed-owner implementation checkpoint (open) | Cranelift previously had no implementation for `LinearOp::PureCall`, so every production estimator invocation fell back to the allocating typed interpreter. The native ABI must preserve Real32/Real64/Integer/Boolean bits across nested owners and must not turn aggregate operations into a Phase Solve graph. | Added one retained JIT module with one symbol per topologically issued owner, compact 64-bit typed-cell input/output ranges, nested owner calls by issued id, and a final legacy-row bridge. A focused end-to-end native test evaluates one typed owner (`4 + 5 = 9`). Fixed-shape tensor operations now lower as runtime loops at the final Cranelift boundary: matrix multiply, fill/construct, scale/broadcast, transpose, cross/reduce, identity/diagonal, concatenate, and compact projection/update/view families. The real RDD2 canary's first fail-closed boundary advanced from `MatrixMultiply` to `Fill`, then `ProjectElement`, and now `Fold`; structured `Conditional`/`Fold`/`Map` CFG lowering is the current implementation task. No runtime speed result is accepted until the complete 326-owner table compiles and executes natively. |
| 2026-08-10 | R2 complete native owner-table/profile checkpoint (open) | The complete 326-owner RDD2 table now finalizes and executes a 0.5 s, 101-point waypoint slice without an interpreter-fallback diagnostic. The first profile records `rumoca_typed_pure_call_0_122` as the largest symbol (9.44%), followed by owner 319 (3.53%), but the capture includes cold frontend/Cranelift preparation and uses `target/debug/rumoca`; its 1.3506 s best hot iteration is therefore not comparable to the authoritative 0.4584 s release optical baseline. | Keep the result as implementation/profiling evidence only. Add native/interpreter parity for structured and dynamic aggregate operations, correct the remaining binary32/error-status review findings, run the same release benchmark and runtime-only `perf` protocol as the baseline, and map hot issued owner ids back to compiler-owned source provenance. Optimize only the largest necessary compact owner at final emission. No Phase Solve expansion, semantic hashing, graph recollapse, or unchecked construction is authorized. Artifacts: `/tmp/rdd2-native-owner-v1-perf.json`, `/tmp/rdd2-native-owner-v1.perf.data`, and `/tmp/rdd2-native-owner-v1-perf-report.txt`. |
| 2026-08-10 | R2 estimator call-cardinality review (open) | Compiler-owned provenance maps owner 319 to `Estimation.MultiSensorInvariant.step`, owner 110 to its single source `predict(...)`, owner 151 to `correctOpticalFlow`, and owner 122 to the correction's 2x2 `solveSPD`. One 0.5 s run dynamically entered `step` 1,120 times and `predict`/`correctOpticalFlow`/`solveSPD` 2,218 times each. Static checked-IR inspection finds 12 `Call` operations to owner 110 and two copies of each correction owner inside owner 319, although the source algorithm contains one prediction site and at most one correction. The DAE retains one exact call owner, but nested source conditionals inside the outer multi-target join are projected into separate conditional expression regions, and each region constructs another reference instead of consuming one correlated invocation owner. | This is compiler-created scheduling duplication, not evidence that the 15-state Kalman equations are intrinsically too expensive. Fix the first representation boundary: retain nested multi-target conditional correlation and exact call invocation identity as a checked compact owner, with explicit captures/results and activation/domain context. Interpreter, Cranelift, GALEC, and generated C must consume the same owner and evaluate it at most once under its issued invocation certificate. Do not hoist inactive branches, hash arguments, compare bodies, add backend CSE, or recollapse an expanded graph. Add a step-shaped regression requiring one source `predict` invocation and one selected correction, then repeat static call counts, dynamic counts, parity, release timing, and runtime-only perf. Artifacts: `/tmp/rdd2-typed-owner-call-graph-t0.stderr`, `/tmp/rdd2-native-owner-call-counts.stderr`, `/tmp/rdd2-owner-site-locations-t0.stderr`, and `/tmp/rdd2-step-dae-function.json`. |
| 2026-08-11 | R2 continuous assertion and fold-semantics review | Continuous call assertions still rebuilt large scalar root programs, and the first compact typed assertion root exposed `solveSPD(ok=false)` for a positive definite rigid-body inertia matrix. A direct transition trace found that later members of one DAE fold tuple read stale entry-state targets rather than earlier sequential redefinitions. | One exact typed call frame now owns value plus all assertion root/action outputs; root/action grouping accepts only pointer-identical issued operations and a matching owner. Typed fold lowering installs each completed target before lowering the next ordered tuple member while `FunctionFoldParameter` retains entry-state meaning. Regressions cover two assertions sharing one `PureCall` and sequential carried redefinitions. Phase Solve 85/85, evaluator 136/136, Cranelift 52/52, and solver 286/286 pass. No expression equality, recollapse, or scalar tensor graph was introduced. |
| 2026-08-11 | R2 native typed-storage review | Perf showed compact `solveSPD` folds were moving complete 15x15 carried tensors repeatedly: every fold point copied carried-to-input and output-to-carried, and every functional update copied its aggregate even at a proven final use. Diff review then found that `ReadOnly` also describes constant tape slots; treating every read-only slot as an immutable external input could let a consumed update corrupt a later constant load. | `SolveOperation` now exposes closed-vocabulary input/output register visitation for backend liveness. Cranelift forwards only true input-buffer loads and final stores, aliases functional updates only at exact last use in private tape storage, and swaps two complete fold-owned frames across the loop backedge. Captures initialize once per frame. Positive storage coverage proves input/output forwarding; negative coverage proves a read-only constant tape slot never aliases its loaded register. Best time improves from `0.5918915 s` to `0.45828918 s`; this is a final-boundary storage result over unchanged compact IR. |
| 2026-08-11 | R2 post-storage runtime profile review (open) | Fresh runtime-only `perf` shows the Kalman filter is not the dominant explanation: estimator `step` is 4.37% self and its 15x15 `solveSPD` 5.04%. The remaining time is broad: assignment schedules are about 20%, legacy compact fold helpers about 20%, rigid-body `solveSPD` 4.79%, and `step` still executes 304 times for 101 task points. Schedule profiles show several distinct compiler refresh closures over 83--212 programs and 162--609 outputs; two 89-row closures have equal size but no issued shared identity. | Keep the 0.05 s gate open. Next issue one typed refresh-execution owner/remainder certificate from construction, route continuous assignment folds through the common typed Solve lowering, and reduce estimator execution to one certified event transaction per tick. Structural/body comparison, pointer identity, backend CSE, skipped event passes, or changed solver tolerances remain forbidden. Artifacts: `/tmp/rdd2-fold-frame-v1.perf.data`, `/tmp/rdd2-assignment-detail-v1.stderr`, and `/tmp/rdd2-fold-assignment-map-full-v1.stderr`. |
| 2026-08-11 | R2 inclusive call-graph and estimator-math review (open) | A frame-pointer `perf` run resolves the complete typed call tree. `MultiSensorInvariant.step`, including prediction, selected correction, tensor algebra, and Cholesky descendants, is `7.78%` inclusive rather than the approximately 9--10% inferred by adding selected self symbols. Three independent residual batches each enter the same issued owner; a 0.02 s run enters `step` 16 times for five output points and its expensive prediction/correction subtree 13 times. The estimator source is conventional but deliberately costly: full 15x15 covariance, third-order transition, Simpson process-noise integration, Joseph correction, and SPD solves. Assignment schedules are `24.10%` inclusive, led by schedule 3 at `11.27%` through a nested compact-fold tree. | Do not rewrite or weaken estimator mathematics from this evidence. Complete SOLVE-C51's invocation-certificate/remainder behavior across outer refresh programs so the same issued call owner can be reused when construction proves an identical coordinate, and give event application an atomic settled transaction proof instead of re-evaluating the clock owner to discover no change. The same certificate must cover continuous derivative/root refresh owners and be consumed by interpreter, Cranelift, GALEC, and Production C. Artifacts: `/tmp/rdd2-fold-frame-callgraph-v1.perf.data`, `/tmp/rdd2-fold-frame-callgraph-v1-report.txt`, `/tmp/rdd2-fold-frame-callgraph-v1.json`, and `/tmp/rdd2-short-native-v1.stderr`. |
| 2026-08-11 | R2 model-event transaction design review (open) | Source/DAE/Solve tracing found the first divergent representation layer. `MixedInvariantNavigationEstimator` has one sampled model algorithm: `step` updates the state tuple, then later statements compute navigation and status from those new definitions. DAE retains one issued call identity, but only B.1c values have an atomic owner; discrete Real outputs remain separately projected B.1b residuals. Solve consequently builds several guarded programs and resets the native invocation scope at each program boundary. A persistent backend result cache would hide the split, consume controller RAM, and leave interpreter/GALEC/C with different schedules. | Added DAE-C21 and SOLVE-C55 plus their construction-catalog obligations. The required long-term owner is one checked model-event transaction spanning mixed B.1b/B.1c targets, source statement order, current/`pre` reaching definitions, clock/history lanes, compact intermediate/final ranges, and one issued call scope. Derived Appendix-B views remain available, while all executable targets consume the transaction. Next implement the DAE construction/wire owner and a mixed Real/Boolean step-shaped negative/positive test before changing Solve runtime behavior. |
| 2026-08-11 | R2 DAE-C21 construction/wire implementation review | The new owner had to preserve statement order and issued call identity without reconstructing either from B.1b/B.1c projections. Review also required failure-atomic validation, one transaction owner per mutable target, exact role/type/clock checks, and schema-gated checked replay. | DAE schema 31 now owns checked mixed discrete-Real/discrete-value model-event transactions. Phase lowering records one aggregate definition region per source assignment/function call/compact loop, retaining sequential SSA values and exact guards; no tensor coordinate is enumerated. Positive, forged-role, undeclared-target, duplicate-owner, and wire-round-trip tests pass. A sampled mixed-result regression proves one Real/Boolean call followed by a new-Real read remains a two-step transaction with one issued call identity. Full DAE IR is 146/146 and Phase DAE is 214/214 at this checkpoint. The real RDD2 DAE contains five transactions; `MixedInvariantNavigationEstimator` is one clock-owned transaction with 28 aggregate targets, eight ordered steps, and 28 definitions. Its first 12 result definitions and six later status projections all retain exact call owner 10965; navigation projection is a second exact owner 10995. Artifact: `/tmp/rdd2-dae-c21.json`. |
| 2026-08-11 | R2 SOLVE-C55 aggregate-capture checkpoint (open) | The sole DAE-to-typed expression lowerer previously accepted only function parameters and compact-domain binders. Giving event transactions a second coordinate lowerer would split semantics, while feeding one scalar register per tensor element would violate the range-preserving contract. | Added one closed semantic model-coordinate key catalog to the existing typed lowerer. A captured model tensor remains one typed register, is ordered deterministically by issued coordinate kind/id, and is reconstructed through nested `Conditional`, `Fold`, and `Map` regions without coordinate enumeration. Function-only owners retain an empty model-coordinate environment. `cargo check -p rumoca-ir-solve -p rumoca-phase-solve` passes. Next attach checked storage sources/atomic targets and a transaction body through this same lowering path, then add construction and interpreter/native parity tests. |
| 2026-08-11 | R2 SOLVE-C55 checked-interface review (open) | Attaching the first transaction body exposed two proof obligations that could not remain implicit: call-scoped predicates must be returned and checked with the same invocation as target values, and the finalized model must prove every transaction site against its sole issued pure-call table. Enabling the whole-problem construction gate also exposed older multi-output-root and conditional-B.1c certificate defects that phase lowering had returned without validation. | Solve schema 55 now gives each eligible transaction an aggregate storage-input ABI, an atomic final-target prefix, and a Boolean assertion-predicate suffix with exact aligned `Assert` actions. Whole-problem validation checks storage/clock bounds; `SolveModel` wire replay visits every scalar and transaction call site and rejects a missing or mismatched owner. The visitor now exposes transactions explicitly. Root refresh roles derive per stored output of one compact multi-output program, and conditional B.1c owners correctly construct as event equations. Solve IR is 160/160 and Phase Solve is 86/86. A fresh real RDD2 canary passes with three eligible transactions: owners 349/350/351; estimator owner 351 has 55 aggregate inputs (397 scalar payload), 28 aggregate targets (286 payload), eight statements, nine predicates/actions, and clock owner 1. Artifact: `/tmp/rdd2-solve-c55-reviewed.json`. Runtime execution, legacy-row suppression, interpreter/native parity, settled certificates, and the one-call-per-tick proof remain open. Strict clippy still reports the roadmap's pre-existing excessive-nesting/too-many-lines debt in legacy DAE/Solve validators; no new transaction module finding is present, so the strict gate is not claimed green. |
| 2026-08-11 | R2 schema-56 event-transaction cutover review | Runtime suppression could not be inferred from target ranges, spans, bodies, or equal scalar plans. One issued assertion predicate can also have several legacy action projections, so a one-to-one action mapping failed closed on the real RDD2 model. | Solve schema 56 carries a target-aligned legacy-owner inventory, event-plan transaction owner/reverse-bijection proof, and a nonempty exact action-index set per predicate. Whole-model replay proves compact target equality, clock equality, complete scalar/guarded producer-program coverage, unique transaction/action claims, and exact action equality. Runtime adapters derive scalar payloads only at the final ABI/storage boundary, evaluate native owners once on the first tick pass, precheck predicates and target bounds, commit atomically, and hold later passes. Compiled-call errors are fatal rather than silently falling back to the interpreter. The post-review helper split introduces no strict-lint finding in the new event runtime files; the affected suites pass 952 unit tests plus three doctests, and a fresh real schema-56 canary passes. Release best improves `0.457804617 -> 0.396933454 s`; `step` falls `304 -> 102` calls for 101 ticks. Post-cutover perf attributes 65.48% inclusive to assignment schedules, led by schedule 3 at 29.98%; estimator-internal `predict` remains 502 calls. Proceed with the compiler-issued refresh-execution owner/remainder contract, not graph comparison or cache recovery. Artifacts: `/tmp/rdd2-c56-event-transaction-runtime-{self,inclusive}.txt` and `/tmp/rdd2-c56-tests-final.txt`. |
| 2026-08-11 | R2 continuous refresh/JVP contract review | The 83-row plant refresh schedule owns 29.98% inclusive runtime and reaches a 95-call legacy fold tree even though the model call table already owns the same rigid-body/SPD computation as compact typed programs. The first divergence is deliberate: continuous scalar lowering declines issued typed calls, and scalar AD rejects `PureCall`, because no checked typed directional relation exists. Existing refresh remainders use `Arc::ptr_eq` plus reconstructed row/block identities, while native assignment schedules clone and filter exact-target programs during runtime setup. | Added SOLVE-C56 and construction-catalog obligations before implementation. One compiler-issued owner must now carry exact row/output/stage/target ownership, purpose, complete coordinate invalidation, and construction-issued coverage/remainder relations. Its assignment schedule is constructed once, not recovered at runtime. Continuous AD evaluates the sole typed owner through its mechanically derived checked compact primal/tangent relation: Real payloads carry tangent lanes, conditions stay primal, lazy regions select before differentiating, and tensor/fold/map operations remain compact. Interpreter, Cranelift, GALEC, and Production C share this relation; no independently lowered derivative body, legacy scalar fold translation, pointer/body identity, expansion, or recollapse is permitted. |
| 2026-08-11 | R2 schema-57 continuous typed-call review | The real RDD2 canary completes after continuous lowering selects the construction-issued typed owner only when that owner carries a checked directional relation. Review found and fixed mismatched singular guards for `asin`/`acos`/`log`/`log10`/`sqrt`/division/power, prevented primal and directional native results from sharing one cache key, and removed a native fallback that could have called a primal helper for a missing directional dependency. Unsupported vocabularies retain the pre-existing path based on the explicit construction result, not on body matching. Focused evaluator/Phase Solve/Cranelift tests and the real native canary pass. | Accept the representation slice: the scalar boundary carries only compact typed range starts, and the directional program is a mechanical constructor replay rather than independent semantic lowering. The 48.4% hot-runtime reduction is real, but R2 remains open. `perf` moves the next optimization target to primal `LinearAlgebra.solveSPD` owner 30 (22.99% self); establish a first-class semantic linear-algebra owner before strengthening final lowering, with no function-name/body recognition or expanded-pattern recollapse. |
| 2026-08-11 | R2 exact structured-capture and final tape review | The same 22-operation `solveSPD` owner carried 799 conditional plus 307 fold capture cells and executed 7,626 times per 0.5 s slice because every typed region captured the complete accumulated environment. Owner 145 carried 2,680 plus 1,140 cells for the same algorithm under a larger caller environment. This violated SOLVE-C52's exact capture ABI and inflated final stack traffic. | DAE now exposes a prunable checked expression walk; DAE-to-typed construction treats an already computed function value as an owned dependency leaf and selects only semantic inputs reached by each region. Current-fold targets/parameters are construction-local and cannot enter its capture ABI. No backend liveness inference or graph reconstruction selects captures. Owner 30 falls to 234 conditional plus 129 fold cells and 425 recursive operations from 883; owner 22 falls from 386 to 170 operations. Cranelift also reuses private typed tape ranges only after a checked strict last-use boundary. Phase Solve 87/87 and Cranelift 58/58 pass. Warmed best improves `0.204968503 -> 0.145504579 s` from exact captures and then to `0.143391616 s` with tape reuse; a post-review repeat is `0.144655433 s`, consistent with noise. Backend construction also drops. Fresh `perf` leaves owner 30 at 13.53%, owner 22 at 5.34%, schedule 3 at 4.89%, and `memmove` at 4.13%. A traced slice has 4,555 adaptive derivative callbacks, 1,154 root callbacks, and 500 legitimate 1 kHz scheduled event boundaries; do not misclassify the latter as five event-iteration passes. Artifacts: `/tmp/rdd2-schema57-exact-capture-{profile,bench,perf}.json`, `/tmp/rdd2-schema57-exact-capture-runtime.perf.data`, `/tmp/rdd2-schema57-exact-capture-reviewed-{profile,bench}.json`, and `/tmp/rdd2-schema57-rk-cardinality.json`. |
| 2026-08-11 | R2 schema-58 construction-issued refresh review (open) | Fixed algebraic, derivative, root, event, and per-clock refresh closures were still constructed after runtime evaluator preparation. Root-after-derivative reuse required `Arc::ptr_eq` and reconstructed row/block equality; coincident clocks rebuilt a union closure at runtime; native schedule caches used row-slice addresses as identities. | Solve schema 58 now serializes one private checked `ContinuousRefreshOwners` aggregate built by Phase Solve. Wire replay validates canonical row ownership, exact assignment target correlation, stage membership, and BLT block replay, then mechanically reconstructs the root-after-derivative remainder. Runtime consumes these owners directly, executes per-clock owners without dynamic union discovery, and keys native schedules only by construction-issued sequence identities. Searches find no remaining pointer identity or runtime refresh-plan builder in the production path. Forged-row, forged-isolator, and wire-replay tests join 164 Solve IR, 139 evaluator, 87 Phase Solve, and 288 solver tests, all passing. The real RDD2 canary completes 101 points at exactly 0.5 s; five-run best is `0.146418998 s` versus the `0.144655433 s` prior reviewed best (about 1.2%, noise-scale). An 80-run delayed `perf` capture averages `0.147577997 s`, best `0.147156184 s`, with zero lost samples; owner 30 remains 12.82% self, owner 22 5.81%, schedule 3 5.41%, and `memmove` 4.42%. Artifact: `/tmp/rdd2-schema58-issued-refresh.perf.data`. This review remains open: row selections are still cloned metadata rather than owner-id lists, exact assignment programs are still isolated during runtime preparation, and the coordinate dependency/invalidation certificate is incomplete. |
| 2026-08-11 | R2 schema-58 SOLVE-C56 conformance review failed | The complete spec review found a first-layer violation that the earlier functional tests and canary could not detect: Phase Solve stored refresh owners derived from `to_scalar_program_block` plus `PreparedScalarProgramBlock`. That makes an evaluator scalar view, runtime-oriented assignment-shape discovery, and cloned per-row metadata part of IR construction, contrary to SPEC 0032 and SOLVE-C56's requirement to issue from canonical `ComputeBlock`/typed ownership before evaluator preparation or target-coordinate enumeration. The outer checked aggregate does not compensate for construction at the wrong representation layer. | No schema-58 commit is permitted from this state. Reopen construction: exact refresh assignments name canonical compute-node/program identities and compact output/target ranges; final scalar fallback returns a mechanically checked source projection; plans carry ids/ranges rather than cloned rows; assignment programs and typed call sites are frozen once; the complete coordinate certificate and compatible ordered-remainder proof are constructor- and wire-issued. Runtime and Cranelift attach execution only through that contract. The initial corrective slice adds an opaque canonical scalar-program source and a row-aligned final scalar projection, but the owner cutover receives no roadmap credit until Phase Solve no longer calls evaluator scalarization/preparation. |
| 2026-08-11 | R2 schema-58 scalar-owner performance review (rejected) | The first checked exact-assignment schedule issued one program per scalar target. It removed runtime schedule discovery, slice-address cache identity, and mixed-schedule segmentation, but expanded the 609-target schedule to 609 native programs. The real canary remained correct while five-run hot time regressed to `0.428158774 s` average/`0.427021441 s` best; a 30-run profile averaged `0.433653729 s`, with issued schedule/owner symbols dominating. | Reject this representation as target-coordinate expansion, not an optimization candidate. The checked constructor now groups only adjacent rows from one compiler-issued canonical source when a conservative register-dependency proof establishes cross-target independence. It never compares bodies, hashes expressions, or recollapses an expanded graph. Rejection artifact: `/tmp/rdd2-schema58-scalar-owner-regression.perf.data`. |
| 2026-08-11 | R2 schema-58 construction-group recovery review (open) | Construction-issued grouping recovers the compact execution scale: the seven real schedules carry programs/targets `21/21`, `212/609`, `89/211`, `84/165`, `89/211`, `153/494`, and `15/15`. Solve IR 169/169, Cranelift 59/59, and diffsol 107/107 pass; the 101-point canary ends exactly at 0.5 s. Five-run hot time recovers to `0.151031463 s` average/`0.150481490 s` best (`3.31x` realtime), close to the pre-cutover baseline but still three times above the `0.05 s` gate. | Keep the review open. The grouping proof currently duplicates evaluator dependency logic, exact programs still materialize scalar stores before the final backend boundary, Production C/GALEC still rediscover exact assignments, and solver-only tests retain a construction fallback. Centralize the dependency certificate in checked Solve IR, make all consumers use issued ids/ranges, remove fallback construction, add positive independent-group and negative dependency-order tests, then re-run review, canary, and delayed `perf`. Profile artifact: `/tmp/rdd2-schema58-grouped-owner-profile.stderr`. |
| 2026-08-11 | R2 schema-58 shared-owner corrective review (open) | Solve IR is now the sole register-to-Y dependency authority used by checked grouping and evaluator assignment recognition. An exact owner retains only its compiler-issued source, row owners, target coordinates, and checked isolator shapes; it no longer stores generated scalar operations. Cranelift and Production C materialize that scalar view only inside their final adapters. Production C consumes issued value-stage sequences instead of scalarizing and rediscovering isolators. The runtime test fallback is removed; explicit fixture helpers invoke the same production owner constructor before the strict runtime boundary. Positive independent grouping and negative sequential-dependency tests pass, as do codegen 99/99 and solver 288/288. | This closes the duplicated dependency interpretation, pre-final expression expansion, Production-C rediscovery, and hidden test fallback findings. The real canary remains correct at `0.150433864 s` best; a delayed 80-run profile averages `0.152021144 s`, best `0.149840320 s`, with 11,423 samples and zero loss. Leaders remain owner 30 `13.25%`, owner 22 `5.86%`, schedule 3 `5.07%`, and `memmove` `4.19%`; owner 30 executes 7,626 times per slice. Compact target-range ownership and the complete coordinate/invalidation certificate remain open. Artifacts: `/tmp/rdd2-schema58-shared-owner.perf.data`, `/tmp/rdd2-schema58-shared-dependency.stderr`, and `/tmp/rdd2-schema58-native-counts.stderr`. |
| 2026-08-11 | R2 schema-58 post-refactor release and maintainability review | Rebuilt the release binary after removing pre-final exact scalar programs and the runtime fixture fallback. The focused runtime suite passes 59/59; the full affected gate immediately before the file split passes evaluator 139, Cranelift 59, Solve IR 171, Phase Solve 87, codegen 99, solver 288, and diffsol 107. `git diff --check` is clean. The touched runtime test module was split by concern at the SPEC 0021 threshold and is now 1,995 lines; no path indirection or exemption was added. | The exact post-refactor binary completes 101 points at exactly 0.5 s with hot average `0.150980822 s`, best `0.150499536 s`, or `3.32x` realtime. This confirms semantic and performance equivalence within noise, but remains `3.01x` above the 0.05 s acceptance gate. Do not credit SOLVE-C56 complete: compact target-range ownership, the complete coordinate/invalidation certificate, GALEC cutover, and removal of remaining row clones are still open. |

## Current next action

Schema 58 now constructs exact assignment sequences before runtime preparation,
uses the Solve-IR dependency authority, materializes scalar execution only in
final adapters, and has no runtime or fixture fallback construction. The exact
post-refactor release reaches `0.150499536 s` best (`3.32x` realtime) for the
real 0.5 s slice. The `0.05 s` gate remains open. Complete SOLVE-C56 before
crediting the owner cutover: replace remaining cloned row schedules with
construction-issued row-owner/compact-range selections and add the complete coordinate
dependency/invalidation certificate
covering time, Y/P generations, event/pre/previous/history state, external
tables, impure state, and arithmetic/AD mode. Runtime preparation must only
attach backend storage/code to those checked objects; neither it nor Production
C/GALEC may call `exact_target_assignment_*` to rediscover programs. Derivative,
root, event, interpreter, Cranelift, GALEC, and Production C consumers must use
that same owner. The repeated 89-row
schedules and their 9-point fold helpers may not be shared by body equality or
pointer coincidence. The event transaction reduces
`MultiSensorInvariant.step` to one checked execution per tick; the remaining
`predict=502` count for 101 ticks must be addressed only if the new issued
refresh contract proves those invocations share the same semantic coordinate.
The certificate must survive the outer-program boundary instead of resetting
inside each native residual call, without hoisting inactive corrections.

Then repeat native/interpreter parity, dynamic call counts, the 0.5 s release
benchmark, and runtime-only `perf`. Optimize `solveSPD` further only if it
remains hot after duplicate schedule/transaction work is removed; the current
profile proves the complete Kalman `step` subtree is about 7.8% inclusive time,
not the order-of-magnitude gap.

The accepted 0.5 s slice must reach at most 0.05 s without Phase Solve
scalarization, inferred body equality, expansion/recollapse, interpreter
callbacks, skipped events, or changed solver/mission semantics. Only then
proceed to full optical-flow and GPS traces against OMC, shared
GALEC/Production-C lowering, and `cerebri_rdd2` eFMU/firmware mission
qualification.

## Prior next-action history (superseded)

Construct one typed `Map` structured region for binder-dependent DAE
comprehensions. It must derive its result shape from the checked compact domain
and body type, own captures and binder slots explicitly, execute a lazy
conditional body once per runtime domain point, and replay through the same
constructor. Do not lower the waypoint domain into eight bodies, tensor
coordinates, or a later recollapse pass. Complete the schema/diff review and
focused negative coverage, then rerun the zero-duration production canary; the
current exact blocker is `Planning.Bezier.prepareWaypointPlan` at source bytes
1333--1352, producing a `[8,3]` conditional comprehension.

Treat `0.458355511 s` as the corrected semantic baseline and retain the
guarded-plan regression. The latest out-of-line conditional-helper state runs
the same 0.5 s slice in about `0.690 s`: it removes the Cranelift size-limit
interpreter fallback and is `4.1x` faster than the immediate broken state, but
is still `13.8x` slower than the `0.05 s` acceptance target. Perf now attributes
about 38% inclusive time to the estimator owner and about 36% to assignment
schedules. The MSL trace gate remains the repository command
`cargo xtask repo msl -- plot-compare`. The matched OMC DASSL control removes
the apparent 65 ms estimator divergence. The construction-owned effective
time-domain certificate is implemented and a fresh comparison improves the
mean-channel result to `7.009e-4`; the remaining leading differences are
genuinely continuous plant/IMU acceleration channels rather than held-alias
interpolation. Finish the production SOLVE-C51 DAE-to-typed pure-call cutover
for the estimator owner before further final-backend tuning; its tensor and
record fields must remain compact through Phase Solve. Then re-profile the
largest necessary owner. Do not optimize metadata or comparator behavior as a
substitute for the 10x runtime requirement.

The checked model-level pure-call table, aggregate-preserving typed operation
vocabulary, and shared compact evaluator are now in place. Continue by
completing SOLVE-C43/C51 at the Phase Solve boundary:
`ScalarProgramBlock` owns one checked function-conditional table and operations
reference issued block-local ids plus compact capture/result ranges; embedded
body serialization and final-lowering rediscovery are not acceptable. Measure
preparation, JSON size, native code size, and hot runtime independently.

Lower each DAE pure function occurrence exactly once into that source-issued
owner with every result and call-scoped assertion projection. The present root
lowering builds one 674/700-operation body per assertion; the replacement must
route residual/root/action consumers through issued owner projections and then
delete the scalar inlining path. Add interpreter/native parity before measuring
the root/event reduction; do not bridge the cutover with an opaque legacy
linear region or post-expansion graph recovery.

The checked DAE-to-typed canary now lowers the complete real RDD2 step graph,
including record tensor leaves, nested compact folds, runtime projections,
slice updates, multi-arm lazy conditionals, and branch-local assertion tuples.
The next boundary is no longer missing estimator vocabulary: attach this sole
lowerer to one model-level pure-call registry used by every Phase Solve
consumer, remove the test gate and legacy scalar function inliner, and compile
typed owners once in interpreter/Cranelift. Unsupported production calls must
fail closed rather than fall back to a second semantic lowering. Re-run the
external full-step canary after attachment, then measure root/event call-frame
deduplication, preparation, native code size, and hot runtime independently.

Then specify the dynamic refresh-execution contract before implementation. It
must issue semantic stage/body identity from compiler row/output ownership,
retain compact external input and output ranges, and prove time, Y, P, discrete,
external-table, and impure-state invalidation. Instrument call coordinates to
establish whether the equal 27-program schedules execute at an identical
coordinate; only that proof may authorize result reuse. No operation hashing,
pointer identity, scalar expansion, tracing, or graph recollapse is allowed.
Apply the same DAE-owned aggregate/action contract to checked GALEC and
`SolveAlgorithmBlock`, re-profile each accepted change, and continue from the
largest necessary compact owner. R2, R3, and R4 remain open until performance,
full-route parity, single-source architecture, and firmware gates pass.
