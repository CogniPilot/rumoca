# Retained Solve template work ledger

This work retains the original analysis DAE and the correlated checked Solve model from one
compilation. Requested representations remain independent; rendering reads the retained views.
The standalone JavaScript executor in Taskyon is still optional and incomplete. This ledger does
not certify its complete runtime contract or FMI compliance.

## Main refresh

The integration checkout now uses upstream `main` at `bd153c97` (109 commits beyond the
previous release checkout). Fifteen retained-compilation tests pass against the integrated
source. The original dirty source was backed up before the branch refresh; no changes were staged.
Upstream now owns complete-pass condition-memory advancement and pulse-history release, replacing
the earlier local complete-pass fix. The local candidate-runtime-alias regression and fix remain.
The upstream fold template boundary exposes `update` and optional `continuation` regions; the JS
executor must implement early exit before each iteration, matching the native evaluator.
WASM, native runtime, checked-fixture and shared diagnostic verification of this refresh is pending.

The last pre-refresh fixed-20 run is unchanged from its pre-manifold-view baseline after excluding
timing fields: 14 compiled, six ToDae refusals, 14 simulation attempts, 12 completions, two solver
failures, no NaNs or timeouts. OMC reference parity remains unmeasured. This comparison does not
certify the refreshed upstream compiler.

## Focused native evidence

- On refreshed main, all 15 retained compilation tests and 17 focused WASM simulation-runtime
  tests pass. These include a checked native tearing render view: certified causal assignments
  and residual programs retain their native order and source provenance. The JS regression
  solves one tear coordinate while reconstructing two algebraic observations under changing
  inputs, with all three integrators, in Node and the headless browser.
- All four exact Taskyon editor samples complete their experiment horizons with all three JS
  integrators in the shared Node and headless-browser diagnostic. The separate MSL Resistor diagnostic passes in
  Node and the headless browser: Newton blocks contain at most two coordinates, and the
  reconstructed observations satisfy the retained implicit equations to 1e-7. A direct JS
  proof measured a worst residual of approximately 2.7e-10. The broader 47-test run had 44
  passes, one explicit manifold refusal and two timeouts under concurrent compilation;
  the editor-sample and event-transaction diagnostics pass when rerun separately.
- The completed refreshed-main browser suite has 48 Solve diagnostics: 47 pass, including the
  additional Resistor equation-consistency regression, and the one manifold diagnostic retains
  its explicit pending-capability refusal. This does not certify trajectory sensitivities,
  external bindings or the separately rejected BooleanNetwork1 compilation.
- The regenerated checked-program inventory now has 96 synthetic cases, including a fold
  continuation that exits before the next iteration. New Node/browser proofs also exercise
  signed Integer modulo/remainder and whole-pass clock pulse condition history. These counts
  do not certify all MSL examples: BooleanNetwork1 still encounters main's checked-profile
  refusal for a return nested in a function loop before Solve lowering.
- The standalone numerical solver's rectangular state-projection callback has a Node/browser
  proof for minimum-norm nominal-scaled corrections and rollback of a failed trial. The JS
  template still refuses manifold models pending complete checked chart and event-reset
  integration; this numerical capability alone does not certify generated manifold execution.

- Retained WASM compilation tests exercise representation selection, independent rendering,
  disposal ownership, and preserving DAE inspection after a Solve refusal. The checked
  `solve_model.visible_value_plan` exposes private discrete storage through the same scalar
  template plan owner as numerical programs, while preserving the original visible rows.
- Integer tensor directional lowering keeps non-Real primal outputs without manufacturing Real
  tangent registers. `integer_tensor_directional_owners_preserve_primal_without_real_tangents`
  checks matrix products, cross products, scaling, and broadcasting. The six directional tests pass.
- Event message formatting preserves integer trailing zeros, including `String(120,
  significantDigits=3)`. The regression failed with `12` before the shared formatter fix.
  Seven focused event evaluator tests pass after the fix.
- Event message template plans reuse the existing checked message-row extraction used by FMI
  codegen. Value and formatting rows are exposed independently of literal UTF-8 segments.
- `checked_solve_fixture_export_matches_native_evaluation` exports synthetic checked scalar
  template plans only when an explicit output environment variable is supplied. Seven fold cases,
  all three random generator state/result profiles, and 84 table cases are compared with the
  native evaluator. Taskyon diagnostics execute the same template macros against those views.
  Table cases cover values, slopes, bounds, selected columns, next events, three interpolation
  profiles, four extrapolation codes, and interior/boundary/exterior queries.

- Checked clock render views expose scalar/guarded producer ownership, scalar program output
  indices, exact rational period/phase strings and anchors, row-aligned clock history schedules,
  and compiler-issued intermediate targets and consumer clock sets. Fourteen retained compilation
  tests pass with the simulation-enabled build. The standalone JS diagnostics verify coincident
  clocks, off-lattice absolute starts, simulation-start anchors, clock-owned history, intermediate
  refresh order, atomic transactions, assertion rollback, warning-only commit, and ordinary
  discrete pre propagation and structured ordinary/clock maps in Node and the browser.
- Native event condition memory now advances after a complete equation pass, so an active when
  equation remains active while its algebraic dependency settles. The previously failing
  `event_equations_hold_condition_history_until_algebraics_settle` certifies both equations.
- Native joint-mode search now propagates candidate relation memory through runtime aliases before
  projecting algebraics. `relation_mode_search_propagates_candidate_runtime_aliases` failed with
  nonconvergence before this change and now finds the consistent two-relation mode. The checked
  root-neighborhood render view reuses the native dependency-analysis owner.
- Fourteen focused WASM simulation-runtime tests and 128 native runtime tests pass. The JS proofs
  cover fixed-pre algebraic projection, coupled recovery of a two-value Picard cycle, runtime alias
  chains, reset-driven algebraic relation changes and a joint-mode search with eliminated algebraics,
  through all three JS integrators. The shared Node/browser suite had 36 Solve tests at that checkpoint.
  At that checkpoint these checks did not certify impure random rollback, manifold/reduced-chart or
  trajectory/event sensitivity behavior.
- Checked delay render views expose the same three scalar plan owners as native delay execution.
  A native zero-delay regression proves that a forward trial with a changed discrete source uses
  its current source value before that point is accepted. All twelve focused delay tests pass.
- JS accepted-history lifecycle and delay proofs exercise accepted internal points, rewind of
  rejected RK45 and localization trials, equal-time event left/right commits, delayed feedback,
  variable-delay forward/backward crossings and same-instant zero-delay source updates through
  all three integrators. The caller can bound numerical steps independently of output spacing.
  Delay-directional callbacks remain unavailable until history directions are implemented.

- After delay support, 40 shared Solve diagnostics passed in Node and the browser. After impure
  operation support, 41 passed in both runtimes. The checked fixture exporter adds one native
  impure-stream case (95 total fixtures), including repeated and distinct full-u64 call sites and
  reversed Integer bounds. JS stream snapshot tests prove continuation replay, failed-event rollback,
  model-instance isolation and initialization reset. These are checked-program proofs, not MSL
  random-model trace evidence.

## Tier 1 fixed canary delta

The fixed `infra/verification/msl-canary-20.json` roster was rerun after the Integer directional
and message formatter fixes, then again after adding the checked visible-value template plan.
The clock render-view additions were also rerun twice, including the intermediate plan views.
Structured-map views, both native event-history/mode-search fixes, and delay views/zero-delay
execution were each rerun on this roster.
All comparisons against their preceding working-tree run retain all 20 per-model phase,
shape, completion, and refusal outcomes are unchanged:

| Outcome | Before | After |
| --- | ---: | ---: |
| Compiled DAE | 14 | 14 |
| ToDae refusal | 6 | 6 |
| Simulation attempted | 14 | 14 |
| Simulation completed | 12 | 12 |
| Solve preparation failure | 2 | 2 |
| NaN or timeout | 0 | 0 |

The parity gate fails because its OMC reference data is absent. **Parity is unmeasured.** These
completion counts are a partial canary tripwire, not cohort parity evidence. Generated reports
remain outside tracked source and are not intended for publication.

## Refreshed main fixed-canary result

After integrating main and the checked tearing view, the same fixed 20-model roster has 17 DAE
compilations and 15 native simulation completions, up from 14 and 12 respectively. Digital Counter,
Digital FullAdder and QuadratureLobatto3 now compile and complete. BooleanNetwork1 and rotational
Friction still refuse in ToDae. AutomaticSeed now refuses earlier, in Flatten, for unordered foreign
library state; that phase change is not a successful compilation.

The two Solve preparation failures remain OpAmpCircuits.Add (structural singularity) and TransformerYD
(a derivative coefficient that is not compile-time numeric). There are no NaNs or timeouts. The native
pipeline took approximately 42 seconds after building its executables; this is a 20-model run, not
one model's solve time. OMC is unavailable, so the parity gate fails explicitly and parity remains
unmeasured. Native completion does not establish standalone JS execution for these additional models.


## BooleanNetwork1 regression restoration

The refreshed main checkout retains Thomas Meschede's earlier compilation fixes
(`a0dca8e4a`, `fcc77f340`, and their ancestors). The checked function-construction change
`f6d041708` subsequently rejects the finite-loop early return used by MSL
`Modelica.Math.BooleanVectors.firstTrueIndex`; this is a later unsupported path, rather than
an identified revert of those earlier fixes.

Function construction now carries one checked mutable return state across nested finite loops,
guards later statements and iterations, and retains runtime output-definedness assertions.
No MSL-specific function shim or model rewrite is used. While-loop returns remain unsupported.
The retained-function suite passes 17 tests; targeted return, guard-fact, and branch-snapshot
suites pass 5, 24, and 6 tests respectively. The shared Taskyon loop-return diagnostic exercises
changing inputs, nested loops, first-match selection, tail suppression, and unreachable faults
with all three JavaScript solvers.

Native evaluation also preserves lazy selection in short rows and selected conditional regions,
including captured values. The evaluator suite passes 282 tests. A separate native event
regression changes a discrete index before its cached relation truth is refreshed: a transient
checked gather fault now permits the existing relation refresh before another event pass. The
regression fails before the fix and passes afterward; an active out-of-range index still faults
at the settled relation side. All 12 focused event-iteration tests pass. This changes native event
execution, not the model's equations or index bounds.

The fixed 20-model Tier 1 canary against the refreshed main baseline changes from 17 compiled
and 15 simulation completions to 18 compiled and 16 completions. BooleanNetwork1 now passes
both initialization and native simulation. There are no NaNs or timeouts. The remaining failures
are AutomaticSeed (Flatten), Friction (ToDae), OpAmpCircuits.Add (structural singularity during
Solve preparation), and TransformerYD (nonconstant affine derivative coefficient).
OMC reference output is unavailable, so the parity gate rejects this partial run as unmeasured;
these completion counts are not parity evidence or full-MSL coverage.

The local full-web WASM package was rebuilt from main plus these uncommitted changes and
installed in Taskyon; generated and installed binary hashes match. The final rebuilt binary passes the BooleanNetwork1 regression in both Node and the
headless browser, preserving 1001 samples, expected Boolean transitions, and the no-Newton
algebraic-discrete assertions. The nested-loop-return diagnostic also passes independently in
both runtimes. The refreshed build reports `bd153c9788bb-dirty`; the package version remains
`0.10.2`, so the git revision and rebuilt capability checks identify this source build.


The final headless browser `Modelica Solve` filter passes 47 of 48 diagnostics. The only
failure is `Test Modelica Solve Generator Manifold Projection`, refused explicitly during
rendering because JS manifold projection is pending. BooleanNetwork1 is verified separately
because its diagnostic name lies outside this filter. This does not complete the full Solve
executor contract or change the generator default. The TypeScript check still reports the
pre-existing P2P errors at `src/p2p/tyP2p.ts:337` and `:341`; it reports no
Modelica errors.
