# Issue 430 verification ledger

PR #421 was merged into main at
`9f507597dd0a462b767a9761a53f4c3e3ffdfbd8`. This issue’s commits are now
rebased directly onto that main commit. Main integration checks use clean
source checkpoint `7cd586b2246b0705acf7700ac606aa61ca62583e` and are recorded
below. All previous numerical, reset, canary, ModelicaTest and compatibility
results retain their original source commits; they are historical rather
than current-main parity claims. Artifact names are relative to
`$HOME/scratch/rumoca/issue430-20261013` unless noted.

## Reproduction inputs

| Source | Immutable revision | Attribution |
| --- | --- | --- |
| [ALSETLab/rumoca-drone-library](https://github.com/ALSETLab/rumoca-drone-library) | `62d6d12f216a89f2df12b26b312d61ec411f86b2` | BSD-3-Clause reproduction, DroneHoverLoop and root adapter |
| [ALSETLab/Modelica-Drone-3D-FMI](https://github.com/ALSETLab/Modelica-Drone-3D-FMI) | `6fbfe933af0c878f9308c38eb93e588a1fb5cdd9` | BSD-3-Clause; Luigi Vanfretti, 2019 |
| [Modelica/ModelicaStandardLibrary](https://github.com/modelica/ModelicaStandardLibrary) v4.0.0 | `6626538a236a34a390655b55d5e8c906bfe31772` | Modelica Association; BSD-3-Clause |

`downloads/reporter400` combines Modelica, ModelicaServices, Complex.mo,
DroneLibrary and DroneHoverLoop.mo in one source root. It applies the exact
reporter's Math external-builtin-to-algorithm substitutions and the three
missing empty DroneLibrary package shims described in the pinned
`tools/build_rumoca_roots.py`. Official ModelicaServices 4.0.0 already lacks the
OpenModelica-specific service calls targeted by that adapter. No equations,
starts, tolerances, or solver settings were changed to repair the model.
OMC was run on this same adapted root.

The unadapted combined root has an independent external Math derivative
structural failure and does not reach initialization. It is not a substitute
for the reporter's executable input context.

## First divergent producer

On the clean da102 stacked base, compilation succeeds and initialization
fails at lambda 0, continuation step 0.125:

```text
row=2178 target=droneChassis1.bodyShape.frame_a.R.T[1,3]
value=-4.166877e0 ratio=4.166877e6 norm=4.166877e0
row_scale=1.000000e0 scaled_tolerance=1.000000e-6
```

Artifact: `baseline-reporter-loop-da102.log`. The reduced FixedClockDomain
model fails at projection row 3, target q, with a NaN residual
(`reduced-before-da102.log`). The equivalent source is retained by the
`initial_relation_truth` simulation tests.

The earliest divergence is a buffered relation read during the first
simultaneous initialization iterate: cold false event memory selects the
flight-time ramp's wrong branch. The compiler previously lowered every such
read to its transient held memory. Algebraic refresh runs before the driver
can update relation memory; the wrong branch reaches a rotation projection
which cannot converge. A controlled historical probe with exactly unchanged
states, starts and tolerances succeeds when current relation truth is settled
before that projection. The fix evaluates the authored comparison at every
initialization iterate, including projected unknowns and algebraic operands,
then uses held event truth after initialization.

| Competing hypothesis | Rejection evidence |
| --- | --- |
| Zero orientation declaration start | Selected quaternion scalar seed is 1; unchanged starts succeed when only relation evaluation ordering changes. |
| Stale reset state | PR #421 baseline simpleHoverTest advances four seconds, changes 4436 of 8305 visible values, then resets all values bit-identically and advances again. |
| Math root adaptation changes the ramp failure | Latest adapted roots reproduce the exact reported rotation entry and value; OMC accepts and simulates these same roots. |
| A looser projection tolerance is necessary | The repaired initialization uses unchanged 1e-6 projection tolerance and all original starts. |

## Fixed 20-model canary (Tier 1 only)

Baseline uses MSL v4.1.0 and OMC `a96aa1a-cmake`, exact OMC reference context
`12ae478f4a9ff9e3e5e34945452e90f9e8d36650b90459b140dd3329f42dff9d`.
Target file: `infra/verification/msl-canary-20.json`.

The first clean da102 attempt completed stages but could not run the comparator
because the modelica shell lacked Node. `canary-before-da102.log` retains that
failed comparator discovery. It is not a passing parity run. A separate full
run with Node 24 available is retained as `canary-before-da102-node.log`; that
run overwrote the results JSON in `canary-before-da102`, so those JSON files
belong only to the second run. No per-model failures were selectively retried.

| Metric | Clean da102, independent Node-corrected run | f6b99d6 change on da102 |
| --- | --- | --- |
| Parse / flatten / DAE / Solve | 20 / 20 / 20 / 20 | 20 / 20 / 20 / 20 (delta 0) |
| Simulation completions | 16 | 16 (delta 0) |
| Compared / high / near / deviation | 15 / 15 / 0 / 0 | 15 / 15 / 0 / 0 (delta 0) |
| Typed skips / missing | 1 / 0 | 1 / 0 (delta 0) |

This historical partial tripwire is not full-cohort parity.

## Historical focused after evidence

The da102 stacked-base build with this change runs DroneHoverLoop for four seconds without the
reported projection failure. `focused-trace.json` compares 402 Rumoca output
points against 404 OMC points from the exact same reporter400 roots, using
linear interpolation on the 401-point 0.01-second grid and the final sample at
repeated exact timestamps. All 8309 runtime channels are finite. All six
inspected initial values agree exactly. Maximum altitude difference is
0.0001035544 m; the previously failing rotation entry differs by at most
1.7221e-8 and the matrix orthogonality residual stays below 1.8641e-10.
These are focused observations, not a full-cohort certification.

Raw artifact SHA256:

- `loop-after-da102.json`: `ac322f39461131ac588985dd1adbcbef95e57700980c47302ba91e9c6ad636ed`
- `omc-reporter400/DroneHoverLoop_res.csv`: `e4103db971b0f54e05d1d4eb543c49daf11eb1d06e5bb6bc3f30f4ca0f658b06`

Both real models exercised the live FMI Model Exchange session reset after four
seconds of changed flight state and warmed evaluation caches:

| Model | Visible values | Changed at 4 s | Different after reset to 0 | Advance to 4 s again |
| --- | --- | --- | --- | --- |
| DroneHoverLoop | 8309 | 4453 | 0 | success |
| DroneLibrary.Tests.simpleHoverTest | 8305 | 4436 | 0 | success |

Reset evidence covers four seconds, not the original reporter's 27-second
failure point. PR #421 already restored pristine reset state; this PR repairs
initialization relation reads shared by session creation and reset.

Checks measured on da102 plus the change:

- `cargo fmt --check`: passed.
- `cargo test --profile msl-fast -p rumoca-ir-solve -p rumoca-phase-solve`:
  900 + 267 unit tests, 3 integration tests and 9 doc tests passed.
- `cargo test --profile msl-fast -p rumoca-sim initial_relation_truth`:
  all five regressions passed; given/projected/algebraic/function-argument
  cases run both BDF and RK. The lazy-call case deliberately withholds the
  typed call table, proving held truth never executes its inactive call while
  initialization requires that call's checked owner.
- The broader sim library run passed the other 210 tests; its old fifth test
  made an incorrect assumption about low-level assertion/domain observation.
  The corrected focused suite above replaces that test and is green.
- Pinned modelica_models CI revision
  `a41f7c0c00b55c1bf54f03c9b66b901ba8e43c6f`: Tests.All compile and
  Tests.LieGroupTests.SO2 / SE2 assertion simulations at t0 passed. Exact
  downloaded archive SHA256:
  `cc10b1a8994d31ab7f20ece0d9d99aca927c56a98ba9dd323d7bac267e98c578`.

Remaining workspace/documentation, coverage and full MSL gates are pending;
the current ModelicaTest result is recorded below. The PR stays draft until
required validation is accounted for.

After rebasing only this change onto eb515, all 268 phase-solve tests passed,
including the new parent Real signum cases and this change's wire/relation-mode
cases. All five simulation initialization regressions passed on eb515.
Numerical/canary measurements above retain their original da102 context.

## eb515 source checks (historical)

On `2d684832794f10d5288b5adbe0af31781b1e475f` (eb515 base), the exact
reporter-root DroneHoverLoop initializes and advances to 0.01 s successfully
(`current-loop-initialization-eb515.log`). Four-second trace/reset numbers
above remain the earlier da102 measurements.

The current ModelicaTest semantic gate passes its one selected model,
ModelicaTest.Blocks.MuxDemux: compiled, simulated and compared, high agreement,
zero skips/missing/near/deviation. Its first invocation stopped at discovery
because the release library omitted ModelicaTest; no model phase was attempted.
The independent prepared-source run uses CI's exact pinned package revision
`8ae3d35c24e519cb2996cab20f3b13daf2b0c50a`, archive SHA256
`b402903aefd4f1397364e3ae1992bb99080ce935ab9a426d3bcdb47e4da8eeb6`.
Both logs are retained (`modelicatest-current.log` and
`modelicatest-current-prepared.log`), with the latter's JSON in
`modelicatest-current-prepared`.

The committed non-coverage verification gate for 2d684832 passed fmt,
CI-scope workspace clippy and lint, then failed workspace tests: 121 Rumoca
library tests passed and the scalar-event FMI render test rejected the new
relation initialization-mode lane as an authored initial event. The first
failure and exact log digest are retained in `gate-first-failure.json`; later
gate steps were not attempted. Coverage requires an
unmeasured fresh instrumented target (the tool estimates approximately 50 GB)
and remains pending because available scratch capacity is below that bound.
Full 566-model MSL validation remains pending. That earlier evidence-only update retained the measured revision’s compiler
and test source.

## eb515 Tier 1 canary delta (historical)

`current-canary.json` retains the clean eb515 pair: before at
`eb515d9b01ef71cc945774578b8e38d733a4b67f`, after at
`1b88adcc993a85a3fe67edf677c127e2bafae7a1`. Both use the fixed 20-model list,
MSL v4.1.0 and OMC a96aa1a-cmake, with exact source/reference context
`ec011871d9a06f0d2670fd4fef02dcdbba0d44800d52ee6a6000072ca044077f`.
The optional CI-pinned ModelicaTest package is installed but excluded from
this list. Receipts are in `canary-before-eb515` and `canary-after-eb515`;
no result directory was replaced and no individual phase was retried.

| Metric | Before | After | Delta |
| --- | --- | --- | --- |
| Parse / flatten / DAE / Solve | 20 / 20 / 20 / 20 | 20 / 20 / 20 / 20 | 0 |
| Simulation completions | 16 | 16 | 0 |
| Compared / high / near / deviation | 15 / 15 / 0 / 0 | 15 / 15 / 0 / 0 | 0 |
| Typed skips / missing | 1 / 0 | 1 / 0 | 0 |

The archived gate has CI's exact scope: clippy uses workspace/all-targets,
excludes rumoca-phase-instantiate, and does not enable all features; lint uses
`cargo xtask verify lint`. Workspace tests exclude rumoca-bind-python and
rumoca-bind-wasm, enable rumoca/msl-sim-tests, and skip snapshot-only
commit_messages. Its completed steps are not a claim that the literal
SPEC_0025 all-features clippy or unrestricted workspace command has passed.

## Generated C admission and lifecycle counterexample

The standard workspace gate exposed a second owner of the same execution
mode: the scalar C profile inferred unsupported authored initial-event logic
from storage presence, and neither FMI template raised/cleared that storage.
Removing only the refusal would therefore have admitted the wrong kernel.

C admission now derives allowed reads from the checked compiler relation
regions using the canonical model visitor and shared parameter-read footprint
and exact scalar-parameter-output helpers. The exact flag guard, stored raw
comparison and relation-memory fallback grant relation-only initialization;
every other scalar/indexed/tensor/nested mode read refuses. Both FMI versions
raise the original hidden lane at EnterInitializationMode, retain it through
initialization and the initial event, and clear it before transient refresh.
Reset returns to its original cleared lane. No schema or metadata field changes.

Owning checks include a source authored initial() refusal, adversarial compiled
mode-read fixtures, and actual FMI2/3 C APIs that initialize, observe a derivative,
change a trial state across a relation without committing an event, observe held
truth, reset, and initialize again. The follow-up passes all four
actual C cases on the eb515 source checkpoint. The 904 IR / 268 phase unit,
3 integration and 9 doc tests also pass; scoped all-targets strict clippy and
fmt pass. `fmi-mode-followup.json` retains exact counts, scope, first failure
and log digests. The stronger algebraic fixture initially exposed the exact
Y-only AD guard/fallback with distinct positive-zero tangents; negative alias,
nonzero and negative-zero controls protect its private C admission proof.
The earlier workspace gate remains a failed attempt.

## bd6 source checks and Tier 1 delta (historical after parent rewrite)

`bd6-checks.json` binds all current checks to clean source checkpoint 8c34dd9e.
The full owning suites pass 904 IR and 270 phase unit tests, 3 integration
and 9 doc tests. All five initialization regressions, all four actual FMI C
cases and all 122 Rumoca library tests pass. Both FMI 2 and 3 initialize the
strong algebraic reconstruction probe, preserve held truth during an
uncommitted trial across the root, reset and initialize again. Authored
initial-event refusal, pulse/state/time-event coexistence and optimized C
warnings also pass. Formatting, scoped all-targets default-feature strict
clippy and the canonical traversal-policy check pass.

The exact reporter DroneHoverLoop initializes and advances to 0.01 s on this
current source (501 points, 20.54 s). Pinned modelica_models Tests.All compiles;
SO2 and SE2 assertion simulations at t0 pass. The CI-pinned prepared
ModelicaTest MuxDemux gate passes one compile/simulation/high comparison, with
zero skipped, missing, near or deviation models. Source pins/archive digests
above are unchanged. The profiling CLI first refused its omitted opt-in
feature before any model attempt; the captured enabled build is explicit.

`bd6-canary.json` records the clean shared bd6 baseline from
`$HOME/scratch/rumoca/issue431-20261013/canary-before-bd6` and this branch’s
clean 8c34dd9e after run in `canary-after-bd6`. Both use the fixed20 list,
MSL v4.1.0 / OMC a96aa1a-cmake and exact context ec011871... shown in the
receipt. All 20 parse/flatten/DAE/Solve/balance outcomes, 16 simulations,
15 compared models/high bands, one typed comparison skip and zero
missing/near/deviation models match. Every selected model’s phase, balance,
initial closure, simulation and initialization outcome matches individually;
all deltas are zero. This is partial Tier 1 evidence, not cohort parity.

The after log explicitly recompiles IR-Solve, phase-solve, sim and test-msl
from this source. Changed-source mtimes were not restored. The historical
eb515 after log likewise shows Dirty IR-Solve followed by producer rebuilds;
its digest is retained. No individual canary phase was retried and no result
directory was replaced.

The first broad gate failure stays preserved in gate-first-failure.json. The
current library test that failed there now passes, but the broad gate’s later
steps have not been completed. Remaining workspace/doc, literal all-features
workspace clippy, coverage and full 566-model MSL validation are explicit gaps.
The approximately 50 GB coverage estimate exceeds available owned storage.
The stacked integration-base PR triggers DCO; standard PR CI is filtered to
main-targeted PRs. The PR remains draft. Historical real-model reset evidence
covers four seconds; the original reported 27-second point is unmeasured.

## Latest 654 parent refresh

After the completed bd6 measurements, PR #421 was rewritten to 65478f8e.
Its four-file delta removes the literal builtin selection test and restores
structural builtin constant folding/spec behavior. Only this issue’s own
commits were rebased; no obsolete parent history was retained.

`latest654-checks.json` binds the newest checks to clean source b1de69c7.
All 268 phase unit tests, five initialization regressions and four actual
FMI C cases pass. Formatting and scoped all-targets default-feature strict
clippy pass. DroneHoverLoop initializes and advances to 0.01 s (501 points,
18.55 s); pinned Tests.All compile and SO2/SE2 t0 assertions pass again.

Cache reuse restores timestamps only for files proven byte-identical to the
last measured own source. The changed structural implementation, expression
module and spec row keep fresh checkout timestamps; the removed test stays
removed. No baseline binary rebuild intervened between those own snapshots.
Current logs explicitly rebuild changed structural/codegen/phase/compiler/sim
and profiling-worker producers. The exact cache/source proof is retained.

The latest paired canary is pending after the moving parent; the completed
bd6 pair and selected ModelicaTest result remain historical, never relabeled
as latest. Broad workspace/doc/all-features/coverage/full-MSL and original
27-second reset gaps remain as recorded above. The final evidence update
changes no compiler or test source from measured b1de69c7; the PR remains draft.

## Main integration after PR #421 merge

`main-integration.json` records the replay of only this issue’s commits onto
main 9f507597 after the parent squash merge. Rebase required no textual conflict
resolution. All five upstream native-library/extremum files are preserved
byte-for-byte. Independent review confirmed the original issue patches are
unchanged, including mode ownership, C admission, affine bounds and lifecycle.

Clean source 7cd586b2 passes 904 IR and 268 phase unit tests, three integration
and nine doc tests, all five initialization regressions and all four actual
FMI C cases. Formatting and scoped all-targets default-feature strict clippy
also pass. Cache reuse restores only files proven byte-identical to the last
measured own source; the five changed main files keep fresh timestamps.

The user explicitly requested resolving the integration and merging both issue
PRs. The coordinator handles merge after the current focused checks. Earlier
main-only CI filtering and draft statements above describe their historical
stacked tips; this PR now targets main and standard CI is applicable. Remaining
broad coverage/workspace/doc/all-features/full-MSL and main canary/ModelicaTest
checks are recorded honestly; no broader pass or parity claim is added.
