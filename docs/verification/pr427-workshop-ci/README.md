# PR 427 workshop CI coverage inventory

The complete selected workspace census at `301b4d566838` ran all 10,352 tests
across 159 binaries: **10,347 passed, five failed, none skipped**. No resource
stop occurred ([receipt](workspace-census-301-complete.json)). The failures are
the unused public accessor, a conditional-chain fixture whose routing premise
changed, the discrete-Real derivative distinction, and two same-tick cycle
models that incorrectly simulate. The cycle failures are correctness blockers;
they are not expected successes. Canonical lint passed at the same snapshot
([receipt](canonical-lint-301-checkpoint.json)). Final combined qualification,
quick/full and fresh remote CI remain pending.

The unused accessor is removed; its existing API ratchet and removed-surface
guard both pass at `bab791f3e`, along with scoped Clippy and formatting
([receipt](unused-same-tick-api-qualification.json)). The reported #403/#411
source paths now pass actual native-WASM execution at `9aa5511d8`: all 228
IR-DAE and 152 WASM tests pass, with scoped lint and formatting
([receipt](../pr427-reported-native-wasm-20261012/README.md)). These repairs
are integrated, but their peer qualification does not certify the combined tip.

The conditional-chain fixture now reaches its intended scalar emission path:
its exact test passes at `822a0d5bd0ea`, retaining all 3,000 links, a 256 KiB
lowering stack, the scalar-op census and the exact 3,000.5 result
([receipt](conditional-chain-822-checkpoint.json)). Its earlier same-source
attempt stopped before any test at the host reserve; that resource stop is
preserved separately.

The [declared discrete Real repair](../pr427-discrete-real-source-20261012/README.md)
now rejects missing source when ownership with ED024 and retains legal when,
input and clock distinctions. Full DAE444, Solve198 and compile439 pass at
`fb0dafc724`; source17, scoped lint, formatting and the legal source-to-Solve
CLI proof pass at `67deb142b`. Source17 overlaps compile439. Its numerical
checked-DAE derivative control remains distinct from source validity. These
repairs are integrated; the complete combined snapshot remains unqualified.

The [scheduled-cycle and event-ordering repairs](../pr427-scheduled-current-cycles-20261012/README.md)
pass structural288, the original two cycle refusals, all 45 simulation contracts,
and both entry/rescheduling controls at `a6c70a34e`. Scoped lint and formatting
also pass. All four ordinary ordering controls pass, including the unchanged
entry-precedence oracle. Its standalone phase suite has 201 passes and the
inherited derivative assertion failure; the separately qualified declaration
repair corrects that assertion in the integrated branch. The combined 202-test
phase suite and whole-workspace result still require execution.

Earlier checkpoints: the [function-definedness repair and original Booster
prefix](function-definedness-compound-writer-qualification.md) pass; all 109
existing browser runtime/playground Node unit tests pass at `d4e415276`
([receipt](browser-unit-configuration-qualification.json)). FullWeb at
`e04d59ad1` passes three artifact smokes, four browser guide scripts and all 16
authored cells, including the safely landed Booster mission
([receipt](fullweb-e04-checkpoint.md)). The AD-only registry checks pass
([receipt](ad-verification-registry-79-qualification.json)). The combined
[event dependency](../pr427-event-derivative-dependencies-20261012/README.md)
checkpoint passes 80 selected tests at `59bc5fd74493`
([receipt](combined-dependency-59-checkpoint.json)); the
[ordinary trigger-buffer repair](../pr427-ordinary-trigger-role-20261012/README.md)
passes its two constructor controls. Its phase suite reports 197 passes and the
one discrete-Real derivative failure now seen in the complete census.

The earlier `7d56996e4` census stopped at the host memory reserve after 1,240
passes and two failures, leaving 9,084 selected tests without a terminal result
([receipt](workspace-census-7d569-resource-stop.json)). The unpublished signoff
rewrite preserves all 433 source trees and subjects
([receipt](dco-source-equivalence-59.json)); earlier baked artifact identities
remain attached to their actual runs. Historical raw MSL results and OMC-cache
data from `2b50ac92a` remain recoverable from
[verified archives](terminal-msl-storage-archives.json). The inventory below
records its named earlier checkpoints.

Source baseline: `cfe53b91d18d53c988ea2b391f2bdde8f8697e5b`. The current candidate
replaces its incompatible Ball/JAX configuration with event-free SympyDecay/JAX.
This is a source-derived roster and candidate regression record. The focused
LSP gate passed at `169ed612c`; the six-model OMC and Node gates passed at
`c758abd70`, as recorded in the [actual qualification receipt](qualification-c758-20261011.md).
Manual PR claims are not CI evidence.

## Candidate CI coverage

| Actual input or workflow | Functional assertion | CI selection |
|---|---|---|
| `examples/models/Circuit.mo`, `Circuit.Test` | Reload all three Modelica exports in Rumoca and OMC; compare scalar equation/variable counts, initial values, and circuit observables over 0–0.002 s. Covers flattened component names and quoted identifiers. | Template Runtime (modelica), `modelica_interchange_runtime::workshop_examples::` |
| `SympyDecay.mo`, `FmiTensorDecay.mo` | Same reload and independent short OMC trace; preserve scalar/tensor starts and coordinate count. | Same Modelica lane |
| `NeuralODETensor.mo`, `NeuralPredatorPrey.mo`, `NeuralODEBackprop.mo` | Same independent OMC checks on actual default dimensions; preserve arrays, matrices, comprehensions, loops, elementwise expressions, and fixed starts. Compare states and algebraic outputs, including neural loss/derivatives. | Same Modelica lane |
| All five TOMLs in `examples/codegen/` | Request `getScenarioConfig`, verify actual model/target/output directory, use the referenced `.mo`, invoke `rumoca.workspace.renderTarget` with scenario URI, require successful complete artifacts. Custom directory/raw template resolve relative to the scenario; negative controls fail beside the model source. TOML is never injected as a Modelica document. | Workspace tests, `cargo test -p rumoca-tool-lsp workshop_codegen_tests::` |
| Actual playground default workspace | Fetch actual tracked codegen files through the loader and require every tracked scenario to appear with its complete original content. Prevents the renamed JAX demo disappearing from the browser workspace. | Playground CI, discovered `default_workspace.test.mjs` within `cargo xtask playground test` |
| Actual `Ball.mo`, independent direct `jax-ode` request | Require `unsupported-feature:events` and no success artifacts. Ball retains its event semantics; the configured JAX demo now uses existing event-free SympyDecay. | Same LSP workspace tests |
| Actual `SympyDecay.mo`, direct CasADi command | Require checked callable and initialization surface. This is an additional request regression, not a sixth tracked scenario. | Same LSP tests; independent numerical CasADi proof belongs to companion backend tests |
| Actual `FmiTensorDecay.mo`, direct FMI2/FMI3 commands | Require every manifest-declared source project file, ME/CS metadata, scalar FMI2 vs tensor FMI3 dimensions/starts, build description, and valid minted identity. These are source previews, not built or executed FMUs. | Same LSP tests; FMI runtime/conformance remains in its existing lane |

The existing xtask Modelica filter is exactly `modelica_interchange_runtime::`.
The new child module is beneath that filter and discovered recursively by
`gate_runs_every_declared_test_of_every_pinned_target`; no selector is weakened.
The Modelica CI lane passes `--require-external-tools` and provides OMC via
`ci-template-modelica`. Each OMC compile/simulation command is bounded by GNU
`timeout` at 60 seconds plus five seconds before process-group kill. Missing
OMC or GNU timeout therefore fails that lane. Missing tracked
fixtures or output files always fail. Ordinary local runs retain the existing
optional-OMC policy.

The comparison fixes OMC solver tolerance at 1e-10 and compares each selected
observable from settled initialization onward with tolerance
`1e-8 * (1 + abs(expected))`. Initialization is the last row at exactly time zero
in each trace, preserving the settled side when OMC emits duplicate initial rows.
Subsequent sample grids must match exactly without interpolation or dropping
event rows. Original/generated source must produce finite, nondecreasing time,
finite channel values, unique nonempty channel names, and rectangular data.
Missing coordinates, empty traces, failed simulation, or missing files fail. No source
rewriting, changed model parameters, retries, or model-name bypasses are used.
This is a bounded six-model tripwire, not a cohort parity claim.

Circuit source declares `src.v` as an algebraic Real through `TwoPin`, then
equates it to parameter `src.V = 5`. The actual original OMC CSV omits that
constant alias. The predeclared measured channels are therefore `cap.v` and
`res.v`, with settled starts 0 and 5, and every original/export sample must
independently satisfy `cap.v + res.v = 5`. No replacement CSV column is minted.

## Tracked codegen scenarios

| Scenario file in `examples/codegen/` | Actual model | Target | Candidate verdict |
|---|---|---|---|
| `rumoca-scenario.sympy_decay_jax_ode.toml` | SympyDecay | jax-ode | Checked RHS callable and actual state/parameter defaults |
| `rumoca-scenario.galec_counter_production.toml` | GalecCounter | galec-production | Five algorithm/production source preview files; no eFMU metadata/package claim |
| `rumoca-scenario.sympy_decay_c_ode.toml` | SympyDecay | c-ode | Header and derivative C source |
| `rumoca-scenario.sympy_decay_checked_dae_report.toml` | SympyDecay | checked_dae_report | Complete checked DAE report |
| `rumoca-scenario.sympy_decay_custom_checked_variables.toml` | SympyDecay | custom_checked_variables.jinja | Checked state/parameter metadata and equation owner |

The five-row roster comes from tracked files. No tracked SymPy target or SymPy
codegen scenario exists at this baseline. The PR body's reported six
CasADi/SymPy/FMI workflows and 32-model manual roster are not reconstructable
from these files and are not counted as coverage. SympyDecay is a model name,
not proof of a SymPy backend.

## External library scope and remaining limits

`examples/modelica_dependencies.toml` pins MSL 4.1.0, archive SHA-256
`5409cfa17797c52d866dc9d2675badc1378b4bbf88066e60899a489cb1cb8a2d`, and CMM
revision `a642c381a551afd247e784648f0aedb16d0c027c`, archive SHA-256
`ca47391811532c89300e52bb8ce53ee4bfd6d6d8412d54e2f13f11387f44dcc3`.
Existing Linux workspace CI stages them with `cargo xtask repo modelica-deps ensure`
for actual example smoke/heavy Solve tests. These candidates select examples
that require neither library; they do not claim export parity for MSL-backed
PID/Kalman/switched RLC, CMM-backed vehicles, or larger neural width variants.
Full external-library editor initialization and GUI interaction also remain
outside this request-level regression. Existing corpus/example/editor lanes
retain their own coverage; source preview rendering is not a substitute.

Specs checked: SPEC_0007 and SPEC_0040 product/stage contracts, SPEC_0029 ownership,
SPEC_0032 array/domain ownership, SPEC_0022 MLS compliance (MLS 7.2.5 attribute
modifications, 8.6 initialization, 10 arrays, 12 functions), SPEC_0018 configuration,
SPEC_0021 complexity, SPEC_0033 evidence/strictness, SPEC_0025 review. GALEC preview
expectations respect the existing source-preview contract; DRAFT SPEC_0034 is
consulted as design context, not promoted to an active spec.

## Commands and status

Completed: direct rustfmt, `git diff --check`, and the focused LSP gate
(4 passed, 0 failed/ignored, including all five configured scenarios).
The first LSP run found a harness expectation bug (`Real` versus canonical
`real` report spelling); the corrected run passed. The first Modelica attempt
found a CSV collection type-inference error before OMC. After its correction,
the actual original Circuit OMC check/simulation succeeded, then the required
`src.v` column check failed because OMC omitted its constant alias. At frozen
`26f474b15`, the physical-channel correction passed and the Node workspace
gate passed all three tests. Five originals and fourteen export controls passed
before OMC rejected the DAE NeuralPredatorPrey derivative projection syntax;
NeuralODEBackprop was not reached. The derivative repair at `4659ef1d5` then
passed all fifteen exports for those five models, before OMC rejected the
authored Backprop array `fixed = true` modifiers. The actual example now uses
`each fixed = true`, retaining the intended fixed starts. The extra record-member
fixture declares exact original/exported CSV column names and checks its flat
Real declaration/shape. At frozen `c758abd70`, both Rust tests passed with exactly
28 completed OMC controls: all six originals and eighteen exports, plus the
separate fixture's four variants. Node passed all three workspace tests.
See the [first receipt](qualification-26f-20261011.md),
[derivative repair attempt](qualification-4659-20261011.md), and
[successful actual qualification](qualification-c758-20261011.md). No full
workspace/clippy, fixed MSL canary, or broader gate is claimed green.

Raw bounded attempts are retained below
`$HOME/scratch/rumoca/slam-integration-cycle-20261010/` with prefixes
`pr427-workshop-qualification-first-20261011`,
`pr427-workshop-qualification-report-spelling-20261011`, and
`pr427-workshop-qualification-omc-typed-csv-20261011`.

Focused qualification commands (run under the resource guard, with outputs and
TMPDIR below `$HOME/scratch/rumoca/`):

```bash
CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 RAYON_NUM_THREADS=4 cargo test -p rumoca-tool-lsp workshop_codegen_tests:: -- --nocapture
CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 RAYON_NUM_THREADS=4 cargo xtask verify template-runtimes --backend modelica --require-external-tools
CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 RAYON_NUM_THREADS=4 cargo test -p xtask gate_runs_every_declared_test_of_every_pinned_target
```

The approved scenario correction is applied: the former Ball/JAX scenario is
renamed to `rumoca-scenario.sympy_decay_jax_ode.toml`, points to existing
`SympyDecay.mo`, and writes to `gen/sympy_decay_jax`. The README command matches
those fields. Ball remains unchanged, and a separate direct-request test proves
its event refusal. All configured scenarios must render successfully. The
previously unapplied proposal file is removed; its contents remain in the prior
review-evidence commit rather than being described as a pending patch.

## Declared built-in targets

Seventeen `target.toml` directories are present. This is a declaration count;
it does not assert that every model/target pair is supported or executed. The
new actual-example roundtrip covers three Modelica targets. Companion backend
regression work owns C/JAX/CasADi numerical execution.

<!-- The exact baseline declaration tables follow. -->

| Target directory | Declared IR | Execution mode |
|---|---|---|
| `base-modelica` | flat | source-transform |
| `c-ode` | solve | compiled |
| `casadi-ode` | solve | symbolic |
| `cuda-ode` | solve | compiled |
| `dae-modelica` | dae | source-transform |
| `embedded-c-galec` | algorithm-code | compiled |
| `flat-modelica` | flat | source-transform |
| `fmi-ls-wasm` | fmi | compiled |
| `fmi2` | fmi | packaged |
| `fmi3` | fmi | packaged |
| `galec` | algorithm-code | packaged |
| `galec-production` | algorithm-code | packaged |
| `jax-ode` | solve | symbolic |
| `mlir` | solve | compiled |
| `rust-fixed-ode` | solve | compiled |
| `rust-ode` | solve | compiled |
| `wgsl-ode` | solve | jit |

## Source model declarations

Thirty `model` declaration lines occur under tracked `examples/**/*.mo`. This
includes package members, nested Motor, helper models, and width variants; it
is not thirty independent runnable workshop demonstrations.

| Source | Line | Declaration |
|---|---|---|
| `examples/interactive/fixedwing/FixedWingSIL.mo` | 18 | `FixedWingPlant` |
| `examples/interactive/fixedwing/FixedWingSIL.mo` | 236 | `FixedWingFBW` |
| `examples/interactive/fixedwing/FixedWingSIL.mo` | 355 | `FixedWing` |
| `examples/interactive/quadrotor/QuadrotorSIL.mo` | 13 | `QuadrotorSIL` |
| `examples/interactive/quadrotor/QuadrotorSIL.mo` | 80 | `Motor` |
| `examples/interactive/quadrotor/QuadrotorSIL.mo` | 194 | `AcroRatePID` |
| `examples/interactive/quadrotor/QuadrotorSIL.mo` | 254 | `QuadrotorAcro` |
| `examples/interactive/reusable_booster/ReusableBoosterLanding.mo` | 942 | `ReusableBoosterLanding` |
| `examples/interactive/reusable_booster/ReusableBoosterLanding.mo` | 1757 | `ReusableBoosterEmbeddedControlLaw` |
| `examples/interactive/rover/Rover.mo` | 26 | `Rover` |
| `examples/models/Ball.mo` | 1 | `Ball` |
| `examples/models/Circuit.mo` | 8 | `TwoPin` |
| `examples/models/Circuit.mo` | 18 | `Ground` |
| `examples/models/Circuit.mo` | 24 | `VoltageSource` |
| `examples/models/Circuit.mo` | 31 | `Resistor` |
| `examples/models/Circuit.mo` | 38 | `Capacitor` |
| `examples/models/Circuit.mo` | 45 | `Test` |
| `examples/models/FmiTensorDecay.mo` | 1 | `FmiTensorDecay` |
| `examples/models/GalecCounter.mo` | 1 | `GalecCounter` |
| `examples/models/KalmanFilterStepTest.mo` | 62 | `KalmanFilterStepTest` |
| `examples/models/NeuralLatentOscillator.mo` | 1 | `NeuralLatentOscillator` |
| `examples/models/NeuralODEBackprop.mo` | 1 | `NeuralODEBackprop` |
| `examples/models/NeuralODEBackprop.mo` | 154 | `NeuralODEBackprop1k` |
| `examples/models/NeuralODETensor.mo` | 1 | `NeuralODETensor` |
| `examples/models/NeuralODETensor.mo` | 60 | `NeuralODETensor100k` |
| `examples/models/NeuralPredatorPrey.mo` | 1 | `NeuralPredatorPrey` |
| `examples/models/PIDMSL.mo` | 1 | `PIDMSL` |
| `examples/models/SwitchedRLC_MSL.mo` | 1 | `SwitchedRLC_MSL` |
| `examples/models/SwitchedRLC.mo` | 1 | `SwitchedRLC` |
| `examples/models/SympyDecay.mo` | 1 | `SympyDecay` |
