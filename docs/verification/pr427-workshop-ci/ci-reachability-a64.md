# Workshop CI selection and strict fixtures

Source audit based on `db0e68b9cd0470f5466e1e2874350505115362c6`;
the historical filename is retained. Remote PR 427 was
`32f75cfa310ac2f1a4b12c874093038203de1b0c`, against base
`0b249f0f725431613c25bb56e40fbbe0df54c4d2`, when inspected.
This is a read-only reachability audit, not an executed test receipt.
Historical and subsequent execution receipts remain bound to their own
revisions and raw logs.

The existing mandatory `test` job in `.github/workflows/ci.yml` runs
`cargo xtask verify examples` on Linux, macOS and Windows. The xtask stages
libraries from `examples/modelica_dependencies.toml`, then runs the complete
Cargo target without a test filter:

```sh
cargo test --package rumoca --features examples-smoke-tests \
  --test suite_examples_smoke -- --nocapture
```

This selects **seven tests**, not seven independent numeric regressions:

- One compilation roster checks all 13 tracked model files and 15 authored
  demo entrypoints. Missing files, new unlisted files, any failed compilation
  or an unbalanced result fail the test. Circuit components are exercised
  through `Circuit.Test`; no partial model is counted as a standalone demo.
- Five tests check six actual models: SympyDecay and FmiTensorDecay against
  exponentials; Ball against free fall before impact; Circuit.Test against RC
  charging; SwitchedRLC against its piecewise voltage-step response; unchanged
  KalmanFilterStepTest against its authored initialization assertions and
  `x(t)=t` at eleven exact sample times under Interpreter and Auto. Kalman
  prints actual execution receipts; Auto is not claimed to force native code.
- The existing synthetic Decay smoke checks a finite nonempty trace.

The roster also compiles GalecCounter, NeuralLatentOscillator, NeuralODEBackprop
and its 1k variant, NeuralODETensor and its 100k variant, NeuralPredatorPrey,
PIDMSL and SwitchedRLC_MSL. Compilation coverage is not numeric proof for
those models. Defaults stay authored: Backprop uses nHidden=12/nBatch=8
(218 trainable states); NeuralODETensor uses nHidden=32, with the separately
declared nHidden=314 variant. No all-15 numeric or 32-model matrix claim is made.

Kalman retains the existing example manifest reader. The original MSL
linear-solve helper now checks both required MSL files rather than only the
directory, and least-squares reuses that private same-suite owner.
`cargo xtask verify workspace` already selects
`rumoca/msl-sim-tests`; under that feature missing or incomplete MSL is a hard
failure. Ordinary local calls without that feature retain the existing optional
MSL policy. No cross-suite path inclusion or new public helper is introduced.
Focused qualification must stage the manifest cache and select the strict feature:

```sh
cargo xtask repo modelica-deps ensure
cargo test -p rumoca --features msl-sim-tests --test suite_core \
  native_linear_solve:: -- --nocapture
cargo test -p rumoca --features msl-sim-tests --test suite_core \
  native_least_squares:: -- --nocapture
```

Other workshop CI owners are already mandatory; no duplicate job is added:

| Existing job / command | Actual source-selected checks |
|---|---|
| Template Runtime (c/jax/casadi), `cargo xtask verify template-runtimes --backend <backend> --require-external-tools` | C filters `c_ode_` and `ode_targets_`, plus JAX/CasADi filters `jax_`/`casadi_`, reach actual numerical-example initialization/RHS/AD tests; backend failures are not optional local success |
| Template Runtime (modelica), same strict command with `--backend modelica` | Exact `modelica_interchange_runtime::` filter reaches **four tests total**: two smoke tests and two workshop tests. The workshop pair simulates six actual originals plus three exports each, and a derivative aggregate fixture plus its three exports: **28 retained OMC cells**. The separate smoke OMC test checks three exports; it does not contribute to those 28 simulation cells |
| Three-OS workspace test job | Four ordinary LSP workshop tests render all five configured codegen scenarios through actual execute-command handling, check relative custom paths, retain Ball/JAX event refusal, and check actual CasADi and complete FMI source projects |
| Playground, `cargo xtask playground test` | Discovers all `*.test.mjs`, including three `default_workspace.test.mjs` checks; actual FullWeb runtime smoke is a separate stage of the same command |

At this source, the exact mandatory runtime filters and source-derived
selected test counts are:

| Backend / exact filters | Tests selected | Count derivation |
|---|---:|---|
| C: `c_ode_`, `ode_targets_` | 11 | Seven tests in `c_ode_pure_call_semantics`, one selected-region test, one scalar smoke, one five-model workshop test, and one compacted-family-call test |
| JAX: `jax_` | 6 | Two selected-sqrt tests, one vector/capture selected-region test, scalar smoke, original issue 418, and the five-model workshop test |
| CasADi: `casadi_` | 3 | Selected-region test, scalar smoke, and five-model workshop test |
| Modelica: `modelica_interchange_runtime::` | 4 | Two smoke tests and the two workshop tests described above |
| Render: `codegen_example_regression::` | 6 | Direct custom-template inventory and five checked-report/source-root/unit controls |

These counts enumerate `#[test]` functions and their declared module paths
under `tests/suite_template_runtime`; they are not libtest execution counts.
The owning table is `TEMPLATE_RUNTIME_GROUPS` in `xtask/src/verify_cmd.rs`.
C/JAX/CasADi/Modelica select `--no-default-features --features
template-runtime-tests`; Render additionally selects `fmu-packaging`.
The three five-model numerical tests use actual SympyDecay, NeuralODETensor,
NeuralPredatorPrey, NeuralLatentOscillator and NeuralODEBackprop sources.
They compare initialized defaults and two RHS points against the checked
runtime; JAX checks JIT, a forward directional derivative and reverse gradient,
and CasADi checks the corresponding Jacobian column. This is not a full
simulation trace or an independent analytic oracle for each neural model.
Missing C/Python/backend tools fail those tests. The CI Modelica lane's strict
marker makes its otherwise optional local OMC/timeout prerequisite failure
mandatory.

The four LSP workshop tests are unconditionally included by the default
`server` feature. Workspace CI uses `nextest run --workspace` with
`rumoca/msl-sim-tests`, excluding Python/WASM bindings for their dedicated
jobs. The actual FullWeb smoke is selected by `playground_cmd.rs` after its
single-threaded release build; it is not an undiscovered `*.mjs` test.

Remaining source-level coverage limits:

- Micah's Python FMI feature repair (`c7d3ab509`) has no direct binding
  regression for `Model.codegen("fmi2")`, `"fmi3"` or `"fmi-ls-wasm"`.
  Installed-wheel smoke/API helpers render C, magic renders DAE Modelica,
  and the binding's Rust dispatch tests render two GALEC targets. CLI/LSP
  FMI tests do not exercise that Python entrypoint.
- Installed-wheel `export_test.py` may skip JAX/CasADi live-object exports
  when those optional dependencies are absent. Strict generated-target
  runtime jobs cover a different entrypoint.
- FullWeb's actual `target_render_smoke.mjs` renders base/DAE Modelica,
  C/JAX/CasADi for SympyDecay and GALEC for GalecCounter, plus custom
  workspace/refusal controls. It does not render `flat-modelica` through
  the WASM entrypoint. FMI packaging is deliberately refused in the browser.
- The PR's manual 32-model/246-cell report is not a mandatory automated
  matrix. Larger neural variants and MSL-backed PID/switched-RLC examples
  have the compilation roster above, not all-target numeric export proof.

OMC cells now always retain their actual source, script, stdout, stderr, status
and any real CSV in a fresh process-owned TempDir under standard `TMPDIR`.
Each path is printed. Missing CSV remains a failure, never a fabricated file.
A successful run yields 28 cells/168 files; the owner must hash those actual
files after execution. No bespoke environment knob or shared fixed-path race
is introduced (SPEC_0018). The existing Modelica CI lane collects its runtime
log and hashes/uploads the retained directory in `always()` steps. Oracles,
tolerances and model sources are unchanged.

Fresh exact-head focused execution, instrumented coverage, full CI and
`cargo xtask verify gate --rev <tip> --coverage` remain required by
SPEC_0033/SPEC_0025. Source reachability alone is not a passed gate. The known
original #425 38x38/2-GiB failure remains unresolved; a potential #421 regression
transfer must preserve its exact source, limit and evidence, and cannot assume
the stateless checked-assignment path applies to its 5776 continuous states.
