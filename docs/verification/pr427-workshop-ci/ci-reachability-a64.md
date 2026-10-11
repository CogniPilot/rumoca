# Workshop CI selection and strict fixtures

Source audit based on `a64ea2d60574433584c8dd1d816b501bb3a24c7a`.
The capture/fixture policy followup is source-only: Cargo and new OMC gates
have not run. Historical receipts remain bound to their original revisions.

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

Kalman retains the existing example manifest reader. The two original MSL
LAPACK solve/least-squares helpers now check both required MSL files rather
than only the directory. `cargo xtask verify workspace` already selects
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
| Template Runtime (modelica), same strict command with `--backend modelica` | Exact `modelica_interchange_runtime::` filter reaches two tests: six actual originals plus three exports each, and a derivative aggregate fixture plus its three exports; 28 OMC cells total |
| Three-OS workspace test job | Four ordinary LSP workshop tests render all five configured codegen scenarios through actual execute-command handling, check relative custom paths, retain Ball/JAX event refusal, and check actual CasADi and complete FMI source projects |
| Playground, `cargo xtask playground test` | Discovers all `*.test.mjs`, including three `default_workspace.test.mjs` checks; actual FullWeb runtime smoke is a separate stage of the same command |

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
