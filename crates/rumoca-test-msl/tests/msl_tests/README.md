# MSL Test Pipeline Notes

This directory contains helper includes for `tests/msl_tests.rs`.
`tests/msl_tests.rs` now exposes exactly one test:
`balance_pipeline::balance_pipeline_core::test_msl_all`.

## Split of Responsibilities

- `balance_pipeline.rs`
  - Owns high-level orchestration and core data structures for parse/session/compile.
- `balance_pipeline_selection.rs`
  - Owns focused simulation subset selection via `RUMOCA_MSL_SIM_*` controls.
  - Owns state-count-ranked fast/long/full simulation-set selection via
    `RUMOCA_MSL_SIM_SET`.
  - Keeps the compile-target reduction behavior for focused runs centralized.
- Focused model compiles reuse the shared parsed/resolved MSL source-root tree, but
  each requested model is compiled on its own uncached reachable closure so
  per-model phase results do not accumulate across the full MSL simulation gate.
- The harness stages the official `ModelicaStandardLibrary_v4.1.0.zip` release
  asset and treats `Complex.mo` plus `Modelica 4.1.0/package.mo` as required
  cache-layout sentinels, so stale partial MSL caches are rejected and rebuilt.
- `balance_pipeline_sim_worker.rs`
  - Owns isolated simulation worker execution and timeout/result mapping.
- `balance_pipeline_render_sim.rs`
  - Owns render + simulation orchestration over compiled model results.
- `balance_pipeline_summary.rs`
  - Owns post-compile summary aggregation and timing bookkeeping.
- `balance_pipeline_reporting.rs`
  - Owns result JSON writing and top-level balance summary printing.
- `balance_pipeline_quality_gate.rs`
  - Owns machine-readable MSL quality baseline gate logic.
- `balance_pipeline_stats_report.rs`
  - Owns simulation/timing/failure stats printing and final stats emission.
- `balance_pipeline_debug_introspection.rs`
  - Owns per-model DAE/flat introspection dumps used during focused simulation triage.

## Pipeline Invariants

- Compile/balance/simulation baseline metrics are measured on discovered root
  models matching `Modelica.*.Examples.*` in MSL 4.1.0. Support/helper packages
  under `Examples` (`Utilities`, `BaseClasses`, `Internal`, `Interfaces`) are
  excluded from this root-model set.
- `ModelicaTest` is a separate semantic regression gate, not part of the
  default MSL example target set. It uses:
  - `tests/msl_tests/modelica_test_targets_ci.json`
  - `RUMOCA_MSL_INCLUDE_MODELICATEST=1`
  - `RUMOCA_MSL_REQUIRE_SELECTED_TARGETS_SUCCESS=1`
  - `RUMOCA_MSL_SIM_TARGETS_FILE=tests/msl_tests/modelica_test_targets_ci.json`
  The official MSL release zip used by the default gate does not always contain
  the test package, so CI overlays the `ModelicaTest` directory from the MSL
  source tag before running this separate gate.
- Focused subset controls (`RUMOCA_MSL_SIM_MATCH`, `RUMOCA_MSL_SIM_LIMIT`) are
  for iterative simulation work and must not be treated as baseline runs.
- By default, the pipeline uses the root-example baseline scope when no target
  environment override is provided. The committed explicit target file can still
  be selected with `RUMOCA_MSL_TARGET_SCOPE=committed-targets` for local triage,
  but that is not the CI baseline.
- Every run writes package pass-rate artifacts usable for table generation:
  - `target/msl/results/msl_package_pass_rates.json`
  - `target/msl/results/msl_package_pass_rates.md`
  - `target/msl/results/msl_package_pass_rates.txt`
  - `target/msl/results/msl_package_pass_rates_compact.txt`
  These reports include parse, flatten, DAE, Solve-IR, initial-condition
  solve, and simulation pass rates by package.
- `cargo xtask verify msl-parity --results-dir <path>` redirects the harness
  JSON, markdown, trace, and debug artifacts for that invocation. CI uses this
  to keep the focused ModelicaTest semantic gate from overwriting the full MSL
  quality gate artifacts used in the PR summary comment.
- Set `RUMOCA_MSL_PRINT_PACKAGE_PASS_TABLE=1` to print the compact percentage
  table at the end of the test output.
- Runs that complete the OMC parity stage also write package-level trace
  accuracy artifacts on the same `n` denominator and 0-100 good scale:
  - `target/msl/results/msl_package_trace_accuracy.json`
  - `target/msl/results/msl_package_trace_accuracy.md`
- Set `RUMOCA_MSL_SKIP_OMC_COMPILE_REFERENCE=1` to skip the optional
  OMC compile/flatten reference artifact while still running OMC simulation
  trace parity and the baseline quality gate. CI uses this mode to avoid cold
  runner timeouts in `checkModel` reference generation.
- Paper/table data runs that only need generated artifacts can set
  `RUMOCA_MSL_WRITE_RESULTS_ONLY=1` to stop after JSON/markdown emission and
  skip the OMC parity/quality-gate stages.
- Baseline JSON (`msl_quality_baseline.json`) stores cumulative stage pass
  counts for the fixed root-example denominator: parse/IR-AST, flatten/IR-flat,
  DAE/IR-DAE, solve/IR-Solve, initial-condition solve, and simulation. The CI
  gate treats any increase in an early-stage cumulative count as an improvement
  and fails only when a cumulative stage count drops below the committed
  baseline for the same fixed target set. Promote reviewed full-run data, and
  keep the committed stage counts conservative enough to absorb compile-timeout
  jitter; focused subsets and one-off explicit target files are not baselines.
- Baseline JSON also captures OMC parity distributions for this set (runtime
  speedup ratio + trace-accuracy min/median/mean/max), populated from
  `omc_simulation_reference.json`.
- Trace parity excludes known stochastic random-input examples listed in:
  - `tests/msl_tests/msl_trace_compare_exclusions.json`
  - these models remain in compile/balance/sim stats, but are skipped from
    OMC-vs-Rumoca trace deviation metrics unless deterministic parity support is added.
- Successful baseline `test_msl_all` runs write current quality snapshot:
  - `target/msl/results/msl_quality_current.json`
- Committed baseline updates are explicit/manual:
  - `cargo xtask repo msl promote-quality-baseline`
- Alternate simulation sets:
  - unset or `RUMOCA_MSL_SIM_SET=full` to keep all models from the selected list
  - `RUMOCA_MSL_SIM_SET=short` for the first N models in the selected list
  - `RUMOCA_MSL_SIM_SET=long` for the last N models in the selected list
  - `RUMOCA_MSL_SIM_SET_LIMIT=<N>` controls short/long set size (default `180`)
- Focused subset controls also support `RUMOCA_MSL_SIM_TARGETS_FILE`:
  - accepts JSON array (`["Modelica...."]`)
  - accepts object with `model_names`
  - accepts object with `records[*].model_name` (parity manifest friendly)
  - explicit target files are allowed to select discovered `ModelicaTest.*`
    models as well as `Modelica.*.Examples.*` models, so the separate
    `modelica_test_targets_ci.json` gate can exercise non-MSL-example semantic
    tests without mixing them into the default MSL example target set
  - missing names in an explicit target file are a hard error, preventing a
    stale or typoed ModelicaTest gate from silently passing
- `RUMOCA_MSL_REQUIRE_SELECTED_TARGETS_SUCCESS=1` turns an explicit target-file
  run into a hard selected-target gate: every selected model must appear in
  results, compile successfully, and simulate with `sim_ok`.
- Local target-set experimentation can opt into generated/cache-driven inputs:
  - `RUMOCA_MSL_USE_GENERATED_SIM_TARGETS=1` allows
    `target/msl/results/msl_simulation_targets.json` to replace the committed
    baseline list when present.
  - `RUMOCA_MSL_USE_PRIOR_COMPLEXITY_SCHEDULE=1` sorts the default compile scope
    by prior `target/msl/results/msl_results.json` complexity metrics instead of
    keeping lexical baseline order.
- Simulation attempts are limited to standalone root MSL examples:
  - explicit `Modelica.*.Examples.*` roots
  - non-partial models
  - no unbound top-level inputs
  - no unbound fixed parameters
- Worker timeout semantics are two-tiered:
  - DAE-to-Solve-IR lowering budget: `IR_SOLVE_TIMEOUT_SECS` (currently 10s)
  - solver budget: `SIM_TIMEOUT_SECS` (currently 10s)
  - parent process budget: lowering budget + solver budget +
    `SIM_WORKER_TIMEOUT_GRACE_SECS`
    (currently +2s)
- Any worker process timeout/failure is reported as explicit simulation status
  (`sim_timeout`/`sim_solver_fail`), never as silent success.
- DAE JSON artifacts are retained under `target/msl/results/ir_dae/`; Solve-IR
  JSON artifacts are retained under `target/msl/results/ir_solve/` when lowering
  reaches that stage.
- Slow-model perf profiles are opt-in because `perf record` is expensive. Set
  `RUMOCA_MSL_COMPILE_PERF_RECORD=1` to profile each model model worker thread
  and retain only failed or slow profiles under
  `target/msl/results/perf/compile/`. Set `RUMOCA_MSL_SIM_PERF_RECORD=1` to
  profile each sim worker process and retain only failed or slow profiles under
  `target/msl/results/perf/sim/`. The slow thresholds default to 5s and can be
  adjusted with `RUMOCA_MSL_COMPILE_PERF_KEEP_THRESHOLD_SECS` and
  `RUMOCA_MSL_SIM_PERF_KEEP_THRESHOLD_SECS`; sampling frequency defaults to 99Hz
  and can be adjusted with `RUMOCA_MSL_COMPILE_PERF_FREQ` and
  `RUMOCA_MSL_SIM_PERF_FREQ`.
- Canonical full-run entry point:
  - `cargo xtask verify msl-parity`
  - raw test equivalent:
    `cargo test --release --package rumoca-test-msl --features msl-full-test --test msl_tests balance_pipeline::balance_pipeline_core::test_msl_all -- --nocapture`
