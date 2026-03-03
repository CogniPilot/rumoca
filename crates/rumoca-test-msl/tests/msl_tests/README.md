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

- Compile/balance/simulation baseline metrics are measured on the committed
  180-model explicit-example set:
  `tests/msl_tests/msl_simulation_targets_180.json`.
- Focused subset controls (`RUMOCA_MSL_SIM_MATCH`, `RUMOCA_MSL_SIM_LIMIT`) are
  for iterative simulation work and must not be treated as baseline runs.
- By default, the pipeline loads `RUMOCA_MSL_SIM_TARGETS_FILE` from this
  committed targets file when no environment override is provided.
- Baseline JSON (`msl_quality_baseline.json`) also captures OMC parity
  distributions for this set (runtime speedup ratio + trace-accuracy
  min/median/mean/max), populated from `omc_simulation_reference.json`.
- Trace parity excludes known stochastic random-input examples listed in:
  - `tests/msl_tests/msl_trace_compare_exclusions.json`
  - these models remain in compile/balance/sim stats, but are skipped from
    OMC-vs-Rumoca trace deviation metrics unless deterministic parity support is added.
- Successful baseline `test_msl_all` runs write current quality snapshot:
  - `target/msl/results/msl_quality_current.json`
- Committed baseline updates are explicit/manual:
  - `cargo run --release --package rumoca-tool-dev --bin rumoca-msl-tools -- promote-quality-baseline`
- Alternate simulation sets:
  - `RUMOCA_MSL_SIM_SET=long` for high-state models within the selected list
  - `RUMOCA_MSL_SIM_SET=full` to keep all models from the selected list
  - `RUMOCA_MSL_SIM_SET_LIMIT=<N>` controls short/long set size (default `180`)
- Focused subset controls also support `RUMOCA_MSL_SIM_TARGETS_FILE`:
  - accepts JSON array (`["Modelica...."]`)
  - accepts object with `model_names`
  - accepts object with `records[*].model_name` (parity manifest friendly)
- Simulation attempts are limited to standalone root MSL examples:
  - explicit `Modelica.*.Examples.*` roots
  - non-partial models
  - no unbound top-level inputs
  - no unbound fixed parameters
- Worker timeout semantics are two-tiered:
  - solver budget: `SIM_TIMEOUT_SECS` (currently 10s)
  - parent process budget: solver budget + `SIM_WORKER_TIMEOUT_GRACE_SECS`
    (currently +1s)
- Any worker process timeout/failure is reported as explicit simulation status
  (`sim_timeout`/`sim_solver_fail`), never as silent success.
- Canonical full-run entry point:
  - `test_msl_all`: compile + balance + simulation over the default 180-model
    set (use `RUMOCA_MSL_SIM_TARGETS_FILE=<json>` for alternate/full lists).
