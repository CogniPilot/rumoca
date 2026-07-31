# MSL Test Pipeline Notes

This directory contains helper includes for `tests/msl_tests.rs`.
`tests/msl_tests.rs` now exposes exactly one test:
`balance_pipeline::balance_pipeline_core::test_msl_all`.

> **The `RUMOCA_MSL_*` environment variables named throughout the rest of this
> file no longer exist.** SPEC_0018 mandates zero `RUMOCA_*` env vars, and the
> harness is now driven exclusively by the JSON config that
> `cargo xtask verify msl-parity` writes (`MslParityConfig` in
> `tests/balance_pipeline/balance_pipeline_config.rs`). Read every
> `RUMOCA_MSL_FOO=bar` below as the corresponding `--foo bar` xtask flag. The
> sections added under "Gate knobs" are current.

## Gate knobs (current)

| Knob | xtask flag | Config field |
|---|---|---|
| Include ModelicaTest sources | `--include-modelica-test` | `include_modelica_test` |
| Require every selected target to simulate | `--require-selected-targets-success` | `require_selected_targets_success` |
| Explicit target list | `--sim-targets-file PATH` | `sim_targets_file` |
| Subset filter / cap | `--sim-match PAT`, `--sim-limit N` | `sim_match`, `sim_limit` |
| Solver wall budget | `--sim-timeout-secs SECS` | `sim_timeout_secs` |
| Solve-IR lowering budget | `--ir-solve-timeout-secs SECS` | `ir_solve_timeout_secs` |
| Per-phase attempt budget (10s default) | `--model-attempt-timeout-secs SECS` | `model_attempt_timeout_secs` |
| Total compile wall ceiling (40s default) | — (config only) | `model_compile_wall_limit_secs` |
| Solve-IR serialized-size ceiling (32 MB default) | — (config only) | `solve_ir_size_limit_mb` |
| OMC baseline for every target | `--all-omc-targets` | `all_omc_targets` |

Every budget knob is **raise-only**: the harness clamps each to at least its
built-in default, because the committed quality baseline was measured with
those defaults and shortening a budget would silently turn real failures into
timeouts. The OMC reference budget scales with `--sim-timeout-secs` so the two
tools keep a comparable amount of time (`omc_sim_reference_timeout_secs`).

Each model has one attempt. If any phase exceeds the attempt budget, the
harness kills that model's worker and records `EMSL_TIMEOUT_MODEL_ATTEMPT`.
There is no diagnostic retry or alternate-budget result.

## Per-model resource ceilings

Five ceilings bound one model attempt; a model that fits all five is accepted
and nothing else about its shape is judged. Overruns are typed, never
message-matched:

| Ceiling | Default | Overrun bucket | Error code |
|---|---|---|---|
| per-phase compile/sim wall | 10 s | `Timeout` | `EMSL_TIMEOUT_MODEL_ATTEMPT` |
| solver wall | 10 s | `Timeout` | `EMSL_TIMEOUT_MODEL_ATTEMPT` |
| worker resident+swap | 6 GiB | `MemoryLimit` | `EMSL_MODEL_WORKER_MEMORY_LIMIT` |
| total compile wall | 40 s | `ResourceBudget` | `EMSL_BUDGET_COMPILE_WALL` |
| Solve-IR serialized size | 32 MB | `ResourceBudget` | `EMSL_BUDGET_SOLVE_IR_SIZE` |

`ResourceBudget` is owned by `Performance`, like `Timeout`/`MemoryLimit`, but is
kept distinct from them: it means "the artifact lowering produced is bigger (or
slower) than the pipeline agreed to carry", which is a lowering defect, not a
scheduling accident. `rumoca_test_msl::resource_budget` holds the full
acceptance contract, including why the size measurement stops at the ceiling
instead of discovering how far past it a model went — a single
`LieGroups.SE23.Quat.log_map` call from the cached CMM snapshot scalarizes to a
~34 MB Solve IR, and four of them to ~142 MB.

`--all-omc-targets` exists for the long-budget diagnostic lanes: the canonical
gate restricts the OMC baseline to models rumoca already simulates, which is
exactly wrong for a cohort whose members are not yet `sim_ok`.

## ModelicaTest growth ratchet

`modelica_test_targets_ci.json` is the blocking semantic gate's target list and
may only ever grow. `balance_pipeline_selection.rs` pins this with
`modelica_test_ci_target_list_only_grows`, which asserts a floor
(`MODELICA_TEST_CI_TARGET_FLOOR`), sortedness, and no duplicates.

To grow it, take the promoted list produced by the nightly-on-main survey step
of the `modelicatest-gate` CI job
(`target/msl/modelicatest-survey/modelica_test_targets_ci.promoted.json`, an
uploaded artifact), or regenerate it locally:

```
cargo xtask verify msl-parity \
  --results-dir target/msl/modelicatest-survey \
  --include-modelica-test --sim-match ModelicaTest. --sim-set full
rumoca-msl-tools modelica-test-catalog \
  --results target/msl/modelicatest-survey/msl_results.json \
  --json-out target/msl/modelicatest-survey/modelica_test_catalog.json \
  --base-targets crates/rumoca-test-msl/tests/msl_tests/modelica_test_targets_ci.json \
  --promote-targets-out crates/rumoca-test-msl/tests/msl_tests/modelica_test_targets_ci.json \
  --per-category 6
```

`modelica-test-catalog` only promotes models that reached `phase_reached ==
"Success"` **and** `sim_status == "sim_ok"`, because the gate runs the list with
`--require-selected-targets-success` and would otherwise go red on every
unrelated PR. Raise `MODELICA_TEST_CI_TARGET_FLOOR` in the same commit that
grows the list; never lower it to make a red gate green.

## Nightly diagnostic lanes

`.github/workflows/nightly.yml` runs three non-blocking lanes:

- `event-cohort-parity` — `event_cohort_targets_nightly.json` with 300s solver /
  60s Solve-IR / 420s attempt budgets and `--all-omc-targets`. Explicit target
  files skip the baseline-relative quality gate, so this lane is purely
  diagnostic; it deliberately does **not** pass
  `--require-selected-targets-success` until the cohort actually passes.
- `cross-backend-msl` — the `msl-external-tests` FMI3 and CasADi suites, which
  were declared in `Cargo.toml` but referenced by no workflow.
- `parser-fuzz` — `cargo xtask verify fuzz --max-total-secs 900` over the
  standalone `fuzz/` cargo-fuzz crate.

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
  and fails only when a cumulative stage count drops below the resolved
  promoted baseline for the same fixed target set. `cargo xtask verify
  msl-parity` downloads the latest promoted baseline from the
  `msl-quality-baseline` GitHub release asset and falls back to the checked-in
  JSON when offline. An explicitly reviewed checked-in full baseline may use
  `omc_context_migration` to declare the exact old and new OMC versions and
  fixed target count; only a declaration matching both baseline contexts is
  used until a successful main run promotes that context. Once both contexts
  match, the promoted baseline is authoritative and the declaration is inactive
  provenance. Focused subsets and one-off explicit target files are not
  baselines.
- Baseline JSON also captures OMC parity distributions for this set (runtime
  speedup ratio + trace-accuracy min/median/mean/max), populated from
  `omc_simulation_reference.json`.
- Trace parity excludes known stochastic random-input examples listed in:
  - `tests/msl_tests/msl_trace_compare_exclusions.json`
  - these models remain in compile/balance/sim stats, but are skipped from
    OMC-vs-Rumoca trace deviation metrics unless deterministic parity support is added.
- Successful baseline `test_msl_all` runs write current quality snapshot:
  - `target/msl/results/msl_quality_current.json`
- Checked-in fallback baseline updates are explicit/manual:
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
  - no unbound top-level input scalars (zero-sized input arrays are allowed)
  - no unbound fixed-parameter scalars (zero-sized parameters are allowed)
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

## Compile-failure attribution and the balance cohort

Failure attribution is *structural*, never derived from message text:

- The worker calls
  `Session::compile_model_dae_strict_reachable_uncached_with_recovery_detailed`,
  which returns a `StrictCompileFailure { phase, error_code, balance_detail, .. }`.
  `phase: None` means the compile never reached a model phase (parse/resolve
  failed first) and is recorded as `Resolve`.
- `error_code` in `msl_results.json` is the bare SPEC_0008 code (`ED001`,
  `ER003`, ...), normalized from the namespaced miette form
  (`rumoca::todae::ED001`) via `rumoca_core::short_phase_error_code`. It is
  therefore safe to key maps and taxonomies by this value.
- `balance_detail` is present for `ED001` (unbalanced concrete model) failures
  and for successful partial-class inspection when its continuous balance is
  nonzero. It carries the full component breakdown, balance clamps, and
  per-reason equation-row exclusions. An unbalanced concrete model never
  proceeds to DAE construction or simulation; partial-class output is
  diagnostic metadata and is never a simulation target.

Historical note: before this plumbing existed, the worker hard-coded
`error_code: None` and re-derived the phase by searching the rendered summary
for a `failed in <Phase>:` marker, defaulting to `ToDae`. Parse/resolve
failures render without a marker, so they were all filed under `ToDae` — which
is why `error_code_counts` was empty and the "ToDae gap" appeared to be a
balance cohort. Do not reintroduce text-derived phase attribution.

### Quality-gate schema v2: `flatten_models` 565 → 555

The stage floors are *cumulative pass* counts derived from `phase_reached`:
`flatten_models` counts models that got past flattening, `dae_models` those that
got past ToDae. `phase_reached` names the phase that failed, so both are computed
from `completed_compile_phase` against `COMPILE_PHASE_ORDER` in
`balance_pipeline_quality_gate.rs` — never from a hand-written phase set.

Correcting the attribution above moved ten rows out of the `ToDae` bucket and
into `Resolve`:

```
Modelica.Fluid.Examples.AST_BatchPlant.BatchPlant_StandardWater
Modelica.Fluid.Examples.AST_BatchPlant.Test.OneTank
Modelica.Fluid.Examples.AST_BatchPlant.Test.TankWithEmptyingPipe1
Modelica.Fluid.Examples.AST_BatchPlant.Test.TankWithEmptyingPipe2
Modelica.Fluid.Examples.AST_BatchPlant.Test.TanksWithEmptyingPipe1
Modelica.Fluid.Examples.AST_BatchPlant.Test.TanksWithEmptyingPipe2
Modelica.Fluid.Examples.AST_BatchPlant.Test.TwoTanks
Modelica.Fluid.Examples.Explanatory.MeasuringTemperature
Modelica.Fluid.Examples.Explanatory.MomentumBalanceFittings
Modelica.Fluid.Examples.InverseParameterization
```

All ten fail with `ER002` ("unresolved component reference"), i.e. they never
reached instantiation, let alone flattening. They were previously counted as
*flattened* only because the marker-free summary defaulted to `ToDae`. The
baseline moves 565 → 555 to record what the pipeline actually achieves; the
compiler did not regress on these models, the measurement did. `dae_models` and
`compiled_models` are unaffected — those rows were never successes.

Quality-gate schema version 2 records this as a metric-attribution migration
instead of presenting it as an ordinary baseline regression. The migration
lists all ten `ER002` models and the before/after counts. Version 2 also makes
the top-level `tensor_preservation` baseline mandatory; a field-less version-1
baseline now fails deserialization instead of silently disabling the tensor
ratchet. Its initial KPI values come from the full `a966d9e8` run, whose source
tree is identical to the reviewed branch tree.

`completed_compile_phase_follows_the_pipeline_order` and
`gate_input_stage_counts_are_derived_from_phase_reached` pin the derivation, so
the metric cannot drift again without a failing test.

### Derived artifacts

- `msl_results.json` gains `compile_dae_balance_failures`, a measured cohort:
  `todae_failures`, `balance_failures`, `todae_error_code_counts`,
  `balance_failures_by_package`, and one record per ED001 model with its
  balance, equations/unknowns, dominant term, exercised clamps and exclusion
  counts.
- `msl_triage.md` renders a `Balance Cohort (ED001)` section grouped by package,
  plus a `compile.dae.unbalanced` taxonomy reason distinct from other
  `compile.dae.<code>` reasons.

### Single-model drill-down

```
cargo run -p rumoca-test-msl --bin rumoca-msl-tools -- \
    debug-model --model '<Model>'
```

prints the strict failure's `error_code`, balance, dominant term, raw component
counts, exercised clamps, and equation-row exclusions. It does not construct
or simulate an invalid DAE. Each clamp names the analysis to audit next:

| Clamp | Audit |
|---|---|
| `interface_flow` | `rumoca-phase-dae/src/analysis/variable_analysis.rs` `count_interface_flows` |
| `oc_interface` | `variable_analysis.rs` `count_overconstrained_interface` |
| `oc_break_edge` | `rumoca-phase-dae/src/overconstrained_interface.rs` |
| `aggregate_candidates` | `balance.rs` aggregate-candidate matching |

A non-zero `excluded.redundant_connection_alias` points at
`is_redundant_connection_alias` in `rumoca-phase-dae/src/balance.rs`.
