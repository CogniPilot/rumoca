# MSL Quality Gate

Rumoca's main MSL baseline is the MSL 4.1.0 root-example set selected by
`Modelica.*.Examples.*`. Helper packages under `Examples` such as `Utilities`,
`BaseClasses`, `Internal`, and `Interfaces` are excluded.

Run the gate with:

```bash
cargo xtask verify msl-parity
```

The raw test command is:

```bash
cargo test --release --package rumoca-test-msl --features msl-full-test \
  --test msl_tests balance_pipeline::balance_pipeline_core::test_msl_all -- --nocapture
```

The gate writes the current run to:

- `target/msl/results/msl_quality_current.json`
- `target/msl/results/msl_package_pass_rates.md`
- `target/msl/results/msl_package_trace_accuracy.md`
- `target/msl/results/mls_contract_coverage.md`
- `target/msl/results/omc_simulation_reference.json`

Local full runs also generate OMC compile/flatten reference data in
`target/msl/results/omc_reference.json` unless
`RUMOCA_MSL_SKIP_OMC_COMPILE_REFERENCE=1` is set. CI sets that flag because
cold GitHub runners repeatedly reload MSL for the compile reference; the CI
gate still checks Rumoca stage counts and OMC simulation trace parity.

CI compares the current run against the resolved MSL quality baseline.
`cargo xtask verify msl-parity` downloads the latest promoted
`msl_quality_baseline.json` from the stable `msl-quality-baseline` GitHub
release asset when available, caches it under `target/msl/baselines/`, and
falls back to `crates/rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json`
for offline runs.
The stage checks are cumulative over the fixed root-example denominator:
parse/IR-AST, flatten/IR-flat, DAE/IR-DAE, solve/IR-Solve,
initial-condition solve, and simulation. Increasing an early-stage pass count
is always treated as an improvement; the gate fails when any cumulative stage
count drops below the committed baseline for the same target set.

The root-example/full quality gate is fail-closed for OMC parity. OMC must be
available during parity preparation, and the current
`omc_simulation_reference.json` must match the active target count and contain
non-empty system and wall runtime samples plus at least one comparable trace
with model bucket percentages. Missing OMC, a missing or stale reference, or a
reference with no comparable runtime/trace metrics fails the run with an
actionable error; compile and simulation counts alone are not a passing full
quality gate.

`msl_quality_current.json` also records release review metadata:

- `omc_version` records the OpenModelica build used for OMC trace parity; the
  quality gate compares the upstream release version and tolerates distro
  package rebuild suffix drift.
- `mls_contract_coverage` groups per-model stage, Solve-IR, balance,
  simulation, and error-code counts by MLS contract category (`ARR`,
  `CONN_STRM`, `FUNC`, `EQN_ALG_SIM`, `CLK_SM`, `DECL_TYPE`, `PKG`, `OTHER`).
  The same data is written as `mls_contract_coverage.{json,md,txt}` so release
  reviews can inspect category coverage without manually querying the quality
  snapshot JSON.

On pull requests, CI also generates
`target/msl/results/msl_pr_comment.md` with `cargo xtask repo msl pr-comment` and
publishes it as a sticky PR comment. The comment embeds the package pass-rate,
MLS contract coverage, and OMC trace-accuracy markdown tables so reviewers can
inspect the MSL gate without downloading artifacts first. Its top summary also
shows deltas against the resolved MSL quality baseline. Forked pull requests
receive the uploaded artifacts from the read-only CI run, then a separate
`workflow_run` publisher comments from the artifact using repository write
permissions.

When a full main CI run improves every ratcheted metric without regressions, the
MSL Baseline Ratchet workflow publishes the new baseline to that release asset.
Do not promote focused subsets or one-off explicit target files as the baseline.
Promotion requires a full-run snapshot with non-empty `omc_version` metadata.

Focused debugging runs can use `RUMOCA_MSL_SIM_MATCH`,
`RUMOCA_MSL_SIM_LIMIT`, `RUMOCA_MSL_SIM_TARGETS_FILE`, or
`RUMOCA_MSL_TARGET_SCOPE=committed-targets`, but those runs are not baseline
updates. These explicit focused/partial modes skip required OMC parity and say
so in their output; they do not report or imply that the full quality gate
passed.

For commit-to-commit regression diffs, run both worktrees with the same focused
target JSON, then generate machine-readable buckets with:

```bash
cargo xtask repo msl parity-manifest \
  --rumoca-results-file <worktree>/target/msl/results/msl_results.json \
  --omc-simulation-reference-file <worktree>/target/msl/results/omc_simulation_reference.json \
  --output-file <worktree>/target/msl/results/parity_fail_manifest.json
```

Compare `msl_quality_current.json`, `parity_fail_manifest.json`, and the
per-model `[sim_*]` log lines before inspecting emitted IR artifacts.

## OMC reference pool and compile-speed comparison

`cargo xtask repo msl omc-simulation-reference` generates the OMC simulation baseline
(`omc_simulation_reference.json`) that the trace gate compares rumoca against,
and emits the rumoca-vs-OMC compile-speed report. It runs a pool of persistent
`omc --interactive=zmq` worker sessions (the OMC analogue of the rumoca warm
worker): each worker loads the MSL once, pulls per-model jobs, and is killed +
respawned (with its whole process group, so hung simulation grandchildren are
reaped) on a per-model timeout.

| Concern | Behavior |
|---|---|
| Pool size | One worker per physical core, minus headroom on large hosts (`--workers 0` = auto); each worker pinned to a core. |
| Per-model timeout | `--batch-timeout-seconds` (wall, compile+simulate). Kept equal to the rumoca per-model budget so timing is fair. |
| Caching | Results are reused while the OMC version and MSL source are unchanged (`cache_key` in the JSON). `--force` re-runs everything. |
| Scope | All targets by default (so models that later pass rumoca already have a baseline); `--rumoca-sim-ok-only` is the CI fast subset. |
| Subsetting | `--model-regex '<re>'` scopes a run to matching models — the fast path for local iteration. |

### Compile-speed artifacts

Restricted to models where the OMC and rumoca traces **agree** (high/near band),
so only matching results are timed:

- `msl_speed_comparison.json` — the single data contract. Its `_about` block
  defines every metric (OMC compile = `timeTotal - timeSimulation`; `speedup =
  omc_compile / rumoca_compile`, >1 = rumoca faster; scaling binned by
  `scalar_equations`, the flattened system size — not states, which are 0 for
  most MSL examples).
- `msl_speed_scaling.html` — a self-contained **local** scatter plot (one point
  per model, x = scalar equations, y = compile seconds, rumoca vs OMC) rendered
  with the same embedded uPlot backend as `plot-compare`. Open it in a browser.

The plot is rendered two ways from that one JSON:

- **Local**: `omc-simulation-reference` writes `msl_speed_scaling.html` (uPlot).
- **PR comment**: `cargo xtask repo msl pr-comment` reads the JSON and renders the table
  plus a mermaid `xychart`. GitHub cannot execute JS, so the PR plot is mermaid,
  not the uPlot viewer — and it is produced only by `pr-comment`, not on every
  OMC run.

### Fast local subset

```bash
# Scope to a regex; reuses cached OMC + existing rumoca traces, then writes
# msl_speed_comparison.json + msl_speed_scaling.html for just that subset.
cargo xtask repo msl omc-simulation-reference \
  --model-regex 'Mechanics\.Translational\.Examples'
```
