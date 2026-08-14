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
updates.

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

## Cohort pinning: the per-model band table

Aggregate band counts (`agreement_high`, `agreement_minor`,
`agreement_deviation`) do not pin the population behind them. A model that stops
simulating disappears from the comparator's `models` map, and the headline count
can stay flat while the compared set moves underneath it — which is how
`DCPM_Start` went from strict-high to `sim_solver_fail` between two
certifications with neither run naming the departure.

A run therefore writes `msl_band_table.json` next to `sim_trace_comparison.json`,
derived from that run's comparator output and stamped with the run's scope
(`full` for a cohort certification, `partial` for a focused or sharded run). The
table has one row per model in the run's `sim_target_models` roster — the cohort,
not the compared set — so a model that was never simulated is a row with a
reason, not a gap:

| Field | Meaning |
|---|---|
| `band` | `high` / `near` / `deviation` for a compared model, `absent` otherwise |
| `exit_reason` | mandatory on `absent`: `sim_failed`, `not_attempted`, `rumoca_trace_missing`, `reference_missing`, `trace_missing_side_unrecorded`, `comparator_failed`, `no_comparable_samples`, `excluded`, `not_compared` |
| `exit_detail` | the solver status + error code, the phase the run stopped at, the OMC message, or that exclusion's own rationale |
| `run_scope` | `full` for a cohort run, `partial` for a focused one |
| `source.trace_comparison_digest` | content hash of the comparator output the rows came from — the table's run identity |
| `git_commit` | the certification's own commit, read from its `msl_results.json` |
| channel counts, `max_channel_bounded_normalized_l1`, `bounded_normalized_l1_score` | the per-model evidence behind the band |

Each exit reason names the boundary that stopped the comparison, and the
comparator decides it where the knowledge is: `sim_trace_comparison.json` records
`{kind, detail}` per entry in `skipped` and `missing_trace`, so a comparator
crash is never filed as a policy exclusion and our own missing trace is never
filed as a missing OMC reference. Policy exclusions come from the tracked
`msl_trace_compare_exclusions.json`, where every entry carries its own reason.

### Rotation and run scope

Rotation is keyed on run identity, not on call count: persisting is idempotent,
so re-running the tool over an unchanged results directory rewrites the same
table and leaves `msl_band_table_previous.json` alone. Only a new comparator
output rotates. A focused (Tier 1) run derives its reading and writes nothing —
its 20-model table is not the cohort's — and `persist_band_table` refuses outright
to rotate a `full` table aside for a `partial` one. `cargo xtask verify msl-parity
--clean-results` preserves both tables for the same reason: wiping them would make
every run a first certification, with no diff and no departures.

### Acceptance contract

A certification artifact is **comparable** only when its band table carries: the
`msl_band_table` schema at a version this build reads; a comparator-output digest
binding it to a run; at least one row; at least one compared row; a unique
`model_name` per row; every banded row with channel counts and no exit reason;
and every `absent` row with a named exit reason. Anything else is rejected
outright — consumers report "not comparable" rather than diffing against a
partial table.

The table is also checked against **itself**: its declared counts and its row
digest are recomputed from the rows, and the row count is held to the recorded
`cohort_roster_models`. A hand-edited band, a count edited upward, and a row set
that is not the cohort are all refused.

Reading a table as a *directory's* evidence adds the binding check: its digests
must match the `sim_trace_comparison.json` and `msl_results.json` sitting beside
it. A well-formed table copied in from another run, or left behind when the
comparator re-ran, is refused rather than quoted as this run's band population.
The exclusion list that attributed the `excluded` rows is recorded by digest too,
so which policy list produced a reading is on the artifact rather than ambient.

The quality gate reads the persisted table for its strict-high accounting and
refuses to quote a number when the table and the OMC reference disagree about the
compared, strict-high, near, or deviation counts (one of the two artifacts is
stale). Its summary states either the cohort movement or the named reason it
could not be computed — never zeros standing in for "we could not tell" — and
lists every model that left the compared set with its exit reason and every model
still compared over fewer channels than before, because a band is a *share* of
the compared channels and coverage can collapse under a band that never moves.

Two movement readings fail the gate on a full-cohort run: a model that held the
strict-high band and is no longer compared, and a certification with no
predecessor table to diff against at all. The second is deliberate — the rules
that read movement are inert without a predecessor, and a run must not pass with
them silently switched off. CI restores the previous certification's table as
`msl_band_table_previous.json` before the gate; `--clean-results` preserves both
tables so a local re-run keeps its predecessor.

```bash
# Persist / re-derive the table for a results directory (--check validates only,
# including that the table on disk belongs to that directory). With no
# --results-dir, the tool reads the directory the parity config names.
cargo xtask repo msl band-table --results-dir target/msl/results

# ENTERED / LEFT / BAND-CHANGED / COVERAGE-DROPPED between two certifications.
# Directories written before the artifact existed are still diffable: the table is
# derived on the fly from sim_trace_comparison.json + msl_results.json.
cargo xtask repo msl transition-diff \
  --before target/msl/results-baseline \
  --after  target/msl/results
```

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
