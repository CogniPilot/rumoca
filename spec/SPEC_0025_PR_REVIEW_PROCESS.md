# SPEC_0025: Pull Request Review Process

## Status
ACCEPTED

## Summary
Defines mandatory review rules for the canonical `.github/pull_request_template.md` artifact.

## Motivation

Consistent reviews catch defects, preserve readability, and enforce spec compliance.

## PR Template Alignment

The PR template MUST contain a section for every mandatory rule below,
mapped by this table.

| SPEC_0025 §       | PR template section       |
|-------------------|---------------------------|
| §0 Branch Naming  | "Branch Naming"           |
| §1 Summary        | "Summary"                 |
| §2 Spec/MLS       | "Spec / MLS Alignment"    |
| §3 Risk/Design    | "Risk and Design Notes"   |
| §4 Testing        | "Testing"                 |
| §5 Size Budget    | "Code Size Budget"        |
| §6 Reviewer Gate  | "Reviewer Checklist"      |

## Mandatory Rules

### 0. Branch Naming

| Rule | Why |
|---|---|
| Use a descriptive branch name without an `agent/` prefix | Describe work rather than tooling |

### 1. Summary

| Rule | Why |
|---|---|
| Describe user-facing behavior change | Reviewers and changelog readers need to know what shipped |
| Cite the issue, spec, or design rule the PR addresses | Anchors the change against a recorded intent |

### 2. Spec / MLS Alignment

| Rule | Why |
|---|---|
| List the relevant active spec(s) checked | SPEC_0029 §6, SPEC_0007, SPEC_0021, etc. — every change must touch known rules |
| Cite the relevant MLS section(s) when Modelica semantics change | MLS is the language contract; changes without a citation are at risk of drifting |
| Name the crate/phase owner | Localizes review attention to the responsible layer |

Citation format in code:

```rust
// MLS §8.3.4: If-equations can contain any equation type in branches
fn flatten_if_equation(...) { ... }
```

### 3. Risk and Design Notes

| Rule | Why |
|---|---|
| State the main correctness risk | Forces the author to think adversarially before merge |
| State the main maintenance risk | Surfaces follow-up debt before the PR is closed |
| Justify the crate(s) the change lives in | Prevents drift across crate boundaries (SPEC_0029) |
| Document any new abstraction, public API, or migration path | Record permanent-surface trade-offs |

### 4. Testing

| Rule | Why |
|---|---|
| List the key commands run | Enable reproduction |
| Describe the behavior or regression covered | Prove behavior |
| State commands NOT run and why | Disclose gaps |
| Report Tier 1 evidence — focused suites plus the fixed 20-model canary delta — for every capability change | Tier 1 is the per-change done-criterion (SPEC_0033 §6a) |
| Quote cohort parity only from a complete Tier 2 566-model sweep, naming its commit | Partial, sharded, focused, and stale runs are not cohort evidence |

Run every command below under `CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4`
(SPEC_0033 §6a).

Pre-landing gate (every tip that lands MUST pass it, with coverage):

```bash
cargo xtask verify gate --rev <tip> --coverage
```

It runs CI's blocking steps on a committed snapshot in a fresh Cargo target,
printing `GATE_OK` or `GATE_FAILED` with revision and log.

Standard verification commands (all merged code MUST pass):

```bash
cargo fmt --check
cargo clippy --workspace --all-targets --all-features -- -D warnings
cargo test --workspace      # includes architecture_hardening_test + spec_budget_test
cargo doc --no-deps
```

MSL gate (compiler / simulator changes):

```bash
cargo test --release --package rumoca-test-msl --features msl-full-test \
  --test msl_tests balance_pipeline::balance_pipeline_core::test_msl_all \
  -- --nocapture
```

ModelicaTest semantic gate (compiler / simulator semantic changes):

```bash
cargo xtask verify msl-parity \
  --results-dir target/msl/modelicatest-results \
  --sim-targets-file crates/rumoca-test-msl/tests/msl_tests/modelica_test_targets_ci.json \
  --include-modelica-test \
  --require-selected-targets-success \
  --sim-set full
```

These flags are the only supported channel: no `RUMOCA_*` environment variable
configures the harness (SPEC_0018). `verify msl-parity` writes them to the
inspectable per-invocation config `target/msl/parity-config.json`, then runs the
same `test_msl_all` libtest as the gate above — Cargo-native underneath, per §4a.

Pinned `modelica_models` compatibility gate (compiler / simulator semantic changes):

```bash
rumoca-msl-profile --source-root /path/to/pinned/modelica_models \
  --model Tests.All --mode compile
rumoca-msl-profile --source-root /path/to/pinned/modelica_models \
  --model Tests.LieGroupTests.SO2 --mode simulate --stop-time 0.0
rumoca-msl-profile --source-root /path/to/pinned/modelica_models \
  --model Tests.LieGroupTests.SE2 --mode simulate --stop-time 0.0
```

### 4a. Test Workflow Interface

Rust developer workflow MUST remain Cargo-native.

- `cargo test` is the primary interface for tests. Developer tooling may wrap
  Cargo commands for CI orchestration or repeatability, but it MUST NOT replace
  or obscure the equivalent `cargo test` invocation.
- Optional heavyweight test suites MUST be selectable through Cargo-native
  mechanisms such as explicit test filters, package/test selection, or Cargo
  features. Do not require user-facing bespoke environment variables solely to
  decide whether a Rust test runs.
- `rum` orchestrates repository maintenance, verification bundles, packaging,
  editor/WASM checks, and releases instead of ad-hoc shell/Python scripts. It
  MAY run Cargo test commands inside a larger workflow, but test ownership and
  documentation stay on the underlying Cargo command.
- The `rumoca` compiler binary is product-facing. It MUST NOT grow repository
  test-runner subcommands.
- The workspace MUST NOT use `#[ignore]` for parked or heavyweight tests. Tests
  are either ordinary Cargo tests, Cargo-feature-selected heavyweight tests, or
  deleted until they encode implemented behavior.

| MSL rule | Why |
|---|---|
| Run the unified MSL gate for any parser, instantiate, flatten, ToDae, or sim change | One gate produces consistent OMC reference + sim trace + quality artifacts |
| Run the separate ModelicaTest semantic gate for language-semantics changes when the MSL source-tree `ModelicaTest` package is available | ModelicaTest is an assertion-heavy semantic suite and must not be conflated with the curated MSL example target set |
| Run the pinned `modelica_models` aggregate-compile and corpus-owned assertion-smoke gate for compiler/simulator semantic changes | A fixed external assertion corpus catches cross-library compatibility regressions without weakening MLS or MSL gates |
| Compare against the resolved MSL quality baseline (`cargo xtask verify msl-parity` downloads the promoted `msl-quality-baseline/msl_quality_baseline.json` release asset and falls back to `crates/rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json` offline) | Baseline is the regression bar |
| Every main commit's complete measured MSL snapshot MUST be retained as a commit-bound CI artifact, independently of improvement or release-asset promotion; newer main pushes MUST NOT cancel earlier main measurements | Reporting history follows every merge |
| PR comments MUST compare against the first parent of the actual CI test merge, print that full main SHA, and use its own push/main artifact; pending snapshots get a bounded wait, while missing, expired, partial or mismatched snapshots fail explicitly without a checked-in or different-commit fallback | Prevents old progress being credited to a new PR |
| The PR-vs-main check MUST fail on any previously successful per-model stage membership lost, even if another model holds the aggregate count constant; retain the complete model-loss report | Gains cannot hide a model regression |
| An explicitly reviewed checked-in full baseline MAY declare an exact `from_omc_version` -> `to_omc_version` migration and fixed target count; only a declaration matching both contexts takes precedence over the older promoted release until the next main run promotes it | Reference contexts must match; malformed or undeclared migrations fail |
| A corrected metric definition MAY lower a checked-in stage count only through a quality-gate schema-version migration that records the prior/new versions, prior/new count, affected cohort, and exact model set; the checked-in migration takes precedence over an older-schema promoted release until main promotes the new schema | Schema corrections cannot conceal compiler regressions |
| A checked-in full baseline MAY bridge a promoted asset across multiple reviewed migrations only with the exact source digest, source/target schemas, target count, and ordered evidence commits; all other old assets MUST fail | Promotion lag must not deadlock CI or permit a generic old-schema fallback |
| Every resolved full baseline MUST own the exact model roster behind its strict-high count; cohort-loss gates compare the current per-model table to that roster, while prior workflow artifacts are diagnostic history only and MUST NOT redefine the ratchet | Only certified rosters detect offsetting model losses |
| Every resolved full baseline MUST also own the roster of completions that are neither strict-high nor covered by a typed trace exception (`unexcepted_non_high_models`); the gate fails on a current completion outside it, the ratchet refuses a promotion adding to it, and the PR comment lists it by triage package (first three segments) with new entries marked | SPEC_0033 targets an empty roster; parity lanes use their own rows; only removals land |
| Cumulative MSL stage counts (parse, flatten, DAE, IR-Solve, initial-condition solve, strict-high simulation) MUST NOT materially decrease on the fixed root-example baseline denominator; full-library runs may tolerate one-model host jitter | A simulation pass requires strict-high trace parity |
| Balanced / OMC-agreement counts MUST NOT decrease | These are headline correctness and numerical-quality numbers |
| Focused or limited MSL runs MUST mark quality snapshots as partial and partial snapshots MUST NOT be promoted | Prevents local-debug subsets from becoming the committed release baseline |
| Trace-quality metrics MUST be gated against the resolved promoted baseline when OMC parity data is available | Prevents balanced-but-numerically-worse simulations from passing unnoticed |
| Runtime speedup medians MUST NOT regress by > 35% for system time or > 45% for wall time, over the baseline runtime cohort | Wall time includes hosted-runner scheduling; the wider wall margin retains the stricter system-time check |
| Fresh schema-14-or-newer snapshots with measured runtime and promotion candidates MUST bind per-model ratios to the exact roster, positive finite values, sample counts and medians; writers MUST check before output. Historical-reference validation activates only in a separately reviewed rollout with an eligible full non-regressing main baseline | Prevents mixed evidence without activating historical readers prematurely |
| Cohort-dependent quality totals and runtime medians MUST be compared over the models both baseline and run measured | Cohort growth must not read as regression |
| The PR comment's speed report MUST take every aggregate over the trace-agreeing (high or near) models, beside a high-only line, and compare compilation as time to runnable labelled JIT vs C toolchain and the FMU path, printed as not measured unless a CI job exports FMUs | A JIT-versus-C-toolchain speedup, or one counting disagreeing traces, is not like for like |
| The speed report MUST take agreement from the comparator's band table, count initialization in rumoca's simulation time as OMC's `timeSimulation` does, time only OMC rows collected under the run's own worker count and host, and print a methodology block of recorded runner, per-stage parallelism, integrator and tolerance per tool, output density, cache state, and parity gating; OMC reference collection MUST use the rumoca compile and simulation worker counts, both tools MUST use the OMC output grid (the experiment `Interval`, else 500 intervals), and unequal counts or grids are flagged | Timings taken under different contention are not comparable |
| Promoted baseline release-asset updates require a successful full main CI run and a non-regressing ratchet decision; checked-in fallback updates remain explicit via `cargo xtask repo msl promote-quality-baseline` | Prevents silent baseline drift |
| The coverage gate (`cargo xtask coverage gate --changed-since <base>`) fails on every function the change adds (its first line lies in an added hunk of `git diff -U0 <base>...HEAD`) that no workspace test executes; closures are exempt | Blame only this change; do not drop error context to satisfy counts |
| The one coverage exemption is `#[cfg_attr(coverage_nightly, coverage(off))]` on a function reachable only through an effect no test can drive (process exit, an interactive terminal, or a toolchain the coverage run lacks), with a comment naming the effect and the exercising job; the crate root enables `#![cfg_attr(coverage_nightly, feature(coverage_attribute))]`, and the gate report lists each one a change adds | Only this reviewed exemption applies |
| Workspace line coverage MUST NOT drop more than 0.25 points below the committed baseline; per-package zero-execution counts are reported, never enforced | Line coverage is stable; per-crate zero-execution counts drift |
| The line-coverage baseline is updated only by `cargo xtask coverage gate --promote-baseline` over CI's measurement artifacts | Coverage promotion is explicit only |

### 5. Code Size Budget

Every PR body MUST report these fields:

```
production_lines_added:
production_lines_deleted:
test_lines_added:
test_lines_deleted:
public_items_added:
public_items_removed:
files_touched:
net_added_lines:
```

| Rule | Why |
|---|---|
| Use `git diff --numstat origin/main...HEAD` for line deltas | Mechanical and reproducible |
| Use a public-API diff tool/script for `public_items_*` | Public surface is hard to reverse; deltas must be visible |
| `net_added_lines > 0` requires written justification + compression plan | Default bias is toward subtraction |
| Refactors and compatibility changes target net-negative or near-zero | Refactor without a size win is suspect |

### 6. Reviewer Gate

| Rule | Why |
|---|---|
| The landed tip passed `cargo xtask verify gate --rev <tip> --coverage` (GATE_OK) before landing | Run CI's blocking checks and coverage on the committed tip before landing |
| At least one approving review | Two-eyes on every merge |
| All CI checks passing | CI gates (incl. `architecture_hardening_test`, `spec_budget_test`) are the non-negotiables |
| Capability PRs show Tier 1 evidence and source every parity number | SPEC_0033 §6a cadence must be checkable at review |
| No unresolved conversations | Open threads = open questions |
| Branch is up-to-date with target | Avoids merge-on-stale surprises |
| Signed-off-by on every commit (`git commit -s`) | DCO compliance |
| Commit messages contain no named AI assistant or AI-session references and no `Co-Authored-By` for AI assistants | The human author owns the code; tooling provenance does not belong in project history |
| External material attributed and Apache-2.0 compatible | Provenance and license compliance |
| No new `#[allow(clippy::...)]` without the SPEC_0021 exception comment directly above it | SPEC_0021 "Exceptions" sanctions documented allows; an undocumented one hides an unfixed maintainability issue |
| No new trait without ≥ 2 concrete impls | Single-impl traits are noise |
| No old/new code paths left side-by-side without explicit migration plan | Dead-but-alive code accretes |

### 7. Maintainability Quick Reference

SPEC_0021 defines function-length, nesting, and argument limits; workspace
Clippy `deny` lints enforce them through §4 commands.

## CI Enforcement

| Check | Enforces |
|---|---|
| `cargo fmt --check` | Format consistency |
| `cargo clippy --all-features -- -D warnings` | SPEC_0021 maintainability limits |
| `cargo test --workspace` | All tests including `architecture_hardening_test` (SPEC_0029) and `spec_budget_test` (SPEC_0000 §3) |
| `cargo doc --no-deps` | Docs build without errors |
| MSL gate (compiler/sim changes) | resolved `msl_quality_baseline.json` regressions |
| ModelicaTest semantic gate (semantic compiler/sim changes) | selected `ModelicaTest.*` models compile, simulate, and preserve assertion/parity diagnostics |
| Pinned `modelica_models` compatibility gate (semantic compiler/sim changes) | the fixed-revision aggregate `Tests.All` model compiles and the corpus-owned Rumoca assertion smoke targets simulate successfully |

## References

- [`.github/pull_request_template.md`](../.github/pull_request_template.md) — canonical PR template
- SPEC_0000 — spec writing and size guidelines
- SPEC_0007 — IR pipeline contracts
- SPEC_0008 — diagnostics and traceability
- SPEC_0021 — code complexity limits
- SPEC_0022 — MLS compiler compliance catalog
- SPEC_0029 — crate boundaries
- SPEC_0033 — development process and the §6a verification cadence
- [Modelica Language Specification](https://specification.modelica.org/)
