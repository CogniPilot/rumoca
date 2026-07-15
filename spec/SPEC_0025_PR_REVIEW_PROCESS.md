# SPEC_0025: Pull Request Review Process

## Status
ACCEPTED

## Summary
Defines the mandatory PR review process and the rules that the GitHub PR
template (`.github/pull_request_template.md`) must implement. The template
is the canonical author/reviewer artifact; this spec defines the rules
the template enforces.

## Motivation

A consistent PR review process catches defects before merge, keeps code
accessible to humans and AI assistants, and enforces spec compliance.
Mirroring the PR template against this spec gives one source of truth.

## PR Template Alignment

The repository PR template at `.github/pull_request_template.md` MUST
contain a section for every mandatory rule below. Section names in this
spec mirror the template's section names so reviewers can map them
one-to-one.

| SPEC_0025 §       | PR template section       |
|-------------------|---------------------------|
| §1 Summary        | "Summary"                 |
| §2 Spec/MLS       | "Spec / MLS Alignment"    |
| §3 Risk/Design    | "Risk and Design Notes"   |
| §4 Testing        | "Testing"                 |
| §5 Size Budget    | "Code Size Budget"        |
| §6 Reviewer Gate  | "Reviewer Checklist"      |

## Mandatory Rules

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
| Document any new abstraction, public API, or migration path | New surface is permanent until removed; the PR is where the trade-off is recorded |

### 4. Testing

| Rule | Why |
|---|---|
| List the key commands run | Reviewers reproduce locally; absent commands signal untested paths |
| Describe the behavior or regression covered | Tests must prove behavior, not just exercise code |
| State commands NOT run and why | Honest disclosure beats silent gaps |

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
RUMOCA_MSL_INCLUDE_MODELICATEST=1 \
RUMOCA_MSL_REQUIRE_SELECTED_TARGETS_SUCCESS=1 \
RUMOCA_MSL_SIM_TARGETS_FILE=crates/rumoca-test-msl/tests/msl_tests/modelica_test_targets_ci.json \
RUMOCA_MSL_SIM_SET=full \
cargo test --release --package rumoca-test-msl --features msl-full-test \
  --test msl_tests balance_pipeline::balance_pipeline_core::test_msl_all \
  -- --nocapture
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
- `rum` is a developer orchestration tool for repository maintenance,
  verification bundles, packaging, editor/WASM checks, release workflows, and
  avoiding ad-hoc shell/Python scripts. It MAY run Cargo test commands as part
  of a larger workflow, but test ownership and documentation remain centered on
  the underlying Cargo command.
- The `rumoca` compiler binary is product-facing. It MUST NOT grow repository
  test-runner subcommands.
- The workspace MUST NOT use `#[ignore]` for parked or heavyweight tests. Tests
  are either ordinary Cargo tests, Cargo-feature-selected heavyweight tests, or
  deleted until they encode implemented behavior.

| MSL rule | Why |
|---|---|
| Run the unified MSL gate for any parser, instantiate, flatten, ToDae, or sim change | One gate produces consistent OMC reference + sim trace + quality artifacts |
| Run the separate ModelicaTest semantic gate for language-semantics changes when the MSL source-tree `ModelicaTest` package is available | ModelicaTest is an assertion-heavy semantic suite and must not be conflated with the curated MSL example target set |
| Compare against the resolved MSL quality baseline (`cargo xtask verify msl-parity` downloads the promoted `msl-quality-baseline/msl_quality_baseline.json` release asset and falls back to `crates/rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json` offline) | Baseline is the regression bar |
| Cumulative MSL stage counts (parse, flatten, DAE, IR-Solve, initial-condition solve, simulation) MUST NOT materially decrease on the fixed root-example baseline denominator; full-library runs may tolerate one-model host jitter | Early-stage pass-rate increases are always improvements, later stages are compared against their own cumulative counts, and CI/OMC host variance must not block equivalent runs |
| Balanced / OMC-agreement counts MUST NOT decrease | These are headline correctness and numerical-quality numbers |
| Focused or limited MSL runs MUST mark quality snapshots as partial and partial snapshots MUST NOT be promoted | Prevents local-debug subsets from becoming the committed release baseline |
| Trace-quality metrics MUST be gated against the resolved promoted baseline when OMC parity data is available | Prevents balanced-but-numerically-worse simulations from passing unnoticed |
| Runtime system-time speedup median MUST NOT regress by > 35%. Wall-time uses the same 35% limit only for fresh, affinity-correct, healthy-host paired measurements; otherwise it remains visible as ADVISORY and does not mask any correctness or system-time failure. | Keeps the system-time regression gate unconditional while preventing cached or noisy wall measurements from blocking a correct run |
| Promoted baseline release-asset updates require a successful full main CI run and a non-regressing ratchet decision; checked-in fallback updates remain explicit via `cargo xtask repo msl promote-quality-baseline` | Prevents silent baseline drift |
| Coverage trim/gate updates follow `cargo xtask coverage {run,report,gate}` workflow | Coverage promotion is explicit only |

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
| At least one approving review | Two-eyes on every merge |
| All CI checks passing | CI gates (incl. `architecture_hardening_test`, `spec_budget_test`) are the non-negotiables |
| No unresolved conversations | Open threads = open questions |
| Branch is up-to-date with target | Avoids merge-on-stale surprises |
| Signed-off-by on every commit (`git commit -s`) | DCO compliance |
| No `Co-Authored-By` for AI assistants | The human author owns the code; AI assistance is human-authored work |
| External material attributed and Apache-2.0 compatible | Provenance and license compliance |
| No `#[allow(clippy::...)]` outside generated code | Allow signals an unfixed maintainability issue (SPEC_0021) |
| No new trait without ≥ 2 concrete impls | Single-impl traits are noise |
| No old/new code paths left side-by-side without explicit migration plan | Dead-but-alive code accretes |

### 7. Maintainability Quick Reference

See SPEC_0021 for the authoritative function-length, nesting, and arg-count
limits. The workspace `Cargo.toml` declares them as clippy `deny` lints, so
they are enforced by §4 commands.

## CI Enforcement

| Check | Enforces |
|---|---|
| `cargo fmt --check` | Format consistency |
| `cargo clippy --all-features -- -D warnings` | SPEC_0021 maintainability limits |
| `cargo test --workspace` | All tests including `architecture_hardening_test` (SPEC_0029) and `spec_budget_test` (SPEC_0000 §3) |
| `cargo doc --no-deps` | Docs build without errors |
| MSL gate (compiler/sim changes) | resolved `msl_quality_baseline.json` regressions |
| ModelicaTest semantic gate (semantic compiler/sim changes) | selected `ModelicaTest.*` models compile, simulate, and preserve assertion/parity diagnostics |

## References

- [`.github/pull_request_template.md`](../.github/pull_request_template.md) — canonical PR template
- SPEC_0000 — spec writing and size guidelines
- SPEC_0007 — IR pipeline contracts
- SPEC_0008 — diagnostics and traceability
- SPEC_0021 — code complexity limits
- SPEC_0022 — MLS compiler compliance catalog
- SPEC_0029 — crate boundaries
- [Modelica Language Specification](https://specification.modelica.org/)
