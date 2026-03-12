# SPEC_0025: Pull Request Review Process

## Status
ACTIVE

## Summary
Defines the mandatory review process for all pull requests to ensure code quality, maintainability, and compliance with project standards.

## Motivation

A consistent PR review process ensures:
1. **Quality** - Defects are caught before merge
2. **Consistency** - Code follows project conventions
3. **Maintainability** - Code remains accessible to humans and AI assistants
4. **Compliance** - Changes adhere to specifications and standards

## Review Checklist

### 1. Specification Compliance

All changes MUST comply with existing specifications in `spec/`:

| Check | Description |
|-------|-------------|
| SPEC_0001 | DefId system consistency |
| SPEC_0002 | Scope tree structure |
| SPEC_0008 | Phase error handling |
| SPEC_0009 | Common crate usage |
| SPEC_0017 | Ordered collections (IndexMap) |
| SPEC_0019 | Array preservation |
| SPEC_0020 | Model algorithm lowering + function algorithm preservation |
| SPEC_0021 | Code complexity limits |
| SPEC_0022 | MLS compiler compliance |
| SPEC_0023 | Crate architecture |
| SPEC_0024 | Diagnostic instrumentation |

**Review questions:**
- Does the change violate any existing specification?
- If introducing new patterns, should a new spec be created?
- Are spec references included in code comments where appropriate?
- Does the change preserve the directed crate dependency graph (no crate-forwarding re-exports that bypass intended layer boundaries)?

### 2. Modelica Language Standard (MLS) Compliance

Changes affecting Modelica semantics MUST cite relevant MLS sections:

**Required for:**
- Parser changes
- Type system modifications
- Equation handling
- Connection processing
- Instantiation logic

**Format:**
```rust
// MLS §8.3.4: If-equations can contain any equation type in branches
fn flatten_if_equation(...) { ... }
```

**Review questions:**
- Is the MLS reference accurate?
- Does the implementation match MLS semantics?
- Are edge cases from MLS considered?

### 3. Code Efficiency Review

Identify potential performance bottlenecks before they reach production:

**Checklist:**

| Check | Concern |
|-------|---------|
| Algorithm complexity | O(n) vs O(n²) vs O(n log n) |
| Memory allocation | Unnecessary clones, Vec growth |
| Collection choice | HashMap vs IndexMap vs Vec |
| Iteration patterns | Nested loops over large data |
| String operations | Repeated allocations |
| Lock contention | Shared mutable state |

**Review questions:**
- What is the algorithmic complexity of new code paths?
- Are there opportunities to avoid cloning?
- Could this become a bottleneck as the codebase scales?
- Are expensive operations inside hot loops?

**Profiling:**
For significant changes, consider running:
```bash
cargo flamegraph --bin rumoca -- --model LargeModel --json model.mo
```

### 4. Code Maintainability Review

Ensure code remains accessible to both human developers and AI assistants.

#### 4.1 Clippy Compliance

All code MUST pass clippy with project-configured lints:

```bash
cargo clippy --workspace --all-targets --all-features -- -D warnings
```

**Zero tolerance for:**
- `clippy::unwrap_used` in library code
- `clippy::panic` in library code
- `clippy::todo` in merged code
- `clippy::dbg_macro` in merged code

**No `#[allow]` for clippy lints:**
- `#[allow(clippy::...)]` attributes MUST NOT be used to silence clippy warnings
- Exception: Generated code (e.g., parser code from parol) may use `#[allow]`
- If clippy flags an issue, the code MUST be refactored to comply
- Complex algorithms should be broken into smaller helper functions

#### 4.2 Code Complexity (SPEC_0021)

| Metric | Target | Limit (clippy) |
|--------|--------|-----------------|
| Function lines | < 60 lines | 100 (SPEC_0021) |
| Nesting depth | < 3 levels | 4 (SPEC_0021) |
| Function parameters | < 5 | 7 (SPEC_0021) |

**Review questions:**
- Should this function be split?
- Can nested logic be extracted?
- Are there too many parameters (consider a struct)?
- Does the change comply with `SPEC_0021` module decomposition rules (no `include!(...)` complexity/file-size bypass)?

#### 4.3 Code Clarity

**Required:**
- Clear, descriptive function names
- Module-level documentation
- Comments for non-obvious logic
- Consistent naming conventions
- Conformance to `SPEC_0029` import/re-export discipline and crate-boundary guardrails.

**Avoid:**
- Abbreviations without context
- Magic numbers (use named constants)
- Deep nesting
- Long parameter lists

**Review questions:**
- Would a new contributor understand this code?
- Could an AI assistant correctly modify this code?
- Is the intent clear from reading the code?

### 5. Test Coverage

**Required tests:**
- Unit tests for new functions
- Integration tests for new features
- Regression tests for bug fixes

**MSL Compile/Balance Test (for compiler changes):**

Changes to parsing, instantiation, flatten, or code generation phases MUST verify no regression in MSL compilation and balance rates:

```bash
cargo test --release --package rumoca-test-msl --test msl_tests \
  balance_pipeline::balance_pipeline_core::test_msl_all -- --ignored
```

**MSL Simulation + Scoreboard Freshness Gate (for compiler or simulator changes):**

For compiler/simulator behavior changes, run the unified MSL gate once:

```bash
cargo test --release --package rumoca-test-msl --test msl_tests \
  balance_pipeline::balance_pipeline_core::test_msl_all -- --ignored
```

The gate is responsible for compile/balance/simulation, OMC parity refresh rules,
trace comparison, and writing `target/msl/results/msl_quality_current.json`.

Coverage-trim workflow and baseline handling are defined in:
- `spec/SPEC_0030_COVERAGE_TRIM_PROCESS.md`

If the run is approved and you want to update the committed baseline:

```bash
rum repo msl promote-quality-baseline
```

Mandatory freshness checks before commit:
- `target/msl/results/msl_results.json` `git_commit` MUST match `git rev-parse HEAD`
- `target/msl/results/omc_reference.json`, `target/msl/results/omc_simulation_reference.json`, and `target/msl/results/sim_trace_comparison.json` MUST come from the same unified gate run window/target set
- `target/msl/results/msl_quality_current.json` MUST be regenerated in the same run window
- `target/msl/results/omc_simulation_reference.json` runtime ratio stats (`runtime_comparison.ratio_stats.system_ratio_both_success`, `runtime_comparison.ratio_stats.wall_ratio_both_success`) MUST be populated with `sample_count > 0` (not `null`)
- `crates/rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json` MUST only be updated via explicit promotion from `target/msl/results/msl_quality_current.json`

Changes MUST be compared against the baseline in
`crates/rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json` (baseline lineage is tracked in git history):

- compiled model count must not decrease
- balanced model count must not decrease
- compile phase time must not increase
- both-balanced agreement count (vs OMC) must not decrease
- lucky-balance count (vs OMC) must not increase
- rumoca-unbalanced-vs-OMC-balanced count must not increase
- rumoca-failed-vs-OMC-succeeded count must not increase
- overall balance rate may stay flat for an iteration, but must not regress versus baseline
- runtime speedup medians (OMC/Rumoca) must not regress by more than 20% versus baseline for both system and wall ratios
- fixes should be root-cause and generalizable; avoid model-specific balancing hacks that are not Modelica-spec compliant

When a regression is unavoidable, the PR MUST include explicit justification and reviewer approval.

**Review questions:**
- Are edge cases covered?
- Do tests verify the fix, not just exercise the code?
- Are tests deterministic (no flaky tests)?
- Does the MSL balance test show any regression?
- Does the PR report deltas versus `msl_quality_baseline.json` for compile, balance, simulation, and OMC parity stats?
- Does the change improve or preserve general Modelica language-specification compliance (instead of special-casing one model)?
- Was the committed baseline promoted from `target/msl/results/msl_quality_current.json` generated at the current commit?

### 6. Documentation

**Required for:**
- Public API changes
- New features
- Changed behavior

**Format:**
- Rust doc comments (`///`) for public items
- Module-level docs (`//!`) for context
- CHANGELOG entries for user-visible changes

### 7. Contribution Provenance and License Compliance

All contributions MUST be Apache-2.0 compliant.

Requirements:
- PRs MUST be reviewed for unattributed copied code.
- External code/content (if any) MUST include clear attribution and source provenance.
- External code/content (if any) MUST be verified as license-compatible with Apache-2.0.
- If external material is adapted, the PR description MUST state what was used, from where, and under which license.

## Review Process

### 1. Author Checklist

Before requesting review:
- [ ] Repository hooks are installed (`rum repo hooks install`)
- [ ] Local `pre-push` hook passes (fast quick gate: fmt, clippy, rustdoc, workspace tests)
- [ ] Manual quick verification passes (`rum verify quick`)
- [ ] `cargo fmt --check` passes
- [ ] `cargo clippy --workspace --all-targets --all-features -- -D warnings` passes
- [ ] `cargo test --workspace` passes
- [ ] MSL balance test shows no regression (for compiler changes)
- [ ] For compiler/simulator behavior changes: ran `balance_pipeline::balance_pipeline_core::test_msl_all`
- [ ] For compiler/simulator behavior changes: `target/msl/results/msl_results.json` `git_commit` matches `git rev-parse HEAD`
- [ ] For compiler/simulator behavior changes: gate refreshed or reused consistent `omc_reference.json`, `omc_simulation_reference.json`, and `sim_trace_comparison.json` for the current target sets
- [ ] For compiler/simulator behavior changes: regenerated `target/msl/results/msl_quality_current.json`
- [ ] For compiler/simulator behavior changes: `omc_simulation_reference.json` has populated runtime ratio stats (`runtime_comparison.ratio_stats.system_ratio_both_success.sample_count > 0`, `runtime_comparison.ratio_stats.wall_ratio_both_success.sample_count > 0`)
- [ ] For compiler/simulator behavior changes: if baseline changed, promoted with `rum repo msl promote-quality-baseline`
- [ ] For coverage-trim or coverage-gate changes: followed `spec/SPEC_0030_COVERAGE_TRIM_PROCESS.md` (run, report, gate, and explicit promotion/rollback policy)
- [ ] MSL metrics are compared against `crates/rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json`
- [ ] No unattributed copied code is introduced
- [ ] Any external material is attributed and verified Apache-2.0 compatible
- [ ] Self-review completed
- [ ] CHANGELOG updated (if applicable)

### 2. Reviewer Checklist

During review:
- [ ] Specification compliance verified
- [ ] Directed crate dependency graph preserved (no cross-layer crate-forwarding re-exports)
- [ ] MLS compliance checked (if applicable)
- [ ] Efficiency concerns addressed
- [ ] Maintainability criteria met
- [ ] Tests are adequate
- [ ] MSL balance test shows no regression (for compiler changes)
- [ ] For compiler/simulator behavior changes: committed baseline (if changed) was explicitly promoted from a fresh current-commit `target/msl/results/msl_quality_current.json`
- [ ] For compiler/simulator behavior changes: unified gate produced a consistent `omc_reference.json`, `omc_simulation_reference.json`, and `sim_trace_comparison.json` for the same target sets and run window
- [ ] PR includes baseline delta report from `msl_quality_baseline.json` for compile, balance, simulation, and OMC parity stats
- [ ] Runtime speedup medians (OMC/Rumoca) do not regress by more than 20% versus baseline, or approved justification is included
- [ ] Reviewed for unattributed copied code or unclear provenance
- [ ] External material (if any) is attributed and Apache-2.0 compatible

### 3. Merge Requirements

- At least one approving review
- All CI checks passing
- No unresolved conversations
- Branch is up-to-date with target
- Provenance and Apache-2.0 license compliance confirmed

### 4. Git Commit Guidelines

**Signed-off-by Requirement:**
- All commits MUST be signed off using `git commit -s`
- This adds a `Signed-off-by: Name <email>` line indicating DCO compliance
- Commits without Signed-off-by should be amended or rebased

**AI Attribution:**
- The human developer is responsible for all committed code
- AI-assisted code is still human-authored code; the human reviewed and accepted it
- Do NOT use `Co-Authored-By` headers for AI assistants

## Automated Checks

The following are enforced by CI:

```yaml
# .github/workflows/ci.yml
- cargo fmt --check
- cargo clippy --workspace --all-targets --all-features -- -D warnings
- cargo test --workspace  # includes architecture_hardening_test guardrails (SPEC_0029)
- cargo doc --no-deps
```

## Examples

### Good PR Description

```markdown
## Summary
Fix for-equation range evaluation when binding references sibling parameters.

## Changes
- Qualify binding expressions with parent prefix in `variables.rs`
- Skip qualification for modification bindings (outer scope references)

## MLS Reference
MLS §8.3.3: For-equation ranges can use parameter expressions.

## Testing
- Added unit test for component parameter references
- Verified with MSL 4.1.0 (reduced failures by 146)

## Checklist
- [x] Clippy clean
- [x] Tests pass
- [x] SPEC_0022 compliance verified
```

### Review Comment Examples

**Efficiency concern:**
> This nested loop over `all_variables` and `all_equations` is O(n*m). Consider building a lookup map first for O(n+m).

**Maintainability concern:**
> This function is 80 lines. Consider extracting the inner match arms into helper functions per SPEC_0021.

**MLS concern:**
> MLS §8.3.4 allows nested equations in if-branches. Does this implementation handle the recursive case?

## References

- SPEC_0021: Code Complexity Guidelines
- SPEC_0022: MLS Compiler Compliance
- SPEC_0024: Diagnostic Instrumentation
- [Modelica Language Specification](https://specification.modelica.org/)
- [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/)
