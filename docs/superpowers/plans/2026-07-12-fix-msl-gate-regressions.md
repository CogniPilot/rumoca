# Repair MSL Gate Regressions Implementation Plan

> **For Codex:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to execute this plan task-by-task.

**Goal:** Repair the shared compiler/runtime defects behind the current Modelica Standard Library Flat/DAE, initialization, simulation, and trace gate regressions, then run the repository gates and push the verified signed-off result to `origin/main`.

**Architecture:** Fix each defect at its owning semantic phase: Flat record-alias canonicalization must preserve instance-unique connector paths; DAE clock lowering must distinguish periodic clock ticks from ordinary Boolean edges; the simulation driver must respect solver-time tolerance at root boundaries; Solve refresh/projection must preserve alternate pivots and validate the complete algebraic residual tail. No model-specific patches, source-text reconstruction, baseline weakening, exclusions, or OpenModelica pin changes are allowed.

**Tech Stack:** Rust workspace, pinned `nightly-2026-02-27`, Rumoca Flat/DAE/Solve IR, SUNDIALS-backed runtime, `xtask repo msl`, Modelica Standard Library 4.1.0, DCO signed-off commits.

---

## Global constraints and evidence

- Work only in `/Users/hechuan/dev-home/worktrees/rumoca/fix-ci-lockfile-merge` on `fix/ci-lockfile-merge`.
- Follow `AGENTS.md`, `CONTRIBUTING.md`, `SPEC_0001_DEFID`, `SPEC_0007_IR_PIPELINE`, `SPEC_0022_MLS_COMPILER_COMPLIANCE`, `SPEC_0025_PR_REVIEW_PROCESS`, `SPEC_0029_CRATE_BOUNDARIES`, and `SPEC_0031_COMPILER_PHILOSOPHY`.
- Start every production change from a focused failing regression test. Record the RED output before implementing GREEN.
- Preserve semantic identity with `DefId` plus instance ancestry. Never reconstruct identity by parsing source spans, display strings, or leaf names.
- Keep DAE lowering deterministic and solver-independent. Keep full algebraic projection as the semantic authority; optimizations require a full-tail postcondition and a correct fallback.
- Do not change MSL baselines, floors, exclusions, selected targets, timeouts, or comparison tolerances to make the gate pass.
- The checked MSL source remains 4.1.0. OpenModelica is reference-only and is not involved in Flat, DAE, IC, or raw simulation success.
- Each implementation task ends in a small signed-off commit and a task review before the next implementation task starts.

Fresh pre-fix evidence:

- Focused `Modelica.Electrical.QuasiStatic.SinglePhase.Examples.SeriesResonance` fails ToDae with `129 equations, 134 unknowns` in `/tmp/rumoca-msl-series-fresh`.
- Current full gate has `563` Flat, `533` DAE, `520` balanced, `176` IC, `154` simulation, and `74` acceptable compared traces; all relevant repository floors are missed.
- Clean historical `84e6cfc0` already reproduced the Flat/DAE and part of the initialization failures. Dependency lockfile changes are therefore not the mechanism owner.

### Task 1: Preserve connector instance identity in Flat record-alias canonicalization

**Files:**
- Modify: `crates/rumoca-phase-flatten/src/postprocess_record_alias.rs`
- Test: `crates/rumoca-phase-flatten/src/postprocess_record_alias_tests.rs`
- Verify: `crates/rumoca-test-msl/tests/msl_tests/msl_flatten_tests.rs`

**Step 1: Write the failing regression test**

Add `record_alias_canonicalization_preserves_distinct_connector_record_endpoints`. Construct variables for `device.i`, `device.pin_p.i.re/im`, and `device.pin_n.i.re/im`; construct an equation whose two aggregate references have distinct instance paths and resolved identity. Assert that canonicalization preserves `device.pin_p.i` and `device.pin_n.i`, including their semantic metadata.

**Step 2: Prove RED**

Run:

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-flatten record_alias_canonicalization_preserves_distinct_connector_record_endpoints -- --exact --nocapture
```

Expected pre-fix failure: both endpoints collapse to `device.i` and their resolved identity is cleared.

**Step 3: Remove the invalid generic fallback**

Remove or narrow `decomposed_record_field_candidate` so a variable merely existing at a sibling leaf cannot authorize alias projection. Legitimate record aliasing must come from the established alias map or a canonicalizer that proves the same declaration and instance ancestry. Update the older sibling-leaf test whose expectation encoded name-based guessing.

**Step 4: Verify the unit scope and real model**

Run:

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-flatten postprocess_record_alias -- --nocapture
target/debug/xtask repo msl rerun --model 'Modelica.Electrical.QuasiStatic.SinglePhase.Examples.SeriesResonance' --results-dir /tmp/rumoca-msl-series-flat-fixed
```

Expected: the new identity regression is green; SeriesResonance no longer loses connector endpoints and progresses past its former Flat/DAE imbalance.

**Step 5: Commit and task-review**

```bash
git add crates/rumoca-phase-flatten/src/postprocess_record_alias.rs crates/rumoca-phase-flatten/src/postprocess_record_alias_tests.rs
git commit -s -m "fix(flatten): preserve connector instance identity"
```

### Task 2: Pin near-future roots to solver time within tolerance

**Files:**
- Modify: `crates/rumoca-eval-solve/src/sim_driver.rs`
- Test: simulation-driver tests colocated with `sim_driver.rs`

**Step 1: Write the failing backend regression**

Add `near_future_root_within_tolerance_pins_to_backend_time`. Use a fake backend at `1.0`, report a root at `1.0 + 5e-13`, and make `state_mut_back` reject forward interpolation. Assert that the shared root handler succeeds and processes the root at backend time.

**Step 2: Prove RED**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-eval-solve near_future_root_within_tolerance_pins_to_backend_time --lib -- --exact --nocapture
```

Expected pre-fix failure: interpolation is requested outside the current step.

**Step 3: Restore the shared tolerance invariant**

In `handle_root_crossing`, when `t_root` is slightly ahead of backend time but within the existing root-time tolerance, use backend time before mutating state. Do not widen the tolerance or add model-specific branches.

**Step 4: Verify driver and representative MSL simulations**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-eval-solve sim_driver --lib -- --nocapture
target/debug/xtask repo msl rerun --model 'Modelica.Electrical.Analog.Examples.ThyristorBehaviourTest' --results-dir /tmp/rumoca-msl-root-thyristor
target/debug/xtask repo msl rerun --model 'Modelica.Electrical.Analog.Examples.SwitchWithArc' --results-dir /tmp/rumoca-msl-root-switch
```

Expected: no `Interpolation time is not within current step` error.

**Step 5: Commit and task-review**

```bash
git add crates/rumoca-eval-solve/src/sim_driver.rs
git commit -s -m "fix(sim): clamp near-future roots within tolerance"
```

### Task 3: Rearm periodic clock aliases at every tick

**Files:**
- Modify: `crates/rumoca-phase-dae/src/when_guard.rs`
- Test: `crates/rumoca/tests/clocked_sample_regression.rs`
- Test as needed: `crates/rumoca-phase-dae/src/runtime_precompute/tests/clock_alias_tests.rs`

**Step 1: Write the failing native regression**

Add `native_simulation_rearms_clock_alias_edge_at_every_periodic_tick` to the existing clocked sample regression. Assert the clocked assignment output at more than one periodic tick, not only the continuous ramp.

**Step 2: Prove RED**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca --test clocked_sample_regression native_simulation_rearms_clock_alias_edge_at_every_periodic_tick -- --exact --nocapture
```

Expected pre-fix failure: the assignment updates at the first tick, then stays stale because `pre(clock)` never rearms.

**Step 3: Fix clock guard lowering at the semantic owner**

Allow `unfold_alias_expression` to return an unfolded expression when it is either relational or `is_clock_constructor_condition`. This lets a proven `clock = Clock(...)` alias chain reach the existing direct-tick lowering path instead of becoming `clock && !pre(clock)`. Retain ordinary Boolean edge semantics for non-clock aliases. Do not refresh every `pre()` value at runtime and do not infer clocks from strings.

**Step 4: Verify focused clock semantics**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-dae clock_alias -- --nocapture
rustup run nightly-2026-02-27 cargo test -p rumoca --test clocked_sample_regression -- --nocapture
target/debug/xtask repo msl rerun --model 'Modelica.Clocked.Examples.Elementary.ClockSignals.AssignClock' --results-dir /tmp/rumoca-msl-clock-fixed
```

Expected: each periodic tick rearms and updates the discrete assignment; ordinary Boolean `when` behavior remains unchanged.

**Step 5: Commit and task-review**

```bash
git add crates/rumoca-phase-dae crates/rumoca/tests/clocked_sample_regression.rs
git commit -s -m "fix(dae): rearm periodic clock alias ticks"
```

### Task 4: Preserve projection alternatives in algebraic refresh plans

**Files:**
- Modify: `crates/rumoca-eval-solve/src/refresh_plan.rs`
- Test: `crates/rumoca-eval-solve/src/runtime/tests.rs`

**Step 1: Write the failing plan/runtime regression**

Add `refresh_uses_projection_alternative_when_explicit_row_target_is_runtime_singular`. Reuse the parameter-select fixture, provide both an explicit row target and projection alternatives for the same target, and force the primary row slope to zero while an alternate row remains solvable.

**Step 2: Prove RED**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-eval-solve refresh_uses_projection_alternative_when_explicit_row_target_is_runtime_singular -- --exact --nocapture
```

Expected pre-fix failure: `.or_insert` keeps the explicit row but drops all projection alternatives, leading to `residual does not depend on it`.

**Step 3: Merge plan semantics without hiding errors**

When explicit and projection rows target the same unknown, preserve the explicit primary row and merge unique projection alternatives in deterministic order. Keep the existing runtime alternate-row solve path; do not revive residual nudging.

**Step 4: Verify unit scope and representative model**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-eval-solve refresh_ --lib -- --nocapture
target/debug/xtask repo msl rerun --model 'Modelica.Electrical.Analog.Examples.CharacteristicIdealDiodes' --results-dir /tmp/rumoca-msl-refresh-fixed
```

Expected: the runtime switches to a valid alternate pivot and the model completes initialization/simulation.

**Step 5: Commit and task-review**

```bash
git add crates/rumoca-eval-solve/src/refresh_plan.rs crates/rumoca-eval-solve/src/runtime/tests.rs
git commit -s -m "fix(solve): preserve algebraic refresh alternatives"
```

### Task 5: Enforce complete algebraic projection coverage and residual validation

**Files:**
- Modify: `crates/rumoca-phase-solve/src/lib.rs`
- Modify: `crates/rumoca-solver/src/runtime/projection.rs`
- Modify as required: initialization/event projection call sites in `crates/rumoca-eval-solve/src/`
- Test: `crates/rumoca-phase-solve/src/tests/projection_plan_more.rs`
- Test: projection tests colocated with `crates/rumoca-solver/src/runtime/projection.rs`

**Step 1: Write the failing runtime postcondition test**

Add `project_algebraics_rejects_nonzero_residual_row_omitted_from_plan`. Create two algebraic residual rows while the plan covers only one; leave the omitted row nonzero. Assert a structured projection error naming the omitted row.

**Step 2: Prove RED**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-solver project_algebraics_rejects_nonzero_residual_row_omitted_from_plan -- --exact --nocapture
```

Expected pre-fix failure: the omitted row is initialized to zero and projection falsely returns success.

**Step 3: Write the failing producer invariant test**

Add `solve_problem_projection_plan_covers_every_algebraic_tail_row` around `lower_projection_plan`, including an algebraic row with empty/unsupported incidence. Assert that lowering either produces complete deterministic coverage or rejects the structurally invalid projection plan explicitly.

**Step 4: Prove producer RED**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-solve solve_problem_projection_plan_covers_every_algebraic_tail_row -- --exact --nocapture
```

**Step 5: Implement producer and runtime invariants**

- Make Solve lowering account for every algebraic residual tail row and its unknown coverage; reject an incomplete balanced projection plan instead of silently skipping it.
- Compute/check the actual full algebraic residual tail after projection. Never initialize omitted rows as successful zeros.
- Keep the full OdeModel projection as the initialization/event semantic authority. A fast refresh may remain only when followed by the full-tail postcondition and a correct full-projection fallback.

**Step 6: Verify projection and initialization models**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-solve projection_plan -- --nocapture
rustup run nightly-2026-02-27 cargo test -p rumoca-solver projection --lib -- --nocapture
rustup run nightly-2026-02-27 cargo test -p rumoca-eval-solve --lib
target/debug/xtask repo msl rerun --model 'Modelica.Electrical.Analog.Examples.OpAmps.LowPass' --results-dir /tmp/rumoca-msl-projection-fixed
```

Expected: no false projection success; initialization either converges with the complete plan or fails at lowering with an actionable structural invariant rather than reaching BDF with a nonzero algebraic tail. The representative model must initialize and simulate.

**Step 7: Commit and task-review**

```bash
git add crates/rumoca-phase-solve crates/rumoca-solver/src/runtime/projection.rs crates/rumoca-eval-solve
git commit -s -m "fix(solve): validate complete algebraic projection"
```

### Task 6: Run full gates, synchronize truth, and push

**Files:**
- Modify only if behavior/contracts changed: `spec/` and repository documentation selected by `architecture-sync-guard`
- Inspect: `crates/rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json`
- Inspect: generated MSL results, but do not commit generated artifacts unless repository rules require them

**Step 1: Run focused regression set together**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-flatten record_alias_canonicalization_preserves_distinct_connector_record_endpoints -- --exact
rustup run nightly-2026-02-27 cargo test -p rumoca-eval-solve near_future_root_within_tolerance_pins_to_backend_time --lib -- --exact
rustup run nightly-2026-02-27 cargo test -p rumoca --test clocked_sample_regression native_simulation_rearms_clock_alias_edge_at_every_periodic_tick -- --exact
rustup run nightly-2026-02-27 cargo test -p rumoca-eval-solve refresh_uses_projection_alternative_when_explicit_row_target_is_runtime_singular -- --exact
rustup run nightly-2026-02-27 cargo test -p rumoca-solver project_algebraics_rejects_nonzero_residual_row_omitted_from_plan -- --exact
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-solve solve_problem_projection_plan_covers_every_algebraic_tail_row -- --exact
```

**Step 2: Run repository Rust gates**

```bash
rustup run nightly-2026-02-27 cargo fmt --all -- --check
rustup run nightly-2026-02-27 cargo clippy --workspace --all-targets -- -D warnings
rustup run nightly-2026-02-27 cargo test --workspace
rustup run nightly-2026-02-27 cargo doc --workspace --no-deps
```

**Step 3: Run the full MSL and semantic gates**

Run the exact commands documented by the repository for the full MSL quality gate and ModelicaTest semantic gate. The full non-partial MSL run must meet every checked-in floor without changing the baseline. Investigate and repair any remaining shared mechanism regression before continuing.

**Step 4: Run architecture and final code review**

Use `architecture-sync-guard`, update the relevant spec only if the enforced contract is not already recorded, then run the SDD final branch reviewer across the full branch diff. Resolve all blocking findings and rerun affected gates.

**Step 5: Verify DCO, branch state, and remote freshness**

```bash
git status --short
git log --format='%h %s%n%(trailers:key=Signed-off-by,valueonly)' origin/main..HEAD
git fetch origin
git rev-list --left-right --count origin/main...HEAD
```

Expected: only intentionally ignored local SDD artifacts remain untracked; every branch commit has `Signed-off-by`; remote main has not advanced unexpectedly.

**Step 6: Push and verify remote main**

Follow the `push` skill and repository branch policy. Push the reviewed branch result to `origin/main`, then fetch/read back the remote SHA and confirm it equals local `HEAD`. If remote main advanced, stop and rebase/merge safely, rerun required gates, and review the new diff before retrying.
