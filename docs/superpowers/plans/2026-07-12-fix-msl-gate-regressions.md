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
- Modify: `crates/rumoca-phase-flatten/src/postprocess.rs`
- Modify: `crates/rumoca-phase-flatten/src/postprocess_record_alias.rs`
- Test: `crates/rumoca-phase-flatten/src/postprocess/substitute_constant_tests.rs`
- Test: `crates/rumoca-phase-flatten/src/postprocess_record_alias_tests.rs`
- Verify: `crates/rumoca-test-msl/tests/msl_tests/msl_flatten_tests.rs`

**Step 1: Write the failing regression test**

Add `record_alias_canonicalization_preserves_distinct_connector_record_endpoints` and `collapse_index_refs_preserves_structured_distinct_record_endpoints`. Construct variables for `device.i.re/im`, `device.pin_p.i.re/im`, and `device.pin_n.i.re/im`; construct an equation whose two aggregate references have distinct instance paths and shared source declaration identity. Assert that both record-alias canonicalization and the record/index collapse pass preserve `device.pin_p.i` and `device.pin_n.i`, including their semantic metadata.

**Step 2: Prove RED**

Run:

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-flatten record_alias_canonicalization_preserves_distinct_connector_record_endpoints -- --nocapture
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-flatten collapse_index_refs_preserves_structured_distinct_record_endpoints -- --nocapture
```

Expected pre-fix failure: both endpoints collapse to `device.i` and their resolved identity is cleared. The first real-model owner is `CollapseIndexRewriter` treating an already-structured reference as a rendered-name recovery candidate; the record-alias fallback is a second instance of the same invalid identity inference.

**Step 3: Remove the invalid generic fallback**

Guard the rendered-name recovery branch in `CollapseIndexRewriter` so only references without structured semantic identity enter repeated/penultimate-field recovery. Remove or narrow `decomposed_record_field_candidate` so a variable merely existing at a sibling leaf cannot authorize alias projection. Legitimate record aliasing must come from the established alias map or a canonicalizer that proves the same declaration and instance ancestry. Update the older sibling-leaf test whose expectation encoded name-based guessing.

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
rustup run nightly-2026-02-27 cargo test -p rumoca-eval-solve near_future_root_within_tolerance_pins_to_backend_time --lib -- --nocapture
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

Expected: no `Interpolation time is not within current step` error. A root genuinely beyond tolerance must remain a structured error and be repaired at its separate root/tstop lifecycle owner; it must not be hidden by this tolerance task.

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

### Task 4: Preserve BLT causal ownership and projection alternatives in refresh plans

**Files:**
- Modify: `crates/rumoca-eval-solve/src/refresh_plan.rs`
- Test: `crates/rumoca-eval-solve/src/runtime/tests.rs`

**Step 1: Write the failing plan/runtime regressions**

Add `refresh_plan_preserves_blt_causal_primary_over_explicit_target`. Use a 2x2 coupled projection block whose `causal_steps` cross-map rows and targets while `implicit_row_targets` suggest the opposite direct placement. Assert that each BLT causal step remains the primary row and that direct placement is retained only as a deterministic alternative. Also add or retain `refresh_uses_projection_alternative_when_primary_is_runtime_singular` to force a primary row slope to zero while an alternate row remains solvable.

**Step 2: Prove RED**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-eval-solve refresh_plan_preserves_blt_causal_primary_over_explicit_target --lib -- --nocapture
rustup run nightly-2026-02-27 cargo test -p rumoca-eval-solve refresh_uses_projection_alternative_when_primary_is_runtime_singular --lib -- --nocapture
```

Expected pre-fix failure: direct-first insertion shadows the structural BLT primary, and `.or_insert` drops projection alternatives, leading either to a wrong stable algebraic fixed point or `residual does not depend on it`.

**Step 3: Merge plan semantics without hiding errors**

For coupled blocks, use `AlgebraicProjectionBlock.causal_steps` as the authoritative primary mapping in producer order. For targets not covered by causal steps, use deterministic incidence matching; keep remaining block rows as stable, deduplicated alternatives. Apply `implicit_row_targets` only to targets not covered by projection, or as alternatives for a covered target. Keep the runtime alternate-row solve path; do not revive residual nudging.

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
- Modify: `crates/rumoca-solver-diffsol/src/runtime.rs`
- Modify as required: initialization/event projection call sites in `crates/rumoca-eval-solve/src/`
- Test: `crates/rumoca-phase-solve/src/tests/projection_plan_more.rs`
- Test: projection tests colocated with `crates/rumoca-solver/src/runtime/projection.rs`

**Step 1: Write the failing runtime postcondition test**

Add `project_algebraics_rejects_nonzero_residual_row_omitted_from_plan`. Create two algebraic residual rows while the plan covers only one; leave the omitted row nonzero. Assert a structured projection error naming the omitted row.

**Step 2: Prove RED**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-solver project_algebraics_rejects_nonzero_residual_row_omitted_from_plan -- --nocapture
```

Expected pre-fix failure: the omitted row is initialized to zero and projection falsely returns success.

**Step 3: Write the failing producer invariant test**

Add `solve_problem_projection_plan_covers_every_algebraic_tail_row` around `lower_projection_plan`, including an algebraic row with empty/unsupported incidence. Assert that lowering either produces complete deterministic coverage or rejects the structurally invalid projection plan explicitly.

**Step 4: Prove producer RED**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-solve solve_problem_projection_plan_covers_every_algebraic_tail_row -- --nocapture
```

**Step 5: Implement producer and runtime invariants**

- Make Solve lowering account for every algebraic residual tail row and its unknown coverage; reject an incomplete balanced projection plan instead of silently skipping it.
- Compute/check the actual full algebraic residual tail after projection. Never initialize omitted rows as successful zeros.
- Restore `rumoca-solver-diffsol` initialization/event callbacks to full OdeModel projection semantics (`project_algebraics_and_detect_changes` or its current equivalent). A fast refresh may remain only as a pre-optimization followed by the full-tail postcondition and a correct full-projection fallback; it must never replace the semantic projector.

**Step 6: Verify projection and initialization models**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-solve projection_plan -- --nocapture
rustup run nightly-2026-02-27 cargo test -p rumoca-solver projection --lib -- --nocapture
rustup run nightly-2026-02-27 cargo test -p rumoca-solver-diffsol runtime --lib -- --nocapture
rustup run nightly-2026-02-27 cargo test -p rumoca-eval-solve --lib
target/debug/xtask repo msl rerun --model 'Modelica.Electrical.Analog.Examples.OpAmps.LowPass' --results-dir /tmp/rumoca-msl-projection-fixed
```

Expected: no false projection success; initialization either converges with the complete plan or fails at lowering with an actionable structural invariant rather than reaching BDF with a nonzero algebraic tail. The representative model must initialize and simulate.

**Step 7: Commit and task-review**

```bash
git add crates/rumoca-phase-solve crates/rumoca-solver/src/runtime/projection.rs crates/rumoca-eval-solve
git commit -s -m "fix(solve): validate complete algebraic projection"
```

### Task 6: Lower record-constructor output projections without executable bodies

**Files:**
- Modify: `crates/rumoca-phase-solve/src/function_validation.rs`
- Modify: `crates/rumoca-phase-solve/src/lower/function_projection.rs`
- Test: Appendix-B validation tests under `crates/rumoca-phase-solve/src/`
- Test: `crates/rumoca-phase-solve/src/lower/tests/function_expression_tests.rs`

**Step 1: Write the failing validation regression**

Add `solve_input_validation_allows_record_constructor_output_projection_without_body`. Build an anonymous constructor function with `is_constructor=true`, positional inputs `re` and `im` where `im` has a default, a record output `result`, and an intentionally empty body. Validate a structural projection call such as `Pkg.RecordCtor.result.im(2.0)` whose synthetic call node is not itself marked constructor.

**Step 2: Prove validator RED**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-solve solve_input_validation_allows_record_constructor_output_projection_without_body -- --nocapture
```

Expected pre-fix failure: Appendix-B resolution finds the base constructor but incorrectly requires an executable body.

**Step 3: Write the failing lowering regression**

Add `lower_expression_projects_record_constructor_output_field_with_default` with the same generic constructor metadata. Lower `Pkg.RecordCtor.result.im(2.0)` and assert that the projected value is the constructor input/default binding for `im` (`0.0`), not a body-local output lookup.

**Step 4: Prove lowering RED**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-solve lower_expression_projects_record_constructor_output_field_with_default -- --nocapture
```

Expected pre-fix failure: `lower_projected_function_call` reports that projected output `result.im` cannot be resolved.

**Step 5: Implement constructor-projection semantics**

- After resolving the base DAE function, classify by its semantic `is_constructor` flag rather than the synthetic projection call flag.
- Appendix-B validation may accept an empty body only for a valid constructor output projection; it must still validate arguments, defaults, arity, and the requested output/field path.
- Lower a constructor output field by binding positional/named/default constructor inputs and selecting the matching record field. Reuse existing constructor argument-binding semantics; do not special-case `Complex`, a model name, or `re/im` strings beyond generic field matching.
- Keep structural scalarization's projection encoding unchanged and retain all validation for ordinary functions.

**Step 6: Verify focused phase and real model**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-solve function_projection -- --nocapture
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-solve appendix_b_validation -- --nocapture
target/debug/xtask repo msl rerun --model 'Modelica.Electrical.QuasiStatic.SinglePhase.Examples.SeriesResonance' --results-dir /tmp/rumoca-msl-series-constructor-fixed
```

Expected: valid constructor projections lower without an executable body; SeriesResonance passes the former Solve Appendix-B/`Complex.result.im` boundary and reaches runtime.

**Step 7: Commit and task-review**

```bash
git add crates/rumoca-phase-solve/src/function_validation.rs crates/rumoca-phase-solve/src/lower/function_projection.rs crates/rumoca-phase-solve/src/lower/tests/function_expression_tests.rs
git commit -s -m "fix(solve): lower record constructor projections"
```

### Task 7: Invalidate deferred roots across earlier scheduled events

**Files:**
- Modify: `crates/rumoca-eval-solve/src/sim_driver.rs`
- Test: simulation-driver tests colocated with `sim_driver.rs`

**Step 1: Write the failing scheduled-boundary regression**

Add `scheduled_event_invalidates_deferred_future_root_from_pre_event_trajectory`. Use a fake backend whose free step reports a root after the requested scheduled boundary (for example root `0.500033856224`, boundary `0.5`). The driver must interpolate the left limit, apply/reset the scheduled event at `0.5`, then step again under the post-event trajectory; it must never call `state_mut_back` with the stale pre-event root.

**Step 2: Prove RED**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-eval-solve scheduled_event_invalidates_deferred_future_root_from_pre_event_trajectory --lib -- --nocapture
```

Expected pre-fix failure: `pending_root_t` survives the scheduled event reset and is replayed against a backend reset to `0.5`, producing `root time ... is after backend time ...`.

**Step 3: Lock the ordinary-output behavior**

Add `ordinary_output_boundary_preserves_deferred_future_root`. A root beyond a normal output sample remains valid because interpolation does not change the continuous trajectory; the next target may resolve that deferred root. This prevents the repair from deleting all deferred-root behavior.

**Step 4: Fix root lifetime at the orchestration owner**

Treat a deferred root as belonging to the exact continuous trajectory on which it was located. Carry it across an ordinary dense-output sample, but invalidate it when an earlier scheduled event is applied and the backend is reset. Order `pending_root_t` assignment after the scheduled-boundary decision, or clear it explicitly as part of the event reset. Do not clamp the stale root, widen tolerance, or special-case a model/time.

**Step 5: Verify driver and representative model**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-eval-solve sim_driver --lib -- --nocapture
target/debug/xtask repo msl rerun --model 'Modelica.Electrical.Analog.Examples.SwitchWithArc' --results-dir /tmp/rumoca-msl-switch-root-lifetime-fixed
```

Expected: the stale `0.500033856224` pre-event root is not replayed after the `0.5` scheduled reset; the model progresses past its former future-root failure.

**Step 6: Commit and task-review**

```bash
git add crates/rumoca-eval-solve/src/sim_driver.rs
git commit -s -m "fix(sim): invalidate roots across scheduled resets"
```

### Task 8: Infer projected function-result cardinality from the selected field

**Files:**
- Modify: `crates/rumoca-phase-dae/src/scalar_inference/mod.rs`
- Test: `crates/rumoca-phase-dae/src/scalar_inference/regression_more_tests.rs`

**Step 1: Write the failing scalar-cardinality regression**

Add `test_infer_scalar_count_scalar_field_function_call_ignores_record_argument_width`. Construct an expression equivalent to `(ird * calc(params).resistance) - (D - Dinternal)`, where `calc` receives wide record arguments but `resistance` is a scalar result field. Assert equation scalar count `1`.

**Step 2: Prove RED**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-dae test_infer_scalar_count_scalar_field_function_call_ignores_record_argument_width -- --nocapture
```

Expected pre-fix failure: `infer_scalar_count_from_varrefs` descends into the function arguments and returns their record width (for Spice3 representatives, `48` or `31`) for a scalar projected output.

**Step 3: Fix expression-shape ownership**

Teach scalar inference that `FieldAccess(FunctionCall(...), field)` gets its cardinality from the selected function-result field metadata. Function arguments are dependencies and cannot determine the output field shape. Preserve array-valued result fields by using their declared dimensions; when a resolved scalar field is selected, return one. Do not clamp counts, special-case Spice3, or change structural balance rules.

**Step 4: Verify phase and representative models**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-dae scalar_inference -- --nocapture
target/debug/xtask repo msl rerun --model 'Modelica.Electrical.Spice3.Examples.Inverter' --results-dir /tmp/rumoca-msl-spice-inverter-cardinality-fixed
```

Expected: the four representative resistor equations remain scalar instead of expanding to `48/31/48/31`; Inverter's equation/unknown balance returns to zero and the broader Spice3 overconstraint cluster improves without exemptions.

**Step 5: Commit and task-review**

```bash
git add crates/rumoca-phase-dae/src/scalar_inference/mod.rs crates/rumoca-phase-dae/src/scalar_inference/regression_more_tests.rs
git commit -s -m "fix(dae): infer projected result field cardinality"
```

### Task 9: Run full gates, synchronize truth, and push

**Files:**
- Modify only if behavior/contracts changed: `spec/` and repository documentation selected by `architecture-sync-guard`
- Inspect: `crates/rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json`
- Inspect: generated MSL results, but do not commit generated artifacts unless repository rules require them

**Step 1: Run focused regression set together**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-flatten record_alias_canonicalization_preserves_distinct_connector_record_endpoints
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-flatten collapse_index_refs_preserves_structured_distinct_record_endpoints
rustup run nightly-2026-02-27 cargo test -p rumoca-eval-solve near_future_root_within_tolerance_pins_to_backend_time --lib
rustup run nightly-2026-02-27 cargo test -p rumoca --test clocked_sample_regression native_simulation_rearms_clock_alias_edge_at_every_periodic_tick -- --exact
rustup run nightly-2026-02-27 cargo test -p rumoca-eval-solve refresh_plan_preserves_blt_causal_primary_over_explicit_target --lib
rustup run nightly-2026-02-27 cargo test -p rumoca-solver project_algebraics_rejects_nonzero_residual_row_omitted_from_plan
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-solve solve_problem_projection_plan_covers_every_algebraic_tail_row
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

### Task 10: Restore the SPEC_0021 runtime-test file-size gate

**Files:**
- Modify: `crates/rumoca-eval-solve/src/runtime/tests.rs`
- Create as needed: a focused test submodule under `crates/rumoca-eval-solve/src/runtime/`

**Step 1: Preserve the actual RED**

Use the final workspace-test failure as RED evidence: `architecture_hardening_test::size_and_validation::span_debt::test_rs_files_stay_under_spec_0021_hard_limit` reports `runtime/tests.rs (2041 lines)` over the 2,000-line hard threshold.

**Step 2: Split by behavioral ownership**

Move one coherent group of runtime tests and only their private helpers into a dedicated submodule. Preserve test names/coverage and production behavior. Do not add a SPEC_0021 exception, suppress the architecture test, delete assertions, or mechanically compress code to evade the threshold.

**Step 3: Verify and review**

Run the moved test group, `cargo test -p rumoca-eval-solve --lib`, the exact architecture-hardening gate, fmt, clippy, and diff-check. Commit with DCO signoff and request independent task review before resuming Task 9's full gates.

### Task 11: Align review-scan with SPEC_0021 file-size exceptions

**Files:**
- Modify: `crates/xtask/src/review_scan_cmd.rs`
- Test: focused tests colocated with the review-scan command

**Step 1: Preserve the actual RED**

The canonical architecture gate accepts an over-2,000-line Rust file only when it contains the complete `SPEC_0021`, `file-size`, and `split plan` exception markers. `repo review-scan --fail-on-forbidden` currently ignores those markers and reports every changed over-limit file as forbidden. Thirteen branch-touched files are already over the threshold on `origin/main` and carry valid exceptions, so the scan fails while architecture hardening passes.

**Step 2: Add policy-parity tests**

Add focused tests proving that a file over the hard threshold with all required markers does not produce a forbidden hard-limit finding, while a file missing any required marker still does. Preserve hard failure for a new unexcepted threshold crossing; do not use base line count as a blanket grandfathering rule.

**Step 3: Implement shared semantics and verify**

Make review-scan honor the same explicit exception contract as the canonical architecture test. Keep any useful size audit visible at non-forbidden severity if appropriate. Run xtask focused tests, the architecture gate, the real `repo review-scan --fail-on-forbidden` command, fmt, clippy, and diff-check. Commit with DCO signoff and request independent review before resuming Task 9.

### Task 12: Preserve sequential last-write semantics for table algorithms across events

**Files:**
- Inspect/modify owning code under `crates/rumoca-phase-dae/src/algorithm_lowering/` and, only if evidence points downstream, discrete-row runtime code under `crates/rumoca-eval-solve/src/runtime/`
- Test: focused algorithm-lowering and end-to-end discrete event regressions

**Steps:**

1. Add an end-to-end RED for a two-point table algorithm: initialize `y`, iterate `i in 1:2`, and conditionally assign `y := x[i]` for `time >= t[i]`; assert `y=4` after `t=1` and `y=3` after `t=3`.
2. Use the RED/IR inspection to identify the first semantic loss among instance parameter-array projection, for-loop sequential expansion/last-write ownership, discrete-row generation, and row commit. Do not assume the scheduler is the owner: the Adder4 artifact proves other table instances handle the same `t=3` event.
3. Repair the first semantic owner without model/table special cases. Preserve sequential algorithm assignment semantics and stable instance identity.
4. Verify the focused tests and exact `Modelica.Electrical.Digital.Examples.Adder4` trace against OMC. Commit with DCO signoff and independent review.

### Task 13: Preserve shaped discrete enum-array start values

**Files:**
- Modify as owned: `crates/rumoca-phase-solve/src/solve_model.rs`
- Modify as owned: `crates/rumoca-eval-dae/src/eval/array_eval.rs`
- Test: focused start-value evaluation/lowering regressions

**Steps:**

1. Add a RED for a two-element discrete enum array whose start is `fill(enum_literal, 2)`, including generated `__pre__` variables. The current failure is `shaped array value: expected 2 value(s), got 1`.
2. Fix array evaluation/lowering so declared shape owns cardinality and `fill` preserves both values. Do not special-case Digital models or pad/clamp a mismatched result.
3. Verify the focused tests and representative DLAT/DFF models; require at least the eight identical-signature failures to cross the former start-value boundary. Commit with DCO signoff and independent review.

### Task 14: Use reference-aware dimensions for indexed aggregate elimination

**Files:**
- Modify: `crates/rumoca-phase-structural/src/eliminate/mod.rs`
- Modify/tests as needed: `crates/rumoca-phase-structural/src/variable_scope.rs`
- Test: structural elimination/incidence regressions

**Steps:**

1. Add a full-pipeline RED with two parent component instances and vector aggregate leaves such as `iH1[1].product2.u[2]` and `spacePhasor_b.v_[2]`. After trivial elimination and incidence reconstruction, assert parent instances do not mix, aggregate uses materialize correctly, and every leaf remains matchable.
2. Replace leaf-name-only aggregate dimension lookup with the existing structured-reference-aware scope API. Do not reconstruct paths from display strings or add model-specific matching.
3. Verify structural crate gates plus representative `PolyphaseRectifier` and machine models. Confirm the shared indexed aggregate cohorts cross the former structural-singular boundary before counting IC gains. Commit with DCO signoff and independent review.

### Task 15: Avoid redundant settled symbolic-dimension evaluation

**Files:**
- Modify: `crates/rumoca-phase-flatten/src/pipeline/flatten_pipeline.rs`
- Modify as owned: `crates/rumoca-phase-flatten/src/pipeline/context_and_tests.rs`
- Test: flatten pipeline symbolic-dimension regressions

**Steps:**

1. Add a deterministic RED with a test-only evaluation-attempt counter for a 3-by-2 record/component array with symbolic dimensions. Prove that settled bindings are currently recomputed across outer and inner fixed-point passes; do not use wall-clock time as the unit assertion.
2. Preserve every fixed-point pass and unresolved-binding retry, but reuse collected bindings and drive reevaluation from pending dependencies/dimension generations. Do not lower pass counts or increase timeout.
3. Verify identical final parameter/dimension maps and Flat IR, then run `CCCV_Stack`, `CCCV_StackRC`, `CCCV_Cell`, and `CCCV_CellRC` under the unchanged worker budget. Require the full gate Flat floor to recover. Commit with DCO signoff and independent review.
