# CLI-52 Upstream-Equivalent Cleanup Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Remove the remaining ClimaMind-owned pure-discrete session implementation and make the unified simulation facade use the upstream solver backends' no-state sessions.

**Architecture:** Keep `SimulationSession` as the upstream-owned facade and delegate every session to the selected Diffsol or RK45 backend. Delete the facade-level `discrete_stepper` implementation, which duplicates and bypasses the canonical backend no-state event orchestration. Preserve all later ClimaMind compiler/runtime deltas that are unrelated to the 28 upstream-equivalent source commits.

**Tech Stack:** Rust workspace, Cargo tests, Rumoca Solve IR/session APIs.

## Global Constraints

- Latest verified refs are `origin/main=17b13432ff6c86c1df6089478a74891f049283d3` and `upstream/main=e6884d035f700955e4e2cccaaa6230ca79f21e20` after a fresh fetch on 2026-07-14.
- Preserve the public Git DAG and the Linear provenance document; do not rebase/rewrite the 411 non-merge commits and do not revert the 28 historical source commits.
- Do not merge the five unrelated commits after merge-base `24209c803442ce0402a2fe47c3533280758c88d3` as part of CLI-52; `git merge-tree --write-tree origin/main upstream/main` reports broad unrelated conflicts.
- Upstream implementations are authoritative for all 28 `upstream-equivalent` items; no old/new dual path may remain.
- Preserve non-equivalent later ClimaMind deltas; do not replace whole current files from `upstream/main`.
- Do not modify CogniPilot upstream and do not create an external PR.
- Follow TDD: the deadline-sensitive no-state session regression must fail against the old facade stepper before production code changes.
- Final commits must include `Signed-off-by` and must not include AI `Co-Authored-By` trailers.

---

### Task 1: Delegate zero-state sessions to upstream solver backends

**Files:**
- Modify: `crates/rumoca-bind-wasm/src/tests.rs`
- Modify: `crates/rumoca-sim/src/simulation_session.rs`
- Modify: `crates/rumoca-sim/src/lib.rs`
- Delete: `crates/rumoca-sim/src/discrete_stepper.rs`
- Modify: `crates/rumoca-solver/src/runtime/pre_params.rs`
- Modify: `crates/rumoca-solver/src/lib.rs`
- Modify: `crates/rumoca-solver-rk45/src/no_state.rs`
- Modify: `crates/rumoca-solver-rk45/src/lib.rs`
- Modify: `crates/rumoca-solver-rk45/src/tests.rs`
- Modify: `crates/rumoca-solver-diffsol/src/tests/mod.rs`

**Interfaces:**
- Consumes: `rumoca_solver_diffsol::SimulationSession::from_solve_model` and `rumoca_solver_rk45::SimulationSession::from_solve_model`, both of which own zero-state behavior.
- Produces: unchanged public `rumoca_sim::SimulationSession` methods (`new_with_diagnostics`, `set_input`, `reset`, `advance_to`, `step`, `time`, `get`, `state`, `input_names`, `variable_names`).

- [x] **Step 1: Write the failing deadline-sensitive regression**

Extend `test_interactive_session_runs_pure_discrete_model_with_guarded_dynamic_subscript` so the controller updates inside `when sample(0.02, 0.02) then ... end when`. Advance first to `0.01` and assert `y` and `k` have not changed; then advance to `0.02` and assert the first sample update; advance to `0.04` with a changed input and assert the second update uses `pre(k)` and the runtime-indexed table.

The behavioral shape must be:

```rust
session.set_input("u", 1.5).expect("set input u");
session.advance_to(0.01).expect("advance before first sample");
assert_eq!(session.get("y").expect("read y"), Some(0.0));

session.advance_to(0.02).expect("advance to first sample");
assert_eq!(session.get("y").expect("read y"), Some(1.5));

session.set_input("u", 2.0).expect("set input u");
session.advance_to(0.04).expect("advance to second sample");
assert_eq!(session.get("y").expect("read y"), Some(4.0));
```

- [x] **Step 2: Run the regression and verify RED**

Run:

```bash
cargo test -p rumoca-bind-wasm --features sim-rk45 test_interactive_session_runs_pure_discrete_model_with_guarded_dynamic_subscript -- --nocapture
```

Expected: FAIL at the first scheduled deadline (`0.02`): the facade-owned `discrete_stepper` has no scheduled-event orchestration. Without `--features sim-rk45`, Cargo silently filters out this feature-gated test and reports `running 0 tests`.

Observed RED: the assertions at `0.01` passed and the first scheduled update at `0.02` failed (`y = 0.0`, expected `1.5`). After removing the facade stepper, the same regression exposed a backend-owned RK45 no-state bug: the first event updated correctly, but the second event at `0.04` remained at `1.5` instead of `4.0`.

- [x] **Step 3: Remove the duplicate implementation**

In `simulation_session.rs`, remove:

```rust
SimulationSessionInner::Discrete(...)
```

and every corresponding match arm, remove `discrete_session_from_solve_model`, and remove both `state_scalar_count() == 0` early returns. Keep the existing Diffsol and RK45 construction paths unchanged so their canonical no-state sessions own zero-state behavior.

In `lib.rs`, remove:

```rust
mod discrete_stepper;
```

Delete `crates/rumoca-sim/src/discrete_stepper.rs` entirely.

The delegated RK45 path also needs the same scheduled-root lifecycle already used by its stateful backend. Its generic root search must ignore roots owned by `scheduled_root_conditions`; at an exact scheduled `EventEntry`, it must clear the relation memory before the event snapshot and seed the projected update with the scheduled root override. Shared at-time helpers in `rumoca-solver` keep the stateful and no-state paths on one mechanism. The ordinary left-limit refresh remains unchanged for non-scheduled caller deadlines.

- [x] **Step 4: Verify GREEN and focused backend behavior**

Run:

```bash
cargo test -p rumoca-bind-wasm --features sim-rk45 test_interactive_session_runs_pure_discrete_model_with_guarded_dynamic_subscript -- --nocapture
cargo test -p rumoca-solver-rk45 rk45_session_runs_no_state_discrete_controller -- --nocapture
cargo test -p rumoca-solver-rk45 rk45_no_state_session_rearms_periodic_sample_edges -- --nocapture
cargo test -p rumoca-bind-wasm --features sim-diffsol test_interactive_session_runs_pure_discrete_model_with_guarded_dynamic_subscript -- --nocapture
cargo test -p rumoca-solver-diffsol no_state -- --nocapture
cargo test -p rumoca-sim --all-features
```

Expected: PASS; no zero-state facade variant remains.

- [x] **Step 5: Run the five upstream-equivalent focused groups**

Run:

```bash
cargo test -p rumoca-phase-solve discrete_history_update_from_input_keeps_state_as_target
cargo test -p rumoca-phase-solve lower_discrete_rhs_keeps_pre_selector_branch_as_runtime_select
cargo test -p rumoca-phase-solve lower_expression_dynamic_param_subscript_emits_indexed_load
cargo test -p rumoca-phase-codegen test_embedded_c_templates_render_solve_ir
cargo test -p rumoca --features template-runtime-tests --test backend_template_runtime_regression embedded_c_ -- --nocapture
cargo test -p rumoca-opt
cargo test -p rumoca --test neural_ode_tensor_solve_ir
cargo test -p rumoca-sim --all-features parses_multirate_lockstep_config
cargo test -p rumoca-sim --all-features rejects_invalid_multirate_lockstep_rates
```

Expected: PASS with no duplicate-path restoration.

The two `rumoca-sim` name-filter commands include `--all-features` so they select their feature-gated tests and each run one test. The full `rumoca-sim --all-features` gate runs all 85 tests.

- [x] **Step 6: Commit the implementation**

```bash
git add docs/superpowers/plans/2026-07-14-cli-52-upstream-equivalent-cleanup.md crates/rumoca-bind-wasm/src/tests.rs crates/rumoca-sim/src/simulation_session.rs crates/rumoca-sim/src/lib.rs crates/rumoca-sim/src/discrete_stepper.rs crates/rumoca-solver/src/runtime/pre_params.rs crates/rumoca-solver/src/lib.rs crates/rumoca-solver-rk45/src/no_state.rs crates/rumoca-solver-rk45/src/lib.rs crates/rumoca-solver-rk45/src/tests.rs crates/rumoca-solver-diffsol/src/tests/mod.rs
git commit -s -m "refactor(sim): adopt upstream no-state sessions"
```

Expected: one signed-off implementation commit, net-negative production code.
