# Fix CI Lockfile Merge Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Restore a valid, deterministic Cargo dependency graph after the upstream merge so CI can parse `Cargo.lock` on the repository-pinned Rust toolchain.

**Architecture:** Treat the malformed lockfile as a merge artifact, not as source-of-truth dependency intent. Re-resolve the merged workspace manifests with the pinned `nightly-2026-02-27` Cargo, then verify both locked parsing and the repository lint gate without changing application code or weakening CI.

**Tech Stack:** Rust, Cargo lockfile v4, `cargo xtask`, GitHub Actions.

## Global Constraints

- Work only in `/Users/hechuan/dev-home/worktrees/rumoca/fix-ci-lockfile-merge` on branch `fix/ci-lockfile-merge`.
- Keep Task 1 limited to `Cargo.lock` plus this implementation-plan artifact. Task 2 may change only the seven merge-damaged source files and the focused dependency test named below; do not alter manifests, CI workflows, unrelated application code, or quality baselines.
- Use the repository-pinned toolchain `nightly-2026-02-27` for reproduction and verification.
- Do not manually delete individual duplicate package blocks; regenerate the lock graph from the merged workspace manifests.
- Do not promote or weaken the MSL quality baseline.
- Commit the completed task locally; do not push.

---

### Task 1: Regenerate and verify the merged Cargo lock graph

**Files:**
- Modify: `Cargo.lock`
- Create: `docs/superpowers/plans/2026-07-12-fix-ci-lockfile-merge.md`
- Test: repository-pinned Cargo parser and `cargo xtask verify lint`

**Interfaces:**
- Consumes: the merged workspace manifests at commit `943dded78f5b0d3468902aa0b80d8e60f8976865`.
- Produces: a Cargo lockfile with one entry per package identity that all CI jobs can parse.

- [x] **Step 1: Verify the failing regression**

Run:

```bash
rustup run nightly-2026-02-27 cargo check --locked --package xtask --quiet
```

Expected: FAIL with `package 'ahash' is specified twice in the lockfile`.

- [x] **Step 2: Re-resolve the merged dependency graph**

Run:

```bash
rm Cargo.lock
rustup run nightly-2026-02-27 cargo generate-lockfile
```

Expected: Cargo creates a lockfile from the merged manifests without duplicate package identities.

- [x] **Step 3: Verify the focused regression is green**

Run:

```bash
rustup run nightly-2026-02-27 cargo check --locked --package xtask --quiet
```

Expected: exit code 0 with no duplicate-package parse error.

- [ ] **Step 4: Run the repository lint gate**

Run:

```bash
rustup run nightly-2026-02-27 cargo xtask verify lint
```

Expected: exit code 0.

- [x] **Step 5: Review scope and commit**

Run:

```bash
git diff --check
git status --short
git add Cargo.lock docs/superpowers/plans/2026-07-12-fix-ci-lockfile-merge.md
git commit -m "fix(ci): regenerate merged Cargo lockfile"
```

Expected: only the lockfile and plan artifact are committed.

---

### Task 2: Reconcile source-level merge artifacts and restore the lint gate

**Files:**
- Modify: `crates/rumoca-phase-structural/src/eliminate/mod.rs`
- Modify: `crates/rumoca-phase-structural/src/eliminate/boundary_scan.rs`
- Modify: `crates/rumoca-phase-structural/src/dae_prepare/mod.rs`
- Modify: `crates/rumoca-phase-structural/src/eliminate/direct_definition_index.rs`
- Modify: `crates/rumoca-phase-structural/src/eliminate/runtime_protection.rs`
- Modify: `crates/rumoca-phase-structural/src/eliminate/substitution_application.rs`
- Modify and test: `crates/rumoca-eval-solve/src/prepared/dependency.rs`
- Modify: `docs/superpowers/plans/2026-07-12-fix-ci-lockfile-merge.md`

**Interfaces:**
- Consumes: parent1 structural mechanism at `84e6cfc0`, upstream additions at `24209c80`, and their broken merge at `943dded7`.
- Produces: one coherent structural API plus dependency traversal for `LinearOp::ExternalCall`.

- [x] **Step 1: Verify the source-level RED**

Run:

```bash
rustup run nightly-2026-02-27 cargo check -p rumoca-phase-structural -p rumoca-eval-solve
```

Expected: FAIL with structural duplicate/missing-helper/signature errors and the non-exhaustive `LinearOp::ExternalCall` match.

- [x] **Step 2: Restore the parent1 structural API at the broken merge hunks**

Use `git show 84e6cfc0:<path>` and `git diff 84e6cfc0..943dded7 -- <path>` as the reference, but edit only the named hunks; do not checkout whole files.

In `eliminate/mod.rs`:

```text
- remove the duplicate substitution-target imports
- import expr_contains_indexed_multiscalar_slice_ref
- restore canonicalize_exact_indexing_in_continuous_equations(dae)?
- restore demote_direct_assigned_states_with_boundary_substitutions(dae, &result.substitutions)
- pass runtime_protected_unknowns and runtime_defined_discrete_targets to finish_boundary_elimination
- restore the parent1 ctx/rhs/live chooser implementation and its helper block
- restore scalar_subscript_ref_matches_substitution as a distinct helper and retain one aggregate_subscript_ref_matches_var
```

In `boundary_scan.rs`, restore the parent1 symbolic-candidate path and guards: substitution-overgrowth, runtime-sensitive, unanchored aggregate/connection alias, indexed-slice and pairwise-flow checks; construct `EliminationChoiceContext` and call the chooser with `ctx`, `rhs`, and `live`. Do not restore the removed `state_names` field.

In `dae_prepare/mod.rs`, restore these parent1 helpers and their derivative/component guard:

```rust
expr_contains_der_of_state_or_component
variable_dims_for_direct_demotion
der_call_target_subscripts
```

In `direct_definition_index.rs`, restore `has_other_non_connection_direct_definition`. In `runtime_protection.rs`, restore `assignment_target_name_in_dae`. In `substitution_application.rs`, call `apply_substitutions_in_order_with_derivatives_and_dae` with the DAE context.

- [x] **Step 3: Add the failing ExternalCall dependency tests**

Add focused tests in `prepared/dependency.rs` that build real `LinearOp::ExternalCall` rows:

```text
case 1: an effective argument register transitively loads the target y index => depends_on_y returns true
case 2: effective arguments contain only constants or another y index => depends_on_y returns false
case 3: arg_count exceeds the fixed args slice => depends_on_y conservatively returns true without panic
```

Run the focused tests before implementation. Expected: FAIL because `ExternalCall` is not handled.

- [x] **Step 4: Implement ExternalCall dependency traversal**

Add a match arm that examines exactly `args[..arg_count]`, recursively calls the existing dependency walker for each effective argument register, and treats malformed `arg_count` as conservatively dependent instead of panicking. Ignore unused capacity entries after `arg_count`.

- [x] **Step 5: Verify focused GREEN**

Run:

```bash
rustup run nightly-2026-02-27 cargo check -p rumoca-phase-structural -p rumoca-eval-solve
rustup run nightly-2026-02-27 cargo test -p rumoca-eval-solve prepared::dependency
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-structural boundary_
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-structural substitution_
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-structural direct_demotion
```

Expected: all commands exit 0.

- [x] **Step 6: Verify the task-scoped strict lint gate and commit**

Run:

```bash
rustup run nightly-2026-02-27 cargo clippy -p rumoca-phase-structural -p rumoca-eval-solve --all-targets --all-features -- -D warnings
git diff --check
git status --short
git add crates/rumoca-phase-structural/src/eliminate/mod.rs \
  crates/rumoca-phase-structural/src/eliminate/boundary_scan.rs \
  crates/rumoca-phase-structural/src/dae_prepare/mod.rs \
  crates/rumoca-phase-structural/src/eliminate/direct_definition_index.rs \
  crates/rumoca-phase-structural/src/eliminate/runtime_protection.rs \
  crates/rumoca-phase-structural/src/eliminate/substitution_application.rs \
  crates/rumoca-eval-solve/src/prepared/dependency.rs \
  docs/superpowers/plans/2026-07-12-fix-ci-lockfile-merge.md
git commit -m "fix(ci): reconcile upstream merge artifacts"
```

Expected: task-scoped strict clippy exits 0 and the commit contains only the named files. The repository-wide lint gate remains the final cross-task gate after all merge-damaged crates are reconciled.

---

### Task 3: Reconcile DAE merge artifacts and restore the repository lint gate

**Files:**
- Modify: `crates/rumoca-phase-dae/src/dae_lowering.rs`
- Modify: `crates/rumoca-phase-dae/src/fold_start_values.rs`
- Modify: `crates/rumoca-phase-dae/src/lib.rs`
- Modify: `docs/superpowers/plans/2026-07-12-fix-ci-lockfile-merge.md`

**Interfaces:**
- Consumes: parent1 shape-aware DAE initialization at `84e6cfc0`, upstream DefId-first DAE lookup at `24209c80`, and their broken merge at `943dded7`.
- Produces: a coherent DefId-first array-parameter map with named actual metadata, restored start-value/package-constant folding, and initialized nonnumeric DAE metadata.

- [x] **Step 1: Verify the DAE-level RED**

Run:

```bash
rustup run nightly-2026-02-27 cargo check -p rumoca-phase-dae
```

Expected: FAIL with duplicate `ArrayParamMap`, missing folding/metadata helpers, and mixed call signatures.

- [x] **Step 2: Reconcile ArrayParamMap and block-constructor lowering**

In `dae_lowering.rs`, delete the old name-only type alias and retain one DefId-first struct with both indexes carrying named parameter metadata:

```rust
struct ArrayParamMap {
    by_def_id: HashMap<DefId, Vec<(usize, String)>>,
    by_name: HashMap<String, Vec<(usize, String)>>,
}
```

Build each value with `.map(|(index, parameter)| (index, parameter.name.clone()))`, insert it under both `func.def_id` and `func.name`, and return `Option<&Vec<(usize, String)>>` from `array_param_indices_for_call`. Restore from parent1 the complete `unwrap_block_constructor_value_wrappers` and `BlockConstructorValueUnwrapper` implementation; do not alter unrelated upstream lowering.

- [x] **Step 3: Restore shape-aware start and package-constant folding**

In `fold_start_values.rs`, restore parent1's fixed-point initialization mechanism:

```text
- collect DAE variable dimensions before evaluating start expressions
- seed Modelica standard constants
- collect declared start names after bindings are built
- call eval_start_const_expr(expr, values, dims)
- retain structured aliases unless their reference is a declared start value
```

Restore `fold_known_package_constants_to_literals` and its rewriter/helper set: `rewrite_variable_attributes`, `rewrite_expressions`, `rewrite_optional_expression`, `KnownPackageConstantRewriter`, its `StatementRewriter` implementation, `seed_modelica_standard_constants`, `MODELICA_STANDARD_CONSTANTS`, `modelica_standard_constant_value`, and `is_declared_start_reference`. Do not remove the imports or the `lib.rs` pass that consume these mechanisms.

- [x] **Step 4: Restore DAE metadata initialization**

In `lib.rs`, restore parent1's `initialize_dae_metadata` and `nonnumeric_variable_names` helpers. Preserve initialization of all prior metadata plus `metadata.nonnumeric_variable_names` for String values and external-constructor handles. Keep the existing calls to metadata initialization and package-constant folding.

- [ ] **Step 5: Verify focused GREEN**

Run:

```bash
rustup run nightly-2026-02-27 cargo check -p rumoca-phase-dae
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-dae --test dae_array_size_args dae_array_size_arg_uses_named_actual_value
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-dae prepare_dae_for_codegen_unwraps_block_constructor_value_wrapper
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-dae fold_start_values_keeps_structured_alias_with_rewritten_display_name
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-dae folds_parameter_start_size_from_dae_variable_dims
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-dae fold_known_package_constants_rewrites_equation_rhs
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-dae test_todae_keeps_external_object_constructor_out_of_fx
```

Expected: all commands exit 0.

- [ ] **Step 6: Restore repository lint and commit**

Run:

```bash
rustup run nightly-2026-02-27 cargo xtask verify lint
git diff --check
git status --short
git add crates/rumoca-phase-dae/src/dae_lowering.rs \
  crates/rumoca-phase-dae/src/fold_start_values.rs \
  crates/rumoca-phase-dae/src/lib.rs \
  docs/superpowers/plans/2026-07-12-fix-ci-lockfile-merge.md
git commit -m "fix(ci): reconcile DAE merge artifacts"
```

Expected: repository lint exits 0 and the commit contains only the named files.

---

### Task 4: Reconcile phase-solve merge artifacts

**Files:**
- Modify: `crates/rumoca-phase-solve/src/lib.rs`
- Modify: `crates/rumoca-phase-solve/src/solve_model.rs`
- Modify: `crates/rumoca-phase-solve/src/lower/array_values/inference.rs`
- Test: focused tests colocated with the three files above
- Modify: `docs/superpowers/plans/2026-07-12-fix-ci-lockfile-merge.md`

**Interfaces:**
- Consumes: parent1 residual-target/runtime-table/record-array mechanisms at `84e6cfc0` and upstream parameter-filtered runtime tables at `24209c80`.
- Produces: one initialization helper, deduplicated continuous targets, stable union of DAE and referenced runtime tables, and complete record-array dimension inference.

- [x] **Step 1: Verify phase-solve RED**

```bash
rustup run nightly-2026-02-27 cargo check -p rumoca-phase-solve
```

Expected: FAIL on duplicate `lower_initialization_updates_only`, missing table merge/helper symbols, missing record-array index helpers, and strict-lint fallout.

- [x] **Step 2: Repair `lib.rs` merge hunks**

Keep exactly one `lower_initialization_updates_only`. Restore `dedupe_continuous_y_targets(&mut residual_targets)` immediately after residual-target generation in the rows-and-targets path; do not remove the mutable collection or import. Preserve the first matching Y slot for duplicate targets.

- [x] **Step 3: Repair runtime external-table composition**

In `solve_model.rs`, build `merged_external_tables` from the explicit DAE tables returned by `external_table_data_for_dae(&dae_model)?` plus only runtime tables referenced by parameters from `external_table_data_for_parameter_values_in(env, parameters)`. Deduplicate by table identity and produce stable ordering. Do not restore the old all-environment scan.

- [x] **Step 4: Restore record-array inference helpers**

In `lower/array_values/inference.rs`, restore parent1 `record_array_prefix_index` and `static_positive_subscript_index`, preserving the existing `infer_record_array_aggregate_dims` call. Static positive subscripts contribute an aggregate extent; dynamic, zero, and negative subscripts must retain the existing error behavior.

- [x] **Step 5: Verify focused GREEN and strict crate lint**

```bash
rustup run nightly-2026-02-27 cargo check -p rumoca-phase-solve
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-solve lower_initialization_updates_only
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-solve continuous_row_targets
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-solve external_table_and_random_tests
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-solve lower_expression_binds_singleton_vectorized_record_array_field_to_vector_input
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-solve rejects_non_singleton_vectorized_record_array_field
rustup run nightly-2026-02-27 cargo clippy -p rumoca-phase-solve --all-targets --all-features -- -D warnings
```

Expected: all commands exit 0.

- [x] **Step 6: Commit**

```bash
git diff --check
git add crates/rumoca-phase-solve/src/lib.rs \
  crates/rumoca-phase-solve/src/solve_model.rs \
  crates/rumoca-phase-solve/src/lower/array_values/inference.rs \
  docs/superpowers/plans/2026-07-12-fix-ci-lockfile-merge.md
git commit -m "fix(ci): reconcile solve merge artifacts"
```

---

### Task 5: Restore the codegen JSON filter registration

**Files:**
- Modify: `crates/rumoca-phase-codegen/src/codegen/mod.rs`
- Test: existing `codegen_tests::test_json_filter`

**Interfaces:**
- Consumes: parent1 `json_filter` implementation and its existing test.
- Produces: a Minijinja environment where the `json` filter is registered by `add_basic_filters`.

- [ ] **Step 1: Verify RED**

```bash
rustup run nightly-2026-02-27 cargo clippy -p rumoca-phase-codegen --all-targets --all-features -- -D warnings
```

Expected: FAIL because `json_filter` is unused.

- [ ] **Step 2: Register the existing filter**

Add exactly this registration next to the other basic filters, after `last_segment`:

```rust
env.add_filter("json", json_filter);
```

Do not delete the filter or its test.

- [ ] **Step 3: Verify and commit**

```bash
rustup run nightly-2026-02-27 cargo test -p rumoca-phase-codegen codegen_tests::test_json_filter
rustup run nightly-2026-02-27 cargo clippy -p rumoca-phase-codegen --all-targets --all-features -- -D warnings
git diff --check
git add crates/rumoca-phase-codegen/src/codegen/mod.rs
git commit -m "fix(ci): register codegen JSON filter"
```

Expected: both commands exit 0 and only the codegen file is committed.

---

### Task 6: Remove orphan test merge residue and run final cross-task gates

**Files:**
- Modify: `crates/rumoca-phase-dae/src/runtime_precompute/tests/mod.rs`
- Modify: `docs/superpowers/plans/2026-07-12-fix-ci-lockfile-merge.md`

**Interfaces:**
- Consumes: upstream removal of the static indexed-resolution-table test at `24209c80`.
- Produces: no orphan `int_lit` helper and a strict phase-dae test build.

- [ ] **Step 1: Verify RED**

```bash
rustup run nightly-2026-02-27 cargo clippy -p rumoca-phase-dae --all-targets --all-features -- -D warnings
```

Expected: FAIL because `runtime_precompute/tests/mod.rs::int_lit` is unused.

- [ ] **Step 2: Remove only the orphan helper**

Delete `int_lit` and no other test code. Do not add a fake call; the only parent1 test that used it was removed upstream.

- [ ] **Step 3: Re-run Task 3 exact focused gates**

Run the six exact phase-dae focused commands from Task 3 Step 5 without warning suppression. Expected: all exit 0.

- [ ] **Step 4: Run final repository gates**

```bash
rustup run nightly-2026-02-27 cargo xtask verify lint
rustup run nightly-2026-02-27 cargo check --locked --package xtask --quiet
git diff --check
```

Expected: all commands exit 0 with the regenerated lockfile and all reconciled source mechanisms.

- [ ] **Step 5: Commit**

```bash
git add crates/rumoca-phase-dae/src/runtime_precompute/tests/mod.rs \
  docs/superpowers/plans/2026-07-12-fix-ci-lockfile-merge.md
git commit -m "fix(ci): remove orphan DAE test helper"
```

Expected: only the orphan helper and plan completion state are committed.
