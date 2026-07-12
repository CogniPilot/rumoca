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

- [ ] **Step 6: Restore the repository lint gate and commit**

Run:

```bash
rustup run nightly-2026-02-27 cargo xtask verify lint
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

Expected: lint exits 0 and the commit contains only the named files.
