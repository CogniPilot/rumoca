# Fix CI Lockfile Merge Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Restore a valid, deterministic Cargo dependency graph after the upstream merge so CI can parse `Cargo.lock` on the repository-pinned Rust toolchain.

**Architecture:** Treat the malformed lockfile as a merge artifact, not as source-of-truth dependency intent. Re-resolve the merged workspace manifests with the pinned `nightly-2026-02-27` Cargo, then verify both locked parsing and the repository lint gate without changing application code or weakening CI.

**Tech Stack:** Rust, Cargo lockfile v4, `cargo xtask`, GitHub Actions.

## Global Constraints

- Work only in `/Users/hechuan/dev-home/worktrees/rumoca/fix-ci-lockfile-merge` on branch `fix/ci-lockfile-merge`.
- Keep the fix limited to `Cargo.lock` plus this implementation-plan artifact; do not alter manifests, CI workflows, application code, tests, or quality baselines.
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
