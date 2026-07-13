# MSL Gate Closeout Plan

**Status:** active. Completed repairs are recorded in the
[completed MSL repair record](../completed/2026-07-12-fix-msl-gate-regressions.md)
and are not active problems.

**Scope:** close the remaining shared semantic deficit and run the final gates. Do not weaken baselines, floors, exclusions, selected targets, timeouts, worker budgets, parity requirements, or the pinned OpenModelica version. This plan does not authorize a push.

## Current evidence

The clean `cf1c0af67d00108e59329b16def8fa8044bb1e1a` artifact at `/tmp/rumoca-task15-dimension-cf1/` records that the symbolic-dimension repair is complete and independently approved:

- `CCCV_Cell`: Flat `0.187 s`, Success, balanced `131/131`;
- `CCCV_CellRC`: Flat `0.207 s`, Success, balanced `148/148`;
- `CCCV_Stack`: Flat `0.646 s`, then ToDae balance deficit `-90`;
- `CCCV_StackRC`: Flat `0.785 s`, then ToDae balance deficit `-120`.

Both `msl_results.json` and `msl_parity_timing.json` identify the same clean commit. The command exited 1 only because the two Stack models expose a distinct downstream ToDae deficit.

## Remaining Task 1: Repair the Stack/StackRC ToDae balance deficit

1. Preserve the two exact Stack failures as the focused RED and inspect the first semantic loss between Flat IR and DAE construction. Treat `-90` and `-120` as evidence, not as values to compensate.
2. Add a deterministic mechanism-level regression at the owning phase before changing production code. Preserve structured identity, dimensions, connector semantics, and equation ownership; do not add model-specific branches or generated-artifact patches.
3. Run the owning crate tests and rerun `CCCV_Stack`, `CCCV_StackRC`, `CCCV_Cell`, and `CCCV_CellRC` under the unchanged budget. Require both Stack models to cross the former ToDae boundary without regressing the two Cell controls.
4. Commit with DCO signoff and obtain an independent task review.

## Remaining Task 2: Run final branch gates

1. Run the repository Rust gates required by `CONTRIBUTING.md` and `SPEC_0025_PR_REVIEW_PROCESS.md`, including formatting, strict lint, workspace tests, and documentation checks.
2. Run the full non-partial MSL quality gate with the exact pinned OpenModelica package and required runtime/trace parity. Meet every checked-in floor without changing the quality baseline.
3. Run the separate ModelicaTest semantic gate using the repository/CI truth. Do not conflate it with the curated MSL example set.
4. Run architecture synchronization and a final independent review across the complete branch diff. Resolve findings, rerun affected gates, verify DCO trailers, and leave a clean worktree.
5. Record final artifact paths, commit SHAs, commands, and any externally blocked verification. Do not push unless the user separately authorizes it.
