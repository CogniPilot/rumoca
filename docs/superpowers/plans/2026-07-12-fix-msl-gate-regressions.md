# MSL Gate Closeout Plan

**Status:** active. Completed repairs are recorded in the
[completed MSL repair record](../completed/2026-07-12-fix-msl-gate-regressions.md)
and are not active problems.

**Scope:** run the final gates for the completed semantic repair branch. Do not weaken baselines, floors, exclusions, selected targets, timeouts, worker budgets, parity requirements, or the pinned OpenModelica version. This plan does not authorize a push.

## Current evidence

The Stack-family Flat, ToDae, and structural mechanisms are repaired and independently reviewed. At clean semantic commit `9e8f2e09a31e4e8f8834f121db3825ec883318c3`, structural admission passes for all four focused controls: `CCCV_Stack` `134/134`, `CCCV_StackRC` `176/176`, `CCCV_Cell` `19/19`, and `CCCV_CellRC` `20/20`. There is no remaining focused Stack/StackRC semantic deficit in this plan.

The full MSL gate at the same commit completed but did not close the promoted quality ratchet. The current artifact is `target/msl/results/msl_quality_current.json`; its resolved promoted baseline is `target/msl/baselines/msl_quality_baseline.json`. DAE/compiled (`556` vs `545`), Solve (`407` vs `381`), and balanced (`543` vs `532`) improved, while Flat (`564` vs `565`), IC success (`206` vs `239`), simulation success (`167` vs `170`), compared traces (`159` vs `166`), and exact state-set matches (`138` vs `152`) remain below the baseline. The full MSL task therefore remains active.

## Remaining Task: Run final branch gates

1. Run the repository Rust gates required by `CONTRIBUTING.md` and `SPEC_0025_PR_REVIEW_PROCESS.md`, including formatting, strict lint, workspace tests, and documentation checks.
2. Run the full non-partial MSL quality gate with the exact pinned OpenModelica package and required runtime/trace parity. Meet every checked-in floor without changing the quality baseline.
3. Run architecture synchronization and a final independent review across the complete branch diff. Resolve findings, rerun affected gates, verify DCO trailers, and leave a clean worktree.
4. Record final artifact paths, commit SHAs, commands, and any externally blocked verification. Do not push unless the user separately authorizes it.
