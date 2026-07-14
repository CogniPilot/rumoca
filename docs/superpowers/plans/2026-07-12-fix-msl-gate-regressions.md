# MSL Gate Closeout Plan

**Status:** active only for the full MSL promoted quality ratchet. Completed repairs and finalization gates are recorded in the
[completed MSL repair record](../completed/2026-07-12-fix-msl-gate-regressions.md)
and are not active problems.

**Scope:** close the full 566-model MSL promoted quality ratchet. Do not weaken baselines, floors, exclusions, selected targets, timeouts, worker budgets, parity requirements, or the pinned OpenModelica version.

## Current evidence

The full MSL gate at `9e8f2e09a31e4e8f8834f121db3825ec883318c3` completed but did not close the promoted quality ratchet. The current artifact is `target/msl/results/msl_quality_current.json`; its resolved promoted baseline is `target/msl/baselines/msl_quality_baseline.json`. DAE/compiled (`556` vs `545`), Solve (`407` vs `381`), and balanced (`543` vs `532`) improved, while Flat (`564` vs `565`), IC success (`206` vs `239`), simulation success (`167` vs `170`), compared traces (`159` vs `166`), and exact state-set matches (`138` vs `152`) remain below the baseline.

## Remaining task

Trace the remaining Flat, IC, simulation, trace-comparison, and state-selection regressions to their first owning mechanisms, add focused regressions, and rerun the unchanged full 566-model gate until every promoted floor closes. A baseline change, exclusion, relaxed parity requirement, or focused/partial promotion is not completion.
