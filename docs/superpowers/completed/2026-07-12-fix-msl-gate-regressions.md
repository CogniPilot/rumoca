# MSL Gate Repairs — Completed Work

**Status:** the items below are complete. This is a concise provenance record, not an active problem list.

## Original semantic repairs

- connector instance identity: `94f6033a`;
- near-future root tolerance: `bfd070fe`;
- periodic Clock alias ticks with builtin identity proof: `8fbc5035`, `6ff3c7cf`;
- algebraic refresh alternatives and complete projection validation: `8d402084`, `66ba724d`;
- record-constructor projections: `30a19e77`;
- scheduled-root invalidation: `d9894cb5`;
- projected result cardinality and call dependencies: `8e709eed`, `176e5fba`.

## Architecture and follow-up repairs

- SPEC_0021 runtime-test split: `8fb3e5ae`;
- review-scan parity with file-size exceptions: `6cde10b7`;
- sequential instance algorithm bindings: `ebb75ad4`;
- shaped discrete enum-array starts: `678a15a2`;
- reference-aware indexed aggregate identity: `7c0355dd`.

## Gate finalization repairs

- fail fast when a warm OpenModelica process exits: `974ae96d`;
- install before verification and pin exact OpenModelica `1.27.0-1`: `b11f0615`;
- require full-run runtime/trace parity and close the selected-target bypass: `82fc87ae`, `438a3947`;
- align selected-target fixtures and isolate full-gate defaults: `427b204e`, `9cd395d9`;
- settle symbolic dimensions without redundant evaluation while preserving every dependency-driven retry: `77ad16a0`, `c30ffec5`, `8e2c6f56`, `e72e724b`, `cf1c0af6`.
- ModelicaTest semantic gate at `9e8f2e09`: selected target `ModelicaTest.Blocks.MuxDemux` completed successfully.

The symbolic-dimension implementation passed independent review with no findings. At clean commit `cf1c0af67d00108e59329b16def8fa8044bb1e1a`, 540 `rumoca-phase-flatten` library tests, strict crate clippy, formatting, and diff checks passed. The clean focused artifact `/tmp/rumoca-task15-dimension-cf1/` records 4/4 Flat success with no timeout or budget increase.

## Stack-family structural repair

The former Stack/StackRC deficit was closed at its producing mechanisms rather than compensated downstream:

- Flat preserves multidimensional member bindings and indexed nested-component boundaries/provenance: `aec66b13`, `bd7b9541`, `22e16326`, `eec5fbdb`, `abed9d04`;
- inherited references resolve through explicit record aliases and fail closed when instance ownership is not proven; the rejected path-prefix heuristic is not present: `5fb3a38e`, `ddff7f88`;
- ToDae canonicalizes and validates expandable-record projection identity before admitting it: `1071d894`, `a31e3be4`, `3711b4c1`;
- structural elimination preserves runtime references, retires invalid boundary targets, rewrites proven indexed substitutions, and rejects ambiguous or symbolic target identity: `c558d2a6`, `dc8fcb4f`, `d5bf4868`, `bdc53572`, `4b92974d`, `9e8f2e09`.

The complete mechanism chain passed independent review. At clean semantic commit `9e8f2e09a31e4e8f8834f121db3825ec883318c3`, focused structural admission is balanced for all controls: `CCCV_Stack` `134/134`, `CCCV_StackRC` `176/176`, `CCCV_Cell` `19/19`, and `CCCV_CellRC` `20/20`.

## Finalization and handoff

- `f84f3cb3` split oversized regression modules without changing behavior;
- `3ba4b26d` gave the Clock fixture compiler-owned builtin `DefId` identity;
- `21ccab5d` extracted the structural runtime-partition test helper;
- `6a5be4b9` replaced hidden fixture sentinels with deterministic spans and an explicit fixed marker.

The complete documentation gate, formatting, strict workspace all-target/all-feature clippy, `cargo test --workspace`, and review scan all passed; review scan reported zero forbidden findings. DCO and clean-worktree checks passed. The final independent review's only finding was stale completed work in the active plan, which this closeout removes.

## Full promoted-ratchet closure

The remaining full-gate regressions were closed at their owning mechanisms:

- `046a2691` stops cache-only symbolic-dimension repairs from restarting the Flat fixpoint; `HeatingSystem` now reaches ToDae in 2.74 seconds instead of exhausting the unchanged 45-second Flat budget;
- `b35191ce` preserves a surviving physical producer when derivative-dependent aggregate leaves are structurally eliminated;
- `63c1d7ee` assigns residual ownership from complete producer incidence and keeps coupled rows simultaneous;
- `e7d8de30` projects coupled algebraics from the accepted local branch;
- `58be4144` refreshes direct and coupled algebraic observations, consumes root relations atomically, and performs branch-preserving consistency polish before publishing reconstructed connection flows;
- `6c546777` preserves accepted derivative/root/observation seeds across BDF restarts, clamps scheduled left limits, retains the triggering root index, and defers deadline roots consistently in session mode.

The unchanged `cargo xtask verify msl-parity` gate passed over all 566 MSL 4.1.0 root examples. The final snapshot records Parse `566`, Flat `565`, DAE `556`, Solve `406`, balanced `543`, simulation success `182`, compared traces `174`, no-severe traces `151`, and exact state-set matches `158`. The resolved promoted baseline was not changed; no timeout, selected target, trace exclusion, tolerance, or parity requirement was relaxed.

No branch push was performed because the user did not authorize one. This is handoff state, not an active problem.

Detailed implementation scripts, stale worktree paths, obsolete pre-fix counts, and already completed checklists were removed. Git history and the cited clean artifacts are the authoritative audit trail.
