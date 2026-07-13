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

The symbolic-dimension implementation passed its fifth independent review with no findings. At clean commit `cf1c0af67d00108e59329b16def8fa8044bb1e1a`, 540 `rumoca-phase-flatten` library tests, strict crate clippy, formatting, and diff checks passed. The clean focused artifact `/tmp/rumoca-task15-dimension-cf1/` records 4/4 Flat success with no timeout or budget increase; the separate Stack/StackRC ToDae deficits remain in the active closeout plan.

Detailed implementation scripts, stale worktree paths, obsolete pre-fix counts, and already completed checklists were removed. Git history and the cited clean artifacts are the authoritative audit trail.
