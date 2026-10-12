# Definedness across function compound writers

The unchanged Booster's `loadedFootSupportMargin` writes `margin` in every
source branch. Before this repair its all-four-loaded branch nevertheless
fails a generated uninitialized-value assertion; the original native session
first fails at displayed time 0.040795571. The temporary provenance probe
identifies FunctionId 8, declaration span 9545..11706, and assertion condition
1823 with a false fallback literal 1822. Its source and log bindings are in the
adjacent JSON. No temporary probe belongs to the production chain.

The first differing producer is
`function_body/path_definedness.rs::DefinednessPredicates::partial_targets`.
Function analysis withdraws path-partial admission after a compound writer;
construction incorrectly treated that withdrawal as loss of an already issued
SSA definedness predicate. The repair carries that existing predicate on
unwritten join paths. Writing paths still establish definedness, the required
predicate lookup for `was_partial` still refuses missing proof, and the original
checked assertion still rejects unassigned paths. Source assignments and folds
cannot undo an existing definition. This preserves MLS §12.4.4's use/return
rule at the construction owner (SPEC_0007 and SPEC_0033), without changing
analysis admission, checkers, wire format, execution profiles or source models.

| Frozen checkpoint | Actual qualification |
|---|---|
| Tests-only `13b9e21aa126a879f7a69a3bb4c13b843ee78eb0` | Both permanent controls fail on paths that assign their returned value |
| Semantic replay `fc3caa6ef8683637804372edfb94d0972d9b7812` | Both controls pass under Interpreter/Auto, including all six original margin branches and the independent unassigned-path fault |
| Same semantic replay | 41 existing function controls and six branch-snapshot controls pass; both scoped Clippy commands and workspace formatting pass |
| Timestamp-only `3099104b716f6ea8a3403248208f88c22b7cd3ee` | Original Booster native prefix passes all eight requested samples from 0 to 0.35; 612 finite observables and Cranelift/no-refusal/no-declines at every sample; scoped lint and formatting pass |

The first repaired prefix run had an exact-bit test failure: actual 0.15 versus
requested `3 * 0.05` (0.15000000000000002). The session's
`resolve_without_advance` explicitly accepts coincident yield coordinates under
`accepted_step_roundoff`; it retains the accepted coordinate. The test now
imports that exact public owner bound and also requires finite, strictly
increasing times at every unchanged request. Its model, input routes, dt,
solver tolerances, horizon and native receipt assertions are unchanged.

The existing Root warm-target profile used 600 seconds, 8 GiB aggregate
resident-plus-swap, 16 GiB host reserve, 8 GiB scratch reserve and 4/4/4 workers.
It has no cache-size ceiling; the returned target allocated 17,344,245,760 bytes.
One nextest invocation refused duplicate argument aliases before any build or
test. One timestamp-prefix build stopped at the host-memory floor before test
execution. Both are retained separately; the serialized, unchanged-guard build
continuation completed successfully. No budget was enlarged. Every frozen
attempt's 4,125 tracked source entries match before/after; raw logs, guardians,
commands, source manifests and three sealed test ELFs are bound by the JSON.

These results qualify the function producer and the original native startup
prefix. The authored browser mission, final combined quick/full runs and remote
CI remain separate qualifications.
