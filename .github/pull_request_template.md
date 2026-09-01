# Rumoca PR Review Template

<!--
Mirrors SPEC_0025. Section names here must match SPEC_0025 §"PR Template
Alignment". Update both files together if you change either one.
-->

## Branch Naming

- Descriptive branch name without an `agent/` prefix:

## Summary

- What user-facing behavior changes?
- What issue, spec, or design rule does this address?

## Spec / MLS Alignment

- Relevant active spec(s) checked:
- Relevant MLS section(s), if semantics changed:
- Crate/phase owner:

## Risk and Design Notes

- Main correctness risk:
- Main maintenance risk:
- Why the change belongs in these crate(s):
- Any new abstraction, public API, or migration path:

## Proof Packet

- spec_mls_anchors:
- construction_invariant:
- construction_authority:
- concrete_reproduction:
- first_divergence:
- rejected_hypotheses:
- producer_artifact_delta:
- dependency_predecessors:
- keystone_files_and_types:
- reservation_window:
- reservation_release_or_checkpoint:
- positive_witness:
- negative_witness:
- mutation_witness:
- claim_status: <!-- VERIFIED, INFERRED, or RELAYED-UNVERIFIED -->
- command_results_with_exit_status:
- review_verdict:
- reviewed_revision:
- commands_not_run:

## Testing

- Focused command(s) run with exit status:
- `cargo xtask verify quick` result:
- `cargo xtask verify full` result on the same frozen revision:
- Behavior or regression covered:
- Commands NOT run and why:
- For compiler/simulator changes: did you run the MSL gate
  (`cargo test --release --package rumoca-test-msl --features msl-full-test --test msl_tests
  balance_pipeline::balance_pipeline_core::test_msl_all -- --nocapture`) and
  confirm no regression vs the resolved `msl_quality_baseline.json`?

## Code Size Budget (required)

- production_lines_added:
- production_lines_deleted:
- test_lines_added:
- test_lines_deleted:
- public_items_added:
- public_items_removed:
- files_touched:
- net_added_lines:

If `net_added_lines` is positive, add:

- Why this net growth is required.
- Which code was removed/merged as part of the first compression pass.
- Follow-up cleanup ticket/commit for remaining growth (if any).

## Reviewer Checklist

- [ ] Relevant active specs were checked.
- [ ] MLS-sensitive changes cite the right MLS section.
- [ ] Crate boundaries and phase ownership preserved (SPEC_0029).
- [ ] Tests prove behavior or explain the remaining gap.
- [ ] Proof packet binds green focused gates and a fresh exact-byte review.
- [ ] Every overlapping keystone reservation was checkpointed or released.
- [ ] `cargo xtask verify quick` and then `cargo xtask verify full` pass on the same frozen revision.
- [ ] MSL gate run for compiler/simulator changes; no regression vs baseline.
- [ ] Size-budget section completed.
- [ ] Positive net diff has explicit compression justification.
- [ ] New APIs are required and minimal.
- [ ] Old/new parallel paths removed unless explicitly migrating.
- [ ] No hand-written `#[allow(clippy::...)]`; any generated-parser exception has explicit generator provenance.
- [ ] Every commit signed off (`git commit -s`); no named AI assistant/session references or AI `Co-Authored-By` trailers.
- [ ] External material (if any) attributed and Apache-2.0 compatible.
