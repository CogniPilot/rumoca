# Rumoca PR Review Template

<!--
Mirrors SPEC_0025. Section names here must match SPEC_0025 §"PR Template
Alignment". Update both files together if you change either one.
-->

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

## Testing

- Key command(s) run (fmt, clippy, workspace test, doc):
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
- [ ] Standard CI gates pass (`fmt`, `clippy -D warnings`, `cargo test`, `cargo doc`).
- [ ] MSL gate run for compiler/simulator changes; no regression vs baseline.
- [ ] Size-budget section completed.
- [ ] Positive net diff has explicit compression justification.
- [ ] New APIs are required and minimal.
- [ ] Old/new parallel paths removed unless explicitly migrating.
- [ ] No `#[allow(clippy::...)]` added outside generated code.
- [ ] Every commit signed off (`git commit -s`); no `Co-Authored-By` for AI.
- [ ] External material (if any) attributed and Apache-2.0 compatible.

## Authorized Broken-Main Recovery (optional)

<!-- Leave blank for normal PRs. Use only for an Explicitly authorized ClimaMind Rumoca broken-main recovery batch. -->

- `authorization_ref` (durable validation integration PR body or maintainer-controlled GitHub artifact):
- `authorized_by` (ClimaMind Rumoca repository maintainer):
- `batch_id`:
- Authorized ordered `owner_prs`:
- `target_branch` / baseline `head_sha`:
- RFC 3339 UTC `expires_at`:
- Owner PR / final `head_sha`:
- Independent technical review / reviewed `head_sha`:
- Owner mechanism test / tested `head_sha`:
- Recovery-rule PR / final `head_sha`:
- Integration PR / `head_sha`:
- Hosted CI workflow / `head_sha`:
- [ ] Authorization exists, matches this merge, and is unexpired.
- [ ] `authorized_by` is a ClimaMind Rumoca maintainer; this task's explicit authorization is sufficient, with no additional maintainer or approval.
- [ ] Recovery is inactive and fails closed if `expires_at` passed, every authorized owner PR landed, or target `main` has all required CI green.
- [ ] Evidence is bound to the owner final head and recorded in order; merge only after all required hosted CI is green on the integration head.
- [ ] Evidence order is recorded without skipping: authorization verification; independent technical review on final owner `head_sha`; passing owner mechanism test on that same `head_sha`; exact-head integration; hosted CI green on the integration head; then merge. No later step occurs before its predecessor.
- [ ] Every listed final owner head, including the recovery-rule PR head, is a Git ancestor of the integration head; no cherry-pick, patch-id, squash, or content-equivalent substitute.
- [ ] Recorded target baseline `head_sha` is a Git ancestor of the integration head.
- [ ] Integration history = target baseline + listed exact owner histories + signed merge commits only; no integration-only production, test, spec, workflow, baseline, validator, tolerance, fixture, or content commit.
- [ ] Every integration merge commit has exactly one `Signed-off-by` trailer and no `Co-Authored-By` trailer.
- [ ] CI workflow head = integration head.
- [ ] Any owner, baseline, or integration head change fails closed; rebuild and rerun affected evidence and CI.
- [ ] Draft, validation-only, never merge.
