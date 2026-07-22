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

- `authorization_url` (maintainer-controlled artifact; required fixed field):
- `authorization_ref` (authoritative record outside owner PR branches):
  the `authorization_url` above; it was published by an independent maintainer, not self-attested by an owner-PR author.
- `authorized_by` (ClimaMind Rumoca repository maintainer):
  must be an independent maintainer, not an owner-PR author.
- `batch_id`:
- Authorized ordered `owner_prs`:
- `target_branch` / baseline `head_sha`:
- RFC 3339 UTC `expires_at`:
- Owner PR / final `head_sha`:
- Independent technical review / reviewed `head_sha`:
- Owner mechanism test / tested `head_sha`:
- Integration PR / `head_sha`:
- Hosted CI workflow / `head_sha`:
- [ ] Authorization exists, matches this merge, and is unexpired.
- [ ] Authorization was published by the listed independent maintainer (not self-attested by an owner-PR author) and matches this final owner head.
- [ ] Evidence is bound to the owner final head and recorded in order; merge only after all required hosted CI is green on the integration head.
- [ ] Evidence order is recorded without skipping: authorization verification; independent technical review on final owner `head_sha`; passing owner mechanism test on that same `head_sha`; exact-head integration; hosted CI green on the integration head; then merge. No later step occurs before its predecessor.
- [ ] Integration input = target baseline + listed pending owner heads only; CI workflow head = integration head.
- [ ] Any owner, baseline, or integration head change fails closed; rebuild and rerun affected evidence and CI.
- [ ] Draft, validation-only, never merge; no unique fixes.
