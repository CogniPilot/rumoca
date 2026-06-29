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
