# Rumoca PR Review Template

## Summary

- What user-facing behavior changes?

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

## Design Notes

- Why each new abstraction was necessary.
- Why this was not solved by editing an existing module directly.
- What duplicate/legacy paths were deleted.

## Testing

- Key command(s) run.
- Files changed covered by tests.

## Reviewer Checklist

- [ ] Size budget section completed.
- [ ] Positive net diff has explicit compression justification.
- [ ] New APIs are required and minimal.
- [ ] Old/new parallel paths were removed unless explicitly migrating.
