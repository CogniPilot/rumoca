# SPEC_0030: Coverage Trim and Gate Process

## Status
ACTIVE

## Summary
Defines the required workflow for workspace coverage generation, trim-candidate analysis, regression gating, and explicit baseline promotion.

## Scope
Applies to:
- `rum coverage*` tooling
- coverage-trim triage/deletion changes
- `crates/rumoca-tool-dev/coverage/trim-gate-baseline.json`

## Required Workflow

Generate workspace coverage artifacts:

```bash
cargo run --bin rum -- coverage
```

Optionally include ignored/slow suites in the same dataset:

```bash
cargo run --bin rum -- coverage --include-ignored
```

Generate trim report and candidate inventory:

```bash
cargo run --bin rum -- coverage-report
```

Run regression gate:

```bash
cargo run --bin rum -- coverage-gate
```

## Artifacts

Generated under `target/llvm-cov/`:
- `workspace-full.json`
- `workspace-summary.json`
- `coverage-trim-report.md`
- `trim-candidates.json`
- `coverage-gate.md`

Committed baseline:
- `crates/rumoca-tool-dev/coverage/trim-gate-baseline.json`

## Triage Labels

`trim-candidates.json` emits both `triage_label` and `owner_decision`.

Current labels:
- `dead_likely` -> `delete_candidate`
- `rare_path_keep` -> `keep_document_rare_path`
- `single_use_helper_keep` -> `keep_single_use_helper`
- `needs_targeted_test` -> `keep_add_targeted_test`
- `public_api_review` -> `keep_public_api_review`

Mandatory rule:
- Do not auto-delete single-use private helpers. They are often intentional to limit nesting and keep code readable.

## Deletion Decision Checklist

Before deleting any candidate:
1. Verify coverage evidence (zero/near-zero execution).
2. Verify usage evidence (`callsites_same_file`, `callsites_workspace`, `callsites_other_crates`, visibility).
3. Verify runtime-path safety (not required for valid rare paths/cfg-gated behavior).
4. Record decision: delete, keep+tests, or keep+document.

## Baseline Promotion Policy

Promotion is explicit only:

```bash
cargo run --bin rum -- coverage-gate --promote-baseline
```

Requirements:
- no implicit baseline updates
- PR summary includes metric deltas
- no unjustified growth in candidate/dead-likely metrics

## Failure Triage and Rollback

When gate fails:
1. Inspect `target/llvm-cov/coverage-gate.md`.
2. Identify failing package(s)/metric(s).
3. Resolve by deletion, targeted tests, or explicitly approved baseline promotion.

If a promotion is incorrect:
1. Revert `crates/rumoca-tool-dev/coverage/trim-gate-baseline.json`.
2. Re-run `coverage`, `coverage-report`, and `coverage-gate`.
3. Confirm pass against restored baseline or re-triage with corrected rationale.
