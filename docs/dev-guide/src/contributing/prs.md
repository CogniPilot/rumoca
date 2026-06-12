# Pull Requests

The PR process — required verification, metrics, MSL evidence, and done
criteria — is normative in
[SPEC_0025](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0025_PR_REVIEW_PROCESS.md).
Read it before opening your first PR. The short practical version:

## Before Opening

- `cargo xtask verify quick` passes locally (use `verify full` when your
  change can affect MSL behavior).
- The change follows the owning spec; if it required bending one, the spec
  change is part of the discussion, not an afterthought.
- Diagnostics added or changed carry spans and phase-local error codes.
- Tests assert specific expected errors, not just failure.

## Commit Hygiene

- Commits are signed off (`git commit -s`) — the DCO sign-off is required.
- Write commit messages about *why*, not just *what*.

## Review Flow

PRs run the full CI matrix: lint, workspace tests, coverage gate, MSL
quality gate, docs build, VS Code extension gate, and the WASM gate. The
MSL gate posts a comparison comment on the PR so reviewers see
compile/simulate/parity movement at a glance.

Upstream-first: when a bug traces to an earlier phase, fix it there rather
than compensating downstream — reviewers will ask for the root-cause fix
(see [Diagnostics and Spans](../compiler/diagnostics.md) for the same
principle applied to error handling).

## Issues and Discussion

Bugs and feature requests: <https://github.com/CogniPilot/rumoca/issues>.
A minimal `.mo` reproduction (the
[playground](https://cognipilot.github.io/rumoca/) makes these easy to
verify) turns a vague report into a fixable one.
