# rumoca-tool-dev

Developer workflow CLI (`rum`) and auxiliary workspace tooling.

## Role in Rumoca
Provides developer automation for tests, coverage, MSL workflows, duplicate detection, and editor/dev environment support.

## Public Surface
- Developer binary: `rum`.
- Developer utility binary: `rumoca-msl-tools`.
- Public command surfaces include workspace test orchestration, coverage reporting, and MSL compare/plot workflows.

## Inputs
- Developer commands, workspace paths, and existing build/test artifacts.

## Outputs
- Workflow status, reports, and helper artifacts for local development and CI.

## Design Constraints
- Keep this crate orchestration-focused, not compiler-semantic.
- Favor deterministic command behavior for CI reproducibility.
- Reuse underlying crates instead of duplicating logic.
