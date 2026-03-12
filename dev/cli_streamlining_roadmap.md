# CLI Streamlining Roadmap

## Goal

Make `rum` obvious to navigate by using one canonical command path per workflow and naming commands after what the user is actually working on.

## Final command shape

- `rum verify ...`
- `rum vscode ...`
- `rum wasm ...`
- `rum python ...`
- `rum coverage ...`
- `rum repo ...`

## Why this layout

- `verify` is for repo-wide gates and CI-facing checks.
- `vscode` and `wasm` are target-first because that is how developers think about those workflows.
- `python` is a build/install workflow, not repo maintenance.
- `coverage` is a cohesive workflow with `run`, `report`, and `gate`.
- `repo` hides maintainer-only tasks, including MSL parity corpus maintenance, that should not dominate top-level help.

## Canonical commands

### `rum verify`

- `rum verify lint`
- `rum verify workspace`
- `rum verify quick`
- `rum verify docs`
- `rum verify binaries`
- `rum verify msl-parity`

### `rum vscode`

- `rum vscode build`
- `rum vscode package --target linux-x64|linux-arm64`
- `rum vscode test`
- `rum vscode edit`

### `rum wasm`

- `rum wasm build`
- `rum wasm test`
- `rum wasm edit`
- `rum wasm clean`

### `rum python`

- `rum python build`

### `rum coverage`

- `rum coverage run`
- `rum coverage report`
- `rum coverage gate`

### `rum repo`

- `rum repo cli install`
- `rum repo hooks install`
- `rum repo graph crates`
- `rum repo msl omc-reference`
- `rum repo msl omc-simulation-reference`
- `rum repo msl compare-balance`
- `rum repo msl parity-manifest`
- `rum repo msl plot-compare`
- `rum repo msl promote-quality-baseline`
- `rum repo completions <shell>`
- `rum repo release ...`
- `rum repo policy rust-file-lines`

## Cleanup rules

- No backwards compatibility aliases.
- No duplicate public entrypoints.
- No process words like `ci`, `dev`, or `msl` at the top level.
- `parity` is reserved for OMC/MSL parity, not for the fast local repo gate.

## Required updates

- Parser and help output in `crates/rumoca-tool-dev/src/main.rs`
- Verification job naming in `verify_cmd.rs`
- VS Code helper wording in `vscode_cmd.rs`
- README, hooks, workflow, changelog, and specs
- Parser regression tests for the new surface
- Parser rejection tests for removed legacy commands

## Acceptance criteria

- `rum --help` is scannable without prior project context.
- A contributor can discover the correct command by target or workflow name.
- CI uses the same public commands that developers run locally.
- There is exactly one documented command path for each supported workflow.
