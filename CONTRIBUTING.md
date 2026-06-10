# Contributing to Rumoca

Contributions are welcome.

## Setup

Install the `xtask` developer CLI launcher once:

```bash
cargo xtask repo cli install
```

That installs the `xtask` launcher, installs shell completions for the detected shell, and uses your cargo bin directory, usually `~/.cargo/bin`.
If that directory is not already on `PATH`, `xtask` will print shell-specific fixups.

If you want `xtask` to write the persistent PATH update for you:

```bash
cargo xtask repo cli install --path
```

Then install the repo hooks:

```bash
cargo xtask repo hooks install
```

## Command Layout

The canonical top-level command groups are:

- `cargo xtask verify full` for the full local/CI verification suite
- `cargo xtask verify quick` for the same verification surface except the long full-MSL parity gate
- `cargo xtask verify ...` for local and CI verification gates
- `cargo xtask vscode ...` for VS Code extension workflows
- `cargo xtask wasm ...` for wasm editor workflows
- `cargo xtask python ...` for Python binding workflows
- `cargo xtask coverage ...` for coverage generation, reporting, and gating
- `cargo xtask repo ...` for hooks, completions, releases, graphs, policy helpers, and MSL reference-data maintenance

## Common Commands

Typical local verification:

```bash
cargo xtask verify full
cargo xtask verify lint
cargo xtask verify workspace
cargo xtask verify quick
cargo xtask verify template-runtimes
```

`cargo xtask verify quick` runs the same verification surface as GitHub CI except
for the slow full-MSL parity gate. `cargo xtask verify full` includes that parity
run. Because those commands include coverage, VS Code, and wasm gates, they
expect the same local prerequisites that CI installs: `cargo-llvm-cov`, Node/npm,
and the wasm Rust target/tooling. `cargo xtask verify template-runtimes` wraps
Cargo-native opt-in example-template execution checks such as
`cargo test -p rumoca --features template-runtime-tests --test backend_template_runtime_regression -- --nocapture`.

Editor validation:

```bash
cargo xtask vscode test
cargo xtask wasm test
```

Extension packaging:

```bash
cargo xtask vscode build
cargo xtask vscode package --target linux-x64
```

MSL/reference maintenance:

```bash
cargo xtask verify msl-parity
cargo xtask repo msl omc-reference
cargo xtask repo msl flamegraph --model Modelica.Electrical.Digital.Examples.DFFREG --mode compile
cargo xtask repo msl promote-quality-baseline
```

Command discovery:

```bash
cargo xtask help
cargo xtask help verify
cargo xtask help repo msl
cargo xtask help repo cli install
```

## Parser Grammar Regeneration

The Modelica parser is generated from
`crates/rumoca-phase-parse/src/modelica.par` by the crate build script. The
generated Rust files are checked in under
`crates/rumoca-phase-parse/src/generated/` so parser changes are reviewable.

When changing the grammar or parser generator settings, regenerate and test
with:

```bash
cargo check -p rumoca-phase-parse
cargo test -p rumoca-phase-parse --test recovery_corpus --quiet
git diff -- crates/rumoca-phase-parse/src/generated
```

The workspace pins `parol` and `parol_runtime` to exact patch versions in
`Cargo.toml`. Do not loosen those pins with a grammar change; update the pin
intentionally and review the generated diff in the same change.

## Process

For compiler-affecting changes, follow:

- `spec/SPEC_0025_PR_REVIEW_PROCESS.md`
- `spec/README.md`

Project specifications live under [`spec/`](spec/).

## Practical Expectations

- Run the smallest verification gate that actually covers your change.
- Prefer `cargo xtask` commands over ad hoc local scripts so local and CI workflows stay aligned.
- Keep contributor-facing command examples in docs synchronized with the actual CLI.
- Include a PR size budget in the pull-request body:
  - production lines added/deleted,
  - test lines added/deleted,
  - net lines and file count,
  - public API item delta.
- If the PR has positive net lines, include a short cleanup/compression pass plan
  and explicit rationale for every new abstraction.
