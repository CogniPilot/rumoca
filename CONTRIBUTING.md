# Contributing to Rumoca

Contributions are welcome.

## Setup

Install the pinned repository workflow runner once:

```bash
cargo install --locked cargo-make --version 0.37.24
```

Nix is optional. `nix develop` supplies cargo-make and the default native
Rust toolchain, while named shells add optional tools:

```bash
nix develop .#wasm
nix develop .#vscode
nix develop .#python
nix develop .#modelica
nix develop .#docs
nix develop .#full
```

Native installations work identically. Node/npm is required for browser and VS
Code workflows, Python 3 plus maturin for notebooks, and mdBook for docs.

Repository completion delegates ordinary Cargo syntax, workspace packages, and
binary targets to Cargo's native completion engine. A small static overlay adds
public `cargo make` tasks and grouped tool commands without invoking Cargo on
the `cargo make` completion path. Re-source it after changing make tasks:

```bash
source "$PWD/infra/cargo-make/completions/cargo-make.bash"
```

Fish users can source `infra/cargo-make/completions/cargo-make.fish`. The Nix
shell sources Bash completion automatically. Cargo's native completion requires
the pinned nightly toolchain used by this repository.

Install the repository hooks without another task runner:

```bash
chmod +x .githooks/pre-commit .githooks/pre-push
git config core.hooksPath .githooks
```

## Command Layout

Cargo owns Rust dependency resolution and freshness. Use ordinary `cargo build`,
`cargo check`, `cargo test`, and `cargo run -p ...` commands for Rust-only
work. Cargo-make owns the small set of multi-tool workflows:

```bash
cargo make vscode-{edit,test}
cargo make playground-{build,edit,test}
cargo make docs-{build,serve}
cargo make verify-{lint,quick,full}
cargo make vscode-package --target linux-x64
```

Run `cargo make` for this concise list. Internal dependency nodes are private
and intentionally absent from completion.

Purpose-specific Rust tools remain grouped behind stable cargo-make entry
points; arguments after the task name go to the owning tool:

```bash
cargo make modelica-deps
cargo make msl-parity
cargo make msl --help
cargo make coverage --help
cargo make crate-graph --format dot
cargo make review scan --help
cargo make release --help
```

The cargo-make DAG does not alias ordinary `cargo build/check/test` commands.
It provides one stable front door for repository workflows and focused tool
owners while keeping implementation nodes private.

## VS Code and notebooks

`cargo make vscode-edit` builds the language-server binaries through Cargo,
stages only changed binaries, refreshes npm assets only when lockfiles or
sources changed, and launches an isolated extension-development profile. It
also installs the official Python and Jupyter extensions and prepares
`examples/.venv` with the local Rumoca binding. A warm relaunch reuses all of
those outputs. Python uses its own `target/python` Cargo cache so switching
between normal Rust work and notebook preparation does not invalidate the
large Python/Zenoh dependency graph.

Force the notebook leaves only when needed:

```bash
cargo make vscode-notebooks-refresh
```

The VS Code Nix shell is convenient but not required:

```bash
nix develop .#vscode --command cargo make vscode-edit
```

## Kani bounded verification

The dedicated Kani shell pins Kani 0.67.0 and its matching Rust toolchain. The
purpose-specific verifier validates `infra/verification/kani-proofs.json`, runs
each manifest harness independently, and writes
`target/verification/kani-summary.json`:

```bash
nix develop .#kani --command cargo make kani
```

Ordinary `cargo test -p rumoca-solver` runs validation fallbacks; only a
successful pinned Kani run is proof evidence.

## Verification

Use the smallest command that covers the change:

```bash
cargo fmt --check
cargo clippy --workspace --all-targets --all-features -- -D warnings
cargo test --workspace
cargo make verify-lint
cargo make verify-quick
cargo make verify-full
```

The Cargo-native template runtime suite is:

```bash
cargo test -p rumoca --no-default-features \\
  --features template-runtime-tests,fmu-packaging \\
  --test suite_template_runtime -- --nocapture
```

The MSL parity gate is owned by the MSL harness package:

```bash
cargo make msl-parity
cargo make msl \\
  flamegraph --model Modelica.Electrical.Digital.Examples.DFFREG --mode compile
```

Verification-surface classification:

- `msl-sim-tests` selects the required MSL simulation regressions.
- `backend-stress-tests` is an opt-in 30-model diagnostic survey, not a
  correctness gate.
- `msl-external-tests` contains opt-in MSL corpus cross-checks for generated
  backends.

## Parser Grammar Regeneration

The Modelica parser is generated from
`crates/rumoca-phase-parse/src/modelica.par`, and the GALEC parser is generated
from `crates/rumoca-phase-parse-galec/src/parse/galec.par`, by their phase-crate
build scripts. Generated Rust files are checked in beside each grammar so
parser changes are reviewable.

When changing the grammar or parser generator settings, regenerate and test
with:

```bash
cargo check -p rumoca-phase-parse
cargo test -p rumoca-phase-parse --test recovery_corpus --quiet
cargo check -p rumoca-phase-parse-galec
cargo test -p rumoca-phase-parse-galec --quiet
git diff -- crates/rumoca-phase-parse/src/generated
git diff -- crates/rumoca-phase-parse-galec/src/parse/generated
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
- Use `cargo make` for multi-tool workflows and each focused Cargo package for
  Rust-only tooling so local and CI workflows stay aligned.
- Keep contributor-facing command examples in docs synchronized with the actual CLI.
- Include a PR size budget in the pull-request body:
  - production lines added/deleted,
  - test lines added/deleted,
  - net lines and file count,
  - public API item delta.
- If the PR has positive net lines, include a short cleanup/compression pass plan
  and explicit rationale for every new abstraction.
