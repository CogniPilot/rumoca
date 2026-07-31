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
- `cargo xtask playground ...` for browser playground workflows
- `cargo xtask python ...` for Python binding workflows
- `cargo xtask coverage ...` for coverage generation, reporting, and gating
- `cargo xtask repo ...` for hooks, completions, releases, graphs, policy helpers, and MSL reference-data maintenance

## Local Prerequisites

Rust-only workflows do not require Node/npm:

```bash
cargo build
cargo check
cargo test
cargo xtask --help
```

Package, playground, VS Code, and browser-asset workflows do require Node/npm.
CI uses Node 20, so local package validation should use Node 20 as well:

```bash
node --version
npm --version
```

Commands that may install npm dependencies or run npm package builds include:

```bash
cargo xtask web build
cargo xtask playground test
cargo xtask vscode build
cargo xtask vscode package --target linux-x64
```

Cargo builds must remain Rust-only. If a selected package/web command reports a
missing `node` or `npm`, install Node 20 using your platform package manager,
Volta, nvm, or the official Node installer, then retry that command.

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
expect the same local prerequisites that CI installs: `cargo-llvm-cov`, Node
20/npm for package/web tasks, and the wasm Rust target/tooling.
`cargo xtask verify template-runtimes` wraps
Cargo-native opt-in example-template execution checks such as
`cargo test -p rumoca --features template-runtime-tests --test suite_template_runtime backend_template_runtime_regression:: -- --nocapture`.

Editor validation:

```bash
cargo xtask vscode test
cargo xtask playground test
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

Verification-surface classification:

- `cargo xtask verify workspace` includes the two required
  `rumoca/msl-sim-tests` MSL simulation regressions. It needs the pinned MSL
  tree at `target/msl/ModelicaStandardLibrary-4.1.0`, which the CI workspace
  job stages before running.
- `backend-stress-tests` is an opt-in 30-model diagnostic survey, not a
  correctness gate: it reports per-model failures and only requires one
  end-to-end comparison for each selected backend.
- `msl-external-tests` contains opt-in MSL corpus cross-checks for generated
  backends. Nightly CI surveys FMI2, FMI3, embedded C, and CasADi under the Nix
  development shell. `fmu_target_discovery` is a manual target-list maintenance
  tool and is not a pass/fail verification gate.

```bash
nix develop --command cargo test --release -p rumoca-test-msl \
  --features backend-stress-tests --test backend_stress_test -- --nocapture
nix develop --command cargo test --release -p rumoca-test-msl \
  --features msl-external-tests --test fmi2_msl_test -- --nocapture
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
- Prefer `cargo xtask` commands over ad hoc local scripts so local and CI workflows stay aligned.
- Keep contributor-facing command examples in docs synchronized with the actual CLI.
- Include a PR size budget in the pull-request body:
  - production lines added/deleted,
  - test lines added/deleted,
  - net lines and file count,
  - public API item delta.
- If the PR has positive net lines, include a short cleanup/compression pass plan
  and explicit rationale for every new abstraction.
