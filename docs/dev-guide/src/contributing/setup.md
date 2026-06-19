# Getting Started

## Clone and Build

Install the Rust toolchain pinned by `rust-toolchain.toml`, then:

```bash
git clone https://github.com/CogniPilot/rumoca
cd rumoca
cargo build --workspace
```

## Install the Developer CLI

The repository uses an `xtask` developer CLI (the
[cargo-xtask](https://github.com/matklad/cargo-xtask) convention) for every
verification, packaging, and maintenance workflow. Install the standalone
launcher and the repo hooks once:

```bash
cargo xtask repo cli install     # `xtask` on PATH + shell completions
cargo xtask repo hooks install   # git hooks
```

After that, `xtask ...` works from anywhere in the workspace (the
`cargo xtask ...` form always works too).

## Fetch Modelica Dependencies

```bash
cargo xtask repo modelica-deps ensure
```

Downloads the pinned MSL and CMM versions into `target/`, which the
examples, tests, and committed VS Code settings expect.

## Sanity Check

```bash
cargo xtask verify quick
```

This runs the same verification surface as GitHub CI except the slow
full-MSL parity gate. It expects the local prerequisites CI installs:
`cargo-llvm-cov`, Node/npm, and the wasm Rust target/tooling. For the
narrow loops you will actually iterate with, see
[Testing and Quality Gates](./testing.md).

## Editor Setup

Open the repository root in VS Code with the **Rumoca Modelica** extension.
For compiler development, enable `rumoca.useSystemServer` and put your
locally built `rumoca-lsp` on `PATH` so the editor exercises your changes.
Launch the extension from source in `packages/vscode` when working on the
extension itself.

## Find Your Bearings

1. Read [Pipeline Overview](../compiler/pipeline-overview.md) if you have
   not yet.
2. Read [Where the Rules Live](./specs-process.md) — five minutes that
   will save your first review round.
3. Pick the chapter for the area you are changing; it links the owning
   spec.
