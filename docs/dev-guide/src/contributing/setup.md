# Getting Started

## Clone and Build

Install the Rust toolchain pinned by `rust-toolchain.toml`, then:

```bash
git clone https://github.com/CogniPilot/rumoca
cd rumoca
cargo build --workspace
```

## Install the Workflow Runner

Cargo owns Rust dependency graphs and freshness. The repository uses
`cargo-make` only to compose Cargo with npm, Python, mdBook, browsers, and
other external tools. Install the pinned version and repository hooks once:

```bash
cargo install --locked cargo-make --version 0.37.24
git config core.hooksPath .githooks
```

`nix develop` already includes cargo-make, but Nix is optional. Run
`cargo make` for the concise public workflow list. Fast Bash/Fish completion
scripts live in `infra/cargo-make/completions/`.

## Fetch Modelica Dependencies

```bash
cargo make modelica-deps
```

Downloads the pinned MSL and CMM versions into `target/`, which the
examples, tests, and committed VS Code settings expect.

## Sanity Check

```bash
cargo make verify-quick
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
