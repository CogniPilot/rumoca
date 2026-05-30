# Installation

## From Source

Install the Rust toolchain selected by `rust-toolchain.toml`, then build the
workspace:

```bash
cargo build --workspace
```

For faster interactive simulation, use release builds:

```bash
cargo run -p rumoca --release -- --help
```

## Developer Helper

The repository includes an `xtask` helper (following the
[cargo-xtask](https://github.com/matklad/cargo-xtask) convention) used by CI and
local development. Run it from anywhere in the workspace via the `cargo xtask`
alias:

```bash
cargo xtask verify quick
```

Optionally install a standalone `xtask` launcher on your `PATH` so you can run
`xtask ...` directly without the `cargo` prefix:

```bash
cargo xtask repo cli install
```

After that, `cargo xtask verify ...`, `cargo xtask vscode ...`, and
`cargo xtask repo ...` commands are available from the shell.

## Modelica Dependencies

The examples use pinned Modelica dependencies declared in
`examples/modelica_dependencies.toml`. Fetch them with:

```bash
cargo xtask repo modelica-deps ensure
```

This downloads MSL and CMM into `target/`. The repository's committed VS Code
settings point at those target directories using workspace-relative paths.

## VS Code Extension

When developing from source, launch the extension from `editors/vscode`.
The extension can use the bundled binary or a system `rumoca-lsp`; for local
compiler work, enable `rumoca.useSystemServer` and put your built `rumoca-lsp`
on `PATH`.
