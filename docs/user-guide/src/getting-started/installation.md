# Installation

Rumoca is distributed from **GitHub Releases**: prebuilt binaries, Python
wheels, the VS Code extension, and WASM assets. It is not published to
crates.io.

## Prebuilt Binaries (Recommended)

Linux and macOS:

```bash
curl --proto '=https' --tlsv1.2 -LsSf https://raw.githubusercontent.com/cognipilot/rumoca/main/infra/install/install.sh | bash
```

Install a specific version, and optionally the `rumoca-lsp` language server:

```bash
curl --proto '=https' --tlsv1.2 -LsSf https://raw.githubusercontent.com/cognipilot/rumoca/main/infra/install/install.sh | bash -s -- --version v0.8.0 --with-lsp
```

Windows PowerShell uses `infra/install/install.ps1` from the same directory.

The installer places binaries in `~/.local/bin` by default; override with
`--bin-dir <path>`. Check the result with:

```bash
rumoca --version
```

## VS Code Extension

Install **Rumoca Modelica** from the
[VS Code marketplace](https://marketplace.visualstudio.com/items?itemName=JamesGoppert.rumoca-modelica).
The extension bundles its own `rumoca-lsp` server, so it works without a
separate compiler install. See [VS Code Extension](../tools/vscode.md).

## Python Package

```bash
pip install rumoca
```

The Python package exposes the compiler for scripting and notebook use.

## Shell Completions

```bash
rumoca completions bash > ~/.local/share/bash-completion/completions/rumoca
```

Other shells (zsh, fish, …) are supported; run `rumoca completions --help`.

## From Source

Install the Rust toolchain selected by `rust-toolchain.toml`, then build the
workspace:

```bash
git clone https://github.com/CogniPilot/rumoca
cd rumoca
cargo build --workspace
```

For interactive simulation, prefer release builds:

```bash
cargo run -p rumoca --release -- --help
```

The repository includes an `xtask` developer CLI used by CI and local
development (`cargo xtask verify quick`, `cargo xtask vscode test`, …). It is
documented in the
[Rumoca Dev Guide](https://cognipilot.github.io/rumoca/dev-guide/) book and
`CONTRIBUTING.md`.

## Modelica Library Dependencies

The repository examples use pinned Modelica dependencies declared in
`examples/modelica_dependencies.toml`. Fetch them with:

```bash
cargo xtask repo modelica-deps ensure
```

This downloads the Modelica Standard Library (MSL) and the CogniPilot
Modelica Models (CMM) into `target/`. The repository's committed VS Code
settings point at those directories with workspace-relative paths, so the
examples work as soon as the download finishes. See
[Using Modelica Libraries](../language/libraries.md) for how library lookup
works in general.
