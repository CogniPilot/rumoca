# rumoca

<img src="editors/icons/rumoca.png" alt="Rumoca Logo" width="128" align="right">

[![CI](https://github.com/climamind/rumoca/actions/workflows/ci.yml/badge.svg)](https://github.com/climamind/rumoca/actions/workflows/ci.yml)
[![GitHub Pages](https://img.shields.io/badge/GitHub%20Pages-live-2ea44f?logo=github)](https://climamind.github.io/rumoca/)
[![PyPI](https://img.shields.io/pypi/v/rumoca)](https://pypi.org/project/rumoca/)
[![License](https://img.shields.io/badge/license-Apache--2.0-blue.svg)](LICENSE)

**[Try Rumoca in your browser](https://climamind.github.io/rumoca/)** (no installation required).

This repository is the ClimaMind-maintained Rumoca line. Rumoca originated in
the CogniPilot community; see [NOTICE](NOTICE) for attribution and third-party
license notes.

Rumoca is a modern **Modelica compiler and symbolic interoperability platform** written in Rust.

Rumoca’s goal is not only to compile and simulate Modelica models, but to turn **real Modelica package trees** into high-quality symbolic systems that can be used across modern scientific computing, optimization, machine learning, and code generation workflows.

> **Rumoca turns Modelica package trees into modern symbolic systems.**

> **Project status:** Rumoca is in active development. You should expect bugs and rough edges; please file issues at https://github.com/climamind/rumoca/issues.

## Why Rumoca Exists

There is a large ecosystem of useful engineering models already written in Modelica, but many modern workflows live elsewhere:

- Julia / SciML
- Python / JAX / CasADi / PyTorch
- embedded and generated-code targets
- browser and tooling workflows via WASM

Rumoca bridges that gap.

It treats Modelica as a **semantic frontend** and compiles models into rich intermediate forms suitable for:

- robust simulation
- symbolic analysis
- optimization and differentiable workflows
- multi-backend code generation
- interoperability with downstream tools

## How Rumoca Differs

### Rumoca vs existing Modelica compilers

Traditional Modelica compilers primarily focus on simulation, FMU export, and tool-specific execution pipelines.

Rumoca also supports simulation, but its broader goal is to expose Modelica models as **portable symbolic systems**.

Rumoca emphasizes:

- explicit compiler phases and IR boundaries
- strong structural analysis and DAE lowering
- backend-neutral code generation
- modern tooling and language integrations
- reusable symbolic outputs rather than a single closed execution path

Rumoca is not just trying to be another simulator. It aims to be a **compiler and interoperability layer** for declarative physical models.

### Rumoca vs host-language modeling approaches

Frameworks like Julia's ModelingToolkit are powerful because they let users build symbolic models directly inside a general-purpose language.

Rumoca starts from a different place:

- **host-language approaches start from a programming environment**
- **Rumoca starts from a dedicated declarative modeling language**

That gives Rumoca a different set of strengths:

- compatibility with actual Modelica packages
- stronger frontend semantics
- a narrower and more analyzable modeling surface
- a better foundation for predictable structural analysis and compilation
- a path to unlock existing engineering packages for downstream symbolic ecosystems

Rumoca complements tools like ModelingToolkit rather than merely competing with them:

> **Rumoca can make standards-based declarative models available to symbolic ecosystems without requiring those ecosystems to adopt Modelica as their authoring language.**

## Project Focus

Rumoca focuses on five things:

1. **Modelica correctness and compiler quality**  
   Parse, resolve, instantiate, flatten, and lower Modelica models into robust canonical forms.

2. **Symbolic interoperability**  
   Generate equation systems and metadata suitable for downstream symbolic tools in Julia, Python, Rust, and beyond.

3. **Robust DAE execution**  
   Provide a Rust-native simulation stack with strong initialization, structural preparation, and solver fallback behavior.

4. **Modern tooling**  
   LSP, formatter, linting, Python bindings, and WASM support.

5. **Multi-backend export**  
   Target numerical, symbolic, learned, and embedded workflows from a common model source.

## Core Features

- Full compiler pipeline: parse -> resolve -> typecheck -> instantiate -> flatten -> DAE
- Multi-file session API for CLI, LSP, WASM, and tests (`rumoca-session`)
- DAE simulation with exact AD Jacobians/mass terms and solver fallbacks (`rumoca-sim`)
- Structural preparation and IC planning for robust initialization (`rumoca-phase-solve`, `rumoca-sim`)
- Explicit template rendering support for custom code generation
- MLS contract test framework (`rumoca-contracts`)
- Spec-driven quality gates (including SPEC_0021 and SPEC_0025)

## What Rumoca Enables

Rumoca is designed so that a model package tree can be authored once and then used across multiple communities.

For example, the same Modelica model can be compiled into forms suitable for:

- simulation in Rust
- symbolic workflows in Julia
- machine learning pipelines in Python
- embedded code generation targets
- browser-based tools via WASM

The goal is to make model package trees belong to the **models themselves**, not to a single language ecosystem.

## Quick Start

### Requirements

- Rust toolchain from `rust-toolchain.toml` (nightly, `wasm32-unknown-unknown` target)

### Build

```bash
cargo build --workspace
```

### Common commands

```bash
# lint a model
cargo run -p rumoca -- lint path/to/model.mo

# compile a model to JSON
cargo run -p rumoca -- \
  compile path/to/model.mo \
  --model MyModel \
  --json

# simulate a model
cargo run -p rumoca -- \
  simulate path/to/model.mo \
  --model MyModel \
  --t-end 1.0

# run the LSP server
cargo run -p rumoca-tool-lsp --bin rumoca-lsp
```

`--model` is optional when the file has a single unambiguous model candidate.

### Generate a self-contained HTML simulator

Assuming you have a template such as `examples/templates/standalone_html.jinja`:

```bash
cargo run -p rumoca -- compile path/to/model.mo --model MyModel --template-file examples/templates/standalone_html.jinja --template-prepared > MyModel_standalone.html
```

MSL Electrical resistor example (downloads MSL 4.1.0, compiles `Modelica.Electrical.Analog.Examples.Resistor` via a tiny wrapper model, and writes standalone HTML):

```bash
# download msl
curl -L -o /tmp/ModelicaStandardLibrary-4.1.0.zip https://github.com/modelica/ModelicaStandardLibrary/archive/refs/tags/v4.1.0.zip && unzip -q -o /tmp/ModelicaStandardLibrary-4.1.0.zip -d /tmp

# add our model
printf 'model MslResistorExample\n  import Complex;\n  import ModelicaServices;\n  extends Modelica.Electrical.Analog.Examples.Resistor;\nend MslResistorExample;\n' > /tmp/MslResistorExample.mo

# convert into standalone html
cargo run -p rumoca -- compile /tmp/MslResistorExample.mo --model MslResistorExample --source-root /tmp/ModelicaStandardLibrary-4.1.0/Modelica --source-root /tmp/ModelicaStandardLibrary-4.1.0/ModelicaServices --source-root /tmp/ModelicaStandardLibrary-4.1.0/Complex.mo --template-file examples/templates/standalone_html.jinja --template-prepared > MslResistorExample_standalone.html
```

### Installation

As of **v0.8.0**, Rumoca is distributed from **GitHub Releases** (binaries, Python wheels, VS Code extension, and WASM assets), not crates.io.

This keeps the compiler’s multi-crate architecture intact (similar to rustc’s internal structure), preserving strict phase boundaries and an explicit dependency graph for compiler integrity.

#### Binary installer (GitHub Releases)

```bash
curl --proto '=https' --tlsv1.2 -LsSf https://raw.githubusercontent.com/climamind/rumoca/main/install/install.sh | bash
```

Install a specific version (and optionally `rumoca-lsp`):

```bash
curl --proto '=https' --tlsv1.2 -LsSf https://raw.githubusercontent.com/climamind/rumoca/main/install/install.sh | bash -s -- --version v0.8.0 --with-lsp
```

Windows PowerShell:

```powershell
irm https://raw.githubusercontent.com/climamind/rumoca/main/install/install.ps1 | iex
```

The installer defaults to:

- Linux/macOS: `~/.local/bin`
- Windows: `~/.rumoca/bin`

#### Python package

```bash
pip install rumoca
```

## Developer CLI

Contributor workflows are standardized through the `rum` developer CLI.

Bootstrap it once from the repository root:

```bash
cargo run --bin rum -- repo cli install
```

If you want the main Modelica parity gate right away:

```bash
rum verify msl-parity
```

After that, the main command groups are:

- `rum verify full` for the full GitHub CI verification suite
- `rum verify ...` for repo-wide verification and CI-facing gates
- `rum vscode ...` for VS Code extension build, test, and edit workflows
- `rum wasm ...` for wasm editor build, test, and edit workflows
- `rum coverage ...` for coverage generation, reporting, and gating
- `rum repo ...` for hooks, releases, completions, graphs, and MSL reference-data maintenance

Common examples:

```bash
rum verify full
rum verify quick
rum verify template-runtimes
rum verify msl-parity
rum vscode test
rum wasm test
rum repo msl promote-quality-baseline
rum help verify
```

`rum verify quick` mirrors the main CI verification suite but skips the slow
180-model MSL parity gate. `rum verify full` includes that parity job.
Both commands assume the local coverage/editor prerequisites are installed
(`cargo-llvm-cov`, Node/npm, and wasm Rust tooling), matching GitHub CI.
`rum verify template-runtimes` runs ignored example-template runtime checks
that depend on optional backend environments such as Python with SymPy.

## Compiler Pipeline

| Stage       | Crate                      | Main Responsibility                                            |
| ----------- | -------------------------- | -------------------------------------------------------------- |
| Parse       | `rumoca-phase-parse`       | Parse Modelica source into AST/class tree                      |
| Resolve     | `rumoca-phase-resolve`     | DefId assignment, scope setup, name resolution                 |
| Typecheck   | `rumoca-phase-typecheck`   | Type resolution, dimension evaluation, structural parameters   |
| Instantiate | `rumoca-phase-instantiate` | Extends/modifier application, model instantiation              |
| Flatten     | `rumoca-phase-flatten`     | Hierarchy flattening, connection expansion, residual equations |
| ToDAE       | `rumoca-phase-dae`         | Variable classification and DAE construction                   |
| Structural  | `rumoca-phase-solve`       | BLT, incidence/matching, IC plan generation                    |
| Simulate    | `rumoca-sim`               | IC solving + runtime integration                               |
| Codegen     | `rumoca-phase-codegen`     | Template-driven target generation                              |

## Code Generation Targets

Use explicit template files you own and version with your project.
Template files in `examples/templates` are works in progress and need cleanup. Treat them as editable starting points, not stable production artifacts.

## VS Code Extension

The VS Code extension is available as **Rumoca Modelica** in the marketplace and includes a bundled `rumoca-lsp` server.

Linux VS Code release binaries are built against `musl` for both `linux-x64` and `linux-arm64`.
This avoids remote-host `glibc` version mismatches for bundled `rumoca-lsp` deployments (for
example over Remote/SSH).

Maintainer reproduction commands for Linux release artifacts:

```bash
rum vscode package --target linux-x64
rum vscode package --target linux-arm64
```

On Debian/Ubuntu, the first run can install `musl-tools` for you:

```bash
rum vscode package --target linux-x64 --install-musl-tools
```

- Extension docs: `editors/vscode/README.md`
- Marketplace: https://marketplace.visualstudio.com/items?itemName=JamesGoppert.rumoca-modelica

## Contributing

Contributions are welcome.

See [CONTRIBUTING.md](CONTRIBUTING.md) for contributor setup, workflow, and verification commands.

Compiler-affecting changes should still follow:

- `spec/SPEC_0025_PR_REVIEW_PROCESS.md`
- `spec/README.md`

## Citation

```bibtex
@inproceedings{condie2025rumoca,
  title={Rumoca: Towards a Translator from Modelica to Algebraic Modeling Languages},
  author={Condie, Micah and Woodbury, Abigaile and Goppert, James and Andersson, Joel},
  booktitle={Modelica Conferences},
  pages={1009--1016},
  year={2025}
}
```

## License

Apache-2.0 (`LICENSE`)
