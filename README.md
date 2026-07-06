# rumoca

<img src="assets/brand/rumoca.svg" alt="Rumoca Logo" width="128" align="right">

[![CI](https://github.com/cognipilot/rumoca/actions/workflows/ci.yml/badge.svg)](https://github.com/cognipilot/rumoca/actions/workflows/ci.yml)
[![GitHub Pages](https://img.shields.io/badge/GitHub%20Pages-live-2ea44f?logo=github)](https://cognipilot.github.io/rumoca/)
[![PyPI](https://img.shields.io/pypi/v/rumoca)](https://pypi.org/project/rumoca/)
[![npm](https://img.shields.io/npm/v/@cognipilot/rumoca)](https://www.npmjs.com/package/@cognipilot/rumoca)
[![License](https://img.shields.io/badge/license-Apache--2.0-blue.svg)](LICENSE)

**[Try Rumoca in your browser](https://cognipilot.github.io/rumoca/)** (no installation required).

Rumoca is a modern **Modelica compiler and symbolic interoperability platform** written in Rust.

Rumoca’s goal is not only to compile and simulate Modelica models, but to turn **real Modelica package trees** into high-quality symbolic systems that can be used across modern scientific computing, optimization, machine learning, and code generation workflows.

> **Rumoca turns Modelica package trees into modern symbolic systems.**

> **Project status:** Rumoca is in active development. You should expect bugs and rough edges; please file issues at https://github.com/cognipilot/rumoca/issues.

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
- Multi-file session API for CLI, LSP, WASM, and tests (`rumoca-compile`)
- DAE simulation with exact AD Jacobians/mass terms and solver fallbacks (`rumoca-sim-core`)
- Structural preparation and IC planning for robust initialization (`rumoca-phase-structural`, `rumoca-sim-core`)
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

Alternatively, a reproducible [Nix](https://nixos.org) flake lives at the repo
root (`flake.nix`): `nix develop` drops you into a shell with the exact pinned
toolchain plus Node/Python, `nix build` produces the `rumoca` CLI, and
`nix flake check` runs the same build + clippy + rustfmt gate CI uses.

### Common commands

```bash
# lint a model
cargo run -p rumoca -- lint path/to/model.mo

# compile a model to solve IR JSON or a built-in target
cargo run -p rumoca -- \
  compile path/to/model.mo \
  --model MyModel \
  --target solve-ir

# simulate a model directly
cargo run -p rumoca -- \
  sim path/to/model.mo \
  --model MyModel \
  --t-end 1.0

# run a colocated scenario TOML
cargo run -p rumoca --release -- \
  sim -c examples/simulation/ball_sim.toml

# run the LSP server
cargo run -p rumoca-tool-lsp --bin rumoca-lsp
```

`--model` is optional when the file has a single unambiguous model candidate.

### External Modelica Packages

The examples use pinned Modelica package archives for MSL and CMM. Fetch them
with the developer helper:

```bash
cargo xtask repo modelica-deps ensure
```

The repository examples declare shared library roots in
`examples/rumoca-workspace.toml`, using repository-relative paths such as
`../target/msl/ModelicaStandardLibrary-4.1.0` and
`../target/cmm/CMM-v0.0.2`. Rumoca loads that visible workspace file in VS Code,
the playground, docs live examples, and native tooling.

Scenario TOMLs may also declare top-level `source_roots` for dependencies that
belong only to that run.

### Scenario TOMLs

Runnable examples are configured by colocated TOML files:

```bash
# batch/results-panel simulation
cargo run -p rumoca --release -- sim -c examples/simulation/ball_sim.toml

# interactive quadrotor SIL viewer
cargo run -p rumoca --release -- sim -c examples/interactive/quadrotor/quadrotor_acro.toml

# validate a scenario without running it
cargo run -p rumoca -- sim check -c examples/interactive/quadrotor/quadrotor_acro.toml
```

Each scenario declares one task:

- `task = "simulate"` for simulation and visualization
- `task = "codegen"` for target rendering into an output directory

Generate a commented starting point with:

```bash
cargo run -p rumoca -- sim init > my_model_sim.toml
```

### Code Generation

Codegen uses built-in targets or custom target directories containing
`target.toml` and Jinja templates. List available targets:

```bash
cargo run -p rumoca -- targets
```

Render a codegen scenario:

```bash
cargo run -p rumoca -- \
  compile examples/models/SympyDecay.mo \
  --model SympyDecay \
  --target examples/codegen/standalone_web \
  --output examples/codegen/gen/sympy_decay_standalone_web
```

Codegen scenarios write generated files under `examples/codegen/gen/`, which is
ignored by git.

### Installation

As of **v0.8.0**, Rumoca is distributed from **GitHub Releases** (binaries, Python wheels, VS Code extension, and WASM assets), not crates.io.

This keeps the compiler’s multi-crate architecture intact (similar to rustc’s internal structure), preserving strict phase boundaries and an explicit dependency graph for compiler integrity.

#### Binary installer (GitHub Releases)

```bash
curl --proto '=https' --tlsv1.2 -LsSf https://raw.githubusercontent.com/cognipilot/rumoca/main/infra/install/install.sh | bash
```

Install a specific version (and optionally `rumoca-lsp`):

```bash
curl --proto '=https' --tlsv1.2 -LsSf https://raw.githubusercontent.com/cognipilot/rumoca/main/infra/install/install.sh | bash -s -- --version v0.8.0 --with-lsp
```

Windows PowerShell:

```powershell
irm https://raw.githubusercontent.com/cognipilot/rumoca/main/infra/install/install.ps1 | iex
```

The installer defaults to:

- Linux/macOS: `~/.local/bin`
- Windows: `%LOCALAPPDATA%\rumoca\bin`

#### Python package

```bash
pip install rumoca
```

## Developer CLI

Contributor workflows are standardized through the `rum` developer CLI.

Bootstrap it once from the repository root:

```bash
cargo xtask repo cli install
```

If you want the main Modelica parity gate right away:

```bash
cargo xtask verify msl-parity
```

To fetch the cached MSL and CogniPilot Modelica Models (CMM) libraries used by the examples and CI smoke tests, use the pins in `examples/modelica_dependencies.toml`:

```bash
cargo xtask repo modelica-deps ensure
cargo xtask verify examples
```

After that, the main command groups are:

- `cargo xtask verify full` for the full GitHub CI verification suite
- `cargo xtask verify ...` for repo-wide verification and CI-facing gates
- `cargo xtask vscode ...` for VS Code extension build, test, and edit workflows
- `cargo xtask playground ...` for browser playground build, test, and edit workflows
- `cargo xtask coverage ...` for coverage generation, reporting, and gating
- `cargo xtask repo ...` for hooks, releases, completions, graphs, cached Modelica dependencies, and MSL reference-data maintenance

## Documentation

- Playground: <https://cognipilot.github.io/rumoca/>
- User book: <https://cognipilot.github.io/rumoca/user-guide/> (`docs/user-guide/`)
- Developer book: <https://cognipilot.github.io/rumoca/dev-guide/> (`docs/dev-guide/`)
- Normative design rules: `spec/`

The books explain how to use Rumoca and how the implementation is organized.
The specs are the source of truth for architecture and contribution rules.
The user book can host native WASM example components: focused Monaco editors
that call Rumoca on book-local files and render the relevant simulation views.

GitHub Pages deploys the WASM playground at the site root and both mdBook
books as subdirectories from the same CI artifact. Locally, build the same
pieces with:

```bash
cargo xtask playground build --variant full-web
cargo xtask docs build
cargo xtask docs serve
```

Common examples:

```bash
cargo xtask verify full
cargo xtask verify quick
cargo xtask verify docs
cargo xtask verify template-runtimes
cargo xtask verify msl-parity
cargo xtask vscode test
cargo xtask playground test
cargo xtask playground build --variant core
cargo xtask playground build --dev --variant sim-diffsol
cargo xtask playground build --variant full-web --rayon --pack
cargo xtask repo msl promote-quality-baseline
cargo xtask help verify
```

For npm-package workflows, use:

```bash
cd packages/rumoca
npm run build
npm run build:release:core
npm run build:release:sim-diffsol:pack
```

Rust-only workflows such as `cargo build`, `cargo check`, `cargo test`, and
`cargo xtask --help` do not require Node/npm. Package, playground, VS Code, and
browser-asset workflows do require Node/npm; CI uses Node 20, so local package
validation should use Node 20 as well. Check local setup with:

```bash
node --version
npm --version
```

`cargo xtask verify quick` runs the fast local gates: lint, workspace tests, binary
builds, and template runtime checks. `cargo xtask verify full` mirrors the main CI
verification suite, including example smoke tests, coverage, docs,
editor/WASM gates, and the slow full MSL parity gate. The full suite assumes
the local coverage/editor prerequisites are installed (`cargo-llvm-cov`,
Node 20/npm for package/web tasks, and wasm Rust tooling).
`cargo xtask verify template-runtimes` wraps the
equivalent Cargo command for opt-in example-template runtime checks:
`cargo test -p rumoca --features template-runtime-tests --test backend_template_runtime_regression -- --nocapture`.

## Compiler Pipeline

| Stage       | Crate                      | Main Responsibility                                            |
| ----------- | -------------------------- | -------------------------------------------------------------- |
| Parse       | `rumoca-phase-parse`       | Parse Modelica source into AST/class tree                      |
| Resolve     | `rumoca-phase-resolve`     | DefId assignment, scope setup, name resolution                 |
| Typecheck   | `rumoca-phase-typecheck`   | Type resolution, dimension evaluation, structural parameters   |
| Instantiate | `rumoca-phase-instantiate` | Extends/modifier application, model instantiation              |
| Flatten     | `rumoca-phase-flatten`     | Hierarchy flattening, connection expansion, residual equations |
| ToDAE       | `rumoca-phase-dae`         | Variable classification and DAE construction                   |
| Structural  | `rumoca-phase-structural`  | BLT, incidence/matching, IC plan generation                    |
| Simulate    | `rumoca-sim-core`               | IC solving + runtime integration                               |
| Codegen     | `rumoca-phase-codegen`     | Template-driven target generation                              |

## Code Generation Targets

Use explicit template files you own and version with your project.
The raw template example in `examples/codegen/custom_casadi.jinja` is a
starting point, not a stable production artifact.

## VS Code Extension

The VS Code extension is available as **Rumoca Modelica** in the marketplace and includes a bundled `rumoca-lsp` server.

Linux VS Code release binaries are built against `musl` for both `linux-x64` and `linux-arm64`.
This avoids remote-host `glibc` version mismatches for bundled `rumoca-lsp` deployments (for
example over Remote/SSH).

Maintainer reproduction commands for Linux release artifacts:

```bash
cargo xtask vscode package --target linux-x64
cargo xtask vscode package --target linux-arm64
```

On Debian/Ubuntu, the first run can install `musl-tools` for you:

```bash
cargo xtask vscode package --target linux-x64 --install-musl-tools
```

- Extension docs: `packages/vscode/README.md`
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

Rumoca is licensed under the [Apache License 2.0](LICENSE).
