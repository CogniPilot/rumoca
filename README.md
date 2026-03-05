# rumoca

<img src="editors/icons/rumoca.png" alt="Rumoca Logo" width="128" align="right">

[![CI](https://github.com/cognipilot/rumoca/actions/workflows/ci.yml/badge.svg)](https://github.com/cognipilot/rumoca/actions/workflows/ci.yml)
[![GitHub Pages](https://img.shields.io/badge/GitHub%20Pages-live-2ea44f?logo=github)](https://cognipilot.github.io/rumoca/)
[![Crates.io](https://img.shields.io/crates/v/rumoca)](https://crates.io/crates/rumoca)
[![PyPI](https://img.shields.io/pypi/v/rumoca)](https://pypi.org/project/rumoca/)
[![Documentation](https://docs.rs/rumoca/badge.svg)](https://docs.rs/rumoca)
[![License](https://img.shields.io/crates/l/rumoca)](LICENSE)

**[Try Rumoca in your browser](https://cognipilot.github.io/rumoca/)** (no installation required).

Rumoca is a modern Modelica compiler written in Rust.

It provides:

- an end-to-end compilation pipeline from Modelica source to DAE
- a Rust-native DAE simulation stack focused on robust, high-quality runs
- code generation templates for multiple targets
- LSP, formatter, linting, Python, and WASM integrations

## Core Features

- Full compiler pipeline: parse -> resolve -> typecheck -> instantiate -> flatten -> DAE
- Multi-file session API for CLI, LSP, WASM, and tests (`rumoca-session`)
- DAE simulation with exact AD Jacobians/mass terms and solver fallbacks (`rumoca-sim-diffsol`)
- Structural preparation and IC planning for robust initialization (`rumoca-phase-structural`, `rumoca-sim-core`)
- Template-based codegen to CasADi, C, JAX, Julia MTK, ONNX, and Modelica render targets
- MLS contract test framework (`rumoca-contracts`)
- Spec-driven quality gates (including SPEC_0021 and SPEC_0025)

## Quick Start

### Requirements

- Rust toolchain from `rust-toolchain.toml` (nightly, `wasm32-unknown-unknown` target)

### Build

```bash
cargo build --workspace
```

### Command Roles

- `rumoca`: user-facing Modelica CLI (`compile`, `simulate`, `solve`, `check`, `fmt`, `lint`, `project`, `completions`)
- `rum`: developer workflow CLI (`test`, `ci-parity`, `vscode-dev`, `wasm-test`, `coverage`, hook/release tooling)

Examples:

```bash
# User-facing
cargo run -p rumoca -- lint path/to/model.mo
cargo run -p rumoca -- project sync --prune-orphans

# Developer-facing
cargo run --bin rum -- test
cargo run --bin rum -- vscode-dev -w dev/sample
```

### Compile a Model to JSON

```bash
cargo run -p rumoca -- \
  compile path/to/model.mo \
  --model MyModel \
  --json
```

`--model` is optional when the file has a single unambiguous model candidate.

### Generate a Self-Contained HTML Simulator (one-liner)

Assuming you added `templates/standalone_html.jinja` to your repo:

```bash
cargo run -p rumoca -- compile path/to/model.mo --model MyModel --template-file templates/standalone_html.jinja --template-prepared > MyModel_standalone.html
```

MSL Electrical resistor example (downloads MSL 4.1.0, compiles `Modelica.Electrical.Analog.Examples.Resistor` via a tiny wrapper model, and writes standalone HTML):

```bash
# download msl
curl -L -o /tmp/ModelicaStandardLibrary-4.1.0.zip https://github.com/modelica/ModelicaStandardLibrary/archive/refs/tags/v4.1.0.zip && unzip -q -o /tmp/ModelicaStandardLibrary-4.1.0.zip -d /tmp

# add our model
printf 'model MslResistorExample\n  import Complex;\n  import ModelicaServices;\n  extends Modelica.Electrical.Analog.Examples.Resistor;\nend MslResistorExample;\n' > /tmp/MslResistorExample.mo

# convert into standalone html
cargo run -p rumoca -- compile /tmp/MslResistorExample.mo --model MslResistorExample --library /tmp/ModelicaStandardLibrary-4.1.0/Modelica --library /tmp/ModelicaStandardLibrary-4.1.0/ModelicaServices --library /tmp/ModelicaStandardLibrary-4.1.0/Complex.mo --template-file examples/templates/standalone_html.jinja --template-prepared > MslResistorExample_standalone.html
```

### Simulate and Generate an HTML Report

```bash
cargo run -p rumoca -- \
  simulate path/to/model.mo \
  --model MyModel \
  --t-end 1.0
```

### Solve via Phase-Solve Contract

Build symbolic `SolveIr` from Modelica (`Modelica -> DAE -> phase-solve -> SolveIr`) and stop at prep:

```bash
cargo run -p rumoca -- \
  solve path/to/model.mo \
  --model MyModel \
  --backend solve-ir \
  --prep-only \
  --emit-form /tmp/MyModel.solve.json
```

Check form capabilities without running simulation:

```bash
cargo run -p rumoca -- solve path/to/model.mo --model MyModel --list-forms
cargo run -p rumoca -- solve path/to/model.mo --model MyModel --can-build-form mass-matrix
cargo run -p rumoca -- solve path/to/model.mo --model MyModel --list-forms-json
```

Run solve with explicit backend selection:

```bash
# Diffsol backend (requires mass-matrix form)
cargo run -p rumoca -- solve path/to/model.mo --model MyModel --backend diffsol --form mass-matrix

# RK45 backend (requires causal form)
cargo run -p rumoca -- solve path/to/model.mo --model MyModel --backend rk45 --form causal
```

When `--form` is omitted, `rumoca solve` picks a default based on backend/solver:
- diffsol path: `mass-matrix`
- rk45 path: `causal`

### Format Modelica Sources

```bash
cargo run -p rumoca -- fmt --check path/to/models
cargo run -p rumoca -- fmt path/to/models
```

### Lint Modelica Sources

```bash
cargo run -p rumoca -- lint path/to/models
```

### Run LSP Server

```bash
cargo run -p rumoca-tool-lsp --bin rumoca-lsp
```

### Shell Completions

```bash
cargo run -p rumoca -- completions bash
cargo run --bin rum -- completions bash
```

## Installation

### Rust CLI

```bash
cargo install rumoca
```

### Python Package

```bash
pip install rumoca
```

## Compiler Pipeline

| Stage       | Crate                                   | Main Responsibility                                            |
| ----------- | --------------------------------------- | -------------------------------------------------------------- |
| Parse       | `rumoca-phase-parse`                    | Parse Modelica source into AST/class tree                      |
| Resolve     | `rumoca-phase-resolve`                  | DefId assignment, scope setup, name resolution                 |
| Typecheck   | `rumoca-phase-typecheck`                | Type resolution, dimension evaluation, structural parameters   |
| Instantiate | `rumoca-phase-instantiate`              | Extends/modifier application, model instantiation              |
| Flatten     | `rumoca-phase-flatten`                  | Hierarchy flattening, connection expansion, residual equations |
| ToDAE       | `rumoca-phase-dae`                      | Variable classification and DAE construction                   |
| Structural  | `rumoca-phase-structural`               | BLT, incidence/matching, IC plan generation                    |
| Simulate    | `rumoca-sim-core`, `rumoca-sim-diffsol` | IC solving + runtime integration                               |
| Codegen     | `rumoca-phase-codegen`                  | Template-driven target generation                              |

Session pipeline invariants and failure contracts:

- `crates/rumoca-session/PIPELINE_INVARIANTS.md`

## Workspace Crate Catalog

### Prefix and Layering Guide

Crate names are intentionally prefixed to show role and dependency direction.

| Prefix            | Meaning                                | Responsibility Boundary                                                                 | Typical Depends On |
| ----------------- | -------------------------------------- | --------------------------------------------------------------------------------------- | ------------------ |
| `rumoca`          | Top-level product and orchestration    | End-user CLI/API entry point and high-level pipeline composition                        | `session`, `phase-*`, `sim-*` |
| `rumoca-core`     | Global shared primitives               | Common diagnostics, IDs, and utilities used across many crates                          | minimal/shared deps |
| `rumoca-ir-*`     | Intermediate representations           | Canonical data models exchanged between phases and runtimes                             | `core`             |
| `rumoca-phase-*`  | Compiler pipeline stages               | One transform/analysis stage per crate (parse -> resolve -> typecheck -> ... -> codegen) | `ir-*`, `eval-*` |
| `rumoca-eval-*`   | Expression evaluation infrastructure   | Compile-time eval, runtime interpreter/AD eval, and lowered/compiled kernel backends    | `ir-*`             |
| `rumoca-sim-*`    | Simulation backend stack               | Shared runtime policies/contracts plus concrete solver backend implementations            | `ir-dae`, `eval-*`, `phase-structural` |
| `rumoca-tool-*`   | Developer/editor tooling               | Formatter, linter, LSP, and developer automation tooling                                | `session`, `rumoca` |
| `rumoca-bind-*`   | Language/runtime bindings              | WASM and Python APIs over compiler/session functionality                                 | `rumoca`, `session`, `phase-codegen` |
| `rumoca-session`  | Multi-file compile session layer       | Shared orchestration API for CLI, LSP, and bindings                                     | `phase-*`, `ir-*`  |
| `rumoca-contracts`| Spec/contract verification             | Contract registry/execution/reporting for compiler compliance testing                    | `rumoca`, `session` |
| `rumoca-test-*`   | Integration/parity test harnesses      | Large external-model and end-to-end quality suites (not runtime libraries)              | `rumoca`, `sim-*`  |

### Crate Family Contracts

Use these rules when deciding where new code belongs.

| Family | Should Own | Should Not Own | Typical Input -> Output |
| ------ | ---------- | -------------- | ----------------------- |
| `rumoca-ir-*` | Data structures for compiler/runtime boundaries; serialization-stable model shapes | Evaluation logic, solver logic, pipeline orchestration | Parsed/derived model facts -> canonical IR objects |
| `rumoca-eval-*` | Expression evaluation machinery (const/runtime/AD/compiled kernels), evaluator layouts and lowerings | Full-model transforms, solver orchestration, backend policy | IR expressions + runtime vectors -> scalar/row values |
| `rumoca-phase-*` | One compiler transform or analysis stage with clear stage contract | Runtime integration loops, backend-specific solver code | Previous stage IR -> next stage IR/diagnostics |
| `rumoca-sim-*` | Simulation orchestration and backend implementations; IC solving and runtime schedule handling | Frontend parsing/resolution logic; broad codegen concerns | Prepared DAE + options -> time series results |
| `rumoca-tool-*` | Developer/editor workflows (fmt/lint/lsp/dev commands) | Core compiler semantics or solver kernels | Files/session state -> diagnostics/edits/tool outputs |
| `rumoca-bind-*` | Host-language or runtime bindings over existing APIs | New compiler/simulation semantics unique to binding | Foreign calls -> mapped Rumoca APIs/results |
| `rumoca-session` | Multi-file/project orchestration and best-effort compile flows | Individual phase internals, solver internals | Project/file set -> compiled artifacts + reports |
| `rumoca-contracts` | Spec/contract definitions, runners, and compliance assertions | Production solver/compiler execution paths | Compiler/runtime behavior -> contract pass/fail evidence |

#### What Is an `eval` Crate?

`eval` crates are for **expression-level execution**, not model-level orchestration.

- `rumoca-eval-ast`: AST-level evaluation contracts and lookup surfaces.
- `rumoca-eval-flat`: flat IR constant and runtime expression evaluation.
- `rumoca-eval-dae`: DAE-level lowering/compiled kernels for residual/Jacobian/root/expression rows.

If code chooses solver fallbacks, schedules events, mutates DAE structure, or manages full simulation loops, it belongs in `sim-*` (or `phase-*` for compile-time transforms), not `eval-*`.

### Catalog Category Legend

| Category                         | What It Means |
| -------------------------------- | ------------- |
| Public Entry Points and Tooling  | End-user/developer-facing crates (CLI, APIs, tools, bindings). |
| IR and Shared Foundations        | Canonical IRs and foundational helpers reused by multiple layers. |
| Compiler Phases                  | Sequential transformation/analysis stages in the compile pipeline. |
| Simulation and Quality           | Simulation runtime stack and quality/compliance harness crates. |

### Public Entry Points and Tooling

| Crate                | Key Features                                                                                                                            |
| -------------------- | --------------------------------------------------------------------------------------------------------------------------------------- |
| `rumoca`             | Primary compiler crate and end-user CLI (`check/compile/simulate/fmt/lint/project`) plus compiler API (`Compiler`, `CompilationResult`) |
| `rumoca-session`     | Unified multi-file session API, parallel parse/compile helpers, best-effort compile reports                                             |
| `rumoca-tool-fmt`    | Modelica formatter engine used by `rumoca fmt` and dev tooling                                                                          |
| `rumoca-tool-lint`   | Modelica lint engine (library API) with configurable rules and severity levels                                                          |
| `rumoca-tool-lsp`    | Language server (`rumoca-lsp`) with diagnostics, completion, hover, symbols, formatting, code actions                                   |
| `rumoca-tool-dev`    | Cross-platform developer workflows (`rum`): hooks, checks, release, WASM, VSCode, Python                                                |
| `rumoca-bind-wasm`   | WASM bindings for parse/lint/check/compile and editor workflows                                                                         |
| `rumoca-bind-python` | Python bindings for parse/lint/check/compile and template code generation                                                               |

### IR and Shared Foundations

| Crate                 | Key Features                                                                    |
| --------------------- | ------------------------------------------------------------------------------- |
| `rumoca-core`         | Shared types, IDs, diagnostics utilities, MSL cache path resolution             |
| `rumoca-ir-ast`       | Class-tree IR structures for parsed/resolved/typed model representation         |
| `rumoca-ir-flat`      | Flat model IR with globally unique variables/equations                          |
| `rumoca-ir-dae`       | Canonical hybrid DAE IR (continuous + discrete equations)                       |
| `rumoca-eval-ast`    | AST-level evaluation contracts and lookup interfaces                             |
| `rumoca-eval-flat`   | Flat IR constant/runtime evaluator with `SimFloat` and dual-number AD support   |
| `rumoca-eval-dae`    | DAE-level lowered compiled kernels for residual/Jacobian/root/expression evaluation |

### Compiler Phases

| Crate                      | Key Features                                                                   |
| -------------------------- | ------------------------------------------------------------------------------ |
| `rumoca-phase-parse`       | Parol-based Modelica grammar parser                                            |
| `rumoca-phase-resolve`     | Scope graph and cross-reference resolution                                     |
| `rumoca-phase-typecheck`   | Type/variability/causality checks and dimension inference                      |
| `rumoca-phase-instantiate` | Model instantiation and modification propagation                               |
| `rumoca-phase-flatten`     | Connection equation generation, algorithm handling, flat equation construction |
| `rumoca-phase-dae`         | Flat-to-DAE transformation with balance-oriented variable/equation accounting  |
| `rumoca-phase-structural`  | Structural analysis, BLT decomposition, IC plan creation                       |
| `rumoca-phase-codegen`     | Minijinja-based template rendering for code and model outputs                  |

### Simulation and Quality

| Crate                | Key Features                                                                                  |
| -------------------- | --------------------------------------------------------------------------------------------- |
| `rumoca-sim-core`    | BLT-guided IC solver blocks (direct/newton/torn/coupled paths)                                |
| `rumoca-sim-diffsol` | DAE runtime integration, solver fallbacks, timeout budgeting, diagnostics/introspection hooks |
| `rumoca-sim-rk45`    | Thin explicit RK45 backend for causal ODE forms (`x_dot = f(x,u,p,t)`)                        |
| `rumoca-contracts`   | MLS contract registry, execution, and compliance reporting framework                          |
| `rumoca-test-msl`    | MSL integration/parity test harness and model corpus quality metrics                          |

## Code Generation Targets

Built-in templates in `rumoca-phase-codegen` include:

- `CASADI_SX`
- `CASADI_MX`
- `CYECCA`
- `JULIA_MTK`
- `JAX`
- `C_CODE`
- `ONNX`
- `DAE_MODELICA`
- `FLAT_MODELICA`

You can use built-in template constants or provide custom template files.

## Simulation Quality and Robustness

The simulator stack is designed to improve result quality and avoid fragile shortcuts.

Key capabilities:

- Exact Jacobian-vector and mass-term evaluation via AD (`Dual`/`SimFloat` path)
- Structured DAE preparation before runtime integration, including:
  - derivative expansion and alias cleanup
  - structural index-reduction pass for missing derivative rows
  - promotion/demotion of state/algebraic variables based on equation structure
  - orphan variable pinning for determinism
- Initial condition solving using structural IC plans, with Newton fallback when needed
- Multi-method integration fallback sequence (BDF, TR-BDF2, ESDIRK34 with startup profiles)
- Wall-clock timeout budgets enforced across setup, IC, and integration stages
- Introspection/trace hooks for deep simulation debugging

Useful simulation/debug environment flags:

- `RUMOCA_SIM_TRACE=1`
- `RUMOCA_SIM_INTROSPECT=1`
- `RUMOCA_SIM_INTROSPECT_EQ_LIMIT=<N>`
- `RUMOCA_DEBUG=1`

## MSL Testing and Caching

MSL workflows target Modelica Standard Library `v4.1.0`.

Cache behavior:

- default cache: `<workspace>/target/msl`
- override: `RUMOCA_MSL_CACHE_DIR=/abs/or/relative/path`

Run full MSL pipeline tests (release mode is required for practical runtime):

```bash
cargo test --release --package rumoca-test-msl --test msl_tests -- --ignored --nocapture
```

Simulation subset controls for faster iteration:

- `RUMOCA_MSL_SIM_MATCH=<comma-separated substrings>`
- `RUMOCA_MSL_SIM_LIMIT=<N>`
- `RUMOCA_MSL_SIM_SET=short|long|full` (default: `short`)
- `RUMOCA_MSL_SIM_SET_LIMIT=<N>` (default: `180`, used by `short`/`long`)
- `RUMOCA_MSL_SIM_TARGETS_FILE=<json>` (overrides default committed target set)
  `RUMOCA_MSL_SIM_SET` is applied within the selected target list; it does not
  expand beyond that list.

Only explicit example models are simulated in the main MSL simulation sweep:

- `Modelica.*.Examples.*`

Default compile/balance/simulation scope is the committed 180-model explicit
example target file:
`crates/rumoca-test-msl/tests/msl_tests/msl_simulation_targets_180.json`.
Use `RUMOCA_MSL_SIM_TARGETS_FILE=<json>` to run an alternate or full model list.

Single-model OMC vs Rumoca overlay plot (regenerates both traces on each run):

```bash
cargo run --release --package rumoca-tool-dev --bin rumoca-msl-tools -- \
  plot-compare --model Modelica.Blocks.Examples.PID_Controller
```

Defaults:

- rumoca trace: `target/msl/results/sim_traces/rumoca/<model>.json`
- OMC trace: `target/msl/results/sim_traces/omc/<model>.json`
- output HTML: `target/msl/results/sim_trace_plots/<model>.html`

## MSL Quality Scoreboard

Canonical gate baseline (committed):

- [`crates/rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json`](crates/rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json)
  - includes minimal compile/balance/simulation counts plus OMC parity distributions (runtime speedup ratio + trace-accuracy stats).

Ephemeral run artifacts (regenerated per run in cache):

- `target/msl/results/msl_results.json`
- `target/msl/results/omc_reference.json` (run with `--model-timeout-seconds 30`)
- `target/msl/results/omc_simulation_reference.json`
- `target/msl/results/sim_trace_comparison.json`
- `target/msl/results/msl_quality_current.json` (current run snapshot used for baseline promotion)

The simulation target set used for scoreboard runs:

- `crates/rumoca-test-msl/tests/msl_tests/msl_simulation_targets_180.json`

Promote latest run to committed baseline after review:

```bash
cargo run --release --package rumoca-tool-dev --bin rumoca-msl-tools -- \
  promote-quality-baseline
```

## PR Requirements (SPEC_0025)

For compiler-affecting PRs, compare MSL results to this baseline and report:

- compiled model count delta
- compilation rate on simulatable models delta (`compiled/simulatable`)
- non-simulatable non-partial model count delta
- balanced model count delta
- initial-balance OK model count delta
- initial-balance deficit model count delta
- compile phase time delta
- both-balanced agreement delta
- balanced-but-eq/var-count-differs-vs-OMC count delta
- rumoca-unbalanced-vs-OMC-balanced count delta
- rumoca-failed-vs-OMC-succeeded count delta

No regressions are allowed unless explicitly justified and approved. Baseline updates are explicit: run tests, inspect `target/msl/results/msl_quality_current.json`, then promote with `rumoca-msl-tools promote-quality-baseline` (from `rumoca-tool-dev`).

Related specs:

- `spec/SPEC_0021_CODE_COMPLEXITY.md`
- `spec/SPEC_0025_PR_REVIEW_PROCESS.md`
- `spec/SPEC_0030_COVERAGE_TRIM_PROCESS.md`

## Git Hooks

Install repository-managed hooks:

```bash
cargo run --bin rum -- install-git-hooks
```

Current `pre-commit` checks (fast path):

- `rum check-rust-file-lines` (staged files, SPEC_0021 guard)
- `cargo fmt --all -- --check`
- `cargo clippy` on changed crates (or workspace when needed)
- `cargo doc` with `RUSTDOCFLAGS="-D warnings"` on changed crates (or workspace when needed)

Current `pre-push` checks (CI-parity, excluding slow MSL/coverage jobs):

- `rum check-rust-file-lines --all-files`
- `rum ci-parity` (fmt + clippy + rustdoc + workspace tests)

Run the same CI-parity gate manually:

```bash
cargo run --bin rum -- ci-parity
```

## Coverage Workflow

Standardized workspace coverage artifacts are generated under:

- `target/llvm-cov/`

Generate unified workspace coverage from tests:

```bash
cargo run --bin rum -- coverage
```

Include ignored/slow suites (for example ignored MSL tests):

```bash
cargo run --bin rum -- coverage --include-ignored
```

Generate workspace trim inventory report + candidate list from llvm-cov artifacts:

```bash
cargo run --bin rum -- coverage-report
```

Enforce candidate-growth guardrails against committed baseline:

```bash
cargo run --bin rum -- coverage-gate
```

Promote current metrics as the new baseline (explicit update step):

```bash
cargo run --bin rum -- coverage-gate --promote-baseline
```

Unified artifacts produced:

- `target/llvm-cov/workspace-full.json`
- `target/llvm-cov/workspace-summary.json`

Coverage inventory artifacts:

- `target/llvm-cov/coverage-trim-report.md`
- `target/llvm-cov/trim-candidates.json`
- `target/llvm-cov/coverage-gate.md` (baseline/current diff used by the gate)

Committed coverage-trim gate baseline:

- `crates/rumoca-tool-dev/coverage/trim-gate-baseline.json`
  - includes workspace line coverage metrics: `workspace_line_coverage_percent`, `workspace_lines_covered`, `workspace_lines_total`

Current committed workspace line coverage (from trim baseline):

- `75.57%` (`97,258 / 128,706` lines)

`trim-candidates.json` includes both:

- `triage_label` (`dead_likely`, `rare_path_keep`, `single_use_helper_keep`, `needs_targeted_test`, `public_api_review`)
- `owner_decision` (`delete_candidate`, `keep_document_rare_path`, `keep_single_use_helper`, `keep_add_targeted_test`, `keep_public_api_review`)
- callsite stats:
  - `callsites_same_file`
  - `callsites_workspace`
  - `callsites_other_crates`

Command runbook:

- `target/llvm-cov/coverage-commands.txt`

Coverage trim SOP (baseline triage/promotion/rollback):

- `spec/SPEC_0030_COVERAGE_TRIM_PROCESS.md`

## VS Code Extension

The VS Code extension is available as **Rumoca Modelica** in the marketplace and includes a bundled `rumoca-lsp` server.

- Extension docs: `editors/vscode/README.md`
- Marketplace: https://marketplace.visualstudio.com/items?itemName=JamesGoppert.rumoca-modelica

## Contributing

Contributions are welcome.

Project specifications:

- [Specifications folder](spec/)

For compiler-affecting changes, follow:

- `spec/SPEC_0025_PR_REVIEW_PROCESS.md`
- `spec/README.md`

Install repo hooks before opening PRs:

- `cargo run --bin rum -- install-git-hooks`

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
