# Changelog

High-level release summary by `0.x` line. Patch releases are rolled up into their parent series.

## Unreleased

- MSL sim-worker IPC: the hot parent-to-worker DAE handoff now streams compact binary DAE payloads over stdin instead of writing large JSON temp files, removing the worst per-model serialization stall while keeping process isolation.
- Focused editor and MSL compiles: shared parsed/resolved library state is now reused while each requested model compiles on its own uncached reachable closure, which removes cross-model memory blow-ups in the parity harness and keeps wasm/VS Code compile behavior aligned.
- MSL staging: the parity harness and CI now use the official `ModelicaStandardLibrary_v4.1.0.zip` release asset and reject stale caches missing `Complex.mo`, fixing incomplete library staging.
- MSL baseline determinism: the default parity run now always starts from the committed 180-model target list and lexical order unless generated targets or prior-result scheduling are explicitly opted in via env vars.
- Developer CLI: the public surface is now target-first and explicit: `rum verify ...`, `rum vscode ...`, `rum wasm ...`, `rum python build`, `rum coverage ...`, and `rum repo ...`, with MSL maintenance under `rum repo msl ...`.
- MSL profiling: `rum repo msl flamegraph --model ... --mode compile|simulate` now wraps focused session-based flamegraphs for one model without dragging in the full parity harness.
- VS Code packaging: Linux musl release packaging is now exposed as `rum vscode package --target linux-x64|linux-arm64`, so the release workflow no longer depends on raw Cargo/musl command recipes in the docs.
- Developer bootstrap: `rum repo cli install` now installs `rum` and shell completions, with `--path` as the explicit opt-in for writing a persistent PATH update.
- VS Code: `rumoca.modelicaPath` updates now restart the language server automatically, and notebook execution/controller availability stays aligned with the active `rumoca` and `rumoca-lsp` pair.
- Editors and CI: `rum vscode test` and GitHub Actions now run the same VS Code checks, and the wasm editor library-loading path has dedicated regression coverage.

## 0.8.x

- Reframed Rumoca as a Modelica compiler and symbolic interoperability platform, not just a translator.
- Solidified the full compiler pipeline and session-oriented architecture used by the CLI, LSP, wasm editor, and tests.
- Expanded the editor story with a browser demo, stronger VS Code and wasm workflows, and better library and diagnostics handling.
- Strengthened MSL parity, balance, trace-quality, and release gating so large-library regressions are tracked more systematically.
- Moved distribution to GitHub Releases with install scripts, Python wheels, VS Code extension artifacts, and packaged wasm assets.

## 0.7.x

- Added the first LSP-based editing workflow and broadened overall language support.
- Shifted the project around Base Modelica IR export and closer integration with downstream symbolic tooling such as Cyecca.
- Added package-directory and library-path workflows, including CLI `-L`, Modelica-path support, and better MSL handling.
- Introduced the wasm target and browser-hosted editor flow, including GitHub Pages deployment and ongoing VS Code notebook/editor improvements.
- Improved formatter, autocomplete, caching, and performance as the tool moved from prototype parsing toward everyday library-backed use.

## 0.6.x

- Focused on packaging and publish hygiene for the parser-generated code and release artifacts.
- Split generated Python output into its own area and tightened the surrounding developer workflow.
- Added early VS Code workspace settings and editor-oriented repo setup.
- Refined template generation for SymPy, CasADi, and Gazebo-oriented outputs.
- Continued stabilizing examples and notebooks while making parser regeneration more explicit.

## 0.5.x

- Switched the parser stack to PAROL and expanded the supported grammar substantially.
- Added a stronger template-generation and visitor-based architecture for downstream code emission.
- Grew support for functions, `when`, `for`, equation and statement blocks, modification expressions, and broader expression handling.
- Added more model semantics such as `extends`, connect equations, causality handling, resets, event logic, and piecewise behavior.
- Built out richer examples and notebooks, including rover, quadrotor, bouncing-ball, and Gazebo-oriented flows.

## 0.4.x

- Turned the early prototype into a more installable Rust package with `cargo install` and cleaner module organization.
- Added better generated-file metadata, including template, model, and build hashes.
- Improved code generation around functions, start values, and non-differential-equation handling.
- Expanded parsing with `if` statements and equations while tightening example coverage around models like Ackermann.
- Added array support by the end of the series.

## 0.3.x

- Repositioned Rumoca from “compiler” to “translator” for Modelica-to-symbolic output workflows.
- Added flat-model and symbolic generation work for SymPy and CasADi.
- Switched templating from Tera to MiniJinja.
- Expanded parser support with arrays, array references, functions, algorithms, and richer expressions.
- Improved parser diagnostics and grew early templates and examples such as multirotor-oriented outputs.

## 0.2.x

- Clarified the project direction around Modelica as input and symbolic/CAS backends as outputs.
- Improved the CLI layout and reorganized templates.
- Strengthened CasADi generation and added early Collimator generation support.
- Expanded the README, roadmap, and install documentation so the project was easier to evaluate and try.

## 0.1.x

- Initial public prototype of Rumoca as a Rust-based Modelica frontend.
- Basic single-file CLI workflow for compiling a Modelica model.
- Early symbolic-output story centered on SymPy, with CasADi, JAX, and Collimator called out as target directions.
- First build, install, and roadmap documentation for the project.
