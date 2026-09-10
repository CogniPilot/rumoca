# Changelog

High-level release summary by `0.x` line. Patch releases are rolled up into their parent series.

## 0.10.x

- Export parameter-guarded rigid-body models through FMI 2/3 while preserving assertions and typed array-function execution. Recompute derived parameter bindings after legal parameter changes so invalid mass, gravity, and inertia fail validation.

- Fixed interactive startup by sharing the native/WASM library cache format, opening the viewer during the Run click, and supplying configured inputs before initialization. FixedWing selects the interactive viewer and documents keyboard capture and low-throttle arming (#320).
- Restored FMI 2 export of explicit algebraic outputs, including PID output equations (#346). FMI 2 and FMI 3 share the checked algebraic assignment kernel and refresh outputs after input, time, and state changes; unsupported implicit systems remain rejected.
- Added atomic simulation input batches with one integrator restart for a changed batch. Repeating bit-identical inputs preserves integrator history (#348).
- Run semantic diagnostics when a document opens and include correctly qualified nested models in code lenses (#335, #336). Structural reports now use the same prepared system as simulation (#350).
- Added the optional `jacobian(f(a, b), a)` source extension and `compile --emit-standard-modelica` for exporting its expansion as ordinary Modelica. Existing declarations named `jacobian` retain precedence.
- **Changed `--emit solve-json`**: the solver-IR dump no longer carries `rounding`. Every `"rounding": "nearest_ties_to_even"` entry is gone, from both `SolveScalarType::Real` and the arithmetic profile, and nothing else about the dump changed: deleting exactly those entries from a 0.9.x dump reproduces the new one byte for byte. The field named the only mode every backend has ever used, so it discriminated nothing; rounding returns as the SPEC_0047 §4.3 contract, together with the operations that can differ under it. Generated code is unaffected: `--target` output is byte-identical.
- The `galec` and `galec-production` targets now share one copy of the vendored eFMI schema tree instead of carrying a byte-identical copy each, so `galec-production/` holds no `schemas/` directory of its own. Emitted eFMU containers are unchanged, including all 46 schema files, whether the target is named as a built-in or copied out and passed to `--target <dir>`.

## 0.9.x

- Added a new SymForce codegen backend with native automatic-differentiation support.
- Rebuilt the docs as a Rumoca User Guide and a Rumoca Dev Guide, with live interactive Monaco examples and visualizations in the browser.
- Made the Python API explicit (`rumoca.compile` / `rumoca.compile_file`) and refreshed the notebook and SymPy templates.
- Improved MSL correctness and performance, including the SwitchedRLC fix, binary sim-worker IPC, per-model compile isolation, and deterministic v4.1.0 staging.
- Sharpened the editor and LSP experience with separate interactive/strict work lanes, binary library caching, and cleaner import diagnostics.
- Restructured the developer CLI around target-first `cargo xtask` commands and moved CI onto the prebuilt dev container image.
- Added an eFMI/GALEC Algorithm Code export path (`--target galec`) that emits schema-valid eFMU and eFMI Production Code containers, with a dedicated GALEC (`.alg`) language server wired into VS Code and an embedded-C equivalence harness.
- Added neural-ODE and optimization support and unified the simulation session integration across the CLI, LSP, and wasm surfaces.
- Continued improving MSL trace parity and performance with an event-driven baseline ratchet published as a release asset, and cut CI time by sharding the MSL gate behind a build-once Nix path.

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
- Built out richer examples and notebooks, including rover, bouncing-ball, and Gazebo-oriented flows.

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
