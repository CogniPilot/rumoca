# Changelog

High-level release summary by `0.x` line. Patch releases are rolled up into their parent series.

## 0.10.x

- **Accepted element and slice connections of compact connector arrays.** `connect(gate.x[1], a)` on a simple-connector array `RealInput x[2]` (and the same shape on `flow` arrays) previously failed flattening with `EF038` ("partial connectivity of a compact array is not representable"). Flat's declaration-wide `connected` flag is replaced by a per-element connected domain: the connection equations cover exactly the selected elements, and every untouched `flow` element still receives its MLS §9.2 `= 0` equation, so a partially connected array is neither over- nor under-constrained. Interface coverage across scopes is decided per element, and a flow element summed by two connection sets in one scope is now refused instead of double counted. `--emit flat-json` renders `connected` as the list of connected selections (`[]` when unconnected) instead of a Boolean. Element and slice connections of compact `stream` arrays remain refused.
- **Added `EP004` for an iterator declared without a range.** `for i loop`, `{e for i}`, and `sum(e for i)` all omit the range MLS 3.7 §11.2.2.1 would deduce from the dimensions the iterator subscripts. Rumoca does not implement that inference. One parser-owned converter now serves for-equations, for-statements, comprehensions, and reduction arguments, and refuses the omission with `EP004` at the source boundary; previously the for-equation and for-statement paths reported the generic `EP001`, while the comprehension and reduction paths were not checked at all and admitted an iterator with no range. Explicit ranges are unaffected in every one of those syntaxes, and explicit-range whole-array assignment stays legal: the whole-array restriction in that MLS paragraph is conditioned on an omitted range and is not applied here. The retired `ER121` has no single successor: `EP004` owns the refusal in recognized source, `ER129` owns the refusal of a forged or deserialized syntax tree, and the whole-array rule itself is still unowned.
- **Retired `c-ode`, `embedded-c-galec`, and `galec-production` permanently.** The old spellings have no aliases. GALEC never emits C: the future deployable eFMI product is the `efmu` target, one `SolveAlgorithmProduct` retaining its checked `AlgorithmCodePackage` and correlated `SolveAlgorithmBlock`. Each output file borrows the appropriate view, and Production Code C renders only from the Solve block. For general C today, use `fmi3` (FMI 3.0 ME+CS). The `galec` Algorithm Code target is unchanged.
- **Changed `--emit solve-json`**: the solver-IR dump no longer carries `rounding`. Every `"rounding": "nearest_ties_to_even"` entry is gone, from both `SolveScalarType::Real` and the arithmetic profile, and nothing else about the dump changed: deleting exactly those entries from a 0.9.x dump reproduces the new one byte for byte. The field named the only mode every backend has ever used, so it discriminated nothing; rounding returns as the SPEC_0047 §4.3 contract, together with the operations that can differ under it. Generated code is unaffected: `--target` output is byte-identical.

## 0.9.x

- Added a new SymForce codegen backend with native automatic-differentiation support.
- Rebuilt the docs as a Rumoca User Guide and a Rumoca Dev Guide, with live interactive Monaco examples and visualizations in the browser.
- Made the Python API explicit (`rumoca.compile` / `rumoca.compile_file`) and refreshed the notebook and SymPy templates.
- Improved MSL correctness and performance, including the SwitchedRLC fix, binary sim-worker IPC, per-model compile isolation, and deterministic v4.1.0 staging.
- Sharpened the editor and LSP experience with separate interactive/strict work lanes, binary library caching, and cleaner import diagnostics.
- Restructured the developer CLI around target-first `cargo xtask` commands and moved CI onto the prebuilt dev container image.
- Added the eFMI/GALEC Algorithm Code export path (`--target galec`) and dedicated GALEC (`.alg`) language server. Its former embedded-C route was retired in 0.10; Algorithm Code never authorizes C/H.
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
