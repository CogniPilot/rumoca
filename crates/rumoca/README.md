# rumoca

Top-level Rumoca compiler crate and primary CLI/API entry point.

## Role in Rumoca
`rumoca` is the user-facing integration crate. It wires session orchestration, simulation backends, code generation, and tool flows into one stable CLI and library API.

## Public Surface
- User binary: `rumoca`.
- Key library API: `Compiler`, `CompilationResult`, `CompilerError`.
- Re-exported integration API used by other crates/tools: `Session`, `SessionConfig`, and core IR result types.

## Inputs
- Modelica source files (single-file or multi-file projects).
- Compile/sim/codegen options from CLI flags or API configuration.

## Outputs
- Compilation artifacts (AST/flat/DAE views via session APIs).
- Diagnostics, simulation results, and generated code.

## Design Constraints
- Orchestrate existing crates; do not reimplement phase/back-end internals.
- Keep CLI/API behavior stable across internal refactors.
- Preserve backend abstraction (`diffsol`, `rk45`) through shared options.
