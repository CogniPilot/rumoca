# rumoca-session

Multi-file compilation session orchestrator for Rumoca.

## Role in Rumoca
Coordinates the phase pipeline and document state for CLI, LSP, WASM, bindings, and tests.

## Public Surface
- Core session types: `Session`, `SessionConfig`, `CompilationResult`, `PhaseResult`, `CompilationSummary`, `BestEffortCompilationReport`.
- Main workflows: `Session::add_document`, `Session::compile_model`, `Session::compile_model_phases`, parallel compile helpers.
- Re-exported IR types used by consumers: `ClassTree`, `TypedTree`, `Model`, `Dae`.

## Inputs
- Source documents, session config, and selected model names.

## Outputs
- Compiled artifacts plus structured phase diagnostics/timing summaries.

## Design Constraints
- Centralize pipeline ordering/failure contracts.
- Keep phase-specific transforms inside phase crates.
- Preserve explicit, debuggable phase boundaries.
