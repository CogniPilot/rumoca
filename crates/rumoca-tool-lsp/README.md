# rumoca-tool-lsp

Language Server Protocol implementation for Modelica using Rumoca services.

## Role in Rumoca
Handles IDE/editor protocol requests by composing session, lint, and formatter crates.

## Public Surface
- User/developer binary: `rumoca-lsp`.
- Server API (feature-gated): `run_server`, `ModelicaLanguageServer`.
- Re-exported handler APIs used by WASM/tests: diagnostics, completion, hover, symbols, formatting, code actions, semantic tokens, rename/references.

## Inputs
- LSP requests, document lifecycle events, and workspace source snapshots.

## Outputs
- LSP responses: diagnostics, edits, symbols, navigation, semantic tokens, and actions.

## Design Constraints
- Keep protocol handling separate from compiler phase implementations.
- Reuse shared parser/session/lint/fmt behavior.
- Support both native server mode and WASM-compatible handler mode.
