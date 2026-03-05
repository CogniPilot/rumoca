# rumoca-bind-wasm

WebAssembly bindings for Rumoca compiler, simulation, and editor workflows.

## Role in Rumoca
Browser/editor-facing entry points compiled with `wasm-bindgen`. Heavy logic stays in session/simulation/tool crates.

## Public Surface
- WASM exports for compiler flows: `parse`, `lint`, `check`, `compile`, `compile_to_json`, `compile_with_libraries`.
- WASM exports for codegen: `generate_code`, `render_template`.
- WASM exports for editor/LSP helpers: `lsp_diagnostics`, `lsp_hover`, `lsp_completion`, `lsp_definition`, `lsp_document_symbols`, `lsp_code_actions`, semantic token helpers.
- WASM export for simulation: `simulate_model`.

## Inputs
- In-memory Modelica documents and tool requests from JS/TS clients.
- Optional model/simulation options from web UIs.

## Outputs
- JSON/JS-serializable diagnostics, symbols, compile artifacts, and simulation data.

## Design Constraints
- Keep WASM interface thin and deterministic.
- Preserve stable response shapes for frontend consumers.
- Avoid backend-specific policy in the WASM layer.
