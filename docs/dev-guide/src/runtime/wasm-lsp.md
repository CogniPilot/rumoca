# WASM, LSP, and Editors

The same compiler serves three editor surfaces: the `rumoca-lsp` language
server (native, bundled with the VS Code extension), the WASM bindings
(browser playground and the live examples in these books), and the CLI's
terminal diagnostics. They share `rumoca-tool-lsp` for language smarts, so
completion and diagnostics behave identically everywhere.

## Crates and Directories

| Location | Role |
|---|---|
| `rumoca-tool-lsp` | Language-server logic: diagnostics, completion, hover, semantic tokens, definitions, code actions |
| `rumoca-tool-fmt`, `rumoca-tool-lint` | Formatter and linter (CLI + LSP + WASM) |
| `rumoca-bind-wasm` | `wasm-bindgen` API: compile, simulate, steppers, source-root management, LSP functions |
| `rumoca-bind-python` | Python bindings (`pip install rumoca`) |
| `editors/vscode` | VS Code extension (TypeScript) |
| `editors/wasm` | Browser playground (Monaco workbench over the WASM package) |
| `docs/user-guide/live/` | The books' live-example runner (mini Monaco editors over the same WASM package) |

## The WASM API Surface

`rumoca-bind-wasm` exposes the pipeline to JavaScript:

- `compile(source, model)` → DAE JSON; `render_target(...)` renders a
  DAE-level target (the books' **Show DAE** uses the `dae-modelica`
  target).
- `simulate_model(source, model, t_end, dt, solver)` → result payload with
  `names`/`allData`/`nStates` plus request/timing metadata. Passing
  `t_end = 0`, `dt = 0`, `solver = ""` defers to the model's `experiment`
  annotation.
- `WasmStepper` — step-at-a-time simulation with `set_input`/`get` for
  interactive use.
- `lsp_diagnostics` / `lsp_completion` / `lsp_hover` /
  `lsp_semantic_tokens` … — thin wrappers over `rumoca-tool-lsp` used by
  the playground workers and the books' editors.
- Source-root management (`load_source_roots`, parsed-document caches,
  bundled archives) so browser sessions can host package trees.

## Build and Test

```bash
cargo xtask wasm build   # wasm-pack build into pkg/<profile>/
cargo xtask wasm test    # CI gate: build + browser smoke tests (Playwright)
cargo xtask vscode test  # VS Code extension gate
```

The Pages deployment copies the WASM package, the playground, and both
books into one artifact — see [Docs and Pages](../tooling/docs-and-pages.md).

## One Language Definition

Monaco's Modelica language definition (tokenizer, comments, brackets) lives
in `editors/wasm/src/modules/modelica_language.js` and is imported by both
the playground and the books' live runner. Edit it there only — the books
load it dynamically from the deployed site layout.
