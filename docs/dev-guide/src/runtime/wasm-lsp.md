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
| `rumoca-bind-wasm` | `wasm-bindgen` API: compile, simulate, simulation sessions, source-root management, LSP functions |
| `rumoca-bind-python` | Python bindings (`pip install rumoca`) |
| `packages/rumoca` | npm/WASM package builder; generated packages land under `packages/rumoca/dist/` |
| `packages/rumoca-web` | Hand-written browser runtime, WebGPU/diffsol drivers, visualization code, and vendored web dependencies |
| `packages/vscode` | VS Code extension (TypeScript) |
| `packages/playground` | Browser playground (Monaco workbench over the WASM package) |
| `docs/user-guide/live/` | The books' live-example harness (mini Monaco editors over the same WASM package) |

## The WASM API Surface

`rumoca-bind-wasm` exposes the pipeline to JavaScript:

- `compile(source, model)` → DAE JSON; `render_target(...)` renders a
  DAE-level target (the books' **Show DAE** uses the `dae-modelica`
  target).
- `simulate_model(source, model, t_end, dt, solver, parameter_overrides_json)`
  → result payload with `names`/`allData`/`nStates` plus request/timing
  metadata. Passing `t_end = 0`, `dt = 0`, `solver = ""` defers to the
  model's `experiment` annotation. `parameter_overrides_json` is a JSON object
  whose keys are tunable parameter names and whose values are finite numbers.
- `model_parameter_metadata(source, model_name)` → tunable parameter metadata used
  by the shared scenario GUI before writing `[parameters]` overrides.
- `WasmSimulationSession` — user-terminated interactive simulation with
  `withInteractiveOptions`, `set_input`, `advance_to`, and `get`; the session
  extends its finite solver horizon as it runs.
- `lsp_diagnostics` / `lsp_completion` / `lsp_hover` /
  `lsp_semantic_tokens` … — thin wrappers over `rumoca-tool-lsp` used by
  the playground workers and the books' editors.
- Source-root management (`load_source_roots`, parsed-document caches,
  bundled archives) so browser sessions can host package trees.

## Build and Test

```bash
cargo xtask playground build  # wasm-pack build into packages/rumoca/dist/<profile>/
cargo xtask playground test   # CI gate: build + browser smoke tests (Playwright)
cargo xtask vscode test       # VS Code extension gate
cargo xtask docs serve        # local books with live examples and WASM package
```

The Pages deployment copies the WASM package, the playground, and both
books into one artifact — see [Docs and Pages](../tooling/docs-and-pages.md).

## Offline Fixed Native Guest Builds

The native fixed-guest builder captures its first-party source, root Cargo
manifest, lockfile, and issuing compiler identity. It runs Cargo with
`--locked --offline`; the caller's Cargo cache must already contain the
registry packages and the exact pinned Diffsol Git source. Prepare these
workspace inputs from the exact checkout used to build the source tool:

```bash
nix develop .#default --command cargo fetch --locked
```

Use the same `CARGO_HOME` for preparation and the tool invocation. A registry-only
cache can fail while resolving the root Git patch, even when the guest itself
does not select Diffsol. Missing inputs remain an offline error.

Installing the tool through Nix does not populate the invoking user's Cargo Git
cache. A relocated executable also requires its captured compiler path and
version to remain available; the executable alone is not a portable guest-build
kit. The qualified guest build still needs its documented target, compiler, and
native-tool prerequisites. Offline metadata checks do not qualify the full build.

## One Language Definition

Monaco's Modelica language definition (tokenizer, comments, brackets) lives
in `packages/rumoca-web/runtime/modelica_language.js` and is imported by both
the playground and the books' live harness. Edit it there only — the books
load it dynamically from the deployed package layout.
