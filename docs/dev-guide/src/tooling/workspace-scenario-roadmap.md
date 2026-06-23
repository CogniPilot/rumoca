# Workspace and Scenario Roadmap

This roadmap tracks the migration to one browser/editor model shared by VS Code,
the playground, and mdBook live examples. The policy source of truth is
`spec/SPEC_0018_TOOL_CONFIG.md`; update that spec before treating any roadmap
item below as a permanent rule.

## Target Shape

Rumoca has two user-visible concepts:

- **Workspace**: the open file tree and performance context, like a VS Code
  workspace. It owns files, folders, active documents, mounted libraries,
  parsed source-root caches, and editor session state.
- **Scenario**: one runnable TOML file, `rumoca-scenario.toml` or
  `rumoca-scenario.<profile>.toml`.
  It owns model selection, task, source roots for that run, solver settings,
  codegen settings, plots, viewer settings, input routing, and visualization
  script paths.

The GUI never owns configuration truth. It renders the selected scenario TOML
and writes back to that same scenario.

## Workspace Settings

Workspace configuration lives in visible `rumoca-workspace.toml` files. Rumoca
does not use hidden project directories for generated caches, results, or local
editor state.

`rumoca-workspace.toml` is for workspace context only:

- global and scoped source roots
- mounted libraries or package archives
- parsed source-root cache policy
- default scenario on open
- browser/book preload bundles
- editor layout defaults when they are portable

It must not contain model-specific solver, plot, codegen, or viewer settings.
Those stay in scenario files.

Nested workspace files cascade from parent to child. The nearest file may add
scoped source roots or override scalar workspace preferences. Scenario-local
`source_roots` are applied after matching workspace roots.

Effective source roots for a scenario are:

1. parent workspace roots
2. nested workspace roots
3. workspace scoped roots matching the scenario path
4. scenario `source_roots`
5. host-local overrides, only for local machine paths

## Shared Surfaces

All three hosts should use the same shared behavior:

- VS Code reads real files from disk and calls the LSP.
- The playground reads an in-memory workspace and calls the WASM worker.
- The Rust book opens a small generated workspace per live example and calls
  the same WASM/runtime helpers.

`packages/rumoca-web` owns shared browser UI and transforms:

- default scenario editor GUI for `rumoca-scenario.toml` and
  `rumoca-scenario.<profile>.toml`
- synchronized GUI/raw TOML mode switching for the same scenario file
- scenario settings GUI
- results viewer settings GUI
- shared Modelica language setup
- runtime helpers for parsed source-root caches and simulation
- host-neutral request/response shapes

Host packages only adapt transport and file access.

## Implementation Phases

Current status:

- Phase 1 is implemented for the browser command surface: playground code uses
  workspace/scenario request names, and Rust WASM export names are treated as an
  internal ABI boundary in the worker.
- Phase 2 has an initial Rust implementation for visible
  `rumoca-workspace.toml` cascading, ordered source-root merging, and LSP
  source-root loading. LSP reload paths now use the focused document/scenario
  path when loading cascading workspace config, so child workspace files and
  scoped source roots participate in completions, diagnostics, and simulation
  preparation. The same Rust workspace-config semantics are now available to
  browser hosts through a WASM/worker command that evaluates visible
  `rumoca-workspace.toml` files from an in-memory workspace map for a focused
  path. Review of that API exposure found no discrete correctness issues. The
  playground now passes visible workspace config files through scenario
  requests and uses the effective source-root command when building simulation
  settings/defaults for the active document. Inherited workspace roots stay out
  of editable scenario overrides, and simulation execution now loads the
  resolved effective source-root contents before dispatching the solver. The
  Rust book live-example runner now loads staged `rumoca-workspace.toml` files
  through the same WASM workspace-config API, and generated book scenario TOMLs
  keep inherited dependency roots out of editable scenario settings. The live
  book runner now validates that repo examples resolve effective workspace
  roots before simulation/DAE compilation, reports missing staged workspace or
  source-root cache state explicitly, and surfaces the resolved root count in
  the widget status.
- Phase 3 is implemented for the planned shared scenario surfaces: playground
  scenario settings go through the
  shared WASM scenario API instead of browser-local TOML rendering. The Rust
  book live settings path now also uses the same WASM scenario config
  round-trip API instead of a browser-local TOML parser/patcher. VS Code
  scenario creation now renders TOML through a
  shared Rust/LSP scenario config command instead of hand-built TOML strings.
  The LSP/VS Code command surface now has full scenario config load/save
  primitives for the shared scenario GUI, and open scenario files are saved
  through the VS Code editor buffer so raw TOML state cannot go stale. VS Code
  now contributes the shared scenario GUI as the default custom editor for
  `rumoca-scenario.toml` and `rumoca-scenario.<profile>.toml`, with a raw TOML
  escape hatch back to the same file. The custom editor parses the live TOML buffer so unsaved raw edits
  and parse errors are reflected immediately when switching back to the GUI, and
  scenario creation/settings commands now route scenario files into that shared
  editor instead of an older side-panel settings path. The scenario GUI refreshes
  from live text when its custom editor becomes active, keeping GUI/raw TOML
  switching synchronized without requiring an intermediate save.
  The playground now opens
  `rumoca-scenario.toml`/`rumoca-scenario.<profile>.toml` in the shared
  scenario GUI by default, uses the worker-backed full scenario config load/save
  commands, and keeps raw TOML edits synchronized against the same in-memory
  workspace file. The Rust book live settings panel now mounts the same shared
  scenario GUI document as VS Code and the playground, with the book acting only
  as a thin host for WASM load/save and raw TOML fallback. The shared GUI now
  exposes a task-aware run action, with VS Code, playground, and mdBook hosts
  all saving the scenario TOML before dispatching their existing scenario
  execution path. The shared GUI can
  now display resolved workspace source roots as read-only context while keeping
  scenario-local `source_roots` as the only editable scenario field. The live
  book Monaco editor follows mdBook theme changes and forces existing editors to
  repaint when switching Light/Rust/Coal/Navy/Ayu. The shared scenario
  GUI is now being upgraded from a generic
  TOML leaf editor into the sole typed scenario authoring surface across VS
  Code, playground, and mdBook: finite choices use dropdowns, simulation values
  use validated numeric controls, plot panels use repeated structured controls
  instead of JSON blobs, top-level sections have readable labels, summaries,
  and visible disclosure affordances, advanced input routing is hidden behind a
  summary disclosure, and normal saves validate data before writing TOML
  through the shared Rust scenario renderer. Structured signal routes now
  round-trip as typed TOML values instead of JSON strings, so input-enabled
  native and browser runs share the same signal mapping shape. Inline mdBook
  examples without a
  scenario file now synthesize an in-memory `rumoca-scenario.toml` before
  opening Settings, so the same GUI configures simulation, plot panels, and 3D
  viewer script paths. 3D plot panels expose viewer script paths as typed
  controls while script bodies stay in files or host-managed assets, and the
  browser runner now renders scenario-configured plot/viewer panels instead of
  treating sidecar JavaScript as a hardcoded viewer path. The canonical file
  names are now
  `rumoca-workspace.toml`, `rumoca-scenario.toml`,
  `rumoca-scenario.<profile>.toml`, `rumoca-result.<timestamp>.<model>.json`,
  and `rumoca-cache/`; old scenario/workspace names and hidden project-result
  directories are being removed rather than aliased. VS Code source-file toolbar
  actions now create/open a scenario file with sensible simulation defaults and
  an output directory instead of prompting for task/model/name internals up
  front. Playground editor toolbar actions now use the same shared scenario
  command path: the lightning action creates/opens a codegen scenario file,
  the run action dispatches from the active scenario task, scenario runs compile
  the configured `[model].file` rather than editor chrome text, and generated
  code respects `[codegen].output_dir`. Scenario-local source roots now use a
  structured add/remove list in the shared GUI, with host-backed browsing where
  the host can provide it, and confusing plot default knobs are hidden from the
  main form in favor of concrete plot-panel configuration. Input routing is now
  treated as a scenario-level simulation capability rather than an interactive
  scenario type: the shared GUI has an explicit input-enable switch plus typed
  local, keyboard key/integrator, gamepad axis/button/integrator,
  and model-input mapping rows, while viewer surfaces remain separate. The
  quadrotor interactive example now demonstrates that split: realtime
  simulation plus input routing in the scenario, with the external 3D scene as
  presentation rather than a distinct scenario kind. The Rust book live widget
  now has the browser side of that path as well: input-enabled/realtime
  scenarios load the shared web interactive runtime, compile a WASM stepper with
  staged workspace/source-root context, route keyboard/gamepad/local inputs into
  Modelica inputs, and run the scenario's 3D scene inline instead of falling
  back to batch plots or a native-only viewer. Interactive capture, HUD/camera
  controls, stop handling, and state reset now live in the shared web runtime so
  mdBook, VS Code, and the playground consume the same control semantics instead
  of rebuilding them per host. Simulation speed and user input are separate
  scenario concepts: `[sim].mode` controls pacing, `[input]` opts into the
  input-capable stepper/viewer path, and viewer mode only chooses presentation.
  The playground interactive-result editor now uses the same runtime failure
  reporting path as the shared runner: source-root loading, scene
  initialization, startup rendering, and animation-frame errors update the tab
  status and flow into the shared Errors panel instead of leaving `.input`
  editor tabs stuck at "compiling stepper".
- Phase 5 has a packaging/runtime baseline: `xtask` owns Rust orchestration and
  web asset freshness checks, while npm-owned package scripts may run npm,
  refresh stale web dependencies, and rebuild shared web assets before copying
  them into VS Code. The browser WASM package and GitHub Pages staging include
  the worker runtime dependencies (`rumoca_runtime.js` and
  `modelica_language.js`), and playground branding uses the shared staged brand
  asset.
- Phase 6 has started in browser code: old browser-only config and generated
  hidden side-data terminology have been removed from the playground
  surface. Browser simulations with workspace sources keep RK/auto on the
  workspace-source WASM path, while BDF syncs those workspace sources into the
  runtime session before using the lazy diffsol addon path. Playground codegen
  target selection now reads/writes `[codegen].target` through shared scenario
  commands instead of persisting a duplicate browser editor-state setting, and
  the same shared scenario command surface is now available through LSP/WASM for
  hosts that need model-scoped codegen configuration. Codegen scenario settings
  no longer save through simulation-preset commands: codegen target and
  scenario-local source roots are task-aware scenario edits, and simulate and
  codegen scenarios for the same model no longer collide in the shared scenario
  config store. VS Code/shared settings helper APIs now use scenario names, and
  the remaining browser dependency bundle generated by `packages/rumoca-web`
  has been renamed away from project terminology. Playground editor-state now
  uses an explicit session-state allowlist, so old hidden
  `sim`, codegen-template, and selected-model values are not persisted as a
  second scenario configuration store. Browser contract tests now pin removal
  of `.rum` handling and generated/project side channels. Current progress is
  no longer considered 100% because the generic scenario form exposed too much
  raw TOML shape to users; remaining work is to finish typed coverage for
  codegen and viewer/input routing, then rerun final cross-host done-criteria
  verification.

### Phase 1: Name the API

- Add `rumoca-workspace.toml` to `SPEC_0018`.
- Define workspace cascade and source-root merge semantics there.
- Define that scenario files are the only run/config truth.
- Define that opening a scenario file defaults to the shared scenario GUI, with
  a raw TOML toggle synchronized against the same file content.
- Rename browser/LSP command concepts away from project-specific config where
  possible:
  - `workspace.*` for file tree, model discovery, and source-root cache work
  - `scenario.*` for reading/writing scenario TOML and derived run settings

### Phase 2: Workspace-Driven Source Roots

- Implement parent-to-child loading for `rumoca-workspace.toml`.
- Merge the nearest workspace file with its parents for the active document or
  selected scenario.
- Use the merged workspace source roots for VS Code completion, hover,
  diagnostics, go-to-definition, and model discovery.
- Apply the same effective workspace source roots in the playground and Rust
  book through WASM.
- Rebuild or invalidate parsed source-root caches when the nearest effective
  workspace context changes.

### Phase 3: Centralize Scenario Logic

- Keep TOML parsing/rendering in Rust shared APIs (`rumoca-compile` through LSP
  and WASM bindings).
- Keep GUI rendering and host-neutral form transforms in `packages/rumoca-web`.
- Remove browser-only TOML parsers and config renderers from the playground.
- Make the playground apply writes returned by the WASM scenario API instead of
  constructing scenario text itself.
- Make the GUI and raw TOML view round-trip through the same scenario file:
  valid GUI edits render TOML; raw TOML edits reparse into the GUI; parse
  errors stay visible without losing raw text.

### Phase 4: Introduce Workspace Settings

- Add a Rust `WorkspaceConfig` parser for `rumoca-workspace.toml`.
- Support parent-to-child cascading and scoped source roots.
- Expose the same effective workspace context through LSP and WASM.
- Store generated parsed source-root caches under visible `rumoca-cache/`.

### Phase 5: Make Hosts Thin

- VS Code maps filesystem events and webview messages to shared scenario and
  workspace commands.
- VS Code contributes a custom editor for scenario files that opens the shared
  GUI by default and can toggle to raw TOML.
- The playground maps in-memory files and workers to the same commands.
- The Rust book builds a small workspace for each live example, including the
  selected scenario, model files, visualization assets, and prebuilt source-root
  caches.

### Phase 6: Delete Old Surfaces

- Remove `.rum` handling.
- Remove project-specific config concepts from browser code.
- Remove separate playground settings state that duplicates scenario TOML.
- Remove hidden generated cache/result/session directories. Persisted
  simulation results are standalone result JSON files written next to the
  scenario TOML by default, or under the scenario's configured output directory.

## Done Criteria

- A scenario that works in VS Code works unchanged in the playground and Rust
  book when the same workspace files are available.
- The settings GUI can be closed and reopened without losing fidelity because
  `rumoca-scenario.toml` remains the source of truth.
- Source-root caches are preloaded per workspace/scenario context so editing,
  autocomplete, diagnostics, and simulation startup stay responsive.
- VS Code autocomplete uses the merged nearest `rumoca-workspace.toml` context
  plus scenario-local roots, matching playground and Rust book behavior.
- Clicking a scenario opens the shared GUI by default, and toggling raw TOML
  edits the same file without drift.
- Browser, book, and VS Code tests exercise the same scenario/workspace command
  shapes instead of three separate implementations.
