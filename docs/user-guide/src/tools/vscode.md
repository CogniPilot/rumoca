# VS Code Extension

The **Rumoca Modelica** extension provides language support (diagnostics,
completion, hover, semantic highlighting), simulation commands, scenario
settings, and result viewers — all inside VS Code.

Install it from the
[marketplace](https://marketplace.visualstudio.com/items?itemName=JamesGoppert.rumoca-modelica).
It bundles a `rumoca-lsp` language server, so no separate install is needed.
To use your own server build instead (for compiler development), enable
`rumoca.useSystemServer` and put `rumoca-lsp` on `PATH`.

## Modelica Source Roots

Modelica package paths for a workspace are configured with
`rumoca-workspace.toml`.

Place the file at the workspace root, or in a subdirectory when the roots only
apply below that directory:

```toml
source_roots = [
  "../target/msl/ModelicaStandardLibrary-4.1.0",
  "../target/cmm/CMM-a642c381",
]
```

The Rumoca repository examples include `examples/rumoca-workspace.toml`. Run
`cargo xtask repo modelica-deps ensure` first so those target directories exist.

## Settings Panel

Open Rumoca settings from the toolbar or command palette
(**Open Rumoca Settings**). Shared package roots belong in
`rumoca-workspace.toml`; **Scenario Source Root Paths** edits the active
`rumoca-scenario.toml` for paths that only apply to that one configured run.

The picker stores workspace-relative paths whenever the selected folder is
inside the workspace.

## Running Models

Run actions operate on `rumoca-scenario.toml` scenario files. The extension contributes
`rumoca-scenario.toml` as a TOML language extension, so normal TOML editor features
attach while Rumoca still activates for `rumoca-scenario.toml` and `.mo` files. Use
**Create Rumoca Scenario** to generate a `rumoca-scenario.toml` for a model; the
runnable source of truth is always the scenario file.

For interactive examples, open the `rumoca-scenario.toml` scenario, then use Play. For
batch simulation, the results panel opens inside VS Code.

| Command | Purpose |
|---|---|
| Create Rumoca Scenario | Generate a `rumoca-scenario.toml` next to a model |
| Run Current Rumoca Scenario | Compile and simulate the active scenario |
| Open Scenario Settings | Edit the active scenario's settings |
| Open Rumoca Settings | Extension settings menu |
| Open Rumoca User Guide | This book |
| Open Rumoca Dev Guide | The contributor book |
| Toggle / Expand All / Collapse All Annotation Expansion | Control inline annotation rendering |

## Diagnostics

Compiler and simulation diagnostics are reported to the Problems panel when
a source span is available. CLI output uses rich terminal diagnostics; VS
Code diagnostics use the raw file/range information without terminal
wrappers.

## Documentation Access

The user guide and the contributor guide are available directly from the
command palette (**Open Rumoca User Guide** / **Open Rumoca Dev Guide
Guide**), so you do not need to hunt for URLs. The same live-example pages
work in any browser.
