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

Modelica package paths are configured with `rumoca.sourceRootPaths`.

Repository-relative paths are supported and resolved against the current VS
Code workspace root before they are sent to the language server. This makes
workspace settings safe to commit:

```json
{
  "rumoca.sourceRootPaths": [
    "target/msl/ModelicaStandardLibrary-4.1.0/Modelica 4.1.0",
    "target/cmm/CMM-v0.0.2"
  ]
}
```

The Rumoca repository includes committed settings for common open modes:

| Workspace opened in VS Code | Settings file |
|---|---|
| repository root | `.vscode/settings.json` |
| `examples/` | `examples/.vscode/settings.json` |
| `examples/interactive/quadrotor/` | `examples/interactive/quadrotor/.vscode/settings.json` |

Run `cargo xtask repo modelica-deps ensure` first so those target
directories exist.

## Settings Panel

Open Rumoca settings from the toolbar or command palette
(**Open Rumoca Settings**). The settings panel has two source-root sections:

- **Workspace Modelica Path** edits `rumoca.sourceRootPaths` at workspace
  scope. Use this for MSL, CMM, and other shared packages.
- **Scenario Source Root Paths** edits the active `rum.toml` scenario. Use
  this for paths that only apply to that one configured run.

The picker stores workspace-relative paths whenever the selected folder is
inside the workspace.

## Running Models

Run actions operate on `rum.toml` scenario files. The extension contributes
`rum.toml` as a TOML language extension, so normal TOML editor features
attach while Rumoca still activates for `rum.toml` and `.mo` files. Use
**Create Rumoca Scenario** to generate a `rum.toml` for a model; the
runnable source of truth is always the scenario file.

For interactive examples, open the `rum.toml` scenario, then use Play. For
batch simulation, the results panel opens inside VS Code.

| Command | Purpose |
|---|---|
| Create Rumoca Scenario | Generate a `rum.toml` next to a model |
| Run Current Rumoca Scenario | Compile and simulate the active scenario |
| Open Scenario Settings | Edit the active scenario's settings |
| Open Rumoca Settings | Extension settings menu |
| Open Rumoca User Guide | This book |
| Open Rumoca Internals Guide | The contributor book |
| Toggle / Expand All / Collapse All Annotation Expansion | Control inline annotation rendering |

## Diagnostics

Compiler and simulation diagnostics are reported to the Problems panel when
a source span is available. CLI output uses rich terminal diagnostics; VS
Code diagnostics use the raw file/range information without terminal
wrappers.

## Documentation Access

The user guide and the contributor guide are available directly from the
command palette (**Open Rumoca User Guide** / **Open Rumoca Internals
Guide**), so you do not need to hunt for URLs. The same live-example pages
work in any browser.
