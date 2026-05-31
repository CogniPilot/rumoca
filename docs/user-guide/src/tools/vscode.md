# VS Code Extension

The Rumoca extension provides diagnostics, completions, simulation commands,
scenario settings, and result viewers.

## Modelica Source Roots

Modelica package paths are configured with `rumoca.sourceRootPaths`.

Repository-relative paths are supported and resolved against the current VS
Code workspace root before they are sent to the language server. This makes
workspace settings safe to commit:

```json
{
  "rumoca.sourceRootPaths": [
    "target/msl/ModelicaStandardLibrary-4.1.0/Modelica 4.1.0",
    "target/cmm/CMM-v0.0.1"
  ]
}
```

The repository includes committed settings for common open modes:

| Workspace opened in VS Code | Settings file |
|---|---|
| repository root | `.vscode/settings.json` |
| `examples/` | `examples/.vscode/settings.json` |
| `examples/interactive/quadrotor/` | `examples/interactive/quadrotor/.vscode/settings.json` |

Run `cargo xtask repo modelica-deps ensure` first so those target directories exist.

## Settings Panel

Open Rumoca settings from the toolbar/command palette. The settings panel has
two source-root sections:

- **Workspace Modelica Path** edits `rumoca.sourceRootPaths` at workspace
  scope. Use this for MSL, CMM, and other shared packages.
- **Scenario Source Root Paths** edits the active `rum.toml` scenario. Use this for
  paths that only apply to that one configured run.

The picker stores workspace-relative paths whenever the selected folder is
inside the workspace.

## Running Models

Run actions operate on `rum.toml` scenario files. The extension contributes `rum.toml`
as a TOML language extension, so normal TOML editor features can attach while
Rumoca still activates for `rum.toml` and `.mo` files. Modelica files may offer a
wizard to create a `rum.toml` scenario, but the runnable source of truth is the
scenario file.

For interactive examples, open the `rum.toml` scenario, then use Play. For batch
simulation, the results panel opens inside VS Code.

## Diagnostics

Compiler and simulation diagnostics are reported to the Problems panel when a
source span is available. CLI output uses rich terminal diagnostics; VS Code
diagnostics use the raw file/range information without terminal wrappers.
