# Rumoca Modelica

A VS Code extension providing language support for [Modelica](https://modelica.org/) using the [rumoca](https://github.com/climamind/rumoca) compiler.

## Features

- **Syntax highlighting** for Modelica files (`.mo`)
- **Real-time diagnostics** - errors and warnings as you type
- **Autocomplete** for Modelica keywords and built-in functions
- **Hover information** for keywords and types
- **Go to definition** for variables and classes
- **Document symbols** - file outline with classes, components, equations
- **Signature help** - function parameter hints
- **Find references** - locate all uses of a symbol
- **Code folding** - collapse classes, equations, comments
- **Formatting** - auto-format Modelica code
- **Inlay hints** - inline parameter names and array dimensions
- **Semantic tokens** - enhanced syntax highlighting
- **Code lens** - reference counts and navigation
- **Document links** - clickable URLs and file paths

## Installation

**From VS Code Marketplace (recommended):**

Search for "Rumoca Modelica" in the VS Code Extensions view (`Ctrl+Shift+X` / `Cmd+Shift+X`) and install.

The extension includes a bundled `rumoca-lsp` language server, so **no additional installation is required** for most users.

For Linux release builds, the bundled `rumoca-lsp` is shipped as a `musl`-linked binary for both
`linux-x64` and `linux-arm64`. This is intentional: it reduces breakage from remote-host `glibc`
version mismatches.

**From VSIX file:**

1. Download the `.vsix` file for your platform from [GitHub Releases](https://github.com/climamind/rumoca/releases)
2. In VS Code, open the Command Palette (`Ctrl+Shift+P` / `Cmd+Shift+P`)
3. Run "Extensions: Install from VSIX..."
4. Select the downloaded `.vsix` file

## Using a Custom/System Server

If you want to use a different version of `rumoca-lsp` (e.g., a development build), you have two options:

### Option 1: Use System Server

Set `rumoca.useSystemServer` to `true` in your VS Code settings. The extension will then search for `rumoca-lsp` in your PATH or `~/.cargo/bin/`.

```json
{
  "rumoca.useSystemServer": true
}
```

### Option 2: Specify Custom Path

Set `rumoca.serverPath` to the full path of your custom `rumoca-lsp` binary:

```json
{
  "rumoca.serverPath": "/path/to/custom/rumoca-lsp"
}
```

### Installing rumoca-lsp Manually

If you need to install `rumoca-lsp` manually:

```bash
# From GitHub Releases installer
curl --proto '=https' --tlsv1.2 -LsSf https://raw.githubusercontent.com/climamind/rumoca/main/install/install.sh | bash -s -- --with-lsp

# Or from source
git clone https://github.com/climamind/rumoca.git
cd rumoca
cargo install --path crates/rumoca-tool-lsp
```

## Configuration

| Setting | Description | Default |
|---------|-------------|---------|
| `rumoca.serverPath` | Path to a custom `rumoca-lsp` executable | `""` (auto-detect) |
| `rumoca.useSystemServer` | Use system-installed `rumoca-lsp` instead of bundled binary | `false` |
| `rumoca.sourceRootPaths` | List of directories containing Modelica source roots (e.g., MSL) | `[]` |
| `rumoca.trace.server` | Traces communication with the language server | `"off"` |
| `rumoca.debug` | Enable debug logging for the extension and language server | `false` |

## Configuring Source Root Paths

To use external Modelica packages like the Modelica Standard Library, configure the `rumoca.sourceRootPaths` setting with the directories containing those source roots:

```json
{
  "rumoca.sourceRootPaths": [
    "/path/to/ModelicaStandardLibrary",
    "/path/to/other/source-root"
  ]
}
```

These paths are added to the effective `MODELICAPATH` search set used for import resolution. Paths configured here take priority over the `MODELICAPATH` environment variable.

**Example workspace configuration** (`.vscode/settings.json`):

```json
{
  "rumoca.sourceRootPaths": [
    "${workspaceFolder}/../ModelicaStandardLibrary"
  ]
}
```

**Note:** The `MODELICAPATH` environment variable is also supported. If set before starting VS Code, those directories will be searched in addition to paths configured in settings.

## Troubleshooting

**Extension shows "Using system-installed rumoca-lsp" warning:**

This means the bundled binary wasn't found or could not execute on your machine. You can:
1. Set `rumoca.useSystemServer` to `true` to suppress the warning
2. Install `rumoca-lsp` manually (see above)

**Extension shows a `glibc`/loader error for the bundled server:**

The extension now probes the bundled `rumoca-lsp` before startup and will fall back to a
system-installed server if one is available. If you still hit this case:

1. update to the latest extension release
2. install `rumoca-lsp` manually and set `rumoca.useSystemServer` to `true`
3. report the failing platform/host details if the latest bundled Linux build still does not run

**Extension can't find rumoca-lsp:**

1. Install `rumoca-lsp` with the GitHub Releases installer shown above
2. Or set `rumoca.serverPath` to the full path of your `rumoca-lsp` binary

**Debug logging:**

To see detailed logs, enable debug mode in settings:

```json
{
  "rumoca.debug": true
}
```

Then check the "Rumoca Modelica" output channel in VS Code.

## Building the Extension from Source

```bash
# one-time bootstrap from repo root
cargo run --bin rum -- repo cli install

# run the extension verification gate
rum vscode test

# build/package/install the extension locally
rum vscode build

# package a target-specific VSIX with bundled release binaries
rum vscode package --target linux-x64

# development loop with watch mode + Extension Development Host
rum vscode edit
```

`rum vscode package` is the maintainer path for release-style VSIX artifacts. On Debian/Ubuntu,
you can let it install `musl-tools` on the first Linux packaging run:

```bash
rum vscode package --target linux-x64 --install-musl-tools
```

If you need the lower-level manual steps, `rum vscode build` and `rum vscode package` wrap the
same TypeScript, Cargo, and VSIX packaging flow that lives under `editors/vscode/`.

## License

Apache-2.0
