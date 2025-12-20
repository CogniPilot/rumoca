# Rumoca Modelica

A VS Code extension providing language support for [Modelica](https://modelica.org/) using the [rumoca](https://github.com/cognipilot/rumoca) compiler.

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

**From VSIX file:**

1. Download the `.vsix` file for your platform from [GitHub Releases](https://github.com/cognipilot/rumoca/releases)
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
# From crates.io
cargo install rumoca

# Or from source
git clone https://github.com/cognipilot/rumoca.git
cd rumoca
cargo install --path .
```

## Configuration

| Setting | Description | Default |
|---------|-------------|---------|
| `rumoca.serverPath` | Path to a custom `rumoca-lsp` executable | `""` (auto-detect) |
| `rumoca.useSystemServer` | Use system-installed `rumoca-lsp` instead of bundled binary | `false` |
| `rumoca.modelicaPath` | List of directories containing Modelica libraries (e.g., MSL) | `[]` |
| `rumoca.trace.server` | Traces communication with the language server | `"off"` |
| `rumoca.debug` | Enable debug logging for the extension and language server | `false` |

## Configuring Library Paths

To use external Modelica libraries (like the Modelica Standard Library), configure the `rumoca.modelicaPath` setting with the directories containing your libraries:

```json
{
  "rumoca.modelicaPath": [
    "/path/to/ModelicaStandardLibrary",
    "/path/to/other/library"
  ]
}
```

These paths are added to the `MODELICAPATH` used for import resolution. Paths configured here take priority over the `MODELICAPATH` environment variable.

**Example workspace configuration** (`.vscode/settings.json`):

```json
{
  "rumoca.modelicaPath": [
    "${workspaceFolder}/../ModelicaStandardLibrary"
  ]
}
```

**Note:** The `MODELICAPATH` environment variable is also supported. If set before starting VS Code, those directories will be searched in addition to paths configured in settings.

## Troubleshooting

**Extension shows "Using system-installed rumoca-lsp" warning:**

This means the bundled binary wasn't found (possibly a platform mismatch). You can:
1. Set `rumoca.useSystemServer` to `true` to suppress the warning
2. Install `rumoca-lsp` manually (see above)

**Extension can't find rumoca-lsp:**

1. The extension will prompt you to install via cargo
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
# Build the LSP server
cargo build --release

# Build the extension
cd editors/vscode
npm install
npm run compile
npx @vscode/vsce package
code --install-extension rumoca-modelica-*.vsix
```

## License

Apache-2.0
