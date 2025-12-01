# Rumoca Modelica

A VS Code extension providing language support for [Modelica](https://modelica.org/) using the [rumoca](https://github.com/jgoppert/rumoca) compiler.

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

### From VS Code Marketplace

Search for "Rumoca Modelica" in the VS Code Extensions view, or install from the [marketplace](https://marketplace.visualstudio.com/items?itemName=JamesGoppert.rumoca-modelica).

The extension includes bundled LSP binaries for all major platforms (Linux, macOS, Windows) - no additional installation required!

### From VSIX file

1. Download the `.vsix` file from [GitHub Releases](https://github.com/jgoppert/rumoca/releases)
2. In VS Code, open the Command Palette (`Ctrl+Shift+P` / `Cmd+Shift+P`)
3. Run "Extensions: Install from VSIX..."
4. Select the downloaded `.vsix` file

## Configuration

| Setting | Description | Default |
|---------|-------------|---------|
| `rumoca.serverPath` | Path to a custom `rumoca-lsp` executable (leave empty to use bundled binary) | `""` |
| `rumoca.trace.server` | Traces communication with the language server | `"off"` |

## Building from source

If you want to build the extension yourself:

```bash
# Build the LSP server
cargo build --release --features lsp

# Build the extension
cd editors/vscode
npm install
npm run compile
npx @vscode/vsce package
code --install-extension rumoca-modelica-*.vsix
```

## License

Apache-2.0
