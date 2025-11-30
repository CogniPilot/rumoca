# Rumoca Modelica

A VS Code extension providing language support for [Modelica](https://modelica.org/) using the [rumoca](https://github.com/jgoppert/rumoca) compiler.

## Features

- **Syntax highlighting** for Modelica files (`.mo`)
- **Real-time diagnostics** - errors and warnings as you type
- **Autocomplete** for Modelica keywords and built-in functions
- **Hover information** for keywords and types
- **Go to definition** for variables and classes

## Requirements

This extension requires the `rumoca-lsp` language server to be installed and available in your PATH.

### Installing rumoca-lsp

```bash
cargo install --git https://github.com/jgoppert/rumoca --features lsp
```

Make sure `~/.cargo/bin` is in your PATH.

## Installation

### From VSIX file

1. Download the `.vsix` file
2. In VS Code, open the Command Palette (`Ctrl+Shift+P` / `Cmd+Shift+P`)
3. Run "Extensions: Install from VSIX..."
4. Select the downloaded `.vsix` file

### From source

```bash
cd editors/vscode
npm install
npm run compile
npx @vscode/vsce package
code --install-extension rumoca-modelica-*.vsix
```

## Configuration

| Setting | Description | Default |
|---------|-------------|---------|
| `rumoca.serverPath` | Path to the `rumoca-lsp` executable | `""` (searches PATH) |
| `rumoca.trace.server` | Traces communication with the language server | `"off"` |

## License

Apache-2.0
