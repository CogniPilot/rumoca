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

### Step 1: Install Rust

The `rumoca-lsp` language server is written in Rust. You need to install Rust first.

**Linux / macOS:**

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

Follow the prompts and restart your terminal (or run `source ~/.cargo/env`).

**Windows:**

Download and run the installer from [rustup.rs](https://rustup.rs/).

Or use winget:

```powershell
winget install Rustlang.Rustup
```

Restart your terminal after installation.

**Verify installation:**

```bash
rustc --version
cargo --version
```

### Step 2: Add Cargo bin to PATH

Cargo installs binaries to `~/.cargo/bin` (Linux/macOS) or `%USERPROFILE%\.cargo\bin` (Windows). This should be added automatically by rustup, but verify it's in your PATH:

**Linux / macOS** (add to `~/.bashrc`, `~/.zshrc`, or equivalent):

```bash
export PATH="$HOME/.cargo/bin:$PATH"
```

**Windows** (add to user PATH via System Properties > Environment Variables):

```
%USERPROFILE%\.cargo\bin
```

### Step 3: Install rumoca-lsp

**Option A: Install from crates.io (recommended)**

```bash
cargo install rumoca
```

This installs the `rumoca-lsp` binary to `~/.cargo/bin/`.

**Option B: Build from source**

If you want the latest development version or need to make modifications:

```bash
git clone https://github.com/jgoppert/rumoca.git
cd rumoca
cargo install --path .
```

This also installs to `~/.cargo/bin/`. The extension will automatically find the installed `rumoca-lsp` binary.

**Verify installation:**

```bash
rumoca-lsp --version
```

### Step 4: Install the Extension

**From VS Code Marketplace:**

Search for "Rumoca Modelica" in the VS Code Extensions view (`Ctrl+Shift+X` / `Cmd+Shift+X`).

**From VSIX file:**

1. Download the `.vsix` file from [GitHub Releases](https://github.com/jgoppert/rumoca/releases)
2. In VS Code, open the Command Palette (`Ctrl+Shift+P` / `Cmd+Shift+P`)
3. Run "Extensions: Install from VSIX..."
4. Select the downloaded `.vsix` file

## Updating

To update `rumoca-lsp` to the latest version:

```bash
# From crates.io
cargo install rumoca --force

# Or from source
cd rumoca
git pull
cargo install --path . --force
```

The extension does not need to be updated separately - it will use whichever `rumoca-lsp` binary is installed.

## Configuration

| Setting | Description | Default |
|---------|-------------|---------|
| `rumoca.serverPath` | Path to a custom `rumoca-lsp` executable | `""` (auto-detect) |
| `rumoca.trace.server` | Traces communication with the language server | `"off"` |

## Troubleshooting

**Extension can't find rumoca-lsp:**

1. Verify it's installed: `rumoca-lsp --version`
2. Verify `~/.cargo/bin` is in your PATH: `echo $PATH` (Linux/macOS) or `echo %PATH%` (Windows)
3. Restart VS Code after installing
4. As a workaround, set `rumoca.serverPath` in VS Code settings to the full path

**Linux/macOS:**

```json
{
  "rumoca.serverPath": "/home/yourusername/.cargo/bin/rumoca-lsp"
}
```

**Windows:**

```json
{
  "rumoca.serverPath": "C:\\Users\\yourusername\\.cargo\\bin\\rumoca-lsp.exe"
}
```

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
