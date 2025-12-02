# Rumoca

<img src="editors/vscode/icon.png" alt="Rumoca Logo" width="128" align="right">

[![CI](https://github.com/cognipilot/rumoca/actions/workflows/ci.yml/badge.svg)](https://github.com/cognipilot/rumoca/actions)
[![Crates.io](https://img.shields.io/crates/v/rumoca)](https://crates.io/crates/rumoca)
[![Documentation](https://docs.rs/rumoca/badge.svg)](https://docs.rs/rumoca)
[![License](https://img.shields.io/crates/l/rumoca)](LICENSE)

> **Note:** Rumoca is in early development. While already usable for many practical tasks, you may encounter issues. Please [file bug reports](https://github.com/cognipilot/rumoca/issues) to help improve the compiler. APIs may change between releases.

A Modelica compiler written in Rust. Rumoca parses Modelica source files and exports to the [DAE IR Format](https://github.com/CogniPilot/modelica_ir) supporting both implicit and explicit model serialization), or via user customizable template leveraging [minijinja](https://github.com/mitsuhiko/minijinja). The DAE IR format is consumed by [Cyecca](https://github.com/cognipilot/cyecca) (see the [`ir` branch](https://github.com/cognipilot/cyecca/tree/ir) for ongoing integration) for model simulation, analysis, and Python library integration with CasADi, SymPy, and other backends planned (e.g. Jax).

Future targets include:
- **Export**: [eFMI/GALEC](https://www.efmi-standard.org/)
- **Import**: [Base Modelica (MCP-0031)](https://github.com/modelica/ModelicaSpecification/blob/MCP/0031/RationaleMCP/0031/ReadMe.md) to interface with more mature compilers (OpenModelica, Dymola, etc.)

## Tools

| Tool | Description |
|------|-------------|
| `rumoca` | Main compiler - parses Modelica and exports DAE IR (JSON) |
| `rumoca-fmt` | Code formatter for Modelica files (like `rustfmt`) |
| `rumoca-lint` | Linter for Modelica files (like `clippy`) |
| `rumoca-lsp` | Language Server Protocol server for editor integration |
| **VSCode Extension** | Full Modelica IDE support via the [Rumoca Modelica](https://marketplace.visualstudio.com/items?itemName=JamesGoppert.rumoca-modelica) extension |

## Installation

### Compiler, Formatter, and Linter

```bash
cargo install rumoca
```

### VSCode Extension

**Prerequisites:**

1. Install [Rust](https://rustup.rs/) if you haven't already (Windows users: see [rustup.rs](https://rustup.rs/) for installer):
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   ```

2. Ensure `~/.cargo/bin` is in your PATH (the rustup installer typically adds this automatically). You may need to restart your terminal or run:
   ```bash
   source ~/.cargo/env
   ```

3. Install the language server:
   ```bash
   cargo install rumoca
   ```

**Install the Extension:**

Search for "Rumoca Modelica" in the VSCode Extensions marketplace, or install from the [marketplace page](https://marketplace.visualstudio.com/items?itemName=JamesGoppert.rumoca-modelica).

**Features:**
- Syntax highlighting (semantic tokens)
- Real-time diagnostics
- Autocomplete for keywords, built-in functions, and class members
- Go to definition / Find references
- Document symbols and outline
- Code formatting
- Hover information
- Signature help
- Code folding
- Inlay hints
- Code lens with reference counts
- Rename symbol
- Call hierarchy
- Document links

## Quick Start

### Compile to DAE IR (JSON)

```bash
rumoca model.mo --json > model.json
```

### Format Modelica Files

```bash
# Format all .mo files in current directory
rumoca-fmt

# Check formatting (CI mode)
rumoca-fmt --check

# Format specific files
rumoca-fmt model.mo library.mo

# Use 4-space indentation
rumoca-fmt --config indent_size=4

# Configure blank lines between classes
rumoca-fmt --config blank_lines_between_classes=1
```

**Configuration:** Create `.rumoca_fmt.toml` or `rumoca_fmt.toml` in your project:

```toml
indent_size = 2
use_tabs = false
max_line_length = 100
blank_lines_between_classes = 1
```

### Lint Modelica Files

```bash
# Lint all .mo files in current directory
rumoca-lint

# Lint specific files
rumoca-lint model.mo

# Show only warnings and errors
rumoca-lint --level warning

# Output as JSON (for CI integration)
rumoca-lint --format json

# List available lint rules
rumoca-lint --list-rules

# Exit with error on warnings (CI mode)
rumoca-lint --deny-warnings
```

**Available Lint Rules:**

| Rule | Level | Description |
|------|-------|-------------|
| `naming-convention` | note | CamelCase for types, camelCase for variables |
| `missing-documentation` | note | Classes without documentation strings |
| `unused-variable` | warning | Declared but unused variables |
| `undefined-reference` | error | References to undefined variables |
| `parameter-no-default` | help | Parameters without default values |
| `empty-section` | note | Empty equation or algorithm sections |
| `magic-number` | help | Magic numbers that should be constants |
| `complex-expression` | note | Overly complex/deeply nested expressions |
| `inconsistent-units` | warning | Potential unit inconsistencies |
| `redundant-extends` | warning | Duplicate or circular extends |

**Configuration:** Create `.rumoca_lint.toml` or `rumoca_lint.toml` in your project. The linter searches for config files starting from the file's directory and walking up to parent directories:

```toml
min_level = "warning"                                    # help, note, warning, error
disabled_rules = ["magic-number", "missing-documentation"]
deny_warnings = false                                    # exit with error on warnings
```

CLI options override config file settings.

### Library Usage

```toml
[dependencies]
rumoca = "0.7"
```

```rust
use rumoca::Compiler;

fn main() -> anyhow::Result<()> {
    let result = Compiler::new()
        .model("MyModel")
        .compile_file("model.mo")?;

    // Export to DAE IR (JSON)
    let json = result.to_json()?;
    println!("{}", json);

    Ok(())
}
```

### Use with Cyecca

```bash
rumoca model.mo --json > model.json
```

```python
from cyecca.io.rumoca import import_rumoca

model = import_rumoca('model.json')
# Use model for simulation, analysis, code generation, etc.
```

### Custom Code Generation with Templates

Rumoca supports [MiniJinja](https://docs.rs/minijinja/) templates for custom code generation:

```bash
# Generate CasADi Python code
rumoca model.mo -m MyModel --template-file templates/examples/casadi.jinja > model.py

# Generate SymPy code
rumoca model.mo -m MyModel --template-file templates/examples/sympy.jinja > model.py
```

The DAE structure is passed to templates as the `dae` variable. Example template:

```jinja
# Generated from {{ dae.model_name }}
{% for name, comp in dae.x | items %}
{{ name }}: {{ comp.type_name }} (start={{ comp.start }})
{% endfor %}
```

See [`templates/examples/`](templates/examples/) for complete template examples (CasADi, SymPy, Base Modelica).

## Modelica Language Support

### Fully Supported

- **Class definitions**: `model`, `class`, `block`, `connector`, `record`, `type`, `package`, `function`
- **Components**: Declarations with modifications, array subscripts
- **Inheritance**: `extends` clause with recursive resolution
- **Equations**: Simple, connect, if, for, when equations
- **Algorithms**: Assignment, if, for, while, when statements
- **Expressions**: Binary/unary operators, function calls, if-expressions, arrays
- **Type prefixes**: `flow`, `stream`, `discrete`, `parameter`, `constant`, `input`, `output`
- **Modifications**: Component and class modifications
- **Packages**: Nested packages, `package.mo`/`package.order` directory structure, MODELICAPATH
- **Imports**: Qualified, renamed, unqualified (`.*`), selective (`{a,b}`)
- **Functions**: Single and multi-output functions, tuple equations `(a,b) = func()`
- **Built-in operators**: `der()`, `pre()`, `reinit()`, `time`, trig functions, array functions
- **Event functions**: `noEvent`, `smooth`, `sample`, `edge`, `change`, `initial`, `terminal`
- **Annotations**: Parsed and exported to JSON on components

### Partially Supported

| Feature | Status |
|---------|--------|
| Connect equations | Flow/potential semantics implemented; `stream` not yet supported |
| External functions | `external` keyword recognized; no linking |

### Not Yet Implemented

| Feature | Notes |
|---------|-------|
| Stream connectors | `inStream`, `actualStream` operators |
| Inner/outer | Keywords recognized; lookup not implemented |
| Redeclarations | `redeclare`, `replaceable` parsed only |
| Overloaded operators | `operator` class prefix recognized only |
| State machines | Synchronous language elements (Ch. 17) |
| Expandable connectors | Dynamic connector sizing |
| Overconstrained connectors | `Connections.root`, `branch`, etc. |

## Architecture

```
Modelica Source -> Parse -> Flatten -> BLT -> DAE -> DAE IR (JSON)
                   (AST)   (Flat)    (Match)  (DAE)
                                                          |
                                                       Cyecca
                                                          |
                                               CasADi/SymPy/JAX/etc.
```

**Structural Analysis:**
- **Hopcroft-Karp matching** (O(E√V)) for equation-variable assignment
- **Tarjan's SCC algorithm** for topological ordering and algebraic loop detection
- **Pantelides algorithm** for DAE index reduction (detects high-index systems)
- **Tearing** for algebraic loops (reduces nonlinear system size)

## Development

```bash
# Build
cargo build --release

# Run tests
cargo test

# Check formatting
cargo fmt --check
rumoca-fmt --check

# Lint
cargo clippy
rumoca-lint
```

## Contributing

Contributions welcome! All contributions must be made under the Apache-2.0 license.

## License

Apache-2.0 ([LICENSE](LICENSE))

## Citation

```bibtex
@inproceedings{condie2025rumoca,
  title={Rumoca: Towards a Translator from Modelica to Algebraic Modeling Languages},
  author={Condie, Micah and Woodbury, Abigaile and Goppert, James and Andersson, Joel},
  booktitle={Modelica Conferences},
  pages={1009--1016},
  year={2025}
}
```

## See Also

- [Modelica IR](https://github.com/CogniPilot/modelica_ir) - DAE IR specification
- [Cyecca](https://github.com/cognipilot/cyecca) - Model simulation, analysis, and code generation
- [Base Modelica (MCP-0031)](https://github.com/modelica/ModelicaSpecification/blob/MCP/0031/RationaleMCP/0031/ReadMe.md) - Planned import format
- [eFMI/GALEC](https://www.efmi-standard.org/) - Planned export format
- [Modelica Language](https://www.modelica.org/)
