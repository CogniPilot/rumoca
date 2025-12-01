# Rumoca

[![CI](https://github.com/jgoppert/rumoca/actions/workflows/ci.yml/badge.svg)](https://github.com/jgoppert/rumoca/actions)
[![Crates.io](https://img.shields.io/crates/v/rumoca)](https://crates.io/crates/rumoca)
[![Documentation](https://docs.rs/rumoca/badge.svg)](https://docs.rs/rumoca)
[![License](https://img.shields.io/crates/l/rumoca)](LICENSE)

A Modelica compiler written in Rust. Rumoca parses Modelica source files and exports a DAE IR (a superset of [Base Modelica](https://github.com/modelica/ModelicaSpecification/blob/MCP/0031/RationaleMCP/0031/ReadMe.md) supporting both implicit and explicit model serialization). The IR is consumed by [Cyecca](https://github.com/cognipilot/cyecca) for model simulation, analysis, and Python library integration with CasADi, SymPy, JAX, and other backends.

Future export targets include [Base Modelica (MCP-0031)](https://github.com/modelica/ModelicaSpecification/blob/MCP/0031/RationaleMCP/0031/ReadMe.md) and [eFMI/GALEC](https://www.efmi-standard.org/).

## Tools

| Tool | Description |
|------|-------------|
| `rumoca` | Main compiler - parses Modelica and exports DAE IR (JSON) |
| `rumoca-fmt` | Code formatter for Modelica files (like `rustfmt`) |
| `rumoca-lsp` | Language Server Protocol server for editor integration |
| **VSCode Extension** | Full IDE support via the [Rumoca Modelica](https://marketplace.visualstudio.com/items?itemName=JamesGoppert.rumoca-modelica) extension |

## Installation

### Compiler and Formatter

```bash
cargo install rumoca
```

### VSCode Extension

Search for "Rumoca Modelica" in the VSCode Extensions marketplace, or install from the [marketplace page](https://marketplace.visualstudio.com/items?itemName=JamesGoppert.rumoca-modelica).

The extension includes bundled LSP binaries for all platforms - no additional setup required.

**Features:**
- Syntax highlighting
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
```

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

### Partially Supported

| Feature | Status |
|---------|--------|
| Connect equations | Flow/potential semantics implemented; `stream` not yet supported |
| Annotations | Parsed but not processed |
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
Modelica Source -> Parse -> Flatten -> DAE -> DAE IR (JSON)
                   (AST)   (Flat)    (DAE)
                                                 |
                                              Cyecca
                                                 |
                                      CasADi/SymPy/JAX/etc.
```

## Development

```bash
# Build
cargo build --release

# Run tests
cargo test --all-features

# Check formatting
cargo fmt --check
rumoca-fmt --check

# Lint
cargo clippy --all-features
```

## Contributing

Contributions welcome! See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

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

- [Cyecca](https://github.com/cognipilot/cyecca) - Model simulation, analysis, and code generation
- [Base Modelica (MCP-0031)](https://github.com/modelica/ModelicaSpecification/blob/MCP/0031/RationaleMCP/0031/ReadMe.md) - Planned export target
- [eFMI/GALEC](https://www.efmi-standard.org/) - Planned export target
- [Modelica Language](https://www.modelica.org/)
