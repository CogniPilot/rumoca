# Rumoca

<img src="editors/icons/rumoca.png" alt="Rumoca Logo" width="128" align="right">

[![CI](https://github.com/cognipilot/rumoca/actions/workflows/ci.yml/badge.svg)](https://github.com/cognipilot/rumoca/actions)
[![Crates.io](https://img.shields.io/crates/v/rumoca)](https://crates.io/crates/rumoca)
[![PyPI](https://img.shields.io/pypi/v/rumoca)](https://pypi.org/project/rumoca/)
[![Documentation](https://docs.rs/rumoca/badge.svg)](https://docs.rs/rumoca)
[![License](https://img.shields.io/crates/l/rumoca)](LICENSE)

**[Try Rumoca in your browser!](https://cognipilot.github.io/rumoca/)** - No installation required.

A Modelica compiler written in Rust. Rumoca parses Modelica source files and exports to the [DAE IR Format](https://github.com/CogniPilot/modelica_ir) (supporting both implicit and explicit model serialization), or via user-customizable templates using [MiniJinja](https://github.com/mitsuhiko/minijinja). The DAE IR format is consumed by [Cyecca](https://github.com/cognipilot/cyecca) for model simulation, analysis, and Python library integration with CasADi, SymPy, and other backends.

> **Note:** Rumoca is in early development. While already usable for many practical tasks, you may encounter issues. Please [file bug reports](https://github.com/cognipilot/rumoca/issues) to help improve the compiler.

## Quick Start

```bash
# Install
cargo install rumoca

# Compile to DAE IR (JSON)
rumoca model.mo -m MyModel --json > model.json

# Format Modelica files
rumoca-fmt

# Lint Modelica files
rumoca-lint
```

## Installation

### Rust (Compiler, Formatter, Linter)

```bash
cargo install rumoca
```

### Python

The Python package bundles the Rust compiler, so no separate Rust installation is needed:

```bash
pip install rumoca
```

```python
import rumoca

# Compile a Modelica file
result = rumoca.compile("model.mo")

# Get as JSON string or Python dict
json_str = result.to_base_modelica_json()
model_dict = result.to_base_modelica_dict()

# Compile from string
result = rumoca.compile_source("""
    model Test
        Real x(start=0);
    equation
        der(x) = 1;
    end Test;
""", "Test")
```

### Rust Library

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

    let json = result.to_dae_ir_json()?;
    println!("{}", json);
    Ok(())
}
```

## Tools

| Tool | Description |
|------|-------------|
| `rumoca` | Main compiler - parses Modelica and exports DAE IR (JSON) |
| `rumoca-fmt` | Code formatter for Modelica files (like `rustfmt`) |
| `rumoca-lint` | Linter for Modelica files (like `clippy`) |
| `rumoca-lsp` | Language Server Protocol server for editor integration |

## MSL Compatibility

Rumoca is tested against the [Modelica Standard Library 4.1.0](https://github.com/modelica/ModelicaStandardLibrary).

| Metric | Value | Description |
|--------|-------|-------------|
| **Parse Rate** | 100% (2551/2551) | All .mo files parse successfully |
| **Compile Rate** | 100% (2283/2283) | All models compile to DAE |
| **Balance Rate** | 94.9% (862 balanced, 1305 partial, 116 unbalanced) | 100% − (unbalanced / total) |

*Partial models are under-determined by design—they have external connectors whose flow variables receive equations when connected in a larger system.*

**Benchmark** (AMD Ryzen 9 7950X, 16 cores):

| Phase | Cold | Warm | Rate |
|-------|------|------|------|
| Parse (2551 files) | 0.80s | 0.80s | 3,189 files/sec |
| Flatten (6491 classes) | 1.51s | 1.50s | 4,311 classes/sec |
| Compile (2283 models) | 22.75s | 3.68s | 100 / 620 models/sec |
| **Total** | **25.06s** | **5.98s** | **4.2x speedup** |

**Caching:**
- **In-memory caches** (extends chain, resolved classes): Built during flatten phase (~1.5s), reset between runs
- **DAE disk cache** (`~/.cache/rumoca/dae/`): Stores compiled model results, provides 6x speedup on balance phase

```bash
# Run the MSL balance test
cargo test --release test_msl_balance_all -- --ignored --nocapture

# Clear disk caches for cold-start benchmark
rm -rf ~/.cache/rumoca/ast ~/.cache/rumoca/dae
```

<details>
<summary><strong>Detailed Compatibility Notes</strong></summary>

**Language Support:**

| Category | Supported |
|----------|-----------|
| Classes | `model`, `class`, `block`, `connector`, `record`, `type`, `package`, `function` |
| Equations | Simple, connect (flow/potential), if, for, when, algorithm sections |
| Expressions | Binary/unary ops, function calls, if-expressions, arrays |
| Type prefixes | `flow`, `discrete`, `parameter`, `constant`, `input`, `output` |
| Packages | Nested packages, `package.mo`/`package.order`, MODELICAPATH |
| Imports | Qualified, renamed, unqualified (`.*`), selective (`{a,b}`) |
| Functions | Single/multi-output, tuple equations `(a,b) = func()`, function inlining |
| Built-ins | `der`, `pre`, `reinit`, `time`, trig, array functions, transpose, etc. |
| Events | `noEvent`, `smooth`, `sample`, `edge`, `change`, `initial`, `terminal` |
| Clocked | `previous`, `hold`, `interval`, `sample`, `shiftSample`, `Clock`, etc. |
| Stream | `inStream`, `actualStream` operators recognized |

**Partial Support:**

| Feature | Status |
|---------|--------|
| Stream connectors | Operators recognized; balance semantics in progress |
| External functions | `external` recognized; no linking |
| Inner/outer | Basic resolution; nested scopes in progress |
| Operator records | Record expansion works; operator overloading in progress |
| Clocked partitions | Operators work; discrete partition balance semantics in progress |

**Not Yet Implemented:**

| Feature | Notes |
|---------|-------|
| Redeclarations | `redeclare`, `replaceable` parsed only |
| Overloaded operators | `operator` class prefix recognized only |
| Expandable connectors | Dynamic connector sizing |
| Overconstrained connectors | `Connections.root`, `branch`, etc. |

**What Works Well** (100% compile rate achieved):
- All MSL packages compile successfully
- Complex type support (operator record)
- Replaceable package/model resolution (Medium.*, etc.)
- Deep inheritance chain type lookup
- ModelicaServices stubs

**Known Limitations** (116 unbalanced models):

| Category | Notes |
|----------|-------|
| Stream connectors | Balance semantics for stream variables |
| External functions | Functions without equation bodies |
| Operator records | Operator overloading not yet implemented |
| Clocked partitions | Discrete partition equation counting |

</details>

### Custom Code Generation

Rumoca supports [MiniJinja](https://docs.rs/minijinja/) templates for custom code generation:

```bash
rumoca model.mo -m MyModel --template-file examples/templates/casadi.jinja > model.py
rumoca model.mo -m MyModel --template-file examples/templates/sympy.jinja > model.py
```

Example template:

```jinja
# Generated from {{ dae.model_name }}
{% for name, comp in dae.x | items %}
{{ name }}: {{ comp.type_name }} (start={{ comp.start }})
{% endfor %}
```

See [`examples/templates/`](examples/templates/) for complete examples (CasADi, SymPy, Base Modelica).

## VSCode Extension

Search for "Rumoca Modelica" in the VSCode Extensions marketplace, or install from the [marketplace page](https://marketplace.visualstudio.com/items?itemName=JamesGoppert.rumoca-modelica).

![Rumoca VSCode Extension Demo](docs/rumoca-demo.gif)

The extension includes a bundled `rumoca-lsp` language server - **no additional installation required**.

**Features:**
- Syntax highlighting (semantic tokens)
- Real-time diagnostics with type checking
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

**Configuring Library Paths:**

```json
{
  "rumoca.modelicaPath": [
    "/path/to/ModelicaStandardLibrary",
    "/path/to/other/library"
  ]
}
```

Alternatively, set the `MODELICAPATH` environment variable. See the [extension documentation](editors/vscode/README.md) for details.

## WebAssembly (Browser)

Rumoca compiles to WebAssembly, enabling browser-based Modelica compilation without a backend server.

**Features:**
- Parse and compile Modelica models in the browser
- DAE IR (JSON) generation
- Template rendering with MiniJinja
- Multi-threaded compilation using Web Workers

**Try the Demo:**

```bash
./tools/wasm-test.sh
# Open http://localhost:8080/examples/wasm_editor/index.html
```

The demo provides a Monaco-based editor with:
- Split-pane Modelica and Jinja2 template editing
- Real-time template preview
- DAE IR JSON export
- Autocomplete for template variables (`dae.x`, `dae.u`, etc.)

[**Building WASM:**](./wasm/README.md)


## Integration with Cyecca

```bash
rumoca model.mo -m MyModel --json > model.json
```

```python
from cyecca.io.rumoca import import_rumoca

model = import_rumoca('model.json')
# Use model for simulation, analysis, code generation, etc.
```

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
cargo build --release   # Build
cargo test              # Run tests
cargo fmt --check       # Check Rust formatting
cargo clippy            # Lint Rust code
rumoca-fmt --check      # Check Modelica formatting
rumoca-lint             # Lint Modelica files
```

<details>
<summary><strong>Formatter & Linter Configuration</strong></summary>

**Formatter:**

```bash
rumoca-fmt                              # Format all .mo files
rumoca-fmt --check                      # Check formatting (CI mode)
rumoca-fmt model.mo                     # Format specific files
rumoca-fmt --config indent_size=4       # Custom indentation
```

Configuration (`.rumoca_fmt.toml`):

```toml
indent_size = 2
use_tabs = false
max_line_length = 100
blank_lines_between_classes = 1
```

**Linter:**

```bash
rumoca-lint                     # Lint all .mo files
rumoca-lint --level warning     # Show only warnings and errors
rumoca-lint --format json       # JSON output for CI
rumoca-lint --list-rules        # List available rules
rumoca-lint --deny-warnings     # Exit with error on warnings
```

Available Rules:

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

Configuration (`.rumoca_lint.toml`):

```toml
min_level = "warning"
disabled_rules = ["magic-number", "missing-documentation"]
deny_warnings = false
```

</details>

### Caching

Rumoca uses multi-level caching: in-memory caches for session-level performance (parsed classes, resolved definitions), and a persistent disk cache (`~/.cache/rumoca/dae/`) that works like `ccache` for CI pipelines. The disk cache is invalidated when source files or the compiler version change.

## Roadmap

**Export Targets:**
- [eFMI/GALEC](https://www.efmi-standard.org/)

**Import Targets:**
- [Base Modelica (MCP-0031)](https://github.com/modelica/ModelicaSpecification/blob/MCP/0031/RationaleMCP/0031/ReadMe.md) - interface with OpenModelica, Dymola, etc.

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
- [Modelica Language](https://www.modelica.org/)
