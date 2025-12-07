# Rumoca

<img src="editors/vscode/icon.png" alt="Rumoca Logo" width="128" align="right">

[![CI](https://github.com/cognipilot/rumoca/actions/workflows/ci.yml/badge.svg)](https://github.com/cognipilot/rumoca/actions)
[![Crates.io](https://img.shields.io/crates/v/rumoca)](https://crates.io/crates/rumoca)
[![PyPI](https://img.shields.io/pypi/v/rumoca)](https://pypi.org/project/rumoca/)
[![Documentation](https://docs.rs/rumoca/badge.svg)](https://docs.rs/rumoca)
[![License](https://img.shields.io/crates/l/rumoca)](LICENSE)

A Modelica compiler written in Rust. Rumoca parses Modelica source files and exports to the [DAE IR Format](https://github.com/CogniPilot/modelica_ir) (supporting both implicit and explicit model serialization), or via user-customizable templates using [MiniJinja](https://github.com/mitsuhiko/minijinja). The DAE IR format is consumed by [Cyecca](https://github.com/cognipilot/cyecca) for model simulation, analysis, and Python library integration with CasADi, SymPy, and other backends.

> **Note:** Rumoca is in early development. While already usable for many practical tasks, you may encounter issues. Please [file bug reports](https://github.com/cognipilot/rumoca/issues) to help improve the compiler.

## Quick Start

```bash
# Install
cargo install rumoca

# Compile to DAE IR (JSON)
rumoca model.mo --json > model.json

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

    let json = result.to_json()?;
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

### Formatter

```bash
rumoca-fmt                              # Format all .mo files
rumoca-fmt --check                      # Check formatting (CI mode)
rumoca-fmt model.mo                     # Format specific files
rumoca-fmt --config indent_size=4       # Custom indentation
```

**Configuration:** Create `.rumoca_fmt.toml` in your project:

```toml
indent_size = 2
use_tabs = false
max_line_length = 100
blank_lines_between_classes = 1
```

### Linter

```bash
rumoca-lint                     # Lint all .mo files
rumoca-lint --level warning     # Show only warnings and errors
rumoca-lint --format json       # JSON output for CI
rumoca-lint --list-rules        # List available rules
rumoca-lint --deny-warnings     # Exit with error on warnings
```

**Available Rules:**

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

**Configuration:** Create `.rumoca_lint.toml` in your project:

```toml
min_level = "warning"
disabled_rules = ["magic-number", "missing-documentation"]
deny_warnings = false
```

### Custom Code Generation

Rumoca supports [MiniJinja](https://docs.rs/minijinja/) templates for custom code generation:

```bash
rumoca model.mo -m MyModel --template-file templates/examples/casadi.jinja > model.py
rumoca model.mo -m MyModel --template-file templates/examples/sympy.jinja > model.py
```

Example template:

```jinja
# Generated from {{ dae.model_name }}
{% for name, comp in dae.x | items %}
{{ name }}: {{ comp.type_name }} (start={{ comp.start }})
{% endfor %}
```

See [`templates/examples/`](templates/examples/) for complete examples (CasADi, SymPy, Base Modelica).

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

## Integration with Cyecca

```bash
rumoca model.mo --json > model.json
```

```python
from cyecca.io.rumoca import import_rumoca

model = import_rumoca('model.json')
# Use model for simulation, analysis, code generation, etc.
```

## Modelica Language Support

| Category | Supported |
|----------|-----------|
| Classes | `model`, `class`, `block`, `connector`, `record`, `type`, `package`, `function` |
| Equations | Simple, connect (flow/potential), if, for, when |
| Expressions | Binary/unary ops, function calls, if-expressions, arrays |
| Type prefixes | `flow`, `discrete`, `parameter`, `constant`, `input`, `output` |
| Packages | Nested packages, `package.mo`/`package.order`, MODELICAPATH |
| Imports | Qualified, renamed, unqualified (`.*`), selective (`{a,b}`) |
| Functions | Single/multi-output, tuple equations `(a,b) = func()` |
| Built-ins | `der`, `pre`, `reinit`, `time`, trig, array functions |
| Events | `noEvent`, `smooth`, `sample`, `edge`, `change`, `initial`, `terminal` |

### Partial Support

| Feature | Status |
|---------|--------|
| Algorithm sections | Parsed; assignments not yet counted in balance check |
| Connect equations | Flow/potential semantics; `stream` not supported |
| External functions | `external` recognized; no linking |
| Inner/outer | Basic resolution; nested scopes in progress |
| Complex type | Record expansion; operator overloading in progress |

### Not Yet Implemented

| Feature | Notes |
|---------|-------|
| Stream connectors | `inStream`, `actualStream` operators |
| Redeclarations | `redeclare`, `replaceable` parsed only |
| Overloaded operators | `operator` class prefix recognized only |
| State machines | Synchronous language elements (Ch. 17) |
| Expandable connectors | Dynamic connector sizing |
| Overconstrained connectors | `Connections.root`, `branch`, etc. |

## MSL Compatibility

Rumoca is tested against the [Modelica Standard Library 4.1.0](https://github.com/modelica/ModelicaStandardLibrary).

| Metric | Result |
|--------|--------|
| **Parse Rate** | 100% (2551/2551 files) |
| **Compile Rate** | 92.1% (2102/2283 models) |

**Balance Check Results:**

| Status | Count | Percentage | Description |
|--------|-------|------------|-------------|
| **Balanced** | 669 | 29.3% | Fully determined (equations = unknowns) |
| **Partial** | 1177 | 51.6% | Under-determined by design (external connectors) |
| **Unbalanced** | 256 | 11.2% | Needs further work |
| **Compile Errors** | 181 | 7.9% | Missing type/class resolution |

*Partial models have external connector flow variables that receive equations when connected in a larger system.*

**What Works Well** (92% of models compile successfully):
- Modelica.Thermal (97% success)
- Modelica.Magnetic (96% success)
- Modelica.Math (excellent coverage)

**Problematic Areas:**
- Modelica.Fluid (Medium type resolution)
- Modelica.ComplexBlocks (Complex type)
- Modelica.Electrical.Digital (algorithm sections)
- Modelica.Mechanics.MultiBody (StateSelect, algorithm sections)
- Modelica.Clocked (synchronous features)

**Top Compile Errors** (181 models):

| Count | Missing Type |
|-------|--------------|
| 41 | `Complex` |
| 32 | `StateSelect` |
| 26 | `Medium.AbsolutePressure` |
| 17 | `Medium.MassFlowRate` |
| 16 | `Medium.ThermodynamicState` |

**Known Limitations** (256 unbalanced models):

| Category | Notes |
|----------|-------|
| Algorithm sections | Assignments not yet counted as equations |
| Stream connectors | `inStream`/`actualStream` not implemented |
| External functions | Functions without equation bodies |
| Operator records | Operator overloading not implemented |

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

## Performance

### MSL Test Statistics

The following benchmarks were run on an AMD Ryzen 9 7950X (16 cores / 32 threads) compiling all 2283 simulatable models from the Modelica Standard Library 4.1.0:

| Phase | Time | Throughput |
|-------|------|------------|
| **Parsing** | 0.99s | 2,586 files/sec |
| **Balance Check (cold)** | 31.07s | 73.5 models/sec |
| **Balance Check (warm)** | 0.01s | 263,242 models/sec |
| **Total (cold)** | 33.60s | — |
| **Total (warm)** | 2.55s | — |

The balance check uses parallel processing with Rayon. Cold cache performance is dominated by flattening and DAE creation; warm cache performance is essentially free (disk I/O only).

### Caching Architecture

Rumoca uses a multi-level caching strategy to optimize repeated compilations:

#### In-Memory Caches (Session-Level)

| Cache | Location | Purpose |
|-------|----------|---------|
| `CLASS_DICT_CACHE` | `flatten.rs` | Index of parsed classes (name → AST) |
| `RESOLVED_CLASS_CACHE` | `flatten.rs` | Fully resolved classes (extends, imports applied) |
| `FILE_HASH_CACHE` | `flatten.rs` | MD5 hashes of source files for invalidation |
| `DAE_CACHE` | `pipeline.rs` | In-memory DAE balance results |

These caches are cleared when the process exits or when source files change (detected via file hashes).

#### Disk Cache (Persistent)

| Cache | Location | Size | Purpose |
|-------|----------|------|---------|
| **DAE Cache** | `~/.cache/rumoca/dae/` | ~8.4 MB (2102 entries) | Persists balance results across sessions |
| **MSL Source** | `~/.cache/rumoca/src/` | ~150 MB | Cached Modelica Standard Library download |

The DAE disk cache works like `ccache` - cached results are only valid when all dependent source files and the compiler version are unchanged. This makes the disk cache primarily useful for **CI pipelines and repeated test runs** where the same models are compiled multiple times without modification.

For the **editing experience** (LSP), the in-memory caches provide the main performance benefit. These caches store parsed class hierarchies and resolved definitions within a session, enabling fast re-analysis as you edit code. The disk cache doesn't help during editing since any file change invalidates the cached result.

**Clearing Caches:**

```rust
use rumoca::ir::transform::flatten::clear_caches;
use rumoca::compiler::pipeline::clear_dae_cache;

clear_caches();      // Clear in-memory caches
clear_dae_cache();   // Clear both in-memory and disk DAE cache
```

```bash
rm -rf ~/.cache/rumoca/dae/   # Manual disk cache removal
```

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
