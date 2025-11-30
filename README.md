# Rumoca

[![CI](https://github.com/jgoppert/rumoca/actions/workflows/ci.yml/badge.svg)](https://github.com/jgoppert/rumoca/actions)
[![Crates.io](https://img.shields.io/crates/v/rumoca)](https://crates.io/crates/rumoca)
[![Documentation](https://docs.rs/rumoca/badge.svg)](https://docs.rs/rumoca)
[![License](https://img.shields.io/crates/l/rumoca)](LICENSE)

A modern Modelica compiler written in Rust that parses Modelica and exports a [Base Modelica](https://github.com/modelica/ModelicaSpecification/blob/MCP/0031/RationaleMCP/0031/ReadMe.md) IR (MCP-0031). This IR is consumed by [Cyecca](https://github.com/cognipilot/cyecca) for simulation and code generation with CasADi, SymPy, JAX, and other backends.

## Design Philosophy

Rumoca parses Modelica source files and exports a **Base Modelica IR** - the equation-based, causal subset designed for interchange between tools:

- **Input**: Modelica source files (`.mo`) - currently parses standard Modelica, with planned support for direct Base Modelica parsing
- **Output**: Base Modelica IR (JSON) - a structured intermediate representation suitable for analysis and code generation
- **Consumer**: [Cyecca](https://github.com/cognipilot/cyecca) - provides symbolic manipulation, simulation, and multi-backend code generation

This separation of concerns allows Rumoca to focus on fast, reliable parsing while Cyecca handles the symbolic analysis and backend-specific code generation.

## Features

- 🚀 **Fast**: Written in Rust with efficient parsing using PAROL
- 🎯 **Type-safe**: Strongly-typed AST with compile-time guarantees
- ✅ **Standards-compliant**: Exports to Base Modelica IR (MCP-0031)
- 🔒 **Reliable**: Native serde_json serialization, not template-based
- 📚 **Well-tested**: Comprehensive test suite with 87 automated tests
- 🛠️ **Developer-friendly**: Clean API for library usage
- 🔄 **Cross-platform**: Works on Linux, macOS, and Windows

## Quick Start

### Installation

Install via Cargo (requires [Rust](https://www.rust-lang.org/tools/install)):

```bash
cargo install rumoca
```

Verify installation:

```bash
rumoca --help
```

### Basic Usage

#### Command Line

Export to Base Modelica JSON (recommended):

```bash
rumoca model.mo --json > model.json
```

Use with Cyecca for code generation:

```bash
# Export to Base Modelica
rumoca model.mo --json > model.json

# Generate CasADi code with Cyecca
python3 -c "
from cyecca.io.base_modelica import import_base_modelica
from cyecca.backends.casadi import generate_casadi

model = import_base_modelica('model.json')
code = generate_casadi(model)
print(code)
"
```

Enable verbose output:

```bash
rumoca model.mo --json -v > model.json
```

#### Library Usage

Add to your `Cargo.toml`:

```toml
[dependencies]
rumoca = "0.7"
```

Use in your code:

```rust
use rumoca::Compiler;

fn main() -> anyhow::Result<()> {
    // Compile from a file
    let result = Compiler::new()
        .model("MyModel")
        .verbose(true)
        .compile_file("model.mo")?;

    // Check model balance (equations == unknowns)
    if !result.is_balanced() {
        eprintln!("Warning: {}", result.balance_status());
    }

    // Export to Base Modelica JSON (recommended for use with Cyecca)
    let json = result.to_base_modelica_json()?;
    println!("{}", json);

    // Or save to file
    std::fs::write("model.json", &json)?;

    // Access the DAE structure directly
    println!("States: {}", result.dae.x.len());
    println!("Parameters: {}", result.dae.p.len());
    println!("Compilation took: {:?}", result.total_time());

    Ok(())
}
```

See [`examples/`](examples/) for more complete examples.

#### Python Usage

The `python/` directory contains a Python wrapper that calls the rumoca CLI:

```python
import rumoca

# Compile a Modelica file
result = rumoca.compile("model.mo")

# Export to Base Modelica JSON
json_str = result.to_base_modelica_json()

# Or get as dict
data = result.to_base_modelica_dict()
```

See [`python/README.md`](python/README.md) for full documentation.

### Multi-File Compilation

When your model depends on libraries in separate files:

```rust
use rumoca::Compiler;

fn main() -> anyhow::Result<()> {
    // Include library files and compile the main model
    let result = Compiler::new()
        .model("MyModel")
        .include("library/utils.mo")
        .include("library/types.mo")
        .compile_file("model.mo")?;

    // Or compile multiple files directly
    let result = Compiler::new()
        .model("MyPackage.MyModel")
        .compile_files(&["lib.mo", "model.mo"])?;

    Ok(())
}
```

### Package Directory Compilation

Compile Modelica Standard Library-style package directories with `package.mo` and `package.order`:

```rust
use rumoca::Compiler;

fn main() -> anyhow::Result<()> {
    // Compile a package directory directly
    let result = Compiler::new()
        .model("MyLib.Examples.SimpleModel")
        .compile_package("path/to/MyLib")?;

    // Or include a package and compile another file
    let result = Compiler::new()
        .model("MyModel")
        .include_package("path/to/MyLib")?
        .compile_file("model.mo")?;

    // Use MODELICAPATH environment variable
    // Set MODELICAPATH=/path/to/libs before running
    let result = Compiler::new()
        .model("Modelica.Mechanics.Rotational.Examples.First")
        .include_from_modelica_path("Modelica")?
        .compile_file("model.mo")?;

    Ok(())
}
```

## Example Workflow

### Step 1: Write Modelica Model

```modelica
model Integrator
    Real x(start=0.0);
equation
    der(x) = 1.0;
end Integrator;
```

### Step 2: Export to Base Modelica JSON

```bash
rumoca integrator.mo --json > integrator.json
```

### Step 3: Generate Code with Cyecca

```python
from cyecca.io.base_modelica import import_base_modelica
from cyecca.backends.casadi import generate_casadi

# Import Base Modelica IR
model = import_base_modelica('integrator.json')

# Generate CasADi code
casadi_code = generate_casadi(model)
print(casadi_code)
```

**Why this approach?**
- ⚡ **Fast**: Native JSON serialization in Rust
- 🔒 **Type-safe**: Compile-time validation
- ✅ **Standard**: MCP-0031 compliant Base Modelica IR
- 🔧 **Flexible**: Use any Cyecca backend (CasADi, SymPy, JAX, etc.)

## Modelica 3.7 Specification Compliance

Rumoca targets the [Modelica Language Specification 3.7-dev](https://specification.modelica.org/). This section documents compliance status, deviations, and known limitations.

### Fully Supported (Spec Chapters 2-8, 11-12)

| Feature | Spec Reference | Notes |
|---------|---------------|-------|
| Class definitions | Ch. 4 | `model`, `class`, `block`, `connector`, `record`, `type`, `package`, `function` |
| Components | Ch. 4.4 | Declarations with modifications, array subscripts |
| Inheritance | Ch. 7.1 | `extends` clause with recursive resolution |
| Equations | Ch. 8 | Simple, connect, if, for, when equations |
| Algorithms | Ch. 11 | Assignment, if, for, while, when statements |
| Expressions | Ch. 3 | Binary/unary ops, function calls, if-expressions, arrays |
| Type prefixes | Ch. 4.4 | `flow`, `stream`, `discrete`, `parameter`, `constant`, `input`, `output` |
| Built-in operators | Ch. 3.7 | `der()`, `pre()`, `reinit()`, `time` |
| Modifications | Ch. 7.2 | Component modifications, class modifications |
| Flattening | Ch. 5.6 | Full hierarchical expansion with proper scoping |

### Partially Supported

| Feature | Spec Reference | Status | Limitation |
|---------|---------------|--------|------------|
| Connect equations | Ch. 9.1 | ✅ Basic | Flow/potential semantics implemented; `stream` not supported |
| Arrays | Ch. 10 | ✅ Good | Dimensions tracked and exported; array functions supported; literals with `{}` supported |
| Functions | Ch. 12 | ✅ Good | Single and multi-output functions inlined; tuple equations `(a,b) = func()` supported |
| Packages | Ch. 13 | ✅ Good | Nested packages; cross-package function calls; path-based model lookup; `package.mo`/`package.order` directory structure (Spec 13.4); MODELICAPATH (Spec 13.3) |
| Imports | Ch. 13.2 | ✅ Good | All import styles: qualified, renamed, unqualified (`.*`), selective (`{a,b}`) |
| Annotations | Ch. 18 | ⚠️ Parsed | Recognized but ignored during processing |
| External functions | Ch. 12.9 | ⚠️ Parsed | `external` keyword recognized; no linking |

### Not Implemented

| Feature | Spec Reference | Notes |
|---------|---------------|-------|
| Overloaded operators | Ch. 14 | `operator` class prefix recognized only |
| State machines | Ch. 17 | Synchronous language elements |
| Balanced models | Ch. 4.8 | ✅ Equation/variable counting with warnings |
| Overconstrained connectors | Ch. 9.4 | `Connections.root`, `branch`, etc. |
| Expandable connectors | Ch. 9.1.3 | Dynamic connector sizing |
| Inner/outer | Ch. 5.4 | Keywords recognized; lookup not implemented |
| Redeclarations | Ch. 7.3 | `redeclare`, `replaceable` parsed only |

### Known Deviations from Spec

1. **Flattening is mandatory** (Spec 5.6): All models are fully flattened; no support for preserving hierarchy
2. **Connect semantics simplified** (Spec 9.1): Uses union-find for connection sets; `stream` variables not handled
3. **Balanced model checking** (Spec 4.8): Reports warnings for over/under-determined models; does not prevent compilation
4. **Limited type checking**: Nominal type system not enforced; structural compatibility assumed
5. **Event semantics** (Spec 8.5): `noEvent`, `smooth`, `sample`, `edge`, `change`, `initial`, `terminal` are supported
6. **Array size inference**: Explicit sizes required; no automatic inference from context

### Built-in Functions

| Function | Status | Notes |
|----------|--------|-------|
| `der(x)` | ✅ | State derivative |
| `pre(x)` | ✅ | Previous discrete value |
| `reinit(x, expr)` | ✅ | In when clauses only |
| `time` | ✅ | Global simulation time |
| `sin`, `cos`, `tan` | ✅ | Basic trig |
| `asin`, `acos`, `atan`, `atan2` | ✅ | Inverse trig |
| `sinh`, `cosh`, `tanh` | ✅ | Hyperbolic trig |
| `exp`, `log`, `log10` | ✅ | Exponential/logarithmic |
| `sqrt` | ✅ | Square root |
| `abs`, `sign` | ✅ | Absolute value/sign |
| `floor`, `ceil` | ✅ | Rounding |
| `mod`, `rem`, `div`, `integer` | ✅ | Integer operations |
| `min`, `max` | ✅ | Scalar min/max |
| `sum`, `product` | ✅ | Array reductions |
| `zeros`, `ones`, `fill`, `identity` | ✅ | Array construction |
| `size`, `ndims` | ✅ | Array information |
| `transpose`, `symmetric`, `cross`, `skew` | ✅ | Array transformations |
| `outerProduct`, `diagonal`, `linspace` | ✅ | Advanced array ops |
| `scalar`, `vector`, `matrix` | ✅ | Type conversion |
| `noEvent` | ✅ | Suppress event generation |
| `smooth` | ✅ | Smoothness assertion |
| `sample`, `edge`, `change` | ✅ | Discrete event detection |
| `initial`, `terminal` | ✅ | Simulation phase |

### Code Generation Backends

Rumoca exports to Base Modelica JSON. Use [Cyecca](https://github.com/cognipilot/cyecca) to generate code for various frameworks:

- **CasADi** (Python) - Automatic differentiation, optimal control
- **SymPy** (Python) - Symbolic computation
- **JAX** (Python) - Machine learning, autodiff
- **NumPy** (Python) - Numerical computation
- **More backends** - Cyecca is extensible!

## Architecture

Rumoca processes Modelica files and exports to Base Modelica IR:

```
Modelica Source → Parse → Flatten → DAE → Base Modelica JSON
                  (AST)   (Flat)   (DAE)   (MCP-0031)
                                              ↓
                                          Cyecca
                                              ↓
                                   CasADi/SymPy/JAX/etc.
```

1. **Parse**: Converts Modelica text to Abstract Syntax Tree (AST)
2. **Flatten**: Expands hierarchical models into flat structure
3. **DAE**: Classifies variables and equations for DAE representation
4. **Export**: Serializes to Base Modelica JSON (MCP-0031 standard)
5. **Code Generation**: Use Cyecca to generate backend-specific code

## Development

### Building from Source

```bash
git clone https://github.com/jgoppert/rumoca.git
cd rumoca
cargo build --release
```

### Running Tests

```bash
cargo test
```

### Running Examples

```bash
cargo run --example basic_usage
cargo run --example file_compilation
```

### Code Quality Checks

```bash
cargo fmt --check    # Format checking
cargo clippy         # Linting
cargo doc            # Documentation
```

## Advanced: Custom Templates

For advanced users, Rumoca supports custom template-based export for specialized use cases (e.g., non-standard formats, embedded systems):

```bash
rumoca model.mo --template-file my_template.jinja > output.txt
```

Rumoca uses [MiniJinja](https://docs.rs/minijinja/) for template rendering. The DAE structure is passed to templates as the `dae` variable.

Example template:

```jinja
{%- for (name, component) in dae.x %}
{{ name }}: {{ component.type_name }}
{%- endfor %}
```

See [`templates/examples/`](templates/examples/) for template examples.

**Note**: For standard code generation (CasADi, SymPy, JAX), use the Base Modelica JSON + Cyecca workflow instead of templates.

## Why Rumoca?

### Motivation

There are many excellent tools for hybrid systems analysis, but porting models between different environments is challenging. Rumoca + Cyecca bridges this gap by:

- **Input**: Accepting [Modelica](https://modelica.org/), a standardized language for cyber-physical systems
- **Intermediate**: Base Modelica JSON (MCP-0031 standard)
- **Output**: Code for various frameworks via [Cyecca](https://github.com/cognipilot/cyecca) (CasADi, SymPy, JAX, etc.)

### Why Rust?

Rumoca is written in Rust for several key advantages:

- **Performance**: Faster parsing and processing than Python-based tools
- **Safety**: Memory safety without garbage collection
- **Type Safety**: Catch errors at compile time, not runtime
- **Modern Tooling**: Excellent package management, testing, and documentation
- **Cross-platform**: Easy deployment on Linux, macOS, and Windows

### Comparison with Other Tools

| Feature | Rumoca | PyMoca | OpenModelica | Marco |
|---------|--------|--------|--------------|-------|
| Language | Rust | Python | Modelica/C++ | C++ |
| Parsing | PAROL | ANTLR | ANTLR | LLVM |
| Speed | Fast | Moderate | Fast | Very Fast |
| Memory Safety | ✅ | ✅ | ❌ | ❌ |
| Type Safety | ✅ | Partial | ✅ | ✅ |
| Customizable Output | ✅ | ✅ | ❌ | ❌ |
| License | Apache | BSD | OSMC-PL | GPL |

## Roadmap

The following features are planned for future releases:

**Short-term:**
- `stream` connector variables and `inStream`/`actualStream` operators (Spec Ch. 15)
- Better error messages with source locations

**Medium-term:**
- Inner/outer lookup (Spec 5.4)
- Redeclarations and replaceable (Spec 7.3)
- Direct Base Modelica (MCP-0031) parsing
- Expandable connectors (Spec 9.1.3)

**Long-term:**
- Modelica Standard Library subset support
- Synchronous language elements (Spec Ch. 16) - Clock, clocked equations, sample/hold operators
- State machines (Spec Ch. 17) - transition, initialState, activeState
- Overconstrained connection graphs (Spec 9.4) - Connections.branch, root, potentialRoot
- External function linking (Spec 12.9)

## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

Key areas where help is needed:

- Additional Modelica language features
- Documentation improvements
- Bug reports and fixes
- Integration with Cyecca backends
- By contributing to this project, you agree that your contributions will be licensed under the Apache License, Version 2.0.

## License

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)

## Dependencies

- [PAROL](https://github.com/jsinger67/parol) - Parser generator and lexer
- [MiniJinja](https://github.com/mitsuhiko/minijinja) - Jinja2 template engine
- [Serde](https://github.com/serde-rs/serde) - Serialization framework
- [Clap](https://github.com/clap-rs/clap) - Command-line argument parser

## Citation

If you use Rumoca in academic work, please cite:

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

- [Cyecca](https://github.com/cognipilot/cyecca) - Code generation from Base Modelica IR (currently ir branch)
- [Base Modelica Specification (MCP-0031)](https://modelica.org/mcp/)
- [Modelica Language](https://www.modelica.org/)

## Acknowledgments

- Inspired by [PyMoca](https://github.com/pymoca/pymoca)
- Built with the excellent Rust ecosystem
- Thanks to all contributors and the Modelica community
