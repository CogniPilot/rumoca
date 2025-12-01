# Rumoca Python Interface

Python wrapper for the [Rumoca](https://github.com/jgoppert/rumoca) Modelica compiler, enabling seamless integration with [Cyecca](https://github.com/cognipilot/cyecca) for code generation and simulation.

## Installation

### Prerequisites

1. **Build Rumoca** (Rust toolchain required):
```bash
cd /path/to/rumoca
cargo build --release
```

2. **Add to PATH** (optional but recommended):
```bash
export PATH=$PATH:/path/to/rumoca/target/release
```

### Install Python Package

```bash
# From source (development)
cd rumoca/python
pip install -e .

# With Cyecca integration
pip install -e ".[cyecca]"

# With Jupyter notebook support
pip install -e ".[notebook]"

# Everything
pip install -e ".[all]"
```

## Quick Start

### Basic Usage

```python
import rumoca

# Compile a Modelica model
result = rumoca.compile("bouncing_ball.mo")

# Export to Base Modelica JSON
result.export_base_modelica_json("bouncing_ball.json")

# Or get as string
json_str = result.to_base_modelica_json()

# Or get as Python dict
model_dict = result.to_base_modelica_dict()
```

### Integration with Cyecca

```python
import rumoca
from cyecca.io import import_base_modelica

# Compile Modelica model
result = rumoca.compile("my_model.mo")
result.export_base_modelica_json("my_model.json")

# Import into Cyecca
model = import_base_modelica("my_model.json")

print(f"Model: {model.name}")
print(f"States: {[v.name for v in model.variables if v.is_state]}")
```

### Code Generation with Cyecca

```python
import rumoca
from cyecca.io.base_modelica import import_base_modelica
from cyecca.backends.casadi import generate_casadi
from cyecca.backends.sympy import generate_sympy

# Compile to Base Modelica
result = rumoca.compile("my_model.mo")
result.export_base_modelica_json("my_model.json")

# Import with Cyecca
model = import_base_modelica("my_model.json")

# Generate CasADi code
casadi_code = generate_casadi(model)
with open("my_model_casadi.py", "w") as f:
    f.write(casadi_code)

# Generate SymPy code
sympy_code = generate_sympy(model)
with open("my_model_sympy.py", "w") as f:
    f.write(sympy_code)
```

## Jupyter Notebook Workflow

See [examples/bouncing_ball_demo.ipynb](../examples/notebooks/bouncing_ball_demo.ipynb) for a complete example.

```python
# Cell 1: Compile
import rumoca
from cyecca.io import import_base_modelica
import matplotlib.pyplot as plt

result = rumoca.compile("bouncing_ball.mo")
result.export_base_modelica_json("bouncing_ball.json")

# Cell 2: Import and inspect
model = import_base_modelica("bouncing_ball.json")
print(f"States: {[v.name for v in model.variables if not v.is_parameter]}")

# Cell 3: Generate simulation code
from cyecca.codegen import generate_simulation_code
code = generate_simulation_code(model, backend="python")

# Cell 4: Simulate and plot
results = code.simulate(t_span=(0, 10), dt=0.01)
plt.plot(results['t'], results['h'], label='Height')
plt.plot(results['t'], results['v'], label='Velocity')
plt.legend()
plt.show()
```

## API Reference

### `rumoca.compile(model_file, rumoca_bin=None)`

Compile a Modelica model file.

**Parameters:**
- `model_file` (str | Path): Path to the Modelica (.mo) file
- `rumoca_bin` (str | Path, optional): Path to rumoca binary (auto-detected if None)

**Returns:**
- `CompilationResult`: Object containing the compiled model

**Raises:**
- `FileNotFoundError`: If model file doesn't exist
- `RuntimeError`: If rumoca binary not found
- `CompilationError`: If compilation fails

**Example:**
```python
result = rumoca.compile("bouncing_ball.mo")
```

### `CompilationResult`

Result of compiling a Modelica model.

#### Methods

##### `to_base_modelica_json() -> str`

Export model to Base Modelica JSON format as a string.

**Returns:**
- `str`: JSON string containing Base Modelica representation (MCP-0031)

**Raises:**
- `CompilationError`: If export fails

**Example:**
```python
result = rumoca.compile("model.mo")
json_str = result.to_base_modelica_json()
```

##### `export_base_modelica_json(output_file)`

Export model to Base Modelica JSON file.

**Parameters:**
- `output_file` (str | Path): Path where JSON file will be written

**Raises:**
- `CompilationError`: If export fails

**Example:**
```python
result = rumoca.compile("model.mo")
result.export_base_modelica_json("model.json")
```

##### `to_base_modelica_dict() -> dict`

Get Base Modelica representation as Python dictionary.

**Returns:**
- `dict`: Dictionary containing Base Modelica model data

**Example:**
```python
result = rumoca.compile("model.mo")
data = result.to_base_modelica_dict()
print(f"Parameters: {len(data['parameters'])}")
```

##### `export(template_file) -> str`

Advanced: Export using a custom template.

**Parameters:**
- `template_file` (str | Path): Path to custom Jinja2 template file

**Returns:**
- `str`: Generated code as string

**Note:** For standard code generation, use `to_base_modelica_json()` + Cyecca instead.

## Examples

### Example 1: Simple Compilation

```python
import rumoca

# Compile and export in one go
result = rumoca.compile("integrator.mo")
result.export_base_modelica_json("integrator.json")
```

### Example 2: Inspect Model Structure

```python
import rumoca

result = rumoca.compile("rover.mo")
model_data = result.to_base_modelica_dict()

print(f"Model: {model_data['model_name']}")
print(f"Parameters: {len(model_data['parameters'])}")
print(f"Variables: {len(model_data['variables'])}")
print(f"Equations: {len(model_data['equations'])}")
```

### Example 3: Full Workflow with Cyecca

```python
import rumoca
from cyecca.io.base_modelica import import_base_modelica
from cyecca.backends.casadi import generate_casadi

# Step 1: Compile Modelica to Base Modelica
result = rumoca.compile("quadrotor.mo")
result.export_base_modelica_json("quadrotor.json")

# Step 2: Import with Cyecca
model = import_base_modelica("quadrotor.json")

# Step 3: Generate CasADi code
casadi_code = generate_casadi(model)

# Step 4: Save generated code
with open("quadrotor_casadi.py", "w") as f:
    f.write(casadi_code)
```

### Example 4: Error Handling

```python
import rumoca

try:
    result = rumoca.compile("my_model.mo")
    result.export_base_modelica_json("output.json")
except FileNotFoundError as e:
    print(f"Model file not found: {e}")
except rumoca.CompilationError as e:
    print(f"Compilation failed: {e}")
```

## Architecture

### Subprocess-Based Wrapper

The current implementation uses a subprocess-based wrapper that:

1. Calls the `rumoca` binary as a subprocess with `--json` flag
2. Receives Base Modelica JSON via stdout
3. Parses JSON into Python dictionaries
4. Captures stderr for error reporting

**Advantages:**
- Simple implementation
- Easy to maintain
- Works with existing Rumoca binary
- Fast native JSON serialization in Rust

**Workflow:**
```
Python → subprocess(rumoca --json) → Base Modelica JSON → Python dict
                                            ↓
                                        Cyecca
                                            ↓
                                     Generated Code
```

## Troubleshooting

### "Rumoca binary not found"

**Solution 1:** Add rumoca to PATH:
```bash
export PATH=$PATH:/path/to/rumoca/target/release
```

**Solution 2:** Specify binary path explicitly:
```python
result = rumoca.compile("model.mo", rumoca_bin="/path/to/rumoca")
```

### Compilation errors

Check the Modelica syntax by running rumoca directly:
```bash
rumoca your_model.mo --json
```

If successful, you should see Base Modelica JSON output.

## Development

### Running Tests

```bash
cd rumoca/python
pip install -e ".[dev]"
pytest
```

### Type Checking

```bash
mypy rumoca
```

### Code Formatting

```bash
black rumoca tests
```

## Contributing

Contributions welcome! Please:

1. Run tests: `pytest`
2. Check types: `mypy rumoca`
3. Format code: `black rumoca tests`
4. Update documentation

## License

Apache 2.0 (same as Rumoca)

## See Also

- [Rumoca Compiler](https://github.com/jgoppert/rumoca)
- [Cyecca Code Generator](https://github.com/cognipilot/cyecca)
- [Base Modelica Specification (MCP-0031)](https://modelica.org/mcp/)
- [Modelica Language](https://www.modelica.org/)
