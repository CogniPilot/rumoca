# Rumoca Python API

Rumoca's typed Python API compiles, inspects, simulates, and exports Modelica
models. The package includes a `py.typed` marker and complete type stubs, so
VS Code/Pylance, Pyright, and other type-aware editors provide signatures,
hover documentation, and completion without loading a model.

## Install

```bash
pip install rumoca
```

Optional features are installed separately:

```bash
pip install "rumoca[plot]"     # matplotlib
pip install "rumoca[data]"     # numpy and pandas
pip install "rumoca[notebook]" # IPython/Jupyter integration
pip install "rumoca[all]"      # all optional integrations
```

For a repository checkout, create or activate a virtual environment and run:

```bash
cargo xtask python build
```

Select that environment as the Python interpreter in your editor so it can
find `rumoca/__init__.pyi` and `rumoca/py.typed`.

## Compile and simulate

```python
from pathlib import Path
import rumoca

session = rumoca.Session(roots=["vendor/Modelica"])
model = session.load(Path("Aircraft.mo"), model="Aircraft")

print(model.summary())
print(model.states.names)
print(model.inputs.names)
print(model.parameters["mass"].value)

result = model.simulate(
    t=(0.0, 10.0),
    dt=0.01,
    config=rumoca.SimConfig(solver="bdf", rtol=1e-7),
    params={"controller.gain": 4.0},
    inputs={"armed": 1.0, "throttle": 0.7},
)

result.plot("position[1]", "position[3]")
frame = result.to_dataframe()
```

`inputs=` sets constant input values before initialization. Time-varying input
functions are not yet supported by the one-shot `Model.simulate` API.

## Fast parameter sweeps

Compilation is shared between models returned by `with_params`, so tunable
parameter sweeps do not recompile the Modelica source:

```python
gains = [2.0, 4.0, 6.0]
runs = {
    gain: model.with_params(**{"controller.gain": gain}).simulate(
        t=10.0,
        dt=0.02,
        inputs={"armed": 1.0},
    )
    for gain in gains
}
```

Use `with_params(recompile=True, ...)` only for structural parameters, such as
array dimensions or parameters annotated with `Evaluate=true`. `ParameterInfo.kind`
reports `"tunable"` or `"structural"`.

## Results

`Result` supports named column lookup without JSON conversion:

```python
time = result.time
altitude = result["position[3]"]
matrix = result.to_numpy(["position[1]", "position[3]"])
frame = result.to_dataframe()
axes = result.plot("airspeed", "altitude")
```

Use `result.names` to discover the exact flattened variable names. Unknown
names raise `KeyError` with available-name suggestions.

## Code generation and FMI 3

```python
generated = model.codegen("fmi3")
print(generated.paths)
generated.save_all("build/fmi3")
```

`codegen("fmi3")` returns generated FMI 3 source and build files. It does not
currently build or execute an FMU from Python; native simulation uses Rumoca's
solver backends.

List capabilities at runtime with `rumoca.targets()` and `rumoca.solvers()`.

## Jupyter

Importing `rumoca` registers the `%%modelica` cell magic when running inside
IPython:

```python
%%modelica --model Decay --action sim --t-end 2.0 --dt 0.01
model Decay
  Real x(start=1);
equation
  der(x) = -x;
end Decay;
```

The returned value is the same typed `Model`, `Result`, or `CodegenResult`
object used by the regular Python API.
