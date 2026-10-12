# Python API

Rumoca ships a first-class, typed Python API. Create a `Session` once, compile
models through it, and everything — metadata, simulation, code generation,
symbolic export — hangs off the returned `Model`. Returned objects are real
typed classes (full autocomplete), never JSON you have to `json.loads`.

```bash
pip install rumoca            # core
pip install rumoca[data]      # + numpy/pandas (result.to_numpy / to_dataframe)
pip install rumoca[plot]      # + matplotlib (result.plot)
pip install rumoca[casadi]    # + casadi   (model.to_casadi)
pip install rumoca[jax]       # + jax      (model.to_jax)
pip install rumoca[all]       # everything, incl. the %%modelica magic
```

Optional dependencies are genuinely optional: `import rumoca` never needs them,
and a method that does (e.g. `result.plot()`) raises a message naming the extra
to install.

## Quick start

Run these examples from the Rumoca repository root. They use the tracked
[`SympyDecay`](https://github.com/CogniPilot/rumoca/blob/main/examples/models/SympyDecay.mo)
model, whose state is `x` and whose tunable decay rate is `k`.

```python
from pathlib import Path
import rumoca as rm

session = rm.Session()
m = session.load("examples/models/SympyDecay.mo", model="SympyDecay")
m                                  # repr shows a summary
m.parameters["k"].value            # 0.5
m.parameters["k"].kind             # "tunable"
m.states["x"]                     # VariableInfo

r = m.simulate(t=(0, 10), dt=0.01)
r.plot("x")                        # needs rumoca[plot]
df = r.to_dataframe()              # needs rumoca[data]
```

Compile from a string with `session.loads(source, model=...)`. Keep and reuse
the `Session`; it owns source roots and compiler caches.

Or load everything from a `rumoca-scenario.toml` in one call — its model, source
roots, and solver settings (paths resolved relative to the file):

```python
scenario = "examples/simulation/rumoca-scenario.sympy_decay.toml"
session, model, config = rm.Session.from_scenario(scenario)
r = model.simulate(t=(0, 10), config=config)   # config carries solver/dt
```

For build systems and scenario-driven runs, use the session scenario entry
point:

```python
session = rm.Session()
run = session.run_scenario(scenario)
run.status          # "completed"
run.output_paths    # generated reports, CSVs, codegen files, debug logs
run.result          # Result for batch simulation scenarios, otherwise None
run.codegen         # CodegenResult for codegen scenarios, otherwise None
```

Runtime knobs are scenario overrides, not a separate execution surface:

```python
session = rm.Session()
run = session.run_scenario(
    scenario,
    overrides={"t_end": 2.0, "mode": "lockstep", "output": str(Path("results.csv").resolve())},
)
```

Relative output paths are resolved beside the scenario file; an absolute path
as above writes into the current working directory.

## Inspecting a model

| Accessor | Returns |
| --- | --- |
| `m.states`, `m.algebraics`, `m.inputs`, `m.outputs` | `VarView` of `VariableInfo` |
| `m.parameters` | `ParamView` of `ParameterInfo` (`.value`, `.kind`) |
| `m.structure()` | `StructuralInfo` (counts, balance, BLT blocks, algebraic loops) |
| `m.to_dict("dae")` / `m.to_json("dae")` | the IR, only when you ask |

Views behave like sequences: `len(m.states)`, `m.states[0]`, `m.states["x"]`,
`for v in m.states`, `m.states.names`. An unknown name raises a `KeyError` that
suggests the closest match.

## Parameter sweeps

Tunable parameters change without recompiling, so a sweep compiles once:

```python
for rate in [0.0, 0.05, 0.1]:
    r = m.with_params(k=rate).simulate(t=(0, 10))
    print(rate, r["x"][-1])
```

`m.simulate(params={...}, start={...})` applies overrides for a single run.
Overriding a **structural** parameter (one that affects sizing/instantiation)
raises `StructuralParamError`; it needs a recompile. Dependent parameter values
are recomputed when their tunable inputs change.

A structural parameter *can* be changed by re-instantiating the model — array
dimensions and conditional components re-evaluate:

```python
array_model = session.loads("""
model ArrayDecay
  parameter Integer n = 2 annotation(Evaluate=true);
  parameter Real k = 0.5;
  Real x[n](each start=1, each fixed=true);
equation
  for i in 1:n loop
    der(x[i]) = -k*x[i];
  end for;
end ArrayDecay;
""", model="ArrayDecay")
m3 = array_model.with_params(n=3, recompile=True)
r3 = m3.simulate(t=(0, 1), dt=0.1)
```

## Live symbolic export

CasADi and JAX exports consume the same checked, computable Solve program used
by compiled backends. A `SolveExport` exposes
`xdot = rhs(t, x, u, p)`, `state_names`, `input_names`, and `parameter_names`:

```python
casadi_model = m.to_casadi()       # rhs is a differentiable ca.Function
jax_model = m.to_jax()             # rhs supports JIT and differentiation
```

These render the same tested codegen targets used by `m.codegen(target)`, so the
live object and the generated files never drift. For writing files instead, use
the same model:

```python
written = m.codegen("casadi-ode").save_all("out/casadi")
```

For build systems that need a single callable operation, use:

```python
session = rm.Session()
written = session.codegen_file(
    "examples/models/GalecCounter.mo",
    "GalecCounter",
    "galec-production",
    "generated/galec",
)
```

## CMake Integration

CMake needs an executable build step, but downstream projects should not invent
a second Rumoca CLI. Use the Python interpreter as the executable and call the
stable API:

Save the following as `CMakeLists.txt` in the repository root, then configure
and build it with a Python interpreter that has Rumoca installed. It uses the
same tracked `GalecCounter` example as the Python code above:

```cmake
cmake_minimum_required(VERSION 3.20)
project(RumocaExamples NONE)
find_package(Python3 REQUIRED COMPONENTS Interpreter)

add_custom_command(
  OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/rumoca-codegen.stamp"
  COMMAND "${Python3_EXECUTABLE}" -c
          "import pathlib, rumoca as rm; \
session = rm.Session(); \
session.codegen_file(r'${CMAKE_CURRENT_SOURCE_DIR}/examples/models/GalecCounter.mo', 'GalecCounter', \
'galec-production', r'${CMAKE_CURRENT_BINARY_DIR}/generated/galec'); \
pathlib.Path(r'${CMAKE_CURRENT_BINARY_DIR}/rumoca-codegen.stamp').write_text('ok\\n')"
  DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/examples/models/GalecCounter.mo"
  VERBATIM
)
add_custom_target(generate_rumoca ALL
  DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/rumoca-codegen.stamp")
```

Append this target to run the tracked simulation scenario with
`cmake --build <build-directory> --target run_rumoca_scenario`:

```cmake
add_custom_target(run_rumoca_scenario
  COMMAND "${Python3_EXECUTABLE}" -c
          "import rumoca as rm; rm.Session().run_scenario(r'${CMAKE_CURRENT_SOURCE_DIR}/examples/simulation/rumoca-scenario.sympy_decay.toml')"
  VERBATIM
)
```

Reusable CMake modules should be thin wrappers around `Session.run_scenario`
and `Session.codegen_file`; they should not parse scenario TOML or probe
private binding names. Create one session per build step and reuse it for all
Rumoca work in that step so compiler caches stay warm.

## Jupyter: the `%%modelica` magic

With `rumoca[notebook]`, write Modelica in a cell and get back a typed object:

```text
%%modelica -m Decay --name m
model Decay Real x(start=1); equation der(x) = -x; end Decay;
```

`%%modelica sim ...` returns a `Result`; `%%modelica export casadi ...` returns a
`SolveExport`. `--name VAR` also binds the result into the notebook namespace.

## Diagnostics

`rm.validate(path)` / `rm.validate_source(src)` return a list of `Diagnostic`
objects and never raise on model errors. `Session.load` / `Session.loads` /
`Model.simulate` raise the typed hierarchy — `RumocaError` and its subclasses
`ParseError`, `CompileError`, `SimulationError`, `StructuralParamError` — each
carrying a message that teaches.

```python
diagnostics = rm.validate("examples/models/SympyDecay.mo")
invalid = rm.validate_source("model Broken Real x = ; end Broken;")
```

The required Linux x86_64 installed-wheel check is
`cargo xtask python test --require-guide`; it also runs in `cargo xtask verify full`.
It builds the current source unless `--wheel` supplies a wheel, and uses
`--python` to select an interpreter provisioned with the optional dependencies
above. The same check executes these Python and notebook examples, both CMake
build steps, and the four linked SympyDecay codegen scenarios. Other wheel
platforms run the base API checks without claiming this guide qualification.
