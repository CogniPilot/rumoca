# Python API

Rumoca ships a first-class, typed Python API. You compile a model once and
everything — metadata, simulation, code generation, symbolic export — hangs off
the returned `Model`. Returned objects are real typed classes (full
autocomplete), never JSON you have to `json.loads`.

```bash
pip install rumoca            # core
pip install rumoca[data]      # + numpy/pandas (result.to_numpy / to_dataframe)
pip install rumoca[plot]      # + matplotlib (result.plot)
pip install rumoca[casadi]    # + casadi   (model.to_casadi)
pip install rumoca[all]       # everything, incl. the %%modelica magic
```

Optional dependencies are genuinely optional: `import rumoca` never needs them,
and a method that does (e.g. `result.plot()`) raises a message naming the extra
to install.

## Quick start

```python
import rumoca as rm

m = rm.load("Quadrotor.mo", model="Quadrotor", roots=["libs/CMM"])
m                                  # repr shows a summary
m.parameters["mass"].value         # typed, autocompletes
m.parameters["mass"].kind          # "tunable" or "structural"
m.states["body.v[1]"]              # subscripted names resolve

r = m.simulate(t=(0, 10), dt=0.01)
r.plot("body.v[1]")                # needs rumoca[plot]
df = r.to_dataframe()              # needs rumoca[data]
```

Compile from a string with `rm.loads(source, model=...)`. Reuse a configured
`rm.Session(roots=[...])` to keep source roots and caches across calls.

Or load everything from a `rumoca-scenario.toml` in one call — its model, source
roots, and solver settings (paths resolved relative to the file):

```python
session, model, config = rm.Session.from_scenario("rumoca-scenario.toml")
r = model.simulate(t=(0, 10), config=config)   # config carries solver/dt
```

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
for drag in [0.0, 0.05, 0.1]:
    r = m.with_params(rotor_drag=drag).simulate(t=(0, 10))
    ...
```

`m.simulate(params={...}, start={...})` applies overrides for a single run.
Overriding a **structural** parameter (one that affects sizing/instantiation) or
a parameter another parameter's value depends on raises `StructuralParamError` —
those need a recompile, not a runtime override, so a sweep is never silently
wrong.

A structural parameter *can* be changed by re-instantiating the model — array
dimensions and conditional components re-evaluate:

```python
m6 = m.with_params(n_rotors=6, recompile=True)   # re-instantiate with n_rotors=6
m6.simulate(t=(0, 10))
```

## Live symbolic export

Turn a model into a live object in the symbolic framework of your choice — no
file dance:

```python
cm = m.to_casadi()                 # CasadiModel (form="dae")
cm.ode                             # explicit RHS (CasADi's native DAE form)
cm.dae                             # {x, z, p, t, ode, alg} — feed ca.integrator
S  = cm.jacobian("ode", "p")       # exact AD parameter sensitivity

sm = m.to_sympy()                  # SymPy model; sm.solve_explicit()
jm = m.to_jax()                    # JAX ode_fn + diffrax simulate
```

Pass `form="solve"` for the scalarized, causalized explicit form (the same
source the C/FMI/Rust backends use) — you get a `SolveExport` exposing the
explicit right-hand side `xdot = rhs(x, u, p)` plus `state_names`/
`parameter_names`:

```python
se = m.to_casadi(form="solve")     # SolveExport; se.rhs is a ca.Function
se = m.to_jax(form="solve")        # SolveExport; se.rhs is jit/grad/vmap-able
```

These render the same tested codegen targets used by `m.codegen(target)`, so the
live object and the generated files never drift. For writing files instead, use
`m.codegen("casadi-mx").save_all("out/")`.

## Jupyter: the `%%modelica` magic

With `rumoca[notebook]`, write Modelica in a cell and get back a typed object:

```text
%%modelica -m Decay --name m
model Decay Real x(start=1); equation der(x) = -x; end Decay;
```

`%%modelica sim ...` returns a `Result`; `%%modelica export casadi ...` returns a
`CasadiModel`. `--name VAR` also binds the result into the notebook namespace.

## Diagnostics

`rm.validate(path)` / `rm.validate_source(src)` return a list of `Diagnostic`
objects and never raise on model errors. `load`/`loads`/`simulate` raise the
typed hierarchy — `RumocaError` and its subclasses `ParseError`, `CompileError`,
`SimulationError`, `StructuralParamError` — each carrying a message that teaches.
