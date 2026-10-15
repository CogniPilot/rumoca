# Inspecting and Debugging Models

When a model misbehaves — fails to compile, fails to initialize, or produces
wrong dynamics — Rumoca gives you structured views into every stage of the
pipeline. All of these work with both `rumoca compile` and `rumoca sim`.

## Dump an Intermediate Representation

`--emit` prints the model as the compiler sees it after each stage:

| Stage | What you see |
|---|---|
| `ast-json` | The parser's syntax tree of the file you pass, before name resolution |
| `flat-mo` / `flat-json` | The flattened model: hierarchy and `connect`s expanded |
| `dae-mo` / `dae-json` | The DAE system: equations partitioned, ready for analysis |
| `solve-json` | The solver IR: sorted, torn, scheduled for execution |

```bash
rumoca compile Model.mo --emit flat-mo          # to stdout
rumoca compile Model.mo --emit dae-json -o m.json
```

`ast-json` is the one stage before name resolution. It dumps the file you
pass exactly as the parser read it: `--model` and `--source-root` are
accepted but ignored, so the dump covers the whole file and stays available
while a library the file references is missing or does not resolve. Every later stage needs the
model's whole class closure, so `flat-mo` and beyond still report unresolved
names. For the Modelica Standard Library that means naming the directory
that holds `Modelica/`, `ModelicaServices/`, and `Complex.mo` as one
`--source-root`, since `Modelica` references both of the other top-level
packages.

`--emit` belongs to `rumoca compile`; a scenario file has no equivalent key
and `rumoca sim --config` does not dump stages. To inspect the model a
scenario runs, pass its `[model]` entries and `source_roots` to `compile`:

```bash
rumoca compile models/Ball.mo --model Ball --source-root ../modelica_libraries --emit ast-json
```

Reading `flat-mo` answers "what did my modifications and connects actually
produce?". Reading `dae-mo` answers "what equation system is the solver
given?" — the live examples in this book expose the same view through their
**Show DAE** button.

The JSON dumps are for debugging and for replay with the same compiler build,
not a stable interface. `dae-json` carries a `schema_version` that changes as
the IR evolves, and its reader accepts only the current version; `flat-json`
has no version. For a model's inputs and outputs, read the `modelDescription.xml`
of an `fmi2` or `fmi3` export instead.

In `dae-json`, a variable's `role` is its runtime classification (`state`,
`algebraic`, `output`, `discrete_value`, ...), not the declared prefix: a
declared `output` that is a state has role `state`, and a discrete one has
role `discrete_value`. The declared prefix is `declared_causality` (`none`,
`input`, or `output`), kept for nested components too, while `causality` is
`input` or `output` only at the top level. FMI exports mark a nested
declaration's prefix with a per-variable annotation and keep its causality
`local`:

```xml
<!-- FMI 2, inside the ScalarVariable after its type element -->
<Annotations><Tool name="rumoca"><DeclaredCausality value="output"/></Tool></Annotations>
<!-- FMI 3, first child of the variable -->
<Annotations><Annotation type="rumoca.declaredCausality">output</Annotation></Annotations>
```

## Structural Analysis

```bash
rumoca compile Model.mo --inspect structure
```

Prints the structural preparation of the system: the matching between
equations and unknowns, the block lower-triangular (BLT) ordering,
simultaneous (coupled) blocks, and tearing decisions. This is the first
place to look when compilation fails with *structurally singular system* —
it names the unmatched equations and unknowns.

## Numerical Evaluation at a Point

```bash
rumoca sim Model.mo --inspect eval
rumoca sim Model.mo --inspect eval --at "x=1.5,v=0@2.0"
```

Evaluates all solver values and state derivatives at a point and names any
non-finite results — the fastest way to find the division-by-zero or domain
error behind a NaN. With no `--at`, it evaluates at the initial state (which
also discovers the state names for you).

The `--at` syntax is `<name=value,...@t>`: states by name, unset states keep
their initial values, time after `@` (default 0).

## Jacobian Analysis

```bash
rumoca sim Model.mo --inspect jacobian --at "x=1.0@0"
```

Prints the dense state Jacobian at a point and flags singular columns and
zero pivots — useful for diagnosing initialization failures and stiff or
degenerate dynamics.

## NaN Tracing

When a simulation fails with a non-finite value, `rumoca sim` automatically
re-runs with NaN tracing to locate the offending variables, so the
diagnostic names the variable instead of just reporting a solver failure.

## Performance

```bash
rumoca sim bench Model.mo            # compile / prepare / hot-loop timing
rumoca cache status                  # compilation cache usage
```

## Verbose Compilation

```bash
rumoca compile Model.mo --target c-ode -o out -v
```

`-v` prints friendly `[rumoca] Phase ...` progress lines, which localizes
slow or failing phases on large models.
