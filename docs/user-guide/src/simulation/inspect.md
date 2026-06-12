# Inspecting and Debugging Models

When a model misbehaves — fails to compile, fails to initialize, or produces
wrong dynamics — Rumoca gives you structured views into every stage of the
pipeline. All of these work with both `rumoca compile` and `rumoca sim`.

## Dump an Intermediate Representation

`--emit` prints the model as the compiler sees it after each stage:

| Stage | What you see |
|---|---|
| `ast-mo` / `ast-json` | The parsed, resolved syntax tree |
| `flat-mo` / `flat-json` | The flattened model: hierarchy and `connect`s expanded |
| `dae-mo` / `dae-json` | The DAE system: equations partitioned, ready for analysis |
| `solve-json` | The solver IR: sorted, torn, scheduled for execution |

```bash
rumoca compile Model.mo --emit flat-mo          # to stdout
rumoca compile Model.mo --emit dae-json -o m.json
```

Reading `flat-mo` answers "what did my modifications and connects actually
produce?". Reading `dae-mo` answers "what equation system is the solver
given?" — the live examples in this book expose the same view through their
**Show DAE** button.

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
rumoca compile Model.mo --target sympy -o out -v
```

`-v` prints friendly `[rumoca] Phase ...` progress lines, which localizes
slow or failing phases on large models.
