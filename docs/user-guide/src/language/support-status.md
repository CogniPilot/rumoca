# Language Support Status

Rumoca implements a substantial and growing subset of the Modelica language.
This page describes what you can rely on today and where the edges are. The
ground truth is the continuously-run Modelica Standard Library (MSL) quality
gate, which compiles and simulates MSL models on every change and blocks
regressions (documented in the
[Rumoca Dev Guide](https://cognipilot.github.io/rumoca/dev-guide/) book).

## What Works Well

- **Core equation modeling** — models, blocks, packages, records,
  functions, `extends`, modifications, nested components, `connect` with
  potential/`flow` semantics.
- **Continuous dynamics** — DAE compilation with structural analysis:
  matching, sorting (BLT), algebraic-loop tearing, and dummy-derivative
  index reduction for many higher-index systems.
- **Events** — `when`/`elsewhen`, `pre`, `edge`, `reinit`, `sample`,
  if-expressions with event generation, `assert`, `terminate`.
- **Arrays and regular stencil loops** — declarations, slicing, common array
  builtins, and source-proven affine stencils from `for`-equation domains.
  Targets that do not have native stencil kernels still receive exact scalar
  fallback code.
- **Initialization** — `start` attributes and initial equation handling.
- **Experiment annotations** — `StopTime`, `StartTime`, `Tolerance`,
  `Interval`, `Solver` are parsed and used by scenario and browser runs.

## Known Limitations

- **High-index DAEs**: index reduction handles many mechanical-style
  systems, but some structurally singular formulations (for example the
  Cartesian pendulum with an explicit length constraint) are still rejected
  with a *structurally singular system* diagnostic. Reformulate with
  generalized coordinates, or watch the release notes — this area is under
  active development.
- **Event-heavy models with implicit solvers**: the default (`auto`)
  solver currently selects an implicit method that can stall on rapidly
  switching models. Workaround: `--solver rk-like` or
  `annotation(experiment(Solver = "rk-like"))`.
- **Arbitrary large libraries**: expect better results with explicit
  examples and pinned packages than with arbitrary unported libraries; the
  MSL gate tracks exactly which MSL models compile, simulate, and match
  reference results.
- **Direct CLI runs ignore `experiment` stop time** — pass `--t-end` or use
  a batch scenario file (`[sim] t_end`). Browser/live runs are
  operator-terminated and do not stop at that horizon.

## Checking a Specific Model

The fastest way to find out whether your model is supported is to compile
it:

```bash
rumoca compile MyModel.mo --emit dae-mo
```

A clean DAE dump means parsing, resolution, type checking, flattening, and
DAE lowering all succeeded. Then `rumoca sim` (or `--inspect structure`)
exercises the structural preparation and the solver. Diagnostics carry
source locations and are designed to name the offending equation or
variable.

If you hit something the compiler should support, please file an issue with
the model at <https://github.com/CogniPilot/rumoca/issues>.
