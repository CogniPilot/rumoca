# Running Simulations

Rumoca offers two ways to run a simulation. They share the same compiler and
runtime; the difference is where the configuration lives.

## Direct Runs

Point `rumoca sim` at a `.mo` file for quick, one-off runs:

```bash
rumoca sim Ball.mo --model Ball --t-end 10 --solver rk-like
```

Everything is a CLI flag: `--t-end` (default 1.0), `--dt`, `--solver`,
`--source-root`, `--output`. The result is an HTML report with interactive
plots of every variable (default `<MODEL>_results.html`).

Direct runs are great while developing a model. As soon as a run has
settings worth repeating, switch to a scenario.

## Scenario Runs

A scenario is a `rum.toml` file colocated with the model it runs:

```bash
rumoca sim -c examples/simulation/rum.ball.toml
```

The scenario records the model file and name, simulation settings, plots,
viewer/transport configuration, and source roots — one runnable thing per
file. This keeps the CLI, VS Code, and the playground aligned: the play
button runs the scenario instead of guessing solver and source roots from a
bare `.mo` file.

See [Scenario Files](./scenario-tomls.md) for the format.

## What a Run Produces

- **Batch runs** write an HTML report with time-series plots of all
  variables, simulation details (solver, tolerances, timing), and any
  termination message (`terminate(...)` in the model).
- **Interactive runs** (scenarios with transports/viewer sections) launch
  the runtime with a browser viewer and input routing instead — see
  [Interactive Simulation](./interactive.md).

## Benchmarking

`rumoca sim bench` measures compile time, preparation time, and hot
simulation throughput separately — useful when you care about iteration
speed on a large model or are comparing solver settings:

```bash
rumoca sim bench Ball.mo --model Ball
rumoca sim bench -c rum.toml
```

## Caching

Compilation artifacts are cached under the platform cache directory, so
repeated runs of unchanged models skip recompilation. `rumoca cache status`
shows usage; `rumoca cache prune` trims it. `--cache-dir` overrides the
location for any command.
