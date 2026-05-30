# Rumoca Scenarios

Rumoca scenarios are plain TOML files and are the preferred way to run
repeatable simulations and generation jobs. They follow a filename convention —
`rum.toml` for the default scenario and `rum.<profile>.toml` for named profiles
(such as `rum.f16.toml` or `rum.bench.toml`) — and keep them next to the model
or example they operate on. The filename is the editor/discovery hook; the
required `[rumoca]` marker section is the authoritative declaration.

```toml
[rumoca]
version = "1"
task = "simulate"

[model]
file = "../models/Ball.mo"
name = "Ball"

[sim]
solver = "rk-like"
t_end = 10.0

[[plot.views]]
id = "states_time"
title = "States vs Time"
type = "timeseries"
x = "time"
y = ["x", "v"]
```

Paths are resolved relative to the `rum.toml` file. Use top-level `source_roots`
for dependencies needed by this scenario, such as MSL or an example package
tree.
Repository scenarios keep reusable models under `examples/models` and reference
them from simulation, codegen, or interactive scenario directories with
relative paths.

## Tasks

`task = "simulate"` runs the model and opens or writes the configured viewer
output. `task = "codegen"` renders a target into an output directory.

Each `rum.toml` file should describe one runnable thing. That keeps VS Code, the
CLI, and the WASM playground aligned: the play button runs the active scenario instead of
guessing from a `.mo` file.

## Validation

Validate a simulation scenario without running it:

```bash
cargo run -p rumoca -- sim check -c examples/simulation/rum.ball.toml
```

For a starting point:

```bash
cargo run -p rumoca -- sim init > rum.toml
```
