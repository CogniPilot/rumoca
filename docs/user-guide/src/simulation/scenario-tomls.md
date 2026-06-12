# Scenario Files (rum.toml)

Rumoca scenarios are plain TOML files and the preferred way to run
repeatable simulation and code generation jobs. They follow a filename
convention — `rum.toml` for the default scenario and `rum.<profile>.toml`
for named profiles (such as `rum.f16.toml` or `rum.bench.toml`) — and live
next to the model they operate on. The filename is the editor/discovery
hook; the required `[rumoca]` marker section is the authoritative
declaration.

Each scenario describes **one runnable thing**. That keeps VS Code, the CLI,
and the playground aligned: the play button runs the active scenario instead
of guessing from a `.mo` file.

## Getting Started

```bash
rumoca sim init > rum.toml    # commented starter template
rumoca sim check -c rum.toml  # validate without running
rumoca sim -c rum.toml        # run
```

## A Minimal Batch Scenario

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

Paths are resolved relative to the `rum.toml` file.

## Section Reference

### `[rumoca]` (required)

The marker section. `version = "1"` declares the schema version;
`task = "simulate"` runs the model, `task = "codegen"` renders a target into
an output directory.

### `[model]` (required)

```toml
[model]
file = "MyVehicle.mo"   # relative to this scenario
name = "MyVehicle"      # top-level class to compile
```

Use the top-level `source_roots` key for package dependencies needed by this
scenario:

```toml
source_roots = ["../modelica_libraries"]
```

Workspace-wide library paths (MSL, CMM) belong in editor settings, not in
every scenario; scenario `source_roots` are for paths specific to this run.

### `[sim]`

```toml
[sim]
dt = 0.01          # simulation timestep [s]
t_end = 10.0       # batch/report stop time; interactive runners may ignore it
solver = "auto"    # auto | bdf | esdirk34 | trbdf2 | rk-like
output = "results.html"
mode = "realtime"  # optional pacing, see below
```

`mode` selects runner pacing:

| Mode | Behavior |
|---|---|
| `as_fast_as_possible` | Drain available inputs and run without sleeping |
| `realtime` | Zero-order-hold inputs, sleep to wall-clock `dt` |
| `lockstep` | Wait for each external packet before stepping |

The default is `lockstep` when external coupling is configured and
`realtime` standalone.

### `[[plot.views]]`

Each view adds a plot to the batch report:

```toml
[[plot.views]]
id = "states_time"
title = "States vs Time"
type = "timeseries"
x = "time"
y = ["x", "v"]
```

### `[transport.*]` — interactive viewer and coupling

HTTP and WebSocket transports serve the interactive browser viewer:

```toml
[transport.websocket]
port = 8081

[transport.http]
port  = 8080
scene = "my_scene.js"  # 3D scene, relative to this scenario
```

UDP is only needed when coupling to an external process:

```toml
[transport.udp]
listen = "0.0.0.0:4244"
send   = "127.0.0.1:4242"

[external_interface]
command = "/path/to/external-process"
```

### `[schema]`, `[receive]`, `[send]` — external interface coupling

These three sections are all-or-nothing. Provide them to couple via
FlatBuffers over UDP; omit all three for standalone mode (gamepad/keyboard
drive the model inputs directly).

```toml
[schema]
bfbs = ["/path/to/your_schema.bfbs"]

[receive]
root_type = "your.namespace.MotorOutput"

[receive.route]
"motors.m0" = { to = "stepper:omega_m1", scale = 1100.0 }
"armed"     = { to = "local:armed" }

[send]
root_type = "your.namespace.SimInput"

[send.route]
"gyro.x" = { key = "gyro_x" }
```

### `[locals]` — named persistent runner state

```toml
[locals]
throttle = { type = "float", default = 0.0 }
my_flag  = { type = "bool", default = false }
```

Types are `"bool"`, `"float"`, or `"array"` (with `element` and `len`).
`default` is optional.

### `[signals]`, `[input]` — input routing

Map keyboard, gamepad, and viewer inputs onto model `input` variables for
interactive runs. The worked interactive examples are the best reference:

- `examples/interactive/quadrotor/rum.acro.toml`
- `examples/interactive/rover/rum.toml`

## Validation

`rumoca sim check -c <file>` validates structure and paths without running.
The VS Code extension surfaces the same validation when editing scenario
files.
