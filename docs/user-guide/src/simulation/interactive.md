# Interactive Simulation

Interactive simulation is regular `task = "simulate"` with live inputs enabled.
The scenario still owns the model, solver, and output/viewer panels; `[sim]`
chooses the clock policy, `[input]` maps devices into local state, and
`[signals.stepper_inputs]` routes that state into Modelica `input` variables.
External browser scenes are just one presentation surface for that same run.

## Running the Examples

The interactive examples live under `examples/interactive/`. They use the
same scenario settings surface as batch simulations, but enable input routing
and viewer presentation.

### Quadrotor SIL

```modelica,interactive
// rumoca-live-scenario: ../repo-examples/interactive/quadrotor/rumoca-scenario.acro.toml
```

Native run:

```bash
cargo xtask repo modelica-deps ensure
cargo run -p rumoca --release -- \
  sim -c examples/interactive/quadrotor/rumoca-scenario.acro.toml
```

### Rover

```modelica,interactive
// rumoca-live-scenario: ../repo-examples/interactive/rover/rumoca-scenario.toml
```

Native run:

```bash
cargo run -p rumoca --release -- \
  sim -c examples/interactive/rover/rumoca-scenario.toml
```

### Fixed-Wing SIL

```modelica,interactive
// rumoca-live-scenario: ../repo-examples/interactive/fixedwing/rumoca-scenario.toml
```

Native run:

```bash
cargo xtask repo modelica-deps ensure
cargo run -p rumoca --release -- \
  sim -c examples/interactive/fixedwing/rumoca-scenario.toml
```

The CLI prints the HTTP and WebSocket endpoints for the viewer. Open the
HTTP URL in a browser if it does not open automatically. Use release builds
— interactive simulation is real-time work.

## Quadrotor Scenario Shape

The quadrotor example is the reference shape:

```toml
[rumoca]
version = "1"
task = "simulate"

[model]
file = "QuadrotorSIL.mo"
name = "QuadrotorAcro"

[sim]
dt = 0.01
mode = "realtime"
solver = "rk-like"

[input]
mode = "auto"
```

Input mappings are scenario-level simulation settings, not viewer settings.
The same file then declares local state, keyboard/gamepad bindings, and the
bridge into the model:

```toml
[locals.throttle]
default = 0.0
type = "float"

[input.gamepad.integrators.throttle]
source = "LeftStickY"
write = "throttle"
deadband = 0.1
rate = 0.7
clamp = [0.0, 1.0]

[input.keyboard.keys.Space]
action = "toggle"
state = "armed"
debounce_ms = 500
precondition = "throttle <= 0.05"

[signals.stepper_inputs]
stick_throttle = "local:throttle"

[signals.stepper_inputs.armed]
from = "local:armed"
when_false = 0.0
when_true = 1.0
```

The external 3D browser view is presentation:

```toml
[transport.http]
port = 8080
scene = "quadrotor_scene.js"
asset_dir = "../../assets"

[transport.websocket]
port = 8081

[viewer]
status_title = "Quadrotor"
show_armed = true
```

Open **Settings** on the scenario in VS Code, the playground, or the user guide
to edit the same TOML through typed controls: input enablement, locals, keys,
gamepad axes/buttons/integrators, model-input routes, solver clocking, and
viewer panels.

## How a Scenario Runs Live

A scenario with only `[model]` and `[sim]` is a regular simulation. Live input
and live presentation are independent additions:

- `[transport.http]` + `[transport.websocket]` — serve the browser viewer
  (`scene = "my_scene.js"` selects the 3D scene file). This asks for an
  external web surface.
- `[input]`, `[locals]`, `[signals.stepper_inputs]` — named runner state and
  routing from input devices to model `input` variables. Input alone does not
  select a special viewer.
- `[transport.udp]` + `[schema]`/`[receive]`/`[send]` — couple an external
  process (e.g. a flight controller) over FlatBuffers/UDP.

See [Scenario Files](./scenario-tomls.md) for each section.

## Pacing

The runtime supports three pacing styles via `[sim] mode`:

| Mode | Behavior | Use |
|---|---|---|
| `as_fast_as_possible` | No sleeping; drain inputs | Batch-like exploration |
| `realtime` | Sleep to wall-clock `dt`, zero-order-hold inputs | Human-in-the-loop browser control |
| `lockstep` | Step only when an external packet arrives | External interfaces that own the timing |

Defaults: `lockstep` when external coupling is configured, otherwise
`realtime`.

## Input Routing

Keyboard, gamepad, and browser inputs go through the generic Rumoca input path.
The model side is plain Modelica `input` variables, so the same model runs in
batch (inputs from equations or defaults) and live input runs (inputs from
devices) without changing the Modelica source.

## Viewer Surfaces

Viewer surfaces are separate from input routing. Standard timeseries/scatter
panels use the results viewer. External web scenes use the native realtime
runner and the configured scene script. Both are launched from scenarios, so
the same configuration works from the CLI, VS Code, and editor tooling.
