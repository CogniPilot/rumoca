# Interactive Simulation

Interactive simulation runs a model continuously with live inputs — keyboard,
gamepad, browser controls, or an external process — and streams the state to
a browser viewer, including 3D scenes. It uses the same `rum.toml` scenario
format as batch simulation; the scenario selects the model, solver, viewer,
input routing, and transport ports.

## Running the Examples

Quadrotor software-in-the-loop:

```bash
cargo run -p rumoca --release -- \
  sim -c examples/interactive/quadrotor/rum.acro.toml
```

Rover:

```bash
cargo run -p rumoca --release -- \
  sim -c examples/interactive/rover/rum.toml
```

The CLI prints the HTTP and WebSocket endpoints for the viewer. Open the
HTTP URL in a browser if it does not open automatically. Use release builds
— interactive simulation is real-time work.

## How a Scenario Becomes Interactive

A scenario with only `[model]` and `[sim]` produces a batch HTML report.
Adding transports and input/viewer sections launches the interactive runner
instead:

- `[transport.http]` + `[transport.websocket]` — serve the browser viewer
  (`scene = "my_scene.js"` selects the 3D scene file).
- `[locals]`, `[signals]` — named runner state and routing from input
  devices to model `input` variables.
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

Keyboard, gamepad, and viewer inputs go through the generic Rumoca input
path; the scenario describes which signals drive which model inputs. The
model side is plain Modelica `input` variables, so the same model runs in
batch (inputs from equations or defaults) and interactively (inputs from
devices) without modification.

## Viewer Modes

The built-in results panel is for plots. External web viewers handle 3D
scenes and interactive SIL. Both are launched from scenarios, so the same
configuration works from the CLI, VS Code, and editor tooling.
