# Interactive Simulation

Interactive simulation uses the same `rum.toml` scenario format as batch
simulation. The scenario selects the model, solver, viewer, input routing, and
transport ports.

Run the quadrotor SIL example:

```bash
cargo run -p rumoca --release -- \
  sim -c examples/interactive/quadrotor/rum.acro.toml
```

Run the rover SIL example:

```bash
cargo run -p rumoca --release -- \
  sim -c examples/interactive/rover/rum.toml
```

The CLI prints the HTTP and WebSocket endpoints for the viewer. Open the HTTP
URL in a browser if it does not open automatically.

## Input Routing

Keyboard, gamepad, and viewer inputs are routed through the generic Rumoca
input path. `rum.toml` scenarios describe which signals drive which model inputs.

The runtime supports three pacing styles:

- as fast as possible
- realtime
- lockstep, where the simulation advances only when an external packet arrives

Use realtime for human-in-the-loop browser control. Use lockstep for external
interfaces that own the timing.

## Viewer Modes

The built-in results panel is useful for plots. External web viewers are useful
for 3D scenes and interactive SIL. Both are launched from `rum.toml` scenarios so the
same config can be used from CLI and editor tooling.
