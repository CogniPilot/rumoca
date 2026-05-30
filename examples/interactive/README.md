# Interactive Examples

Interactive scenarios use the same `rum.toml` TOML format as batch simulation, but
add viewer, input, and transport configuration.

```bash
cargo run -p rumoca --release -- sim -c examples/interactive/rover/rum.toml
cargo run -p rumoca --release -- sim -c examples/interactive/quadrotor/rum.acro.toml
```

- `rover/`: standalone Ackermann rover controlled by keyboard or gamepad.
- `quadrotor/`: 6-DOF quadrotor with closed-loop Modelica control.

Open the HTTP URL printed by the CLI to view the scene.
