# Interactive Examples

Interactive scenarios use the same `rum.toml` TOML format as batch simulation, but
add viewer, input, and transport configuration.

```bash
cargo run -p rumoca --release -- sim -c examples/interactive/rover/rum.toml
cargo run -p rumoca --release -- sim -c examples/interactive/quadrotor/rum.acro.toml
cargo run -p rumoca --release -- sim -c examples/interactive/fixedwing/rum.toml
cargo run -p rumoca --release -- sim -c examples/interactive/sim_fb_demo/rum.sil.toml
```

- `rover/`: standalone Ackermann rover controlled by keyboard or gamepad.
- `quadrotor/`: 6-DOF quadrotor with closed-loop Modelica control.
- `fixedwing/`: 6-DOF fixed-wing aircraft with attitude-stabilized fly-by-wire control.
- `sim_fb_demo/`: quadrotor software-in-the-loop example coupled to an external binary over UDP/FlatBuffers.

Open the HTTP URL printed by the CLI to view the scene.
