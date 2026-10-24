# Interactive Examples

Interactive scenarios use the same `rumoca-scenario.toml` TOML format as batch simulation, but
add viewer, input, and transport configuration.

```bash
cargo run -p rumoca --release -- sim -c examples/interactive/rover/rumoca-scenario.toml
cargo run -p rumoca --release -- sim -c examples/interactive/quadrotor/rumoca-scenario.acro.toml
cargo run -p rumoca --release -- sim -c examples/interactive/fixedwing/rumoca-scenario.toml
cargo run -p rumoca --release -- sim -c examples/interactive/reusable_booster/rumoca-scenario.toml
cargo run -p rumoca --release -- sim -c examples/interactive/canyon_run/rumoca-scenario.toml
```

- `rover/`: standalone Ackermann rover controlled by keyboard or gamepad.
- `quadrotor/`: 6-DOF quadrotor with closed-loop Modelica control.
- `fixedwing/`: 6-DOF fixed-wing aircraft with attitude-stabilized fly-by-wire control.
- `reusable_booster/`: autonomous 6-DOF booster landing with flatness planning and `SE_2(3)` control.
- `canyon_run/`: the fixed-wing aircraft learns to fly a winding canyon with Q-learning written in Modelica.

Open the HTTP URL printed by the CLI to view the scene.
