# Rover SIL example

Standalone Ackermann-steered rover demo — no controller, no autopilot, no
FlatBuffer protocol. Gamepad (or keyboard) drives the model inputs
directly.

## Files

| File | Role |
|---|---|
| `Rover.mo` | Plant model (kinematic bicycle; 5 states: x, y, θ, v, δ) |
| `rover.toml` | Config: locals, input bindings, direct `[signals.stepper_inputs]` |
| `rover_scene.js` | Three.js desert scene: rover body + 4 wheels (front pair steers) + chase camera |

## Controls

| Input | Action |
|---|---|
| Gamepad left stick Y | throttle (forward / reverse) |
| Gamepad right stick X | steering (left / right) |
| Keyboard ↑/↓ | throttle |
| Keyboard ←/→ | steering |
| Keyboard R | reset |
| Keyboard Q | quit |

The Start button is intentionally unbound — there is nothing to arm in
standalone mode, and binding it to reset led to accidental resets.

## Running

```
cargo run -p rumoca --features sim --release -- \
  sim run -c examples/rover_sil/rover.toml
```

The `[physics].file` and `[transport.http].scene` values in the config
are resolved relative to the TOML's directory, so no other flags are
needed.

Then open [http://localhost:8080](http://localhost:8080).

## Plant model

Standard kinematic bicycle (Ackermann steering at the front, no rear
slip):

```
der(v)     = (throttle * v_max - v) / tau_speed
der(delta) = (steering * delta_max - delta) / tau_steer
der(x)     = v * cos(theta)
der(y)     = v * sin(theta)
der(theta) = v * tan(delta) / wheelbase
```

Defaults: `wheelbase = 0.35 m`, `wheel_radius = 0.06 m`, `v_max = 4 m/s`,
`delta_max = 0.6 rad`, `tau_speed = 0.25 s`, `tau_steer = 0.10 s`. The
rover follows the tangent of the front wheels by construction, so it
cannot skid.

## Standalone mode (no FB / no autopilot)

This example omits the `[controller]`, `[autopilot]`, `[schema]`,
`[receive]`, and `[send]` sections entirely. The sim runtime detects
this and skips wrapper synthesis, UDP socket binding, and FB codec
compilation — see
[SimulationConfig::has_fb](../../crates/rumoca-session/src/config.rs).

Model inputs (`throttle`, `steering`) are wired straight from local
state via `[signals.stepper_inputs]`:

```toml
[signals.stepper_inputs]
throttle = "local:throttle"
steering = "local:steering"
```

The input engine writes those locals from the gamepad axes; the sim
loop applies them to the stepper each frame. No external process.

## Scene-visualization helpers

`Rover.mo` exposes three derived outputs purely for the viewer:

- `wheel_rpm = v / wheel_radius` — drives wheel-roll animation
- `front_wheel_yaw = delta` — yaws the two front-wheel pivot groups
- `yaw_rate = der(theta)` — available for HUDs / chase-cam smoothing

The Three.js scene uses the same desert environment as the quadrotor
example.
