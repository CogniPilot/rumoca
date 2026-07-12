# Rover SIL example

Standalone Ackermann-steered rover demo — no controller, no external interface, no
FlatBuffer protocol. Gamepad (or keyboard) drives the model inputs
directly.

## Files

| File | Role |
|---|---|
| `Rover.mo` | Plant model (kinematic bicycle; 5 states: x, y, theta, v, delta) |
| `rumoca-scenario.toml` | Config: locals, input bindings, direct `[signals.model_inputs]` |
| `rover_scene.js` | Three.js desert scene: buggy GLB model + chase camera |

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
cargo run -p rumoca --release -- \
  sim -c examples/interactive/rover/rumoca-scenario.toml
```

The `[model].file` and `[transport.http].scene` values in the config
are resolved relative to the scenario's directory, so no other flags are
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

## Standalone mode (no FB / no external interface)

This example omits `[external_interface]`, `[schema]`, `[receive]`, and
`[send]` entirely. The sim runtime detects this and skips UDP socket binding
and FB codec compilation.

Model inputs (`throttle`, `steering`) are wired straight from local
state via `[signals.model_inputs]`:

```toml
[signals.model_inputs]
throttle = "local:throttle"
steering = "local:steering"
```

The input engine writes those locals from the gamepad axes; the sim
loop applies them to the session each frame. No external process.

## Scene-visualization helpers

`Rover.mo` exposes three derived outputs that are available to the viewer:

- `wheel_rpm = v / wheel_radius`
- `front_wheel_yaw = delta`
- `yaw_rate = der(theta)`

The Three.js scene uses the same desert environment as the quadrotor
example.

## 3D Model Assets

The rover visual model uses `examples/assets/models/buggy.glb`, loaded by
`rover_scene.js`.

Model asset licenses and attributions are recorded in
[`../../assets/models/README.md`](../../assets/models/README.md).
