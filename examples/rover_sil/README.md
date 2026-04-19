# Rover SIL example

Standalone differential-drive rover demo — no autopilot, no FlatBuffer
protocol. Gamepad (or keyboard) drives the model inputs directly.

## Files

| File | Role |
|---|---|
| `Rover.mo` | Plant model (kinematic diff-drive; 3 states: x, y, θ) |
| `rover.toml` | Config: locals, input bindings, direct `[signals.stepper_inputs]` |
| `rover_scene.js` | Three.js scene: ground + body + 4 wheels + chase camera |

## Controls

| Input | Action |
|---|---|
| Gamepad left stick Y | forward / reverse |
| Gamepad left stick X | turn |
| Gamepad Start button | reset |
| Keyboard ↑/↓ | forward / reverse |
| Keyboard ←/→ | turn left / right |
| Keyboard R | reset |
| Keyboard Q | quit |

## Running

```
cargo run -p rumoca --features sim --release -- \
  sim run -c examples/rover_sil/rover.toml
```

The `[model].file` and `[transport.http].scene` values in the config are
resolved relative to the TOML's directory, so no other flags are needed.
`--model <path>` or `--scene <path>` override them on the CLI.

Then open [http://localhost:8080](http://localhost:8080).

## Standalone mode vs autopilot coupling

This example omits the `[schema]`, `[receive]`, and `[send]` sections
entirely. The sim runtime detects this and skips UDP socket binding and
FB codec compilation — see
[SimulationConfig::has_fb](../../crates/rumoca-session/src/config.rs).

Model inputs (`forward_cmd`, `turn_cmd`) are wired from local state via
`[signals.stepper_inputs]`:

```toml
[signals.stepper_inputs]
forward_cmd = "local:forward_cmd"
turn_cmd    = "local:turn_cmd"
```

The input engine writes to those locals from the gamepad axes; the sim
loop applies them to the stepper each frame. No external process.
