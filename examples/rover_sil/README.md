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

## Running (once CLI Phase 5 lands)

```
rumoca sim-fb \
  --config examples/rover_sil/rover.toml \
  --model examples/rover_sil/Rover.mo \
  --scene examples/rover_sil/rover_scene.js
```

Then open [http://localhost:8080](http://localhost:8080).

## Standalone mode vs autopilot coupling

This example omits the `[schema]`, `[receive]`, and `[send]` sections
entirely. The sim-fb runtime detects this and skips UDP socket binding and
FB codec compilation — see
[SimFbConfig::has_fb](../../crates/rumoca-sim-fb/src/config.rs).

Model inputs (`forward_cmd`, `turn_cmd`) are wired from local state via
`[signals.stepper_inputs]`:

```toml
[signals.stepper_inputs]
forward_cmd = "local:forward_cmd"
turn_cmd    = "local:turn_cmd"
```

The input engine writes to those locals from the gamepad axes; the sim
loop applies them to the stepper each frame. No external process.
