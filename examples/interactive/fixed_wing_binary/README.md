# Fixed-Wing SIL

Software-in-the-loop plant model for a small fixed-wing aircraft with a
cascaded rate-command/attitude-hold fly-by-wire controller, identical to the
`examples/interactive/fixedwing/` example but with an additional **binary**
profile for autonomous external-process control.

## Profiles

| Profile | Control | How to run |
|---|---|---|
| `rum.manual.toml` | Manual — keyboard/gamepad (identical to `fixedwing/rum.toml`) | `cargo run -p rumoca --release -- sim -c examples/interactive/fixed_wing_binary/rum.manual.toml` |
| `rum.binary.toml` | Autonomous — external process speaks `CubControl` FlatBuffer over UDP | `cargo run -p rumoca --release -- sim -c examples/interactive/fixed_wing_binary/rum.binary.toml` |

All profiles share the same 6-DOF aero plant (`FixedWingSIL.mo`), the same
attitude-stabilized FBW controller, and the same desert-scene browser viewer
(`fixed_wing_scene.js` with `airplane.glb`, skybox, PBR sand ground, clouds,
rocks, cacti). Open `http://localhost:8082` in any of them.

## Files

| File | Role |
|---|---|
| `FixedWingSIL.mo` | `FixedWingPlant` (aero + tricycle gear) + `FixedWingFBW` (cascaded rate-command/attitude-hold) + `FixedWing` (closed-loop wrapper with Euler-angle outputs) |
| `rum.manual.toml` | Manual keyboard/gamepad profile |
| `rum.binary.toml` | Autonomous binary profile via `[external_interface]` + UDP FlatBuffers |
| `fixed_wing_outer_loop_stub.py` | Reference autonomous waypoint controller (takes off, climbs, navigates waypoints) |
| `fixed_wing_scene.js` | Three.js desert scene with rigged airplane.glb (control surfaces driven from sim state) |
| `verify_sil.py` | Headless telemetry capture and sanity check |
| `../../assets/skybox/` | `arid2` desert skybox (shared) |
| `../../assets/sand_pbr/` | Tileable PBR sand texture maps (shared) |
| `../../assets/models/airplane.glb` | Aircraft model with riggable control-surface pivots (shared) |

## Manual Flight

Aircraft starts at rest on the runway. Press **Space** to arm, hold **W** for
full throttle, and ease back (**↑**) to rotate around 10 m/s.

## Binary (Autonomous) Profile

The `rum.binary.toml` profile launches `fixed_wing_outer_loop_stub.py` as an
external process. The stub receives `CubControl.VehicleState` (position +
attitude) on UDP 4250 and sends `CubControl.ActuatorCommand` (stick commands)
on UDP 4251. The receive route maps FlatBuffer fields to `FixedWingFBW` stick
inputs:

| ActuatorCommand field | FBW input |
|---|---|
| `aileron` | `stick_roll` (positive = roll right) |
| `elevator` | `stick_pitch` (positive = pitch up) |
| `throttle` | `stick_throttle` [0..1] |
| `rudder` | `stick_yaw` (positive = yaw right) |

## Architecture

```
rumoca sim                                                   Browser
  │                                                             │
  ├── rum.manual.toml: input engine → stepper → FixedWing
  │   (keyboard/gamepad drives stick inputs via locals)
  │                                                             │
  ├── rum.binary.toml: UDP FlatBuffer → stepper → FixedWing
  │   (external process sends stick commands via CubControl)    │
  │                                                             │
  └── viewer JSON ──────────── WS ───────────────────────────► │
```

## UDP Contract (binary profile)

| Direction | Endpoint | Root type | Fields |
|---|---|---|---|
| Rumoca → external | UDP `127.0.0.1:4250` | `CubControl.VehicleState` | `timestamp_ns`, `x`, `y`, `z`, `roll`, `pitch`, `yaw` |
| External → Rumoca | UDP `127.0.0.1:4251` | `CubControl.ActuatorCommand` | `timestamp_ns`, `aileron`, `elevator`, `throttle`, `rudder`, `stabilizer` |

## Headless Verification

```bash
python3 examples/interactive/fixed_wing_binary/verify_sil.py --duration 10
```

Connects to the WebSocket, captures telemetry to optional CSV, and runs sanity
checks (NaN/Inf detection, altitude range, speed range, surface deflection
bounds).
