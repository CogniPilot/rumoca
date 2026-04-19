# Quadrotor SIL Plant Simulator

Software-in-the-loop plant model for a quadrotor. Couples to the Cerebri
flight controller via UDP with FlatBuffer-encoded packets, and streams
live state to a Three.js viewer in the browser.

## Files

| File | Role |
|---|---|
| `QuadrotorSIL.mo` | 6-DOF physics plant (NED frame, FRD body) |
| `quadrotor.toml` | Complete simulation config (model + transport + codec + input + viz) |
| `quadrotor_scene.js` | Three.js scene (quadrotor body, motors, propellers) |

## Running

```bash
cargo run -p rumoca --features sim --release -- \
  sim run -c examples/quadrotor_sil/quadrotor.toml
```

Then open [http://localhost:8080](http://localhost:8080).

`sim run` reads the TOML, compiles `QuadrotorSIL.mo`, loads the FlatBuffer
schemas, spawns the autopilot subprocess, binds UDP, starts the HTTP/WS
viewer servers, and enters the lockstep loop.

## Setup before first run

Edit these two settings in `quadrotor.toml` to match your machine:

```toml
[autopilot]
command = "/path/to/cerebri/build/zephyr/zephyr.exe"

[schema]
bfbs = [
  "/path/to/cerebri/build-native_sim/generated/flatbuffers/cerebri2_topics.bfbs",
  "/path/to/cerebri/build-native_sim/generated/flatbuffers/cerebri2_sil.bfbs",
]
```

The `.bfbs` files are produced by the Cerebri build; the `command` is the
Zephyr native-sim executable.

## Controls

| Input | Action |
|---|---|
| Gamepad left stick | throttle (Y) + yaw (X) |
| Gamepad right stick | roll + pitch |
| Gamepad **Start** | arm / disarm (requires throttle low) |
| Gamepad **South** (A) | reset |
| Gamepad **North** (Y) | save debug log (if `--debug`) |
| Keyboard ↑ / ↓ | throttle |
| Keyboard ← / → | yaw |
| Keyboard W / S | pitch |
| Keyboard A / D | roll |
| Keyboard Space | arm / disarm |
| Keyboard R | reset |
| Keyboard L | save debug log (if `--debug`) |
| Keyboard Q | quit |

`[input].mode = "auto"` — a plugged-in gamepad wins; keyboard is always
polled for hotkeys.

## Protocol

### Receive: `motor_output` (48 bytes, from Cerebri)

| Field | Routed to |
|---|---|
| `motors[0..3]` | `stepper:omega_m{1..4}` (scale 1100.0 to rad/s) |
| `armed` | `local:armed` |
| `test_mode` | `local:test_mode` |

Routing is declared in `[receive.route]`.

### Send: `flight_snapshot` (164 bytes, to Cerebri)

`[signals.send]` populates the outgoing `SignalFrame`; `[send.route]`
marshals it into the FlatBuffer. Covers IMU (`gyro_*`, `accel_*`), the
16 RC channels, and link-health flags.

## Architecture at a glance

```
Cerebri autopilot                  rumoca sim                     Browser
  │                                    │                             │
  │── motor_output (48B UDP) ────────► │── step physics (dt=0.00125)
  │                                    │── build SignalFrame
  │                                    │── pack → FB  ─────────────► │
  │◄── flight_snapshot (164B UDP) ──── │── build viewer JSON ──► WS ►│
```

Everything in that diagram is driven by `quadrotor.toml` — no Rust code
knows this is a quadrotor.

## Conventions

- **World frame:** NED (North-East-Down)
- **Body frame:** FRD (Forward-Right-Down)
- **Quaternion:** `{w, x, y, z}` scalar-first, body-to-world
- **Motor layout:** X-config matching Cerebri MixQuadX
  - Motor 1: front-right CW • Motor 2: rear-right CCW
  - Motor 3: rear-left CW  • Motor 4: front-left CCW

## Physical parameters (from `QuadrotorSIL.mo`)

| Parameter | Value | Unit |
|---|---|---|
| mass | 2.0 | kg |
| Ixx / Iyy / Izz | 0.020 / 0.020 / 0.040 | kg·m² |
| Ct (thrust coeff) | 8.5e-6 | N/(rad/s)² |
| Cm (torque coeff) | 1.36e-7 | N·m/(rad/s)² |
| arm_length | 0.2 | m |
| ω_max (scale factor) | 1100 | rad/s |
| hover ω | ~759.5 | rad/s |

## Ports

| Service | Default | Configured in |
|---|---|---|
| HTTP viewer | 8080 | `[transport.http].port` |
| WebSocket state stream | 8081 | `[transport.websocket].port` |
| UDP listen (motors in) | 0.0.0.0:4244 | `[transport.udp].listen` |
| UDP send (sensors out) | 127.0.0.1:4242 | `[transport.udp].send` |

## Making a new vehicle

Copy this directory, edit the three files:

- `.mo` — your physics
- `.toml` — point at your model; adjust schemas, inputs, signals
- `.js` — your scene

See `examples/rover_sil/` for a standalone-mode example (no autopilot,
no FlatBuffer — just gamepad → stepper directly).
