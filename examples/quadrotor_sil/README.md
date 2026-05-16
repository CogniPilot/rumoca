# Quadrotor SIL Plant Simulator

Software-in-the-loop plant model for a quadrotor. Runs out of the box
with an in-process Modelica rate-PID controller (`AcroRatePID.mo`) and a
Three.js viewer. Can also be rewired to couple with the Cerebri flight
controller via UDP + FlatBuffer.

## Files

| File | Role |
|---|---|
| `QuadrotorSIL.mo` | 6-DOF physics plant (NED frame, FRD body) |
| `AcroRatePID.mo` | In-process acro-mode rate PID controller |
| `quadrotor_acro.toml` | Config — physics + in-process Modelica controller |
| `quadrotor_cerebri.toml` | Config — physics + external Cerebri autopilot (UDP+FB) |
| `quadrotor_scene.js` | Three.js scene (body, motors, propellers) |

Two configs, same physics and scene. Pick one.

## Running

**In-process Modelica controller** (default, no external deps):

```bash
cargo run -p rumoca --release -- \
  lockstep run -c examples/quadrotor_sil/quadrotor_acro.toml
```

`lockstep run` reads the TOML, composes `QuadrotorSIL` + `AcroRatePID`
into a single DAE (via a synthesized wrapper), compiles, starts the HTTP
/ WS viewer servers, and enters the free-run loop.

**External Cerebri autopilot** (requires a Cerebri build on this machine):

```bash
cargo run -p rumoca --release -- \
  lockstep run -c examples/quadrotor_sil/quadrotor_cerebri.toml
```

Edit `[autopilot].command` and `[schema].bfbs` paths in the Cerebri config
to match your install before running.

Then open [http://localhost:8080](http://localhost:8080) for either.

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

## Architecture

```
input engine          rumoca lockstep                     Browser
  │                       │                                  │
  │── sticks → stepper ─► │── step composed DAE (dt=5ms)
  │   inputs              │   (QuadrotorSIL + AcroRatePID)
  │                       │                                  │
  │                       │── viewer JSON ─────── WS ──────► │
```

The composition wrapper is synthesized at load time from
`[controller.actuate]` (controller output → physics input) and
`[controller.sense]` (physics output → controller input). Physics public
variables pass through automatically, so `stepper:px`, `stepper:omega_m1`,
etc. resolve from the top level without the user knowing they live under
a sub-component.

## Swapping between controllers

Change the `-c` flag:

- `quadrotor_acro.toml` — in-process Modelica PID
- `quadrotor_cerebri.toml` — external Cerebri autopilot

Each file is self-contained: no editing needed to switch modes, just pass
the other file to `lockstep run`.

## Conventions

- **World frame:** NED (North-East-Down)
- **Body frame:** FRD (Forward-Right-Down)
- **Quaternion:** `{w, x, y, z}` scalar-first, body-to-world
- **Motor layout:** X-config matching Cerebri MixQuadX
  - Motor 0 (cerebri) / m1 (rumoca): front-right, CCW
  - Motor 1 / m2: rear-right,  CW
  - Motor 2 / m3: rear-left,   CCW
  - Motor 3 / m4: front-left,  CW

## Physical parameters (from `QuadrotorSIL.mo`, match cyecca rdd2)

| Parameter | Value | Unit |
|---|---|---|
| mass | 2.0 | kg |
| Ixx / Iyy / Izz | 0.0217 / 0.0217 / 0.040 | kg·m² |
| Ct (thrust coeff) | 8.55e-6 | N/(rad/s)² |
| Cm (torque coeff) | 0.016 | — |
| arm_length | 0.25 | m |
| tau_motor | 0.02 | s (first-order lag) |

## Ports

| Service | Default | Configured in |
|---|---|---|
| HTTP viewer | 8080 | `[transport.http].port` |
| WebSocket state stream | 8081 | `[transport.websocket].port` |

(Cerebri mode additionally uses UDP 4242 out / 4244 in.)

## Making a new vehicle

Copy this directory, edit the files:

- `.mo` — your physics plant
- Optional: a second `.mo` for an in-process controller
- `.toml` — point at your files; set `[controller].actuate`/`.sense` routes
- `.js` — your Three.js scene

See `examples/rover_sil/` for a standalone example (no controller —
input engine drives the stepper directly).
