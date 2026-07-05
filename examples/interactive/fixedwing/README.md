# Fixed-Wing Plant Simulator

Software-in-the-loop plant model for a small fixed-wing aircraft. Runs out of
the box with `FixedWing`, a Modelica model that composes a 6-DOF aero plant and
an attitude-stabilized fly-by-wire controller in the same simulation, plus a
Three.js viewer.

The controller is a **cascaded rate-command / attitude-hold autopilot**:
holding a key keeps **rolling / pitching** (the stick commands an attitude
*rate*), and releasing **holds** the new bank / pitch — an inner body-rate loop
tracks the setpoints. So the aircraft is responsive on a keyboard and recovers
itself from upsets.

Note: with attitude hold, **throttle sets airspeed, not altitude** — to climb,
pull up (pitch) and add power. Stall protection keeps it from departing if you
get slow.

## Files

| File | Role |
|---|---|
| `FixedWingSIL.mo` | 6-DOF aero plant, `FixedWingFBW` controller, and `FixedWing` wrapper |
| `../../../target/cmm/CMM-v0.0.2/LieGroup/package.mo` | SO(3) attitude utilities |
| `../../../target/cmm/CMM-v0.0.2/RigidBody/package.mo` | Reusable rigid-body 6-DOF model |
| `rumoca-scenario.toml` | Config — `FixedWing` closed-loop Modelica model |
| `fixedwing_scene.js` | Three.js scene — desert environment + rigged airplane |
| `../../assets/models/airplane.glb` | Aircraft model with riggable control-surface pivots |
| `../../assets/skybox/` | `arid2` desert skybox (shared with the quadrotor example) |
| `../../assets/sand_pbr/` | Tileable PBR sand texture maps for the ground |

Skybox, PBR textures, and glb models are served from the shared
[`examples/assets/`](../../assets) root (`[transport.http].asset_dir = "../../assets"`).
Model asset licenses and attributions are recorded in
[`../../assets/models/README.md`](../../assets/models/README.md).

## Running

```bash
cargo xtask repo modelica-deps ensure
cargo run -p rumoca --release -- \
  sim -c examples/interactive/fixedwing/rumoca-scenario.toml
```

`sim -c` reads the `rumoca-scenario.toml` scenario, loads the shared `LieGroup` and
`RigidBody` Modelica packages from `source_roots`, compiles `FixedWing`, starts
the HTTP / WS viewer servers, and enters the realtime pacing loop.
Then open [http://localhost:8080](http://localhost:8080).

The aircraft starts **at rest on the runway**. Press **Space** to arm, hold
**W** for full throttle to accelerate down the runway, and ease back (**↑**) to
rotate and climb away around 10 m/s. To land, reduce throttle and descend —
the tricycle gear absorbs the touchdown and the aircraft rolls out.

## Controls

| Input | Action |
|---|---|
| Keyboard ↑ / ↓ | pitch (elevator) — ↑ noses up |
| Keyboard ← / → | roll (ailerons) — → banks right |
| Keyboard A / D | yaw (rudder) |
| Keyboard W / S | throttle |
| Keyboard Space | arm / disarm (requires throttle low) |
| Keyboard R | reset |
| Keyboard Q | quit |
| Gamepad right stick | pitch + roll |
| Gamepad left stick | rudder (X) + throttle (Y) |
| Gamepad **Start** | arm / disarm |
| Gamepad **South** (A) | reset |

`[input].mode = "auto"` — a plugged-in gamepad wins; otherwise the browser
keyboard drives the aircraft when the viewer page has focus.

## Architecture

```
input engine          rumoca sim                          Browser
  │                       │                                  │
  │── sticks → session ─► │── step FixedWing (dt=10ms)
  │   inputs              │   (plant + FBW controller in Modelica)
  │                       │                                  │
  │                       │── viewer JSON ─────── WS ──────► │
```

The closed-loop route lives in Modelica: `FixedWing` instantiates
`FixedWingPlant` and `FixedWingFBW`, feeds the controller the body rate
(`gyro`), the world-up direction in body axes (`up_body`), and `airspeed`, and
drives the plant's aileron / elevator / rudder / throttle. It exposes
viewer-facing outputs such as `position`, `quat`, the surface deflections,
`airspeed`, and `alpha_deg`.

## Model

`FixedWingPlant` uses the **identified SportCub aerodynamics from CogniPilot
[cubs2](https://github.com/CogniPilot/cubs2)** — the same coefficients, 6° wing
incidence, smooth flat-plate stall blend, full side-force / lateral model, and
a wind-axis DCM force rotation — on this example's larger airframe. It builds
forces and moments in FRD / stability axes, rotates them into the library's FLU
body frame via the wind frame, adds body-axis thrust, and hands the non-gravity
wrench to `RigidBody.RigidBody6DOF`. It also models **tricycle landing gear** as
a per-wheel spring-damper (normal force along world-up, light rolling
resistance, firmer lateral grip) so the aircraft can sit, take off, and land.
Holding wings-level it trims near 10 m/s.

`FixedWingFBW` is a cascaded rate-command / attitude-hold controller:

- **Outer loop (attitude hold):** the stick commands an attitude *rate* that
  integrates into a held bank / pitch setpoint (so holding a key keeps
  rotating; releasing holds). Bank / pitch are estimated from `up_body` as
  singularity-free tilt angles, and a proportional law on the setpoint error
  produces body-rate setpoints. Centered stick + level setpoints hold
  wings-level — and pull back to it from an upset.
- **Inner loop (body-rate PI):** tracks the rate setpoints with the measured
  `gyro`, driving the surfaces. The integral supplies the trim deflection, so
  attitude holds with no steady-state droop.
- **Stall protection:** nose-up authority fades out as airspeed nears the
  stall, and below a floor the controller commands nose-down to trade altitude
  for speed — so on too little throttle the aircraft *sinks* rather than
  departing, and stays recoverable.
- **Throttle:** manual, gated by the arm signal.

Surface-to-attitude signs were measured from open-loop step responses (see the
sign notes in `FixedWingSIL.mo`).

## Conventions

- **World frame:** Z-up, rendered as Three.js Y-up
- **Body frame:** FLU (x forward / nose, y left, z up)
- **Quaternion:** `{w, x, y, z}` scalar-first, body-to-world
- **Surfaces:** `ail_rad`, `elev_rad`, `rud_rad` in radians; `thr_out` in `[0,1]`
