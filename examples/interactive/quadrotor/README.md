# Quadrotor Plant Simulator

Software-in-the-loop plant model for a quadrotor. Runs out of the box with
`QuadrotorAcro`, a Modelica model that composes the vehicle and rate-PID
controller in the same simulation, plus a Three.js viewer.

## Files

| File | Role |
|---|---|
| `QuadrotorSIL.mo` | 6-DOF plant, `AcroRatePID`, and `QuadrotorAcro` wrapper |
| `../../../target/cmm/CMM-v0.0.1/LieGroup/package.mo` | SO(3) attitude utilities |
| `../../../target/cmm/CMM-v0.0.1/RigidBody/package.mo` | Reusable rigid-body 6-DOF model |
| `rum.acro.toml` | Config — `QuadrotorAcro` closed-loop Modelica model |
| `quadrotor_scene.js` | Three.js scene |
| `../../assets/models/drone.glb` | CC-BY-4.0 quadrotor model used by the scene |
| `../../assets/skybox/` | CC-BY-SA-3.0 `arid2` skybox used by the scene |
| `../../assets/sand_pbr/` | CC0 tileable PBR sand texture maps used by the ground plane |

Skybox, PBR textures, and glb models are served from the shared
[`examples/assets/`](../../assets) root (`[transport.http].asset_dir = "../../assets"`).

## Running

```bash
cargo xtask repo modelica-deps ensure
cargo run -p rumoca --release -- \
  sim -c examples/interactive/quadrotor/rum.acro.toml
```

`sim -c` reads the `rum.toml` scenario, loads the shared `LieGroup` and `RigidBody`
Modelica packages from `source_roots`, compiles `QuadrotorAcro`, starts the
HTTP / WS viewer servers, and enters the configured pacing loop.
Then open [http://localhost:8080](http://localhost:8080).

## Controls

| Input | Action |
|---|---|
| Gamepad left stick | throttle (Y) + yaw (X) |
| Gamepad right stick | roll + pitch |
| Gamepad **Start** | arm / disarm (requires throttle low) |
| Gamepad **South** (A) | reset |
| Gamepad **North** (Y) | save debug log (if `--debug`) |
| Browser/terminal keyboard W / S | throttle |
| Browser/terminal keyboard ↑ / ↓ | roll |
| Browser/terminal keyboard ← / → | pitch |
| Browser/terminal keyboard A / D | yaw |
| Browser/terminal keyboard Space | arm / disarm |
| Browser/terminal keyboard R | reset |
| Browser/terminal keyboard L | save debug log (if `--debug`) |
| Browser/terminal keyboard Q | quit |

`[input].mode = "auto"` — a plugged-in gamepad wins. Browser keyboard controls
are active when the viewer page has focus; terminal keyboard controls also work
when raw terminal input is available.

## Architecture

```
input engine          rumoca sim                          Browser
  │                       │                                  │
  │── sticks → stepper ─► │── step QuadrotorAcro (dt=5ms)
  │   inputs              │   (vehicle + controller in Modelica)
  │                       │                                  │
  │                       │── viewer JSON ─────── WS ──────► │
```

The closed-loop route lives in Modelica: `QuadrotorAcro` instantiates
`QuadrotorSIL` and `AcroRatePID`, wires sensor feedback and motor commands,
and exposes viewer-facing outputs such as `position`, `omega_m`, `accel`,
and `gyro`.

## Conventions

- **World frame:** Z-up internal world coordinates, rendered as Three.js Y-up
- **Body frame:** FLU internally, with FRD sensor outputs
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
| tau_up / tau_down | 0.0125 / 0.025 | s (asymmetric first-order lag) |

## Ports

| Service | Default | Configured in |
|---|---|---|
| HTTP viewer | 8080 | `[transport.http].port` |
| WebSocket state stream | 8081 | `[transport.websocket].port` |

## Making a new vehicle

Copy this directory, edit the files:

- `.mo` — your top-level Modelica model; include any controller composition there
- `rum.toml` — point `[model]` at that top-level class
- `.js` — your Three.js scene

See `examples/interactive/rover/` for a standalone example where the input engine
drives model inputs directly.

## 3D Model Attribution

The viewer uses a Sketchfab model:

- Title: `Drone`
- Source: <https://sketchfab.com/3d-models/drone-2588b8a0917d474e97886763c98a65af>
- Author: Cafitz3D (<https://sketchfab.com/Cafitz3D>)
- License: CC-BY-4.0 (<http://creativecommons.org/licenses/by/4.0/>)

Credit text:

This work is based on "Drone" (<https://sketchfab.com/3d-models/drone-2588b8a0917d474e97886763c98a65af>) by Cafitz3D (<https://sketchfab.com/Cafitz3D>) licensed under CC-BY-4.0 (<http://creativecommons.org/licenses/by/4.0/>).

## Skybox Attribution

The viewer uses the `arid2` skybox from `skiingpenguins' skybox pack`:

- Source: <https://opengameart.org/content/skiingpenguins-skybox-pack>
- Author: Zachery "skiingpenguins" Slocum (<http://www.freezurbern.com>)
- License: CC-BY-SA-3.0 (<http://creativecommons.org/licenses/by-sa/3.0/>)
- Original context: created for the first-person shooter engine `Cube 2: Sauerbraten`

The original attribution readme is preserved at
`../../assets/skybox/README_skiingpenguins_arid2.txt`.

## Ground Texture Attribution

The viewer uses the `GroundSand005` PBR texture set from ambientCG for the
ground plane:

- Source: <https://ambientcg.com/>
- Asset: `GroundSand005`
- License: CC0 1.0 Universal (<https://creativecommons.org/publicdomain/zero/1.0/>)

ambientCG states that its assets are released under Creative Commons CC0.
