# Reusable Booster Autonomous Landing

This example launches a reusable orbital booster from a moving drone
ship, cuts the engine after a six-second ascent burn, coasts through apogee, and
returns to the deck. A quintic differential-flatness planner is initialized
from the captured descending state, a GPS/IMU error-state Kalman filter
estimates the `SE_2(3)` state, and a geometric controller drives an
illustrative hybrid allocation between the center-engine gimbal and RCS.

This is an educational rigid-body and controls model. It uses representative
dimensions and landing physics, but it is not a model of any operational
vehicle or flight software.

## Files

| File | Role |
|---|---|
| `ReusableBoosterLanding.mo` | Flatness planner, GPS/IMU filter, `SE_2(3)` controller, 6-DOF plant, actuators, and contact |
| `rumoca-scenario.toml` | Realtime simulation, tunable parameters, viewer telemetry, and controls |
| `rumoca-scenario.controller-efmi.toml` | eFMI Algorithm Code + Production Code export of the shared discrete feedback law |
| `reusable_booster_scene.js` | Three.js ocean, drone ship, scale-correct booster, plumes, and trajectory display |
| `../../../target/cmm/CMM-a642c381/LieGroups/package.mo` | Pinned CogniPilot LieGroups dependency |

## Running

```bash
cargo xtask repo modelica-deps ensure
cargo run -p rumoca --release -- \
  sim -c examples/interactive/reusable_booster/rumoca-scenario.toml
```

Open [http://localhost:8080](http://localhost:8080). Press `Space` to launch,
`R` to restart, `T` to toggle realtime/fast pacing, `F` for fullscreen, and `Q`
to stop. The run continues after touchdown so the landed state remains live;
`R` starts it again from `t = 0`.

The same scenario is embedded in the user guide's default WASM widget. Its
Settings panel discovers the scalar Modelica parameters and writes overrides
to the TOML `[parameters]` table.

## Mission Parameters

| Parameter | Default | Meaning |
|---|---:|---|
| `launch_burn_duration` | 6 s | Initial center-engine ascent burn |
| `launch_thrust_fraction` | 0.62 | Ascent thrust fraction |
| `landing_trigger_speed` | 8 m/s | Downward speed that captures the landing initial condition |
| `landing_duration` | 18 s | Nominal flatness-plan duration |
| `rcs_authority` | 1 | Available fraction of modeled RCS authority |
| `wind_speed` | 5 m/s | Steady horizontal wind magnitude |
| `wind_direction_deg` | 35 deg | Direction wind blows toward from world +x toward +y |
| `wind_speed_variation` | 0 m/s | Slow mean-wind speed variation |
| `wind_direction_variation_deg` | 15 deg | Slow mean-wind direction variation |
| `wind_variation_period` | 10 s | Mean-wind variation period |
| `dryden_intensity` | 4 m/s | Deterministic Dryden-shaped gust scale |
| `wave_heave_amplitude` | 0.35 m | Deck heave amplitude |
| `wave_roll_amplitude_deg` | 1.2 deg | Deck roll amplitude |
| `wave_pitch_amplitude_deg` | 0.8 deg | Deck pitch amplitude |
| `wave_period` | 8.5 s | Dominant deck-motion period |

The in-scene tuning panel changes all wind and deck amplitudes plus position,
velocity, attitude, and angular-rate controller-gain multipliers during a run.
These are TOML locals routed to Modelica inputs, rather than compile-time
parameters.

## Planner

Position is the translational flat output. For each axis, the planner evaluates
a quintic polynomial that matches captured position, velocity, and acceleration
to the predicted terminal deck CG position, velocity, and acceleration. The CG
target is offset from the deck centre along its predicted tilted normal:

```text
p(t) = c0 + c1 t + c2 t^2 + c3 t^3 + c4 t^4 + c5 t^5
(p, v, a)(0) = (p0, v0, a0),  (p, v, a)(T) = (pf, vf, af)
```

Analytic first and second derivatives provide reference velocity and
acceleration. The reference thrust direction is `a_ref + g e3`; aligning the
body axis with that vector supplies the flatness-derived reference attitude.
The visualizer reconstructs this same polynomial from planner telemetry, so
the amber future path remains correct when settings change.

A sampled preflight check evaluates the reference at 21 points for throttle,
tilt, positive vertical thrust, deck clearance, and approximate propellant
budget. The scene reports `REFERENCE INFEASIBLE` when the unconstrained
polynomial exceeds those modeled limits; the controller still saturates safely.

## SE_2(3) Controller

The controller forms the group states

```text
X    = (p, v, R)
Xref = (pref, vref, Rref)
xi   = Log(X^-1 Xref)
```

using `LieGroups.SE23.Quat.inverse`, `product`, and `log_map`. The
log-linear correction uses the library's full left Jacobian:

```text
delta = J_left(xi) K xi
a_cmd = a_ref + R (delta_position + delta_velocity)
```

The commanded force is constrained to a positive vertical component and a
20-degree attitude envelope. An SO(3) inner loop aligns the booster axis with
that force. Its geodesically rate-limited reference, per-axis gain vectors,
desired-rate tracking, and rigid-body/rotating-reference feedforward avoid
turning a commanded slew into a steady attitude lag. The engine gimbal is
prioritized for requested pitch/yaw moment;
the RCS allocator receives only the saturated residual and the requested axial
moment. RCS authority fades in between 80 and 120 m engine-plane altitude and
is unavailable near the deck. Its force and roll-couple moment use an 80 ms
first-order response and contribute to vehicle mass depletion through a
representative cold-gas mass-flow model.

The geometric controller is evaluated atomically at 20 Hz and held between
updates. The physical plant remains continuous.

### eFMI Production Code

The translational force law and rotational moment law are shared by the full
simulation controller and `ReusableBoosterEmbeddedControlLaw`. The export model
is a fixed-sample discrete block whose vector interface begins after the
Lie-group adapter has produced world correction, attitude error, and reference
body rate. Generate a schema-valid eFMU containing both GALEC Algorithm Code
and C99 Production Code with:

```bash
cargo run -p rumoca -- \
  compile examples/interactive/reusable_booster/ReusableBoosterLanding.mo \
  --model ReusableBoosterEmbeddedControlLaw \
  --source-root target/cmm/CMM-a642c381 \
  --target efmi \
  --output examples/interactive/reusable_booster/gen/control_law_efmu
```

The scenario `rumoca-scenario.controller-efmi.toml` exposes the
same target in the code-generation editor.

## GPS/IMU Estimator

The filter state is `(p, v, R)` on `SE_2(3)`. At 10 Hz it passes sampled
specific force and body rate to CMM's `LieGroups.SE23.Quat.exp_mixed`, with
world-frame gravity and the nilpotent position-velocity coupling matrix. That
function is the library implementation of the closed-form mixed-invariant
prediction in Purdue's
[preintegration paper](https://docs.lib.purdue.edu/cgi/viewcontent.cgi?article=1061&context=aaepubs).

A synthetic differential-GPS position fix corrects the state at 5 Hz. The
filter propagates a coupled `9 x 9` position, velocity, and attitude-error
covariance, including the sensitivity of inertial acceleration to attitude
uncertainty. The GPS update uses the full cross-covariance, a Joseph-form
covariance update, and multiplicative attitude correction. This is still a
compact educational error-state filter: it omits sensor biases, time delay,
outlier rejection, Earth rotation, and geodetic effects.

## Plant

- 40.9 m first stage, 3.66 m diameter, and 34,000 kg representative landing mass
- 845 kN maximum center-engine thrust with 282 s sea-level specific impulse,
  combined as a representative public-data propulsion model
- propellant mass depletion and 120 ms thrust response; CG and inertia remain
  fixed at representative landing values
- 6-DOF quaternion rigid-body dynamics with representative landing inertia
- body-axis aerodynamic drag using axial and broadside projected areas
- changing mean wind and deterministic three-axis Dryden-shaped gusts applied
  through relative airspeed
- 5-degree center-engine gimbal and upper-stage RCS moment arm
- heaving/rolling/pitching finite drone-ship contact plane
- four moving-deck foot contacts with normal spring/damping and tangential
  damping capped by a regularized Coulomb friction cone

The cyan line is the flown trajectory, the green line is the estimated path,
the magenta octahedron is the most recent GPS fix, and the amber line and
markers are the complete flatness reference. Green and magenta wireframes are
the estimator and GPS axis-aligned 3-sigma marginal-variance envelopes,
magnified `4x` for visibility. They do not show covariance orientation. Main plume
length, shock cells, haze, color, light, and centre-nozzle direction follow
actual commanded thrust and gimbal. Every visible RCS jet follows its
corresponding signed force or axial-moment command. A deck arrow, numeric
indicator, and articulated windsock visualize the same wind vector used by the
plant.

A safe landing requires at least three feet to carry meaningful load, the CG
projection to remain at least 0.25 m inside their support polygon, normal and
tangential foot speeds no greater than 2.0 and 1.5 m/s, and tilt no greater
than 10 degrees from the deck normal. Safe first contact cuts the engine and
allows the suspension a bounded interval to establish stable support. Unsafe
contact, or failure to establish support before that interval expires, latches
a crash. `R` clears either state.

Press `Capture` to control the scene camera. Mouse movement orbits and tilts,
middle/right drag pans, the wheel zooms, and `Escape` releases capture.

## Model Scope

The stage geometry, engine layout, landing mass, inertia, throttle floor,
controller gains, RCS thrust, and contact coefficients are representative
values selected for a stable, inspectable terminal-landing example. They are
not intended to reproduce any particular operational vehicle.
