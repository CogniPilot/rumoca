# Reusable Booster Autonomous Landing

This live example starts a reusable orbital booster on a moving drone
ship. Press `Space` to ignite the center engine. The stage performs a six-second
ascent burn, coasts through apogee, captures its descending state, and plans a
return to the deck. A differential-flatness planner generates the landing
reference, an `SE_2(3)` GPS/IMU error-state Kalman filter estimates the vehicle
state, and a geometric controller drives an illustrative hybrid allocation
between the center-engine gimbal and RCS.

```modelica,interactive
// rumoca-live-scenario: ../repo-examples/interactive/reusable_booster/rumoca-scenario.toml
```

The standard live widget opens the Modelica source and TOML scenario in its
editor. Open **Tune environment and controller** over the scene to change wind,
gust intensity, deck heave/roll/pitch, or the four controller-gain multipliers
while the simulation is running. Those sliders write scenario locals directly
to Modelica inputs, so they do not recompile or reset the run. This makes it
possible to search for the landing-failure boundary. The editor's **Settings**
panel remains available for mission constants stored in TOML `[parameters]`.

Press `R` at any time to restore the vehicle and restart at `t = 0`. The live
run has no preset end time; it continues after touchdown so the landed state
and deck motion remain observable until you stop or reset it. The upper-right
badge shows mission phase, wind and gust speed, and deck roll/pitch. `T`
switches between realtime and fast pacing, `F` toggles fullscreen, and `Q`
stops the run.

For a native run:

```bash
cargo xtask repo modelica-deps ensure
cargo run -p rumoca --release -- \
  sim -c examples/interactive/reusable_booster/rumoca-scenario.toml
```

## Reference Planning

Position is the translational flat output. For each axis, the Modelica planner
constructs a quintic from the position, velocity, and acceleration captured
when the coasting stage reaches 8 m/s downward speed. Its terminal state is the
predicted deck CG target along the tilted deck normal, including the target
point's translational and rotational velocity and acceleration:

```text
p(t) = c0 + c1 t + c2 t^2 + c3 t^3 + c4 t^4 + c5 t^5
(p, v, a)(0) = (p0, v0, a0),  (p, v, a)(T) = (pf, vf, af)
```

Analytic derivatives provide reference velocity and acceleration. The vector
`a_ref + g e3` determines the flatness-derived reference thrust direction and
attitude. The amber Three.js line is the complete polynomial reference; its
marker advances along the path while the cyan line records the simulated
trajectory. The green line is the navigation estimate and the magenta marker
is the latest GPS fix consumed by the filter. Green and magenta wireframes show
their respective axis-aligned 3-sigma marginal-variance envelopes at `4x`
visual scale so the sub-metre posterior remains legible beside a 40.9 m stage.
They show per-axis marginal variance, not covariance orientation.

The nominal landing-plan duration is 18 seconds. Guidance continues to hold the
terminal reference until a safe physical deck contact is detected; reaching
the polynomial end does not shut down the engine above the ship.

A 21-point sampled preflight check covers throttle, tilt, positive vertical
thrust, deck clearance, and approximate propellant budget. The scene explicitly
reports `REFERENCE INFEASIBLE` when the unconstrained quintic exceeds those
modeled limits; this warning is not a constrained trajectory optimizer.

## GPS/IMU `SE_2(3)` Kalman Filter

The navigation state uses the same group representation as the controller:

```text
Xhat = (phat, vhat, Rhat) in SE_2(3)
```

At 10 Hz, piecewise-constant gyroscope and accelerometer measurements drive
`LieGroups.SE23.Quat.exp_mixed`. The function implements the closed-form mixed
invariant propagation described in Purdue's
[On Closed-Form Preintegration for a Class of Mixed-Invariant Systems in
SE_n(3)](https://docs.lib.purdue.edu/cgi/viewcontent.cgi?article=1061&context=aaepubs):

```text
Xhat(k+1) = exp(M dt) Xhat(k) exp(N dt)
```

The call includes gravity as a world-frame right increment and the nilpotent
position-velocity coupling matrix. This is the library implementation from the
pinned CogniPilot `modelica_models` checkout; the example does not duplicate
the exponential.

A 5 Hz synthetic differential-GPS position fix performs the Kalman correction.
The filter propagates a coupled `9 x 9` position, velocity, and attitude-error
covariance. Its inertial transition includes position-velocity coupling and
the sensitivity of specific-force integration to attitude error, allowing a
position fix to correct velocity and attitude through cross-covariance. The
GPS update uses Joseph form for covariance robustness and a multiplicative
attitude correction. The model intentionally omits sensor biases, time delay,
outlier rejection, Earth rotation, and geodetic effects.

## Geometric Tracking

The controller represents estimated pose and velocity together on `SE_2(3)`:

```text
Xhat = (phat, vhat, Rhat)
Xref = (pref, vref, Rref)
xi   = Log(Xhat^-1 Xref)
delta = J_left(xi) K xi
```

`inverse`, `product`, `log_map`, and `left_jacobian` come directly from the
pinned CogniPilot `modelica_models` `LieGroups.SE23.Quat` package. Translation
feedback corrects the flatness acceleration, and an SO(3) inner loop aligns the
booster axis with the resulting force vector. The inner loop tracks the
geodesically rate-limited attitude command with per-axis gain vectors and
includes rigid-body gyroscopic and rotating-reference feedforward terms.

The geometric controller runs at 20 Hz with a zero-order hold, while the
rigid-body, engine, drag, mass, and contact dynamics remain continuous. This
matches a sampled flight-control implementation and avoids reevaluating the
Lie-group maps at every adaptive integrator stage.

The control allocator prioritizes center-engine thrust vectoring for
pitch/yaw moment. RCS receives only the saturated residual and the requested
axial moment. RCS availability fades in between 80 and 120 m engine-plane
altitude, its aggregate force/moment has an 80 ms first-order response, and its
representative cold-gas mass flow contributes to vehicle mass depletion.
Engine and RCS plume geometry, light, and opacity follow actual actuator state.
Landing-foot tangential damping is regularized by a Coulomb cone, so friction
is capped by normal load and vanishes for an unloaded foot.

## Embeddable eFMI Control Law

The continuous vehicle and the CMM-based Lie-group preprocessing remain in the
simulation model. The force/moment feedback law behind them is a separate deep
module with four vector inputs: translational terms, rotational terms, gain
scales, and principal inertia. The 20 Hz simulation controller and the export
model call the same functions, so the generated code does not maintain a
second copy of the control equations.

The export model is fixed-sample and discrete, which makes it admissible for
Rumoca's `galec-production` target. Select **Generate .alg** below to inspect
the eFMI Algorithm Code, then **Generate C/H** to inspect the corresponding C99
Production Code.

```modelica,codegen
// rumoca-live-scenario: ../repo-examples/interactive/reusable_booster/rumoca-scenario.controller-galec-production.toml
```

The native packaging command emits both a directory-form eFMU and a matching
`.efmu` archive with Algorithm Code and Production Code representations:

```bash
cargo xtask repo modelica-deps ensure
cargo run -p rumoca -- \
  compile examples/interactive/reusable_booster/ReusableBoosterLanding.mo \
  --model ReusableBoosterEmbeddedControlLaw \
  --source-root target/cmm/CMM-a642c381 \
  --target galec-production \
  --output examples/interactive/reusable_booster/gen/control_law_efmu
```

This exported seam starts after the geometric adapter has computed the
world-frame translational correction, attitude error, and reference body rate.
Exporting the full estimator and Lie-group adapter would additionally require
GALEC projection support for the indexed array outputs and matrix operations
used by those library functions.

## Dryden Wind and Moving Deck

The default mean wind is 5 m/s. Its direction swings by 15 degrees on a
10-second period, while longitudinal, lateral, and vertical deterministic
inputs pass through standard Dryden-shaped filters:

```text
Hu(s)   = sigma sqrt(2 Lu / (pi V)) / (1 + Lu s / V)
Hv,w(s) = sigma sqrt(L / (pi V)) (1 + sqrt(3) L s / V) / (1 + L s / V)^2
```

Band-limited multi-sine inputs replace ideal white noise so a browser run is
deterministic and repeatable. The resulting three-axis gust is rotated with the
mean-wind direction, added to the mean wind, and subtracted from inertial
velocity before the body-axis drag calculation. The numeric indicator,
windsock, and deck arrow use this same changing total wind vector.

The drone ship has analytic heave, roll, and pitch kinematics. The planner
predicts the terminal CG position along the tilted deck normal and matches that
point's velocity and acceleration, including rotational motion. For each
landing foot, the contact model transforms its position into the deck frame and
computes

```text
penetration = max(0, -z_foot_deck)
vrel = vfoot - (vdeck + omega_deck x (pfoot - pdeck))
```

Normal spring/damping acts along the tilted deck normal and tangential damping
uses the relative deck-point velocity. Contact is limited to the physical deck
bounds. The same Modelica deck position and quaternion drive the Three.js ship,
so visible wave motion and collision geometry remain synchronized.
The telemetry badge reports deck roll and pitch beside the changing wind and
gust values.

## Touchdown and Failure State

At landing-phase contact, at least three feet must carry meaningful normal load
and the CG projection must remain at least `0.25 m` inside their support polygon.
The maximum foot speed normal to the deck must be at most `2.0 m/s`, tangential
speed at most `1.5 m/s`, and booster tilt at most `10 deg` from the local deck
normal. Safe first contact cuts the engine and starts a bounded suspension-
settling interval. Stable support during that interval latches **LANDED**;
unsafe kinematics or failure to establish support latches a crash. The scene
reports foot count, support margin, and separate normal/tangential speeds.

## Vehicle and Contact Model

The visual and physical model use a representative 40.9 m stage and 3.66 m
diameter. The plant includes an illustrative landing mass and fixed landing
inertia/CG, a generic center-engine propulsion model, propellant
depletion, thrust response, anisotropic aerodynamic drag, quaternion 6-DOF
dynamics, and four spring-damper landing contacts. The
scene includes an interstage, nine-engine mount, deployed grid fins,
articulated landing legs, and a scale-matched autonomous drone ship.

After pressing **Capture**, mouse movement orbits and tilts the scene, the
middle or right mouse button pans, and the wheel zooms. Press `Escape` to
release capture.

The geometry, landing mass, inertia, RCS force, throttle floor, gains, and
contact coefficients are mutually consistent educational values. They are not
intended to reproduce any particular operational vehicle.
