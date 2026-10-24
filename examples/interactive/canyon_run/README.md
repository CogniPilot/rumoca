# Canyon Run

A small fixed-wing aircraft learns to fly down a winding canyon. The learner is
deep Q-learning, and all of it is ordinary Modelica in `CanyonRun.mo`: the
network layers, the loss gradient, the Adam optimizer and the replay ring. Edit
a layer width or swap an activation, re-run, and watch the reward curve change.

The aircraft is the 6-DOF plant and rate-command / attitude-hold fly-by-wire
controller from the [`../fixedwing`](../fixedwing) example, copied unchanged into
`CanyonRun.Aircraft`. The agent flies it with the same stick commands a pilot
would use, so every episode runs the full flight physics.

## Files

| File | Role |
|---|---|
| `CanyonRun.mo` | The aircraft, the course and the occupancy grid; the `Net` learner; `Agent`, `Episode` and the `Run` model; `Physics`, the in-flight plant used for gradients |
| `../../../target/cmm/CMM-a642c381/` | CMM `RigidBody` and `LieGroups`, used by the plant |
| `rumoca-scenario.toml` | Scenario: runs `CanyonRun.Run` and streams it to the viewer |
| `canyon_run_scene.js` | Three.js scene: the canyon, the Courier craft, the reward chart and sampled paths |

## Running

```bash
cargo xtask repo modelica-deps ensure
cargo run -p rumoca --release -- sim -c examples/interactive/canyon_run/rumoca-scenario.toml
```

Open the HTTP URL the CLI prints. Learning starts at once and runs as fast as
the machine allows. Press `T` to slow to real time and watch a flight, and `P`
to show or hide the kept paths. The chart in the corner plots the return of
every episode with its moving average.

## What the agent sees and does

Every 0.1 s the agent gets 79 numbers:

- 15 true-state features: lateral offset from the canyon centreline as a
  fraction of the half-width, height relative to mid-rim, the local canyon
  slope, the attitude quaternion, the world velocity, the body rates, and the
  bank and pitch setpoints the fly-by-wire is holding.
- An 8 x 8 occupancy grid of the ground ahead, 40 m deep and 40 m wide in 5 m
  cells, turned with the craft's heading. Each cell holds the share of four
  sample points in it that lie outside the canyon walls, computed from the
  canyon geometry; there is no camera or mapping in the loop.

It picks one of five manoeuvres at cruise throttle (0.25): level, bank left,
bank right, pitch up, pitch down. A manoeuvre is a full stick deflection on one
axis, which the fly-by-wire turns into a held bank or pitch setpoint that keeps
moving while the stick is held.

Reward is 0.1 per metre flown down the canyon, minus a small penalty that grows
with the distance from the centreline, -5 for hitting a wall, the floor or
leaving over the rim, and +5 for crossing the finish line 600 m away. An episode
also ends after 60 s.

Each sample the agent stores the last transition in the replay ring, acts with
the network as it stood at the start of the sample, then takes one Adam step on
a batch drawn from the ring as it stood at the start of the sample. A new
transition is therefore available for learning from the next sample on.

## Differentiable flight physics

`CanyonRun.Physics.AirborneFixedWing` is the same aircraft in flight: the
aerodynamics of `FixedWingPlant` unchanged, without the landing gear and with
the wind-axis reference fixed to the body z axis (in flight the gear is off the
ground and the angle of attack stays below 90 deg, so neither switch can fire).
Its products are written out per component, so Rumoca can differentiate whole
trajectories of it in both modes:

```bash
cd examples/interactive/canyon_run
for mode in forward adjoint; do
  cargo run -p rumoca --release -- sim CanyonRun.mo -m CanyonRun.Physics.Glide \
    --source-root ../../../target/cmm/CMM-a642c381 --solver rk-like --t-end 3 \
    --inspect objective-gradient --grad-mode $mode --terminal z \
    --wrt elevator,aileron,throttleSetting,vehicle.CD0 --format json
done
```

Both print the gradient of the altitude after 3 s with respect to the elevator,
aileron and throttle settings and the drag coefficient, and the two agree.
`Physics.HybridGlide` flies the same flight on the original plant; in the
flights tested, its positions are bit-identical to `Glide`'s. The hybrid plant
itself, and the closed-loop episode, are not
differentiated: Rumoca refuses to differentiate across their switching events.

## Things to try

- Throttle as a choice: in `Run`, set `nThrottle = 3` for 15 actions, each
  manoeuvre at a low, cruise or high throttle (0.15, 0.25, 0.4).
- Wider network: in `Run`, change `nHidden1 = 32` to `64`.
- Different activation: replace `relu` and `reluSlope` in `Net.forward` and
  `Net.backward` with `tanh` and its slope `1 - tanh(z)^2`.
- Longer memory: raise `ringSize`. Every slot adds 158 numbers to the model's
  discrete state, which costs compile and step time.
- Finer grid: set `nRows = 16` and `nCols = 16`.
