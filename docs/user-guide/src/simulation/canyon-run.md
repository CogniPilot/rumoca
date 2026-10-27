# Canyon Run: Continuous Reinforcement Learning

A fixed-wing aircraft learns to fly a winding, 600 m canyon. Rumoca simulates
the aircraft and its inner rate controller. A PPO actor and critic learn four
simultaneous, continuous commands: forward thrust and body roll, pitch and yaw
rates. Start with [CartPole](./cartpole.md) for a smaller learning example.

The embedded view keeps the aircraft, canyon, occupancy grid and network
display together. **Replay trained flight** shows the recorded frozen policy.
**Train from scratch** uses the native companion server described below. On a
static guide host, the replay works without that server and the training
buttons are disabled.

{{#include ../../../../examples/reinforcement_learning/canyon_run/widget.inc}}

## Run the Live Trainer

The current live trainer uses the native CLI on Linux, Python 3.10 or newer,
and NumPy. It does not require a new Rumoca compiler build if you already have
a native executable. Nix is optional. From the repository root:

```bash
python3 -m pip install -r examples/reinforcement_learning/canyon_run/requirements.txt
cargo xtask repo modelica-deps ensure
npm --prefix packages/rumoca-web ci
npm --prefix packages/rumoca-web run build
cargo xtask docs build --book user
python3 examples/reinforcement_learning/canyon_run/serve.py \
  --rumoca rumoca --port 8080
```

Open `http://127.0.0.1:8080/user-guide/simulation/canyon-run.html`. If using a
locally built executable, pass `--rumoca target/release/rumoca`. The companion
also needs the generated `packages/rumoca-web/vendor/three_viewer.js` bundle;
the repository web build supplies it. `--guide-root` and `--vendor-root` can
point to an existing built guide and its vendor directory.

Training starts only when requested. One run is allowed at a time. Fast mode
uses one CPU core, while **Watch training in real time** slows the same native
loop to its 0.05 s control period. Switch back to fast mode to speed learning
up. The default budget is 131,072 transitions with a 900-second wall-time
limit, including time spent watching. The native child has a 2 GiB RSS guard
and a 4 GiB address-space limit. Results, rollouts, episode records and
checkpoints go into a new directory under `canyon-runs`.

The view uses the same detailed airplane and canyon scene as the earlier
example. The occupancy panel shows the actual 8×8 observations. The network
panel shows the actor's actual weights, transposed for the diagram's
destination-row convention, with four continuous outputs. `N` toggles those
actor panel while keeping the occupancy map visible, `P` toggles paths, `T` switches training speed and `F` toggles fullscreen. Buttons expose the same visibility controls.
Drag to orbit, right-drag to pan, and scroll or pinch to zoom. **Overview camera**
looks down the course; **Top-down camera** shows its plan view. **Follow camera**
returns to the aircraft. These camera changes do not affect the simulated flight.
The HUD reports a rolling ratio of simulated seconds to wall seconds during live
training, including policy-update pauses and episode resets. Replay labels its
playback speed separately. The actor panel is reduced in the default view; the
episode-return overlay is hidden during replay. Live pose updates are fetched at the
control rate and visually interpolated between samples; episode resets snap to
the new start. The position readout follows that interpolated pose; other HUD values and
occupancy remain sampled simulator data.
Decorative rock erosion and animated surfaces are visualization; termination
uses the Modelica canyon geometry at the sample times.

## Train or Evaluate Without the Viewer

```bash
python3 examples/reinforcement_learning/canyon_run/train.py \
  --rumoca rumoca --output canyon-training --steps 131072 --wall 900
python3 examples/reinforcement_learning/canyon_run/evaluate_policy.py \
  --rumoca rumoca --checkpoint canyon-training/checkpoint-131072.npz \
  --output canyon-frozen
```

Both output directories must be new. Override `--source-root` if the CMM
library is somewhere other than `target/cmm/CMM-a642c381`.
To fly the included trained actor, use
`--checkpoint examples/reinforcement_learning/canyon_run/actor.npz` instead.
That actor-only export preserves the trained arrays; it does not contain
optimizer or critic state and is not a full training checkpoint.

Each training checkpoint includes the actor, critic, Adam state and random
generator state. The native environment is not checkpointed, so resuming the
entire simulator state is not supported. Starting another experiment from
saved learning state must start a new episode and be recorded as a new run.

## Controls and Observations

The aircraft and canyon physics remain those of the earlier guide. The agent
now supplies all four commands at once; it does not select bank/climb bins:

| Command | Units | Bounds |
|---|---|---|
| Forward propeller thrust divided by mass | m/s² | 0 to 8 |
| Body roll-rate demand, p | rad/s | −4 to 4 |
| Body pitch-rate demand, q | rad/s | −2.5 to 2.5 |
| Body yaw-rate demand, r | rad/s | −1 to 1 |

Rates use body forward-left-up axes. Positive q is nose-down in those axes;
the inner elevator controller converts that sign once. The thrust command
scales to throttle using the aircraft mass and maximum propeller thrust.
This is a fixed-wing adaptation of continuous thrust/rate control, not a
quadrotor's vertical collective-thrust model.

The actor and critic each read 86 values: normalized position and world
velocity, a rotation matrix, body rates, a heading-aligned 8×8 occupancy grid
and the previous applied controls. Inputs are clipped to [−5, 5] before the
networks. The occupancy grid is computed from true canyon geometry. This is
a control-learning example, without camera perception or state estimation.

The policy has two 64-unit tanh layers. It samples four Gaussian latent
actions and maps them through tanh to the physical command bounds. The
critic has the same hidden widths and one value output. PPO uses the stored
latent actions for likelihood ratios; it never substitutes clipped commands
or discrete action indices into that ratio. Reported entropy is the latent
Gaussian entropy, rather than the entropy after the tanh transform.

The defaults are Adam at 3×10⁻⁴, discount 0.99, GAE parameter 0.95, PPO clip
0.2, 1,024-step rollouts, eight update epochs and 128-row minibatches. A KL
check can stop an update early. The core and its finite-difference controls
are in `ppo.py` and `test_ppo.py`.

## Rewards, Termination and Reset

Reward combines forward progress, +0.1 per metre, with lateral-centering,
height, command-rate and command-change costs. A crash costs 5 and finishing
adds 5. The quadratic height cost coefficient is 0.1; the earlier 0.01
development variant finished but flew too close to the floor.

An episode ends at a sampled wall/floor/rim collision, speed below 4 m/s, or
the 600 m finish. It is truncated after 1,200 steps, or 60 simulated seconds.
The host captures the old episode's state, reward and terminal flags before
resetting. A true terminal state has zero value bootstrap. A time limit
bootstraps from that old final observation, then stops GAE across the reset.
The Modelica model does not reset itself underneath the learner.

## Measured Development Result

A seed-1 run completed 131,072 transitions and 342 episodes, with 110
finishes. Its last 50 training episodes all finished. The final frozen policy,
with learning and action noise disabled, flew 600.60 m in 34.35 simulated
seconds. Its minimum sampled altitude was 6.50 m and minimum sampled lateral
clearance was 4.95 m. The weights stayed unchanged during that flight.

The same policy also finished six predeclared development checks with ±1 m
lateral/height offsets and ±0.05 rad heading offsets. Two initial model-rendering
setup failures were retained; those two cases ran successfully after the
signed-offset expressions were corrected. No policy tuning occurred between
the six cases. These are small perturbations on the training course, not
unseen-course, wind, sensor-noise or hardware generalization results.

The included replay records the simulated pose, controls, occupancy observations
and actuator commands. `demo-flight.json` binds it to the source training
checkpoint; `actor.npz` is a separately hashed actor-only export. The replay
is labeled as recorded data. Live training comes from the native companion
and is labeled separately.

## Relationship to the Earlier Example

`CanyonRun.Run` remains the earlier five-action, fixed-throttle Modelica
Q-learner. This chapter's PPO path uses `CanyonRun.ContinuousControl.Run` and
the Python learner. The old Q-learning results do not measure this new
controller. The native PPO demo also does not imply that PPO training has
been ported into the WebAssembly learner.

The airborne physics sensitivity examples remain available in
`CanyonRun.Physics`. PPO itself learns from reward and does not use those
physics gradients. For the continuous-control motivation, see
[Kaufmann et al., Nature 2023](https://rpg.ifi.uzh.ch/docs/Nature23_Kaufmann.pdf).
The fixed-wing plant, observations, bounds and rewards here are an engineering
adaptation, not a reproduction of that quadrotor racing system.

## Run the Frozen Actor in Modelica

The trained continuous actor can also run without a Python control loop.
`CanyonRun.ContinuousControl.CanyonFly` samples the same observation, calls
the shared [NeuralNetwork functions](./neural-network.md), then holds its four
commands for 0.05 s. `TrainedCanyonPolicy.mo` contains only exported weights.
Previous commands are explicit state, so a newly computed action cannot feed
back into the observation used to select it.

```bash
rumoca sim examples/interactive/canyon_run/CanyonRun.mo \
  --model CanyonRun.ContinuousControl.CanyonFly \
  --source-root examples/reinforcement_learning/NeuralNetwork.mo \
  --source-root examples/reinforcement_learning/canyon_run/TrainedCanyonPolicy.mo \
  --source-root target/cmm/CMM-a642c381 --solver rk-like
```

A checked native frozen-actor flight reached 600.60 m at 34.35 simulated
seconds. Its endpoint differed from the Python-driven flight by less than
1e-11 m. This checks one nominal inference flight, not a new training result
or held-out generalization. Continuous PPO training still uses the Python
implementation; its optimizer has not been ported to Modelica.

<details>
<summary>Read the Python network and PPO math</summary>

```python
{{#include ../../../../examples/reinforcement_learning/canyon_run/ppo.py}}
```

</details>

<details>
<summary>Read the rollout and training loop</summary>

```python
{{#include ../../../../examples/reinforcement_learning/canyon_run/train.py}}
```

</details>
