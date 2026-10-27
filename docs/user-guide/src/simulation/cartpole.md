# CartPole: Learn Swing-Up and Balance

Start with CartPole before [Canyon Run](./canyon-run.md). The cart applies one
horizontal force to swing a hanging pole up and then balance it. The physics,
learner, random-number state and episode resets all run in **Modelica**.
The viewer displays their results; it does not select weights.

## Train in the Editor

Press **Run** below. The initial pole angle is π − 0.01 radians: 0.57° off
straight down, with the cart and pole at rest. This small asymmetry is explicit;
exactly symmetric down-start performance is not claimed. All seven policy
gains begin at zero.

The example starts in **Realtime**. Choose **Fast** to learn sooner and switch
back to watch the learned policy. It freezes its gains after five simulated
seconds of balance. The cart centre must stay within **±2 m**. Attempting to
leave a rail ends and penalizes that episode, then resets the plant.

```modelica,interactive
// rumoca-live-scenario: ../repo-examples/reinforcement_learning/cartpole/rumoca-scenario.toml
```

Drag the 3D view to rotate; right-drag to pan; scroll or pinch to zoom.
**Reset view** restores the camera. **R** restarts training. After learning,
**Bump left/right** requests an actual ±20 N force for 0.3 simulated seconds.
A disturbance can still cause failure: the plant resets, retaining the frozen
gains. The model bounds the combined policy and bump force to ±30 N.

The diagrams show actual learned gains and observed episode rewards. Fast mode
can skip intermediate episodes in the displayed plot; training still evaluates
each episode. Realtime pacing and simulation time are separate from rendering.

## A Small Policy with an Explicit Physics Prior

A single tanh neuron combines seven clipped features. Three pump the pole's
energy while it hangs down; four emphasize balance near the upright position.
The features depend on cart position/velocity, sin/cos of the pole angle and
angular velocity. This physical feature map is fixed and documented in the
source. **All seven gains are learned**; no stabilizing gains are preset.
This is a small policy-search example, not an end-to-end deep actor.

The learner retains the two best policies across episodes and refits a diagonal
Gaussian every 32 evaluations, with a minimum standard deviation of 1. Each
candidate gets at most 20 simulated seconds. Its reward favors pole height and
sustained balance, penalizes energy error, cart displacement and force, and
subtracts 50 on a rail failure. Success requires cos(angle) > 0.98,
|angular velocity| < 1 rad/s and |cart position| < 1.2 m for 250 control ticks.
The control period is 0.02 s. A one-sample observation/weight delay and the
one-candidate distribution-refit delay are explicit in the source.

The shared [NeuralNetwork package](./neural-network.md) supplies the dense layer,
tanh, clipping and Gaussian samples. `PolicySearch.mo` owns candidate sampling,
elite retention and refitting; `CartPoleSwingUp.mo` owns the task and force.

One checked native seed-1 run evaluated 84 policies before freezing. A separate
30-second run from the same near-down start, with learning disabled, held the
balance criterion from 1.92–30.00 s: **28.08 s**. Maximum absolute cart position
was **1.498 m**, with no rail contacts or resets and unchanged weights. Six
separate boundary controls checked both signs, inward noncontacts and outward
starts at the limits. These are nominal development results, not held-out,
multiple-seed, disturbance or Gymnasium benchmark acceptance.

Native equivalent, from the repository root:

```bash
rumoca sim examples/reinforcement_learning/cartpole/CartPoleSwingUp.mo \
  --source-root examples/reinforcement_learning/NeuralNetwork.mo \
  --source-root examples/reinforcement_learning/cartpole/PolicySearch.mo \
  --source-root examples/reinforcement_learning/cartpole/CartPole.mo \
  --model CartPoleSwingUp --solver rk-like
```

<details>
<summary>Read the Modelica policy-search implementation</summary>

```modelica
{{#include ../../../../examples/reinforcement_learning/cartpole/PolicySearch.mo}}
```

</details>

## Compare the Python Upright-Balance Route


This older comparison teaches upright balancing from a small initial tilt;
it does not perform the swing-up task above. It uses a four-gain policy and
a separate cross-entropy search. The checked replay below belongs to
that separate run; it is not presented as new training.

{{#include ../../../../examples/reinforcement_learning/cartpole/widget.inc}}

### Run the Python Learner

From the repository root, with a native `rumoca` executable on your path:

```bash
python3 examples/reinforcement_learning/cartpole/train.py \
  --rumoca rumoca --output cartpole-run
```

If you built the CLI locally, use `--rumoca target/release/rumoca` instead.
The output directory must be new. The default run uses seed 1, eight
generations of 16 candidate policies, and a 180-second wall-time limit.

The program first measures a zero-force baseline, then prints the best episode
length and population mean after each generation. It saves the learned gains,
training history and a final evaluation in `cartpole-run/result.json`.
`cartpole-run/flight.csv` records that final flight's states and force, with
learning disabled.

In a checked native run, the zero-force policy lasted 34 steps. The best policy
reached the 500-step limit in generation 2 and again in its final frozen
evaluation. The population mean rose from 44.5 steps in generation 1 to 479.9
in generation 8. The run took about 9 seconds on one core; elapsed time depends
on the machine and compiler build.

## What Is Learned?

The policy is a linear combination of the four normalized states, passed
through `tanh` to bound the force to ±10 N:

```python
scales = [2.4, 2.0, math.radians(12), 2.0]
force = 10 * math.tanh(sum(w*x/s for w, x, s in zip(weights, state, scales)))
```

The four `weights` start unknown. The learner uses **cross-entropy policy
search**:

1. Sample 16 sets of gains from a Gaussian distribution.
2. Run one episode for each set and score it by how many steps it lasts.
3. Keep the best quarter and fit the next Gaussian to their gains.
4. Repeat, then evaluate the best policy without changing its gains.

This is reinforcement learning by direct policy search: episode reward selects
the gains. It does not use a hand-tuned balancing controller, a value network,
physics derivatives or backpropagation. It is smaller than the actor-critic
methods used for more complex continuous-control tasks.

Each control step advances the Modelica plant by 0.02 s. An episode ends if
the cart leaves ±2.4 m or the pole tilts beyond ±12 degrees; otherwise it is
truncated after 500 steps, or 10 s. Every step, including a terminating step,
contributes one point. The old episode is scored before resetting the simulator.
The policy search retains its gains and random-number state across resets.

## Physics and Scope

The model is an upright, frictionless cart with a hinged rigid pole. Its
parameters are a 1 kg cart, a 0.1 kg pole, a 0.5 m pivot-to-centre-of-mass
distance and gravity of 9.8 m/s². The cart starts at rest, with a 0.03 rad pole
tilt. Rumoca integrates the equations; the Python program supplies force and
reads the four states.

The equations and failure limits follow the familiar
[CartPole problem](https://gymnasium.farama.org/environments/classic_control/cart_pole/).
This example uses a continuous bounded force, a fixed initial state and
Rumoca's solver, so its scores are not Gymnasium CartPole benchmark results.
Balancing from this one start also does not prove robustness to disturbances.

Try changing `initialAngle` in the model, increasing the training population,
or evaluating the saved gains from a different starting state. Keep those
evaluation results separate from the episodes used to select the gains.

<details>
<summary>Read the complete Python learner</summary>

```python
{{#include ../../../../examples/reinforcement_learning/cartpole/train.py}}
```

</details>
