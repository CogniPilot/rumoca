# Canyon Run: Learning to Fly a Box Canyon

This live example teaches a small fixed-wing aircraft to fly down a winding box
canyon by reinforcement learning. Three things are worth knowing about it:

- **The vehicle is real.** The aircraft in every episode is the full 6-DOF
  aerodynamic plant and fly-by-wire autopilot of the fixed-wing example, not a
  simplified point mass.
- **The learner is fast because its view is simple.** Instead of a camera, the
  network is handed a noise-free local occupancy grid of the canyon around the
  aircraft, so ten decisions a second cost little; natively, learning shows up
  within a few minutes of computing time.
- **The physics is differentiable.** The aircraft's in-flight physics
  (`CanyonRun.Physics.Glide`, which reproduces the full plant's position
  trajectory bit for bit over the tested airborne envelope) has exact
  forward-mode and reverse-mode (adjoint) sensitivities.

Every part is ordinary Modelica in one editable source: the aircraft, the
canyon, the network, its gradient and its optimizer. Change the network in the
editor and the next run trains the new one, with no change to the compiler.

```modelica,interactive
// rumoca-live-scenario: ../repo-examples/interactive/canyon_run/rumoca-scenario.toml
```

In the viewer, `T` switches between watching in real time and training as fast
as the host allows, `P` shows or hides the sampled flight paths, `R` restarts
learning from the initial network, `C` changes the camera, `F` toggles
fullscreen and `Q` stops the run.

The live view plots the result of each episode. The reward is 0.1 for every
metre flown down the canyon, minus 5 for a crash and plus 5 for reaching the
finish 600 m downstream. An episode ends when the aircraft touches a wall, the
floor or the rim, slows below 4 m/s, reaches the finish, or runs out of time
after 60 s. On that same sample the aircraft and its autopilot are put back at
the canyon entrance; the learned weights and the replay ring carry over.

For a native run:

```bash
cargo xtask repo modelica-deps ensure
cargo run -p rumoca --release -- \
  sim -c examples/interactive/canyon_run/rumoca-scenario.toml
```

## The Aircraft

The aircraft is the plant and fly-by-wire controller from the fixed-wing
example (`examples/interactive/fixedwing`): a 6-DOF aerodynamic model on the
shared rigid-body package, flown through a rate-command, attitude-hold
autopilot. The learner never moves a control surface. It moves the sticks, as a
pilot would: a roll or pitch deflection slews the held bank or pitch setpoint,
and releasing the stick holds the new attitude.

It has five actions: fly level, bank left, bank right, pitch up and pitch down,
all at a fixed cruise throttle of 0.25. Smooth turns come from holding a bank action for
several steps, because the autopilot keeps whatever attitude it was last given.
Five is enough: a scripted pilot using exactly these five actions flies the
whole canyon every time, while one that may only roll drifts over the rim, so
pitch is needed and throttle choice is not.

## The Canyon and What the Agent Sees

The canyon is a box canyon 600 m long with a 30 m rim. Its centreline winds
from side to side by up to about 11.5 m, and its half-width varies between 13
and 19 m, so the walls close in and open out as it turns. Both are plain
Modelica functions of distance along the canyon (`Course.centreLine`,
`Course.halfWidth`). The aircraft starts 12 m above the floor at 15 m/s,
pointing down the canyon.

The network sees 79 numbers ten times a second:

- an 8 by 8 occupancy grid covering 40 m ahead of the aircraft and 20 m to
  either side, in 5 m cells. The grid turns with the aircraft's heading but
  stays level, so a cell always means the same place relative to where the
  aircraft is going. Each cell holds the fraction of four sample points in it
  that lie outside the canyon walls, computed from the canyon geometry
  (`Course.occupancy`);
- 15 numbers of true state: the sideways offset from the centreline as a
  fraction of the half-width, the height, the centreline's slope, the attitude
  quaternion, velocity, body rates, and the autopilot's two held setpoints,
  bank and pitch, which say what the last stick commands asked for.

The grid is perfect on purpose. This example is about learning a control
policy, not about mapping or state estimation, and a noise-free local view keeps
each decision cheap so that learning is visible quickly.

## The Learner, in Plain Modelica

The network maps the 79 observations through hidden layers of 32 and 16 units
to one value per action. It, its loss and its optimizer are Modelica functions
in the `CanyonRun.Net` package:

- `dense` computes `W*x + b`;
- `relu` is written as `0.5*(x + abs(x))`;
- `forward` returns the value of every action;
- `backward` applies the chain rule by hand, mirroring `forward` layer by layer;
- `tdTarget` forms the Q-learning target, and the loss is a Huber loss on the
  taken action's value;
- `adamStep` performs one Adam update of one weight array.

The weights are `discrete` arrays. Their initial values come from a fixed,
counter-based hash scaled for each layer's width, so every run starts from the
same network, and the same hash drives exploration. Learning happens in one
`when sample(...)` block in the agent, in three steps:

1. **remember** stores the last transition (observation, action, reward, next
   observation, whether it ended the episode) in a replay ring of 64;
2. **act** chooses the next action from the network as it stood at the start of
   the sample, epsilon-greedy, with exploration falling from 1 to 0.05 over the
   first 8000 decisions;
3. **learn** draws a batch of 16 from the ring as it stood at the start of the
   sample, forms the targets (discount 0.97), back-propagates and takes one Adam
   step. A transition
   stored on this sample joins the draws on the next one.

Nothing is hidden in a runtime hook; the block you read is the block that runs.
The forward pass is three lines of algorithm:

```modelica,fragment,context-canyon-forward
    function forward "Q-value of every action for observation x"
      input Real W1[:, :];
      input Real b1[size(W1, 1)];
      input Real W2[:, size(W1, 1)];
      input Real b2[size(W2, 1)];
      input Real W3[:, size(W2, 1)];
      input Real b3[size(W3, 1)];
      input Real x[size(W1, 2)];
      output Real q[size(W3, 1)];
    protected
      Real h1[size(W1, 1)];
      Real h2[size(W2, 1)];
    algorithm
      h1 := relu(dense(W1, b1, x));
      h2 := relu(dense(W2, b2, h1));
      q := dense(W3, b3, h2);
    end forward;
```

and the agent's sample block reads in the same order as the list above
(Adam steps for the remaining layers elided):

```modelica,fragment,context-canyon-agent
    when sample(0, dt) then
      (ringObs, ringAct, ringReward, ringNext, ringDone, stored) := Net.remember(
        pre(ringObs), pre(ringAct), pre(ringReward), pre(ringNext), pre(ringDone), pre(stored),
        pre(prevValid), pre(prevObs), pre(action), reward, obs, terminal);
      epsilon := max(epsEnd, epsStart - (epsStart - epsEnd)*pre(steps)/epsDecaySteps);
      q := Net.forward(pre(W1), pre(b1), pre(W2), pre(b2), pre(W3), pre(b3), obs);
      action := Net.chooseAction(q, epsilon, pre(steps), 2*seed, terminal or truncated, defaultAction);
      learning := min(pre(stored), ringSize) >= batchSize;
      (gW1, gb1, gW2, gb2, gW3, gb3) := Net.batchGradient(
        pre(W1), pre(b1), pre(W2), pre(b2), pre(W3), pre(b3),
        pre(T1), pre(c1), pre(T2), pre(c2), pre(T3), pre(c3),
        pre(ringObs), pre(ringAct), pre(ringReward), pre(ringNext), pre(ringDone),
        min(pre(stored), ringSize), batchSize, pre(steps), 2*seed + 1, gamma);
      updates := pre(updates) + (if learning then 1 else 0);
      (W1, mW1, vW1) := Net.adamStep(pre(W1), gW1, pre(mW1), pre(vW1), max(1, updates), learningRate);
```

## What It Learns

Starting from its initial network, the agent learns to fly farther down the
canyon. The table is the mean distance flown per block of 100 episodes, for
three training runs that differ only in their random seed and for a control
that keeps the initial network frozen but explores the same way. Each run is
6000 s of simulated flight, advanced 1 s at a time, so runs with shorter
episodes fit more of them in.
A cell marked with its own range and n holds only the episodes left at the
end of that run. The
frozen control, whose episodes are shortest, stays at 28 m for all 2168 of its
episodes; the table shows its first thirteen blocks.

| Episodes | Seed 1 | Seed 2 | Seed 3 | Frozen control |
|---|---|---|---|---|
| 1–100 | 61 m | 54 m | 61 m | 42 m |
| 101–200 | 68 m | 56 m | 55 m | 31 m |
| 201–300 | 77 m | 58 m | 48 m | 28 m |
| 301–400 | 63 m | 57 m | 57 m | 28 m |
| 401–500 | 70 m | 63 m | 63 m | 28 m |
| 501–600 | 91 m | 64 m | 60 m | 28 m |
| 601–700 | 91 m | 68 m | 65 m | 28 m |
| 701–800 | 99 m | 75 m | 64 m | 28 m |
| 801–900 | 129 m | 71 m | 67 m | 28 m |
| 901–1000 | 125 m (901–928, n = 28) | 78 m | 70 m | 28 m |
| 1001–1100 |  | 85 m | 81 m | 28 m |
| 1101–1200 |  | 86 m (1101–1181, n = 81) | 72 m | 28 m |
| 1201–1300 |  |  | 58 m (1201–1214, n = 14) | 28 m |

Over its last 100 episodes each trained run flies farther than over its first
100 (61 → 128 m, 54 → 81 m and 61 → 69 m), and together they average 93 m
against 28 m for the frozen network. The progress is real but noisy, and
uneven between seeds. The longest flights were 543, 391 and 312 m. No run
reached the finish 600 m downstream within its 6000 s. Of the trained runs'
3323 episodes, 2697 end at a wall, 625 on the floor and one over the rim; the
frozen network hits a wall in every episode. The runs are
deterministic when driven the same way: repeating seed 1 with the same
simulation advance interval reproduces every episode exactly, while a different
advance interval can change the solver's steps and give a different, equally
valid run.

Each 6000 s run took between 266 and 337 s on one core, 18 to 23 times faster
than real time, using under half a gigabyte of memory.

## The Aircraft Is Differentiable


The plant the agent flies can be linearized as it stands, and in flight it can be
differentiated along a whole trajectory.
Two scopes are worth keeping apart; neither covers the closed-loop learning
episode, whose autopilot limits, saturations and resets are deliberate switches.

**At a point, the full hybrid plant.** Linearizing the original plant, gear and
all, at a flight condition needs no changes to the model:

```bash
cargo xtask repo modelica-deps ensure
cargo run -p rumoca --release -- \
  sim examples/interactive/canyon_run/CanyonRun.mo -m CanyonRun.Physics.HybridGlide \
  --source-root target/cmm/CMM-a642c381 --t-end 0.1 --inspect linearize
```

This prints the 13 by 13 state matrix `A` at the start of the glide: how the
rate of each position, velocity, attitude and body-rate state responds to each
of them.

**Along a flight, the airborne plant.** A whole flight is differentiable only
where the model has no switching surface for the derivative to cross. The
shipped plant has two that matter on the ground, not in the air: landing-gear
contact, and the choice of reference axis in its wind frame.
`CanyonRun.Physics.AirborneFixedWing` keeps the plant's aerodynamics unchanged,
drops the gear, fixes the reference axis to the body z axis (valid while the
angle of attack stays below 90 degrees) and writes each vector product out per
component. In flight it matches the original where tested: six 3 s flights
spanning elevator, aileron and throttle settings, start speeds of 12 to 20 m/s
and up to 21 m of crossrange produce bit-identical positions at twelve sample
times across each 3 s flight, and at every output step of the nominal flight,
on it and on the original hybrid plant (`CanyonRun.Physics.Glide` and
`CanyonRun.Physics.HybridGlide`).

On that plant, forward-mode and reverse-mode (adjoint) sensitivities of the
altitude after the glide agree with each other to about eight digits, and
with central finite differences:

| Parameter | Forward mode | Reverse mode (adjoint) | Finite difference |
|---|---|---|---|
| elevator | 25.858287 | 25.858287 | 25.8583 |
| throttle | 25.088116 | 25.088116 | 25.0881 |
| aileron | −1.695122 | −1.695121 | |
| drag coefficient `CD0` | −83.193099 | −83.193098 | |

```bash
cargo run -p rumoca --release -- \
  sim examples/interactive/canyon_run/CanyonRun.mo -m CanyonRun.Physics.Glide \
  --source-root target/cmm/CMM-a642c381 --solver rk-like --t-end 3 \
  --inspect objective-gradient --grad-mode forward \
  --terminal z --wrt elevator,aileron,throttleSetting,vehicle.CD0
```

Replace `forward` with `adjoint` for reverse mode. Forward mode carries one
sensitivity per parameter along with the flight; reverse mode integrates once
forward and once backward and yields the gradient with respect to every
parameter from that single backward pass.

The learning loop itself does not use these gradients: the agent learns from
reward alone, as Q-learning does. They show that the vehicle model in the loop
is a real, differentiable flight model.

## Try This

All sizes are parameters of `CanyonRun.Run`, marked `annotation(Evaluate = true)`
so that the compiler can fix the array shapes they control.

- **Wider:** change `nHidden1 = 32` to `nHidden1 = 64`. One line.
- **Deeper:** add a hidden layer: its weights, one `dense` and `relu` line in
  `forward`, the mirrored lines in `backward`, and one more `adamStep`.
- **Smoother:** swap `relu` for `tanh` wherever it appears in `forward` and
  `backward`, and its slope `reluSlope(z)` for `1 .- tanh(z).^2` in `backward`.
- **Longer memory:** raise `ringSize = 64`.
- **Throttle:** set `nThrottle = 3` to give each manoeuvre low, cruise and high
  throttle (fifteen actions).
- **A wider view:** raise `nRows = 8` and `nCols = 8`; the observation size
  follows.

Each edit recompiles in the editor and trains from scratch. In this release a
larger network, grid or replay ring takes longer to compile and to run, roughly
in proportion to its size, so the defaults are kept small enough for the
browser.

## What the Compiler Does With It

Natively, the example compiles and prepares in about 4 s on one core. In the
browser the same model runs through the WebAssembly backend, and the live
widget shows its progress as it trains.

The edits above change only the Modelica source. Before this release the
example was checked once, outside the repository's test suite: `backward`
matches central differences of `forward` at the shipped sizes; the exploration
hash and the initial weights match an independent reference implementation bit
for bit, and the initial weights have full rank; a short run stores exactly the
transitions the three steps above describe; and repeating a training run with
the same seed and simulation advance interval reproduces every episode exactly
(a different advance interval can change the solver's steps, and so the run).

## Going Further

- An actor-critic learner with continuous stick commands.
- A camera and a map built from it, instead of a noise-free grid.
- Estimating the pose from sensors instead of reading the true state.
