# A Neural Network in Modelica

A neural network is a composition of ordinary array functions. The shared
`NeuralNetwork.mo` package keeps those functions separate from the plant,
policy parameters and learning algorithm. CartPole uses a single neuron;
`CanyonRun.ContinuousControl.CanyonFly` uses two hidden layers and the learned CanyonRun weights.

## Rows Are Neurons

`dense(W, b, x)` calculates `y = W*x + b`. Each row of `W` belongs to one
output neuron, so its second dimension must match the input length. The
array dimensions in each function signature express those contracts directly.
The loops make the accumulation easy to inspect.

`activate(x)` applies `tanh` element by element. Its outputs lie between −1
and 1. `forward` composes two dense/tanh hidden layers and a final linear
layer; the controller applies its final activation and physical units.

```modelica
{{#include ../../../../examples/reinforcement_learning/NeuralNetwork.mo}}
```

## Learning Is Separate from Inference

The Modelica CartPole model calls `dense` and `activate` to choose a force.
The swing-up learner uses `normal` to sample candidate gains and refits its
search distribution from the two best retained policies. The older upright
example uses `perturb` to propose weights around its best policy. `normal` uses an explicit Park–Miller state and Box–Muller transform,
so learning and resets do not depend on a hidden global generator. Use a seed
between 1 and 2147483646.

The Canyon actor uses `forward` with frozen matrices from its trained policy.
The matrices are stored in a separate data package, `TrainedCanyonPolicy.mo`.
They are transposed once during export from the Python trainer's input-by-output
layout to this package's output-by-input layout. The exported actor does not
include the critic or optimizer state.

The current continuous Canyon **training** route still uses the Python PPO
implementation shown in that chapter. Calling a trained actor in Modelica
does not mean that PPO's optimizer has also been ported to Modelica.
