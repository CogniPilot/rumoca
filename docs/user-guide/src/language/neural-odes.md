# Neural ODEs

A Neural ODE replaces a hand-written right-hand side with a neural network:

```text
der(x) = f_theta(x, time, inputs)
```

The model is still an ODE. The difference is that `f_theta` is an MLP, CNN,
or other differentiable computation with trained weights `theta`. In Rumoca,
those weights are ordinary Modelica parameters, and the state trajectory is
simulated with the same solver/runtime path used by physical models.

The [Correll Lab Neural ODE review][correll-neural-odes] uses the common
spiral demonstration: train a network to describe a two-dimensional rotating
trajectory, then integrate the learned vector field. The repository example
below follows that shape. It is not a training loop; it is the deploy/simulate
side of the workflow, where a trained or initialized network is written as
tensor-shaped Modelica arrays.

```modelica,interactive
// rumoca-live-scenario: ../repo-examples/simulation/rumoca-scenario.neural_ode_tensor.toml
```

Run the same scenario from the repository root:

```bash
rumoca sim -c examples/simulation/rumoca-scenario.neural_ode_tensor.toml
```

## What the Model Does

The core equations are three matrix-vector layers with smooth activations:

```modelica
z1 = W1 * x;
a1[i] = tanh(z1[i] + b1[i]);
z2 = Wmid * a1;
a2[i] = tanh(z2[i] + bmid[i]);
der(x) = W2 * a2 + b2;
```

`x[1]` and `x[2]` are the latent state coordinates. The default parameters
seed the first two hidden channels with a damped spiral vector field and add
small dense tensor layers around it, so the phase portrait behaves like the
blog-post spiral without requiring an external training step.

The useful part for larger models is the array shape:

- `W1 * x`, `Wmid * a1`, and `W2 * a2` are Modelica matrix-vector products.
- Rumoca lowers these as native tensor operations in Solve IR instead of
  requiring you to write every scalar multiply-add by hand.
- CPU, browser, and codegen paths still have scalar fallback behavior, but the
  model source keeps the neural network structure visible.

## Parameter Count

For this two-hidden-layer network with `nState = 2` and hidden width `H`:

```text
W1:   H x 2
b1:   H
Wmid: H x H
bmid: H
W2:   2 x H
b2:   2
total = H^2 + 6H + 2
```

The committed scenario uses `H = 32`, which is small enough for a quick browser
run and gives `1186` trainable parameters. To exercise a roughly 100k-parameter
model, edit `nHidden` in `examples/models/NeuralODETensor.mo`:

```modelica
parameter Integer nHidden(min = 2) = 314;
```

That instantiates `100482` trainable parameters. For this size, prefer the CLI
over the browser guide page, because the report and default plots can dominate
runtime and memory.

## Training and Backpropagation

The examples above are simulation/deployment examples. They keep weights as
Modelica parameters and run the learned vector field. Rumoca's long-term
training surface lives one layer above Modelica in the `rumoca-opt` crate:
Modelica defines the differentiable model, while `rumoca-opt` defines the
trainables, objective, gradient method, and optimizer.

The first supported API shape is RHS fitting, which is the core Neural ODE
vector-field training problem:

```rust
let mut model = rumoca_opt::DifferentiableModel::from_dae_default(
    &compiled.dae,
    &rumoca_solver::SimOptions::default(),
)?;
let trainables = rumoca_opt::TrainableSet::by_names(&model, &["W[1,1]", "b[1]"])?;
let objective = rumoca_opt::RhsMseObjective::new(0.0, target_derivatives);
let gradient = rumoca_opt::rhs_mse_value_and_gradient(
    &model,
    &objective,
    &trainables,
    rumoca_opt::GradientMode::Auto,
)?;
```

`GradientMode::Auto` uses reverse-mode VJP for pure ODE models and falls back
to forward-mode parameter Jacobians for models with solver algebraics. That
keeps backpropagation correct now while preserving a clear extension point for
future algebraic-projection reverse mode. A simple optimizer loop is available
through `rumoca_opt::GradientDescent`.

`NeuralODEBackprop.mo` is also included as a native-equation demonstration:
the trainable weights are states and the backprop equations are written
directly in Modelica. It proves the tensor math and training dynamics can run
inside Rumoca, but it is intentionally not the preferred public API.

```modelica,interactive
// rumoca-live-scenario: ../repo-examples/simulation/rumoca-scenario.neural_ode_backprop.toml
```

## Benchmarking Against JAX

The repository includes a reproducible Neural ODE parity benchmark:

```bash
python3 -m venv target/jax-bench-venv
target/jax-bench-venv/bin/python -m pip install --upgrade pip
target/jax-bench-venv/bin/python -m pip install jax numpy
target/jax-bench-venv/bin/python examples/benchmarks/neural_ode_jax_parity.py --platform cpu
```

For CUDA-capable JAX installs, use `--platform gpu`. The script compares
`NeuralODEBackprop1k` against an equivalent JAX implementation with x64
enabled. It reports:

- Rumoca front-end `compile_seconds`, Solve IR/runtime `prepare_seconds`, and
  hot simulation time.
- JAX JIT compile-plus-first-execute time for the reverse-mode RHS, manual RHS,
  diagnostics, and full integration.
- A `comparison` block with Rumoca ready time versus JAX integrated compile
  time, hot-loop ratio, structural-prepare fraction, and max parity error.
- Accuracy checks for initial parameters, observables, gradients, final
  parameters, and JAX manual gradients versus `jax.value_and_grad`.

Use Rumoca `ready_seconds` versus JAX `integrate_compile_seconds` when you care
about first-run wait time. The benchmark also makes clear which Rumoca path is
being measured: the native Solve IR CPU simulator executes the Modelica
backpropagation equations, while the JAX side uses `jax.value_and_grad` for the
same RHS.

## Using Trained Weights

A typical workflow is:

1. Train the Neural ODE in Python/JAX/PyTorch using your preferred optimizer.
2. Export the learned arrays with the same shapes as `W1`, `b1`, `Wmid`,
   `bmid`, `W2`, and `b2`.
3. Replace the parameter declarations in the Modelica model, or generate a
   small Modelica package containing those parameter arrays.
4. Simulate, inspect, or generate code from the scenario.

Keep activation functions smooth when possible. `tanh` is a good default for
simulation because it is continuous and differentiable. If your training code
uses piecewise activations, check event behavior and solver step size before
scaling up.

## Predator-Prey Neural ODE

Lotka-Volterra predator-prey dynamics are a more interesting Neural ODE test
case because the state has physical meaning, nonlinear interaction, and a phase
portrait that should remain coherent over long horizons. The
[original Neural ODE paper][neural-ode-paper] frames these models as
continuous-depth dynamics, while [recent SciML examples][lotka-volterra-sciml]
use Lotka-Volterra systems as benchmarks for both full Neural ODEs and hybrid
Universal Differential Equations.

This second repository example takes the hybrid view. The state stores
normalized prey and predator populations. The neural network reads normalized
prey, normalized predator, and a seasonal forcing term, then outputs per-capita
growth rates:

```modelica
features[1] = population[1] / preyEquilibrium - 1.0;
features[2] = population[2] / predatorEquilibrium - 1.0;
features[3] = sin(seasonRate * time);
z1 = W1 * features;
z2 = Wmid * a1;
growth = W2 * a2 + b2;
der(population[1]) = population[1] * growth[1];
der(population[2]) = population[2] * growth[2];
```

Run it in the guide:

```modelica,interactive
// rumoca-live-scenario: ../repo-examples/simulation/rumoca-scenario.neural_predator_prey.toml
```

Or from the repository root:

```bash
rumoca sim -c examples/simulation/rumoca-scenario.neural_predator_prey.toml
```

The default width is small for report size. With hidden width `H`, this
network has:

```text
W1:   H x 3
b1:   H
Wmid: H x H
bmid: H
W2:   2 x H
b2:   2
total = H^2 + 7H + 2
```

`H = 313` instantiates `100162` trainable parameters. The two large matrix
products, `W1 * features` and `Wmid * a1`, remain tensor-shaped in the model
source, so the same example scales from a small guide run to a much larger
compiled model.

## Larger Latent Neural ODE

The [Latent ODE][latent-ode-paper] idea is to evolve a hidden continuous state
with an ODE and decode that latent trajectory into observed signals. That shape
needs more parameters than the two-state examples above: the vector field lives
in latent space, and the decoder is another learned map.

`NeuralLatentOscillator` uses eight latent states, three decoded output
channels, time features, a large recurrent tensor block, and a learned decoder:

```modelica
z1 = W1 * features;
z2 = Wmid * a1;
latentResidual = Wdyn * a2 + bdyn;
observed = Wobs * a2 + bobs;
```

The baseline dynamics are four lightly damped oscillator pairs. The neural
network adds a learned residual to the latent derivative and decodes the hidden
trajectory into three observable signals:

```modelica
der(latent[2 * k - 1]) =
  -damping * latent[2 * k - 1] - freq[k] * latent[2 * k]
  + residualGain * latentResidual[2 * k - 1];
der(latent[2 * k]) =
  freq[k] * latent[2 * k - 1] - damping * latent[2 * k]
  + residualGain * latentResidual[2 * k];
```

Run it in the guide:

```modelica,interactive
// rumoca-live-scenario: ../repo-examples/simulation/rumoca-scenario.neural_latent_oscillator.toml
```

Or from the repository root:

```bash
rumoca sim -c examples/simulation/rumoca-scenario.neural_latent_oscillator.toml
```

With `nLatent = 8`, `nFeature = 10`, and `nObserved = 3`, hidden width `H`
gives:

```text
W1:   H x 10
b1:   H
Wmid: H x H
bmid: H
Wdyn: 8 x H
bdyn: 8
Wobs: 3 x H
bobs: 3
total = H^2 + 23H + 11
```

The committed default `H = 128` instantiates `19339` trainable parameters, so
this is already much larger than the spiral and predator-prey guide examples
while staying reasonable for the examples smoke gate. For heavier runs, set
`nHidden = 256` for `71435` trainable parameters, or `nHidden = 512` for
`273931` trainable parameters while keeping the same source structure.

[correll-neural-odes]: https://medium.com/correll-lab/neural-odes-a-review-35457d66ea2b
[neural-ode-paper]: https://papers.neurips.cc/paper/7892-neural-ordinary-differential-equations
[lotka-volterra-sciml]: https://arxiv.org/html/2411.06858v1
[latent-ode-paper]: https://arxiv.org/abs/1907.03907
