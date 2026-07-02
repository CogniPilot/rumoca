# Rumoca Examples

Examples are organized by workflow:

- `models/`: shared Modelica models used by multiple examples.
- `simulation/`: batch simulation scenarios that produce plots or reports.
- `interactive/`: browser or input-driven simulations with scene assets.
- `benchmarks/`: reproducible cross-runtime comparisons for selected models.
- `codegen/`: code generation scenarios and custom target bundles.
- `codegen/custom_casadi.jinja`: direct raw-template codegen example.

Generated code from `codegen/` scenarios goes under `codegen/gen/`, which is
ignored by git.

The Neural ODE example in `models/NeuralODETensor.mo` is written as tensor
array equations. Its derivative RHS follows the blog-style spiral Neural ODE
shape with `W1 * x`, `Wmid * a1`, and `W2 * a2` matrix-vector products that
Solve IR preserves as `MatMul` tensor nodes. The runnable simulation profile
keeps `nHidden` small; edit that structural parameter to re-instantiate larger
networks.

`models/NeuralPredatorPrey.mo` applies the same tensor setup to a
Lotka-Volterra-style Neural ODE. The network predicts prey/predator
per-capita growth rates from normalized populations and a seasonal input.

`models/NeuralLatentOscillator.mo` is the larger Neural ODE example: an
eight-state latent oscillator with a learned residual vector field and decoder.
Its default hidden width instantiates 19339 trainable parameters; increasing
`nHidden` to 256 instantiates 71435 parameters.

`models/NeuralODEBackprop.mo` is a native training demonstration. It treats
network weights as states, computes mini-batch spiral vector-field loss, and
evaluates the backpropagation equations directly in Rumoca. Public training
APIs should use `rumoca-opt`; this model is a transparent equation-level demo.

## Modelica Dependencies

Pinned external Modelica packages are listed in `modelica_dependencies.toml`.
The MSL-backed examples use `ModelicaStandardLibrary`; the quadrotor examples
use CogniPilot Modelica Models.

The easiest setup is:

```bash
cargo xtask repo modelica-deps ensure
```

That downloads the pinned archives into `target/msl/` and `target/cmm/`, which
matches the committed example `source_roots`.

Without `rum`, download the same packages yourself, then either:

- Put their package roots in the `rumoca-scenario.toml` scenario `source_roots`.
- Add their package roots to `MODELICAPATH`.

Scenario `source_roots` are checked before `MODELICAPATH`, so a scenario can
pin a dependency path even when your shell has a broader Modelica path.
