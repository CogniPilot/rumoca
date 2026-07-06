# Simulation Examples

These scenarios run the Rumoca simulator in batch/report mode.

```bash
cargo run -p rumoca --release -- sim -c examples/simulation/rumoca-scenario.ball.toml
```

Scenarios:

- `rumoca-scenario.ball.toml`: bouncing ball with a browser results view.
- `rumoca-scenario.neural_latent_oscillator.toml`: larger latent Neural ODE with learned decoder.
- `rumoca-scenario.neural_ode_backprop.toml`: native backpropagation training demonstration.
- `rumoca-scenario.neural_ode_tensor.toml`: tensor-shaped spiral Neural ODE with matrix-vector layers.
- `rumoca-scenario.neural_predator_prey.toml`: tensor-shaped predator-prey Neural ODE.
- `rumoca-scenario.sympy_decay.toml`: small scalar decay model.
- `rumoca-scenario.switched_rlc.toml`: local switched RLC model.
- `rumoca-scenario.switched_rlc_msl.toml`: MSL-backed switched RLC model.
- `rumoca-scenario.pidmsl.toml`: MSL-backed PID model.
- `rumoca-scenario.kalman_filter_step_test.toml`: MSL-backed Kalman filter step test.

MSL-backed scenarios use `source_roots` that point at the pinned cache under
`target/msl/`. Run `cargo xtask repo modelica-deps ensure` first.

The Neural ODE scenario uses a modest default hidden width for report size.
Edit `nHidden` in `examples/models/NeuralODETensor.mo` to re-instantiate a
larger tensor network; `nHidden = 314` gives about 100000 trainable parameters.
The predator-prey Neural ODE has a similar large-model knob; `nHidden = 313`
gives about 100000 trainable parameters.
The latent Neural ODE is larger by default with 19339 trainable parameters;
`nHidden = 256` gives 71435 and `nHidden = 512` gives 273931 trainable
parameters.
