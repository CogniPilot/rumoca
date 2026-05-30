# Simulation Examples

These scenarios run the Rumoca simulator in batch/report mode.

```bash
cargo run -p rumoca --release -- sim -c examples/simulation/rum.ball.toml
```

Scenarios:

- `rum.ball.toml`: bouncing ball with a browser results view.
- `rum.sympy_decay.toml`: small scalar decay model.
- `rum.switched_rlc.toml`: local switched RLC model.
- `rum.switched_rlc_msl.toml`: MSL-backed switched RLC model.
- `rum.pidmsl.toml`: MSL-backed PID model.
- `rum.kalman_filter_step_test.toml`: MSL-backed Kalman filter step test.

MSL-backed scenarios use `source_roots` that point at the pinned cache under
`target/msl/`. Run `cargo xtask repo modelica-deps ensure` first.
