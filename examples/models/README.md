# Shared Models

This directory contains Modelica models shared by simulation and codegen
scenarios. The files are intentionally top-level models or packages, not an
`Examples` package, so a `rum.toml` scenario can sit next to any workflow and point
at the model it needs.

- `Ball.mo`: bouncing ball with `reinit` event behavior.
- `Circuit.mo`: small package-style circuit model.
- `KalmanFilterStepTest.mo`: MSL-backed Kalman filter step test.
- `PIDMSL.mo`: MSL-backed PID example.
- `SwitchedRLC.mo`: local switched RLC model.
- `SwitchedRLC_MSL.mo`: MSL-backed switched RLC model.
- `SympyDecay.mo`: small model used by symbolic codegen examples.
