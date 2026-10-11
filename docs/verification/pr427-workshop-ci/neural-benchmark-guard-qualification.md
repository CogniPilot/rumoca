# Neural benchmark evidence owner

Original source plus guards at `e642c54b9c81b82463c3c1484cf669934991bccf`
fails all three lightweight controls. The parser cannot consume a trailing
nested JSON object; the parity helper accepts missing required channels and
NaN/Inf values. The nested-report control is parser robustness evidence, not
an observed failure of the current flat CLI benchmark report.

At `2636a6b3e7932dab948c0f4d41d424f4a6227555`, the unchanged controls pass
three of three. Both runs use the provisioned Python environment without Cargo
or simulator execution:

```bash
nix develop .#ci-python-wheel --command bash -c '
  unset PYTHONPATH
  "$HOME/scratch/rumoca/workshop427-exports/cache/venv/bin/python" \
    crates/rumoca-bind-python/tests/benchmark_test.py --guards-only
'
```

The full benchmark remains **UNRUN**. Its required CI registration shares the
installed-wheel roster in `cargo xtask python test --require-guide`, selected
by `verify full` and the existing Linux GNU wheel lane. It preserves the
authored Backprop1k dimensions (hidden 30, batch 8, **1082** trainable states),
0.2-second horizon, 0.02 output interval, three iterations and one warmup.
Only CSV/JSON destinations are moved into a process-owned temporary directory.

The full check requires an exact eleven-point grid, all six actual output
channels, finite values, independent JAX output curves at every sample, and
all 1082 manual/reverse JAX gradients. Initial algebraic outputs use a `1e-10`
bound; integrated outputs use `20 * (1e-6 + 1e-6 * abs(expected))`, anchored
to the documented default RK absolute/relative solver tolerances. Gradient
agreement uses `1e-10`. Timings are reported separately from correctness.

The public CSV exposes outputs rather than internal weights or gradients.
Those cross-backend comparisons are explicitly unavailable; missing columns
never manufacture a zero error. Actual native engine selection is not inferred
from Auto policy. The guide prose states the observable and JAX-gradient scope.

[The JSON receipt](neural-benchmark-guard-qualification.json) pins actual raw
logs, guardians, source revisions and resource outcomes. Fresh full benchmark,
combined-source wheel, remote CI and instrumented coverage are still pending.
