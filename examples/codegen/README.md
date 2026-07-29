# Codegen Examples

Codegen scenarios render built-in or custom targets from shared models.
Generated files go under `gen/`, which is ignored by git.

```bash
cargo run -p rumoca -- \
  compile examples/models/Ball.mo \
  --model Ball \
  --target jax-solve \
  --output examples/codegen/gen/ball_jax_solve

cargo run -p rumoca -- \
  compile examples/models/SympyDecay.mo \
  --model SympyDecay \
  --target examples/codegen/checked_dae_report \
  --output examples/codegen/gen/sympy_decay_checked_dae_report

cargo run -p rumoca -- \
  compile examples/models/SympyDecay.mo \
  --model SympyDecay \
  --target examples/codegen/custom_checked_variables.jinja \
  --output examples/codegen/gen/sympy_decay_custom_checked_variables.txt

cargo run -p rumoca -- \
  compile examples/models/GalecCounter.mo \
  --model GalecCounter \
  --target galec-production \
  --output examples/codegen/gen/galec_counter_production
```

Scenarios:

- `rumoca-scenario.ball_jax_solve.toml`: checked Solve JAX target.
- `rumoca-scenario.galec_counter_production.toml`: GALEC/eFMI Production
  Code target (`.alg` plus generated C).
- `rumoca-scenario.sympy_decay_c_solve.toml`: checked Solve C target.
- `rumoca-scenario.sympy_decay_checked_dae_report.toml`: custom target
  directory that renders a readable report from the canonical checked DAE
  projection.
- `rumoca-scenario.sympy_decay_custom_checked_variables.toml`: direct raw
  Jinja template over the canonical checked DAE projection.

Custom target directories and direct templates live beside scenarios:

- `checked_dae_report/`
- `custom_checked_variables.jinja`
