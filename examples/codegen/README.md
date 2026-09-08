# Codegen Examples

Codegen scenarios render built-in or custom targets from shared models.
Generated files go under `gen/`, which is ignored by git.

```bash
cargo run -p rumoca -- \
  compile examples/models/Ball.mo \
  --model Ball \
  --target jax-ode \
  --output examples/codegen/gen/ball_jax_ode

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
  --target galec \
  --output examples/codegen/gen/galec_counter
```

Scenarios:

- `rumoca-scenario.ball_jax_ode.toml`: checked ODE RHS JAX target.
- `rumoca-scenario.galec_counter.toml`: GALEC/eFMI Algorithm Code target.
- `rumoca-scenario.sympy_decay_fmi3.toml`: FMI 3.0 ME+CS export.
- `rumoca-scenario.sympy_decay_checked_dae_report.toml`: custom target
  directory that renders a readable report from the canonical checked DAE
  projection.
- `rumoca-scenario.sympy_decay_custom_checked_variables.toml`: direct raw
  Jinja template over the canonical checked DAE projection.

Custom target directories and direct templates live beside scenarios:

- `checked_dae_report/`
- `custom_checked_variables.jinja`
