# Codegen Examples

Codegen scenarios render built-in or custom targets from shared models.
Generated files go under `gen/`, which is ignored by git.

```bash
cargo run -p rumoca -- \
  compile examples/models/Ball.mo \
  --model Ball \
  --target jax \
  --output examples/codegen/gen/ball_jax

cargo run -p rumoca -- \
  compile examples/models/SympyDecay.mo \
  --model SympyDecay \
  --target examples/codegen/standalone_web \
  --output examples/codegen/gen/sympy_decay_standalone_web

cargo run -p rumoca -- \
  compile examples/models/SympyDecay.mo \
  --model SympyDecay \
  --target examples/codegen/custom_casadi.jinja \
  --output examples/codegen/gen/sympy_decay_custom_casadi.py
```

Scenarios:

- `rumoca-scenario.ball_jax.toml`: built-in JAX target.
- `rumoca-scenario.sympy_decay_sympy.toml`: built-in SymPy target.
- `rumoca-scenario.sympy_decay_standalone_web.toml`: custom target directory that renders
  standalone HTML and companion JavaScript.
- `rumoca-scenario.sympy_decay_custom_casadi.toml`: direct raw Jinja template example.

Custom target directories and direct templates live beside scenarios:

- `standalone_web/`
- `custom_casadi.jinja`
