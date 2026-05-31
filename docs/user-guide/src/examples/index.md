# Examples Overview

Examples live under `examples/`.

Useful starting points:

- `examples/simulation/rum.ball.toml`: small simulation with plots
- `examples/models/`: shared Modelica models used by simulation and codegen scenarios
- `examples/codegen/rum.ball_jax.toml`: built-in JAX target example
- `examples/codegen/rum.sympy_decay_sympy.toml`: built-in SymPy target example
- `examples/codegen/rum.sympy_decay_standalone_web.toml`: custom standalone web target example
- `examples/codegen/rum.sympy_decay_custom_casadi.toml`: direct raw Jinja template example
- `examples/interactive/quadrotor/rum.acro.toml`: interactive quadrotor scenario
- `examples/interactive/rover/rum.toml`: interactive rover scenario

Codegen scenarios write generated files under `examples/codegen/gen/`, which is
ignored by git.

Browser examples should be native book components: a focused Monaco editor,
in-page model files, Rumoca WASM calls, and the simulation view needed for the
example. They should not embed the full playground workbench.

Fetch pinned external Modelica package dependencies before running examples that
use MSL or CogniPilot Modelica Models:

```bash
cargo xtask repo modelica-deps ensure
```

The repository includes VS Code workspace settings that point to the fetched
packages from common open modes.
