# Examples Overview

Runnable examples live under `examples/` in the repository, organized by
what they demonstrate. Each runnable example is driven by a colocated
`rum.toml` scenario.

| Example | What it shows |
|---|---|
| `examples/simulation/rum.ball.toml` | Small batch simulation with plots |
| `examples/models/` | Shared Modelica models used by simulation and codegen scenarios |
| `examples/codegen/rum.ball_jax.toml` | Built-in JAX target |
| `examples/codegen/rum.sympy_decay_sympy.toml` | Built-in SymPy target |
| `examples/codegen/rum.sympy_decay_standalone_web.toml` | Custom standalone web target bundle |
| `examples/codegen/rum.sympy_decay_custom_casadi.toml` | Direct raw Jinja template |
| `examples/interactive/quadrotor/rum.acro.toml` | Interactive quadrotor SIL with 3D viewer |
| `examples/interactive/rover/rum.toml` | Interactive rover |

Run any of them the same way:

```bash
cargo run -p rumoca --release -- sim -c examples/simulation/rum.ball.toml
```

Codegen scenarios write generated files under `examples/codegen/gen/`,
which is ignored by git.

## Library Dependencies

Examples that use MSL or the CogniPilot Modelica Models need the pinned
packages fetched first:

```bash
cargo xtask repo modelica-deps ensure
```

The repository's committed VS Code workspace settings point at the fetched
packages for the common open modes (repository root, `examples/`, and the
quadrotor directory).

## In-Browser Examples

The [Live Examples](./live.md) page collects runnable models embedded
directly in this book — no install needed. The same live blocks appear
throughout the guide chapters, including the
[turkey PDE animation](../language/arrays-pde.md).
