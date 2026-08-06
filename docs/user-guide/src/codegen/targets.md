# Targets and Templates

Rumoca can render a compiled model into symbolic frameworks, compiled
simulation kernels, eFMI artifacts, or Modelica source. Code generation is
*target-directory based*: a
target is a `target.toml` manifest plus Jinja templates, and each target
declares which compiler IR stage it consumes.

## Listing Targets

```bash
rumoca targets
```

Built-in targets include:

| Target | IR | Mode | Output |
|---|---|---|---|
| `casadi-ode` | solve | symbolic | Differentiable CasADi explicit RHS |
| `jax-ode` | solve | symbolic | JIT/AD-capable JAX explicit RHS |
| `rust-fixed-ode` | solve | compiled | Fixed-size, allocation-free Rust explicit-ODE derivative kernel |
| `rust-ode` / `c-ode` | solve | compiled | Checked explicit-ODE derivative kernels |
| `cuda-ode` | solve | compiled | Batched CUDA explicit-ODE derivative kernel |
| `wgsl-ode` | solve | JIT | Experimental WebGPU explicit-ODE kernels for browser execution |
| `mlir` | solve | source | Inspectible MLIR solve-kernel source with affine tensor loops |
| `flat-modelica` / `base-modelica` | flat | source-transform | Flattened Modelica-family interchange artifacts |
| `dae-modelica` | dae | source-transform | Modelica representation of the checked DAE |
| `fmi2` / `fmi3` | fmi | standards container | Source-code Model Exchange and Co-Simulation FMUs |
| `fmi-ls-wasm` | fmi | compiled component | Experimental FMI-LS WebAssembly component crate |
| `galec` / `galec-production` | algorithm-code | eFMI | Algorithm Code and Production Code eFMU containers |
| `embedded-c-galec` | algorithm-code | compiled | GALEC-derived embedded C without an eFMI container |

Targets without a complete checked artifact and executable or independent
validation evidence are intentionally absent. Rumoca does not expose aliases
for removed target names or route those names through a weaker IR.

The `rumoca targets` table also reports a readiness level (0 = experimental
… 2 = validated) and per-feature support columns (scalarization, tensor
features such as matmul/linear solve/elementwise/stencil kernels, events, AD,
…) for each target. Treat the table — not this page — as the current source of
truth.

## Rendering a Target

```bash
rumoca compile examples/models/SympyDecay.mo \
  --model SympyDecay \
  --target c-ode \
  --output /tmp/decay_c_ode
```

`--output` may be a file or directory depending on what the target renders.

## Codegen Scenarios

Like simulations, generation jobs worth repeating belong in a `rumoca-scenario.toml`
with `task = "codegen"`. Runnable examples live under `examples/codegen/`
and write into `examples/codegen/gen/` (git-ignored):

- `examples/codegen/rumoca-scenario.ball_jax_ode.toml` — checked ODE RHS JAX target
- `examples/codegen/rumoca-scenario.sympy_decay_c_ode.toml` — checked ODE RHS C target
- `examples/codegen/rumoca-scenario.sympy_decay_checked_dae_report.toml` —
  custom checked-DAE report target
- `examples/codegen/rumoca-scenario.sympy_decay_custom_checked_variables.toml`
  — raw checked-DAE Jinja template

## IR Dumps vs Targets

If what you want is to *see* a compiler stage rather than generate project
code, use `--emit` instead of a target — see
[Inspecting and Debugging Models](../simulation/inspect.md).
