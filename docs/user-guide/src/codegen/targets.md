# Targets and Templates

Rumoca can render a compiled model into other languages and ecosystems:
symbolic math packages, compiled simulation kernels, FMUs, or Modelica
source at any pipeline stage. Code generation is *target-directory based*: a
target is a `target.toml` manifest plus Jinja templates, and each target
declares which compiler IR stage it consumes.

## Listing Targets

```bash
rumoca targets
```

Built-in targets include:

| Target | IR | Mode | Output |
|---|---|---|---|
| `sympy` | dae | symbolic | SymPy model classes |
| `jax` | dae | symbolic | JAX functions |
| `casadi-sx` / `casadi-mx` | dae | symbolic | CasADi expressions |
| `julia-mtk` | dae | symbolic | ModelingToolkit.jl |
| `symforce` | dae | symbolic | SymForce, with native AD support |
| `onnx` | dae | symbolic | ONNX graph |
| `rust-solve` / `c-solve` / `embedded-c` | solve | compiled | Self-contained simulation kernels |
| `cuda-c` / `cuda-nvrtc-solve-jit` | solve | compiled/JIT | GPU kernels |
| `wgsl-solve` | solve | compiled | Experimental WebGPU kernels for browser runs |
| `cranelift-solve-jit` / `mlir` | solve | JIT/compiled | In-process execution backends |
| `fmi2` / `fmi3` | solve | packaged | FMU export |
| `modelica` / `flat-modelica` / `dae-modelica` / `base-modelica` | ast/flat/dae | source-transform | Modelica source at each stage |

The `rumoca targets` table also reports a readiness level (0 = experimental
… 2 = validated) and per-feature support columns (scalarization, matmul,
linear solve, affine stencils, events, AD, …) for each target. Treat the table
— not this page — as the current source of truth.

Regular `for`-equation domains in array/PDE-style models can lower to
source-proven affine stencils. Native stencil-aware targets, currently the
experimental `wgsl-solve` path used by browser GPU runs, can emit one
parametric kernel for the repeated grid operation. Other Solve targets still
receive the scalar fallback, so stencil support changes performance and code
shape rather than model semantics.

## Rendering a Target

```bash
rumoca compile examples/models/SympyDecay.mo \
  --model SympyDecay \
  --target sympy \
  --output /tmp/sympy_decay
```

`--output` may be a file or directory depending on what the target renders.

## Codegen Scenarios

Like simulations, generation jobs worth repeating belong in a `rumoca-scenario.toml`
with `task = "codegen"`. Runnable examples live under `examples/codegen/`
and write into `examples/codegen/gen/` (git-ignored):

- `examples/codegen/rumoca-scenario.ball_jax.toml` — built-in JAX target
- `examples/codegen/rumoca-scenario.sympy_decay_sympy.toml` — built-in SymPy target
- `examples/codegen/rumoca-scenario.sympy_decay_standalone_web.toml` — custom web target
- `examples/codegen/rumoca-scenario.sympy_decay_custom_casadi.toml` — raw Jinja template

## IR Dumps vs Targets

If what you want is to *see* a compiler stage rather than generate project
code, use `--emit` instead of a target — see
[Inspecting and Debugging Models](../simulation/inspect.md).
