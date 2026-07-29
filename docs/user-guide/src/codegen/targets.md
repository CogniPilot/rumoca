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
| `casadi-solve` | solve | symbolic | Differentiable CasADi explicit RHS |
| `jax-solve` | solve | symbolic | JIT/AD-capable JAX explicit RHS |
| `rust-fixed-solve` | solve | compiled | Fixed-size Rust derivative kernel with `State`, `Parameters`, `Derivative`, and `derivative_rhs_into` |
| `rust-solve` / `c-solve` | solve | compiled | Checked derivative kernels |
| `cuda-c` / `cuda-nvrtc-solve-jit` | solve | compiled/JIT | GPU kernels |
| `wgsl-solve` | solve | compiled | Experimental WebGPU kernels for browser runs |
| `cranelift-solve-jit` / `mlir` | solve | JIT/compiled | In-process execution backends |
| `modelica` / `flat-modelica` / `dae-modelica` / `base-modelica` | ast/flat/dae | source-transform | Modelica source at each stage |
| `galec` / `galec-production` | dae | eFMI | Algorithm Code and Production Code eFMU containers |
| `embedded-c-galec` | dae | compiled | GALEC-derived embedded C without an eFMI container |

FMI 2/3 and the former DAE-shaped SymPy, SymForce, ONNX, Julia, JAX, and
CasADi targets are being rebuilt against checked DAE/Solve contracts. They are
intentionally absent until their end-to-end capability is restored; Rumoca
does not expose aliases or silently route them through a weaker schema.

The `rumoca targets` table also reports a readiness level (0 = experimental
… 2 = validated) and per-feature support columns (scalarization, tensor
features such as matmul/linear solve/elementwise/stencil kernels, events, AD,
…) for each target. Treat the table — not this page — as the current source of
truth.

## Rendering a Target

```bash
rumoca compile examples/models/SympyDecay.mo \
  --model SympyDecay \
  --target c-solve \
  --output /tmp/decay_c_solve
```

`--output` may be a file or directory depending on what the target renders.

## Codegen Scenarios

Like simulations, generation jobs worth repeating belong in a `rumoca-scenario.toml`
with `task = "codegen"`. Runnable examples live under `examples/codegen/`
and write into `examples/codegen/gen/` (git-ignored):

- `examples/codegen/rumoca-scenario.ball_jax_solve.toml` — checked Solve JAX target
- `examples/codegen/rumoca-scenario.sympy_decay_c_solve.toml` — checked Solve C target
- `examples/codegen/rumoca-scenario.sympy_decay_checked_dae_report.toml` —
  custom checked-DAE report target
- `examples/codegen/rumoca-scenario.sympy_decay_custom_checked_variables.toml`
  — raw checked-DAE Jinja template

## IR Dumps vs Targets

If what you want is to *see* a compiler stage rather than generate project
code, use `--emit` instead of a target — see
[Inspecting and Debugging Models](../simulation/inspect.md).
