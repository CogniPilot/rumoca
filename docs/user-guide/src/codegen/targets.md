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
| `rust-ode` | solve | compiled | Checked explicit-ODE derivative kernel |
| `cuda-ode` | solve | compiled | Batched CUDA explicit-ODE derivative kernel |
| `wgsl-ode` | solve | JIT | Experimental WebGPU explicit-ODE kernels for browser execution |
| `mlir` | solve | source | Inspectible MLIR solve-kernel source with affine tensor loops |
| `dae-modelica` | dae | source-transform | Modelica representation of the checked DAE |
| `fmi2` / `fmi3` | fmi | standards container | Source-code Model Exchange and Co-Simulation FMUs |
| `fmi-ls-wasm` | fmi | compiled component | Experimental FMI-LS WebAssembly component crate |
| `galec` | algorithm-code | eFMI | Algorithm Code eFMU container |

Targets without a complete checked artifact and executable or independent
validation evidence are intentionally absent. Rumoca does not expose aliases
for removed target names or route those names through a weaker IR.
`flat-modelica` and `base-modelica` are therefore not registered: until Flat
construction retains every equation body exactly, use `--emit flat-json`.

The deleted `c-ode`, `embedded-c-galec`, and `galec-production` spellings have
no registry entries or tailored compatibility behavior; like any unknown
target, they do not resolve. For general C today, use the `fmi3` target
(FMI 3.0 ME+CS): Model Exchange serves host-owned integration and
Co-Simulation serves the built-in solver. GALEC never emits C. The eventual
deployable eFMI product is the `efmu` target: one `SolveAlgorithmProduct`
retains its checked `AlgorithmCodePackage` and correlated `SolveAlgorithmBlock`,
and each output file borrows the appropriate view. Production Code C renders
only from the Solve block.

The `rumoca targets` table also reports a readiness level (0 = experimental
… 2 = validated) and per-feature support columns (scalarization, tensor
features such as matmul/linear solve/elementwise/stencil kernels, events, AD,
…) for each target. Treat the table — not this page — as the current source of
truth.

## Rendering a Target

```bash
rumoca compile examples/models/SympyDecay.mo \
  --model SympyDecay \
  --target fmi3 \
  --output /tmp/decay_fmi3
```

`--output` may be a file or directory depending on what the target renders.

## Codegen Scenarios

Like simulations, generation jobs worth repeating belong in a `rumoca-scenario.toml`
with `task = "codegen"`. Runnable examples live under `examples/codegen/`
and write into `examples/codegen/gen/` (git-ignored):

- `examples/codegen/rumoca-scenario.ball_jax_ode.toml` — checked ODE RHS JAX target
- `examples/codegen/rumoca-scenario.sympy_decay_fmi3.toml` — FMI 3.0 ME+CS export
- `examples/codegen/rumoca-scenario.sympy_decay_checked_dae_report.toml` —
  custom checked-DAE report target
- `examples/codegen/rumoca-scenario.sympy_decay_custom_checked_variables.toml`
  — raw checked-DAE Jinja template

## IR Dumps vs Targets

If what you want is to *see* a compiler stage rather than generate project
code, use `--emit` instead of a target — see
[Inspecting and Debugging Models](../simulation/inspect.md).
