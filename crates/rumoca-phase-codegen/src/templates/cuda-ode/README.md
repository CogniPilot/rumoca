# `cuda-ode`

## Use case

Use this target to evaluate one explicit ODE RHS across many independent states
on an NVIDIA GPU—for Monte Carlo analysis, parameter sweeps, ensemble filters,
or batched control-design workloads. The host owns allocation, launch policy,
and integration.

## Contract

- Input: checked Solve IR.
- Output: deterministic CUDA C defining one batched derivative kernel.
- Every CUDA thread evaluates one checked `(y, p)` lane with explicit strides.
- Tensor nodes remain in Solve IR; this readiness level uses the evaluator's
  checked scalar view until native CUDA tensor kernels earn separate evidence.

## Unsupported

This is not an NVRTC executor, an FMU, or a full simulator. Linear solves,
implicit residual systems, events, initialization, callbacks, and unsupported
operations fail before source is written. No GPU availability is inferred from
successful source generation.

## Verification

- `cuda_ode_generated_kernel_compiles_and_executes_cpu_emulation` checks batched
  indexing and numerical outputs without requiring a GPU.
- `cuda_ode_generated_kernel_compiles_with_required_nvcc` is a required CI source
  compilation with the pinned Nix CUDA compiler.
- `cuda_ode_builtin_target_rejects_linsolve_before_writing_source` and
  `explicit_rhs_targets_reject_implicit_algebraic_models` are negative controls.

## Example

```sh
rumoca compile Ensemble.mo --model Ensemble --target cuda-ode --output generated
nvcc -c generated/Ensemble_ode.cu
```
