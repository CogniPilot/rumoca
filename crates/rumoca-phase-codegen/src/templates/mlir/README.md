# `mlir`

## Use case

Use this target when building an ahead-of-time or JIT numerical backend through
MLIR/LLVM. It is the low-level tensor-aware path for CPU execution, accelerator
experiments, Jacobian products, and implicit residual kernels.

## Contract

- Readiness 1: CI parses and lowers the generated MLIR through the pinned toolchain.
- Input: checked Solve IR plus explicitly requested checked Solve artifacts.
- Output: MLIR containing derivative, residual, tensor map/stencil, matrix
  multiply, linear solve, and available Jacobian-vector functions.
- Affine tensor domains remain compact loops; scalar fallback is explicit.
- `SolveProblem` construction proves exact-once dense derivative coverage, so
  the backend emits only semantic stores and never repairs missing rows by
  pre-filling the derivative buffer.
- `rumoca-exec-mlir` owns toolchain invocation and execution.

## Unsupported

The target is not an FMI lifecycle or a Modelica semantic lowering pass.
Events, runtime callbacks, and unsupported dynamic control flow fail before
execution. Missing optional Solve artifacts cannot become empty functions.

## Verification

- `rumoca-exec-mlir/tests` parses, lowers, compiles, and executes CPU kernels,
  including integration, tensor matmul, and linear solve.
- `solve_template_context_tests` and the WGSL mixed-schedule tests cover function
  inventory, native/scalar output placement, exact-once coverage, and the
  absence of a backend derivative prefill.
- `mlir_verification_wiring` keeps the required external-toolchain lane live.

## Example

```sh
rumoca compile Plant.mo --model Plant --target mlir --output generated
mlir-opt generated/Plant.mlir
```
