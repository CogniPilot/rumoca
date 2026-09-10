# `rust-fixed-ode`

## Use case

Use this target for a no-allocation derivative call in a Rust control loop or
other fixed-shape CPU hot path. Compile-time array sizes make the storage
contract visible to embedded integration code and static analysis.

## Contract

- Readiness 2: CI compiles and numerically executes the kernel while proving the
  derivative call performs no heap allocation.
- Input: checked Solve IR with statically known scalar extents.
- Output: one Rust module whose state, parameter, and derivative types are
  fixed-size arrays.
- `derivative_rhs_into` performs no heap allocation.
- Tensor scalar views retain the checked row-major layout.

## Unsupported

This target is not currently `no_std`, does not own time integration, and does
not implement linear solves, events, initialization, residual equations, or
callbacks. Unsupported input is rejected before source is written.

## Verification

- `backend_template_runtime_regression::rust_fixed_ode_checked_target_executes_without_heap_allocation`
  compiles and executes the fixed-array API while an allocation counter proves
  the derivative call performs no heap allocation.
- `rust_fixed_ode_builtin_target_accepts_scalarized_matmul` checks tensor
  fallback without dynamic allocation.
- `rust_fixed_ode_builtin_target_rejects_linsolve_before_writing_source` is
  the focused negative control.

## Example

```sh
rumoca compile Decay.mo --model Decay --target rust-fixed-ode --output generated
rustc --crate-type lib generated/Decay_fixed_ode.rs
```
