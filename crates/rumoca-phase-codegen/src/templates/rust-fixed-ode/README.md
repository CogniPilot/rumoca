# `rust-fixed-ode`

## Use case

Use this target for a no-allocation derivative call in a Rust control loop or
other fixed-shape CPU hot path. Compile-time array sizes make the storage
contract visible to embedded integration code and static analysis.

## Contract

- Readiness 2: CI compiles and numerically executes the kernel while proving the
  derivative call performs no heap allocation.
- Input: checked Solve IR with statically known scalar extents.
- Algebraic variables: when every algebraic slot has an issued exact
  assignment (`exact_algebraic_assignments`), the function assigns them
  in-function (into a stack copy of `State`), in checked order, before the
  derivative rows. Only the state slots of the input are read. Coupled or
  implicit algebraic blocks, and tensor-affine assignments, are still
  rejected (`residual_equations`).
- `refresh_algebraics(time, y, p)` applies the same exact assignments to the
  host's `State`, so a host reads algebraic variables and outputs without
  paying for them on every derivative evaluation.
- `Y_SLOTS` and `P_SLOTS` name the base slot of every variable, parameter, and
  input; `slot(table, name)` looks one up.
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
- `backend_template_runtime_regression::rust_fixed_ode_refreshes_algebraics_into_named_slots`
  reads a refreshed algebraic slot by name.
- `rust_fixed_ode_builtin_target_accepts_scalarized_matmul` checks tensor
  fallback without dynamic allocation.
- `rust_fixed_ode_builtin_target_rejects_linsolve_before_writing_source` is
  the focused negative control.

## Example

```sh
rumoca compile Decay.mo --model Decay --target rust-fixed-ode --output generated
rustc --crate-type lib generated/Decay_fixed_ode.rs
```
