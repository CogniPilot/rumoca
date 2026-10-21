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
- Events run the scalar event profile of the FMI C component (relations,
  `when`/`reinit`, discrete equations, `pre`, time events). The host keeps an
  `EventState` and calls `handle_events(&mut events, time, y, p)` once at the
  start time and after every accepted step, never inside a multistage step.
  State events are detected at step ends; time events at `TIME_EVENTS`
  instants. `root_conditions` exposes the raw roots for hosts that locate
  crossings themselves.
- `step(method, events, time, dt, y, p)` is the minimal host: one explicit
  Euler or RK4 step, shortened to land on the next time event, then
  `handle_events`.
- Output: one Rust module whose state, parameter, and derivative types are
  fixed-size arrays.
- `derivative_rhs_into` performs no heap allocation.
- Tensor scalar views retain the checked row-major layout.

## Unsupported

This target is not currently `no_std`, does not locate state events inside a
step, and does not implement linear solves, clocks, delays, assertions,
discrete-valued inputs, initialization, residual equations, or callbacks.
Unsupported input is rejected before source is written.

## Verification

- `backend_template_runtime_regression::rust_fixed_ode_checked_target_executes_without_heap_allocation`
  compiles and executes the fixed-array API while an allocation counter proves
  the derivative call performs no heap allocation.
- `backend_template_runtime_regression::rust_fixed_ode_refreshes_algebraics_into_named_slots`
  reads a refreshed algebraic slot by name.
- `backend_template_runtime_regression::rust_fixed_ode_events::*` run a
  `reinit` angle wrap, a bouncing ball, a time event, a timed `reinit` and an
  event-free model against `rumoca sim` values, and check that assertions are refused.
- `rust_fixed_ode_builtin_target_accepts_scalarized_matmul` checks tensor
  fallback without dynamic allocation.
- `rust_fixed_ode_builtin_target_rejects_linsolve_before_writing_source` is
  the focused negative control.

## Example

```sh
rumoca compile Decay.mo --model Decay --target rust-fixed-ode --output generated
rustc --crate-type lib generated/Decay_fixed_ode.rs
```
