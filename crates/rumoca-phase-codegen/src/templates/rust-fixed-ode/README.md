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
- `Simulator` advances the model by fixed communication steps with events:
  `Simulator::new(method, location, settings, t0, y, p)`, `initialize()` (the initial
  event), then `do_step(h)`, which lands exactly on `t + h` and returns the
  number of events it applied. Inputs and tunable parameters are written
  through `p_mut()` between steps.
- `Method` is `Euler`, `Heun`, or `Rk4`. A step is clipped to land on every
  time event (static instants and `sample` clock ticks).
- `EventLocation` decides where a state event found at a step end is applied:
  `StepEnd` (no extra evaluations, up to one step late), `Secant` (at most
  `settings.secant_trials` refinements on the step's cubic Hermite interpolant), or
  `Bracketed` (refined to the roundoff of the step). Located events land past
  the crossing, then the rest of the step is integrated.
- The model's facts (roots, relation memories, pre lanes, discrete rows, the
  issued event iteration schedule) are generated; the event algorithm is
  the hand-written `events.rs`, copied in unchanged. It applies the same
  event rules as the FMI C target, and finds where a state event crosses
  zero with a copy of `rumoca-ir-solve`'s `RootBracket` search.
- Output: one Rust module whose state, parameter, and derivative types are
  fixed-size arrays.
- `derivative_rhs_into` and `Simulator::do_step` perform no heap allocation.
- Tensor scalar views retain the checked row-major layout.

## Unsupported

Events cover everything the compiler's scalar event profile admits, as the
FMI C component executes it: relation memory and its mode search, pre lanes,
discrete and guarded rows, pulses, observation rows, clocks and static time
events, discrete-valued inputs, error- and warning-level assertions, and the
location plan's sliding modes. Discrete rows whose `pre` is held for a pass
(`Fixed`, as stick-slip state machines lower) run; rows that read `pre` at
the event entry in every pass (`EventEntry`) are refused. The profile itself refuses delays,
`terminate`, dynamic time events, structured updates, event transactions and
reads of `initial()` outside a checked relation region. Assertions raised
inside called functions, and user-function calls the compiler does not inline
into event rows (roots, `when` bodies, `reinit`, assertions), are refused here.

A `reinit` is copied into its state at the end of each event pass, so it
reads the state's left limit (MLS 8.3.6). After a static time event the model
reads `time` at the instant's right limit. Static instants and clock ticks are
fixed when the module is generated, from the parameters' values then; writing
a parameter that sets one does not move it.

This target is not currently `no_std`, and does not implement linear solves,
initialization residuals, residual equations, or callbacks. More than
`settings.max_events_per_step` state events in one step (Zeno behaviour) end in
`SimError::TooManyEvents`. A crossing that leaves and returns within one step
is not seen. Unsupported input is rejected before source is written.

The module needs Rust 1.86 or later. The event iteration keeps about twenty
copies of `State` and `Parameters` on the stack, about 160 bytes per slot of
`Y_LEN + P_LEN`; size the calling thread's stack for large models.

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
