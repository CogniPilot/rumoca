# `rust-ode`

## Use case

Use this target to embed an explicit Rumoca derivative kernel in an independent
Rust application that owns its solver and allocation policy. It is suitable for
prototyping custom hosts and for cross-language differential tests against the C
kernel.

## Contract

- Readiness 2: CI compiles and numerically executes the generated Rust module.
- Input: checked Solve IR.
- Output: one standalone Rust source module using slice-based state, parameter,
  and output storage.
- Scalar fallback is a generated view of compact tensor nodes, not a second IR.
- Linear-solve helpers report `SolveError` rather than fabricating values.

## Unsupported

The module is not a simulator or FMI implementation. It rejects implicit
residual systems, events, initialization, callbacks, and unsupported operations
at render time.

## Verification

- `backend_template_runtime_regression::rust_ode_checked_target_compiles_and_executes`
  compiles and runs the generated module.
- `scalar_plan_template_tests` verifies operation semantics and failure ABI.
- `explicit_rhs_targets_reject_implicit_algebraic_models` proves fail-closed
  residual handling.

## Example

```sh
rumoca compile Decay.mo --model Decay --target rust-ode --output generated
rustc --crate-type lib generated/Decay_ode.rs
```
