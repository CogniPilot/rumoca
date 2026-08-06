# `c-ode`

## Use case

Use this target when a C or C++ host already owns integration and needs a small,
portable explicit-ODE derivative kernel. Typical consumers are custom solver
experiments, foreign-function prototypes, and numerical differential tests.

## Contract

- Readiness 2: the emitted C is compiled and numerically executed in CI.
- Input: checked Solve IR.
- Output: a C header and C11 implementation of `derivative_rhs(t, y, p, out)`.
- The ABI returns an error for numerical helper failures and writes outputs in
  the checked Solve layout.
- Tensor nodes are scalar views derived by the evaluator; Solve IR remains
  tensor-native.

## Unsupported

This is not an FMI component, an integrator, or an embedded application.
Events, initialization, residual systems, host callbacks, and unsupported
tensor operations are rejected before files are written.

## Verification

- `backend_template_runtime_regression::c_ode_checked_target_compiles_and_executes`
  compiles and numerically executes the generated C.
- `scalar_plan_template_tests` checks indexed access, sparse outputs, and
  explicit linear-solve failure behavior.
- `explicit_rhs_targets_reject_implicit_algebraic_models` is the negative gate.

## Example

```sh
rumoca compile Decay.mo --model Decay --target c-ode --output generated
cc -O2 -Wall -Werror -c generated/Decay_ode.c
```
