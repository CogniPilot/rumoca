# `casadi-ode`

## Use case

Use this target to bring an explicit Rumoca ODE into CasADi for nonlinear
optimization, model-predictive control, sensitivity analysis, or symbolic
linearization while retaining Rumoca's checked variable layout.

## Contract

- Readiness 1: CI imports, evaluates, and differentiates the generated module.
- Input: checked Solve IR.
- Output: a Python module exporting a CasADi `rhs(t, x, u, p)` expression.
- Indexed reads and scalar tensor views remain CasADi operations so automatic
  differentiation sees the actual computation.
- The host owns integration and optimization policy.

## Unsupported

Implicit residual equations, runtime events, initialization, callbacks, and
unsupported Solve operations fail rendering. The target does not emit an FMU
or choose an optimizer.

## Verification

- `casadi_ode_target_imports_evaluates_and_differentiates` imports the module,
  evaluates the RHS, and checks a CasADi Jacobian.
- `indexed_python_targets_keep_indexing_inside_symbolic_array_dialects` checks
  symbolic indexed access.
- `explicit_rhs_targets_reject_implicit_algebraic_models` is the negative gate.

## Example

```sh
rumoca compile Plant.mo --model Plant --target casadi-ode --output generated
python generated/Plant_casadi_ode.py
```
