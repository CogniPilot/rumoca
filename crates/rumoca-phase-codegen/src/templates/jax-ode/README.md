# `jax-ode`

## Use case

Use this target for differentiable simulation experiments, batched parameter
studies, learned-model coupling, and control optimization in JAX. It provides a
pure explicit RHS suitable for `jax.jit`, `vmap`, and JAX differentiation.

## Contract

- Readiness 1: CI imports, JITs, evaluates, and differentiates the generated module.
- Input: checked Solve IR.
- Output: a Python module exporting `rhs(t, x, u, p)` with JAX array operations.
- The function is side-effect free and preserves checked storage ordering.
- Scalar tensor views are emitted as JAX expressions rather than precomputed
  host values.

## Unsupported

The target does not own an integrator. Implicit residual systems, events,
initialization, callbacks, and unsupported operations fail before output.

## Verification

- `jax_ode_target_imports_jits_evaluates_and_differentiates` covers import,
  JIT execution, numerical output, and forward- and reverse-mode Jacobians.
- `indexed_python_targets_keep_indexing_inside_symbolic_array_dialects` covers
  indexed parameter access.
- `explicit_rhs_targets_reject_implicit_algebraic_models` is the negative gate.

## Example

```sh
rumoca compile Plant.mo --model Plant --target jax-ode --output generated
python generated/Plant_jax_ode.py
```
