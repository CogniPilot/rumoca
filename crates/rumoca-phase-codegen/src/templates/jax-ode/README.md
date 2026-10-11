# `jax-ode`

## Use case

Use this target for differentiable simulation experiments, batched parameter
studies, learned-model coupling, and control optimization in JAX. It provides a
pure explicit RHS suitable for `jax.jit` and JAX differentiation. Checked
function conditionals run only their selected branch for scalar evaluation.
JAX transforms `lax.cond` to eager selection under `vmap` over predicates,
which can make reverse derivatives non-finite when an inactive branch has an
undefined derivative. Use `jax.lax.map` for those branch-sensitive batched
computations; ordinary branch-free RHS functions remain suitable for `vmap`.

## Contract

- Readiness 1: CI imports, JITs, evaluates, and differentiates the generated module.
- Input: checked Solve IR.
- Algebraic variables: when every algebraic slot has an issued exact
  assignment (`exact_algebraic_assignments`), the function assigns them
  in-function, in checked order, before the derivative rows. Only the state
  slots of the input are read. Coupled or implicit algebraic blocks, and
  tensor-affine assignments, are still rejected (`residual_equations`).
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
- `fmi2_and_fmi3_consume_an_exact_isolable_algebraic_schedule` renders the exact
  algebraic refresh for every scalar ODE target and executes the c-ode one.

## Example

```sh
rumoca compile Plant.mo --model Plant --target jax-ode --output generated
python generated/Plant_jax_ode.py
```
