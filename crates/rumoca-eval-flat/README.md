# rumoca-eval-flat

Flat IR evaluators for Rumoca (constant and runtime).

## Role in Rumoca
Provides expression evaluation over `rumoca-ir-flat` for both:
- compile-time constant evaluation used by compiler phases, and
- runtime evaluation used by simulation backends.

## Public Surface
- Constant-eval APIs: `EvalContext`, `Value`, `EvalError`, `EvalLimits`,
  `eval_expr`, `eval_expr_with_span`, `eval_function`,
  `try_eval_integer`, `try_eval_real`, `try_eval_bool`.
- Runtime-eval APIs: `eval::*`, `statement::*`, `SimFloat`, `Dual`, `VarEnv`.

## Inputs
- Flat expressions, variable environments, and metadata (`dims`, function table).
- Optional function bodies with constant arguments for compile-time folding.

## Outputs
- Constant-eval values/errors and runtime scalar/array expression results.

## Design Constraints
- Keep expression semantics centralized for flat IR.
- Preserve deterministic constant-eval behavior.
- Keep solver orchestration and backend policy out of this crate.
