use super::*;

/// Try to evaluate a flat expression to a boolean value with context.
pub fn try_eval_flat_expr_boolean(
    expr: &rumoca_core::Expression,
    known_ints: &FxHashMap<String, i64>,
    known_bools: &FxHashMap<String, bool>,
    known_enums: &FxHashMap<String, String>,
) -> Result<Option<bool>, crate::constant::EvalError> {
    let known_reals = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let functions = FxHashMap::default();
    let param_ctx = ParamEvalContext::new_structural(
        known_ints,
        &known_reals,
        known_bools,
        known_enums,
        &array_dims,
        &functions,
        None,
    );
    ParamEvaluator::new(&param_ctx)?.eval_boolean(expr, None)
}
