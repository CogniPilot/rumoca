use super::*;

/// Try to evaluate a flat expression to a boolean value with context.
pub fn try_eval_flat_expr_boolean(
    expr: &rumoca_core::Expression,
    known_ints: &FxHashMap<String, i64>,
    known_bools: &FxHashMap<String, bool>,
    known_enums: &FxHashMap<String, String>,
) -> Option<bool> {
    let param_ctx = ParamEvalContext {
        known_ints,
        known_reals: &FxHashMap::default(),
        known_bools,
        known_enums,
        array_dims: &FxHashMap::default(),
        functions: &FxHashMap::default(),
        var_context: None,
    };
    ParamEvaluator::new(&param_ctx).eval_boolean(expr, None)
}
