//! Constant expression evaluation over DAE expressions.

use rumoca_ir_dae as dae;

/// Evaluate a DAE expression with an empty environment.
pub fn eval_const_expr(expr: &dae::Expression) -> f64 {
    crate::runtime::eval::eval_const_expr_dae(expr)
}
