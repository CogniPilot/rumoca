//! Structural integer evaluation against a flat model.
//!
//! Array dimensions, for-loop ranges and array subscripts must be known at
//! compile time (MLS §4.5, §10.1). This module is the single evaluator that
//! resolves such an expression to an `i64` by following parameter/constant
//! bindings recorded in `flat::Model`.

use rumoca_ir_flat as flat;

/// Maximum binding-chain depth followed while resolving a structural value.
const MAX_EVAL_DEPTH: u8 = 8;

/// Recursively evaluate a flat expression to an integer value.
///
/// Handles literals, parameter references (via binding), binary arithmetic
/// (`+`, `-`, `*`, `/`), and `size(array, dim)` calls. Returns `None` when the
/// expression is not structurally constant.
pub fn try_eval_flat_expr_i64(
    expr: &rumoca_core::Expression,
    flat: &flat::Model,
    depth: u8,
) -> Option<i64> {
    if depth > MAX_EVAL_DEPTH {
        return None;
    }
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(n),
            ..
        } => Some(*n),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(f),
            ..
        } => {
            let n = *f as i64;
            if (n as f64 - *f).abs() < 0.001 {
                Some(n)
            } else {
                None
            }
        }
        rumoca_core::Expression::VarRef { name, .. } => {
            let var = flat.variables.get(name.var_name())?;
            let binding = var.binding.as_ref()?;
            try_eval_flat_expr_i64(binding, flat, depth + 1)
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            let l = try_eval_flat_expr_i64(lhs, flat, depth + 1)?;
            let r = try_eval_flat_expr_i64(rhs, flat, depth + 1)?;
            eval_binary_op_i64(op, l, r)
        }
        rumoca_core::Expression::BuiltinCall { function, args, .. }
            if matches!(function, rumoca_core::BuiltinFunction::Size) && args.len() == 2 =>
        {
            eval_size_call_i64(&args[0], &args[1], flat, depth)
        }
        _ => None,
    }
}

/// Evaluate a binary arithmetic operation on two integer values.
pub fn eval_binary_op_i64(op: &rumoca_core::OpBinary, l: i64, r: i64) -> Option<i64> {
    match op {
        rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem => Some(l + r),
        rumoca_core::OpBinary::Sub | rumoca_core::OpBinary::SubElem => Some(l - r),
        rumoca_core::OpBinary::Mul | rumoca_core::OpBinary::MulElem => Some(l * r),
        rumoca_core::OpBinary::Div | rumoca_core::OpBinary::DivElem => {
            if r != 0 {
                Some(l / r)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Evaluate `size(array, dim)` from literal array shape or variable dimensions.
pub fn eval_size_call_i64(
    array_arg: &rumoca_core::Expression,
    dim_arg: &rumoca_core::Expression,
    flat: &flat::Model,
    depth: u8,
) -> Option<i64> {
    let dim = try_eval_flat_expr_i64(dim_arg, flat, depth + 1)? as usize;
    if let Some(size) = literal_array_dim_size(array_arg, dim) {
        return Some(size);
    }
    if let rumoca_core::Expression::VarRef { name, .. } = array_arg {
        let var = flat.variables.get(name.var_name())?;
        let idx = dim.checked_sub(1)?;
        var.dims.get(idx).copied()
    } else {
        None
    }
}

/// Size of dimension `dim` (1-based) of a literal array expression.
pub fn literal_array_dim_size(expr: &rumoca_core::Expression, dim: usize) -> Option<i64> {
    let rumoca_core::Expression::Array { elements, .. } = expr else {
        return None;
    };
    match dim {
        1 => Some(elements.len() as i64),
        2 => elements.first().and_then(|first| match first {
            rumoca_core::Expression::Array { elements, .. } => Some(elements.len() as i64),
            _ => None,
        }),
        _ => None,
    }
}

/// Structural integer value of a flat variable that is a `constant` or
/// `parameter` with a compile-time-known binding.
///
/// Returns `None` for continuous/discrete variables, for parameters without a
/// binding, and for bindings that are not structurally constant. Per MLS §4.5
/// only such values may legally appear in a subscript that has to be folded at
/// compile time.
pub fn structural_integer_value(var: &flat::Variable, flat: &flat::Model) -> Option<i64> {
    if !matches!(
        var.variability,
        rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
    ) {
        return None;
    }
    let binding = var.binding.as_ref()?;
    try_eval_flat_expr_i64(binding, flat, 0)
}
