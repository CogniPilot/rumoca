use super::*;

/// Context for boolean expression evaluation.
struct BoolEvalContext<'a> {
    known_ints: &'a FxHashMap<String, i64>,
    known_bools: &'a FxHashMap<String, bool>,
    known_enums: &'a FxHashMap<String, String>,
}

/// Try to evaluate a flat expression to a boolean value with context.
pub fn try_eval_flat_expr_boolean(
    expr: &rumoca_core::Expression,
    known_ints: &FxHashMap<String, i64>,
    known_bools: &FxHashMap<String, bool>,
    known_enums: &FxHashMap<String, String>,
) -> Option<bool> {
    let ctx = BoolEvalContext {
        known_ints,
        known_bools,
        known_enums,
    };
    eval_bool_inner(expr, &ctx)
}

/// Inner boolean evaluation.
fn eval_bool_inner(expr: &rumoca_core::Expression, ctx: &BoolEvalContext) -> Option<bool> {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Boolean(b),
            ..
        } => Some(*b),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => ctx.known_bools.get(&name.to_string()).copied(),
        rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Not,
            rhs,
            ..
        } => eval_bool_inner(rhs, ctx).map(|v| !v),
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => eval_bool_binary(op, lhs, rhs, ctx),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => eval_bool_if(branches, else_branch, ctx),
        _ => None,
    }
}

/// Evaluate binary boolean operations.
fn eval_bool_binary(
    op: &rumoca_core::OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    ctx: &BoolEvalContext,
) -> Option<bool> {
    match op {
        rumoca_core::OpBinary::And => {
            Some(eval_bool_inner(lhs, ctx)? && eval_bool_inner(rhs, ctx)?)
        }
        rumoca_core::OpBinary::Or => Some(eval_bool_inner(lhs, ctx)? || eval_bool_inner(rhs, ctx)?),
        rumoca_core::OpBinary::Eq => eval_equality(lhs, rhs, ctx, true),
        rumoca_core::OpBinary::Neq => eval_equality(lhs, rhs, ctx, false),
        rumoca_core::OpBinary::Lt => eval_int_compare(lhs, rhs, ctx.known_ints, |l, r| l < r),
        rumoca_core::OpBinary::Le => eval_int_compare(lhs, rhs, ctx.known_ints, |l, r| l <= r),
        rumoca_core::OpBinary::Gt => eval_int_compare(lhs, rhs, ctx.known_ints, |l, r| l > r),
        rumoca_core::OpBinary::Ge => eval_int_compare(lhs, rhs, ctx.known_ints, |l, r| l >= r),
        _ => None,
    }
}

/// Evaluate equality/inequality comparisons across types.
fn eval_equality(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    ctx: &BoolEvalContext,
    eq: bool,
) -> Option<bool> {
    if let (Some(l), Some(r)) = (
        try_eval_flat_expr_integer_with_dims(lhs, ctx.known_ints, &FxHashMap::default()),
        try_eval_flat_expr_integer_with_dims(rhs, ctx.known_ints, &FxHashMap::default()),
    ) {
        return Some(if eq { l == r } else { l != r });
    }
    if let (Some(l), Some(r)) = (eval_bool_inner(lhs, ctx), eval_bool_inner(rhs, ctx)) {
        return Some(if eq { l == r } else { l != r });
    }
    if let (Some(l), Some(r)) = (
        resolve_enum_value(lhs, ctx.known_enums),
        resolve_enum_value(rhs, ctx.known_enums),
    ) {
        let l_norm = canonicalize_enum_literal(&l, ctx.known_enums);
        let r_norm = canonicalize_enum_literal(&r, ctx.known_enums);
        let equal = rumoca_core::enum_values_equal(&l_norm, &r_norm);
        return Some(if eq { equal } else { !equal });
    }
    None
}

/// Evaluate integer comparisons.
fn eval_int_compare(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    known_ints: &FxHashMap<String, i64>,
    cmp: fn(i64, i64) -> bool,
) -> Option<bool> {
    let l = try_eval_flat_expr_integer_with_dims(lhs, known_ints, &FxHashMap::default())?;
    let r = try_eval_flat_expr_integer_with_dims(rhs, known_ints, &FxHashMap::default())?;
    Some(cmp(l, r))
}

/// Evaluate if-expression branches.
fn eval_bool_if(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    ctx: &BoolEvalContext,
) -> Option<bool> {
    for (cond, then_expr) in branches {
        match eval_bool_inner(cond, ctx) {
            Some(true) => return eval_bool_inner(then_expr, ctx),
            Some(false) => continue,
            None => return None,
        }
    }
    eval_bool_inner(else_branch, ctx)
}
