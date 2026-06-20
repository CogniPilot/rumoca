use super::*;

/// Evaluate a scalar expression, returning an error for missing bindings or
/// expression forms that the DAE evaluator cannot safely reduce to a scalar.
pub fn eval_expr<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    let value = match expr {
        rumoca_core::Expression::Literal { value: lit, .. } => try_eval_literal::<T>(lit),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => try_eval_var_ref::<T>(name.var_name(), subscripts, env),
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            try_eval_binary::<T>(op, lhs, rhs, env)
        }
        rumoca_core::Expression::Unary { op, rhs, .. } => try_eval_unary::<T>(op, rhs, env),
        rumoca_core::Expression::BuiltinCall { function, args, .. } => {
            eval_builtin::<T>(*function, args, env)
        }
        rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } => {
            validate_expr(expr, env)?;
            eval_function_call::<T>(name.var_name(), args, *is_constructor, env)
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => try_eval_if::<T>(branches, else_branch, env),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => try_eval_index_expr::<T>(base, subscripts, env),
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            try_eval_field_access::<T>(base, field, env)
        }
        rumoca_core::Expression::Empty { .. } => {
            Err(EvalError::UnsupportedExpression { kind: "empty" })
        }
        rumoca_core::Expression::Array { .. } => {
            Err(EvalError::UnsupportedExpression { kind: "array" })
        }
        rumoca_core::Expression::Range { .. } => {
            Err(EvalError::UnsupportedExpression { kind: "range" })
        }
        rumoca_core::Expression::Tuple { .. } => {
            Err(EvalError::UnsupportedExpression { kind: "tuple" })
        }
        rumoca_core::Expression::ArrayComprehension { .. } => {
            Err(EvalError::UnsupportedExpression {
                kind: "array comprehension",
            })
        }
    };
    value.map_err(|err| match expr.span() {
        Some(span) => err.with_span_if_missing(span),
        None => err,
    })
}

fn try_eval_literal<T: SimFloat>(lit: &rumoca_core::Literal) -> Result<T, EvalError> {
    validate_literal(lit)?;
    Ok(eval_literal::<T>(lit))
}

fn try_eval_if<T: SimFloat>(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    for (condition, value) in branches {
        if try_eval_condition_truth(condition, env)? {
            return eval_expr::<T>(value, env);
        }
    }
    eval_expr::<T>(else_branch, env)
}

fn try_eval_binary<T: SimFloat>(
    op: &rumoca_core::OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    if matches!(
        op,
        rumoca_core::OpBinary::Empty | rumoca_core::OpBinary::Assign
    ) {
        return Err(EvalError::UnsupportedExpression {
            kind: "placeholder binary operator",
        });
    }
    let may_be_array_product = matches!(op, rumoca_core::OpBinary::Mul)
        && (expression_is_array_like(lhs, env)? || expression_is_array_like(rhs, env)?);
    if may_be_array_product && let Some(dot) = try_eval_vector_dot_product(lhs, rhs, env)? {
        return Ok(dot);
    }
    if may_be_array_product
        && let Some(values) = eval_binary_array_values(op, lhs, rhs, env)?
        && let Some(first) = values.first().copied()
    {
        return Ok(first);
    }

    let l = eval_expr::<T>(lhs, env)?;
    let r = eval_expr::<T>(rhs, env)?;
    Ok(match op {
        rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem => l + r,
        rumoca_core::OpBinary::Sub | rumoca_core::OpBinary::SubElem => l - r,
        rumoca_core::OpBinary::Mul | rumoca_core::OpBinary::MulElem => l * r,
        rumoca_core::OpBinary::Div | rumoca_core::OpBinary::DivElem => {
            if r.real() == 0.0 {
                if l.real() == 0.0 {
                    T::zero()
                } else {
                    T::infinity()
                }
            } else {
                l / r
            }
        }
        rumoca_core::OpBinary::Exp | rumoca_core::OpBinary::ExpElem => l.powf(r),
        rumoca_core::OpBinary::And => T::from_bool(l.to_bool() && r.to_bool()),
        rumoca_core::OpBinary::Or => T::from_bool(l.to_bool() || r.to_bool()),
        rumoca_core::OpBinary::Lt => T::from_bool(l.lt(r)),
        rumoca_core::OpBinary::Le => T::from_bool(l.le(r)),
        rumoca_core::OpBinary::Gt => T::from_bool(l.gt(r)),
        rumoca_core::OpBinary::Ge => T::from_bool(l.ge(r)),
        rumoca_core::OpBinary::Eq => T::from_bool(l.eq_approx(r)),
        rumoca_core::OpBinary::Neq => T::from_bool(!l.eq_approx(r)),
        rumoca_core::OpBinary::Empty | rumoca_core::OpBinary::Assign => unreachable!(),
    })
}

fn try_eval_unary<T: SimFloat>(
    op: &rumoca_core::OpUnary,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    let r = eval_expr::<T>(rhs, env)?;
    Ok(match op {
        rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => -r,
        rumoca_core::OpUnary::Plus | rumoca_core::OpUnary::DotPlus => r,
        rumoca_core::OpUnary::Not => T::from_bool(!r.to_bool()),
        rumoca_core::OpUnary::Empty => r,
    })
}
