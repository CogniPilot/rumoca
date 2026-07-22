//! Scalar evaluation of `re`/`im` selections on Complex arithmetic.
//!
//! DAE variables are scalarized into their record fields, but parameter and
//! start expressions can retain a structured Complex expression until runtime.
//! Evaluate the selected component directly so no temporary Complex value is
//! introduced into the scalar solver environment.

use super::eval_expr_impl::{EvalError, try_eval_function_call_field};
use super::{SimFloat, VarEnv, eval_expr, try_eval_field_access_path};

type Expression = rumoca_core::Expression;

pub(super) fn try_eval_arithmetic_field<T: SimFloat>(
    base: &Expression,
    field: &str,
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    let Some((re, im)) = try_eval_complex_pair(base, env)? else {
        return Ok(None);
    };
    match field {
        "re" => Ok(Some(re)),
        "im" => Ok(Some(im)),
        _ => Ok(None),
    }
}

fn try_eval_complex_pair<T: SimFloat>(
    expr: &Expression,
    env: &VarEnv<T>,
) -> Result<Option<(T, T)>, EvalError> {
    match expr {
        Expression::VarRef { .. } => try_eval_bound_pair(expr, env),
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } => try_eval_call_pair(name, args, *is_constructor, env),
        Expression::Binary { op, lhs, rhs, .. } => try_eval_binary_pair(op, lhs, rhs, env),
        Expression::Unary { op, rhs, .. } => try_eval_unary_pair(op, rhs, env),
        _ => Ok(None),
    }
}

fn try_eval_bound_pair<T: SimFloat>(
    expr: &Expression,
    env: &VarEnv<T>,
) -> Result<Option<(T, T)>, EvalError> {
    let Some(path) = try_eval_field_access_path(expr, env)? else {
        return Ok(None);
    };
    Ok(env
        .get_optional(&format!("{path}.re"))
        .zip(env.get_optional(&format!("{path}.im"))))
}

fn try_eval_call_pair<T: SimFloat>(
    name: &rumoca_core::Reference,
    args: &[Expression],
    is_constructor: bool,
    env: &VarEnv<T>,
) -> Result<Option<(T, T)>, EvalError> {
    let re = try_eval_function_call_field(name, args, is_constructor, "re", env)?;
    let im = try_eval_function_call_field(name, args, is_constructor, "im", env)?;
    Ok(re.zip(im))
}

fn try_eval_binary_pair<T: SimFloat>(
    op: &rumoca_core::OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    env: &VarEnv<T>,
) -> Result<Option<(T, T)>, EvalError> {
    let lhs_pair = try_eval_complex_pair(lhs, env)?;
    let rhs_pair = try_eval_complex_pair(rhs, env)?;
    if lhs_pair.is_none() && rhs_pair.is_none() {
        return Ok(None);
    }
    let (a, b) = scalar_or_pair(lhs, lhs_pair, env)?;
    let (c, d) = scalar_or_pair(rhs, rhs_pair, env)?;
    let value = match op {
        rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem => (a + c, b + d),
        rumoca_core::OpBinary::Sub | rumoca_core::OpBinary::SubElem => (a - c, b - d),
        rumoca_core::OpBinary::Mul | rumoca_core::OpBinary::MulElem => {
            (a * c - b * d, a * d + b * c)
        }
        rumoca_core::OpBinary::Div | rumoca_core::OpBinary::DivElem => {
            let denominator = c * c + d * d;
            ((a * c + b * d) / denominator, (b * c - a * d) / denominator)
        }
        _ => return Ok(None),
    };
    Ok(Some(value))
}

fn scalar_or_pair<T: SimFloat>(
    expr: &Expression,
    pair: Option<(T, T)>,
    env: &VarEnv<T>,
) -> Result<(T, T), EvalError> {
    match pair {
        Some(pair) => Ok(pair),
        None => eval_expr(expr, env).map(|value| (value, T::zero())),
    }
}

fn try_eval_unary_pair<T: SimFloat>(
    op: &rumoca_core::OpUnary,
    rhs: &Expression,
    env: &VarEnv<T>,
) -> Result<Option<(T, T)>, EvalError> {
    let Some((re, im)) = try_eval_complex_pair(rhs, env)? else {
        return Ok(None);
    };
    match op {
        rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => Ok(Some((-re, -im))),
        rumoca_core::OpUnary::Plus | rumoca_core::OpUnary::DotPlus => Ok(Some((re, im))),
        _ => Ok(None),
    }
}
