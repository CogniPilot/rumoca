//! Compile-time constant evaluation for DAE expressions.

use std::collections::HashMap;

use rumoca_core::{
    BuiltinFunction, Expression, Literal, OpBinary, OpUnary, Reference, Subscript,
    apply_scalar_binary_math, apply_scalar_unary_math,
};

#[derive(Clone, Debug, PartialEq)]
pub enum ConstValue {
    Real(f64),
    Bool(bool),
    String(String),
}

impl ConstValue {
    pub fn as_real(&self) -> Option<f64> {
        match self {
            Self::Real(value) => Some(*value),
            Self::Bool(value) => Some(bool_to_scalar(*value)),
            Self::String(_) => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Self::Bool(value) => Some(*value),
            Self::Real(value) => scalar_to_bool(*value),
            Self::String(_) => None,
        }
    }

    pub fn into_literal(self) -> Literal {
        match self {
            Self::Real(value) => Literal::Real(value),
            Self::Bool(value) => Literal::Boolean(value),
            Self::String(value) => Literal::String(value),
        }
    }

    pub fn is_finite(&self) -> bool {
        match self {
            Self::Real(value) => value.is_finite(),
            Self::Bool(_) | Self::String(_) => true,
        }
    }
}

pub fn eval_const_expr_with<F>(expr: &Expression, lookup: &F) -> Option<ConstValue>
where
    F: Fn(&Reference, &[Subscript]) -> Option<ConstValue>,
{
    match expr {
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Some(ConstValue::Real(*value as f64)),
        Expression::Literal {
            value: Literal::Real(value),
            ..
        } => Some(ConstValue::Real(*value)),
        Expression::Literal {
            value: Literal::Boolean(value),
            ..
        } => Some(ConstValue::Bool(*value)),
        Expression::Literal {
            value: Literal::String(value),
            ..
        } => Some(ConstValue::String(value.clone())),
        Expression::VarRef {
            name, subscripts, ..
        } => lookup(name, subscripts),
        Expression::Index {
            base, subscripts, ..
        } => {
            let Expression::VarRef {
                name,
                subscripts: base_subscripts,
                ..
            } = base.as_ref()
            else {
                return None;
            };
            let mut merged = base_subscripts.clone();
            merged.extend(subscripts.iter().cloned());
            lookup(name, &merged)
        }
        Expression::Unary { op, rhs, .. } => {
            let rhs = eval_const_expr_with(rhs, lookup)?;
            eval_unary_const(op, rhs)
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = eval_const_expr_with(lhs, lookup)?;
            let rhs = eval_const_expr_with(rhs, lookup)?;
            eval_binary_const(op, lhs, rhs)
        }
        Expression::BuiltinCall { function, args, .. } => eval_builtin(*function, args, lookup),
        Expression::FunctionCall { name, args, .. } => {
            eval_named_function(name.last_segment(), args, lookup)
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, then_expr) in branches {
                if eval_const_expr_with(cond, lookup)?.as_bool()? {
                    return eval_const_expr_with(then_expr, lookup);
                }
            }
            eval_const_expr_with(else_branch, lookup)
        }
        _ => None,
    }
}

pub fn eval_scalar_const_expr_with<F>(expr: &Expression, lookup: &F) -> Option<f64>
where
    F: Fn(&Reference, &[Subscript]) -> Option<ConstValue>,
{
    eval_const_expr_with(expr, lookup)?.as_real()
}

pub fn eval_scalar_const_expr(expr: &Expression, constants: &HashMap<String, f64>) -> Option<f64> {
    eval_scalar_const_expr_with(expr, &|name, subscripts| {
        if subscripts.is_empty() {
            constants.get(name.as_str()).copied().map(ConstValue::Real)
        } else {
            None
        }
    })
}

fn eval_unary_const(op: &OpUnary, rhs: ConstValue) -> Option<ConstValue> {
    match op {
        OpUnary::Minus | OpUnary::DotMinus => Some(ConstValue::Real(-rhs.as_real()?)),
        OpUnary::Plus | OpUnary::DotPlus => Some(rhs),
        OpUnary::Not => Some(ConstValue::Bool(!rhs.as_bool()?)),
        _ => None,
    }
}

fn eval_binary_const(op: &OpBinary, lhs: ConstValue, rhs: ConstValue) -> Option<ConstValue> {
    match op {
        OpBinary::Eq => Some(ConstValue::Bool(const_values_equal(&lhs, &rhs)?)),
        OpBinary::Neq => Some(ConstValue::Bool(!const_values_equal(&lhs, &rhs)?)),
        OpBinary::Add | OpBinary::AddElem => {
            Some(ConstValue::Real(lhs.as_real()? + rhs.as_real()?))
        }
        OpBinary::Sub | OpBinary::SubElem => {
            Some(ConstValue::Real(lhs.as_real()? - rhs.as_real()?))
        }
        OpBinary::Mul | OpBinary::MulElem => {
            Some(ConstValue::Real(lhs.as_real()? * rhs.as_real()?))
        }
        OpBinary::Div | OpBinary::DivElem => {
            let rhs = rhs.as_real()?;
            (rhs.abs() > f64::EPSILON).then_some(ConstValue::Real(lhs.as_real()? / rhs))
        }
        OpBinary::Exp | OpBinary::ExpElem => {
            Some(ConstValue::Real(lhs.as_real()?.powf(rhs.as_real()?)))
        }
        OpBinary::Lt => Some(ConstValue::Bool(lhs.as_real()? < rhs.as_real()?)),
        OpBinary::Le => Some(ConstValue::Bool(lhs.as_real()? <= rhs.as_real()?)),
        OpBinary::Gt => Some(ConstValue::Bool(lhs.as_real()? > rhs.as_real()?)),
        OpBinary::Ge => Some(ConstValue::Bool(lhs.as_real()? >= rhs.as_real()?)),
        OpBinary::And => Some(ConstValue::Bool(lhs.as_bool()? && rhs.as_bool()?)),
        OpBinary::Or => Some(ConstValue::Bool(lhs.as_bool()? || rhs.as_bool()?)),
        _ => None,
    }
}

fn eval_builtin<F>(function: BuiltinFunction, args: &[Expression], lookup: &F) -> Option<ConstValue>
where
    F: Fn(&Reference, &[Subscript]) -> Option<ConstValue>,
{
    let arg = |i: usize| {
        args.get(i)
            .and_then(|expr| eval_scalar_const_expr_with(expr, lookup))
    };

    match function {
        BuiltinFunction::NoEvent => eval_const_expr_with(args.first()?, lookup),
        BuiltinFunction::Smooth => args
            .get(1)
            .and_then(|expr| eval_const_expr_with(expr, lookup))
            .or_else(|| {
                args.first()
                    .and_then(|expr| eval_const_expr_with(expr, lookup))
            }),
        BuiltinFunction::Integer => arg(0).map(f64::floor).map(ConstValue::Real),
        _ => arg(0)
            .and_then(|lhs| {
                apply_scalar_unary_math(function, lhs)
                    .or_else(|| arg(1).and_then(|rhs| apply_scalar_binary_math(function, lhs, rhs)))
            })
            .map(ConstValue::Real),
    }
}

fn eval_named_function<F>(short_name: &str, args: &[Expression], lookup: &F) -> Option<ConstValue>
where
    F: Fn(&Reference, &[Subscript]) -> Option<ConstValue>,
{
    if short_name == "substring" {
        return eval_substring(args, lookup).map(ConstValue::String);
    }
    if short_name == "ln" {
        return eval_builtin(BuiltinFunction::Log, args, lookup);
    }
    let function = BuiltinFunction::from_name(short_name)?;
    eval_builtin(function, args, lookup)
}

fn numeric_arg<F>(args: &[Expression], lookup: &F, index: usize) -> Option<f64>
where
    F: Fn(&Reference, &[Subscript]) -> Option<ConstValue>,
{
    eval_const_expr_with(args.get(index)?, lookup)?.as_real()
}

fn integer_arg<F>(args: &[Expression], lookup: &F, index: usize) -> Option<usize>
where
    F: Fn(&Reference, &[Subscript]) -> Option<ConstValue>,
{
    let value = numeric_arg(args, lookup, index)?;
    (value.fract().abs() <= 1.0e-12 && value >= 1.0).then_some(value as usize)
}

fn string_arg<F>(args: &[Expression], lookup: &F, index: usize) -> Option<String>
where
    F: Fn(&Reference, &[Subscript]) -> Option<ConstValue>,
{
    match eval_const_expr_with(args.get(index)?, lookup)? {
        ConstValue::String(value) => Some(value),
        _ => None,
    }
}

fn eval_substring<F>(args: &[Expression], lookup: &F) -> Option<String>
where
    F: Fn(&Reference, &[Subscript]) -> Option<ConstValue>,
{
    let input = string_arg(args, lookup, 0)?;
    let start = integer_arg(args, lookup, 1)?;
    let stop = integer_arg(args, lookup, 2)?;
    if stop < start {
        return Some(String::new());
    }
    let skip = start.checked_sub(1)?;
    Some(input.chars().skip(skip).take(stop - start + 1).collect())
}

fn const_values_equal(lhs: &ConstValue, rhs: &ConstValue) -> Option<bool> {
    match (lhs, rhs) {
        (ConstValue::Real(lhs), ConstValue::Real(rhs)) => Some(scalar_almost_eq(*lhs, *rhs)),
        (ConstValue::Bool(lhs), ConstValue::Bool(rhs)) => Some(lhs == rhs),
        (ConstValue::String(lhs), ConstValue::String(rhs)) => Some(lhs == rhs),
        _ => None,
    }
}

fn bool_to_scalar(flag: bool) -> f64 {
    if flag { 1.0 } else { 0.0 }
}

fn scalar_to_bool(value: f64) -> Option<bool> {
    value.is_finite().then_some(value.abs() > 1.0e-12)
}

fn scalar_almost_eq(lhs: f64, rhs: f64) -> bool {
    (lhs - rhs).abs() <= 1.0e-12 * (1.0 + lhs.abs().max(rhs.abs()))
}
