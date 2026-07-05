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
    eval_const_expr_with_shape(expr, lookup, &|_| None)
}

pub fn eval_const_expr_with_shape<F, S>(
    expr: &Expression,
    lookup: &F,
    shape_lookup: &S,
) -> Option<ConstValue>
where
    F: Fn(&Reference, &[Subscript]) -> Option<ConstValue>,
    S: Fn(&str) -> Option<Vec<i64>>,
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
            let rhs = eval_const_expr_with_shape(rhs, lookup, shape_lookup)?;
            eval_unary_const(op, rhs)
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = eval_const_expr_with_shape(lhs, lookup, shape_lookup)?;
            let rhs = eval_const_expr_with_shape(rhs, lookup, shape_lookup)?;
            eval_binary_const(op, lhs, rhs)
        }
        Expression::BuiltinCall { function, args, .. } => {
            eval_builtin(*function, args, lookup, shape_lookup)
        }
        Expression::FunctionCall { name, args, .. } => {
            eval_named_function(name.last_segment(), args, lookup, shape_lookup)
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, then_expr) in branches {
                if eval_const_expr_with_shape(cond, lookup, shape_lookup)?.as_bool()? {
                    return eval_const_expr_with_shape(then_expr, lookup, shape_lookup);
                }
            }
            eval_const_expr_with_shape(else_branch, lookup, shape_lookup)
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

pub fn eval_scalar_const_expr_with_shape<F, S>(
    expr: &Expression,
    lookup: &F,
    shape_lookup: &S,
) -> Option<f64>
where
    F: Fn(&Reference, &[Subscript]) -> Option<ConstValue>,
    S: Fn(&str) -> Option<Vec<i64>>,
{
    eval_const_expr_with_shape(expr, lookup, shape_lookup)?.as_real()
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

fn eval_builtin<F, S>(
    function: BuiltinFunction,
    args: &[Expression],
    lookup: &F,
    shape_lookup: &S,
) -> Option<ConstValue>
where
    F: Fn(&Reference, &[Subscript]) -> Option<ConstValue>,
    S: Fn(&str) -> Option<Vec<i64>>,
{
    let arg = |i: usize| {
        args.get(i)
            .and_then(|expr| eval_scalar_const_expr_with_shape(expr, lookup, shape_lookup))
    };

    match function {
        BuiltinFunction::NoEvent => eval_const_expr_with_shape(args.first()?, lookup, shape_lookup),
        BuiltinFunction::Smooth => args
            .get(1)
            .and_then(|expr| eval_const_expr_with_shape(expr, lookup, shape_lookup))
            .or_else(|| {
                args.first()
                    .and_then(|expr| eval_const_expr_with_shape(expr, lookup, shape_lookup))
            }),
        BuiltinFunction::Integer => arg(0).map(f64::floor).map(ConstValue::Real),
        BuiltinFunction::Size => eval_size(args, lookup, shape_lookup).map(ConstValue::Real),
        _ => arg(0)
            .and_then(|lhs| {
                apply_scalar_unary_math(function, lhs)
                    .or_else(|| arg(1).and_then(|rhs| apply_scalar_binary_math(function, lhs, rhs)))
            })
            .map(ConstValue::Real),
    }
}

fn eval_named_function<F, S>(
    short_name: &str,
    args: &[Expression],
    lookup: &F,
    shape_lookup: &S,
) -> Option<ConstValue>
where
    F: Fn(&Reference, &[Subscript]) -> Option<ConstValue>,
    S: Fn(&str) -> Option<Vec<i64>>,
{
    if short_name == "substring" {
        return eval_substring(args, lookup, shape_lookup).map(ConstValue::String);
    }
    if short_name == "ln" {
        return eval_builtin(BuiltinFunction::Log, args, lookup, shape_lookup);
    }
    let function = BuiltinFunction::from_name(short_name)?;
    eval_builtin(function, args, lookup, shape_lookup)
}

fn eval_size<F, S>(args: &[Expression], lookup: &F, shape_lookup: &S) -> Option<f64>
where
    F: Fn(&Reference, &[Subscript]) -> Option<ConstValue>,
    S: Fn(&str) -> Option<Vec<i64>>,
{
    let array = args.first()?;
    let dim = integer_arg(args, lookup, shape_lookup, 1)?;
    let dims = expr_dims(array, shape_lookup)?;
    let value = *dims.get(dim.checked_sub(1)?)?;
    (value >= 0).then_some(value as f64)
}

fn expr_dims<S>(expr: &Expression, shape_lookup: &S) -> Option<Vec<i64>>
where
    S: Fn(&str) -> Option<Vec<i64>>,
{
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => {
            if !subscripts.is_empty() {
                return None;
            }
            reference_shape_lookup(name, shape_lookup)
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            let mut dims = expr_dims(base, shape_lookup)?;
            if subscripts.len() > dims.len() {
                return None;
            }
            dims.drain(0..subscripts.len());
            Some(dims)
        }
        Expression::FieldAccess { base, field, .. } => {
            let base = expr_path(base)?;
            shape_lookup(&format!("{base}.{field}"))
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            Some(vec![i64::try_from(elements.len()).ok()?])
        }
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            ..
        } => args.first().and_then(|arg| expr_dims(arg, shape_lookup)),
        _ => None,
    }
}

fn reference_shape_lookup<S>(name: &Reference, shape_lookup: &S) -> Option<Vec<i64>>
where
    S: Fn(&str) -> Option<Vec<i64>>,
{
    if let Some(component_ref) = name.component_ref()
        && let Some(dims) = shape_lookup(&component_ref.to_string())
    {
        return Some(dims);
    }
    shape_lookup(name.as_str())
}

fn expr_path(expr: &Expression) -> Option<String> {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(
            name.component_ref()
                .map(|component_ref| component_ref.to_string())
                .unwrap_or_else(|| name.as_str().to_string()),
        ),
        Expression::FieldAccess { base, field, .. } => {
            Some(format!("{}.{}", expr_path(base)?, field))
        }
        _ => None,
    }
}

fn numeric_arg<F, S>(args: &[Expression], lookup: &F, shape_lookup: &S, index: usize) -> Option<f64>
where
    F: Fn(&Reference, &[Subscript]) -> Option<ConstValue>,
    S: Fn(&str) -> Option<Vec<i64>>,
{
    eval_const_expr_with_shape(args.get(index)?, lookup, shape_lookup)?.as_real()
}

fn integer_arg<F, S>(
    args: &[Expression],
    lookup: &F,
    shape_lookup: &S,
    index: usize,
) -> Option<usize>
where
    F: Fn(&Reference, &[Subscript]) -> Option<ConstValue>,
    S: Fn(&str) -> Option<Vec<i64>>,
{
    let value = numeric_arg(args, lookup, shape_lookup, index)?;
    (value.fract().abs() <= 1.0e-12 && value >= 1.0).then_some(value as usize)
}

fn string_arg<F, S>(
    args: &[Expression],
    lookup: &F,
    shape_lookup: &S,
    index: usize,
) -> Option<String>
where
    F: Fn(&Reference, &[Subscript]) -> Option<ConstValue>,
    S: Fn(&str) -> Option<Vec<i64>>,
{
    match eval_const_expr_with_shape(args.get(index)?, lookup, shape_lookup)? {
        ConstValue::String(value) => Some(value),
        _ => None,
    }
}

fn eval_substring<F, S>(args: &[Expression], lookup: &F, shape_lookup: &S) -> Option<String>
where
    F: Fn(&Reference, &[Subscript]) -> Option<ConstValue>,
    S: Fn(&str) -> Option<Vec<i64>>,
{
    let input = string_arg(args, lookup, shape_lookup, 0)?;
    let start = integer_arg(args, lookup, shape_lookup, 1)?;
    let stop = integer_arg(args, lookup, shape_lookup, 2)?;
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

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{Span, VarName};

    fn span() -> Span {
        Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2)
    }

    fn int(value: i64) -> Expression {
        Expression::Literal {
            value: Literal::Integer(value),
            span: span(),
        }
    }

    fn var(name: &str) -> Expression {
        Expression::VarRef {
            name: VarName::new(name).into(),
            subscripts: vec![],
            span: span(),
        }
    }

    #[test]
    fn scalar_const_eval_supports_size_with_shape_lookup() {
        let expr = Expression::BuiltinCall {
            function: BuiltinFunction::Size,
            args: vec![var("table"), int(1)],
            span: span(),
        };

        let value = eval_scalar_const_expr_with_shape(&expr, &|_, _| None, &|name| match name {
            "table" => Some(vec![4, 2]),
            _ => None,
        });

        assert_eq!(value, Some(4.0));
    }
}
