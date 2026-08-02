//! Expression dispatch: the top-level constant evaluator and the per-node
//! evaluators for references, calls, conditionals, arrays, and subscripts.

use rumoca_core::Span;

use super::builtin_dispatch::eval_builtin_function;
use super::context::EvalContext;
use super::errors::EvalError;
use super::operators::{eval_binary_op, eval_unary_op};
use super::range_eval::eval_range;
use super::value::Value;
use super::{
    BuiltinFunction, EvalLimits, Expression, Function, Literal, OpBinary, OpUnary, Subscript,
    eval_builtin, function_eval, is_builtin,
};

/// Evaluate a flat expression to a constant value.
///
/// Returns an error if the expression cannot be evaluated at compile time
/// (e.g., references time-varying variables, uses unsupported operations).
pub fn eval_expr(expr: &Expression, ctx: &EvalContext) -> Result<Value, EvalError> {
    let span = expr.span().ok_or_else(|| {
        EvalError::missing_source_context("constant expression is missing source provenance")
    })?;
    eval_expr_with_span(expr, ctx, span)
}

/// Evaluate with a span for error reporting.
pub fn eval_expr_with_span(
    expr: &Expression,
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    let span = expr.span().unwrap_or(span);
    match expr {
        Expression::Literal { value: lit, .. } => Ok(eval_literal(lit)),
        Expression::VarRef {
            name, subscripts, ..
        } => eval_var_ref(name.as_str(), subscripts, ctx, span),
        Expression::Binary { op, lhs, rhs, .. } => eval_flat_binary(op, lhs, rhs, ctx, span),
        Expression::Unary { op, rhs, .. } => eval_flat_unary(op, rhs, ctx, span),
        Expression::BuiltinCall { function, args, .. } => {
            eval_builtin_call(function, args, ctx, span)
        }
        Expression::FunctionCall { name, args, .. } => eval_fn_call(name.as_str(), args, ctx, span),
        Expression::StringConversion { .. } => Err(EvalError::UnsupportedExpression {
            kind: "predefined String conversion".to_string(),
            span,
        }),
        Expression::If {
            branches,
            else_branch,
            ..
        } => eval_flat_if(branches, else_branch, ctx, span),
        Expression::Array { elements, .. } => eval_flat_array(elements, ctx, span),
        Expression::Range {
            start, step, end, ..
        } => eval_range(start, step.as_deref(), end, ctx, span),
        Expression::ArrayComprehension { .. } => Err(EvalError::UnsupportedExpression {
            kind: "ArrayComprehension".to_string(),
            span,
        }),
        Expression::Index {
            base, subscripts, ..
        } => eval_flat_index(base, subscripts, ctx, span),
        Expression::Tuple { elements, .. } => eval_flat_array(elements, ctx, span),
        Expression::FieldAccess {
            base,
            field,
            field_def_id,
            ..
        } => {
            if let Some(path) = rumoca_core::flat_expression_component_path(expr)
                && let Some(value) = ctx.get(&path.to_flat_string())
            {
                return Ok(value.clone());
            }
            // Field access on complex expressions (e.g., func().field)
            // requires evaluating the base and then extracting the field
            let base_val = eval_expr_with_span(base, ctx, span)?;
            if function_eval::is_exact_single_record_output(base, *field_def_id, ctx) {
                return Ok(base_val);
            }
            eval_field_access(&base_val, field, span)
        }
        Expression::Empty { .. } => Ok(Value::Integer(0)),
    }
}

/// Evaluate a variable reference.
fn eval_var_ref(
    name: &str,
    subscripts: &[Subscript],
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    // First try as a parameter
    if let Some(value) = ctx.get(name) {
        let value = value.clone();
        return if subscripts.is_empty() {
            Ok(value)
        } else {
            apply_subscripts(&value, subscripts, ctx, span)
        };
    }
    // Then try as an enum literal from context
    if let Some((type_name, literal)) = ctx.get_enum(name) {
        return Ok(Value::Enum(type_name.clone(), literal.clone()));
    }
    // A declared `fixed = false` parameter resolves; only its value is absent
    // (MLS §8.6). Reporting that as an unknown name reads as a resolution
    // defect, so name the construct the fold actually hit.
    if let Some(source) = ctx.deferred_parameter(name) {
        return Err(EvalError::initialization_deferred(name, source, span));
    }
    // DON'T guess that qualified names are enums - this causes bugs where
    // qualified variable names like "data.m" are incorrectly treated as enum literals
    // when they haven't been evaluated yet in multi-pass parameter evaluation.
    // Enum literals are explicitly added to context via add_parameter().
    Err(EvalError::unknown_variable(name, span))
}

/// Evaluate a binary expression.
fn eval_flat_binary(
    op: &OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    if matches!(op, OpBinary::And | OpBinary::Or) {
        return eval_flat_logical(op, lhs, rhs, ctx, span);
    }
    let lhs_val = eval_expr_with_span(lhs, ctx, span)?;
    let rhs_val = eval_expr_with_span(rhs, ctx, span)?;
    eval_binary_op(op, &lhs_val, &rhs_val, span)
}

fn eval_flat_logical(
    op: &OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    match eval_expr_with_span(lhs, ctx, span) {
        Ok(Value::Bool(lhs)) if logical_value_determines_result(op, lhs) => Ok(Value::Bool(lhs)),
        Ok(lhs) => {
            let rhs = eval_expr_with_span(rhs, ctx, span)?;
            eval_binary_op(op, &lhs, &rhs, span)
        }
        // A deferred parameter (MLS §8.6) is undetermined in exactly the sense
        // this short circuit tolerates: the operand has no value yet, and the
        // other operand may still decide the result on its own.
        Err(
            lhs_error @ (EvalError::UnknownVariable { .. }
            | EvalError::InitializationDeferred { .. }
            | EvalError::NotConstant { .. }),
        ) => match eval_expr_with_span(rhs, ctx, span) {
            Ok(Value::Bool(rhs)) if logical_value_determines_result(op, rhs) => {
                Ok(Value::Bool(rhs))
            }
            Ok(Value::Bool(_)) | Err(_) => Err(lhs_error),
            Ok(value) => Err(EvalError::type_mismatch("Boolean", value.type_name(), span)),
        },
        Err(error) => Err(error),
    }
}

fn logical_value_determines_result(op: &OpBinary, value: bool) -> bool {
    matches!((op, value), (OpBinary::And, false) | (OpBinary::Or, true))
}

/// Evaluate a unary expression.
fn eval_flat_unary(
    op: &OpUnary,
    rhs: &Expression,
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    let rhs_val = eval_expr_with_span(rhs, ctx, span)?;
    eval_unary_op(op, &rhs_val, span)
}

/// Evaluate a builtin call expression.
fn eval_builtin_call(
    function: &BuiltinFunction,
    args: &[Expression],
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    if matches!(function, BuiltinFunction::Size | BuiltinFunction::Ndims)
        && let Some(value) = eval_shape_builtin(function, args, ctx, span)?
    {
        return Ok(value);
    }
    let arg_values: Vec<Value> = args
        .iter()
        .map(|a| eval_expr_with_span(a, ctx, span))
        .collect::<Result<_, _>>()?;
    eval_builtin_function(function, &arg_values, span)
}

fn eval_shape_builtin(
    function: &BuiltinFunction,
    args: &[Expression],
    ctx: &EvalContext,
    span: Span,
) -> Result<Option<Value>, EvalError> {
    let Some(first) = args.first() else {
        return Ok(None);
    };
    let Some(path) = rumoca_core::flat_expression_component_path(first) else {
        return Ok(None);
    };
    let Some(dims) = ctx.get_array_dimensions(&path.to_flat_string()) else {
        return Ok(None);
    };
    if matches!(function, BuiltinFunction::Ndims) {
        return Ok(Some(Value::Integer(dims.len() as i64)));
    }
    match args {
        [_] if dims.len() == 1 => Ok(Some(Value::Integer(dims[0]))),
        [_] => Ok(Some(Value::Array(
            dims.iter().copied().map(Value::Integer).collect(),
        ))),
        [_, dimension] => {
            let dimension = eval_expr_with_span(dimension, ctx, span)?
                .as_integer()
                .ok_or_else(|| EvalError::type_mismatch("Integer", "non-integer", span))?;
            let index = dimension
                .checked_sub(1)
                .and_then(|dimension| usize::try_from(dimension).ok())
                .filter(|index| *index < dims.len())
                .ok_or_else(|| EvalError::function_error("dimension out of range", span))?;
            Ok(Some(Value::Integer(dims[index])))
        }
        _ => Ok(None),
    }
}

/// Evaluate a function call expression.
fn eval_fn_call(
    name: &str,
    args: &[Expression],
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    if is_builtin(name) {
        let arg_values: Vec<Value> = args
            .iter()
            .map(|a| eval_expr_with_span(a, ctx, span))
            .collect::<Result<_, _>>()?;
        return eval_builtin(name, &arg_values, span);
    }
    if let Some(func) = ctx.functions.get(name) {
        return eval_user_function(func, args, ctx, span);
    }
    Err(EvalError::not_constant(
        format!("unknown function: {}", name),
        span,
    ))
}

/// Evaluate a user-defined function.
fn eval_user_function(
    func: &Function,
    args: &[Expression],
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    if !func.pure {
        return Err(EvalError::not_constant(
            format!("impure function: {}", func.name),
            span,
        ));
    }
    if func.external.is_some() {
        return Err(EvalError::not_constant(
            format!("external function: {}", func.name),
            span,
        ));
    }
    let mut call_args = Vec::with_capacity(args.len());
    for arg in args {
        if let Some((name, value)) = named_function_call_arg(arg) {
            call_args.push(function_eval::FunctionCallArg::named(
                name.to_string(),
                eval_expr_with_span(value, ctx, span)?,
            ));
        } else {
            call_args.push(function_eval::FunctionCallArg::positional(
                eval_expr_with_span(arg, ctx, span)?,
            ));
        }
    }
    function_eval::eval_function_with_call_args(
        func,
        call_args,
        ctx,
        &EvalLimits::default(),
        0,
        span,
    )
}

fn named_function_call_arg(expr: &Expression) -> Option<(&str, &Expression)> {
    const PREFIX: &str = "__rumoca_named_arg__.";
    let Expression::FunctionCall {
        name,
        args,
        is_constructor: true,
        ..
    } = expr
    else {
        return None;
    };
    let name = name.as_str().strip_prefix(PREFIX)?;
    (args.len() == 1).then(|| (name, &args[0]))
}

/// Evaluate an if expression.
fn eval_flat_if(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    let mut unknown_branch_values = Vec::new();
    for (cond, then_expr) in branches {
        match eval_expr_with_span(cond, ctx, span) {
            Ok(Value::Bool(true)) => {
                let value = eval_expr_with_span(then_expr, ctx, span)?;
                return require_equal_unknown_outcomes(&unknown_branch_values, value, span);
            }
            Ok(Value::Bool(false)) => {}
            Ok(value) => {
                return Err(EvalError::type_mismatch("Boolean", value.type_name(), span));
            }
            Err(
                EvalError::UnknownVariable { .. }
                | EvalError::InitializationDeferred { .. }
                | EvalError::NotConstant { .. },
            ) => {
                unknown_branch_values.push(eval_expr_with_span(then_expr, ctx, span)?);
            }
            Err(error) => return Err(error),
        }
    }
    let else_value = eval_expr_with_span(else_branch, ctx, span)?;
    require_equal_unknown_outcomes(&unknown_branch_values, else_value, span)
}

fn require_equal_unknown_outcomes(
    unknown_branch_values: &[Value],
    outcome: Value,
    span: Span,
) -> Result<Value, EvalError> {
    if unknown_branch_values
        .iter()
        .all(|value| values_semantically_equal(value, &outcome))
    {
        Ok(outcome)
    } else {
        Err(EvalError::not_constant(
            "if-expression condition is not constant and branches differ",
            span,
        ))
    }
}

fn values_semantically_equal(lhs: &Value, rhs: &Value) -> bool {
    match (lhs, rhs) {
        (Value::Real(lhs), Value::Real(rhs)) => lhs == rhs || lhs.to_bits() == rhs.to_bits(),
        (Value::Array(lhs), Value::Array(rhs)) => {
            lhs.len() == rhs.len()
                && lhs
                    .iter()
                    .zip(rhs)
                    .all(|(lhs, rhs)| values_semantically_equal(lhs, rhs))
        }
        (Value::Record(lhs), Value::Record(rhs)) => {
            lhs.len() == rhs.len()
                && lhs.iter().all(|(name, lhs)| {
                    rhs.get(name)
                        .is_some_and(|rhs| values_semantically_equal(lhs, rhs))
                })
        }
        _ => lhs == rhs,
    }
}

/// Evaluate an array expression.
fn eval_flat_array(
    elements: &[Expression],
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    let values: Vec<Value> = elements
        .iter()
        .map(|e| eval_expr_with_span(e, ctx, span))
        .collect::<Result<_, _>>()?;
    Ok(Value::Array(values))
}

/// Evaluate an index expression.
fn eval_flat_index(
    base: &Expression,
    subscripts: &[Subscript],
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    let base_val = eval_expr_with_span(base, ctx, span)?;
    apply_subscripts(&base_val, subscripts, ctx, span)
}

/// Evaluate field access on a record value.
fn eval_field_access(base_val: &Value, field: &str, span: Span) -> Result<Value, EvalError> {
    match base_val {
        Value::Record(fields) => {
            if let Some(value) = fields.get(field) {
                Ok(value.clone())
            } else {
                Err(EvalError::TypeMismatch {
                    expected: format!("record with field '{}'", field),
                    actual: format!("record without field '{}'", field),
                    span,
                })
            }
        }
        _ => Err(EvalError::TypeMismatch {
            expected: "record".to_string(),
            actual: format!("{:?}", base_val),
            span,
        }),
    }
}

/// Convert a literal to a value.
fn eval_literal(lit: &Literal) -> Value {
    match lit {
        Literal::Real(v) => Value::Real(*v),
        Literal::Integer(v) => Value::Integer(*v),
        Literal::Boolean(v) => Value::Bool(*v),
        Literal::String(s) => Value::String(s.clone()),
    }
}

/// Apply subscripts to a value.
fn apply_subscripts(
    value: &Value,
    subscripts: &[Subscript],
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    let mut current = value.clone();

    for subscript in subscripts {
        match subscript {
            Subscript::Index { value: idx, .. } => {
                let idx = *idx as usize;

                let arr = current
                    .as_array()
                    .ok_or_else(|| EvalError::type_mismatch("Array", current.type_name(), span))?;

                // Modelica uses 1-based indexing
                if idx < 1 || idx > arr.len() {
                    return Err(EvalError::IndexOutOfBounds {
                        index: idx as i64,
                        size: arr.len(),
                        span,
                    });
                }
                current = arr[idx - 1].clone();
            }
            Subscript::Colon { .. } => {
                // Colon means "all elements" - just pass through
                // (this is a simplification; real slicing would need more work)
            }
            Subscript::Expr { expr, .. } => {
                // Evaluate the expression to get the index
                let idx_val = eval_expr_with_span(expr, ctx, span)?;
                let idx = idx_val
                    .as_integer()
                    .ok_or_else(|| EvalError::type_mismatch("Integer", idx_val.type_name(), span))?
                    as usize;

                let arr = current
                    .as_array()
                    .ok_or_else(|| EvalError::type_mismatch("Array", current.type_name(), span))?;

                // Modelica uses 1-based indexing
                if idx < 1 || idx > arr.len() {
                    return Err(EvalError::IndexOutOfBounds {
                        index: idx as i64,
                        size: arr.len(),
                        span,
                    });
                }
                current = arr[idx - 1].clone();
            }
        }
    }

    Ok(current)
}

/// Try to evaluate an expression to an integer.
/// Returns None if evaluation fails or result is not an integer.
pub fn try_eval_integer(expr: &Expression, ctx: &EvalContext) -> Option<i64> {
    eval_expr(expr, ctx).ok().and_then(|v| v.as_integer())
}

/// Try to evaluate an expression to a real.
/// Returns None if evaluation fails or result is not numeric.
pub fn try_eval_real(expr: &Expression, ctx: &EvalContext) -> Option<f64> {
    eval_expr(expr, ctx).ok().and_then(|v| v.to_real())
}

/// Try to evaluate an expression to a boolean.
/// Returns None if evaluation fails or result is not a boolean.
pub fn try_eval_bool(expr: &Expression, ctx: &EvalContext) -> Option<bool> {
    eval_expr(expr, ctx).ok().and_then(|v| v.as_bool())
}
