//! Built-in function implementations for constant evaluation.
//!
//! This module provides implementations of Modelica built-in functions
//! that can be evaluated at compile time (MLS §3.7).
//!
//! Implemented functions:
//! - Mathematical functions (MLS §3.7.3): sin, cos, tan, asin, acos, atan, etc.
//! - Array functions (MLS §10.3): size, ndims, sum, product, fill, zeros, ones, linspace, cat
//! - Conversion functions (MLS §3.7.2): integer, div, mod, rem

use super::errors::EvalError;
use super::value::{
    Value, materialized_node_count, rectangular_materialized_node_count,
    rectangular_shape_is_representable,
};
use super::{DEFAULT_EVAL_BUDGET, DEFAULT_MATERIALIZED_RANK_BUDGET, EvalContext};
use rumoca_core::{BuiltinFunction, Span, apply_scalar_binary_math, apply_scalar_unary_math};

/// Evaluate a built-in function call.
pub fn eval_builtin(name: &str, args: &[Value], span: Span) -> Result<Value, EvalError> {
    if let Some(function) = BuiltinFunction::ALL.iter().copied().find(|function| {
        function.name() == name || (*function == BuiltinFunction::Integer && name == "Integer")
    }) {
        super::builtin_dispatch::validate_builtin_arity(function, args.len(), span)?;
    }
    if let Some(result) = eval_vectorized_call(name, args, span) {
        return result;
    }
    if let Some(result) = eval_scalar_math_builtin(name, args, span) {
        return result;
    }
    match name {
        "sqrt" => eval_math_1(args, f64::sqrt, span),
        "abs" => eval_abs(args, span),
        "sign" => eval_sign(args, span),
        "floor" => eval_floor(args, span),
        "ceil" => eval_ceil(args, span),

        // Math functions (two arguments)
        "atan2" => eval_math_2(args, f64::atan2, span),
        "min" => eval_min_max(args, true, span),
        "max" => eval_min_max(args, false, span),
        "mod" => eval_mod(args, span),
        "rem" => eval_rem(args, span),

        // Array functions
        "size" => eval_size(args, span),
        "ndims" => eval_ndims(args, span),
        "sum" => eval_sum(args, span),
        "product" => eval_product(args, span),
        "fill" => eval_fill(args, span),
        "zeros" => eval_zeros(args, span),
        "ones" => eval_ones(args, span),
        "linspace" => eval_linspace(args, span),
        "cat" => eval_cat(args, span),

        // Conversion functions
        "integer" => eval_integer(args, span),
        "div" => eval_div(args, span),

        _ => Err(EvalError::unknown_function(name, span)),
    }
}

/// Evaluate a builtin whose result may depend on resolved declaration data.
///
/// Enumeration ordinals are not encoded in a literal's spelling.  They come
/// from declaration order, so `Integer(enumValue)` must consult the resolved
/// enumeration catalog instead of guessing or rejecting a legal conversion.
pub(super) fn eval_builtin_in_context(
    name: &str,
    args: &[Value],
    _ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    if matches!(name, "integer" | "Integer") {
        check_arg_count(args, 1, span)?;
        return eval_integer_value(&args[0], span);
    }
    eval_builtin(name, args, span)
}

fn eval_integer_value(value: &Value, span: Span) -> Result<Value, EvalError> {
    match value {
        Value::Array(elements) => elements
            .iter()
            .map(|element| eval_integer_value(element, span))
            .collect::<Result<Vec<_>, _>>()
            .map(Value::Array),
        Value::Enum(type_name, literal) => Err(EvalError::UnsupportedExpression {
            kind: format!(
                "Integer conversion of structural enumeration `{type_name}.{literal}` without a resolved declaration ordinal"
            ),
            span,
        }),
        Value::ResolvedEnum(value) => Ok(Value::Integer(value.ordinal())),
        _ => eval_integer(std::slice::from_ref(value), span),
    }
}

/// Apply a one-argument scalar builtin element-wise to an array actual.
///
/// MLS 3.6 §12.4.6 (referenced from §10.6.12): "Functions with one scalar
/// return value can be applied to arrays element-wise, e.g., if `A` is a vector
/// of reals, then `sin(A)` is a vector where each element is the result of
/// applying the function `sin` to the corresponding element in `A`" —
/// `sin({a, b, c}) = {sin(a), sin(b), sin(c)}`. The array actual is the
/// *foreach argument* of that rule, and the result has its dimension sizes; the
/// recursion carries the rule through a matrix row by row.
///
/// Only the one-argument scalar functions of MLS §3.7.3 (elementary
/// mathematical), §3.7.1 (numeric `abs`/`sign`/`sqrt`) and §3.7.2
/// (event-triggering `floor`/`ceil`/`integer`) are vectorized here, because
/// those are exactly the builtins whose formal parameter is a scalar — an array
/// actual can only be a foreach argument. `size`, `ndims`, `sum`, `product`,
/// `fill`, `cat`, `linspace` and the reduction forms of `min`/`max`
/// declare array formals, so an array actual there is the ordinary call and
/// vectorizing it would change what the model means.
fn eval_vectorized_call(
    name: &str,
    args: &[Value],
    span: Span,
) -> Option<Result<Value, EvalError>> {
    if !is_scalar_argument_builtin(name) {
        return None;
    }
    let [Value::Array(elements)] = args else {
        return None;
    };
    Some(
        elements
            .iter()
            .map(|element| eval_builtin(name, std::slice::from_ref(element), span))
            .collect::<Result<Vec<_>, _>>()
            .map(Value::Array),
    )
}

/// The builtins whose single formal parameter is a scalar (MLS §3.7.1, §3.7.2,
/// §3.7.3), so an array actual is a foreach argument under MLS §12.4.6.
fn is_scalar_argument_builtin(name: &str) -> bool {
    matches!(
        name,
        "sin"
            | "cos"
            | "tan"
            | "asin"
            | "acos"
            | "atan"
            | "sinh"
            | "cosh"
            | "tanh"
            | "exp"
            | "log"
            | "log10"
            | "sqrt"
            | "abs"
            | "sign"
            | "floor"
            | "ceil"
            | "integer"
    )
}

fn eval_scalar_math_builtin(
    name: &str,
    args: &[Value],
    span: Span,
) -> Option<Result<Value, EvalError>> {
    let function = BuiltinFunction::from_name(name)?;
    if !function.is_unary_real_math() {
        return None;
    }
    Some((|| {
        check_arg_count(args, 1, span)?;
        let arg = to_real(&args[0], span)?;
        let value = apply_scalar_unary_math(function, arg)
            .ok_or_else(|| EvalError::function_error("unsupported scalar math builtin", span))?;
        Ok(Value::Real(value))
    })())
}

// Helper: single-argument math function
fn eval_math_1<F>(args: &[Value], f: F, span: Span) -> Result<Value, EvalError>
where
    F: Fn(f64) -> f64,
{
    check_arg_count(args, 1, span)?;
    let x = to_real(&args[0], span)?;
    Ok(Value::Real(f(x)))
}

// Helper: two-argument math function
fn eval_math_2<F>(args: &[Value], f: F, span: Span) -> Result<Value, EvalError>
where
    F: Fn(f64, f64) -> f64,
{
    check_arg_count(args, 2, span)?;
    let x = to_real(&args[0], span)?;
    let y = to_real(&args[1], span)?;
    Ok(Value::Real(f(x, y)))
}

// abs: works on both Real and Integer
fn eval_abs(args: &[Value], span: Span) -> Result<Value, EvalError> {
    check_arg_count(args, 1, span)?;
    match &args[0] {
        Value::Real(x) => Ok(Value::Real(x.abs())),
        Value::Integer(x) => x
            .checked_abs()
            .map(Value::Integer)
            .ok_or_else(|| integer_overflow_error("abs(...)", span)),
        other => Err(EvalError::type_mismatch(
            "Real or Integer",
            other.type_name(),
            span,
        )),
    }
}

// sign: returns -1, 0, or 1
fn eval_sign(args: &[Value], span: Span) -> Result<Value, EvalError> {
    check_arg_count(args, 1, span)?;
    match &args[0] {
        Value::Real(x) => Ok(Value::Real(rumoca_core::modelica_sign(*x))),
        Value::Integer(x) => {
            let s = if *x > 0 {
                1
            } else if *x < 0 {
                -1
            } else {
                0
            };
            Ok(Value::Integer(s))
        }
        other => Err(EvalError::type_mismatch(
            "Real or Integer",
            other.type_name(),
            span,
        )),
    }
}

// floor: returns Integer (MLS §3.7.2)
fn eval_floor(args: &[Value], span: Span) -> Result<Value, EvalError> {
    check_arg_count(args, 1, span)?;
    match &args[0] {
        Value::Real(x) => checked_real_to_i64(x.floor(), span, "floor(...)").map(Value::Integer),
        Value::Integer(x) => Ok(Value::Integer(*x)),
        other => Err(EvalError::type_mismatch(
            "Real or Integer",
            other.type_name(),
            span,
        )),
    }
}

// ceil: returns Integer (MLS §3.7.2)
fn eval_ceil(args: &[Value], span: Span) -> Result<Value, EvalError> {
    check_arg_count(args, 1, span)?;
    match &args[0] {
        Value::Real(x) => checked_real_to_i64(x.ceil(), span, "ceil(...)").map(Value::Integer),
        Value::Integer(x) => Ok(Value::Integer(*x)),
        other => Err(EvalError::type_mismatch(
            "Real or Integer",
            other.type_name(),
            span,
        )),
    }
}

/// Collect the scalar elements of an array of any rank in row-major order.
///
/// MLS §3.7.2.4 defines `min(A)`/`max(A)` over an array expression with any
/// number of dimensions, so a matrix such as `[a; b]` (a `Value::Array` of
/// row `Value::Array`s) must be reduced element-wise, not row-wise.
fn collect_array_scalars<'a>(value: &'a Value, out: &mut Vec<&'a Value>) {
    match value.as_array() {
        Some(elements) => {
            for element in elements {
                collect_array_scalars(element, out);
            }
        }
        None => out.push(value),
    }
}

// min/max: works on two values or array
fn eval_min_max(args: &[Value], is_min: bool, span: Span) -> Result<Value, EvalError> {
    if args.len() == 1 {
        // Array version (any rank; MLS §3.7.2.4)
        if args[0].as_array().is_none() {
            return Err(EvalError::type_mismatch("Array", args[0].type_name(), span));
        }
        let mut arr: Vec<&Value> = Vec::new();
        collect_array_scalars(&args[0], &mut arr);
        if arr.is_empty() {
            return Err(EvalError::function_error("min/max on empty array", span));
        }

        // Check if all Integer
        let all_int = arr.iter().all(|v| matches!(v, Value::Integer(_)));
        if all_int {
            let result = arr[1..]
                .iter()
                .try_fold(integer_value(arr[0], span)?, |acc, value| {
                    let value = integer_value(value, span)?;
                    let result = select_min_max_integer(acc, value, is_min);
                    Ok(result)
                })?;
            return Ok(Value::Integer(result));
        }

        // Convert to Real
        let values: Vec<f64> = arr
            .iter()
            .map(|v| to_real(v, span))
            .collect::<Result<_, _>>()?;
        let result = if is_min {
            values[1..]
                .iter()
                .copied()
                .fold(values[0], |acc, value| acc.min(value))
        } else {
            values[1..]
                .iter()
                .copied()
                .fold(values[0], |acc, value| acc.max(value))
        };
        Ok(Value::Real(result))
    } else {
        // Two-argument version
        check_arg_count(args, 2, span)?;

        // If both Integer, return Integer
        if let (Value::Integer(x), Value::Integer(y)) = (&args[0], &args[1]) {
            let result = if is_min { (*x).min(*y) } else { (*x).max(*y) };
            return Ok(Value::Integer(result));
        }

        let x = to_real(&args[0], span)?;
        let y = to_real(&args[1], span)?;
        let function = if is_min {
            BuiltinFunction::Min
        } else {
            BuiltinFunction::Max
        };
        let result = apply_scalar_binary_math(function, x, y)
            .ok_or_else(|| EvalError::function_error("min/max evaluation failed", span))?;
        Ok(Value::Real(result))
    }
}

fn select_min_max_integer(acc: i64, value: i64, is_min: bool) -> i64 {
    if is_min {
        acc.min(value)
    } else {
        acc.max(value)
    }
}

// mod: x - floor(x/y) * y
fn eval_mod(args: &[Value], span: Span) -> Result<Value, EvalError> {
    check_arg_count(args, 2, span)?;
    if let (Value::Integer(x), Value::Integer(y)) = (&args[0], &args[1]) {
        if *y == 0 {
            return Err(EvalError::DivisionByZero { span });
        }
        return rumoca_core::eval_integer_mod_builtin(*x, *y)
            .map(Value::Integer)
            .ok_or_else(|| integer_overflow_error("mod(...)", span));
    }
    let x = to_real(&args[0], span)?;
    let y = to_real(&args[1], span)?;
    if y == 0.0 {
        return Err(EvalError::DivisionByZero { span });
    }
    apply_scalar_binary_math(BuiltinFunction::Mod, x, y)
        .map(Value::Real)
        .ok_or(EvalError::DivisionByZero { span })
}

// rem: x - div(x,y) * y (truncated division)
fn eval_rem(args: &[Value], span: Span) -> Result<Value, EvalError> {
    check_arg_count(args, 2, span)?;
    if let (Value::Integer(x), Value::Integer(y)) = (&args[0], &args[1]) {
        if *y == 0 {
            return Err(EvalError::DivisionByZero { span });
        }
        return rumoca_core::eval_integer_rem_builtin(*x, *y)
            .map(Value::Integer)
            .ok_or_else(|| integer_overflow_error("rem(...)", span));
    }
    let x = to_real(&args[0], span)?;
    let y = to_real(&args[1], span)?;
    if y == 0.0 {
        return Err(EvalError::DivisionByZero { span });
    }
    apply_scalar_binary_math(BuiltinFunction::Rem, x, y)
        .map(Value::Real)
        .ok_or(EvalError::DivisionByZero { span })
}

// div: integer division truncated toward zero
fn eval_div(args: &[Value], span: Span) -> Result<Value, EvalError> {
    check_arg_count(args, 2, span)?;
    match (&args[0], &args[1]) {
        (Value::Integer(x), Value::Integer(y)) => {
            if *y == 0 {
                return Err(EvalError::DivisionByZero { span });
            }
            rumoca_core::eval_integer_div_builtin(*x, *y)
                .map(Value::Integer)
                .ok_or_else(|| integer_overflow_error("div(...)", span))
        }
        (a, b) => {
            let x = to_real(a, span)?;
            let y = to_real(b, span)?;
            if y == 0.0 {
                return Err(EvalError::DivisionByZero { span });
            }
            let result = (x / y).trunc();
            if result.is_finite() {
                Ok(Value::Real(result))
            } else {
                Err(EvalError::function_error(
                    "div(...) produced a non-finite Real",
                    span,
                ))
            }
        }
    }
}

// integer: convert Real to Integer by floor (MLS §3.7.2)
fn eval_integer(args: &[Value], span: Span) -> Result<Value, EvalError> {
    check_arg_count(args, 1, span)?;
    match &args[0] {
        Value::Integer(x) => Ok(Value::Integer(*x)),
        Value::Real(x) => checked_real_to_i64(
            rumoca_core::modelica_integer_value(*x),
            span,
            "integer(...)",
        )
        .map(Value::Integer),
        other => Err(EvalError::type_mismatch(
            "Real or Integer",
            other.type_name(),
            span,
        )),
    }
}

// size: get array dimension
fn eval_size(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.is_empty() || args.len() > 2 {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            actual: args.len(),
            span,
        });
    }

    let dimensions = array_dimensions(&args[0], span)?;

    if args.len() == 1 {
        dimensions
            .into_iter()
            .map(|extent| {
                i64::try_from(extent)
                    .map(Value::Integer)
                    .map_err(|_| EvalError::Internal {
                        message: "array extent exceeds i64 during size evaluation".to_string(),
                    })
            })
            .collect::<Result<Vec<_>, _>>()
            .map(Value::Array)
    } else {
        let dim = args[1]
            .as_integer()
            .ok_or_else(|| EvalError::type_mismatch("Integer", args[1].type_name(), span))?;
        let axis = dim
            .checked_sub(1)
            .and_then(|axis| usize::try_from(axis).ok());
        let Some(axis) = axis else {
            return Err(EvalError::range_error("size dimension must be >= 1", span));
        };
        if let Some(extent) = dimensions.get(axis) {
            return i64::try_from(*extent)
                .map(Value::Integer)
                .map_err(|_| EvalError::Internal {
                    message: "array extent exceeds i64 during size evaluation".to_string(),
                });
        }
        Err(EvalError::range_error(
            format!(
                "size dimension {dim} exceeds array rank {}",
                dimensions.len()
            ),
            span,
        ))
    }
}

// ndims: number of dimensions
fn eval_ndims(args: &[Value], span: Span) -> Result<Value, EvalError> {
    check_arg_count(args, 1, span)?;
    let rank = array_dimensions(&args[0], span)?.len();
    i64::try_from(rank)
        .map(Value::Integer)
        .map_err(|_| EvalError::Internal {
            message: "array rank exceeds i64 during ndims evaluation".to_string(),
        })
}

// sum: sum of array elements
fn eval_sum(args: &[Value], span: Span) -> Result<Value, EvalError> {
    check_arg_count(args, 1, span)?;
    array_dimensions(&args[0], span)?;
    let arr = args[0]
        .as_array()
        .ok_or_else(|| EvalError::type_mismatch("Array", args[0].type_name(), span))?;
    reduce_numeric_array(arr, false, span)
}

// product: product of array elements
fn eval_product(args: &[Value], span: Span) -> Result<Value, EvalError> {
    check_arg_count(args, 1, span)?;
    array_dimensions(&args[0], span)?;
    let arr = args[0]
        .as_array()
        .ok_or_else(|| EvalError::type_mismatch("Array", args[0].type_name(), span))?;

    reduce_numeric_array(arr, true, span)
}

#[derive(Clone, Copy)]
enum NumericReduction {
    Integer(i64),
    Real(f64),
}

fn reduce_numeric_array(values: &[Value], multiply: bool, span: Span) -> Result<Value, EvalError> {
    let mut accumulator = None;
    reduce_numeric_values(values, &mut accumulator, multiply, span)?;
    let accumulator = accumulator.ok_or_else(|| EvalError::UnsupportedExpression {
        kind: "empty numeric reduction requires retained array element type".to_string(),
        span,
    })?;
    Ok(match accumulator {
        NumericReduction::Integer(value) => Value::Integer(value),
        NumericReduction::Real(value) => Value::Real(value),
    })
}

fn reduce_numeric_values(
    values: &[Value],
    accumulator: &mut Option<NumericReduction>,
    multiply: bool,
    span: Span,
) -> Result<(), EvalError> {
    let overflow_context = if multiply { "product(...)" } else { "sum(...)" };
    for value in values {
        if let Value::Array(nested) = value {
            reduce_numeric_values(nested, accumulator, multiply, span)?;
            continue;
        }
        *accumulator = Some(match (*accumulator, value) {
            (None, Value::Integer(value)) => NumericReduction::Integer(*value),
            (None, Value::Real(value)) => NumericReduction::Real(*value),
            (Some(NumericReduction::Integer(lhs)), Value::Integer(rhs)) => {
                let value = if multiply {
                    lhs.checked_mul(*rhs)
                } else {
                    lhs.checked_add(*rhs)
                }
                .ok_or_else(|| integer_overflow_error(overflow_context, span))?;
                NumericReduction::Integer(value)
            }
            (Some(NumericReduction::Integer(lhs)), Value::Real(rhs)) => {
                NumericReduction::Real(if multiply {
                    lhs as f64 * rhs
                } else {
                    lhs as f64 + rhs
                })
            }
            (Some(NumericReduction::Real(lhs)), Value::Integer(rhs)) => {
                NumericReduction::Real(if multiply {
                    lhs * *rhs as f64
                } else {
                    lhs + *rhs as f64
                })
            }
            (Some(NumericReduction::Real(lhs)), Value::Real(rhs)) => {
                NumericReduction::Real(if multiply { lhs * rhs } else { lhs + rhs })
            }
            (None, other) => {
                return Err(EvalError::type_mismatch(
                    "numeric array element",
                    other.type_name(),
                    span,
                ));
            }
            (_, other) => {
                return Err(EvalError::type_mismatch(
                    "numeric array",
                    other.type_name(),
                    span,
                ));
            }
        });
    }
    Ok(())
}

fn array_dimensions(value: &Value, span: Span) -> Result<Vec<usize>, EvalError> {
    array_dimensions_at_depth(value, span, 0)
}

fn array_dimensions_at_depth(
    value: &Value,
    span: Span,
    depth: usize,
) -> Result<Vec<usize>, EvalError> {
    if depth >= DEFAULT_MATERIALIZED_RANK_BUDGET {
        return Err(EvalError::UnsupportedExpression {
            kind: format!(
                "array rank exceeds the constant-evaluation rank budget of \
                 {DEFAULT_MATERIALIZED_RANK_BUDGET}"
            ),
            span,
        });
    }
    let Value::Array(values) = value else {
        return Err(EvalError::type_mismatch("Array", value.type_name(), span));
    };
    let mut dimensions = vec![values.len()];
    let Some(first) = values.first() else {
        return Err(EvalError::UnsupportedExpression {
            kind: "empty materialized array requires retained shape metadata".to_string(),
            span,
        });
    };
    let nested = match first {
        Value::Array(_) => array_dimensions_at_depth(first, span, depth + 1)?,
        _ => Vec::new(),
    };
    for value in &values[1..] {
        let candidate = match value {
            Value::Array(_) => array_dimensions_at_depth(value, span, depth + 1)?,
            _ => Vec::new(),
        };
        if candidate != nested {
            return Err(EvalError::function_error(
                "array value is not rectangular".to_string(),
                span,
            ));
        }
    }
    dimensions.extend(nested);
    Ok(dimensions)
}

// fill: create array filled with value
fn eval_fill(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            actual: args.len(),
            span,
        });
    }

    let value = args[0].clone();

    // Handle multi-dimensional fill: fill(v, n1, n2, ...)
    let value_nodes =
        materialized_node_count(&value).ok_or_else(|| EvalError::UnsupportedExpression {
            kind: "fill value is beyond the constant-evaluation element budget".to_string(),
            span,
        })?;
    let dims = checked_materialized_dimensions(&args[1..], value_nodes, "fill", span)?;

    fn fill_recursive(value: &Value, dims: &[usize]) -> Value {
        if dims.is_empty() {
            value.clone()
        } else {
            let size = dims[0];
            let rest = &dims[1..];
            Value::Array((0..size).map(|_| fill_recursive(value, rest)).collect())
        }
    }

    Ok(fill_recursive(&value, &dims))
}

// zeros: create array of Integer zeros
fn eval_zeros(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            actual: 0,
            span,
        });
    }

    let dims = checked_materialized_dimensions(args, 1, "zeros", span)?;

    fn zeros_recursive(dims: &[usize]) -> Value {
        if dims.is_empty() {
            Value::Integer(0)
        } else {
            let size = dims[0];
            let rest = &dims[1..];
            Value::Array((0..size).map(|_| zeros_recursive(rest)).collect())
        }
    }

    Ok(zeros_recursive(&dims))
}

// ones: create array of Integer ones
fn eval_ones(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            actual: 0,
            span,
        });
    }

    let dims = checked_materialized_dimensions(args, 1, "ones", span)?;

    fn ones_recursive(dims: &[usize]) -> Value {
        if dims.is_empty() {
            Value::Integer(1)
        } else {
            let size = dims[0];
            let rest = &dims[1..];
            Value::Array((0..size).map(|_| ones_recursive(rest)).collect())
        }
    }

    Ok(ones_recursive(&dims))
}

// linspace: linearly spaced vector from x1 to x2 with n points
fn eval_linspace(args: &[Value], span: Span) -> Result<Value, EvalError> {
    check_arg_count(args, 3, span)?;
    let x1 = to_real(&args[0], span)?;
    let x2 = to_real(&args[1], span)?;
    let n = args[2]
        .as_integer()
        .ok_or_else(|| EvalError::type_mismatch("Integer", args[2].type_name(), span))?;

    if n < 2 {
        return Err(EvalError::function_error("linspace requires n >= 2", span));
    }

    let n_usize = usize::try_from(n).map_err(|_| EvalError::UnsupportedExpression {
        kind: format!("linspace length {n} is beyond the host index range"),
        span,
    })?;
    if n_usize
        .checked_add(1)
        .is_none_or(|nodes| nodes > DEFAULT_EVAL_BUDGET)
    {
        return Err(EvalError::UnsupportedExpression {
            kind: format!(
                "linspace length {n} is beyond the constant-evaluation retained-node budget"
            ),
            span,
        });
    }

    let step = (x2 - x1) / ((n_usize - 1) as f64);
    let mut out = Vec::with_capacity(n_usize);
    for i in 0..n_usize {
        out.push(Value::Real(x1 + step * (i as f64)));
    }
    if let Some(last) = out.last_mut() {
        *last = Value::Real(x2);
    }
    Ok(Value::Array(out))
}

// cat: concatenate arrays along a dimension
fn eval_cat(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let Some((dimension, arrays)) = args.split_first() else {
        return Err(EvalError::Internal {
            message: "shared cat signature validation admitted no dimension argument".to_string(),
        });
    };
    if arrays.len() < 2 {
        return Err(EvalError::Internal {
            message: "shared cat signature validation admitted fewer than two arrays".to_string(),
        });
    }
    let dim = dimension
        .as_integer()
        .ok_or_else(|| EvalError::type_mismatch("Integer", dimension.type_name(), span))?;

    if dim < 1 {
        return Err(EvalError::range_error(
            format!("cat dimension must be at least 1, got {dim}"),
            span,
        ));
    }

    if dim != 1 {
        return Err(EvalError::UnsupportedExpression {
            kind: format!("cat along dimension {dim} is not implemented by constant evaluation"),
            span,
        });
    }

    let mut expected_tail = None;
    let mut result_nodes = 1_usize;
    for arg in arrays {
        let dimensions = array_dimensions(arg, span)?;
        let tail = &dimensions[1..];
        if let Some(expected) = expected_tail.as_ref()
            && *expected != tail
        {
            return Err(EvalError::function_error(
                "cat arguments must have equal non-concatenated dimensions".to_string(),
                span,
            ));
        }
        expected_tail = Some(tail.to_vec());
        let nodes =
            materialized_node_count(arg).ok_or_else(|| EvalError::UnsupportedExpression {
                kind: "cat argument is beyond the constant-evaluation node budget".to_string(),
                span,
            })?;
        result_nodes = result_nodes
            .checked_add(nodes.saturating_sub(1))
            .filter(|nodes| *nodes <= DEFAULT_EVAL_BUDGET)
            .ok_or_else(|| EvalError::UnsupportedExpression {
                kind: "cat result is beyond the constant-evaluation node budget".to_string(),
                span,
            })?;
    }

    // Concatenate all arrays after shape and budget validation.
    let mut result = Vec::new();
    for arg in &args[1..] {
        let arr = arg
            .as_array()
            .ok_or_else(|| EvalError::type_mismatch("Array", arg.type_name(), span))?;
        result.extend(arr.iter().cloned());
    }

    Ok(Value::Array(result))
}

// Helper: check argument count
fn check_arg_count(args: &[Value], expected: usize, span: Span) -> Result<(), EvalError> {
    if args.len() != expected {
        Err(EvalError::WrongArgCount {
            expected,
            actual: args.len(),
            span,
        })
    } else {
        Ok(())
    }
}

// Helper: convert value to f64
fn to_real(v: &Value, span: Span) -> Result<f64, EvalError> {
    v.to_real()
        .ok_or_else(|| EvalError::type_mismatch("Real or Integer", v.type_name(), span))
}

fn integer_value(v: &Value, span: Span) -> Result<i64, EvalError> {
    v.as_integer()
        .ok_or_else(|| EvalError::type_mismatch("Integer", v.type_name(), span))
}

fn checked_real_to_i64(value: f64, span: Span, context: &str) -> Result<i64, EvalError> {
    if !value.is_finite() || value < i64::MIN as f64 || value >= i64::MAX as f64 {
        return Err(EvalError::range_error(
            format!("real value {value} is outside i64 range while evaluating {context}"),
            span,
        ));
    }
    Ok(value as i64)
}

fn integer_overflow_error(context: &str, span: Span) -> EvalError {
    EvalError::function_error(
        format!("compile-time integer overflow while evaluating {context}"),
        span,
    )
}

fn checked_materialized_dimensions(
    args: &[Value],
    payload_nodes: usize,
    builtin: &str,
    span: Span,
) -> Result<Vec<usize>, EvalError> {
    if args.len() > DEFAULT_MATERIALIZED_RANK_BUDGET {
        return Err(EvalError::UnsupportedExpression {
            kind: format!(
                "{builtin} rank exceeds the constant-evaluation rank budget of \
                 {DEFAULT_MATERIALIZED_RANK_BUDGET}"
            ),
            span,
        });
    }
    let mut dimensions = Vec::with_capacity(args.len());
    for argument in args {
        let Value::Integer(extent) = argument else {
            return Err(EvalError::type_mismatch(
                "Integer",
                argument.type_name(),
                span,
            ));
        };
        if *extent < 0 {
            return Err(EvalError::range_error(
                format!("{builtin} extent must be nonnegative, got {extent}"),
                span,
            ));
        }
        let extent = usize::try_from(*extent).map_err(|_| EvalError::UnsupportedExpression {
            kind: format!("{builtin} extent is beyond the host index range"),
            span,
        })?;
        dimensions.push(extent);
    }
    if !rectangular_shape_is_representable(&dimensions) {
        return Err(EvalError::UnsupportedExpression {
            kind: format!(
                "{builtin} has a zero extent before its final dimension; the \
                 compatibility value representation cannot retain that shape"
            ),
            span,
        });
    }
    if rectangular_materialized_node_count(&dimensions, payload_nodes)
        .is_none_or(|nodes| nodes > DEFAULT_EVAL_BUDGET)
    {
        return Err(EvalError::UnsupportedExpression {
            kind: format!("{builtin} result is beyond the constant-evaluation node budget"),
            span,
        });
    }
    Ok(dimensions)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sin() {
        let result = eval_builtin("sin", &[Value::Real(0.0)], Span::DUMMY).unwrap();
        assert_eq!(result.as_real().unwrap(), 0.0);
    }

    #[test]
    fn test_sqrt() {
        let result = eval_builtin("sqrt", &[Value::Real(4.0)], Span::DUMMY).unwrap();
        assert!((result.as_real().unwrap() - 2.0).abs() < 1e-10);
    }

    #[test]
    fn named_string_function_dispatch_remains_retired() {
        assert!(matches!(
            eval_builtin("String", &[Value::Integer(42)], Span::DUMMY),
            Err(EvalError::UnknownFunction { .. })
        ));
    }

    #[test]
    fn test_abs() {
        let result = eval_builtin("abs", &[Value::Integer(-5)], Span::DUMMY).unwrap();
        assert_eq!(result.as_integer().unwrap(), 5);

        let result = eval_builtin("abs", &[Value::Real(-2.5)], Span::DUMMY).unwrap();
        assert!((result.as_real().unwrap() - 2.5).abs() < 1e-10);
    }

    #[test]
    fn test_min_max() {
        let result =
            eval_builtin("min", &[Value::Real(3.0), Value::Real(5.0)], Span::DUMMY).unwrap();
        assert_eq!(result.as_real().unwrap(), 3.0);

        let result =
            eval_builtin("max", &[Value::Real(3.0), Value::Real(5.0)], Span::DUMMY).unwrap();
        assert_eq!(result.as_real().unwrap(), 5.0);

        // Integer version
        let result =
            eval_builtin("min", &[Value::Integer(3), Value::Integer(5)], Span::DUMMY).unwrap();
        assert_eq!(result.as_integer().unwrap(), 3);
    }

    #[test]
    fn test_sum() {
        let arr = Value::Array(vec![Value::Real(1.0), Value::Real(2.0), Value::Real(3.0)]);
        let result = eval_builtin("sum", &[arr], Span::DUMMY).unwrap();
        assert_eq!(result.as_real().unwrap(), 6.0);

        // Integer sum
        let arr = Value::Array(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
        ]);
        let result = eval_builtin("sum", &[arr], Span::DUMMY).unwrap();
        assert_eq!(result.as_integer().unwrap(), 6);

        let matrix = Value::Array(vec![
            Value::Array(vec![Value::Integer(1), Value::Integer(2)]),
            Value::Array(vec![Value::Integer(3), Value::Integer(4)]),
        ]);
        assert_eq!(
            eval_builtin("sum", std::slice::from_ref(&matrix), Span::DUMMY).unwrap(),
            Value::Integer(10)
        );
        assert_eq!(
            eval_builtin("product", &[matrix], Span::DUMMY).unwrap(),
            Value::Integer(24)
        );
        let ragged = Value::Array(vec![
            Value::Array(vec![Value::Integer(1)]),
            Value::Array(vec![Value::Integer(2), Value::Integer(3)]),
        ]);
        for reduction in ["sum", "product"] {
            assert!(matches!(
                eval_builtin(reduction, std::slice::from_ref(&ragged), Span::DUMMY),
                Err(EvalError::FunctionError { .. })
            ));
        }
    }

    #[test]
    fn test_zeros() {
        let result = eval_builtin("zeros", &[Value::Integer(3)], Span::DUMMY).unwrap();
        let arr = result.as_array().unwrap();
        assert_eq!(arr.len(), 3);
        assert!(arr.iter().all(|v| v.as_integer() == Some(0)));
    }

    #[test]
    fn test_zeros_2d() {
        let result = eval_builtin(
            "zeros",
            &[Value::Integer(2), Value::Integer(3)],
            Span::DUMMY,
        )
        .unwrap();
        let arr = result.as_array().unwrap();
        assert_eq!(arr.len(), 2);
        for row in arr {
            let row_arr = row.as_array().unwrap();
            assert_eq!(row_arr.len(), 3);
        }
    }

    #[test]
    fn test_fill() {
        let result = eval_builtin(
            "fill",
            &[Value::Integer(42), Value::Integer(3)],
            Span::DUMMY,
        )
        .unwrap();
        let arr = result.as_array().unwrap();
        assert_eq!(arr.len(), 3);
        assert!(arr.iter().all(|v| v.as_integer() == Some(42)));
    }

    #[test]
    fn test_cat() {
        let arr1 = Value::Array(vec![Value::Integer(1), Value::Integer(2)]);
        let arr2 = Value::Array(vec![Value::Integer(3), Value::Integer(4)]);
        assert!(matches!(
            eval_builtin("cat", &[Value::Integer(1), arr1.clone()], Span::DUMMY),
            Err(EvalError::WrongArgCount {
                expected: 3,
                actual: 2,
                ..
            })
        ));
        let result = eval_builtin("cat", &[Value::Integer(1), arr1, arr2], Span::DUMMY).unwrap();
        let arr = result.as_array().unwrap();
        assert_eq!(arr.len(), 4);
        assert_eq!(arr[0].as_integer(), Some(1));
        assert_eq!(arr[3].as_integer(), Some(4));

        let wide = Value::Array(vec![Value::Array(vec![Value::Integer(1)])]);
        let wider = Value::Array(vec![Value::Array(vec![
            Value::Integer(2),
            Value::Integer(3),
        ])]);
        assert!(matches!(
            eval_builtin("cat", &[Value::Integer(1), wide, wider], Span::DUMMY),
            Err(EvalError::FunctionError { .. })
        ));

        let large = Value::Array(vec![Value::Integer(0); DEFAULT_EVAL_BUDGET / 2 + 1]);
        assert!(matches!(
            eval_builtin(
                "cat",
                &[Value::Integer(1), large.clone(), large],
                Span::DUMMY
            ),
            Err(EvalError::UnsupportedExpression { .. })
        ));
    }

    #[test]
    fn test_size() {
        let arr = Value::Array(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
        ]);
        let result = eval_builtin("size", std::slice::from_ref(&arr), Span::DUMMY).unwrap();
        assert_eq!(result, Value::Array(vec![Value::Integer(3)]));

        let result = eval_builtin("size", &[arr, Value::Integer(1)], Span::DUMMY).unwrap();
        assert_eq!(result.as_integer().unwrap(), 3);

        let matrix = Value::Array(vec![
            Value::Array(vec![Value::Integer(1), Value::Integer(2)]),
            Value::Array(vec![Value::Integer(3), Value::Integer(4)]),
            Value::Array(vec![Value::Integer(5), Value::Integer(6)]),
        ]);
        assert_eq!(
            eval_builtin("size", std::slice::from_ref(&matrix), Span::DUMMY).unwrap(),
            Value::Array(vec![Value::Integer(3), Value::Integer(2)])
        );
        assert_eq!(
            eval_builtin("size", &[matrix.clone(), Value::Integer(2)], Span::DUMMY).unwrap(),
            Value::Integer(2)
        );
        let ragged = Value::Array(vec![
            Value::Array(vec![Value::Integer(1)]),
            Value::Array(vec![Value::Integer(2), Value::Integer(3)]),
        ]);
        assert!(matches!(
            eval_builtin("size", &[ragged], Span::DUMMY),
            Err(EvalError::FunctionError { .. })
        ));
    }

    #[test]
    fn untyped_empty_arrays_cannot_fabricate_size_or_rank() {
        for empty in [
            Value::Array(Vec::new()),
            Value::Array(vec![Value::Array(Vec::new())]),
        ] {
            for (operation, args) in [
                ("size", vec![empty.clone()]),
                ("size", vec![empty.clone(), Value::Integer(1)]),
                ("ndims", vec![empty.clone()]),
            ] {
                let error = eval_builtin(operation, &args, Span::DUMMY)
                    .expect_err("an empty value cannot prove its complete declared shape");
                assert!(
                    matches!(error, EvalError::UnsupportedExpression { .. }),
                    "unexpected {operation} error: {error}"
                );
            }
        }
    }

    #[test]
    fn test_integer() {
        let result = eval_builtin("integer", &[Value::Real(3.7)], Span::DUMMY).unwrap();
        assert_eq!(result.as_integer().unwrap(), 3);

        let result = eval_builtin("integer", &[Value::Real(-2.3)], Span::DUMMY).unwrap();
        assert_eq!(result.as_integer().unwrap(), -3);
    }

    #[test]
    fn test_floor_and_ceil_return_integer() {
        let floor_result = eval_builtin("floor", &[Value::Real(3.7)], Span::DUMMY).unwrap();
        assert_eq!(floor_result.as_integer(), Some(3));

        let ceil_result = eval_builtin("ceil", &[Value::Real(3.2)], Span::DUMMY).unwrap();
        assert_eq!(ceil_result.as_integer(), Some(4));
    }

    #[test]
    fn test_integer_out_of_range_real_returns_error() {
        let err = eval_builtin("integer", &[Value::Real(-1e40)], Span::DUMMY).unwrap_err();
        assert!(
            err.to_string()
                .contains("outside i64 range while evaluating integer(...)")
        );
    }

    #[test]
    fn integer_rejects_the_positive_i64_boundary_without_saturation() {
        let positive_boundary = 9_223_372_036_854_775_808.0_f64;
        assert!(matches!(
            eval_builtin("integer", &[Value::Real(positive_boundary)], Span::DUMMY),
            Err(EvalError::RangeError { .. })
        ));
        assert_eq!(
            eval_builtin(
                "integer",
                &[Value::Real(-9_223_372_036_854_775_808.0_f64)],
                Span::DUMMY
            )
            .unwrap(),
            Value::Integer(i64::MIN)
        );
    }

    #[test]
    fn unsupported_array_builtins_defer_and_materialization_is_bounded() {
        let vector = Value::Array(vec![Value::Integer(1)]);
        assert!(matches!(
            eval_builtin(
                "cat",
                &[Value::Integer(2), vector.clone(), vector],
                Span::DUMMY
            ),
            Err(EvalError::UnsupportedExpression { .. })
        ));

        let rank_three = Value::Array(vec![Value::Array(vec![Value::Array(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
        ])])]);
        assert_eq!(
            eval_builtin("size", &[rank_three, Value::Integer(3)], Span::DUMMY).unwrap(),
            Value::Integer(3)
        );

        assert!(matches!(
            eval_builtin("zeros", &[Value::Integer(-1)], Span::DUMMY),
            Err(EvalError::RangeError { .. })
        ));
        assert!(matches!(
            eval_builtin(
                "fill",
                &[
                    Value::Array(Vec::new()),
                    Value::Integer(DEFAULT_EVAL_BUDGET as i64 + 1)
                ],
                Span::DUMMY
            ),
            Err(EvalError::UnsupportedExpression { .. })
        ));
        for invalid_dimension in [-1, 0] {
            assert!(matches!(
                eval_builtin(
                    "cat",
                    &[
                        Value::Integer(invalid_dimension),
                        Value::Array(Vec::new()),
                        Value::Array(Vec::new())
                    ],
                    Span::DUMMY
                ),
                Err(EvalError::RangeError { .. })
            ));
        }
        assert!(matches!(
            eval_builtin(
                "cat",
                &[
                    Value::Integer(2),
                    Value::Array(Vec::new()),
                    Value::Array(Vec::new())
                ],
                Span::DUMMY
            ),
            Err(EvalError::UnsupportedExpression { .. })
        ));
        let unit_extents = vec![Value::Integer(1); DEFAULT_EVAL_BUDGET];
        assert!(matches!(
            eval_builtin("zeros", &unit_extents, Span::DUMMY),
            Err(EvalError::UnsupportedExpression { .. })
        ));
        let excessive_rank = vec![Value::Integer(1); DEFAULT_MATERIALIZED_RANK_BUDGET + 1];
        assert!(matches!(
            eval_builtin("ones", &excessive_rank, Span::DUMMY),
            Err(EvalError::UnsupportedExpression { .. })
        ));
        assert!(matches!(
            eval_builtin(
                "fill",
                &[Value::Integer(0), Value::Integer(0), Value::Integer(3)],
                Span::DUMMY
            ),
            Err(EvalError::UnsupportedExpression { .. })
        ));
        assert!(matches!(
            eval_builtin(
                "fill",
                &[Value::Integer(0), Value::Integer(50_000), Value::Integer(2)],
                Span::DUMMY
            ),
            Err(EvalError::UnsupportedExpression { .. })
        ));
    }

    #[test]
    fn test_sum_integer_overflow_returns_error() {
        let arr = Value::Array(vec![Value::Integer(i64::MAX), Value::Integer(1)]);
        let err = eval_builtin("sum", &[arr], Span::DUMMY).unwrap_err();
        assert!(
            err.to_string()
                .contains("compile-time integer overflow while evaluating sum(...)")
        );
    }

    #[test]
    fn empty_numeric_reductions_defer_until_element_type_is_retained() {
        let empty_real = eval_builtin("fill", &[Value::Real(0.0), Value::Integer(0)], Span::DUMMY)
            .expect("a zero-length Real fill is a valid empty array");
        let nested_empty = Value::Array(vec![Value::Array(Vec::new())]);

        for value in [empty_real, nested_empty] {
            for operation in ["sum", "product"] {
                let error = eval_builtin(operation, std::slice::from_ref(&value), Span::DUMMY)
                    .expect_err("an untyped empty reduction must stay with typed array ownership");
                assert!(
                    matches!(error, EvalError::UnsupportedExpression { .. }),
                    "unexpected {operation} error: {error}"
                );
            }
        }
    }

    #[test]
    fn test_abs_integer_overflow_returns_error() {
        let err = eval_builtin("abs", &[Value::Integer(i64::MIN)], Span::DUMMY).unwrap_err();
        assert!(
            err.to_string()
                .contains("compile-time integer overflow while evaluating abs(...)")
        );
    }

    #[test]
    fn test_ones() {
        let result = eval_builtin("ones", &[Value::Integer(3)], Span::DUMMY).unwrap();
        let arr = result.as_array().unwrap();
        assert_eq!(arr.len(), 3);
        assert!(arr.iter().all(|v| v.as_integer() == Some(1)));
    }

    #[test]
    fn test_linspace() {
        let result = eval_builtin(
            "linspace",
            &[Value::Real(0.0), Value::Real(1.0), Value::Integer(5)],
            Span::DUMMY,
        )
        .unwrap();
        let arr = result.as_array().unwrap();
        assert_eq!(arr.len(), 5);
        assert_eq!(arr[0].as_real(), Some(0.0));
        assert_eq!(arr[4].as_real(), Some(1.0));
        assert_eq!(arr[2].as_real(), Some(0.5));

        assert!(matches!(
            eval_builtin(
                "linspace",
                &[
                    Value::Real(0.0),
                    Value::Real(1.0),
                    Value::Integer(DEFAULT_EVAL_BUDGET as i64)
                ],
                Span::DUMMY
            ),
            Err(EvalError::UnsupportedExpression { .. })
        ));
    }

    #[test]
    fn test_div() {
        let result =
            eval_builtin("div", &[Value::Integer(7), Value::Integer(3)], Span::DUMMY).unwrap();
        assert_eq!(result.as_integer().unwrap(), 2);

        let result =
            eval_builtin("div", &[Value::Integer(-7), Value::Integer(3)], Span::DUMMY).unwrap();
        assert_eq!(result.as_integer().unwrap(), -2);

        let result =
            eval_builtin("div", &[Value::Real(-7.0), Value::Integer(3)], Span::DUMMY).unwrap();
        assert_eq!(result.as_real(), Some(-2.0));
    }

    #[test]
    fn quotient_builtins_preserve_integer_result_types_and_sign_rules() {
        let modulo =
            eval_builtin("mod", &[Value::Integer(-7), Value::Integer(3)], Span::DUMMY).unwrap();
        assert_eq!(modulo.as_integer(), Some(2));
        let remainder =
            eval_builtin("rem", &[Value::Integer(-7), Value::Integer(3)], Span::DUMMY).unwrap();
        assert_eq!(remainder.as_integer(), Some(-1));

        let real_modulo =
            eval_builtin("mod", &[Value::Real(-7.0), Value::Integer(3)], Span::DUMMY).unwrap();
        assert_eq!(real_modulo.as_real(), Some(2.0));
    }

    #[test]
    fn is_equal_spellings_are_not_evaluator_builtins() {
        // isEqual is an MSL library function, not an MLS predefined builtin
        // (§3.7). Dispatch must reach it only through a registered function
        // body; an unregistered call is an unknown function, never an
        // emulation keyed on the name's shape.
        for spelling in [
            "isEqual",
            "Modelica.Math.Vectors.isEqual",
            "Modelica.Math.Matrices.isEqual",
        ] {
            let v = Value::Array(vec![Value::Real(1.0)]);
            assert!(eval_builtin(spelling, &[v.clone(), v], Span::DUMMY).is_err());
        }
    }
}
