//! Built-in function implementations for constant evaluation.
//!
//! This module provides implementations of Modelica built-in functions
//! that can be evaluated at compile time (MLS §3.7).
//!
//! Implemented functions:
//! - Mathematical functions (MLS §3.7.3): sin, cos, tan, asin, acos, atan, etc.
//! - Array functions (MLS §10.3): size, ndims, sum, product, fill, zeros, ones, linspace, cat
//! - Conversion functions (MLS §3.7.2): integer, div, mod, rem

use crate::errors::EvalError;
use crate::value::Value;
use rumoca_core::Span;

/// Evaluate a built-in function call.
pub fn eval_builtin(name: &str, args: &[Value], span: Span) -> Result<Value, EvalError> {
    match name {
        // Math functions (single argument)
        "sin" => eval_math_1(args, f64::sin, span),
        "cos" => eval_math_1(args, f64::cos, span),
        "tan" => eval_math_1(args, f64::tan, span),
        "asin" => eval_math_1(args, f64::asin, span),
        "acos" => eval_math_1(args, f64::acos, span),
        "atan" => eval_math_1(args, f64::atan, span),
        "sinh" => eval_math_1(args, f64::sinh, span),
        "cosh" => eval_math_1(args, f64::cosh, span),
        "tanh" => eval_math_1(args, f64::tanh, span),
        "exp" => eval_math_1(args, f64::exp, span),
        "log" => eval_math_1(args, f64::ln, span),
        "log10" => eval_math_1(args, f64::log10, span),
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

        // String functions
        "String" => eval_string_convert(args, span),

        // Array comparison functions (MLS library functions used for structural parameters)
        "isEqual" | "Modelica.Math.Vectors.isEqual" | "Modelica.Math.Matrices.isEqual" => {
            eval_is_equal(args, span)
        }

        _ => Err(EvalError::unknown_function(name, span)),
    }
}

/// Check if a function name is a known built-in.
pub fn is_builtin(name: &str) -> bool {
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
            | "atan2"
            | "min"
            | "max"
            | "mod"
            | "rem"
            | "size"
            | "ndims"
            | "sum"
            | "product"
            | "fill"
            | "zeros"
            | "ones"
            | "linspace"
            | "cat"
            | "integer"
            | "div"
            | "String"
            | "isEqual"
            | "Modelica.Math.Vectors.isEqual"
            | "Modelica.Math.Matrices.isEqual"
    )
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
        Value::Integer(x) => Ok(Value::Integer(x.abs())),
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
        Value::Real(x) => {
            let s = if *x > 0.0 {
                1.0
            } else if *x < 0.0 {
                -1.0
            } else {
                0.0
            };
            Ok(Value::Real(s))
        }
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
        Value::Real(x) => Ok(Value::Integer(x.floor() as i64)),
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
        Value::Real(x) => Ok(Value::Integer(x.ceil() as i64)),
        Value::Integer(x) => Ok(Value::Integer(*x)),
        other => Err(EvalError::type_mismatch(
            "Real or Integer",
            other.type_name(),
            span,
        )),
    }
}

// min/max: works on two values or array
fn eval_min_max(args: &[Value], is_min: bool, span: Span) -> Result<Value, EvalError> {
    if args.len() == 1 {
        // Array version
        let arr = args[0]
            .as_array()
            .ok_or_else(|| EvalError::type_mismatch("Array", args[0].type_name(), span))?;
        if arr.is_empty() {
            return Err(EvalError::function_error("min/max on empty array", span));
        }

        // Check if all Integer
        let all_int = arr.iter().all(|v| matches!(v, Value::Integer(_)));
        if all_int {
            // SAFETY: all_int guard above guarantees as_integer() succeeds
            let values: Vec<i64> = arr.iter().map(|v| v.as_integer().unwrap()).collect();
            // SAFETY: arr.is_empty() check above guarantees min/max returns Some
            let result = if is_min {
                *values.iter().min().unwrap()
            } else {
                *values.iter().max().unwrap()
            };
            return Ok(Value::Integer(result));
        }

        // Convert to Real
        let values: Vec<f64> = arr
            .iter()
            .map(|v| to_real(v, span))
            .collect::<Result<_, _>>()?;
        // SAFETY: arr.is_empty() check above guarantees reduce returns Some
        let result = if is_min {
            values.iter().copied().reduce(f64::min).unwrap()
        } else {
            values.iter().copied().reduce(f64::max).unwrap()
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
        let result = if is_min { x.min(y) } else { x.max(y) };
        Ok(Value::Real(result))
    }
}

// mod: x - floor(x/y) * y
fn eval_mod(args: &[Value], span: Span) -> Result<Value, EvalError> {
    check_arg_count(args, 2, span)?;
    let x = to_real(&args[0], span)?;
    let y = to_real(&args[1], span)?;
    if y == 0.0 {
        return Err(EvalError::DivisionByZero { span });
    }
    Ok(Value::Real(x - (x / y).floor() * y))
}

// rem: x - div(x,y) * y (truncated division)
fn eval_rem(args: &[Value], span: Span) -> Result<Value, EvalError> {
    check_arg_count(args, 2, span)?;
    let x = to_real(&args[0], span)?;
    let y = to_real(&args[1], span)?;
    if y == 0.0 {
        return Err(EvalError::DivisionByZero { span });
    }
    Ok(Value::Real(x - (x / y).trunc() * y))
}

// div: integer division truncated toward zero
fn eval_div(args: &[Value], span: Span) -> Result<Value, EvalError> {
    check_arg_count(args, 2, span)?;
    match (&args[0], &args[1]) {
        (Value::Integer(x), Value::Integer(y)) => {
            if *y == 0 {
                return Err(EvalError::DivisionByZero { span });
            }
            Ok(Value::Integer(x / y))
        }
        (a, b) => {
            let x = to_real(a, span)?;
            let y = to_real(b, span)?;
            if y == 0.0 {
                return Err(EvalError::DivisionByZero { span });
            }
            Ok(Value::Integer((x / y).trunc() as i64))
        }
    }
}

// integer: convert Real to Integer by floor (MLS §3.7.2)
fn eval_integer(args: &[Value], span: Span) -> Result<Value, EvalError> {
    check_arg_count(args, 1, span)?;
    match &args[0] {
        Value::Integer(x) => Ok(Value::Integer(*x)),
        Value::Real(x) => Ok(Value::Integer(x.floor() as i64)),
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

    let arr = args[0]
        .as_array()
        .ok_or_else(|| EvalError::type_mismatch("Array", args[0].type_name(), span))?;

    if args.len() == 1 {
        // Return size as integer (1D case)
        Ok(Value::Integer(arr.len() as i64))
    } else {
        // Return size of specific dimension
        let dim = args[1]
            .as_integer()
            .ok_or_else(|| EvalError::type_mismatch("Integer", args[1].type_name(), span))?;
        if dim == 1 {
            Ok(Value::Integer(arr.len() as i64))
        } else if dim > 1 {
            // For nested arrays, get the size of the nested dimension
            get_nested_dim_size(arr, dim, span)
        } else {
            Err(EvalError::function_error("dimension must be >= 1", span))
        }
    }
}

// ndims: number of dimensions
fn eval_ndims(args: &[Value], span: Span) -> Result<Value, EvalError> {
    check_arg_count(args, 1, span)?;
    fn count_dims(v: &Value) -> i64 {
        match v {
            Value::Array(arr) => {
                if let Some(first) = arr.first() {
                    1 + count_dims(first)
                } else {
                    1
                }
            }
            _ => 0,
        }
    }
    Ok(Value::Integer(count_dims(&args[0])))
}

// sum: sum of array elements
fn eval_sum(args: &[Value], span: Span) -> Result<Value, EvalError> {
    check_arg_count(args, 1, span)?;
    let arr = args[0]
        .as_array()
        .ok_or_else(|| EvalError::type_mismatch("Array", args[0].type_name(), span))?;

    // Check if all Integer
    let all_int = arr.iter().all(|v| matches!(v, Value::Integer(_)));
    if all_int {
        // SAFETY: all_int guard above guarantees as_integer() succeeds
        let sum: i64 = arr.iter().map(|v| v.as_integer().unwrap()).sum();
        return Ok(Value::Integer(sum));
    }

    let mut sum = 0.0;
    for v in arr {
        sum += to_real(v, span)?;
    }
    Ok(Value::Real(sum))
}

// product: product of array elements
fn eval_product(args: &[Value], span: Span) -> Result<Value, EvalError> {
    check_arg_count(args, 1, span)?;
    let arr = args[0]
        .as_array()
        .ok_or_else(|| EvalError::type_mismatch("Array", args[0].type_name(), span))?;

    // Check if all Integer
    let all_int = arr.iter().all(|v| matches!(v, Value::Integer(_)));
    if all_int {
        // SAFETY: all_int guard above guarantees as_integer() succeeds
        let prod: i64 = arr.iter().map(|v| v.as_integer().unwrap()).product();
        return Ok(Value::Integer(prod));
    }

    let mut prod = 1.0;
    for v in arr {
        prod *= to_real(v, span)?;
    }
    Ok(Value::Real(prod))
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
    let dims: Vec<usize> = args[1..]
        .iter()
        .map(|a| {
            a.as_integer()
                .ok_or_else(|| EvalError::type_mismatch("Integer", a.type_name(), span))
                .map(|v| v as usize)
        })
        .collect::<Result<_, _>>()?;

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

    let dims: Vec<usize> = args
        .iter()
        .map(|a| {
            a.as_integer()
                .ok_or_else(|| EvalError::type_mismatch("Integer", a.type_name(), span))
                .map(|v| v as usize)
        })
        .collect::<Result<_, _>>()?;

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

    let dims: Vec<usize> = args
        .iter()
        .map(|a| {
            a.as_integer()
                .ok_or_else(|| EvalError::type_mismatch("Integer", a.type_name(), span))
                .map(|v| v as usize)
        })
        .collect::<Result<_, _>>()?;

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

    let n_usize = n as usize;
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
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            actual: args.len(),
            span,
        });
    }

    let dim = args[0]
        .as_integer()
        .ok_or_else(|| EvalError::type_mismatch("Integer", args[0].type_name(), span))?
        as usize;

    if dim != 1 {
        return Err(EvalError::function_error(
            "only cat with dimension 1 is currently supported",
            span,
        ));
    }

    // Concatenate all arrays
    let mut result = Vec::new();
    for arg in &args[1..] {
        let arr = arg
            .as_array()
            .ok_or_else(|| EvalError::type_mismatch("Array", arg.type_name(), span))?;
        result.extend(arr.iter().cloned());
    }

    Ok(Value::Array(result))
}

// String: convert value to string
fn eval_string_convert(args: &[Value], span: Span) -> Result<Value, EvalError> {
    check_arg_count(args, 1, span)?;
    Ok(Value::String(args[0].to_string()))
}

/// isEqual: Compare two arrays/matrices for numerical equality.
///
/// MLS library function: Modelica.Math.{Vectors,Matrices}.isEqual
/// Signature: isEqual(v1, v2, eps=0) -> Boolean
///
/// Returns true if:
/// - Both arrays have the same shape (dimensions)
/// - All corresponding elements differ by at most `eps`
fn eval_is_equal(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            actual: args.len(),
            span,
        });
    }

    let eps = if args.len() == 3 {
        to_real(&args[2], span)?
    } else {
        0.0
    };

    Ok(Value::Bool(values_equal(&args[0], &args[1], eps)))
}

/// Recursively compare two values for equality within tolerance.
fn values_equal(v1: &Value, v2: &Value, eps: f64) -> bool {
    match (v1, v2) {
        // Array comparison: must have same length and all elements equal
        (Value::Array(arr1), Value::Array(arr2)) => {
            if arr1.len() != arr2.len() {
                return false;
            }
            arr1.iter()
                .zip(arr2.iter())
                .all(|(a, b)| values_equal(a, b, eps))
        }
        // Real comparison with tolerance
        (Value::Real(r1), Value::Real(r2)) => (r1 - r2).abs() <= eps,
        // Integer comparison (exact, or convert to real if eps > 0)
        (Value::Integer(i1), Value::Integer(i2)) => {
            if eps == 0.0 {
                i1 == i2
            } else {
                ((*i1 as f64) - (*i2 as f64)).abs() <= eps
            }
        }
        // Mixed Integer/Real comparison
        (Value::Integer(i), Value::Real(r)) | (Value::Real(r), Value::Integer(i)) => {
            ((*i as f64) - r).abs() <= eps
        }
        // Boolean comparison (exact)
        (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
        // String comparison (exact)
        (Value::String(s1), Value::String(s2)) => s1 == s2,
        // Different types are not equal
        _ => false,
    }
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

// Helper: get nested array dimension size (reduces nesting in eval_size)
fn get_nested_dim_size(arr: &[Value], dim: i64, span: Span) -> Result<Value, EvalError> {
    let Some(first) = arr.first() else {
        return Err(EvalError::function_error(
            format!("dimension {} not available", dim),
            span,
        ));
    };
    let Some(inner) = first.as_array() else {
        return Err(EvalError::function_error(
            format!("dimension {} not available", dim),
            span,
        ));
    };
    if dim == 2 {
        return Ok(Value::Integer(inner.len() as i64));
    }
    Err(EvalError::function_error(
        format!("dimension {} not available", dim),
        span,
    ))
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
        let result = eval_builtin("cat", &[Value::Integer(1), arr1, arr2], Span::DUMMY).unwrap();
        let arr = result.as_array().unwrap();
        assert_eq!(arr.len(), 4);
        assert_eq!(arr[0].as_integer(), Some(1));
        assert_eq!(arr[3].as_integer(), Some(4));
    }

    #[test]
    fn test_size() {
        let arr = Value::Array(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
        ]);
        let result = eval_builtin("size", std::slice::from_ref(&arr), Span::DUMMY).unwrap();
        assert_eq!(result.as_integer().unwrap(), 3);

        let result = eval_builtin("size", &[arr, Value::Integer(1)], Span::DUMMY).unwrap();
        assert_eq!(result.as_integer().unwrap(), 3);
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
    }

    #[test]
    fn test_div() {
        let result =
            eval_builtin("div", &[Value::Integer(7), Value::Integer(3)], Span::DUMMY).unwrap();
        assert_eq!(result.as_integer().unwrap(), 2);

        let result =
            eval_builtin("div", &[Value::Integer(-7), Value::Integer(3)], Span::DUMMY).unwrap();
        assert_eq!(result.as_integer().unwrap(), -2);
    }

    #[test]
    fn test_is_equal_vectors() {
        // Equal vectors
        let v1 = Value::Array(vec![Value::Real(0.0), Value::Real(1.0), Value::Real(1.0)]);
        let v2 = Value::Array(vec![Value::Real(0.0), Value::Real(1.0), Value::Real(1.0)]);
        let result = eval_builtin("isEqual", &[v1, v2], Span::DUMMY).unwrap();
        assert_eq!(result.as_bool(), Some(true));

        // Different vectors
        let v1 = Value::Array(vec![Value::Real(0.0), Value::Real(1.0), Value::Real(1.0)]);
        let v2 = Value::Array(vec![Value::Real(0.0), Value::Real(1.0), Value::Real(0.0)]);
        let result = eval_builtin("isEqual", &[v1, v2], Span::DUMMY).unwrap();
        assert_eq!(result.as_bool(), Some(false));

        // Different lengths
        let v1 = Value::Array(vec![Value::Real(0.0), Value::Real(1.0)]);
        let v2 = Value::Array(vec![Value::Real(0.0), Value::Real(1.0), Value::Real(1.0)]);
        let result = eval_builtin("isEqual", &[v1, v2], Span::DUMMY).unwrap();
        assert_eq!(result.as_bool(), Some(false));
    }

    #[test]
    fn test_is_equal_with_tolerance() {
        let v1 = Value::Array(vec![Value::Real(1.0), Value::Real(2.0)]);
        let v2 = Value::Array(vec![Value::Real(1.001), Value::Real(2.001)]);

        // Without tolerance: not equal
        let result = eval_builtin("isEqual", &[v1.clone(), v2.clone()], Span::DUMMY).unwrap();
        assert_eq!(result.as_bool(), Some(false));

        // With tolerance: equal
        let result = eval_builtin("isEqual", &[v1, v2, Value::Real(0.01)], Span::DUMMY).unwrap();
        assert_eq!(result.as_bool(), Some(true));
    }

    #[test]
    fn test_is_equal_matrices() {
        // 2x2 matrix comparison (nested arrays)
        let m1 = Value::Array(vec![
            Value::Array(vec![Value::Real(1.0), Value::Real(2.0)]),
            Value::Array(vec![Value::Real(3.0), Value::Real(4.0)]),
        ]);
        let m2 = Value::Array(vec![
            Value::Array(vec![Value::Real(1.0), Value::Real(2.0)]),
            Value::Array(vec![Value::Real(3.0), Value::Real(4.0)]),
        ]);
        let result = eval_builtin("isEqual", &[m1, m2], Span::DUMMY).unwrap();
        assert_eq!(result.as_bool(), Some(true));
    }

    #[test]
    fn test_is_equal_qualified_name() {
        // Test with qualified function names (as used in MSL)
        let v1 = Value::Array(vec![Value::Integer(0), Value::Integer(1)]);
        let v2 = Value::Array(vec![Value::Integer(0), Value::Integer(1)]);

        let result = eval_builtin(
            "Modelica.Math.Vectors.isEqual",
            &[v1.clone(), v2.clone()],
            Span::DUMMY,
        )
        .unwrap();
        assert_eq!(result.as_bool(), Some(true));

        let result =
            eval_builtin("Modelica.Math.Matrices.isEqual", &[v1, v2], Span::DUMMY).unwrap();
        assert_eq!(result.as_bool(), Some(true));
    }
}
