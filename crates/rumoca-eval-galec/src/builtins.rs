use std::collections::BTreeSet;

use crate::Value;
use crate::interpreter::{EvaluationError, Evaluator};

type Factorization = (Vec<Vec<f64>>, Vec<usize>, bool);

pub(super) fn lift_builtin(
    evaluator: &mut Evaluator<'_>,
    base: &str,
    arguments: Vec<Value>,
) -> Result<Value, EvaluationError> {
    let lengths = arguments
        .iter()
        .filter_map(|value| match value {
            Value::Array(values) => Some(values.len()),
            _ => None,
        })
        .collect::<BTreeSet<_>>();
    if lengths.len() != 1 {
        return Err(EvaluationError::Type("lifted builtin shape"));
    }
    let length = *lengths.first().expect("one lifted length");
    let mut result = Vec::with_capacity(length);
    for index in 0..length {
        let scalar_args = arguments
            .iter()
            .map(|argument| match argument {
                Value::Array(values) => values[index].clone(),
                scalar => scalar.clone(),
            })
            .collect();
        result.push(scalar_builtin(evaluator, base, scalar_args)?);
    }
    Ok(Value::Array(result))
}

pub(super) fn scalar_builtin(
    evaluator: &mut Evaluator<'_>,
    name: &str,
    arguments: Vec<Value>,
) -> Result<Value, EvaluationError> {
    match name {
        "minInteger" | "maxInteger" | "minReal" | "maxReal" | "posMinReal" | "epsReal" | "nan"
        | "minusInfinite" | "plusInfinite" | "euler" | "pi" | "isNaN" | "isInfinite"
        | "isFinite" | "real" | "integer" => {
            scalar_constant_or_conversion(evaluator, name, &arguments)
        }
        "safe_posdiv" | "safe_sqrt" | "safe_ln" | "safe_lg" | "safe_tan" | "safe_asin"
        | "safe_acos" => scalar_safe_math(name, &arguments),
        "roundDown" | "roundUp" | "roundHalfToEven" | "sign" | "absolute" | "fractional"
        | "sqrt" | "exp" | "ln" | "lg" | "sin" | "cos" | "tan" | "asin" | "acos" | "atan"
        | "sinh" | "cosh" | "tanh" => scalar_unary_math(name, &arguments),
        _ => scalar_binary_or_array(evaluator, name, &arguments),
    }
}

fn scalar_constant_or_conversion(
    evaluator: &mut Evaluator<'_>,
    name: &str,
    arguments: &[Value],
) -> Result<Value, EvaluationError> {
    Ok(match name {
        "minInteger" => Value::Integer(evaluator.integer_domain.min()),
        "maxInteger" => Value::Integer(evaluator.integer_domain.max()),
        "minReal" => Value::Real(-f64::MAX),
        "maxReal" => Value::Real(f64::MAX),
        "posMinReal" => Value::Real(f64::MIN_POSITIVE),
        "epsReal" => Value::Real(f64::EPSILON),
        "nan" => Value::Real(f64::NAN),
        "minusInfinite" => Value::Real(f64::NEG_INFINITY),
        "plusInfinite" => Value::Real(f64::INFINITY),
        "euler" => Value::Real(std::f64::consts::E),
        "pi" => Value::Real(std::f64::consts::PI),
        "isNaN" => Value::Boolean(builtin_real(arguments, 0)?.is_nan()),
        "isInfinite" => Value::Boolean(builtin_real(arguments, 0)?.is_infinite()),
        "isFinite" => Value::Boolean(builtin_real(arguments, 0)?.is_finite()),
        "real" => Value::Real(
            arguments
                .first()
                .and_then(Value::integer)
                .ok_or(EvaluationError::Type("real conversion"))? as f64,
        ),
        "integer" => {
            let value = builtin_real(arguments, 0)?;
            if value.is_nan() {
                evaluator.active_signals.insert("NAN".to_owned());
                Value::Integer(0)
            } else if value > evaluator.integer_domain.max() as f64
                || value < evaluator.integer_domain.min() as f64
            {
                evaluator.active_signals.insert("OVERFLOW".to_owned());
                Value::Integer(0)
            } else {
                Value::Integer(value.trunc() as i64)
            }
        }
        _ => return Err(EvaluationError::UnsupportedBuiltin(name.to_owned())),
    })
}

fn scalar_safe_math(name: &str, arguments: &[Value]) -> Result<Value, EvaluationError> {
    let value = builtin_real(arguments, 0)?;
    Ok(match name {
        "safe_posdiv" => {
            let denominator = builtin_real(arguments, 1)?;
            let epsilon = builtin_real(arguments, 2)?;
            Value::Real(
                if value.is_nan() || denominator.is_nan() || epsilon.is_nan() {
                    f64::NAN
                } else {
                    value / denominator.max(epsilon.max(f64::MIN_POSITIVE))
                },
            )
        }
        "safe_sqrt" => Value::Real(if value.is_nan() {
            f64::NAN
        } else {
            value.max(0.0).sqrt()
        }),
        "safe_ln" => Value::Real(if value.is_nan() {
            f64::NAN
        } else {
            value.max(0.0).ln()
        }),
        "safe_lg" => Value::Real(if value.is_nan() {
            f64::NAN
        } else {
            value.max(0.0).log10()
        }),
        "safe_tan" => Value::Real(if value >= std::f64::consts::FRAC_PI_2 {
            f64::INFINITY
        } else if value <= -std::f64::consts::FRAC_PI_2 {
            f64::NEG_INFINITY
        } else {
            value.tan()
        }),
        "safe_asin" => Value::Real(if value.is_nan() {
            f64::NAN
        } else {
            value.clamp(-1.0, 1.0).asin()
        }),
        "safe_acos" => Value::Real(if value.is_nan() {
            f64::NAN
        } else {
            value.clamp(-1.0, 1.0).acos()
        }),
        _ => return Err(EvaluationError::UnsupportedBuiltin(name.to_owned())),
    })
}

fn scalar_unary_math(name: &str, arguments: &[Value]) -> Result<Value, EvaluationError> {
    let value = builtin_real(arguments, 0)?;
    Ok(Value::Real(match name {
        "roundDown" => value.floor(),
        "roundUp" => value.ceil(),
        "roundHalfToEven" => value.round_ties_even(),
        "sign" => value.signum(),
        "absolute" => value.abs(),
        "fractional" => value.fract(),
        "sqrt" => value.sqrt(),
        "exp" => value.exp(),
        "ln" => value.ln(),
        "lg" => value.log10(),
        "sin" => value.sin(),
        "cos" => value.cos(),
        "tan" => value.tan(),
        "asin" => value.asin(),
        "acos" => value.acos(),
        "atan" => value.atan(),
        "sinh" => value.sinh(),
        "cosh" => value.cosh(),
        "tanh" => value.tanh(),
        _ => return Err(EvaluationError::UnsupportedBuiltin(name.to_owned())),
    }))
}

fn scalar_binary_or_array(
    evaluator: &Evaluator<'_>,
    name: &str,
    arguments: &[Value],
) -> Result<Value, EvaluationError> {
    Ok(match name {
        "atan2" => {
            let (y, x) = builtin_real_pair(arguments)?;
            Value::Real(y.atan2(x))
        }
        "min" => {
            let (a, b) = builtin_real_pair(arguments)?;
            Value::Real(a.min(b))
        }
        "max" => {
            let (a, b) = builtin_real_pair(arguments)?;
            Value::Real(a.max(b))
        }
        "imin" | "imax" => {
            let a = arguments
                .first()
                .and_then(Value::integer)
                .ok_or(EvaluationError::Type("Integer builtin argument"))?;
            let b = arguments
                .get(1)
                .and_then(Value::integer)
                .ok_or(EvaluationError::Type("Integer builtin argument"))?;
            Value::Integer(if name == "imin" { a.min(b) } else { a.max(b) })
        }
        "divisionTowardsZero" | "remainderTowardsZero" => {
            let a = arguments
                .first()
                .and_then(Value::integer)
                .ok_or(EvaluationError::Type("Integer builtin argument"))?;
            let b = arguments
                .get(1)
                .and_then(Value::integer)
                .ok_or(EvaluationError::Type("Integer builtin argument"))?;
            if b == 0 {
                return Err(EvaluationError::IntegerDivisionByZero);
            }
            return evaluator.checked_integer(Some(if name == "divisionTowardsZero" {
                a.checked_div(b).ok_or(EvaluationError::IntegerOverflow)?
            } else {
                a.checked_rem(b).ok_or(EvaluationError::IntegerOverflow)?
            }));
        }
        "realRemainderTowardsZero" => {
            let (a, b) = builtin_real_pair(arguments)?;
            Value::Real(a % b)
        }
        "hasNaN1D" | "hasNaN2D" => Value::Boolean(has_nan(
            arguments
                .first()
                .ok_or(EvaluationError::Type("array builtin argument"))?,
        )),
        _ => return Err(EvaluationError::UnsupportedBuiltin(name.to_owned())),
    })
}

fn builtin_real(arguments: &[Value], index: usize) -> Result<f64, EvaluationError> {
    arguments
        .get(index)
        .and_then(Value::real)
        .ok_or(EvaluationError::Type("Real builtin argument"))
}

fn builtin_real_pair(arguments: &[Value]) -> Result<(f64, f64), EvaluationError> {
    Ok((builtin_real(arguments, 0)?, builtin_real(arguments, 1)?))
}

pub(super) fn solve_linear_equations(
    evaluator: &mut Evaluator<'_>,
    arguments: Vec<Value>,
) -> Result<Value, EvaluationError> {
    let matrix = real_matrix(&arguments[0], "solveLinearEquations")?;
    let rhs = real_vector(&arguments[1], "solveLinearEquations")?;
    let (lu, pivots, singular) = factorize(matrix)?;
    let (solution, solve_failed) = solve_lu(&lu, &pivots, &rhs)?;
    if singular || solve_failed {
        evaluator
            .active_signals
            .insert("SOLVE_LINEAR_EQUATIONS_FAILED".to_owned());
    }
    Ok(Value::Array(
        solution.into_iter().map(Value::Real).collect(),
    ))
}

pub(super) fn lu_factorize_builtin(
    evaluator: &mut Evaluator<'_>,
    arguments: Vec<Value>,
) -> Result<Vec<Value>, EvaluationError> {
    let matrix = real_matrix(&arguments[0], "luFactorize")?;
    let (lu, pivots, singular) = factorize(matrix)?;
    if singular {
        evaluator
            .active_signals
            .insert("SOLVE_LINEAR_EQUATIONS_FAILED".to_owned());
    }
    Ok(vec![
        matrix_value(lu),
        Value::Array(
            pivots
                .into_iter()
                .map(|pivot| Value::Integer(i64::try_from(pivot + 1).unwrap_or(i64::MAX)))
                .collect(),
        ),
    ])
}

pub(super) fn lu_solve_builtin(
    evaluator: &mut Evaluator<'_>,
    arguments: Vec<Value>,
) -> Result<Value, EvaluationError> {
    let lu = real_matrix(&arguments[0], "luSolve")?;
    let pivots = integer_vector(&arguments[1], "luSolve")?
        .into_iter()
        .map(|pivot| {
            pivot
                .checked_sub(1)
                .and_then(|pivot| usize::try_from(pivot).ok())
                .ok_or(EvaluationError::InvalidBuiltinArgument {
                    name: "luSolve",
                    detail: "pivot is not a positive index",
                })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let rhs = real_vector(&arguments[2], "luSolve")?;
    let (solution, failed) = solve_lu(&lu, &pivots, &rhs)?;
    if failed {
        evaluator
            .active_signals
            .insert("SOLVE_LINEAR_EQUATIONS_FAILED".to_owned());
    }
    Ok(Value::Array(
        solution.into_iter().map(Value::Real).collect(),
    ))
}

fn factorize(mut matrix: Vec<Vec<f64>>) -> Result<Factorization, EvaluationError> {
    let n = matrix.len();
    if n == 0 || matrix.iter().any(|row| row.len() != n) {
        return Err(EvaluationError::InvalidBuiltinArgument {
            name: "luFactorize",
            detail: "matrix must be non-empty and square",
        });
    }
    let mut pivots = (0..n).collect::<Vec<_>>();
    let mut singular = false;
    for column in 0..n {
        let pivot = (column..n)
            .max_by(|&lhs, &rhs| {
                matrix[lhs][column]
                    .abs()
                    .total_cmp(&matrix[rhs][column].abs())
            })
            .expect("non-empty pivot range");
        matrix.swap(column, pivot);
        pivots.swap(column, pivot);
        let diagonal = matrix[column][column];
        if diagonal == 0.0 || diagonal.is_nan() {
            matrix[column][column] = f64::NAN;
            singular = true;
            continue;
        }
        for row in column + 1..n {
            matrix[row][column] /= diagonal;
            for inner in column + 1..n {
                matrix[row][inner] -= matrix[row][column] * matrix[column][inner];
            }
        }
    }
    Ok((matrix, pivots, singular))
}

fn solve_lu(
    lu: &[Vec<f64>],
    pivots: &[usize],
    rhs: &[f64],
) -> Result<(Vec<f64>, bool), EvaluationError> {
    let n = lu.len();
    if n == 0
        || lu.iter().any(|row| row.len() != n)
        || pivots.len() != n
        || rhs.len() != n
        || pivots.iter().any(|pivot| *pivot >= n)
    {
        return Err(EvaluationError::InvalidBuiltinArgument {
            name: "luSolve",
            detail: "matrix, pivots, and right-hand side have incompatible shapes",
        });
    }
    let mut solution = vec![0.0; n];
    for row in 0..n {
        solution[row] = rhs[pivots[row]];
        for column in 0..row {
            solution[row] -= lu[row][column] * solution[column];
        }
    }
    let mut failed = false;
    for row in (0..n).rev() {
        for column in row + 1..n {
            solution[row] -= lu[row][column] * solution[column];
        }
        solution[row] /= lu[row][row];
        failed |= solution[row].is_nan();
    }
    Ok((solution, failed))
}

pub(super) fn interpolation_1d(arguments: Vec<Value>) -> Result<Value, EvaluationError> {
    let x = arguments[0]
        .real()
        .ok_or(EvaluationError::Type("interpolation1D x"))?;
    let axis = real_vector(&arguments[1], "interpolation1D")?;
    let count = interpolation_count(&arguments[2], axis.len(), "interpolation1D")?;
    let values = real_vector(&arguments[3], "interpolation1D")?;
    if values.len() < count {
        return invalid_interpolation("interpolation1D");
    }
    let mode = interpolation_options(&arguments[4..6], "interpolation1D")?;
    Ok(Value::Real(interpolate_axis(
        x,
        &axis[..count],
        &values[..count],
        mode,
    )?))
}

pub(super) fn interpolation_2d(arguments: Vec<Value>) -> Result<Value, EvaluationError> {
    let x1 = arguments[0]
        .real()
        .ok_or(EvaluationError::Type("interpolation2D x1"))?;
    let x2 = arguments[1]
        .real()
        .ok_or(EvaluationError::Type("interpolation2D x2"))?;
    let axis1 = real_vector(&arguments[2], "interpolation2D")?;
    let n1 = interpolation_count(&arguments[3], axis1.len(), "interpolation2D")?;
    let axis2 = real_vector(&arguments[4], "interpolation2D")?;
    let n2 = interpolation_count(&arguments[5], axis2.len(), "interpolation2D")?;
    let values = real_matrix(&arguments[6], "interpolation2D")?;
    if values.len() < n1 || values.iter().take(n1).any(|row| row.len() < n2) {
        return invalid_interpolation("interpolation2D");
    }
    let mode = interpolation_options(&arguments[7..9], "interpolation2D")?;
    let along_second = values
        .iter()
        .take(n1)
        .map(|row| interpolate_axis(x2, &axis2[..n2], &row[..n2], mode))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Value::Real(interpolate_axis(
        x1,
        &axis1[..n1],
        &along_second,
        mode,
    )?))
}

pub(super) fn interpolation_3d(arguments: Vec<Value>) -> Result<Value, EvaluationError> {
    let x1 = real_argument(&arguments[0], "interpolation3D")?;
    let x2 = real_argument(&arguments[1], "interpolation3D")?;
    let x3 = real_argument(&arguments[2], "interpolation3D")?;
    let axis1 = real_vector(&arguments[3], "interpolation3D")?;
    let n1 = interpolation_count(&arguments[4], axis1.len(), "interpolation3D")?;
    let axis2 = real_vector(&arguments[5], "interpolation3D")?;
    let n2 = interpolation_count(&arguments[6], axis2.len(), "interpolation3D")?;
    let axis3 = real_vector(&arguments[7], "interpolation3D")?;
    let n3 = interpolation_count(&arguments[8], axis3.len(), "interpolation3D")?;
    let values = real_array3(&arguments[9], "interpolation3D")?;
    let mode = interpolation_options(&arguments[10..12], "interpolation3D")?;
    if values.len() < n1
        || values
            .iter()
            .take(n1)
            .any(|plane| plane.len() < n2 || plane.iter().take(n2).any(|row| row.len() < n3))
    {
        return invalid_interpolation("interpolation3D");
    }
    let mut along_first = Vec::with_capacity(n1);
    for plane in values.iter().take(n1) {
        let along_second = plane
            .iter()
            .take(n2)
            .map(|row| interpolate_axis(x3, &axis3[..n3], &row[..n3], mode))
            .collect::<Result<Vec<_>, _>>()?;
        along_first.push(interpolate_axis(x2, &axis2[..n2], &along_second, mode)?);
    }
    Ok(Value::Real(interpolate_axis(
        x1,
        &axis1[..n1],
        &along_first,
        mode,
    )?))
}

#[derive(Clone, Copy)]
struct InterpolationMode {
    linear: bool,
    linear_extrapolation: bool,
}

fn interpolation_options(
    values: &[Value],
    name: &'static str,
) -> Result<InterpolationMode, EvaluationError> {
    let interpolation = values[0]
        .integer()
        .ok_or(EvaluationError::Type("interpolation mode"))?;
    let extrapolation = values[1]
        .integer()
        .ok_or(EvaluationError::Type("extrapolation mode"))?;
    if !matches!(interpolation, 1 | 2) || !matches!(extrapolation, 1 | 2) {
        return invalid_interpolation(name);
    }
    Ok(InterpolationMode {
        linear: interpolation == 2,
        linear_extrapolation: extrapolation == 2,
    })
}

fn interpolate_axis(
    x: f64,
    axis: &[f64],
    values: &[f64],
    mode: InterpolationMode,
) -> Result<f64, EvaluationError> {
    if axis.len() < 2
        || values.len() != axis.len()
        || axis.windows(2).any(|pair| pair[0] >= pair[1])
    {
        return invalid_interpolation("interpolation");
    }
    let last = axis.len() - 1;
    if x <= axis[0] && !mode.linear_extrapolation {
        return Ok(values[0]);
    }
    if x >= axis[last] && !mode.linear_extrapolation {
        return Ok(values[last]);
    }
    let lower = if x <= axis[0] {
        0
    } else if x >= axis[last] {
        last - 1
    } else {
        axis.partition_point(|point| *point <= x) - 1
    };
    if !mode.linear && x < axis[last] {
        return Ok(values[lower]);
    }
    let weight = (x - axis[lower]) / (axis[lower + 1] - axis[lower]);
    Ok(values[lower] + weight * (values[lower + 1] - values[lower]))
}

fn interpolation_count(
    value: &Value,
    available: usize,
    name: &'static str,
) -> Result<usize, EvaluationError> {
    let count = value
        .integer()
        .and_then(|value| usize::try_from(value).ok())
        .ok_or(EvaluationError::Type("interpolation count"))?;
    if count < 2 || count > available {
        return invalid_interpolation(name);
    }
    Ok(count)
}

fn invalid_interpolation<T>(name: &'static str) -> Result<T, EvaluationError> {
    Err(EvaluationError::InvalidBuiltinArgument {
        name,
        detail: "inconsistent table, grid, count, or option",
    })
}

fn real_argument(value: &Value, name: &'static str) -> Result<f64, EvaluationError> {
    value.real().ok_or(EvaluationError::InvalidBuiltinArgument {
        name,
        detail: "expected Real scalar",
    })
}

fn real_vector(value: &Value, name: &'static str) -> Result<Vec<f64>, EvaluationError> {
    let Value::Array(values) = value else {
        return invalid_interpolation(name);
    };
    values
        .iter()
        .map(|value| real_argument(value, name))
        .collect()
}

fn integer_vector(value: &Value, name: &'static str) -> Result<Vec<i64>, EvaluationError> {
    let Value::Array(values) = value else {
        return invalid_interpolation(name);
    };
    values
        .iter()
        .map(|value| {
            value
                .integer()
                .ok_or(EvaluationError::InvalidBuiltinArgument {
                    name,
                    detail: "expected Integer vector",
                })
        })
        .collect()
}

fn real_matrix(value: &Value, name: &'static str) -> Result<Vec<Vec<f64>>, EvaluationError> {
    let Value::Array(rows) = value else {
        return invalid_interpolation(name);
    };
    rows.iter().map(|row| real_vector(row, name)).collect()
}

fn real_array3(value: &Value, name: &'static str) -> Result<Vec<Vec<Vec<f64>>>, EvaluationError> {
    let Value::Array(planes) = value else {
        return invalid_interpolation(name);
    };
    planes
        .iter()
        .map(|plane| real_matrix(plane, name))
        .collect()
}

fn matrix_value(matrix: Vec<Vec<f64>>) -> Value {
    Value::Array(
        matrix
            .into_iter()
            .map(|row| Value::Array(row.into_iter().map(Value::Real).collect()))
            .collect(),
    )
}

fn has_nan(value: &Value) -> bool {
    match value {
        Value::Real(value) => value.is_nan(),
        Value::Array(values) => values.iter().any(has_nan),
        _ => false,
    }
}
