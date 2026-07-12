//! Matrix-shaped evaluation: matrix literals, products, elementwise and
//! scalar-broadcast matrix arithmetic, and matrix-valued builtins.

use super::*;

pub(super) fn transpose_matrix<T: SimFloat>(matrix: Vec<Vec<T>>) -> Vec<Vec<T>> {
    let rows = matrix.len();
    let Some(first_row) = matrix.first() else {
        return Vec::new();
    };
    let cols = first_row.len();
    if cols == 0 {
        return Vec::new();
    }

    let mut out = vec![vec![T::zero(); rows]; cols];
    for (r, row) in matrix.iter().enumerate() {
        for (c, out_col) in out.iter_mut().enumerate().take(cols) {
            out_col[r] = row[c];
        }
    }
    out
}

pub(super) fn flatten_matrix<T: SimFloat>(matrix: &[Vec<T>]) -> Vec<T> {
    matrix.iter().flat_map(|row| row.iter().copied()).collect()
}

pub(in crate::eval) fn eval_matrix_literal_rows<T: SimFloat>(
    elements: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Vec<Vec<T>>, EvalError> {
    let mut rows = Vec::new();
    for element in elements {
        match element {
            rumoca_core::Expression::Array {
                elements: row_elements,
                ..
            } => rows.extend(eval_matrix_literal_row(row_elements, env)?),
            _ => rows.push(eval_array_values::<T>(element, env)?),
        }
    }
    rectangular_matrix_cols(&rows, "matrix literal")?;
    Ok(rows)
}

pub(super) fn rectangular_matrix_cols<T>(
    matrix: &[Vec<T>],
    context: &'static str,
) -> Result<usize, EvalError> {
    let Some(first) = matrix.first() else {
        return Ok(0);
    };
    let expected = first.len();
    for row in matrix.iter().skip(1) {
        if row.len() != expected {
            return Err(EvalError::ShapeMismatch {
                context,
                expected,
                actual: row.len(),
            });
        }
    }
    Ok(expected)
}

pub(super) fn eval_matrix_literal_row<T: SimFloat>(
    row_elements: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Vec<Vec<T>>, EvalError> {
    // MLS §10.6: matrix rows may be formed from array-valued expressions. In
    // that case each row expression contributes columns, not one ragged row.
    let columns = row_elements
        .iter()
        .map(|element| eval_array_like_values::<T>(element, env))
        .collect::<Result<Vec<_>, EvalError>>()?;
    let Some(row_count) = columns.iter().map(Vec::len).max() else {
        return Err(EvalError::UnsupportedExpression {
            kind: "matrix literal",
        });
    };
    if row_count == 0 {
        return Err(EvalError::UnsupportedExpression {
            kind: "matrix literal",
        });
    }
    if columns
        .iter()
        .any(|column| column.is_empty() || (column.len() != 1 && column.len() != row_count))
    {
        return Err(EvalError::UnsupportedExpression {
            kind: "matrix literal",
        });
    }
    Ok((0..row_count)
        .map(|row| {
            columns
                .iter()
                .map(|column| column[if column.len() == 1 { 0 } else { row }])
                .collect()
        })
        .collect())
}

pub fn eval_matrix_values<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Option<Vec<Vec<T>>>, EvalError> {
    with_expr_span(
        expr,
        match expr {
            rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
                eval_binary_matrix_values(op, lhs, rhs, env)
            }
            rumoca_core::Expression::FunctionCall {
                name,
                args,
                is_constructor: false,
                span,
            } => eval_function_call_matrix_values(name, args, *span, env),
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => matrix_values_from_env_path(name.as_str(), env),
            rumoca_core::Expression::FieldAccess { .. } => {
                let Some(path) = try_eval_field_access_path(expr, env)? else {
                    return Ok(None);
                };
                matrix_values_from_env_path(path.as_str(), env)
            }
            rumoca_core::Expression::Array { elements, .. }
                if elements
                    .iter()
                    .all(|element| matches!(element, rumoca_core::Expression::Array { .. })) =>
            {
                Ok(Some(eval_matrix_literal_rows(elements, env)?))
            }
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Transpose,
                args,
                ..
            } if args.len() == 1 => Ok(eval_matrix_values(&args[0], env)?.map(transpose_matrix)),
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Matrix,
                args,
                ..
            } if args.len() == 1 => eval_matrix_values(&args[0], env),
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Diagonal,
                args,
                ..
            } if args.len() == 1 => {
                let values = eval_array_like_values(&args[0], env)?;
                Ok(Some(diagonal_matrix(values)))
            }
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => {
                for (condition, value) in branches {
                    if eval_expr::<T>(condition, env)?.real() != 0.0 {
                        return eval_matrix_values(value, env);
                    }
                }
                eval_matrix_values(else_branch, env)
            }
            rumoca_core::Expression::BuiltinCall { .. } => {
                // Shape-defining builtins (identity, zeros, fill, ...) yield a
                // matrix exactly when their runtime shape is rank 2.
                let dims = try_infer_runtime_expr_dims(expr, env)?;
                if dims.len() != 2 {
                    return Ok(None);
                }
                let values = eval_array_like_values(expr, env)?;
                Ok(Some(reshape_flat_matrix_generic(
                    &values, dims[0], dims[1],
                )?))
            }
            _ => Ok(None),
        },
    )
}

pub(super) fn eval_binary_matrix_values<T: SimFloat>(
    op: &OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Option<Vec<Vec<T>>>, EvalError> {
    match op {
        OpBinary::Mul => {
            if let Some(product) = eval_matrix_matrix_product_rows(lhs, rhs, env)? {
                return Ok(Some(product));
            }
            eval_scalar_matrix_broadcast(op, lhs, rhs, env)
        }
        OpBinary::MulElem
        | OpBinary::Div
        | OpBinary::DivElem
        | OpBinary::Add
        | OpBinary::AddElem
        | OpBinary::Sub
        | OpBinary::SubElem => {
            if let Some(rows) = eval_elementwise_matrix_values(op, lhs, rhs, env)? {
                return Ok(Some(rows));
            }
            eval_scalar_matrix_broadcast(op, lhs, rhs, env)
        }
        _ => Ok(None),
    }
}

/// Matrix values of a flattened array binding reachable by `path`.
pub(super) fn matrix_values_from_env_path<T: SimFloat>(
    path: &str,
    env: &VarEnv<T>,
) -> Result<Option<Vec<Vec<T>>>, EvalError> {
    let Some(flat_values) = array_values_from_env_name_generic(path, env)? else {
        return Ok(None);
    };
    if flat_values.is_empty() {
        return Ok(Some(Vec::new()));
    }
    let raw_dims = declared_dims(path, env)?;
    let inferred = infer_dims_from_values(&raw_dims, flat_values.len())?;
    if inferred.len() >= 2 {
        return Ok(Some(reshape_flat_matrix_generic(
            &flat_values,
            inferred[0],
            inferred[1],
        )?));
    }
    Ok(None)
}

pub(super) fn apply_matrix_binary_op<T: SimFloat>(op: &OpBinary, l: T, r: T) -> Option<T> {
    Some(match op {
        OpBinary::Add | OpBinary::AddElem => l + r,
        OpBinary::Sub | OpBinary::SubElem => l - r,
        OpBinary::Mul | OpBinary::MulElem => l * r,
        OpBinary::Div | OpBinary::DivElem => l / r,
        _ => return None,
    })
}

/// Elementwise combination of two same-shape matrix operands (MLS §10.6).
pub(super) fn eval_elementwise_matrix_values<T: SimFloat>(
    op: &OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Option<Vec<Vec<T>>>, EvalError> {
    let Some(lhs_rows) = eval_matrix_values(lhs, env)? else {
        return Ok(None);
    };
    let Some(rhs_rows) = eval_matrix_values(rhs, env)? else {
        return Ok(None);
    };
    if lhs_rows.len() != rhs_rows.len() {
        return Err(EvalError::ShapeMismatch {
            context: "elementwise matrix rows",
            expected: lhs_rows.len(),
            actual: rhs_rows.len(),
        });
    }
    let mut rows = Vec::with_capacity(lhs_rows.len());
    for (lhs_row, rhs_row) in lhs_rows.iter().zip(rhs_rows.iter()) {
        if lhs_row.len() != rhs_row.len() {
            return Err(EvalError::ShapeMismatch {
                context: "elementwise matrix columns",
                expected: lhs_row.len(),
                actual: rhs_row.len(),
            });
        }
        let row = lhs_row
            .iter()
            .zip(rhs_row.iter())
            .map(|(l, r)| {
                apply_matrix_binary_op(op, *l, *r).ok_or(EvalError::UnsupportedExpression {
                    kind: "elementwise matrix operator",
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        rows.push(row);
    }
    Ok(Some(rows))
}

/// Broadcast a scalar operand over a matrix operand (e.g. `0.5 * A`).
pub(super) fn eval_scalar_matrix_broadcast<T: SimFloat>(
    op: &OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Option<Vec<Vec<T>>>, EvalError> {
    let scalar_of = |expr: &rumoca_core::Expression| -> Result<Option<T>, EvalError> {
        let values = eval_array_like_values(expr, env)?;
        Ok((values.len() == 1).then(|| values[0]))
    };
    if let Some(rows) = eval_matrix_values(lhs, env)? {
        let Some(scalar) = scalar_of(rhs)? else {
            return Ok(None);
        };
        return Ok(Some(map_matrix_rows(rows, |value| {
            apply_matrix_binary_op(op, value, scalar)
        })?));
    }
    if let Some(rows) = eval_matrix_values(rhs, env)? {
        // Plain matrix division requires the scalar on the right (MLS §10.6.7).
        if matches!(op, OpBinary::Div) {
            return Ok(None);
        }
        let Some(scalar) = scalar_of(lhs)? else {
            return Ok(None);
        };
        return Ok(Some(map_matrix_rows(rows, |value| {
            apply_matrix_binary_op(op, scalar, value)
        })?));
    }
    Ok(None)
}

pub(super) fn map_matrix_rows<T: SimFloat>(
    rows: Vec<Vec<T>>,
    mut f: impl FnMut(T) -> Option<T>,
) -> Result<Vec<Vec<T>>, EvalError> {
    rows.into_iter()
        .map(|row| {
            row.into_iter()
                .map(|value| {
                    f(value).ok_or(EvalError::UnsupportedExpression {
                        kind: "matrix broadcast operator",
                    })
                })
                .collect::<Result<Vec<_>, _>>()
        })
        .collect()
}

pub(super) fn eval_function_call_matrix_values<T: SimFloat>(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    source_span: rumoca_core::Span,
    env: &VarEnv<T>,
) -> Result<Option<Vec<Vec<T>>>, EvalError> {
    let Some(function) = env.functions.get(name.as_str()) else {
        return Ok(None);
    };
    let Some(output) = function.outputs.first() else {
        return Ok(None);
    };
    if output.dims.len() < 2 && output.shape_expr.len() < 2 {
        return Ok(None);
    }
    let dims = resolve_user_function_output_dims_pub(name, args, None, env)?.ok_or(
        EvalError::UnsupportedExpression {
            kind: "function output shape",
        },
    )?;
    let rows = dims
        .first()
        .copied()
        .and_then(|value| usize::try_from(value).ok())
        .filter(|rows| *rows > 0)
        .ok_or(EvalError::UnsupportedExpression {
            kind: "function output shape",
        })?;
    let cols = dims
        .get(1)
        .copied()
        .and_then(|value| usize::try_from(value).ok())
        .filter(|cols| *cols > 0)
        .ok_or(EvalError::UnsupportedExpression {
            kind: "function output shape",
        })?;
    let values = try_eval_function_call_array_values(name, args, source_span, env)?;
    let expected = rows
        .checked_mul(cols)
        .ok_or(EvalError::UnsupportedExpression {
            kind: "function output shape",
        })?;
    if values.len() != expected {
        return Err(EvalError::ShapeMismatch {
            context: "function matrix output",
            expected,
            actual: values.len(),
        });
    }
    Ok(Some(reshape_flat_matrix_generic(&values, rows, cols)?))
}

pub(in crate::eval) fn eval_matrix_index<T: SimFloat>(
    expr: &rumoca_core::Expression,
    indices: &[usize],
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    if indices.len() != 2 {
        return Ok(None);
    }
    let Some(matrix) = eval_matrix_values(expr, env)? else {
        return Ok(None);
    };
    let (Some(row), Some(col)) = (indices[0].checked_sub(1), indices[1].checked_sub(1)) else {
        return Ok(None);
    };
    Ok(matrix.get(row).and_then(|row| row.get(col)).copied())
}

pub(in crate::eval) fn eval_transpose_values<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Option<Vec<T>>, EvalError> {
    let Some(arg) = args.first() else {
        return Ok(None);
    };
    let Some(matrix) = eval_matrix_values(arg, env)? else {
        return Ok(None);
    };
    Ok(Some(flatten_matrix(&transpose_matrix(matrix))))
}

pub(in crate::eval) fn eval_cross_values<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Option<Vec<T>>, EvalError> {
    if args.len() != 2 {
        return Ok(None);
    }
    let lhs = eval_array_like_values(&args[0], env)?;
    let rhs = eval_array_like_values(&args[1], env)?;
    if lhs.len() != 3 || rhs.len() != 3 {
        return Ok(None);
    }
    Ok(Some(vec![
        lhs[1] * rhs[2] - lhs[2] * rhs[1],
        lhs[2] * rhs[0] - lhs[0] * rhs[2],
        lhs[0] * rhs[1] - lhs[1] * rhs[0],
    ]))
}

pub(in crate::eval) fn eval_skew_values<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Option<Vec<T>>, EvalError> {
    let Some(arg) = args.first() else {
        return Ok(None);
    };
    let values = eval_array_like_values(arg, env)?;
    if values.len() != 3 {
        return Ok(None);
    }
    let zero = T::default();
    Ok(Some(vec![
        zero, -values[2], values[1], values[2], zero, -values[0], -values[1], values[0], zero,
    ]))
}

pub(in crate::eval) fn eval_outer_product_values<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Option<Vec<T>>, EvalError> {
    if args.len() != 2 {
        return Ok(None);
    }
    let lhs = eval_array_like_values(&args[0], env)?;
    let rhs = eval_array_like_values(&args[1], env)?;
    Ok(Some(
        lhs.iter()
            .flat_map(|lhs_value| rhs.iter().map(move |rhs_value| *lhs_value * *rhs_value))
            .collect(),
    ))
}

pub(in crate::eval) fn eval_symmetric_values<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Option<Vec<T>>, EvalError> {
    let Some(arg) = args.first() else {
        return Ok(None);
    };
    let Some(matrix) = eval_matrix_values(arg, env)? else {
        return Ok(None);
    };
    let n = matrix.len();
    if n == 0 || matrix.iter().any(|row| row.len() != n) {
        return Ok(None);
    }
    let mut out = Vec::with_capacity(n * n);
    for row in 0..n {
        for col in 0..n {
            let (source_row, source_col) = if row >= col { (row, col) } else { (col, row) };
            out.push(matrix[source_row][source_col]);
        }
    }
    Ok(Some(out))
}

pub(in crate::eval) fn eval_matrix_vector_product<T: SimFloat>(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Option<Vec<T>>, EvalError> {
    let Some(matrix) = eval_matrix_values(lhs, env)? else {
        return Ok(None);
    };
    let vector = eval_array_like_values(rhs, env)?;
    let cols = rectangular_matrix_cols(&matrix, "matrix-vector product")?;
    if matrix.is_empty() || cols == 0 || vector.len() != cols {
        return Ok(None);
    }

    Ok(Some(
        matrix
            .iter()
            .map(|row| (0..cols).fold(T::zero(), |acc, col| acc + row[col] * vector[col]))
            .collect(),
    ))
}

pub(in crate::eval) fn eval_vector_matrix_product<T: SimFloat>(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Option<Vec<T>>, EvalError> {
    let vector = eval_array_like_values(lhs, env)?;
    let Some(matrix) = eval_matrix_values(rhs, env)? else {
        return Ok(None);
    };
    let rows = matrix.len();
    let cols = rectangular_matrix_cols(&matrix, "vector-matrix product")?;
    if rows == 0 || cols == 0 || vector.len() != rows {
        return Ok(None);
    }

    Ok(Some(
        (0..cols)
            .map(|col| (0..rows).fold(T::zero(), |acc, row| acc + vector[row] * matrix[row][col]))
            .collect(),
    ))
}

pub(super) fn eval_matrix_matrix_product<T: SimFloat>(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Option<Vec<T>>, EvalError> {
    Ok(eval_matrix_matrix_product_rows(lhs, rhs, env)?.map(|rows| flatten_matrix(&rows)))
}

pub(super) fn eval_matrix_matrix_product_rows<T: SimFloat>(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Option<Vec<Vec<T>>>, EvalError> {
    let Some(lhs_matrix) = eval_matrix_values(lhs, env)? else {
        return Ok(None);
    };
    let Some(rhs_matrix) = eval_matrix_values(rhs, env)? else {
        return Ok(None);
    };
    let rows = lhs_matrix.len();
    let inner = rectangular_matrix_cols(&lhs_matrix, "matrix-matrix lhs")?;
    let rhs_rows = rhs_matrix.len();
    let cols = rectangular_matrix_cols(&rhs_matrix, "matrix-matrix rhs")?;
    if rows == 0 || inner == 0 || rhs_rows != inner || cols == 0 {
        return Ok(None);
    }

    Ok(Some(
        (0..rows)
            .map(|row| {
                (0..cols)
                    .map(|col| {
                        (0..inner).fold(T::zero(), |acc, k| {
                            let l = lhs_matrix[row][k];
                            let r = rhs_matrix[k][col];
                            acc + l * r
                        })
                    })
                    .collect()
            })
            .collect(),
    ))
}
