use super::*;
use crate::eval::special::{
    eval_selected_runtime_special_array_output, resolve_runtime_special_target,
};

pub(super) fn declared_dims<T: SimFloat>(
    name: &str,
    env: &VarEnv<T>,
) -> Result<Vec<i64>, EvalError> {
    env.dims
        .get(name)
        .cloned()
        .ok_or_else(|| EvalError::MissingBinding {
            name: format!("{name} dimensions"),
        })
}

pub fn eval_array_values<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    with_expr_span(
        expr,
        match expr {
            rumoca_core::Expression::Array {
                elements,
                is_matrix,
                ..
            } => try_eval_array_literal_values(elements, *is_matrix, env),
            rumoca_core::Expression::Tuple { elements, .. } => elements
                .iter()
                .map(|element| eval_array_values(element, env))
                .collect::<Result<Vec<_>, _>>()
                .map(|values| values.into_iter().flatten().collect()),
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => try_eval_if_values(branches, else_branch, env),
            rumoca_core::Expression::Range {
                start, step, end, ..
            } => try_eval_range_values(start, step.as_deref(), end, env),
            rumoca_core::Expression::ArrayComprehension { .. } => {
                try_eval_array_comprehension_values(expr, env)
            }
            _ => eval_array_like_values(expr, env),
        },
    )
}

pub fn eval_shaped_array_values<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
    expected_len: usize,
) -> Result<Vec<T>, EvalError> {
    let values = eval_array_values(expr, env)?;
    if values.len() != expected_len {
        return Err(EvalError::ShapeMismatch {
            context: "shaped array value",
            expected: expected_len,
            actual: values.len(),
        });
    }
    Ok(values)
}

pub(super) fn eval_array_like_values<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    with_expr_span(
        expr,
        match expr {
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => {
                if let Some(values) = encoded_slice_field_values(name.as_str(), env)? {
                    return Ok(values);
                }
                if let Some(values) = array_values_from_env_name_generic(name.as_str(), env)? {
                    return Ok(values);
                }
                Ok(vec![eval_expr(expr, env)?])
            }
            rumoca_core::Expression::VarRef {
                name,
                subscripts,
                span,
            } => eval_index_array_values(
                &rumoca_core::Expression::VarRef {
                    name: name.clone(),
                    subscripts: vec![],
                    span: *span,
                },
                subscripts,
                env,
            ),
            rumoca_core::Expression::FieldAccess { base, field, .. } => {
                try_eval_field_access_array_values(base, field, env)
            }
            rumoca_core::Expression::Index {
                base, subscripts, ..
            } => eval_index_array_values(base, subscripts, env),
            rumoca_core::Expression::BuiltinCall { function, args, .. } => {
                try_eval_builtin_array_like_values(expr, *function, args, env)
            }
            rumoca_core::Expression::Binary { op, lhs, rhs, span } => {
                try_eval_binary_array_values(op, lhs, rhs, *span, env)
            }
            rumoca_core::Expression::Unary { op, rhs, .. } => {
                try_eval_unary_array_values(op, rhs, env)
            }
            rumoca_core::Expression::FunctionCall {
                name,
                args,
                is_constructor: false,
                span,
            } => try_eval_function_call_array_values(name, args, *span, env),
            rumoca_core::Expression::Array { .. }
            | rumoca_core::Expression::Tuple { .. }
            | rumoca_core::Expression::Range { .. }
            | rumoca_core::Expression::If { .. }
            | rumoca_core::Expression::ArrayComprehension { .. } => eval_array_values(expr, env),
            _ => Ok(vec![eval_expr(expr, env)?]),
        },
    )
}

pub(super) fn eval_index_array_values<T: SimFloat>(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    let dims = try_infer_runtime_expr_dims(base, env)?;
    if dims.is_empty() || subscripts.len() > dims.len() {
        return Err(EvalError::ShapeMismatch {
            context: "array slice rank",
            expected: dims.len(),
            actual: subscripts.len(),
        });
    }
    let base_values = eval_array_values(base, env)?;
    let mut selections = Vec::with_capacity(dims.len());
    for (axis, extent) in dims.iter().copied().enumerate() {
        let selection = match subscripts.get(axis) {
            Some(rumoca_core::Subscript::Index { value, .. }) => {
                vec![checked_array_selection_index(*value as f64, extent)?]
            }
            Some(rumoca_core::Subscript::Expr { expr, .. })
                if matches!(expr.as_ref(), rumoca_core::Expression::Range { .. }) =>
            {
                eval_array_values(expr.as_ref(), env)?
                    .into_iter()
                    .map(|value| checked_array_selection_index(value.real(), extent))
                    .collect::<Result<Vec<_>, _>>()?
            }
            Some(rumoca_core::Subscript::Expr { expr, .. }) => vec![checked_array_selection_index(
                eval_expr::<T>(expr, env)?.real(),
                extent,
            )?],
            Some(rumoca_core::Subscript::Colon { .. }) | None => (1..=extent).collect(),
        };
        selections.push(selection);
    }

    let selection_dims = selections.iter().map(Vec::len).collect::<Vec<_>>();
    let total = selection_dims
        .iter()
        .try_fold(1usize, |count, extent| count.checked_mul(*extent));
    let total = total.ok_or(EvalError::UnsupportedExpression {
        kind: "array slice dimensions",
    })?;
    let mut values = Vec::with_capacity(total);
    for flat_index in 0..total {
        let selected_subscripts = flat_index_to_usize_subscripts(&selection_dims, flat_index)?;
        let actual_subscripts = selected_subscripts
            .iter()
            .enumerate()
            .map(|(axis, selected)| selections[axis][selected - 1])
            .collect::<Vec<_>>();
        let base_index = subscripts_to_flat_index(&dims, &actual_subscripts)?;
        values.push(
            *base_values
                .get(base_index)
                .ok_or(EvalError::ShapeMismatch {
                    context: "array slice source",
                    expected: base_values.len(),
                    actual: base_index + 1,
                })?,
        );
    }
    Ok(values)
}

fn checked_array_selection_index(value: f64, extent: usize) -> Result<usize, EvalError> {
    if !value.is_finite() || value.fract() != 0.0 {
        return Err(EvalError::UnsupportedExpression {
            kind: "array slice index",
        });
    }
    usize::try_from(value as i64)
        .ok()
        .filter(|index| *index >= 1 && *index <= extent)
        .ok_or(EvalError::UnsupportedExpression {
            kind: "array slice index",
        })
}

fn flat_index_to_usize_subscripts(
    dims: &[usize],
    flat_index: usize,
) -> Result<Vec<usize>, EvalError> {
    let dims = dims
        .iter()
        .map(|dim| {
            i64::try_from(*dim).map_err(|_| EvalError::UnsupportedExpression {
                kind: "array slice dimensions",
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    dae::flat_index_to_subscripts(&dims, flat_index).ok_or(EvalError::UnsupportedExpression {
        kind: "array slice index",
    })
}

fn subscripts_to_flat_index(dims: &[usize], subscripts: &[usize]) -> Result<usize, EvalError> {
    if dims.len() != subscripts.len() {
        return Err(EvalError::ShapeMismatch {
            context: "array slice index rank",
            expected: dims.len(),
            actual: subscripts.len(),
        });
    }
    dims.iter()
        .zip(subscripts)
        .try_fold(0usize, |flat_index, (extent, subscript)| {
            if *subscript < 1 || *subscript > *extent {
                return Err(EvalError::UnsupportedExpression {
                    kind: "array slice index",
                });
            }
            flat_index
                .checked_mul(*extent)
                .and_then(|value| value.checked_add(*subscript - 1))
                .ok_or(EvalError::UnsupportedExpression {
                    kind: "array slice index",
                })
        })
}

fn with_expr_span<T>(
    expr: &rumoca_core::Expression,
    result: Result<T, EvalError>,
) -> Result<T, EvalError> {
    result.map_err(|err| match expr.span() {
        Some(span) => err.with_span_if_missing(span),
        None => err,
    })
}

fn try_eval_builtin_array_like_values<T: SimFloat>(
    expr: &rumoca_core::Expression,
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    match function {
        rumoca_core::BuiltinFunction::Cat => try_eval_cat_values(args, env),
        rumoca_core::BuiltinFunction::Linspace => eval_linspace_values(args, env),
        rumoca_core::BuiltinFunction::Zeros
        | rumoca_core::BuiltinFunction::Ones
        | rumoca_core::BuiltinFunction::Fill
        | rumoca_core::BuiltinFunction::Identity
        | rumoca_core::BuiltinFunction::Diagonal => {
            eval_array_constructor_values(&function, args, env)
        }
        rumoca_core::BuiltinFunction::Transpose => {
            eval_transpose_values(args, env)?.ok_or(EvalError::UnsupportedExpression {
                kind: "transpose shape",
            })
        }
        rumoca_core::BuiltinFunction::Cross => {
            eval_cross_values(args, env)?.ok_or(EvalError::UnsupportedExpression {
                kind: "cross product shape",
            })
        }
        rumoca_core::BuiltinFunction::Skew => eval_skew_values(args, env)?
            .ok_or(EvalError::UnsupportedExpression { kind: "skew shape" }),
        rumoca_core::BuiltinFunction::OuterProduct => {
            eval_outer_product_values(args, env)?.ok_or(EvalError::UnsupportedExpression {
                kind: "outerProduct shape",
            })
        }
        rumoca_core::BuiltinFunction::Symmetric => {
            eval_symmetric_values(args, env)?.ok_or(EvalError::UnsupportedExpression {
                kind: "symmetric shape",
            })
        }
        rumoca_core::BuiltinFunction::Vector if args.len() == 1 => {
            eval_array_like_values(&args[0], env)
        }
        _ if args.len() == 1 => try_eval_unary_builtin_array_like_values(expr, function, args, env),
        _ => Ok(vec![eval_expr(expr, env)?]),
    }
}

fn try_eval_unary_builtin_array_like_values<T: SimFloat>(
    expr: &rumoca_core::Expression,
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    let values = eval_array_like_values(&args[0], env)?;
    if values.len() > 1
        && let Some(mapped) = eval_unary_builtin_array_values(function, values)
    {
        return Ok(mapped);
    }
    Ok(vec![eval_expr(expr, env)?])
}

fn try_eval_array_literal_values<T: SimFloat>(
    elements: &[rumoca_core::Expression],
    is_matrix: bool,
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    if can_interleave_matrix_columns(elements, is_matrix) {
        return try_interleave_matrix_columns(elements, env);
    }
    if is_matrix {
        return eval_matrix_literal_rows(elements, env).map(|rows| flatten_matrix(&rows));
    }
    elements
        .iter()
        .map(|element| eval_array_values(element, env))
        .collect::<Result<Vec<_>, _>>()
        .map(|values| values.into_iter().flatten().collect())
}

fn can_interleave_matrix_columns(elements: &[rumoca_core::Expression], is_matrix: bool) -> bool {
    is_matrix
        && !elements.is_empty()
        && !elements
            .iter()
            .all(|element| matches!(element, rumoca_core::Expression::Array { .. }))
}

fn try_interleave_matrix_columns<T: SimFloat>(
    elements: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    let columns = elements
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

    let mut out = Vec::with_capacity(row_count.saturating_mul(columns.len()));
    for row in 0..row_count {
        for column in &columns {
            out.push(if column.len() == 1 {
                column[0]
            } else {
                column[row]
            });
        }
    }
    Ok(out)
}

fn try_eval_if_values<T: SimFloat>(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    for (cond, then_expr) in branches {
        if try_eval_condition_truth(cond, env)? {
            return eval_array_values(then_expr, env);
        }
    }
    eval_array_values(else_branch, env)
}

fn try_eval_range_values<T: SimFloat>(
    start: &rumoca_core::Expression,
    step: Option<&rumoca_core::Expression>,
    end: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    let start_v = eval_expr::<T>(start, env)?.real();
    let end_v = eval_expr::<T>(end, env)?.real();
    let step_v = if let Some(step_expr) = step {
        eval_expr::<T>(step_expr, env)?.real()
    } else {
        1.0
    };
    if !start_v.is_finite()
        || !end_v.is_finite()
        || !step_v.is_finite()
        || step_v.abs() <= f64::EPSILON
    {
        return Err(EvalError::UnsupportedExpression { kind: "range" });
    }
    let mut out = Vec::new();
    extend_range_values(start_v, end_v, step_v, &mut out);
    Ok(out)
}

fn extend_range_values<T: SimFloat>(start_v: f64, end_v: f64, step_v: f64, out: &mut Vec<T>) {
    let limit = 100_000usize;
    let tol = step_v.abs() * 1e-9 + 1e-12;
    let mut value = start_v;
    for _ in 0..limit {
        let past_end =
            (step_v > 0.0 && value > end_v + tol) || (step_v < 0.0 && value < end_v - tol);
        if past_end {
            return;
        }
        out.push(T::from_f64(value));
        value += step_v;
    }
}

fn try_eval_array_comprehension_values<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    let rumoca_core::Expression::ArrayComprehension {
        expr,
        indices,
        filter,
        ..
    } = expr
    else {
        return Err(EvalError::UnsupportedExpression {
            kind: "array comprehension",
        });
    };

    let mut out = Vec::new();
    let mut local_env = env.clone();
    expand_array_comprehension_values(
        0,
        expr,
        indices,
        filter.as_deref(),
        &mut local_env,
        &mut out,
    )?;
    Ok(out)
}

fn expand_array_comprehension_values<T: SimFloat>(
    level: usize,
    expr: &rumoca_core::Expression,
    indices: &[rumoca_core::ComprehensionIndex],
    filter: Option<&rumoca_core::Expression>,
    env: &mut VarEnv<T>,
    out: &mut Vec<T>,
) -> Result<(), EvalError> {
    if level >= indices.len() {
        if filter
            .map(|filter_expr| eval_expr::<T>(filter_expr, &*env).map(|value| value.to_bool()))
            .transpose()?
            .unwrap_or(true)
        {
            out.extend(eval_array_values(expr, &*env)?);
        }
        return Ok(());
    }

    let index = &indices[level];
    for value in eval_array_values::<T>(&index.range, &*env)? {
        let previous = env.get_optional(index.name.as_str());
        env.set(index.name.as_str(), value);
        let result = expand_array_comprehension_values(level + 1, expr, indices, filter, env, out);
        match previous {
            Some(previous) => env.set(index.name.as_str(), previous),
            None => {
                env.remove(index.name.as_str());
            }
        }
        result?;
    }
    Ok(())
}

pub(super) fn reshape_flat_matrix(
    flat_values: &[f64],
    rows: usize,
    cols: usize,
) -> Result<Vec<Vec<f64>>, EvalError> {
    ensure_flat_matrix_len(flat_values.len(), rows, cols)?;
    let mut matrix = Vec::with_capacity(rows);
    for r in 0..rows {
        let start = r * cols;
        let end = start + cols;
        let row = flat_values[start..end].to_vec();
        matrix.push(row);
    }
    Ok(matrix)
}

fn reshape_flat_matrix_generic<T: SimFloat>(
    flat_values: &[T],
    rows: usize,
    cols: usize,
) -> Result<Vec<Vec<T>>, EvalError> {
    ensure_flat_matrix_len(flat_values.len(), rows, cols)?;
    let mut matrix = Vec::with_capacity(rows);
    for r in 0..rows {
        let start = r * cols;
        let end = start + cols;
        let row = flat_values[start..end].to_vec();
        matrix.push(row);
    }
    Ok(matrix)
}

fn ensure_flat_matrix_len(len: usize, rows: usize, cols: usize) -> Result<(), EvalError> {
    let expected = rows
        .checked_mul(cols)
        .ok_or(EvalError::UnsupportedExpression {
            kind: "matrix shape",
        })?;
    if len != expected {
        return Err(EvalError::ShapeMismatch {
            context: "matrix reshape",
            expected,
            actual: len,
        });
    }
    Ok(())
}

fn transpose_matrix<T: SimFloat>(matrix: Vec<Vec<T>>) -> Vec<Vec<T>> {
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

fn flatten_matrix<T: SimFloat>(matrix: &[Vec<T>]) -> Vec<T> {
    matrix.iter().flat_map(|row| row.iter().copied()).collect()
}

pub(super) fn eval_matrix_literal_rows<T: SimFloat>(
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

fn rectangular_matrix_cols<T>(
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

fn eval_matrix_literal_row<T: SimFloat>(
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
            } if subscripts.is_empty() => {
                let Some(flat_values) = array_values_from_env_name_generic(name.as_str(), env)?
                else {
                    return Ok(None);
                };
                if flat_values.is_empty() {
                    return Ok(Some(Vec::new()));
                }
                let raw_dims = declared_dims(name.as_str(), env)?;
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
            _ => Ok(None),
        },
    )
}

fn eval_binary_matrix_values<T: SimFloat>(
    op: &OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Option<Vec<Vec<T>>>, EvalError> {
    match op {
        OpBinary::Mul => eval_matrix_matrix_product_rows(lhs, rhs, env),
        _ => Ok(None),
    }
}

fn eval_function_call_matrix_values<T: SimFloat>(
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
    let dims = resolve_user_function_output_dims_pub(name.var_name(), args, None, env)?.ok_or(
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

pub(super) fn eval_matrix_index<T: SimFloat>(
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

pub(super) fn eval_transpose_values<T: SimFloat>(
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

pub(super) fn eval_cross_values<T: SimFloat>(
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

pub(super) fn eval_skew_values<T: SimFloat>(
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

pub(super) fn eval_outer_product_values<T: SimFloat>(
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

pub(super) fn eval_symmetric_values<T: SimFloat>(
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

pub(super) fn eval_matrix_vector_product<T: SimFloat>(
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

pub(super) fn eval_vector_matrix_product<T: SimFloat>(
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

fn eval_matrix_matrix_product<T: SimFloat>(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Option<Vec<T>>, EvalError> {
    Ok(eval_matrix_matrix_product_rows(lhs, rhs, env)?.map(|rows| flatten_matrix(&rows)))
}

fn eval_matrix_matrix_product_rows<T: SimFloat>(
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

pub(super) fn eval_binary_array_values<T: SimFloat>(
    op: &OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Option<Vec<T>>, EvalError> {
    match op {
        OpBinary::Mul => {
            if let Some(value) = try_eval_vector_dot_product(lhs, rhs, env)? {
                return Ok(Some(vec![value]));
            }
            if let Some(values) = eval_matrix_matrix_product(lhs, rhs, env)? {
                return Ok(Some(values));
            }
            if let Some(values) = eval_matrix_vector_product(lhs, rhs, env)? {
                return Ok(Some(values));
            }
            if let Some(values) = eval_vector_matrix_product(lhs, rhs, env)? {
                return Ok(Some(values));
            }
            eval_elementwise_binary_array_values(lhs, rhs, env, |l, r| l * r)
        }
        OpBinary::MulElem => eval_elementwise_binary_array_values(lhs, rhs, env, |l, r| l * r),
        OpBinary::Add | OpBinary::AddElem => {
            eval_elementwise_binary_array_values(lhs, rhs, env, |l, r| l + r)
        }
        OpBinary::Sub | OpBinary::SubElem => {
            eval_elementwise_binary_array_values(lhs, rhs, env, |l, r| l - r)
        }
        OpBinary::Div | OpBinary::DivElem => {
            eval_elementwise_binary_array_values(lhs, rhs, env, |l, r| l / r)
        }
        _ => Ok(None),
    }
}

fn try_eval_binary_array_values<T: SimFloat>(
    op: &OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    source_span: rumoca_core::Span,
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    match op {
        OpBinary::Mul => {
            if let Some(value) = try_eval_vector_dot_product(lhs, rhs, env)? {
                return Ok(vec![value]);
            }
            if let Some(values) = eval_matrix_matrix_product(lhs, rhs, env)? {
                return Ok(values);
            }
            if let Some(values) = eval_matrix_vector_product(lhs, rhs, env)? {
                return Ok(values);
            }
            if let Some(values) = eval_vector_matrix_product(lhs, rhs, env)? {
                return Ok(values);
            }
            try_eval_elementwise_binary_array_values(lhs, rhs, env, |l, r| l * r)
        }
        OpBinary::MulElem => try_eval_elementwise_binary_array_values(lhs, rhs, env, |l, r| l * r),
        OpBinary::Add | OpBinary::AddElem => {
            try_eval_elementwise_binary_array_values(lhs, rhs, env, |l, r| l + r)
        }
        OpBinary::Sub | OpBinary::SubElem => {
            try_eval_elementwise_binary_array_values(lhs, rhs, env, |l, r| l - r)
        }
        OpBinary::Div | OpBinary::DivElem => {
            try_eval_elementwise_binary_array_values(lhs, rhs, env, |l, r| l / r)
        }
        _ => Ok(vec![eval_expr(
            &rumoca_core::Expression::Binary {
                op: op.clone(),
                lhs: Box::new(lhs.clone()),
                rhs: Box::new(rhs.clone()),
                span: source_span,
            },
            env,
        )?]),
    }
}

fn eval_elementwise_binary_array_values<T: SimFloat>(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
    op: impl Fn(T, T) -> T,
) -> Result<Option<Vec<T>>, EvalError> {
    let lhs_values = eval_array_like_values(lhs, env)?;
    let rhs_values = eval_array_like_values(rhs, env)?;
    let len = match (lhs_values.len(), rhs_values.len()) {
        (0, _) | (_, 0) => return Ok(None),
        (1, 1) => return Ok(None),
        (1, rhs_len) => rhs_len,
        (lhs_len, 1) => lhs_len,
        (lhs_len, rhs_len) if lhs_len == rhs_len => lhs_len,
        _ => return Ok(None),
    };
    let mut values = Vec::with_capacity(len);
    for idx in 0..len {
        let lhs_index = if lhs_values.len() == 1 { 0 } else { idx };
        let lhs_value = lhs_values
            .get(lhs_index)
            .copied()
            .ok_or(EvalError::ShapeMismatch {
                context: "elementwise binary lhs",
                expected: len,
                actual: lhs_values.len(),
            })?;
        let rhs_index = if rhs_values.len() == 1 { 0 } else { idx };
        let rhs_value = rhs_values
            .get(rhs_index)
            .copied()
            .ok_or(EvalError::ShapeMismatch {
                context: "elementwise binary rhs",
                expected: len,
                actual: rhs_values.len(),
            })?;
        values.push(op(lhs_value, rhs_value));
    }
    Ok(Some(values))
}

fn try_eval_elementwise_binary_array_values<T: SimFloat>(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
    op: impl Fn(T, T) -> T,
) -> Result<Vec<T>, EvalError> {
    let lhs_values = eval_array_like_values(lhs, env)?;
    let rhs_values = eval_array_like_values(rhs, env)?;
    let len = match (lhs_values.len(), rhs_values.len()) {
        (0, _) | (_, 0) => {
            return Err(EvalError::UnsupportedExpression {
                kind: "empty array operand",
            });
        }
        (1, 1) => return Ok(vec![op(lhs_values[0], rhs_values[0])]),
        (1, rhs_len) => rhs_len,
        (lhs_len, 1) => lhs_len,
        (lhs_len, rhs_len) if lhs_len == rhs_len => lhs_len,
        (lhs_len, rhs_len) => {
            return Err(EvalError::ShapeMismatch {
                context: "binary array operand",
                expected: lhs_len,
                actual: rhs_len,
            });
        }
    };
    Ok((0..len)
        .map(|idx| {
            let lhs_value = lhs_values[if lhs_values.len() == 1 { 0 } else { idx }];
            let rhs_value = rhs_values[if rhs_values.len() == 1 { 0 } else { idx }];
            op(lhs_value, rhs_value)
        })
        .collect())
}

fn try_eval_unary_array_values<T: SimFloat>(
    op: &rumoca_core::OpUnary,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    let values = eval_array_like_values(rhs, env)?;
    Ok(values
        .into_iter()
        .map(|value| apply_unary_value(op, value))
        .collect())
}

fn apply_unary_value<T: SimFloat>(op: &rumoca_core::OpUnary, value: T) -> T {
    match op {
        rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => -value,
        rumoca_core::OpUnary::Plus
        | rumoca_core::OpUnary::DotPlus
        | rumoca_core::OpUnary::Empty => value,
        rumoca_core::OpUnary::Not => T::from_bool(!value.to_bool()),
    }
}

pub(super) fn eval_array_constructor_values<T: SimFloat>(
    function: &rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    match function {
        rumoca_core::BuiltinFunction::Zeros => {
            if args.is_empty() {
                return Err(EvalError::UnsupportedExpression {
                    kind: "zeros arguments",
                });
            }
            let len = constructor_extent(args, env)?;
            Ok(vec![T::zero(); len])
        }
        rumoca_core::BuiltinFunction::Ones => {
            if args.is_empty() {
                return Err(EvalError::UnsupportedExpression {
                    kind: "ones arguments",
                });
            }
            let len = constructor_extent(args, env)?;
            Ok(vec![T::one(); len])
        }
        rumoca_core::BuiltinFunction::Fill => {
            if args.len() < 2 {
                return Err(EvalError::UnsupportedExpression {
                    kind: "fill arguments",
                });
            }
            let first = args.first().ok_or(EvalError::UnsupportedExpression {
                kind: "fill arguments",
            })?;
            let fill = eval_expr::<T>(first, env)?;
            let len = constructor_extent(&args[1..], env)?;
            Ok(vec![fill; len])
        }
        rumoca_core::BuiltinFunction::Identity => {
            let first = args.first().ok_or(EvalError::UnsupportedExpression {
                kind: "identity arguments",
            })?;
            let n = constructor_dim(first, env)?;
            let len = n.checked_mul(n).ok_or(EvalError::UnsupportedExpression {
                kind: "identity dimension",
            })?;
            let mut values = vec![T::zero(); len];
            for idx in 0..n {
                values[idx * n + idx] = T::one();
            }
            Ok(values)
        }
        rumoca_core::BuiltinFunction::Diagonal => {
            let first = args.first().ok_or(EvalError::UnsupportedExpression {
                kind: "diagonal arguments",
            })?;
            let values = eval_array_like_values(first, env)?;
            Ok(flatten_matrix(&diagonal_matrix(values)))
        }
        _ => Err(EvalError::UnsupportedExpression {
            kind: "array constructor",
        }),
    }
}

fn diagonal_matrix<T: SimFloat>(values: Vec<T>) -> Vec<Vec<T>> {
    let n = values.len();
    let mut matrix = vec![vec![T::zero(); n]; n];
    for (idx, value) in values.into_iter().enumerate() {
        matrix[idx][idx] = value;
    }
    matrix
}

fn constructor_extent<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<usize, EvalError> {
    let mut extent = 1usize;
    for arg in args {
        let dim = constructor_dim(arg, env)?;
        extent = extent
            .checked_mul(dim)
            .ok_or(EvalError::UnsupportedExpression {
                kind: "array constructor extent",
            })?;
    }
    Ok(extent)
}

fn constructor_dim<T: SimFloat>(
    arg: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<usize, EvalError> {
    let value = eval_expr::<T>(arg, env)?.real();
    let dim = value.round();
    if !dim.is_finite() || (dim - value).abs() > 1.0e-9 || dim < 0.0 {
        return Err(EvalError::UnsupportedExpression {
            kind: "array constructor dimension",
        });
    }
    Ok(dim as usize)
}

fn try_eval_function_call_array_values<T: SimFloat>(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    source_span: rumoca_core::Span,
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    if let Some((resolved_name, Some(selection))) = resolve_runtime_special_target(name.as_str())
        && let Some(values) = eval_selected_runtime_special_array_output(
            resolved_name.as_str(),
            &selection,
            args,
            env,
        )?
    {
        return Ok(values);
    }
    let (resolved_name, selection) =
        resolve_user_function_target(name.as_str(), env).ok_or_else(|| {
            EvalError::MissingFunction {
                name: name.to_string(),
            }
        })?;
    if selection.is_some() {
        return eval_user_function_array_output_pub(name.var_name(), args, env);
    }
    let function =
        env.functions
            .get(resolved_name.as_str())
            .ok_or_else(|| EvalError::MissingFunction {
                name: resolved_name.to_string(),
            })?;
    let output = function
        .outputs
        .first()
        .ok_or(EvalError::UnsupportedExpression {
            kind: "function output shape",
        })?;
    if function_param_has_array_shape(output) {
        return eval_user_function_array_output_pub(name.var_name(), args, env);
    }
    let size = function_param_size(output)?;
    if size <= 1 {
        if let Some(values) = eval_vectorized_scalar_function_call(name, args, source_span, env)? {
            return Ok(values);
        }
        return Ok(vec![eval_expr(
            &rumoca_core::Expression::FunctionCall {
                name: name.clone(),
                args: args.to_vec(),
                is_constructor: false,
                span: source_span,
            },
            env,
        )?]);
    }

    let mut values = Vec::with_capacity(size);
    for flat_index in 0..size {
        let suffix =
            function_output_scalar_suffix(output, flat_index).ok_or(EvalError::ShapeMismatch {
                context: "function output shape",
                expected: size,
                actual: flat_index,
            })?;
        let output_path = format!("{}{}", output.name, suffix);
        values.push(eval_user_function_output_path_pub(
            name.var_name(),
            args,
            output_path.as_str(),
            env,
        )?);
    }
    Ok(values)
}

fn eval_vectorized_scalar_function_call<T: SimFloat>(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    source_span: rumoca_core::Span,
    env: &VarEnv<T>,
) -> Result<Option<Vec<T>>, EvalError> {
    let arg_values = args
        .iter()
        .map(|arg| eval_array_like_values(arg, env))
        .collect::<Result<Vec<_>, EvalError>>()?;
    let Some(len) = arg_values.iter().map(Vec::len).max() else {
        return Ok(None);
    };
    if len <= 1 {
        return Ok(None);
    }
    if arg_values
        .iter()
        .any(|values| values.is_empty() || (values.len() != 1 && values.len() != len))
    {
        return Ok(None);
    }

    Ok(Some(
        (0..len)
            .map(|idx| {
                let scalar_args = arg_values
                    .iter()
                    .map(|values| {
                        scalar_literal_expr(vectorized_arg_value(values, idx), source_span)
                    })
                    .collect::<Vec<_>>();
                eval_expr(
                    &rumoca_core::Expression::FunctionCall {
                        name: name.clone(),
                        args: scalar_args,
                        is_constructor: false,
                        span: source_span,
                    },
                    env,
                )
            })
            .collect::<Result<Vec<_>, EvalError>>()?,
    ))
}

fn vectorized_arg_value<T: SimFloat>(values: &[T], idx: usize) -> T {
    values[if values.len() == 1 { 0 } else { idx }]
}

fn scalar_literal_expr<T: SimFloat>(
    value: T,
    source_span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value.real()),
        span: source_span,
    }
}

fn function_param_size(param: &rumoca_core::FunctionParam) -> Result<usize, EvalError> {
    if param.dims.is_empty() {
        return Ok(1);
    }
    let mut size = 1usize;
    for &dim in &param.dims {
        let dim = usize::try_from(dim).map_err(|_| {
            EvalError::InvalidShape {
                context: "function output dimensions",
                reason: format!("dimension must be non-negative, got {dim}"),
            }
            .with_span_if_missing(param.span)
        })?;
        size = size.checked_mul(dim).ok_or_else(|| {
            EvalError::InvalidShape {
                context: "function output dimensions",
                reason: format!(
                    "dimension product for `{}` overflows usize: {:?}",
                    param.name, param.dims
                ),
            }
            .with_span_if_missing(param.span)
        })?;
    }
    Ok(size)
}

fn function_param_has_array_shape(param: &rumoca_core::FunctionParam) -> bool {
    !param.shape_expr.is_empty() || !param.dims.is_empty()
}

fn function_output_scalar_suffix(
    output: &rumoca_core::FunctionParam,
    flat_index: usize,
) -> Option<String> {
    if output.dims.is_empty() {
        return Some(String::new());
    }
    let subscripts = flat_index_to_subscripts(&output.dims, flat_index)?;
    let joined = subscripts
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(",");
    Some(format!("[{joined}]"))
}

fn flat_index_to_subscripts(dims: &[i64], flat_index: usize) -> Option<Vec<usize>> {
    if dims.is_empty() {
        return Some(Vec::new());
    }
    let dims = dims
        .iter()
        .map(|dim| usize::try_from(*dim).ok())
        .collect::<Option<Vec<_>>>()?;
    let total = dims.iter().product::<usize>();
    if flat_index >= total {
        return None;
    }
    let mut remaining = flat_index;
    let mut subscripts = Vec::with_capacity(dims.len());
    for dim in dims.iter().rev() {
        subscripts.push(remaining % dim + 1);
        remaining /= dim;
    }
    subscripts.reverse();
    Some(subscripts)
}

#[derive(Debug)]
struct RuntimeArrayOperand<T: SimFloat> {
    values: Vec<T>,
    dims: Vec<usize>,
}

pub(super) fn try_eval_cat_values<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    // MLS §10.4.2.1: cat(k, A, B, ...) concatenates along dimension k.
    let dim = checked_cat_dimension(args, env)?;
    let operands = args
        .iter()
        .skip(1)
        .map(|arg| {
            Ok(RuntimeArrayOperand {
                values: eval_array_like_values(arg, env)?,
                dims: try_infer_runtime_expr_dims(arg, env)?,
            })
        })
        .collect::<Result<Vec<_>, EvalError>>()?;
    match dim {
        1 => Ok(operands
            .into_iter()
            .flat_map(|operand| operand.values)
            .collect::<Vec<_>>()),
        2 => try_eval_cat_dim2_values(&operands),
        _ => Err(EvalError::UnsupportedExpression {
            kind: "cat dimension",
        }),
    }
}

fn checked_cat_dimension<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<usize, EvalError> {
    let dim_expr = args.first().ok_or(EvalError::UnsupportedExpression {
        kind: "cat arguments",
    })?;
    if args.len() <= 1 {
        return Err(EvalError::UnsupportedExpression {
            kind: "cat arguments",
        });
    }
    let dim = eval_expr::<T>(dim_expr, env)?.real();
    let rounded = dim.round();
    if !rounded.is_finite() || (rounded - dim).abs() > 1.0e-9 || rounded < 1.0 {
        return Err(EvalError::UnsupportedExpression {
            kind: "cat dimension",
        });
    }
    Ok(rounded as usize)
}

pub(super) fn try_infer_runtime_expr_dims<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Vec<usize>, EvalError> {
    if let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = expr
        && subscripts.is_empty()
        && let Some(dims) = env.dims.get(name.as_str())
    {
        return infer_declared_or_value_dims(dims, 0);
    }
    if let rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args,
        ..
    } = expr
    {
        let arg = args
            .first()
            .ok_or(EvalError::UnsupportedExpression { kind: "der arity" })?;
        return try_infer_runtime_expr_dims(arg, env);
    }
    if let rumoca_core::Expression::Array {
        elements,
        is_matrix,
        ..
    } = expr
    {
        return if *is_matrix {
            try_runtime_matrix_literal_dims(elements, env)
        } else {
            Ok(runtime_vector_dims(elements.len()))
        };
    }
    if let rumoca_core::Expression::Tuple { elements, .. } = expr {
        return Ok(runtime_vector_dims(elements.len()));
    }

    let values = eval_array_like_values(expr, env)?;
    let dims = match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            if let Some(dims) = env.dims.get(name.as_str()) {
                infer_declared_or_value_dims(dims, values.len())?
            } else if values.len() <= 1 {
                Vec::new()
            } else {
                declared_dims(name.as_str(), env)?
                    .into_iter()
                    .map(|dim| {
                        usize::try_from(dim).map_err(|_| EvalError::UnsupportedExpression {
                            kind: "array dimensions",
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?
            }
        }
        rumoca_core::Expression::Array {
            elements,
            is_matrix: true,
            ..
        } => try_runtime_matrix_literal_dims(elements, env)?,
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Cat,
            args,
            ..
        } => try_infer_runtime_cat_dims(args, env)?,
        rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
            ..
        } => function_call_runtime_dims(name, args, values.len(), env)?,
        rumoca_core::Expression::BuiltinCall { function, args, .. } => {
            builtin_call_runtime_dims(*function, args, values.len(), env)?
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            binary_expr_runtime_dims(op, lhs, rhs, values.len(), env)?
        }
        rumoca_core::Expression::Unary { rhs, .. } => try_infer_runtime_expr_dims(rhs, env)?,
        rumoca_core::Expression::Array { .. }
        | rumoca_core::Expression::Tuple { .. }
        | rumoca_core::Expression::Range { .. }
        | rumoca_core::Expression::ArrayComprehension { .. } => runtime_vector_dims(values.len()),
        _ => runtime_vector_dims(values.len()),
    };
    Ok(dims)
}

/// Result shape of an array-valued binary expression (MLS §10.6).
///
/// Elementwise and scalar-broadcast operators preserve the array operand's
/// shape; `Mul` follows matrix-product shape rules.
fn binary_expr_runtime_dims<T: SimFloat>(
    op: &rumoca_core::OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    value_count: usize,
    env: &VarEnv<T>,
) -> Result<Vec<usize>, EvalError> {
    let (lhs_dims, rhs_dims) = match (
        try_infer_runtime_expr_dims(lhs, env),
        try_infer_runtime_expr_dims(rhs, env),
    ) {
        (Ok(lhs_dims), Ok(rhs_dims)) => (lhs_dims, rhs_dims),
        // Operand shapes the inferencer cannot express fall back to the
        // flat value count, matching the pre-existing generic behavior.
        _ => return Ok(runtime_vector_dims(value_count)),
    };
    if matches!(op, rumoca_core::OpBinary::Mul) {
        return Ok(match (lhs_dims.as_slice(), rhs_dims.as_slice()) {
            ([rows, _], [_, cols]) => vec![*rows, *cols],
            ([rows, _], [_]) => vec![*rows],
            ([_], [_, cols]) => vec![*cols],
            ([_], [_]) => Vec::new(), // vector dot product is a scalar
            ([], _) => rhs_dims.clone(),
            (_, []) => lhs_dims.clone(),
            _ => runtime_vector_dims(value_count),
        });
    }
    Ok(if lhs_dims.is_empty() { rhs_dims } else { lhs_dims })
}

/// Runtime result shape for shape-defining builtin calls (MLS §10.3).
fn builtin_call_runtime_dims<T: SimFloat>(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    value_count: usize,
    env: &VarEnv<T>,
) -> Result<Vec<usize>, EvalError> {
    use rumoca_core::BuiltinFunction as B;
    let usize_arg = |expr: &rumoca_core::Expression| -> Result<usize, EvalError> {
        let value = eval_expr::<T>(expr, env)?.real();
        if !value.is_finite() || value.fract() != 0.0 || value < 0.0 {
            return Err(EvalError::UnsupportedExpression {
                kind: "builtin dimension argument",
            });
        }
        Ok(value as usize)
    };
    fn single_arg(
        args: &[rumoca_core::Expression],
    ) -> Result<&rumoca_core::Expression, EvalError> {
        args.first().ok_or(EvalError::UnsupportedExpression {
            kind: "builtin dimension arity",
        })
    }
    match function {
        B::Identity => {
            let n = usize_arg(single_arg(args)?)?;
            Ok(vec![n, n])
        }
        B::Zeros | B::Ones => args.iter().map(usize_arg).collect(),
        B::Fill => args.iter().skip(1).map(usize_arg).collect(),
        B::Diagonal => {
            let n = eval_array_like_values::<T>(single_arg(args)?, env)?.len();
            Ok(vec![n, n])
        }
        B::Transpose => {
            let mut dims = try_infer_runtime_expr_dims(single_arg(args)?, env)?;
            if dims.len() != 2 {
                return Err(EvalError::UnsupportedExpression {
                    kind: "transpose operand shape",
                });
            }
            dims.reverse();
            Ok(dims)
        }
        B::Symmetric => try_infer_runtime_expr_dims(single_arg(args)?, env),
        B::OuterProduct => {
            let rows = eval_array_like_values::<T>(single_arg(args)?, env)?.len();
            let cols = eval_array_like_values::<T>(
                args.get(1).ok_or(EvalError::UnsupportedExpression {
                    kind: "builtin dimension arity",
                })?,
                env,
            )?
            .len();
            Ok(vec![rows, cols])
        }
        B::Skew => Ok(vec![3, 3]),
        B::Cross => Ok(vec![3]),
        _ => Ok(runtime_vector_dims(value_count)),
    }
}

fn function_call_runtime_dims<T: SimFloat>(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    value_count: usize,
    env: &VarEnv<T>,
) -> Result<Vec<usize>, EvalError> {
    if let Some((resolved_name, Some(selection))) = resolve_runtime_special_target(name.as_str()) {
        let Some(values) = eval_selected_runtime_special_array_output(
            resolved_name.as_str(),
            &selection,
            args,
            env,
        )?
        else {
            return Err(EvalError::UnsupportedExpression {
                kind: "function output shape",
            });
        };
        return Ok(runtime_vector_dims(values.len()));
    }

    let (resolved_name, selection) = resolve_user_function_target(name.as_str(), env).ok_or(
        EvalError::UnsupportedExpression {
            kind: "function output shape",
        },
    )?;
    let function =
        env.functions
            .get(resolved_name.as_str())
            .ok_or(EvalError::UnsupportedExpression {
                kind: "function output shape",
            })?;
    let output = match selection {
        Some(selection) => function
            .outputs
            .iter()
            .find(|output| output.name == selection.output_name())
            .ok_or(EvalError::UnsupportedExpression {
                kind: "function output shape",
            })?,
        None => function
            .outputs
            .first()
            .ok_or(EvalError::UnsupportedExpression {
                kind: "function output shape",
            })?,
    };
    let dims = resolve_user_function_output_dims_pub(
        &resolved_name,
        args,
        Some(output.name.as_str()),
        env,
    )?;
    Ok(dims
        .map(|dims| {
            dims.into_iter()
                .map(|dim| {
                    usize::try_from(dim).map_err(|_| EvalError::UnsupportedExpression {
                        kind: "function output shape",
                    })
                })
                .collect::<Result<Vec<_>, _>>()
        })
        .transpose()?
        .unwrap_or_else(|| runtime_vector_dims(value_count)))
}

fn runtime_vector_dims(len: usize) -> Vec<usize> {
    (len > 1).then_some(len).into_iter().collect()
}

fn infer_declared_or_value_dims(dims: &[i64], value_count: usize) -> Result<Vec<usize>, EvalError> {
    if value_count == 0 && !dims.is_empty() {
        return dims
            .iter()
            .map(|dim| {
                usize::try_from(*dim).map_err(|_| EvalError::UnsupportedExpression {
                    kind: "array dimensions",
                })
            })
            .collect();
    }
    infer_dims_from_values(dims, value_count)
}

fn try_runtime_matrix_literal_dims<T: SimFloat>(
    elements: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Vec<usize>, EvalError> {
    if elements.is_empty() {
        return Ok(Vec::new());
    }
    let rows = elements.len();
    let cols = elements
        .iter()
        .map(|element| match element {
            rumoca_core::Expression::Array { elements, .. }
            | rumoca_core::Expression::Tuple { elements, .. } => Ok(elements.len()),
            _ => eval_array_like_values::<T>(element, env).map(|values| values.len()),
        })
        .collect::<Result<Vec<_>, EvalError>>()?
        .into_iter()
        .max()
        .ok_or(EvalError::UnsupportedExpression {
            kind: "matrix shape",
        })?;
    if cols == 0 {
        Ok(Vec::new())
    } else {
        Ok(vec![rows, cols])
    }
}

fn try_infer_runtime_cat_dims<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Vec<usize>, EvalError> {
    if args.len() <= 1 {
        return Err(EvalError::UnsupportedExpression {
            kind: "cat arguments",
        });
    }
    let dim = checked_cat_dimension(args, env)?;
    let mut operands = args
        .iter()
        .skip(1)
        .map(|arg| try_infer_runtime_expr_dims(arg, env));
    let first = operands.next().ok_or(EvalError::UnsupportedExpression {
        kind: "cat arguments",
    })??;
    match dim {
        1 if first.is_empty() => Ok(runtime_vector_dims(args.len() - 1)),
        1 => {
            let mut total = first[0];
            let tail = &first[1..];
            for dims in operands {
                let dims = dims?;
                if dims.len() != first.len() || &dims[1..] != tail {
                    return Err(EvalError::UnsupportedExpression { kind: "cat shape" });
                }
                total += dims[0];
            }
            let mut dims = vec![total];
            dims.extend_from_slice(tail);
            Ok(dims)
        }
        2 => {
            let [rows, cols] = first.as_slice() else {
                return Err(EvalError::UnsupportedExpression { kind: "cat shape" });
            };
            let rows = *rows;
            let mut total_cols = *cols;
            for dims in operands {
                let dims = dims?;
                let [operand_rows, operand_cols] = dims.as_slice() else {
                    return Err(EvalError::UnsupportedExpression { kind: "cat shape" });
                };
                if *operand_rows != rows {
                    return Err(EvalError::ShapeMismatch {
                        context: "cat rows",
                        expected: rows,
                        actual: *operand_rows,
                    });
                }
                total_cols += *operand_cols;
            }
            Ok(vec![rows, total_cols])
        }
        _ => Err(EvalError::UnsupportedExpression {
            kind: "cat dimension",
        }),
    }
}

fn try_eval_cat_dim2_values<T: SimFloat>(
    operands: &[RuntimeArrayOperand<T>],
) -> Result<Vec<T>, EvalError> {
    let Some(first) = operands.first() else {
        return Err(EvalError::UnsupportedExpression {
            kind: "cat arguments",
        });
    };
    let [rows, _] = first.dims.as_slice() else {
        return Err(EvalError::UnsupportedExpression { kind: "cat shape" });
    };
    for operand in operands {
        let [operand_rows, _] = operand.dims.as_slice() else {
            return Err(EvalError::UnsupportedExpression { kind: "cat shape" });
        };
        if *operand_rows != *rows {
            return Err(EvalError::ShapeMismatch {
                context: "cat rows",
                expected: *rows,
                actual: *operand_rows,
            });
        }
    }
    let mut out = Vec::new();
    for row in 0..*rows {
        for operand in operands {
            let [_, cols] = operand.dims.as_slice() else {
                return Err(EvalError::UnsupportedExpression { kind: "cat shape" });
            };
            let start = row * cols;
            let end = start + cols;
            if end > operand.values.len() {
                return Err(EvalError::ShapeMismatch {
                    context: "cat operand values",
                    expected: end,
                    actual: operand.values.len(),
                });
            }
            out.extend_from_slice(&operand.values[start..end]);
        }
    }
    Ok(out)
}

pub(super) fn eval_linspace_values<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::UnsupportedExpression {
            kind: "linspace arity",
        });
    }
    let start = eval_expr::<T>(&args[0], env)?.real();
    let end = eval_expr::<T>(&args[1], env)?.real();
    let n = eval_expr::<T>(&args[2], env)?.real().round() as i64;
    if n < 2 {
        return Err(EvalError::UnsupportedExpression {
            kind: "linspace size",
        });
    }
    let n_usize = n as usize;
    let step = (end - start) / ((n_usize - 1) as f64);
    let mut out: Vec<T> = (0..n_usize)
        .map(|i| T::from_f64(start + step * i as f64))
        .collect();
    if let Some(last) = out.last_mut() {
        *last = T::from_f64(end);
    }
    Ok(out)
}

pub(super) fn eval_array_like_f64_values<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Vec<f64>, EvalError> {
    Ok(eval_array_like_values(expr, env)?
        .into_iter()
        .map(|v| v.real())
        .collect())
}

pub(super) fn eval_columns_arg<T: SimFloat>(
    expr: Option<&rumoca_core::Expression>,
    env: &VarEnv<T>,
) -> Result<Vec<usize>, EvalError> {
    let Some(expr) = expr else {
        return Ok(Vec::new());
    };
    if matches!(expr, rumoca_core::Expression::Empty { .. }) {
        return Ok(Vec::new());
    }
    Ok(eval_array_like_f64_values(expr, env)?
        .into_iter()
        .map(|v| v.round() as i64)
        .filter(|v| *v > 0)
        .map(|v| v as usize)
        .collect())
}
