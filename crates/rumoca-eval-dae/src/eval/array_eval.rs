use super::*;

pub(super) fn declared_dims_or_scalar<T: SimFloat>(name: &str, env: &VarEnv<T>) -> Vec<i64> {
    match env.dims.get(name) {
        Some(dims) => dims.clone(),
        None => Vec::new(),
    }
}

/// Evaluate an expression as a flattened array of scalar values.
///
/// Nested array literals are flattened recursively; scalar expressions produce
/// a single-element vector.
pub fn eval_array_values<T: SimFloat>(expr: &rumoca_core::Expression, env: &VarEnv<T>) -> Vec<T> {
    let mut out = Vec::new();
    collect_array_values(expr, env, &mut out);
    out
}

pub fn eval_shaped_array_values<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
    expected_len: usize,
) -> Result<Vec<T>, EvalError> {
    let values = try_eval_array_like_values(expr, env)?;
    if values.len() != expected_len {
        return Err(EvalError::ShapeMismatch {
            context: "shaped array value",
            expected: expected_len,
            actual: values.len(),
        });
    }
    Ok(values)
}

pub(super) fn try_eval_array_like_values<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            if let Some(values) = encoded_slice_field_values(name.as_str(), env) {
                return Ok(values);
            }
            if let Some(values) = array_values_from_env_name_generic(name.as_str(), env) {
                return Ok(values);
            }
            Ok(vec![eval_expr(expr, env)?])
        }
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            try_eval_field_access_array_values(base, field, env)
        }
        rumoca_core::Expression::BuiltinCall { function, args, .. } => {
            try_eval_builtin_array_like_values(expr, *function, args, env)
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            try_eval_binary_array_values(op, lhs, rhs, env)
        }
        rumoca_core::Expression::Unary { op, rhs, .. } => try_eval_unary_array_values(op, rhs, env),
        rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
            ..
        } => try_eval_function_call_array_values(name, args, env),
        rumoca_core::Expression::Array { .. }
        | rumoca_core::Expression::Tuple { .. }
        | rumoca_core::Expression::Range { .. }
        | rumoca_core::Expression::If { .. }
        | rumoca_core::Expression::ArrayComprehension { .. } => Ok(eval_array_values(expr, env)),
        _ => Ok(vec![eval_expr(expr, env)?]),
    }
}

fn try_eval_builtin_array_like_values<T: SimFloat>(
    expr: &rumoca_core::Expression,
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    match function {
        rumoca_core::BuiltinFunction::Cat => Ok(eval_cat_values(args, env)),
        rumoca_core::BuiltinFunction::Linspace => Ok(eval_linspace_values(args, env)),
        rumoca_core::BuiltinFunction::Zeros
        | rumoca_core::BuiltinFunction::Ones
        | rumoca_core::BuiltinFunction::Fill
        | rumoca_core::BuiltinFunction::Identity
        | rumoca_core::BuiltinFunction::Diagonal => eval_array_constructor_values(
            &function, args, env,
        )
        .ok_or(EvalError::UnsupportedExpression {
            kind: "array constructor shape",
        }),
        rumoca_core::BuiltinFunction::Transpose => {
            eval_transpose_values(args, env).ok_or(EvalError::UnsupportedExpression {
                kind: "transpose shape",
            })
        }
        rumoca_core::BuiltinFunction::Cross => {
            eval_cross_values(args, env).ok_or(EvalError::UnsupportedExpression {
                kind: "cross product shape",
            })
        }
        rumoca_core::BuiltinFunction::Skew => eval_skew_values(args, env)
            .ok_or(EvalError::UnsupportedExpression { kind: "skew shape" }),
        rumoca_core::BuiltinFunction::OuterProduct => {
            eval_outer_product_values(args, env).ok_or(EvalError::UnsupportedExpression {
                kind: "outerProduct shape",
            })
        }
        rumoca_core::BuiltinFunction::Symmetric => {
            eval_symmetric_values(args, env).ok_or(EvalError::UnsupportedExpression {
                kind: "symmetric shape",
            })
        }
        rumoca_core::BuiltinFunction::Vector if args.len() == 1 => {
            try_eval_array_like_values(&args[0], env)
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
    let values = try_eval_array_like_values(&args[0], env)?;
    if values.len() > 1
        && let Some(mapped) = eval_unary_builtin_array_values(function, values)
    {
        return Ok(mapped);
    }
    Ok(vec![eval_expr(expr, env)?])
}

fn collect_array_values<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
    out: &mut Vec<T>,
) {
    match expr {
        rumoca_core::Expression::Array {
            elements,
            is_matrix,
            ..
        } => collect_array_literal_values(elements, *is_matrix, env, out),
        rumoca_core::Expression::Tuple { elements, .. } => {
            for element in elements {
                collect_array_values(element, env, out);
            }
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => collect_if_values(branches, else_branch, env, out),
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            collect_range_values(start, step, end, env, out);
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => collect_array_comprehension_values(expr, indices, filter.as_deref(), env, out),
        rumoca_core::Expression::Binary { .. } | rumoca_core::Expression::Unary { .. } => {
            out.extend(eval_array_like_values::<T>(expr, env));
        }
        rumoca_core::Expression::BuiltinCall {
            function:
                rumoca_core::BuiltinFunction::Cat
                | rumoca_core::BuiltinFunction::Cross
                | rumoca_core::BuiltinFunction::Diagonal
                | rumoca_core::BuiltinFunction::Fill
                | rumoca_core::BuiltinFunction::Identity
                | rumoca_core::BuiltinFunction::Linspace
                | rumoca_core::BuiltinFunction::Ones
                | rumoca_core::BuiltinFunction::OuterProduct
                | rumoca_core::BuiltinFunction::Skew
                | rumoca_core::BuiltinFunction::Symmetric
                | rumoca_core::BuiltinFunction::Transpose
                | rumoca_core::BuiltinFunction::Vector
                | rumoca_core::BuiltinFunction::Zeros,
            ..
        } => {
            out.extend(eval_array_like_values::<T>(expr, env));
        }
        rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
            ..
        } => {
            if let Some(values) = eval_function_call_array_values(name, args, env) {
                out.extend(values);
            } else {
                out.push(eval_expr_or_default::<T>(expr, env));
            }
        }
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            if let Some(values) = array_values_from_env_name_generic(name.as_str(), env) {
                out.extend(values);
            } else {
                out.push(eval_expr_or_default::<T>(expr, env));
            }
        }
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            if let Some(values) = eval_field_access_array_values(base, field, env) {
                out.extend(values);
            } else {
                out.push(eval_expr_or_default::<T>(expr, env));
            }
        }
        _ => out.push(eval_expr_or_default::<T>(expr, env)),
    }
}

fn collect_array_literal_values<T: SimFloat>(
    elements: &[rumoca_core::Expression],
    is_matrix: bool,
    env: &VarEnv<T>,
    out: &mut Vec<T>,
) {
    if can_interleave_matrix_columns(elements, is_matrix)
        && interleave_matrix_columns(elements, env, out)
    {
        return;
    }
    for element in elements {
        collect_array_values(element, env, out);
    }
}

fn can_interleave_matrix_columns(elements: &[rumoca_core::Expression], is_matrix: bool) -> bool {
    is_matrix
        && !elements.is_empty()
        && !elements
            .iter()
            .all(|element| matches!(element, rumoca_core::Expression::Array { .. }))
}

fn interleave_matrix_columns<T: SimFloat>(
    elements: &[rumoca_core::Expression],
    env: &VarEnv<T>,
    out: &mut Vec<T>,
) -> bool {
    let columns: Vec<Vec<T>> = elements
        .iter()
        .map(|element| eval_array_like_values::<T>(element, env))
        .collect();
    let row_count = columns.iter().map(Vec::len).max().unwrap_or(0);
    if row_count == 0 {
        return false;
    }

    for row in 0..row_count {
        for column in &columns {
            out.push(interleaved_column_value(column, row));
        }
    }
    true
}

fn interleaved_column_value<T: SimFloat>(column: &[T], row: usize) -> T {
    if column.is_empty() {
        return T::zero();
    }
    if row < column.len() {
        return column[row];
    }
    if column.len() == 1 {
        return column[0];
    }
    *column.last().unwrap_or(&T::zero())
}

fn collect_if_values<T: SimFloat>(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    env: &VarEnv<T>,
    out: &mut Vec<T>,
) {
    for (cond, then_expr) in branches {
        if eval_condition_truth(cond, env) {
            collect_array_values(then_expr, env, out);
            return;
        }
    }
    collect_array_values(else_branch, env, out);
}

fn collect_range_values<T: SimFloat>(
    start: &rumoca_core::Expression,
    step: &Option<Box<rumoca_core::Expression>>,
    end: &rumoca_core::Expression,
    env: &VarEnv<T>,
    out: &mut Vec<T>,
) {
    let start_v = eval_expr_or_default::<T>(start, env).real();
    let end_v = eval_expr_or_default::<T>(end, env).real();
    let step_v = step
        .as_ref()
        .map(|step_expr| eval_expr_or_default::<T>(step_expr, env).real())
        .unwrap_or_else(|| if end_v >= start_v { 1.0 } else { -1.0 });
    if !start_v.is_finite()
        || !end_v.is_finite()
        || !step_v.is_finite()
        || step_v.abs() <= f64::EPSILON
    {
        return;
    }
    extend_range_values(start_v, end_v, step_v, out);
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

fn collect_array_comprehension_values<T: SimFloat>(
    expr: &rumoca_core::Expression,
    indices: &[rumoca_core::ComprehensionIndex],
    filter: Option<&rumoca_core::Expression>,
    env: &VarEnv<T>,
    out: &mut Vec<T>,
) {
    expand_array_comprehension(0, expr, indices, filter, env, out);
}

fn expand_array_comprehension<T: SimFloat>(
    level: usize,
    expr: &rumoca_core::Expression,
    indices: &[rumoca_core::ComprehensionIndex],
    filter: Option<&rumoca_core::Expression>,
    env: &VarEnv<T>,
    out: &mut Vec<T>,
) {
    if level >= indices.len() {
        if filter.is_none_or(|f| eval_expr_or_default::<T>(f, env).to_bool()) {
            collect_array_values(expr, env, out);
        }
        return;
    }

    let index = &indices[level];
    for value in eval_array_values::<T>(&index.range, env) {
        let mut local_env = env.clone();
        local_env.set(index.name.as_str(), value);
        expand_array_comprehension(level + 1, expr, indices, filter, &local_env, out);
    }
}

pub(super) fn reshape_flat_matrix(flat_values: &[f64], rows: usize, cols: usize) -> Vec<Vec<f64>> {
    let mut matrix = Vec::with_capacity(rows);
    for r in 0..rows {
        let start = r.saturating_mul(cols).min(flat_values.len());
        let end = start.saturating_add(cols).min(flat_values.len());
        let mut row = flat_values[start..end].to_vec();
        row.resize(cols, 0.0);
        matrix.push(row);
    }
    matrix
}

fn reshape_flat_matrix_generic<T: SimFloat>(
    flat_values: &[T],
    rows: usize,
    cols: usize,
) -> Vec<Vec<T>> {
    let mut matrix = Vec::with_capacity(rows);
    for r in 0..rows {
        let start = r.saturating_mul(cols).min(flat_values.len());
        let end = start.saturating_add(cols).min(flat_values.len());
        let mut row = flat_values[start..end].to_vec();
        row.resize(cols, T::zero());
        matrix.push(row);
    }
    matrix
}

fn transpose_matrix<T: SimFloat>(matrix: Vec<Vec<T>>) -> Vec<Vec<T>> {
    let rows = matrix.len();
    let cols = matrix.iter().map(Vec::len).max().unwrap_or(0);
    if rows == 0 || cols == 0 {
        return Vec::new();
    }

    let mut out = vec![vec![T::zero(); rows]; cols];
    for (r, row) in matrix.iter().enumerate() {
        for (c, out_col) in out.iter_mut().enumerate().take(cols) {
            if let Some(value) = row.get(c) {
                out_col[r] = *value;
            }
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
) -> Vec<Vec<T>> {
    let mut rows = Vec::new();
    for element in elements {
        match element {
            rumoca_core::Expression::Array {
                elements: row_elements,
                ..
            } => rows.extend(eval_matrix_literal_row(row_elements, env)),
            _ => rows.push(eval_array_values::<T>(element, env)),
        }
    }
    rows
}

fn eval_matrix_literal_row<T: SimFloat>(
    row_elements: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Vec<Vec<T>> {
    // MLS §10.6: matrix rows may be formed from array-valued expressions. In
    // that case each row expression contributes columns, not one ragged row.
    let columns: Vec<Vec<T>> = row_elements
        .iter()
        .map(|element| eval_array_like_values::<T>(element, env))
        .collect();
    let row_count = columns.iter().map(Vec::len).max().unwrap_or(0);
    if row_count == 0 {
        return vec![Vec::new()];
    }
    (0..row_count)
        .map(|row| {
            columns
                .iter()
                .map(|column| interleaved_column_value(column, row))
                .collect()
        })
        .collect()
}

pub fn eval_matrix_values<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Option<Vec<Vec<T>>> {
    match expr {
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            eval_binary_matrix_values(op, lhs, rhs, env)
        }
        rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
            ..
        } => eval_function_call_matrix_values(name, args, env),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            let flat_values = array_values_from_env_name_generic(name.as_str(), env)?;
            if flat_values.is_empty() {
                return Some(Vec::new());
            }
            let raw_dims = declared_dims_or_scalar(name.as_str(), env);
            let inferred = infer_dims_from_values(&raw_dims, flat_values.len());
            if inferred.len() >= 2 {
                return Some(reshape_flat_matrix_generic(
                    &flat_values,
                    inferred[0].max(1),
                    inferred[1].max(1),
                ));
            }
            None
        }
        rumoca_core::Expression::Array { elements, .. }
            if elements
                .iter()
                .all(|element| matches!(element, rumoca_core::Expression::Array { .. })) =>
        {
            Some(eval_matrix_literal_rows(elements, env))
        }
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Transpose,
            args,
            ..
        } if args.len() == 1 => eval_matrix_values(&args[0], env).map(transpose_matrix),
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
            let values = eval_array_like_values(&args[0], env);
            Some(diagonal_matrix(values))
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                if eval_expr_or_default::<T>(condition, env).real() != 0.0 {
                    return eval_matrix_values(value, env);
                }
            }
            eval_matrix_values(else_branch, env)
        }
        _ => None,
    }
}

fn eval_binary_matrix_values<T: SimFloat>(
    op: &OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Option<Vec<Vec<T>>> {
    match op {
        OpBinary::Mul => eval_matrix_matrix_product_rows(lhs, rhs, env),
        _ => None,
    }
}

fn eval_function_call_matrix_values<T: SimFloat>(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Option<Vec<Vec<T>>> {
    let function = env.functions.get(name.as_str())?;
    let output = function.outputs.first()?;
    if output.dims.len() < 2 {
        return None;
    }
    let rows = usize::try_from(output.dims[0])
        .ok()
        .filter(|rows| *rows > 0)?;
    let cols = usize::try_from(output.dims[1])
        .ok()
        .filter(|cols| *cols > 0)?;
    let values = eval_function_call_array_values(name, args, env)?;
    if values.len() != rows.checked_mul(cols)? {
        return None;
    }
    Some(reshape_flat_matrix_generic(&values, rows, cols))
}

pub(super) fn eval_matrix_index<T: SimFloat>(
    expr: &rumoca_core::Expression,
    indices: &[usize],
    env: &VarEnv<T>,
) -> Option<T> {
    if indices.len() != 2 {
        return None;
    }
    let matrix = eval_matrix_values(expr, env)?;
    let row = indices[0].checked_sub(1)?;
    let col = indices[1].checked_sub(1)?;
    matrix.get(row)?.get(col).copied()
}

pub(super) fn eval_transpose_values<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Option<Vec<T>> {
    let arg = args.first()?;
    let matrix = eval_matrix_values(arg, env)?;
    Some(flatten_matrix(&transpose_matrix(matrix)))
}

pub(super) fn eval_cross_values<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Option<Vec<T>> {
    if args.len() != 2 {
        return None;
    }
    let lhs = eval_array_like_values(&args[0], env);
    let rhs = eval_array_like_values(&args[1], env);
    if lhs.len() != 3 || rhs.len() != 3 {
        return None;
    }
    Some(vec![
        lhs[1] * rhs[2] - lhs[2] * rhs[1],
        lhs[2] * rhs[0] - lhs[0] * rhs[2],
        lhs[0] * rhs[1] - lhs[1] * rhs[0],
    ])
}

pub(super) fn eval_skew_values<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Option<Vec<T>> {
    let values = eval_array_like_values(args.first()?, env);
    if values.len() != 3 {
        return None;
    }
    let zero = T::default();
    Some(vec![
        zero, -values[2], values[1], values[2], zero, -values[0], -values[1], values[0], zero,
    ])
}

pub(super) fn eval_outer_product_values<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Option<Vec<T>> {
    if args.len() != 2 {
        return None;
    }
    let lhs = eval_array_like_values(&args[0], env);
    let rhs = eval_array_like_values(&args[1], env);
    Some(
        lhs.iter()
            .flat_map(|lhs_value| rhs.iter().map(move |rhs_value| *lhs_value * *rhs_value))
            .collect(),
    )
}

pub(super) fn eval_symmetric_values<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Option<Vec<T>> {
    let matrix = eval_matrix_values(args.first()?, env)?;
    let n = matrix.len();
    if n == 0 || matrix.iter().any(|row| row.len() != n) {
        return None;
    }
    let mut out = Vec::with_capacity(n * n);
    for row in 0..n {
        for col in 0..n {
            let (source_row, source_col) = if row >= col { (row, col) } else { (col, row) };
            out.push(matrix[source_row][source_col]);
        }
    }
    Some(out)
}

fn eval_matrix_vector_product<T: SimFloat>(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Option<Vec<T>> {
    let matrix = eval_matrix_values(lhs, env)?;
    let vector = eval_array_like_values(rhs, env);
    let cols = matrix.iter().map(Vec::len).max().unwrap_or(0);
    if matrix.is_empty() || cols == 0 || vector.len() != cols {
        return None;
    }

    Some(
        matrix
            .iter()
            .map(|row| {
                (0..cols).fold(T::zero(), |acc, col| {
                    acc + row.get(col).copied().unwrap_or_else(T::zero) * vector[col]
                })
            })
            .collect(),
    )
}

fn eval_vector_matrix_product<T: SimFloat>(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Option<Vec<T>> {
    let vector = eval_array_like_values(lhs, env);
    let matrix = eval_matrix_values(rhs, env)?;
    let rows = matrix.len();
    let cols = matrix.iter().map(Vec::len).max().unwrap_or(0);
    if rows == 0 || cols == 0 || vector.len() != rows {
        return None;
    }

    Some(
        (0..cols)
            .map(|col| {
                (0..rows).fold(T::zero(), |acc, row| {
                    acc + vector[row] * matrix[row].get(col).copied().unwrap_or_else(T::zero)
                })
            })
            .collect(),
    )
}

fn eval_matrix_matrix_product<T: SimFloat>(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Option<Vec<T>> {
    eval_matrix_matrix_product_rows(lhs, rhs, env).map(|rows| flatten_matrix(&rows))
}

fn eval_matrix_matrix_product_rows<T: SimFloat>(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Option<Vec<Vec<T>>> {
    let lhs_matrix = eval_matrix_values(lhs, env)?;
    let rhs_matrix = eval_matrix_values(rhs, env)?;
    let rows = lhs_matrix.len();
    let inner = lhs_matrix.iter().map(Vec::len).max().unwrap_or(0);
    let rhs_rows = rhs_matrix.len();
    let cols = rhs_matrix.iter().map(Vec::len).max().unwrap_or(0);
    if rows == 0 || inner == 0 || rhs_rows != inner || cols == 0 {
        return None;
    }

    Some(
        (0..rows)
            .map(|row| {
                (0..cols)
                    .map(|col| {
                        (0..inner).fold(T::zero(), |acc, k| {
                            let l = lhs_matrix[row].get(k).copied().unwrap_or_else(T::zero);
                            let r = rhs_matrix[k].get(col).copied().unwrap_or_else(T::zero);
                            acc + l * r
                        })
                    })
                    .collect()
            })
            .collect(),
    )
}

pub(super) fn eval_binary_array_values<T: SimFloat>(
    op: &OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Option<Vec<T>> {
    match op {
        OpBinary::Mul => eval_matrix_matrix_product(lhs, rhs, env)
            .or_else(|| eval_matrix_vector_product(lhs, rhs, env))
            .or_else(|| eval_vector_matrix_product(lhs, rhs, env))
            .or_else(|| eval_elementwise_binary_array_values(lhs, rhs, env, |l, r| l * r)),
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
        _ => None,
    }
}

fn try_eval_binary_array_values<T: SimFloat>(
    op: &OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    match op {
        OpBinary::Mul => eval_matrix_matrix_product(lhs, rhs, env)
            .or_else(|| eval_matrix_vector_product(lhs, rhs, env))
            .or_else(|| eval_vector_matrix_product(lhs, rhs, env))
            .map(Ok)
            .unwrap_or_else(|| {
                try_eval_elementwise_binary_array_values(lhs, rhs, env, |l, r| l * r)
            }),
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
                span: rumoca_core::Span::DUMMY,
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
) -> Option<Vec<T>> {
    let lhs_values = eval_array_like_values(lhs, env);
    let rhs_values = eval_array_like_values(rhs, env);
    let len = match (lhs_values.len(), rhs_values.len()) {
        (0, _) | (_, 0) => return None,
        (1, 1) => return None,
        (1, rhs_len) => rhs_len,
        (lhs_len, 1) => lhs_len,
        (lhs_len, rhs_len) if lhs_len == rhs_len => lhs_len,
        _ => return None,
    };
    Some(
        (0..len)
            .map(|idx| {
                let lhs_value = lhs_values
                    .get(if lhs_values.len() == 1 { 0 } else { idx })
                    .copied()
                    .unwrap_or_else(T::zero);
                let rhs_value = rhs_values
                    .get(if rhs_values.len() == 1 { 0 } else { idx })
                    .copied()
                    .unwrap_or_else(T::zero);
                op(lhs_value, rhs_value)
            })
            .collect(),
    )
}

fn try_eval_elementwise_binary_array_values<T: SimFloat>(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
    op: impl Fn(T, T) -> T,
) -> Result<Vec<T>, EvalError> {
    let lhs_values = try_eval_array_like_values(lhs, env)?;
    let rhs_values = try_eval_array_like_values(rhs, env)?;
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

pub(super) fn eval_array_like_values<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Vec<T> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            if let Some(values) = encoded_slice_field_values(name.as_str(), env) {
                return values;
            }
            if let Some(values) = array_values_from_env_name_generic(name.as_str(), env) {
                return values;
            }
            vec![eval_expr_or_default::<T>(expr, env)]
        }
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            if let Some(values) = eval_field_access_array_values(base, field, env) {
                return values;
            }
            vec![eval_expr_or_default::<T>(expr, env)]
        }
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Cat,
            args,
            ..
        } => eval_cat_values(args, env),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Linspace,
            args,
            ..
        } => eval_linspace_values(args, env),
        rumoca_core::Expression::BuiltinCall {
            function:
                function @ (rumoca_core::BuiltinFunction::Zeros
                | rumoca_core::BuiltinFunction::Ones
                | rumoca_core::BuiltinFunction::Fill
                | rumoca_core::BuiltinFunction::Identity
                | rumoca_core::BuiltinFunction::Diagonal),
            args,
            ..
        } => eval_array_constructor_values(function, args, env)
            .unwrap_or_else(|| vec![eval_expr_or_default::<T>(expr, env)]),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Transpose,
            args,
            ..
        } => eval_transpose_values(args, env)
            .unwrap_or_else(|| vec![eval_expr_or_default::<T>(expr, env)]),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Cross,
            args,
            ..
        } => eval_cross_values(args, env)
            .unwrap_or_else(|| vec![eval_expr_or_default::<T>(expr, env)]),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Skew,
            args,
            ..
        } => eval_skew_values(args, env)
            .unwrap_or_else(|| vec![eval_expr_or_default::<T>(expr, env)]),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::OuterProduct,
            args,
            ..
        } => eval_outer_product_values(args, env)
            .unwrap_or_else(|| vec![eval_expr_or_default::<T>(expr, env)]),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Symmetric,
            args,
            ..
        } => eval_symmetric_values(args, env)
            .unwrap_or_else(|| vec![eval_expr_or_default::<T>(expr, env)]),
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            eval_binary_array_values(op, lhs, rhs, env)
                .unwrap_or_else(|| vec![eval_expr_or_default::<T>(expr, env)])
        }
        rumoca_core::Expression::Unary { op, rhs, .. } => eval_unary_array_values(op, rhs, env),
        rumoca_core::Expression::BuiltinCall { function, args, .. } if args.len() == 1 => {
            let values = eval_array_like_values::<T>(&args[0], env);
            if values.len() > 1
                && let Some(mapped) = eval_unary_builtin_array_values(*function, values)
            {
                return mapped;
            }
            vec![eval_expr_or_default::<T>(expr, env)]
        }
        rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
            ..
        } => eval_function_call_array_values(name, args, env)
            .unwrap_or_else(|| vec![eval_expr_or_default::<T>(expr, env)]),
        rumoca_core::Expression::Array { .. }
        | rumoca_core::Expression::Tuple { .. }
        | rumoca_core::Expression::Range { .. }
        | rumoca_core::Expression::If { .. }
        | rumoca_core::Expression::ArrayComprehension { .. } => eval_array_values::<T>(expr, env),
        _ => vec![eval_expr_or_default::<T>(expr, env)],
    }
}

fn eval_unary_array_values<T: SimFloat>(
    op: &rumoca_core::OpUnary,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Vec<T> {
    let values = eval_array_like_values(rhs, env);
    values
        .into_iter()
        .map(|value| apply_unary_value(op, value))
        .collect()
}

fn try_eval_unary_array_values<T: SimFloat>(
    op: &rumoca_core::OpUnary,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    let values = try_eval_array_like_values(rhs, env)?;
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
) -> Option<Vec<T>> {
    match function {
        rumoca_core::BuiltinFunction::Zeros => {
            let len = constructor_extent(args, env)?;
            Some(vec![T::zero(); len])
        }
        rumoca_core::BuiltinFunction::Ones => {
            let len = constructor_extent(args, env)?;
            Some(vec![T::one(); len])
        }
        rumoca_core::BuiltinFunction::Fill => {
            let fill = eval_expr_or_default::<T>(args.first()?, env);
            let len = constructor_extent(&args[1..], env)?;
            Some(vec![fill; len])
        }
        rumoca_core::BuiltinFunction::Identity => {
            let n = constructor_dim(args.first()?, env)?;
            let mut values = vec![T::zero(); n.checked_mul(n)?];
            for idx in 0..n {
                values[idx * n + idx] = T::one();
            }
            Some(values)
        }
        rumoca_core::BuiltinFunction::Diagonal => {
            let values = eval_array_like_values(args.first()?, env);
            Some(flatten_matrix(&diagonal_matrix(values)))
        }
        _ => None,
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
) -> Option<usize> {
    args.iter().try_fold(1usize, |acc, arg| {
        constructor_dim(arg, env).and_then(|dim| acc.checked_mul(dim))
    })
}

fn constructor_dim<T: SimFloat>(arg: &rumoca_core::Expression, env: &VarEnv<T>) -> Option<usize> {
    let dim = eval_expr_or_default::<T>(arg, env).real().round();
    (dim.is_finite() && dim > 0.0).then_some(dim as usize)
}

fn eval_function_call_array_values<T: SimFloat>(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Option<Vec<T>> {
    let function = env.functions.get(name.as_str())?;
    let output = function.outputs.first()?;
    if function_param_has_array_shape(output) {
        return eval_user_function_array_output_pub(name.var_name(), args, env).ok();
    }
    let size = function_param_size(output);
    if size <= 1 {
        if let Some(values) = eval_vectorized_scalar_function_call(name, args, env) {
            return Some(values);
        }
        return Some(vec![eval_function_call(name.var_name(), args, false, env)]);
    }

    let mut values = Vec::with_capacity(size);
    for flat_index in 0..size {
        let suffix = function_output_scalar_suffix(output, flat_index)?;
        values.push(eval_selected_function_output_pub(
            name.var_name(),
            output.name.as_str(),
            suffix.as_str(),
            args,
            env,
        ));
    }
    Some(values)
}

fn try_eval_function_call_array_values<T: SimFloat>(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    let function = env
        .functions
        .get(name.as_str())
        .ok_or_else(|| EvalError::MissingFunction {
            name: name.to_string(),
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
    let size = function_param_size(output);
    if size <= 1 {
        if let Some(values) = eval_vectorized_scalar_function_call(name, args, env) {
            return Ok(values);
        }
        return Ok(vec![eval_expr(
            &rumoca_core::Expression::FunctionCall {
                name: name.clone(),
                args: args.to_vec(),
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
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
    env: &VarEnv<T>,
) -> Option<Vec<T>> {
    let arg_values = args
        .iter()
        .map(|arg| eval_array_like_values(arg, env))
        .collect::<Vec<_>>();
    let len = arg_values.iter().map(Vec::len).max().unwrap_or(0);
    if len <= 1 {
        return None;
    }
    if arg_values
        .iter()
        .any(|values| values.is_empty() || (values.len() != 1 && values.len() != len))
    {
        return None;
    }

    Some(
        (0..len)
            .map(|idx| {
                let scalar_args = arg_values
                    .iter()
                    .map(|values| scalar_literal_expr(vectorized_arg_value(values, idx)))
                    .collect::<Vec<_>>();
                eval_function_call(name.var_name(), &scalar_args, false, env)
            })
            .collect(),
    )
}

fn vectorized_arg_value<T: SimFloat>(values: &[T], idx: usize) -> T {
    values
        .get(if values.len() == 1 { 0 } else { idx })
        .copied()
        .unwrap_or_else(T::zero)
}

fn scalar_literal_expr<T: SimFloat>(value: T) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value.real()),
        span: rumoca_core::Span::DUMMY,
    }
}

fn function_param_size(param: &rumoca_core::FunctionParam) -> usize {
    if param.dims.is_empty() {
        return 1;
    }
    param
        .dims
        .iter()
        .try_fold(1usize, |acc, dim| {
            usize::try_from(*dim)
                .ok()
                .filter(|dim| *dim > 0)
                .and_then(|dim| acc.checked_mul(dim))
        })
        .unwrap_or(0)
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
        .map(|dim| usize::try_from(*dim).ok().filter(|dim| *dim > 0))
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

fn eval_cat_values<T: SimFloat>(args: &[rumoca_core::Expression], env: &VarEnv<T>) -> Vec<T> {
    // MLS §10.4.2.1: cat(k, A, B, ...) concatenates along dimension k.
    if args.len() <= 1 {
        return Vec::new();
    }
    let dim = eval_expr_or_default::<T>(&args[0], env).real().round();
    if !dim.is_finite() || dim < 1.0 {
        return Vec::new();
    }

    let operands = args
        .iter()
        .skip(1)
        .map(|arg| RuntimeArrayOperand {
            values: eval_array_like_values(arg, env),
            dims: infer_runtime_expr_dims(arg, env),
        })
        .collect::<Vec<_>>();
    match dim as usize {
        1 => operands
            .into_iter()
            .flat_map(|operand| operand.values)
            .collect(),
        2 => eval_cat_dim2_values(&operands),
        _ => Vec::new(),
    }
}

pub(super) fn infer_runtime_expr_dims<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Vec<usize> {
    let values = eval_array_like_values(expr, env);
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => env
            .dims
            .get(name.as_str())
            .map(|dims| infer_declared_or_value_dims(dims, values.len()))
            .unwrap_or_else(|| runtime_vector_dims(values.len())),
        rumoca_core::Expression::Array {
            elements,
            is_matrix: true,
            ..
        } => runtime_matrix_literal_dims(elements, env),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Cat,
            args,
            ..
        } => infer_runtime_cat_dims(args, env).unwrap_or_else(|| runtime_vector_dims(values.len())),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Der,
            args,
            ..
        } => match args.first() {
            Some(arg) => infer_runtime_expr_dims(arg, env),
            None => Vec::new(),
        },
        rumoca_core::Expression::FunctionCall {
            name,
            is_constructor: false,
            ..
        } => env
            .functions
            .get(name.as_str())
            .and_then(|function| function.outputs.first())
            .map(|output| {
                output
                    .dims
                    .iter()
                    .filter_map(|dim| usize::try_from(*dim).ok().filter(|dim| *dim > 0))
                    .collect::<Vec<_>>()
            })
            .filter(|dims| !dims.is_empty())
            .unwrap_or_else(|| runtime_vector_dims(values.len())),
        rumoca_core::Expression::Array { .. }
        | rumoca_core::Expression::Tuple { .. }
        | rumoca_core::Expression::Range { .. }
        | rumoca_core::Expression::ArrayComprehension { .. } => runtime_vector_dims(values.len()),
        _ => runtime_vector_dims(values.len()),
    }
}

fn runtime_vector_dims(len: usize) -> Vec<usize> {
    (len > 1).then_some(len).into_iter().collect()
}

fn infer_declared_or_value_dims(dims: &[i64], value_count: usize) -> Vec<usize> {
    let declared = dims
        .iter()
        .map(|dim| usize::try_from(*dim).ok().filter(|dim| *dim > 0))
        .collect::<Option<Vec<_>>>();
    if let Some(declared) = declared
        && !declared.is_empty()
    {
        return declared;
    }
    infer_dims_from_values(dims, value_count)
}

fn runtime_matrix_literal_dims<T: SimFloat>(
    elements: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Vec<usize> {
    if elements.is_empty() {
        return Vec::new();
    }
    let rows = elements.len();
    let cols = elements
        .iter()
        .map(|element| eval_array_like_values::<T>(element, env).len())
        .max()
        .unwrap_or(0);
    if cols == 0 {
        Vec::new()
    } else {
        vec![rows, cols]
    }
}

fn infer_runtime_cat_dims<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Option<Vec<usize>> {
    if args.len() <= 1 {
        return Some(Vec::new());
    }
    let dim = eval_expr_or_default::<T>(&args[0], env).real().round() as usize;
    let mut operands = args
        .iter()
        .skip(1)
        .map(|arg| infer_runtime_expr_dims(arg, env));
    let first = operands.next()?;
    match dim {
        1 if first.is_empty() => Some(runtime_vector_dims(args.len() - 1)),
        1 => {
            let mut total = first[0];
            let tail = &first[1..];
            for dims in operands {
                if dims.len() != first.len() || &dims[1..] != tail {
                    return None;
                }
                total += dims[0];
            }
            let mut dims = vec![total];
            dims.extend_from_slice(tail);
            Some(dims)
        }
        2 => {
            let [rows, cols] = first.as_slice() else {
                return None;
            };
            let rows = *rows;
            let mut total_cols = *cols;
            for dims in operands {
                let [operand_rows, operand_cols] = dims.as_slice() else {
                    return None;
                };
                if *operand_rows != rows {
                    return None;
                }
                total_cols += *operand_cols;
            }
            Some(vec![rows, total_cols])
        }
        _ => None,
    }
}

fn eval_cat_dim2_values<T: SimFloat>(operands: &[RuntimeArrayOperand<T>]) -> Vec<T> {
    let Some(first) = operands.first() else {
        return Vec::new();
    };
    let [rows, _] = first.dims.as_slice() else {
        return Vec::new();
    };
    let mut out = Vec::new();
    for row in 0..*rows {
        for operand in operands {
            let [_, cols] = operand.dims.as_slice() else {
                return Vec::new();
            };
            let start = row * cols;
            let end = start + cols;
            if end > operand.values.len() {
                return Vec::new();
            }
            out.extend_from_slice(&operand.values[start..end]);
        }
    }
    out
}

pub(super) fn eval_linspace_values<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Vec<T> {
    if args.len() != 3 {
        return Vec::new();
    }
    let start = eval_expr_or_default::<T>(&args[0], env).real();
    let end = eval_expr_or_default::<T>(&args[1], env).real();
    let n = eval_expr_or_default::<T>(&args[2], env).real().round() as i64;
    if n < 2 {
        return Vec::new();
    }
    let n_usize = n as usize;
    let step = (end - start) / ((n_usize - 1) as f64);
    let mut out: Vec<T> = (0..n_usize)
        .map(|i| T::from_f64(start + step * i as f64))
        .collect();
    if let Some(last) = out.last_mut() {
        *last = T::from_f64(end);
    }
    out
}

pub(super) fn eval_array_like_f64_values<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Vec<f64> {
    eval_array_like_values(expr, env)
        .into_iter()
        .map(|v| v.real())
        .collect()
}

pub(super) fn eval_cat_f64_values<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Vec<f64> {
    eval_cat_values(args, env)
        .into_iter()
        .map(|v| v.real())
        .collect()
}

pub(super) fn eval_columns_arg<T: SimFloat>(
    expr: Option<&rumoca_core::Expression>,
    env: &VarEnv<T>,
) -> Vec<usize> {
    let Some(expr) = expr else { return Vec::new() };
    eval_array_like_f64_values(expr, env)
        .into_iter()
        .map(|v| v.round() as i64)
        .filter(|v| *v > 0)
        .map(|v| v as usize)
        .collect()
}
