use super::*;

pub(super) fn flat_index_for_subscripts(dims: &[usize], indices: &[usize]) -> Option<usize> {
    if dims.len() != indices.len() {
        return None;
    }
    let mut flat_index = 0usize;
    let mut stride = 1usize;
    for (dim, index) in dims.iter().zip(indices.iter()) {
        if *dim == 0 || *index == 0 || *index > *dim {
            return None;
        }
        flat_index = flat_index.checked_add((index - 1).checked_mul(stride)?)?;
        stride = stride.checked_mul(*dim)?;
    }
    Some(flat_index)
}

impl ArrayOperand {
    pub(super) fn with_shape_span(
        values: Vec<Reg>,
        dims: Vec<usize>,
        span: rumoca_core::Span,
    ) -> Result<Self, LowerError> {
        let dims = normalize_operand_dims(values.len(), dims, span)?;
        Ok(Self {
            values,
            dims,
            shape_span: span,
        })
    }

    pub(super) fn scalar_with_span(value: Reg, span: rumoca_core::Span) -> Self {
        Self {
            values: vec![value],
            dims: Vec::new(),
            shape_span: span,
        }
    }

    pub(super) fn is_scalar(&self) -> bool {
        self.dims.is_empty() && self.values.len() == 1
    }
}

pub(super) fn cat_array_operands(
    dim: usize,
    operands: &[ArrayOperand],
    span: rumoca_core::Span,
) -> Result<ArrayOperand, LowerError> {
    let dims = cat_array_dims(dim, operands, span)?;
    if operands.is_empty() {
        return ArrayOperand::with_shape_span(Vec::new(), dims, span);
    }
    let values = match dim {
        1 => operands
            .iter()
            .flat_map(|operand| operand.values.iter().copied())
            .collect(),
        2 => cat_matrix_columns(operands, span)?,
        _ => {
            return Err(unsupported_at(
                format!("cat dimension {dim} is unsupported for compiled array lowering"),
                span,
            ));
        }
    };
    ArrayOperand::with_shape_span(values, dims, span)
}

pub(super) fn cat_array_dims(
    dim: usize,
    operands: &[ArrayOperand],
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let Some(first) = operands.first() else {
        return Ok(Vec::new());
    };
    match dim {
        1 => cat_dim1_dims(operands, &first.dims, span),
        2 => cat_dim2_dims(operands, &first.dims, span),
        _ => Err(unsupported_at(
            format!("cat dimension {dim} is not supported yet"),
            span,
        )),
    }
}

pub(super) fn cat_dim1_dims(
    operands: &[ArrayOperand],
    first_dims: &[usize],
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    if first_dims.is_empty() {
        return Ok(vector_dims(operands.len()));
    }
    let tail = &first_dims[1..];
    let mut total = 0usize;
    for operand in operands {
        if operand.dims.len() != first_dims.len() || &operand.dims[1..] != tail {
            return Err(unsupported_at(
                format!(
                    "cat(1, ...) requires matching trailing dimensions, got {} and {}",
                    format_usize_dims(first_dims),
                    format_usize_dims(&operand.dims)
                ),
                operand_shape_span(operand, span),
            ));
        }
        total = checked_array_dim_sum_at(
            total,
            operand.dims[0],
            "cat(1, ...) dimension",
            operand_shape_span(operand, span),
        )?;
    }
    let mut dims = vec![total];
    dims.extend_from_slice(tail);
    Ok(dims)
}

pub(super) fn cat_dim2_dims(
    operands: &[ArrayOperand],
    first_dims: &[usize],
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let [rows, cols] = first_dims else {
        return Err(unsupported_at(
            "cat(2, ...) currently requires matrix operands",
            span,
        ));
    };
    let mut total_cols = *cols;
    for operand in operands.iter().skip(1) {
        let [operand_rows, operand_cols] = operand.dims.as_slice() else {
            return Err(unsupported_at(
                "cat(2, ...) currently requires matrix operands",
                operand_shape_span(operand, span),
            ));
        };
        if operand_rows != rows {
            return Err(unsupported_at(
                format!("cat(2, ...) requires matching row counts, got {rows} and {operand_rows}"),
                operand_shape_span(operand, span),
            ));
        }
        total_cols = checked_array_dim_sum_at(
            total_cols,
            *operand_cols,
            "cat(2, ...) column count",
            operand_shape_span(operand, span),
        )?;
    }
    Ok(vec![*rows, total_cols])
}

pub(super) fn cat_matrix_columns(
    operands: &[ArrayOperand],
    span: rumoca_core::Span,
) -> Result<Vec<Reg>, LowerError> {
    let Some(first) = operands.first() else {
        return Ok(Vec::new());
    };
    let [rows, _] = first.dims.as_slice() else {
        return Err(unsupported_at(
            "cat(2, ...) currently requires matrix operands",
            operand_shape_span(first, span),
        ));
    };

    let mut values = Vec::new();
    for row in 0..*rows {
        for operand in operands {
            let [_, cols] = operand.dims.as_slice() else {
                return Err(unsupported_at(
                    "cat(2, ...) currently requires matrix operands",
                    operand_shape_span(operand, span),
                ));
            };
            let start = checked_matrix_row_start_at(
                row,
                *cols,
                "cat(2, ...) row offset",
                operand_shape_span(operand, span),
            )?;
            let end = start.checked_add(*cols).ok_or_else(|| {
                LowerError::contract_violation(
                    "cat(2, ...) row slice end overflows host index range",
                    operand_shape_span(operand, span),
                )
            })?;
            values.extend_from_slice(&operand.values[start..end]);
        }
    }
    Ok(values)
}

fn operand_shape_span(operand: &ArrayOperand, fallback: rumoca_core::Span) -> rumoca_core::Span {
    if operand.shape_span.is_dummy() {
        fallback
    } else {
        operand.shape_span
    }
}

pub(super) fn normalize_operand_dims(
    value_count: usize,
    dims: Vec<usize>,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    if !dims.is_empty() && checked_shape_size(&dims, "array operand shape", span)? == value_count {
        return Ok(dims);
    }
    if value_count <= 1 {
        return Ok(Vec::new());
    }
    if let [outer] = dims.as_slice()
        && *outer > 1
        && value_count.is_multiple_of(*outer)
        && value_count / *outer > 1
    {
        // MLS §10.4.1 prefixes the element dimensions for array constructors.
        // If only the outer dimension survived shape inference, recover the
        // trailing dimension from the lowered scalar count instead of erasing
        // the matrix shape into a flat vector.
        return Ok(vec![*outer, value_count / *outer]);
    }
    Ok(vector_dims(value_count))
}

pub(super) fn one_based_index_tuples(
    dims: &[usize],
    span: rumoca_core::Span,
) -> Result<Vec<Vec<usize>>, LowerError> {
    let count = checked_shape_size(dims, "array-like dynamic index tuple count", span)?;
    let mut tuples =
        fallible_vec_with_capacity(count, "array-like dynamic index tuple count", span)?;
    collect_one_based_index_tuples(dims, 0, &mut Vec::new(), &mut tuples);
    Ok(tuples)
}

fn collect_one_based_index_tuples(
    dims: &[usize],
    depth: usize,
    current: &mut Vec<usize>,
    tuples: &mut Vec<Vec<usize>>,
) {
    if depth >= dims.len() {
        tuples.push(current.clone());
        return;
    }
    for index in 1..=dims[depth] {
        current.push(index);
        collect_one_based_index_tuples(dims, depth + 1, current, tuples);
        current.pop();
    }
}

pub(in crate::lower) fn index_choice_tuples(
    choices: &[Vec<usize>],
    span: rumoca_core::Span,
) -> Result<Vec<Vec<usize>>, LowerError> {
    let count = checked_choice_tuple_count(choices, "array-like dynamic slice tuple count", span)?;
    let mut tuples =
        fallible_vec_with_capacity(count, "array-like dynamic slice tuple count", span)?;
    collect_index_choice_tuples(choices, 0, &mut Vec::new(), &mut tuples);
    Ok(tuples)
}

fn collect_index_choice_tuples(
    choices: &[Vec<usize>],
    depth: usize,
    current: &mut Vec<usize>,
    tuples: &mut Vec<Vec<usize>>,
) {
    if depth >= choices.len() {
        tuples.push(current.clone());
        return;
    }
    for index in &choices[depth] {
        current.push(*index);
        collect_index_choice_tuples(choices, depth + 1, current, tuples);
        current.pop();
    }
}

pub(super) fn inferred_subscripted_dims(
    base_dims: &[usize],
    subscripts: &[rumoca_core::Subscript],
    context_span: Option<rumoca_core::Span>,
    builder: &LowerBuilder<'_>,
    scope: &Scope,
) -> Result<Vec<usize>, LowerError> {
    let span = expr_span_from_subscripts(subscripts).or(context_span);
    let mut dims = match span {
        Some(span) => array_vec_with_capacity(
            base_dims.len(),
            "inferred subscripted dimension count",
            span,
        )?,
        None => {
            let mut dims = Vec::new();
            dims.try_reserve_exact(base_dims.len()).map_err(|_| {
                LowerError::UnspannedContractViolation {
                    reason:
                        "inferred subscripted dimension count capacity exceeds host memory limits"
                            .to_string(),
                }
            })?;
            dims
        }
    };
    for (dim_index, subscript) in subscripts.iter().enumerate() {
        let dim = base_dims[dim_index];
        match subscript {
            rumoca_core::Subscript::Colon { .. } => {
                dims.push(dim);
            }
            rumoca_core::Subscript::Expr { expr, span }
                if matches!(expr.as_ref(), rumoca_core::Expression::Range { .. }) =>
            {
                let len = builder.slice_expr_indices(expr, dim, scope, *span)?.len();
                dims.push(len);
            }
            _ => {}
        }
    }
    for &dim in &base_dims[subscripts.len()..] {
        dims.push(dim);
    }
    Ok(dims)
}

pub(super) fn subscript_preserves_array_rank(subscript: &rumoca_core::Subscript) -> bool {
    match subscript {
        rumoca_core::Subscript::Colon { .. } => true,
        rumoca_core::Subscript::Expr { expr, .. } => {
            matches!(expr.as_ref(), rumoca_core::Expression::Range { .. })
        }
        _ => false,
    }
}

pub(super) fn infer_array_literal_dims(
    elements: &[rumoca_core::Expression],
    is_matrix: bool,
) -> Vec<usize> {
    infer_array_literal_dims_with(elements, is_matrix, infer_literal_expr_dims)
}

pub(super) fn infer_array_literal_dims_with(
    elements: &[rumoca_core::Expression],
    is_matrix: bool,
    mut infer_child_dims: impl FnMut(&rumoca_core::Expression) -> Vec<usize>,
) -> Vec<usize> {
    let child_dims = elements
        .iter()
        .map(&mut infer_child_dims)
        .collect::<Vec<_>>();
    infer_array_literal_dims_from_children(elements, is_matrix, &child_dims)
}

pub(super) fn infer_array_literal_dims_with_result<E>(
    elements: &[rumoca_core::Expression],
    is_matrix: bool,
    mut infer_child_dims: impl FnMut(&rumoca_core::Expression) -> Result<Vec<usize>, E>,
) -> Result<Vec<usize>, E> {
    let child_dims = elements
        .iter()
        .map(&mut infer_child_dims)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(infer_array_literal_dims_from_children(
        elements,
        is_matrix,
        &child_dims,
    ))
}

fn infer_array_literal_dims_from_children(
    elements: &[rumoca_core::Expression],
    is_matrix: bool,
    child_dims: &[Vec<usize>],
) -> Vec<usize> {
    if let [dims] = child_dims
        && is_matrix
        && dims.len() > 1
    {
        return dims.clone();
    }
    if !is_matrix {
        return infer_array_sequence_dims_from_children(elements.len(), child_dims);
    }
    if elements.iter().all(|element| {
        matches!(
            element,
            rumoca_core::Expression::Array { .. } | rumoca_core::Expression::Tuple { .. }
        )
    }) {
        return infer_matrix_row_literal_dims(elements.len(), child_dims);
    }
    if let Some(dims) = infer_matrix_column_concat_dims(elements.len(), child_dims) {
        return dims;
    }
    if child_dims.iter().all(Vec::is_empty) {
        return vec![1, elements.len()];
    }
    Vec::new()
}

fn infer_matrix_row_literal_dims(row_count: usize, child_dims: &[Vec<usize>]) -> Vec<usize> {
    let Some(cols) = child_dims
        .first()
        .and_then(|dims| matrix_row_literal_column_count(dims))
    else {
        return vec![row_count, 1];
    };
    if child_dims
        .iter()
        .all(|dims| matrix_row_literal_column_count(dims) == Some(cols))
    {
        vec![row_count, cols]
    } else {
        Vec::new()
    }
}

fn matrix_row_literal_column_count(dims: &[usize]) -> Option<usize> {
    match dims {
        [cols] => Some(*cols),
        [1, cols] => Some(*cols),
        _ => None,
    }
}

fn infer_matrix_column_concat_dims(
    operand_count: usize,
    child_dims: &[Vec<usize>],
) -> Option<Vec<usize>> {
    let first = child_dims.first()?;
    match first.as_slice() {
        [rows] if *rows > 0
            && child_dims
                .iter()
                .all(|dims| matches!(dims.as_slice(), [candidate] if *candidate == *rows)) =>
        {
            Some(vec![*rows, operand_count])
        }
        [rows, _] if *rows > 0
            && child_dims
                .iter()
                .all(|dims| matches!(dims.as_slice(), [candidate_rows, _] if *candidate_rows == *rows)) =>
        {
            Some(vec![
                *rows,
                child_dims
                    .iter()
                    .filter_map(|dims| dims.get(1).copied())
                    .sum(),
            ])
        }
        _ => None,
    }
}

pub(super) fn single_high_rank_matrix_concat_element(
    elements: &[rumoca_core::Expression],
    is_matrix: bool,
) -> Option<&rumoca_core::Expression> {
    let [element] = elements else {
        return None;
    };
    // MLS §10.4.2.1: matrix-constructor syntax is array concatenation, not an
    // extra semantic dimension. A single high-rank argument such as the MSL
    // Digital truth tables `[{{...}}]` indexes as the enclosed array itself.
    if is_matrix && infer_literal_expr_dims(element).len() > 1 {
        Some(element)
    } else {
        None
    }
}

pub(super) fn infer_literal_expr_dims(expr: &rumoca_core::Expression) -> Vec<usize> {
    match expr {
        rumoca_core::Expression::Array {
            elements,
            is_matrix,
            ..
        } => infer_array_literal_dims(elements, *is_matrix),
        rumoca_core::Expression::Tuple { elements, .. } => infer_array_sequence_dims(elements),
        _ => Vec::new(),
    }
}

pub(super) fn infer_array_sequence_dims(elements: &[rumoca_core::Expression]) -> Vec<usize> {
    let child_dims = elements
        .iter()
        .map(infer_literal_expr_dims)
        .collect::<Vec<_>>();
    infer_array_sequence_dims_from_children(elements.len(), &child_dims)
}

fn infer_array_sequence_dims_from_children(
    element_count: usize,
    child_dims: &[Vec<usize>],
) -> Vec<usize> {
    let Some(first_dims) = child_dims.first() else {
        return Vec::new();
    };
    if first_dims.is_empty() || child_dims.iter().skip(1).any(|dims| dims != first_dims) {
        return vec![element_count];
    }
    let mut dims = vec![element_count];
    dims.extend(first_dims);
    dims
}

pub(super) fn unary_array_builtin_op(function: &rumoca_core::BuiltinFunction) -> Option<UnaryOp> {
    match function {
        rumoca_core::BuiltinFunction::Abs => Some(UnaryOp::Abs),
        rumoca_core::BuiltinFunction::Sign => Some(UnaryOp::Sign),
        rumoca_core::BuiltinFunction::Sqrt => Some(UnaryOp::Sqrt),
        rumoca_core::BuiltinFunction::Floor | rumoca_core::BuiltinFunction::Integer => {
            Some(UnaryOp::Floor)
        }
        rumoca_core::BuiltinFunction::Ceil => Some(UnaryOp::Ceil),
        rumoca_core::BuiltinFunction::Sin => Some(UnaryOp::Sin),
        rumoca_core::BuiltinFunction::Cos => Some(UnaryOp::Cos),
        rumoca_core::BuiltinFunction::Tan => Some(UnaryOp::Tan),
        rumoca_core::BuiltinFunction::Asin => Some(UnaryOp::Asin),
        rumoca_core::BuiltinFunction::Acos => Some(UnaryOp::Acos),
        rumoca_core::BuiltinFunction::Atan => Some(UnaryOp::Atan),
        rumoca_core::BuiltinFunction::Sinh => Some(UnaryOp::Sinh),
        rumoca_core::BuiltinFunction::Cosh => Some(UnaryOp::Cosh),
        rumoca_core::BuiltinFunction::Tanh => Some(UnaryOp::Tanh),
        rumoca_core::BuiltinFunction::Exp => Some(UnaryOp::Exp),
        rumoca_core::BuiltinFunction::Log => Some(UnaryOp::Log),
        rumoca_core::BuiltinFunction::Log10 => Some(UnaryOp::Log10),
        _ => None,
    }
}

pub(super) fn vector_dims(len: usize) -> Vec<usize> {
    if len <= 1 { Vec::new() } else { vec![len] }
}

pub(super) fn checked_shape_size(
    dims: &[usize],
    context: &str,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    if dims.is_empty() {
        return Ok(1);
    }
    dims.iter().copied().try_fold(1usize, |total, dim| {
        checked_dim_product(total, dim, context, span)
    })
}

pub(super) fn checked_shape_size_or_scalar(
    dims: &[usize],
    context: &str,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    Ok(checked_shape_size(dims, context, span)?.max(1))
}

pub(super) fn copy_shape_dims(
    dims: &[usize],
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let mut copied = crate::lower_vec_with_capacity(dims.len(), context, span)?;
    copied.extend(dims.iter().copied());
    Ok(copied)
}

pub(super) fn broadcast_shape(
    lhs: &[usize],
    rhs: &[usize],
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    if lhs.is_empty() {
        return copy_shape_dims(rhs, "array broadcast rhs shape rank", span);
    }
    if rhs.is_empty() || lhs == rhs {
        return copy_shape_dims(lhs, "array broadcast lhs shape rank", span);
    }
    if checked_shape_size(lhs, "array broadcast lhs shape", span)?
        == checked_shape_size(rhs, "array broadcast rhs shape", span)?
    {
        // Record-valued function outputs can arrive at Solve IR with their
        // scalar width preserved but rank metadata flattened. At this boundary
        // elementwise rows are already scalarized, so equal scalar counts can
        // still be paired deterministically.
        return copy_shape_dims(lhs, "array broadcast equal-size lhs shape rank", span);
    }
    Err(unsupported_at(
        // MLS §10 and §10.6.2: array binary operations require matching sizes
        // unless one operand is scalar.
        format!(
            "array operands have incompatible shapes {} and {}",
            format_usize_dims(lhs),
            format_usize_dims(rhs)
        ),
        span,
    ))
}

pub(super) fn multiplication_dims(
    lhs: &[usize],
    rhs: &[usize],
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    match (lhs, rhs) {
        ([], []) => Ok(Vec::new()),
        ([], dims) | (dims, []) => {
            copy_shape_dims(dims, "array multiplication scalar shape rank", span)
        }
        ([lhs_len], [rhs_len]) if lhs_len == rhs_len => Ok(Vec::new()),
        ([rows, inner], [rhs_len]) if inner == rhs_len => {
            copy_shape_dims(&[*rows], "matrix-vector multiplication output rank", span)
        }
        ([lhs_len], [inner, cols]) if lhs_len == inner => {
            copy_shape_dims(&[*cols], "vector-matrix multiplication output rank", span)
        }
        ([rows, inner], [rhs_inner, cols]) if inner == rhs_inner => {
            copy_shape_dims(&[*rows, *cols], "matrix multiplication output rank", span)
        }
        _ => Err(unsupported_at(
            // MLS §10.6.5 defines ordinary multiplication for scalar scaling,
            // vector scalar products, matrix-vector, vector-matrix, and
            // matrix-matrix products.
            format!(
                "array multiplication shapes {} and {} are incompatible",
                format_usize_dims(lhs),
                format_usize_dims(rhs)
            ),
            span,
        )),
    }
}

pub(super) fn broadcast_pairs(
    lhs: &ArrayOperand,
    rhs: &ArrayOperand,
) -> Result<Vec<(Reg, Reg)>, LowerError> {
    if let Some(pairs) = trailing_vector_broadcast_pairs(lhs, rhs)? {
        return Ok(pairs);
    }
    let dims = broadcast_dims(lhs, rhs)?;
    let span = operand_pair_shape_span(lhs, rhs)?;
    let count = checked_shape_size_or_scalar(&dims, "array broadcast value count", span)?;
    let mut pairs = array_vec_with_capacity(count, "array broadcast pair count", span)?;
    for idx in 0..count {
        let lhs = if lhs.is_scalar() {
            lhs.values[0]
        } else {
            lhs.values[idx]
        };
        let rhs = if rhs.is_scalar() {
            rhs.values[0]
        } else {
            rhs.values[idx]
        };
        pairs.push((lhs, rhs));
    }
    Ok(pairs)
}

fn trailing_vector_broadcast_pairs(
    lhs: &ArrayOperand,
    rhs: &ArrayOperand,
) -> Result<Option<Vec<(Reg, Reg)>>, LowerError> {
    match (lhs.dims.as_slice(), rhs.dims.as_slice()) {
        ([_, cols], [rhs_cols]) if cols == rhs_cols => {
            let span = operand_pair_shape_span(lhs, rhs)?;
            let mut pairs = array_vec_with_capacity(
                lhs.values.len(),
                "trailing vector broadcast pair count",
                span,
            )?;
            for (idx, lhs) in lhs.values.iter().copied().enumerate() {
                pairs.push((lhs, rhs.values[idx % cols]));
            }
            Ok(Some(pairs))
        }
        ([lhs_cols], [_, cols]) if lhs_cols == cols => {
            let span = operand_pair_shape_span(lhs, rhs)?;
            let mut pairs = array_vec_with_capacity(
                rhs.values.len(),
                "trailing vector broadcast pair count",
                span,
            )?;
            for (idx, rhs) in rhs.values.iter().copied().enumerate() {
                pairs.push((lhs.values[idx % cols], rhs));
            }
            Ok(Some(pairs))
        }
        _ => Ok(None),
    }
}

pub(super) fn broadcast_dims(
    lhs: &ArrayOperand,
    rhs: &ArrayOperand,
) -> Result<Vec<usize>, LowerError> {
    let span = operand_pair_shape_span(lhs, rhs)?;
    broadcast_shape(&lhs.dims, &rhs.dims, span)
}

fn operand_pair_shape_span(
    lhs: &ArrayOperand,
    rhs: &ArrayOperand,
) -> Result<rumoca_core::Span, LowerError> {
    if !lhs.shape_span.is_dummy() {
        return Ok(lhs.shape_span);
    }
    if !rhs.shape_span.is_dummy() {
        return Ok(rhs.shape_span);
    }
    Err(LowerError::UnspannedContractViolation {
        reason: "array operand pair requires shape span metadata".to_string(),
    })
}

pub(super) fn lower_static_range_values(
    start: &rumoca_core::Expression,
    step: Option<&rumoca_core::Expression>,
    end: &rumoca_core::Expression,
    range_span: rumoca_core::Span,
) -> Result<Option<Vec<f64>>, LowerError> {
    let range_span = static_range_diagnostic_span(start, step, end, range_span)?;
    let Some(start_v) = lower_static_index_numeric(start)? else {
        return Ok(None);
    };
    let Some(end_v) = lower_static_index_numeric(end)? else {
        return Ok(None);
    };
    let step_v = if let Some(step_expr) = step {
        let Some(value) = lower_static_index_numeric(step_expr)? else {
            return Ok(None);
        };
        value
    } else if end_v >= start_v {
        1.0
    } else {
        -1.0
    };

    if !start_v.is_finite()
        || !end_v.is_finite()
        || !step_v.is_finite()
        || step_v.abs() <= f64::EPSILON
    {
        return Err(unsupported_at(
            "invalid static range expression in compiled lowering",
            step.and_then(rumoca_core::Expression::span)
                .unwrap_or(range_span),
        ));
    }

    let tol = step_v.abs() * 1e-9 + 1e-12;
    let mut values = Vec::new();
    let mut value = start_v;
    for _ in 0..MAX_STATIC_RANGE_VALUES {
        let past_end =
            (step_v > 0.0 && value > end_v + tol) || (step_v < 0.0 && value < end_v - tol);
        if past_end {
            break;
        }
        values.push(value);
        let next = value + step_v;
        if !next.is_finite() {
            return Err(unsupported_at(
                "static range overflows finite Real values",
                range_span,
            ));
        }
        value = next;
    }
    let past_end = (step_v > 0.0 && value > end_v + tol) || (step_v < 0.0 && value < end_v - tol);
    if !past_end {
        return Err(unsupported_at(
            format!("static range exceeds maximum lowered range length {MAX_STATIC_RANGE_VALUES}"),
            range_span,
        ));
    }
    Ok(Some(values))
}

fn static_range_diagnostic_span(
    start: &rumoca_core::Expression,
    step: Option<&rumoca_core::Expression>,
    end: &rumoca_core::Expression,
    range_span: rumoca_core::Span,
) -> Result<rumoca_core::Span, LowerError> {
    if !range_span.is_dummy() {
        return Ok(range_span);
    }
    start
        .span()
        .or_else(|| step.and_then(rumoca_core::Expression::span))
        .or_else(|| end.span())
        .ok_or_else(|| LowerError::UnspannedContractViolation {
            reason: "static range lowering requires source span metadata".to_string(),
        })
}

pub(super) fn scoped_indexed_binding_values(
    scope: &Scope,
    path: &ComponentReferenceKey,
    span: rumoca_core::Span,
) -> Result<Option<Vec<Reg>>, LowerError> {
    scope.indexed_values(path, span)
}

pub(super) fn function_output_values(
    output: &rumoca_core::FunctionParam,
    scope: &Scope,
) -> Result<Vec<Reg>, LowerError> {
    let width = dims_scalar_count(
        &output.dims,
        format!("function output `{}`", output.name),
        output.span,
    )?;
    let output_path = generated_scope_key(&output.name);
    if width <= 1 {
        return scope
            .get(&output_path)
            .copied()
            .map(|reg| vec![reg])
            .ok_or_else(|| LowerError::InvalidFunction {
                name: output.name.clone(),
                reason: "function output was not assigned".to_string(),
            });
    }
    if let Some(values) = scoped_indexed_binding_values(scope, &output_path, output.span)?
        && values.len() == width
    {
        return Ok(values);
    }

    let mut values =
        crate::lower_vec_with_capacity(width, "function output value count", output.span)?;
    for flat_index in 0..width {
        let key = dae::scalar_name_text_for_flat_index(&output.name, &output.dims, flat_index);
        let key_path = generated_scope_key(&key);
        let Some(reg) = scope.get(&key_path).copied() else {
            return Err(LowerError::InvalidFunction {
                name: output.name.clone(),
                reason: format!("function output component `{key}` was not assigned"),
            });
        };
        values.push(reg);
    }
    Ok(values)
}

pub(super) fn record_output_component_values(
    output_name: &str,
    scope: &Scope,
    span: rumoca_core::Span,
) -> Result<Option<Vec<Reg>>, LowerError> {
    let output_prefix = format!("{output_name}.");
    let scope_entries = scope.iter_checked("record output scope source count", span)?;
    let mut entries = crate::lower_vec_with_capacity(
        scope_entries.len(),
        "record output component entry count",
        span,
    )?;
    for (key, reg) in scope_entries {
        if generated_scope_key_name(&key).is_some_and(|name| name.starts_with(&output_prefix)) {
            entries.push((key, reg));
        }
    }
    if entries.is_empty() {
        return Ok(None);
    }

    let mut keys =
        crate::lower_vec_with_capacity(entries.len(), "record output component key count", span)?;
    for (key, _) in &entries {
        let Some(name) = generated_scope_key_name(key) else {
            continue;
        };
        keys.push(name.to_string());
    }
    let mut values =
        crate::lower_vec_with_capacity(entries.len(), "record output component value count", span)?;
    for (key, reg) in entries {
        let Some(key) = generated_scope_key_name(&key) else {
            continue;
        };
        let indexed_prefix = format!("{key}[");
        let component_prefix = format!("{key}.");
        if keys.iter().any(|candidate| {
            candidate.len() > key.len()
                && (candidate.starts_with(&indexed_prefix)
                    || candidate.starts_with(&component_prefix))
        }) {
            continue;
        }
        values.push(reg);
    }
    Ok((!values.is_empty()).then_some(values))
}

pub(super) fn collect_slice_binding_keys(
    base_name: &str,
    selections: &[Vec<usize>],
    depth: usize,
    current: &mut Vec<usize>,
    keys: &mut Vec<String>,
) {
    if depth >= selections.len() {
        keys.push(dae::format_subscript_key(base_name, current));
        return;
    }

    for &index in &selections[depth] {
        current.push(index);
        collect_slice_binding_keys(base_name, selections, depth + 1, current, keys);
        current.pop();
    }
}

pub(super) fn validate_slice_indices(
    indices: &[usize],
    dim: usize,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    if indices.iter().all(|index| *index > 0 && *index <= dim) {
        return Ok(());
    }
    Err(unsupported_at(
        "array slice index is outside dimension bounds",
        span,
    ))
}

fn checked_array_dim_sum_at(
    lhs: usize,
    rhs: usize,
    context: &str,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    lhs.checked_add(rhs).ok_or_else(|| {
        LowerError::contract_violation(format!("{context} overflows host index range"), span)
    })
}

pub(super) fn checked_dim_product(
    lhs: usize,
    rhs: usize,
    context: &str,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    lhs.checked_mul(rhs).ok_or_else(|| {
        LowerError::contract_violation(format!("{context} overflows host index range"), span)
    })
}

fn checked_matrix_row_start_at(
    row: usize,
    cols: usize,
    context: &str,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    checked_dim_product(row, cols, context, span)
}

pub(super) fn fallible_vec_with_capacity<T>(
    capacity: usize,
    context: &str,
    span: rumoca_core::Span,
) -> Result<Vec<T>, LowerError> {
    let mut values = Vec::new();
    values.try_reserve_exact(capacity).map_err(|_| {
        LowerError::contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })?;
    Ok(values)
}

fn checked_choice_tuple_count(
    choices: &[Vec<usize>],
    context: &str,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    choices.iter().try_fold(1usize, |total, choice| {
        checked_dim_product(total, choice.len(), context, span)
    })
}

pub(super) fn is_scalar_selector_subscript(subscript: &rumoca_core::Subscript) -> bool {
    match subscript {
        rumoca_core::Subscript::Index { value, .. } => *value > 0,
        rumoca_core::Subscript::Expr { expr, .. } => {
            !matches!(expr.as_ref(), rumoca_core::Expression::Range { .. })
        }
        rumoca_core::Subscript::Colon { .. } => false,
    }
}

pub(super) fn is_array_like_sample_value_form(args: &[rumoca_core::Expression]) -> bool {
    matches!(args, [_] | [_, _])
}

pub(super) fn is_synchronous_array_like_intrinsic(name: &str) -> bool {
    matches!(
        intrinsic_short_name(name),
        "previous"
            | "hold"
            | "noClock"
            | "subSample"
            | "superSample"
            | "shiftSample"
            | "backSample"
    )
}

/// Detect sparsity of a `rows × cols` matrix whose elements are held in `values`
/// (register indices, row-major) produced by `ops`.
///
/// Returns `Diagonal` if the matrix is square and every off-diagonal element is
/// provably zero (a `Const { value: 0.0 }` or a `Move` of such a constant).
/// Otherwise returns `Dense`.
pub(super) fn detect_matrix_sparsity(
    ops: &[LinearOp],
    values: &[Reg],
    rows: usize,
    cols: usize,
) -> rumoca_ir_solve::SparsityPattern {
    let Some(extent) = rows.checked_mul(cols) else {
        return rumoca_ir_solve::SparsityPattern::Dense;
    };
    if rows != cols || values.len() != extent {
        return rumoca_ir_solve::SparsityPattern::Dense;
    }
    // Build reg → constant-value map, propagating through Move chains.
    let mut const_val: std::collections::HashMap<Reg, f64> =
        std::collections::HashMap::with_capacity(ops.len());
    for op in ops {
        match *op {
            LinearOp::Const { dst, value } => {
                const_val.insert(dst, value);
            }
            LinearOp::Move { dst, src } => {
                if let Some(&v) = const_val.get(&src) {
                    const_val.insert(dst, v);
                }
            }
            _ => {}
        }
    }
    // A square matrix is diagonal iff every off-diagonal position is zero.
    for r in 0..rows {
        for c in 0..cols {
            if r != c && const_val.get(&values[r * cols + c]) != Some(&0.0) {
                return rumoca_ir_solve::SparsityPattern::Dense;
            }
        }
    }
    rumoca_ir_solve::SparsityPattern::Diagonal
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cat_dim1_dims_rejects_overflowing_first_dimension() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_array_values_helpers_source_10.mo",
            ),
            4,
            9,
        );
        let operands = [
            ArrayOperand {
                values: Vec::new(),
                dims: vec![usize::MAX, 2],
                shape_span: span,
            },
            ArrayOperand {
                values: Vec::new(),
                dims: vec![1, 2],
                shape_span: span,
            },
        ];

        let err = cat_dim1_dims(&operands, &[usize::MAX, 2], span)
            .expect_err("cat(1) dimension overflow must fail");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "invalid IR contract: cat(1, ...) dimension overflows host index range"
        );
    }

    #[test]
    fn cat_dim2_dims_rejects_overflowing_column_count() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_array_values_helpers_source_11.mo",
            ),
            6,
            14,
        );
        let operands = [
            ArrayOperand {
                values: Vec::new(),
                dims: vec![2, usize::MAX],
                shape_span: span,
            },
            ArrayOperand {
                values: Vec::new(),
                dims: vec![2, 1],
                shape_span: span,
            },
        ];

        let err = cat_dim2_dims(&operands, &[2, usize::MAX], span)
            .expect_err("cat(2) column overflow must fail");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "invalid IR contract: cat(2, ...) column count overflows host index range"
        );
    }

    #[test]
    fn cat_dim2_dims_reports_row_mismatch_operand_span() -> Result<(), String> {
        let first_span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_array_values_helpers_source_12.mo",
            ),
            1,
            5,
        );
        let mismatch_span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_array_values_helpers_source_12.mo",
            ),
            8,
            13,
        );
        let operands = [
            ArrayOperand {
                values: Vec::new(),
                dims: vec![2, 3],
                shape_span: first_span,
            },
            ArrayOperand {
                values: Vec::new(),
                dims: vec![4, 3],
                shape_span: mismatch_span,
            },
        ];

        let Err(err) = cat_dim2_dims(&operands, &[2, 3], first_span) else {
            return Err("cat(2) row mismatch succeeded".to_string());
        };

        assert_eq!(err.source_span(), Some(mismatch_span));
        assert_eq!(
            err.reason(),
            "cat(2, ...) requires matching row counts, got 2 and 4"
        );
        Ok(())
    }

    #[test]
    fn checked_matrix_row_start_rejects_overflow() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_array_values_helpers_source_12.mo",
            ),
            3,
            7,
        );
        let err = checked_matrix_row_start_at(usize::MAX, 2, "cat(2, ...) row offset", span)
            .expect_err("matrix row offset overflow must fail");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "invalid IR contract: cat(2, ...) row offset overflows host index range"
        );
    }

    #[test]
    fn broadcast_shape_reports_mismatch_span() -> Result<(), String> {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_array_values_helpers_source_13.mo",
            ),
            2,
            11,
        );

        let Err(err) = broadcast_shape(&[2, 3], &[4, 5], span) else {
            return Err("incompatible broadcast shapes succeeded".to_string());
        };

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "array operands have incompatible shapes [2, 3] and [4, 5]"
        );
        Ok(())
    }

    #[test]
    fn multiplication_dims_reports_mismatch_span() -> Result<(), String> {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_array_values_helpers_source_14.mo",
            ),
            6,
            18,
        );

        let Err(err) = multiplication_dims(&[2, 3], &[4, 5], span) else {
            return Err("incompatible multiplication shapes succeeded".to_string());
        };

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "array multiplication shapes [2, 3] and [4, 5] are incompatible"
        );
        Ok(())
    }

    #[test]
    fn checked_shape_size_rejects_overflow_with_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_array_values_helpers_source_5.mo",
            ),
            5,
            8,
        );
        let err = checked_shape_size(&[usize::MAX, 2], "array operand shape", span)
            .expect_err("shape product overflow must fail");

        assert_eq!(
            err.reason(),
            "invalid IR contract: array operand shape overflows host index range"
        );
        assert_eq!(err.source_span(), Some(span));
    }

    #[test]
    fn one_based_index_tuples_reports_overflow_with_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("dynamic_index.mo"),
            13,
            21,
        );
        let err = one_based_index_tuples(&[usize::MAX, 2], span)
            .expect_err("overflowing index tuple count must fail");

        assert_eq!(err.source_span(), Some(span));
        assert!(
            err.to_string()
                .contains("array-like dynamic index tuple count"),
            "error should explain dynamic index tuple overflow: {err}"
        );
    }

    #[test]
    fn validate_slice_indices_reports_bounds_error_with_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("array_slice_bounds.mo"),
            20,
            23,
        );
        let err =
            validate_slice_indices(&[4], 3, span).expect_err("out-of-bounds slice index must fail");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "array slice index is outside dimension bounds"
        );
    }

    #[test]
    fn array_operand_with_shape_span_rejects_overflowing_shape() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("array_operand_shape.mo"),
            8,
            12,
        );
        let Err(err) = ArrayOperand::with_shape_span(Vec::new(), vec![usize::MAX, 2], span) else {
            panic!("overflowing normalized shape must fail");
        };

        assert_eq!(
            err.reason(),
            "invalid IR contract: array operand shape overflows host index range"
        );
        assert_eq!(err.source_span(), Some(span));
    }

    #[test]
    fn fallible_vec_with_capacity_rejects_impossible_capacity() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_array_values_helpers_source_9.mo",
            ),
            9,
            11,
        );
        let err = fallible_vec_with_capacity::<Reg>(
            usize::MAX,
            "array-like dynamic index tuple count",
            span,
        )
        .expect_err("impossible vector capacity must fail");

        assert_eq!(
            err.reason(),
            "invalid IR contract: array-like dynamic index tuple count capacity exceeds host memory limits"
        );
        assert_eq!(err.source_span(), Some(span));
    }

    #[test]
    fn fallible_vec_with_capacity_rejects_impossible_capacity_without_fabricating_span() {
        let err = fallible_vec_with_capacity::<Reg>(
            usize::MAX,
            "array-like dynamic index tuple count",
            rumoca_core::Span::DUMMY,
        )
        .expect_err("impossible vector capacity must fail");

        assert_eq!(
            err.reason(),
            "invalid IR contract: array-like dynamic index tuple count capacity exceeds host memory limits"
        );
        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    }

    #[test]
    fn checked_dim_product_rejects_overflow_without_fabricating_span() {
        let err = checked_dim_product(
            usize::MAX,
            2,
            "array test dimension product",
            rumoca_core::Span::DUMMY,
        )
        .expect_err("dimension product overflow must fail");

        assert_eq!(
            err.reason(),
            "invalid IR contract: array test dimension product overflows host index range"
        );
        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    }

    #[test]
    fn lower_static_range_values_rejects_real_overflow() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("static_range_overflow.mo"),
            3,
            16,
        );
        let start = rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(f64::MAX),
            span,
        };
        let step = rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(f64::MAX),
            span,
        };
        let end = rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(f64::MAX),
            span,
        };

        let err = lower_static_range_values(&start, Some(&step), &end, span)
            .expect_err("overflowing Real range step must fail");

        assert_eq!(err.reason(), "static range overflows finite Real values");
        assert_eq!(err.source_span(), Some(span));
    }

    #[test]
    fn detect_matrix_sparsity_treats_overflowing_extent_as_dense() {
        assert_eq!(
            detect_matrix_sparsity(&[], &[], usize::MAX, 2),
            rumoca_ir_solve::SparsityPattern::Dense
        );
    }
}
