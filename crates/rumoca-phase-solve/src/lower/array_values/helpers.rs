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
    pub(super) fn new(values: Vec<Reg>, dims: Vec<usize>) -> Self {
        let dims = normalize_operand_dims(values.len(), dims);
        Self { values, dims }
    }

    pub(super) fn scalar(value: Reg) -> Self {
        Self {
            values: vec![value],
            dims: Vec::new(),
        }
    }

    pub(super) fn is_scalar(&self) -> bool {
        self.dims.is_empty() && self.values.len() == 1
    }
}

pub(super) fn cat_array_operands(
    dim: usize,
    operands: &[ArrayOperand],
) -> Result<ArrayOperand, LowerError> {
    let dims = cat_array_dims(dim, operands)?;
    if operands.is_empty() {
        return Ok(ArrayOperand::new(Vec::new(), dims));
    }
    let values = match dim {
        1 => operands
            .iter()
            .flat_map(|operand| operand.values.iter().copied())
            .collect(),
        2 => cat_matrix_columns(operands)?,
        _ => {
            return Err(LowerError::Unsupported {
                reason: format!("cat dimension {dim} is unsupported for compiled array lowering"),
            });
        }
    };
    Ok(ArrayOperand::new(values, dims))
}

pub(super) fn cat_array_dims(
    dim: usize,
    operands: &[ArrayOperand],
) -> Result<Vec<usize>, LowerError> {
    let Some(first) = operands.first() else {
        return Ok(Vec::new());
    };
    match dim {
        1 => cat_dim1_dims(operands, &first.dims),
        2 => cat_dim2_dims(operands, &first.dims),
        _ => Err(LowerError::Unsupported {
            reason: format!("cat dimension {dim} is not supported yet"),
        }),
    }
}

pub(super) fn cat_dim1_dims(
    operands: &[ArrayOperand],
    first_dims: &[usize],
) -> Result<Vec<usize>, LowerError> {
    if first_dims.is_empty() {
        return Ok(vector_dims(operands.len()));
    }
    let tail = &first_dims[1..];
    let mut total = 0usize;
    for operand in operands {
        if operand.dims.len() != first_dims.len() || &operand.dims[1..] != tail {
            return Err(LowerError::Unsupported {
                reason: format!(
                    "cat(1, ...) requires matching trailing dimensions, got {:?} and {:?}",
                    first_dims, operand.dims
                ),
            });
        }
        total += operand.dims[0];
    }
    let mut dims = vec![total];
    dims.extend_from_slice(tail);
    Ok(dims)
}

pub(super) fn cat_dim2_dims(
    operands: &[ArrayOperand],
    first_dims: &[usize],
) -> Result<Vec<usize>, LowerError> {
    let [rows, cols] = first_dims else {
        return Err(LowerError::Unsupported {
            reason: "cat(2, ...) currently requires matrix operands".to_string(),
        });
    };
    let mut total_cols = *cols;
    for operand in operands.iter().skip(1) {
        let [operand_rows, operand_cols] = operand.dims.as_slice() else {
            return Err(LowerError::Unsupported {
                reason: "cat(2, ...) currently requires matrix operands".to_string(),
            });
        };
        if operand_rows != rows {
            return Err(LowerError::Unsupported {
                reason: format!(
                    "cat(2, ...) requires matching row counts, got {rows} and {operand_rows}"
                ),
            });
        }
        total_cols += operand_cols;
    }
    Ok(vec![*rows, total_cols])
}

pub(super) fn cat_matrix_columns(operands: &[ArrayOperand]) -> Result<Vec<Reg>, LowerError> {
    let Some(first) = operands.first() else {
        return Ok(Vec::new());
    };
    let [rows, _] = first.dims.as_slice() else {
        return Err(LowerError::Unsupported {
            reason: "cat(2, ...) currently requires matrix operands".to_string(),
        });
    };

    let mut values = Vec::new();
    for row in 0..*rows {
        for operand in operands {
            let [_, cols] = operand.dims.as_slice() else {
                return Err(LowerError::Unsupported {
                    reason: "cat(2, ...) currently requires matrix operands".to_string(),
                });
            };
            let start = row * cols;
            values.extend_from_slice(&operand.values[start..start + cols]);
        }
    }
    Ok(values)
}

pub(super) fn normalize_operand_dims(value_count: usize, dims: Vec<usize>) -> Vec<usize> {
    if !dims.is_empty() && shape_size(&dims) == value_count {
        return dims;
    }
    if value_count <= 1 {
        return Vec::new();
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
        return vec![*outer, value_count / *outer];
    }
    vector_dims(value_count)
}

pub(super) fn one_based_index_tuples(dims: &[usize]) -> Vec<Vec<usize>> {
    let mut tuples = Vec::new();
    collect_one_based_index_tuples(dims, 0, &mut Vec::new(), &mut tuples);
    tuples
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

pub(super) fn index_choice_tuples(choices: &[Vec<usize>]) -> Vec<Vec<usize>> {
    let mut tuples = Vec::new();
    collect_index_choice_tuples(choices, 0, &mut Vec::new(), &mut tuples);
    tuples
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
    builder: &LowerBuilder<'_>,
    scope: &Scope,
) -> Result<Vec<usize>, LowerError> {
    let mut dims = Vec::new();
    for (dim_index, subscript) in subscripts.iter().enumerate() {
        let dim = base_dims[dim_index];
        match subscript {
            rumoca_core::Subscript::Colon { .. } => dims.extend(vector_dims(dim)),
            rumoca_core::Subscript::Expr { expr, .. }
                if matches!(expr.as_ref(), rumoca_core::Expression::Range { .. }) =>
            {
                dims.extend(vector_dims(
                    builder.slice_expr_indices(expr, dim, scope)?.len(),
                ));
            }
            _ => {}
        }
    }
    for &dim in &base_dims[subscripts.len()..] {
        dims.extend(vector_dims(dim));
    }
    Ok(dims)
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

pub(super) fn shape_size(dims: &[usize]) -> usize {
    if dims.is_empty() {
        1
    } else {
        dims.iter().product()
    }
}

pub(super) fn shape_size_or_scalar(dims: &[usize]) -> usize {
    shape_size(dims).max(1)
}

pub(super) fn broadcast_shape(lhs: &[usize], rhs: &[usize]) -> Result<Vec<usize>, LowerError> {
    if lhs.is_empty() {
        return Ok(rhs.to_vec());
    }
    if rhs.is_empty() || lhs == rhs {
        return Ok(lhs.to_vec());
    }
    if shape_size(lhs) == shape_size(rhs) {
        // Record-valued function outputs can arrive at Solve IR with their
        // scalar width preserved but rank metadata flattened. At this boundary
        // elementwise rows are already scalarized, so equal scalar counts can
        // still be paired deterministically.
        return Ok(lhs.to_vec());
    }
    Err(LowerError::Unsupported {
        // MLS §10 and §10.6.2: array binary operations require matching sizes
        // unless one operand is scalar.
        reason: format!("array operands have incompatible shapes {lhs:?} and {rhs:?}"),
    })
}

pub(super) fn multiplication_dims(lhs: &[usize], rhs: &[usize]) -> Result<Vec<usize>, LowerError> {
    match (lhs, rhs) {
        ([], []) => Ok(Vec::new()),
        ([], dims) | (dims, []) => Ok(dims.to_vec()),
        ([lhs_len], [rhs_len]) if lhs_len == rhs_len => Ok(Vec::new()),
        ([rows, inner], [rhs_len]) if inner == rhs_len => Ok(vec![*rows]),
        ([lhs_len], [inner, cols]) if lhs_len == inner => Ok(vec![*cols]),
        ([rows, inner], [rhs_inner, cols]) if inner == rhs_inner => Ok(vec![*rows, *cols]),
        _ => Err(LowerError::Unsupported {
            // MLS §10.6.5 defines ordinary multiplication for scalar scaling,
            // vector scalar products, matrix-vector, vector-matrix, and
            // matrix-matrix products.
            reason: format!("array multiplication shapes {lhs:?} and {rhs:?} are incompatible"),
        }),
    }
}

pub(super) fn broadcast_pairs(
    lhs: &ArrayOperand,
    rhs: &ArrayOperand,
) -> Result<Vec<(Reg, Reg)>, LowerError> {
    if let Some(pairs) = trailing_vector_broadcast_pairs(lhs, rhs) {
        return Ok(pairs);
    }
    let dims = broadcast_dims(lhs, rhs)?;
    let count = shape_size_or_scalar(&dims);
    let pairs = (0..count)
        .map(|idx| {
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
            (lhs, rhs)
        })
        .collect();
    Ok(pairs)
}

fn trailing_vector_broadcast_pairs(
    lhs: &ArrayOperand,
    rhs: &ArrayOperand,
) -> Option<Vec<(Reg, Reg)>> {
    match (lhs.dims.as_slice(), rhs.dims.as_slice()) {
        ([_, cols], [rhs_cols]) if cols == rhs_cols => Some(
            lhs.values
                .iter()
                .copied()
                .enumerate()
                .map(|(idx, lhs)| (lhs, rhs.values[idx % cols]))
                .collect(),
        ),
        ([lhs_cols], [_, cols]) if lhs_cols == cols => Some(
            rhs.values
                .iter()
                .copied()
                .enumerate()
                .map(|(idx, rhs)| (lhs.values[idx % cols], rhs))
                .collect(),
        ),
        _ => None,
    }
}

pub(super) fn broadcast_dims(
    lhs: &ArrayOperand,
    rhs: &ArrayOperand,
) -> Result<Vec<usize>, LowerError> {
    broadcast_shape(&lhs.dims, &rhs.dims)
}

pub(super) fn lower_static_range_values(
    start: &rumoca_core::Expression,
    step: Option<&rumoca_core::Expression>,
    end: &rumoca_core::Expression,
) -> Result<Option<Vec<f64>>, LowerError> {
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
        return Err(LowerError::Unsupported {
            reason: "invalid static range expression in compiled lowering".to_string(),
        });
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
        value += step_v;
    }
    let past_end = (step_v > 0.0 && value > end_v + tol) || (step_v < 0.0 && value < end_v - tol);
    if !past_end {
        return Err(LowerError::Unsupported {
            reason: format!(
                "static range exceeds maximum lowered range length {MAX_STATIC_RANGE_VALUES}"
            ),
        });
    }
    Ok(Some(values))
}

pub(super) fn scoped_indexed_binding_values(
    scope: &Scope,
    path: &ComponentReferenceKey,
) -> Option<Vec<Reg>> {
    scope.indexed_values(path)
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
    if let Some(values) = scoped_indexed_binding_values(scope, &output_path)
        && values.len() == width
    {
        return Ok(values);
    }

    let mut values = Vec::with_capacity(width);
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

pub(super) fn record_output_component_values(output_name: &str, scope: &Scope) -> Option<Vec<Reg>> {
    let output_prefix = format!("{output_name}.");
    let entries = scope
        .iter()
        .into_iter()
        .filter(|(key, _)| {
            generated_scope_key_name(key).is_some_and(|name| name.starts_with(&output_prefix))
        })
        .collect::<Vec<_>>();
    if entries.is_empty() {
        return None;
    }

    let keys = entries
        .iter()
        .filter_map(|(key, _)| generated_scope_key_name(key).map(ToString::to_string))
        .collect::<Vec<_>>();
    let values = entries
        .into_iter()
        .filter(|(key, _)| {
            let Some(key) = generated_scope_key_name(key) else {
                return false;
            };
            !keys.iter().any(|candidate| {
                candidate.len() > key.len()
                    && (candidate.starts_with(&format!("{key}["))
                        || candidate.starts_with(&format!("{key}.")))
            })
        })
        .map(|(_, reg)| reg)
        .collect::<Vec<_>>();
    (!values.is_empty()).then_some(values)
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

pub(super) fn validate_slice_indices(indices: &[usize], dim: usize) -> Result<(), LowerError> {
    if indices.iter().all(|index| *index > 0 && *index <= dim) {
        return Ok(());
    }
    Err(LowerError::Unsupported {
        reason: "array slice index is outside dimension bounds".to_string(),
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
    if rows != cols || values.len() != rows * cols {
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
