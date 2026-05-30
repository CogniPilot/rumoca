use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ExpressionShape {
    Scalar,
    Vector(usize),
    Matrix(usize, usize),
    Other,
}

impl ExpressionShape {
    pub(super) fn is_singleton_array(self) -> bool {
        matches!(self, Self::Vector(1) | Self::Matrix(1, 1))
    }
}

pub(super) fn shape_from_dims(dims: &[i64]) -> ExpressionShape {
    match dims {
        [] => ExpressionShape::Scalar,
        [n] => ExpressionShape::Vector((*n).max(0) as usize),
        [rows, cols] => ExpressionShape::Matrix((*rows).max(0) as usize, (*cols).max(0) as usize),
        _ => ExpressionShape::Other,
    }
}

pub(super) fn range_subscript_indices(
    expr: &Expression,
    structural_values: &HashMap<String, i64>,
) -> Option<Vec<i64>> {
    let Expression::Range {
        start, step, end, ..
    } = expr
    else {
        return None;
    };
    let empty_dims = HashMap::new();
    let start = eval_structural_int_expr(start, structural_values, &empty_dims)?;
    let end = eval_structural_int_expr(end, structural_values, &empty_dims)?;
    let step = step
        .as_ref()
        .map(|expr| eval_structural_int_expr(expr, structural_values, &empty_dims))
        .unwrap_or_else(|| Some(if end >= start { 1 } else { -1 }))?;
    if step == 0 {
        return None;
    }

    let mut values = Vec::new();
    let mut value = start;
    for _ in 0..100_000 {
        if (step > 0 && value > end) || (step < 0 && value < end) {
            break;
        }
        values.push(value);
        value = value.checked_add(step)?;
    }
    Some(values)
}

pub(super) fn apply_subscripts_to_dims(
    dims: &[i64],
    subscripts: &[Subscript],
    structural_values: &HashMap<String, i64>,
) -> Option<Vec<i64>> {
    let mut remaining = Vec::new();
    let mut dim_idx = 0usize;
    for subscript in subscripts {
        if dim_idx >= dims.len() {
            break;
        }
        match subscript {
            Subscript::Index { .. } => dim_idx += 1,
            Subscript::Expr { expr, .. } => {
                if let Some(indices) = range_subscript_indices(expr, structural_values) {
                    remaining.push(indices.len() as i64);
                }
                dim_idx += 1;
            }
            Subscript::Colon { .. } => {
                remaining.push(dims[dim_idx]);
                dim_idx += 1;
            }
        }
    }
    remaining.extend_from_slice(&dims[dim_idx..]);
    Some(remaining)
}

pub(super) fn linear_index_for_static_subscripts(
    dims: &[i64],
    subscripts: &[Subscript],
    structural_values: &HashMap<String, i64>,
) -> Option<usize> {
    if dims.len() != subscripts.len() {
        return None;
    }
    let mut linear = 0usize;
    let mut stride = 1usize;
    for (dim, subscript) in dims.iter().zip(subscripts).rev() {
        let dim = (*dim > 0).then_some(*dim as usize)?;
        let index = match subscript {
            Subscript::Index { value: i, .. } => (*i > 0).then_some(*i as usize)?,
            Subscript::Expr { expr, .. } => {
                let value = eval_structural_int_expr(expr, structural_values, &HashMap::new())?;
                (value > 0).then_some(value as usize)?
            }
            Subscript::Colon { .. } => return None,
        };
        if index > dim {
            return None;
        }
        linear += (index - 1) * stride;
        stride *= dim;
    }
    Some(linear + 1)
}

pub(super) fn array_literal_shape(elements: &[Expression], is_matrix: bool) -> ExpressionShape {
    if elements.is_empty() {
        return if is_matrix {
            ExpressionShape::Matrix(0, 0)
        } else {
            ExpressionShape::Vector(0)
        };
    }
    if let Expression::Array {
        elements: first_row,
        ..
    } = &elements[0]
    {
        ExpressionShape::Matrix(elements.len(), first_row.len())
    } else if !is_matrix {
        ExpressionShape::Vector(elements.len())
    } else {
        ExpressionShape::Matrix(1, elements.len())
    }
}

pub(super) fn shape_scalar_count(shape: ExpressionShape) -> Option<usize> {
    match shape {
        ExpressionShape::Scalar => Some(1),
        ExpressionShape::Vector(n) => Some(n),
        ExpressionShape::Matrix(rows, cols) => Some(rows * cols),
        ExpressionShape::Other => None,
    }
}

pub(super) fn combine_additive_shapes(
    lhs: ExpressionShape,
    rhs: ExpressionShape,
) -> ExpressionShape {
    match (lhs, rhs) {
        (ExpressionShape::Scalar, ExpressionShape::Scalar) => ExpressionShape::Scalar,
        (ExpressionShape::Vector(n), ExpressionShape::Scalar)
        | (ExpressionShape::Scalar, ExpressionShape::Vector(n)) => ExpressionShape::Vector(n),
        (ExpressionShape::Matrix(r, c), ExpressionShape::Scalar)
        | (ExpressionShape::Scalar, ExpressionShape::Matrix(r, c)) => ExpressionShape::Matrix(r, c),
        (ExpressionShape::Vector(a), ExpressionShape::Vector(b)) if a == b => {
            ExpressionShape::Vector(a)
        }
        (ExpressionShape::Matrix(a_r, a_c), ExpressionShape::Matrix(b_r, b_c))
            if a_r == b_r && a_c == b_c =>
        {
            ExpressionShape::Matrix(a_r, a_c)
        }
        _ => ExpressionShape::Other,
    }
}

pub(super) fn combine_matrix_mul_shapes(
    lhs: ExpressionShape,
    rhs: ExpressionShape,
) -> ExpressionShape {
    match (lhs, rhs) {
        (ExpressionShape::Scalar, ExpressionShape::Scalar) => ExpressionShape::Scalar,
        (ExpressionShape::Vector(n), ExpressionShape::Scalar)
        | (ExpressionShape::Scalar, ExpressionShape::Vector(n)) => ExpressionShape::Vector(n),
        (ExpressionShape::Matrix(r, c), ExpressionShape::Scalar)
        | (ExpressionShape::Scalar, ExpressionShape::Matrix(r, c)) => ExpressionShape::Matrix(r, c),
        (ExpressionShape::Vector(a), ExpressionShape::Vector(b)) if a == b => {
            ExpressionShape::Scalar
        }
        (ExpressionShape::Matrix(r, c), ExpressionShape::Vector(n)) if c == n => {
            ExpressionShape::Vector(r)
        }
        (ExpressionShape::Vector(n), ExpressionShape::Matrix(r, c)) if n == r => {
            ExpressionShape::Vector(c)
        }
        (ExpressionShape::Matrix(a_r, a_c), ExpressionShape::Matrix(b_r, b_c)) if a_c == b_r => {
            ExpressionShape::Matrix(a_r, b_c)
        }
        _ => ExpressionShape::Other,
    }
}

pub(super) fn combine_elementwise_shapes(
    lhs: ExpressionShape,
    rhs: ExpressionShape,
) -> ExpressionShape {
    combine_additive_shapes(lhs, rhs)
}

pub(super) fn combine_division_shapes(
    lhs: ExpressionShape,
    rhs: ExpressionShape,
) -> ExpressionShape {
    match (lhs, rhs) {
        (shape, ExpressionShape::Scalar) => shape,
        _ => ExpressionShape::Other,
    }
}

pub(super) fn row_major_subscripts_2d(linear_index: usize, cols: usize) -> (usize, usize) {
    let zero_based = linear_index.saturating_sub(1);
    (zero_based / cols + 1, zero_based % cols + 1)
}

pub(super) fn matrix_linear_index(row: usize, col: usize, cols: usize) -> usize {
    (row - 1) * cols + col
}

pub(super) fn linear_subscripts_for_dims(dims: &[i64], linear_index: usize) -> Vec<Subscript> {
    if dims.is_empty() {
        return Vec::new();
    }
    let mut remainder = linear_index.saturating_sub(1);
    let mut indices = vec![1usize; dims.len()];
    for dim_idx in (0..dims.len()).rev() {
        let dim = dims[dim_idx].max(1) as usize;
        indices[dim_idx] = remainder % dim + 1;
        remainder /= dim;
    }
    indices
        .into_iter()
        .map(|idx| Subscript::generated_index(idx as i64, rumoca_core::Span::DUMMY))
        .collect()
}
