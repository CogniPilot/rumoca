use super::*;

pub(super) fn function_loop_point(
    domain: dae::DomainView<'_>,
    point_index: usize,
    span: Span,
) -> Result<Vec<i64>, NumericEvaluationError> {
    domain
        .structured()
        .index_tuple_at(point_index)
        .map_err(|_| {
            failure(
                NumericEvaluationErrorKind::Overflow,
                "function loop domain projection overflowed",
                span,
            )
        })?
        .ok_or_else(|| {
            failure(
                NumericEvaluationErrorKind::OutOfBounds,
                "function loop domain point does not resolve",
                span,
            )
        })
}

/// Reorder one checked transpose result in row-major order. The result shape
/// already owns the exchanged first two extents, so swapping its first two
/// coordinates maps each result scalar back to the unique operand scalar.
pub(super) fn transpose_values(values: &[Scalar], result_dimensions: &[u32]) -> Vec<Scalar> {
    let mut operand_dimensions = result_dimensions.to_vec();
    operand_dimensions.swap(0, 1);
    (0..values.len())
        .map(|scalar| {
            let mut coordinates = row_major_coordinates(result_dimensions, scalar)
                .expect("checked transpose scalar belongs to its result shape");
            coordinates.swap(0, 1);
            let operand_scalar = flatten_coordinates(&operand_dimensions, &coordinates)
                .expect("transposed coordinate belongs to its checked operand shape");
            values[operand_scalar]
        })
        .collect()
}

pub(super) fn identity_values(dimensions: &[u32]) -> Vec<Scalar> {
    let [rows, columns] = dimensions else {
        unreachable!("checked identity result has rank two")
    };
    let count = (*rows as usize)
        .checked_mul(*columns as usize)
        .expect("checked identity scalar count fits usize");
    (0..count)
        .map(|scalar| {
            Scalar::Integer(i64::from(u8::from(
                scalar / *columns as usize == scalar % *columns as usize,
            )))
        })
        .collect()
}

pub(super) fn diagonal_values(values: &[Scalar]) -> Vec<Scalar> {
    let count = values
        .len()
        .checked_mul(values.len())
        .expect("checked diagonal scalar count fits usize");
    (0..count)
        .map(|scalar| {
            let row = scalar / values.len();
            let column = scalar % values.len();
            if row == column {
                values[row]
            } else {
                Scalar::Integer(0)
            }
        })
        .collect()
}

pub(super) fn outer_product_values(lhs: &[Scalar], rhs: &[Scalar]) -> Vec<Scalar> {
    lhs.iter()
        .flat_map(|lhs| rhs.iter().map(move |rhs| *lhs * *rhs))
        .collect()
}

pub(super) fn skew_values(values: &[Scalar]) -> Vec<Scalar> {
    let [x, y, z] = values else {
        unreachable!("checked skew operand has exactly three scalars")
    };
    vec![
        Scalar::Integer(0),
        -*z,
        *y,
        *z,
        Scalar::Integer(0),
        -*x,
        -*y,
        *x,
        Scalar::Integer(0),
    ]
}

pub(super) fn multiply_values(
    lhs: &[Scalar],
    rhs: &[Scalar],
    lhs_dimensions: &[u32],
    rhs_dimensions: &[u32],
    span: Span,
) -> Result<Vec<Scalar>, NumericEvaluationError> {
    if lhs_dimensions.is_empty() {
        return Ok(rhs.iter().map(|value| lhs[0] * *value).collect());
    }
    if rhs_dimensions.is_empty() {
        return Ok(lhs.iter().map(|value| *value * rhs[0]).collect());
    }
    let (rows, inner, columns) = match (lhs_dimensions, rhs_dimensions) {
        ([inner], [rhs_inner]) if inner == rhs_inner => (1usize, *inner as usize, 1usize),
        ([rows, inner], [rhs_inner]) if inner == rhs_inner => {
            (*rows as usize, *inner as usize, 1usize)
        }
        ([inner], [rhs_inner, columns]) if inner == rhs_inner => {
            (1usize, *inner as usize, *columns as usize)
        }
        ([rows, inner], [rhs_inner, columns]) if inner == rhs_inner => {
            (*rows as usize, *inner as usize, *columns as usize)
        }
        _ => {
            return Err(failure(
                NumericEvaluationErrorKind::ShapeMismatch,
                "checked multiplication shape is not computable",
                span,
            ));
        }
    };
    let mut result = Vec::with_capacity(rows * columns);
    for row in 0..rows {
        for column in 0..columns {
            let mut sum = Scalar::Integer(0);
            for term in 0..inner {
                let lhs_index = matrix_lhs_index(lhs_dimensions, row, inner, term);
                let rhs_index = matrix_rhs_index(rhs_dimensions, term, columns, column);
                sum += lhs[lhs_index] * rhs[rhs_index];
            }
            result.push(sum);
        }
    }
    Ok(result)
}

fn matrix_lhs_index(dimensions: &[u32], row: usize, inner: usize, term: usize) -> usize {
    if dimensions.len() == 1 {
        term
    } else {
        row * inner + term
    }
}

fn matrix_rhs_index(dimensions: &[u32], term: usize, columns: usize, column: usize) -> usize {
    if dimensions.len() == 1 {
        term
    } else {
        term * columns + column
    }
}

pub(super) fn expect_same_length(
    lhs: &[Scalar],
    rhs: &[Scalar],
    span: Span,
) -> Result<(), NumericEvaluationError> {
    if lhs.len() == rhs.len() {
        Ok(())
    } else {
        Err(failure(
            NumericEvaluationErrorKind::ShapeMismatch,
            "builtin argument shape mismatch",
            span,
        ))
    }
}

pub(super) fn combine_extremum(builtin: dae::PureBuiltin, lhs: &mut [Scalar], rhs: Vec<Scalar>) {
    for (lhs, rhs) in lhs.iter_mut().zip(rhs) {
        *lhs = if builtin == dae::PureBuiltin::Min {
            lhs.min(rhs)
        } else {
            lhs.max(rhs)
        };
    }
}

pub(super) fn range_values(
    start: i64,
    step: i64,
    stop: i64,
    span: Span,
) -> Result<Vec<Scalar>, NumericEvaluationError> {
    let count = if step > 0 && start <= stop {
        (i128::from(stop) - i128::from(start)) / i128::from(step) + 1
    } else if step < 0 && start >= stop {
        (i128::from(start) - i128::from(stop)) / -i128::from(step) + 1
    } else if step == 0 {
        return Err(failure(
            NumericEvaluationErrorKind::InvalidValue,
            "integer range step is zero",
            span,
        ));
    } else {
        0
    };
    let count = usize::try_from(count).map_err(|_| {
        failure(
            NumericEvaluationErrorKind::Overflow,
            "integer range cardinality overflowed",
            span,
        )
    })?;
    (0..count)
        .map(|ordinal| {
            let ordinal = i64::try_from(ordinal).map_err(|_| {
                failure(
                    NumericEvaluationErrorKind::Overflow,
                    "integer range ordinal overflowed",
                    span,
                )
            })?;
            start
                .checked_add(step.checked_mul(ordinal).ok_or_else(|| {
                    failure(
                        NumericEvaluationErrorKind::Overflow,
                        "integer range value overflowed",
                        span,
                    )
                })?)
                .map(Scalar::Integer)
                .ok_or_else(|| {
                    failure(
                        NumericEvaluationErrorKind::Overflow,
                        "integer range value overflowed",
                        span,
                    )
                })
        })
        .collect()
}

pub(super) fn expand_flat_indices(
    flats: Vec<usize>,
    selected: &[usize],
    extent: u32,
    span: Span,
) -> Result<Vec<usize>, NumericEvaluationError> {
    let capacity = flats.len().checked_mul(selected.len()).ok_or_else(|| {
        failure(
            NumericEvaluationErrorKind::Overflow,
            "array selection size overflowed",
            span,
        )
    })?;
    let mut expanded = Vec::with_capacity(capacity);
    for flat in flats {
        for index in selected {
            expanded.push(checked_expanded_index(flat, *index, extent, span)?);
        }
    }
    Ok(expanded)
}

fn checked_expanded_index(
    flat: usize,
    index: usize,
    extent: u32,
    span: Span,
) -> Result<usize, NumericEvaluationError> {
    flat.checked_mul(extent as usize)
        .and_then(|value| value.checked_add(index))
        .ok_or_else(|| {
            failure(
                NumericEvaluationErrorKind::Overflow,
                "array index calculation overflowed",
                span,
            )
        })
}
