//! Source-bound compact READ indexing; update matching has a separate owner.
use crate::{EvalSolveError, invalid_row};
use rumoca_core::Span;
use rumoca_ir_solve::{Reg, TensorIndex};

pub(super) fn tensor_register_offset(
    dimensions: &[u32],
    indices: &[TensorIndex],
    span: Option<Span>,
    mut read: impl FnMut(Reg) -> Result<f64, EvalSolveError>,
) -> Result<usize, EvalSolveError> {
    if dimensions.is_empty() || dimensions.len() != indices.len() {
        return Err(
            invalid_row("tensor read rank does not match its checked dimensions")
                .with_source_span(span),
        );
    }
    let mut offset = 0usize;
    for (axis, (&extent, index)) in dimensions.iter().zip(indices).enumerate() {
        if extent == 0 {
            return Err(invalid_row("tensor read has an empty indexed axis").with_source_span(span));
        }
        let index = match *index {
            TensorIndex::Constant(coordinate) => i64::from(coordinate) + 1,
            TensorIndex::Runtime(register) => exact_index(read(register)?, axis + 1, span)?,
        };
        if index < 1 || index > i64::from(extent) {
            return Err(EvalSolveError::TensorIndexOutOfBounds {
                axis: axis + 1,
                index,
                extent,
                span,
            });
        }
        offset = offset
            .checked_mul(extent as usize)
            .and_then(|n| n.checked_add(index as usize - 1))
            .ok_or_else(|| {
                invalid_row("tensor read row-major offset overflows").with_source_span(span)
            })?;
    }
    Ok(offset)
}

fn exact_index(value: f64, axis: usize, span: Option<Span>) -> Result<i64, EvalSolveError> {
    // The upper endpoint is exclusive: i64::MAX rounds to 2^63 in Binary64.
    if !value.is_finite()
        || value.trunc() != value
        || !(-9223372036854775808.0..9223372036854775808.0).contains(&value)
    {
        return Err(EvalSolveError::InvalidTensorIndex { axis, value, span });
    }
    Ok(value as i64)
}

pub(super) fn fmt_index_fault(
    error: &EvalSolveError,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    match error {
        EvalSolveError::InvalidTensorIndex { axis, value, .. } => write!(
            f,
            "Solve-IR tensor index {value} on axis {axis} is not an exact finite signed Integer"
        ),
        EvalSolveError::TensorIndexOutOfBounds {
            axis,
            index,
            extent,
            ..
        } => write!(
            f,
            "Solve-IR tensor index {index} on axis {axis} is outside 1..={extent}"
        ),
        _ => unreachable!("tensor-index diagnostic dispatch receives an index fault"),
    }
}
