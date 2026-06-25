//! Affine stride-term inference and numeric-conversion helpers for structured
//! tensor preservation: deriving the per-dimension integer/float stride terms that
//! reconstruct a regular family's accesses from its corner rows, and the checked
//! index conversions they use. Split out of `stencil.rs` to keep that module under
//! the SPEC_0021 size limit.

use super::*;

pub(super) fn infer_index_terms_with_span(
    base_value: usize,
    values: &[usize],
    domain: &rumoca_core::StructuredIndexDomain,
    index_tuples: &[Vec<i64>],
    span: rumoca_core::Span,
) -> Result<Option<Vec<solve::AffineStencilIndexStrideTerm>>, LowerError> {
    let Some(base_tuple) = index_tuples.first() else {
        return Ok(None);
    };
    let mut strides =
        stencil_vec_with_capacity(base_tuple.len(), "affine index stride term count", span)?;
    for dimension in 0..base_tuple.len() {
        let Some(stride) = infer_integer_dimension_stride_with_span(
            base_value,
            values,
            domain,
            index_tuples,
            dimension,
            span,
        )?
        else {
            return Ok(None);
        };
        strides.push(stride);
    }
    for (value, tuple) in values.iter().zip(index_tuples) {
        let Some(expected) =
            apply_integer_terms_with_span(base_value, &strides, domain, base_tuple, tuple, span)?
        else {
            return Ok(None);
        };
        if expected != *value {
            return Ok(None);
        }
    }
    Ok(Some(index_stride_terms(strides, span)?))
}

pub(super) fn infer_const_terms_with_span(
    base_value: f64,
    values: &[f64],
    domain: &rumoca_core::StructuredIndexDomain,
    index_tuples: &[Vec<i64>],
    span: rumoca_core::Span,
) -> Result<Option<Vec<solve::AffineStencilConstStrideTerm>>, LowerError> {
    let Some(base_tuple) = index_tuples.first() else {
        return Ok(None);
    };
    let mut strides =
        stencil_vec_with_capacity(base_tuple.len(), "affine const stride term count", span)?;
    for dimension in 0..base_tuple.len() {
        let Some(stride) = infer_float_dimension_stride_with_span(
            base_value,
            values,
            domain,
            index_tuples,
            dimension,
            span,
        )?
        else {
            return Ok(None);
        };
        strides.push(stride);
    }
    for (value, tuple) in values.iter().zip(index_tuples) {
        let Some(expected) =
            apply_float_terms_with_span(base_value, &strides, domain, base_tuple, tuple, span)?
        else {
            return Ok(None);
        };
        if (expected - value).abs() > 1e-9 {
            return Ok(None);
        }
    }
    Ok(Some(const_stride_terms(strides, span)?))
}

pub(super) fn infer_integer_dimension_stride_with_span(
    base_value: usize,
    values: &[usize],
    domain: &rumoca_core::StructuredIndexDomain,
    index_tuples: &[Vec<i64>],
    dimension: usize,
    span: rumoca_core::Span,
) -> Result<Option<isize>, LowerError> {
    let Some(base_tuple) = index_tuples.first() else {
        return Ok(None);
    };
    let base_value = checked_usize_to_isize(base_value, "affine index base value", span)?;
    for (value, tuple) in values.iter().zip(index_tuples).skip(1) {
        if !only_dimension_changes(base_tuple, tuple, dimension) {
            continue;
        }
        let Some(ordinal) = ordinal_delta_with_span(dimension, domain, base_tuple, tuple, span)?
        else {
            return Ok(None);
        };
        if ordinal == 0 {
            continue;
        }
        let value = checked_usize_to_isize(*value, "affine index value", span)?;
        let ordinal = checked_i64_to_isize(ordinal, "affine index ordinal", span)?;
        let delta = value.checked_sub(base_value).ok_or_else(|| {
            stencil_contract_violation("affine index delta overflows host index range", span)
        })?;
        let remainder = delta.checked_rem(ordinal).ok_or_else(|| {
            stencil_contract_violation(
                "affine index delta remainder overflows host index range",
                span,
            )
        })?;
        if remainder != 0 {
            return Ok(None);
        }
        let stride = delta.checked_div(ordinal).ok_or_else(|| {
            stencil_contract_violation("affine index stride overflows host index range", span)
        })?;
        return Ok(Some(stride));
    }
    Ok(Some(0))
}

pub(super) fn infer_float_dimension_stride_with_span(
    base_value: f64,
    values: &[f64],
    domain: &rumoca_core::StructuredIndexDomain,
    index_tuples: &[Vec<i64>],
    dimension: usize,
    span: rumoca_core::Span,
) -> Result<Option<f64>, LowerError> {
    let Some(base_tuple) = index_tuples.first() else {
        return Ok(None);
    };
    for (value, tuple) in values.iter().zip(index_tuples).skip(1) {
        if !only_dimension_changes(base_tuple, tuple, dimension) {
            continue;
        }
        let Some(ordinal) = ordinal_delta_with_span(dimension, domain, base_tuple, tuple, span)?
        else {
            return Ok(None);
        };
        if ordinal == 0 {
            continue;
        }
        let stride = (*value - base_value) / ordinal as f64;
        if !stride.is_finite() {
            return Err(stencil_contract_violation(
                "affine const stride is not finite",
                span,
            ));
        }
        return Ok(Some(stride));
    }
    Ok(Some(0.0))
}

pub(super) fn only_dimension_changes(base_tuple: &[i64], tuple: &[i64], dimension: usize) -> bool {
    tuple
        .iter()
        .enumerate()
        .all(|(idx, value)| idx == dimension || *value == base_tuple[idx])
        && tuple[dimension] != base_tuple[dimension]
}

pub(super) fn apply_integer_terms_with_span(
    base_value: usize,
    strides: &[isize],
    domain: &rumoca_core::StructuredIndexDomain,
    base_tuple: &[i64],
    tuple: &[i64],
    span: rumoca_core::Span,
) -> Result<Option<usize>, LowerError> {
    let mut value = checked_usize_to_isize(base_value, "affine index base value", span)?;
    for (dimension, stride) in strides.iter().enumerate() {
        let Some(ordinal) = ordinal_delta_with_span(dimension, domain, base_tuple, tuple, span)?
        else {
            return Ok(None);
        };
        let ordinal = checked_i64_to_isize(ordinal, "affine index ordinal", span)?;
        let term = ordinal.checked_mul(*stride).ok_or_else(|| {
            stencil_contract_violation("affine index stride term overflows host index range", span)
        })?;
        value = value.checked_add(term).ok_or_else(|| {
            stencil_contract_violation("affine index term sum overflows host index range", span)
        })?;
    }
    Ok(usize::try_from(value).ok())
}

pub(super) fn apply_float_terms_with_span(
    base_value: f64,
    strides: &[f64],
    domain: &rumoca_core::StructuredIndexDomain,
    base_tuple: &[i64],
    tuple: &[i64],
    span: rumoca_core::Span,
) -> Result<Option<f64>, LowerError> {
    let mut value = base_value;
    for (dimension, stride) in strides.iter().enumerate() {
        if let Some(ordinal) = ordinal_delta_with_span(dimension, domain, base_tuple, tuple, span)?
        {
            let term = ordinal as f64 * stride;
            if !term.is_finite() {
                return Err(stencil_contract_violation(
                    "affine const stride term is not finite",
                    span,
                ));
            }
            value += term;
            if !value.is_finite() {
                return Err(stencil_contract_violation(
                    "affine const term sum is not finite",
                    span,
                ));
            }
        }
    }
    Ok(Some(value))
}

pub(super) fn ordinal_delta_with_span(
    dimension: usize,
    domain: &rumoca_core::StructuredIndexDomain,
    base_tuple: &[i64],
    tuple: &[i64],
    span: rumoca_core::Span,
) -> Result<Option<i64>, LowerError> {
    let Some(binder) = domain.binders.get(dimension) else {
        return Ok(None);
    };
    let step = binder.step;
    if step == 0 {
        return Err(stencil_contract_violation(
            "structured affine domain binder has zero step",
            span,
        ));
    }
    let delta = tuple[dimension]
        .checked_sub(base_tuple[dimension])
        .ok_or_else(|| {
            stencil_contract_violation("structured affine ordinal delta overflows i64", span)
        })?;
    if delta % step != 0 {
        return Ok(None);
    }
    delta
        .checked_div(step)
        .map(Some)
        .ok_or_else(|| stencil_contract_violation("structured affine ordinal overflows i64", span))
}

pub(super) fn checked_usize_to_isize(
    value: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<isize, LowerError> {
    isize::try_from(value).map_err(|_| {
        stencil_contract_violation(
            format!("{context} does not fit in host signed index range"),
            span,
        )
    })
}

pub(super) fn checked_i64_to_isize(
    value: i64,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<isize, LowerError> {
    isize::try_from(value).map_err(|_| {
        stencil_contract_violation(
            format!("{context} does not fit in host signed index range"),
            span,
        )
    })
}
