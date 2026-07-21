use super::*;

pub(super) fn projected_array_expression(
    values: &[rumoca_core::Expression],
    dims: &[i64],
    span: rumoca_core::Span,
) -> Result<rumoca_core::Expression, LowerError> {
    if dims.is_empty() {
        let [value] = values else {
            return Err(LowerError::contract_violation(
                format!(
                    "scalar projected array leaf contains {} values",
                    values.len()
                ),
                span,
            ));
        };
        return Ok(value.clone());
    }
    let expected = scalar_count_for_dims(dims, "projected array expression dimensions", span)?;
    if values.len() != expected {
        return Err(LowerError::contract_violation(
            format!(
                "projected array expression dimensions {} require {expected} values, got {}",
                crate::lower::helpers::format_i64_dims(dims),
                values.len()
            ),
            span,
        ));
    }
    let outer = usize::try_from(dims[0]).map_err(|_| {
        LowerError::contract_violation(
            format!(
                "projected array dimension must be non-negative, got {}",
                dims[0]
            ),
            span,
        )
    })?;
    let child_width = scalar_count_for_dims(&dims[1..], "projected array child dimensions", span)?;
    let mut elements =
        projection_vec_with_capacity(outer, "projected array expression element count", span)?;
    for index in 0..outer {
        let start = index.checked_mul(child_width).ok_or_else(|| {
            LowerError::contract_violation(
                "projected array child offset overflows host index range",
                span,
            )
        })?;
        let end = start.checked_add(child_width).ok_or_else(|| {
            LowerError::contract_violation(
                "projected array child end overflows host index range",
                span,
            )
        })?;
        elements.push(projected_array_expression(
            &values[start..end],
            &dims[1..],
            span,
        )?);
    }
    Ok(rumoca_core::Expression::Array {
        elements,
        is_matrix: dims.len() == 2,
        span,
    })
}
