use rumoca_ir_flat as flat;

use crate::errors::FlattenError;

pub(crate) fn structured_array_equation_family(
    equation_index: usize,
    equation: &flat::Equation,
    preferred_dims: Option<&[i64]>,
) -> Result<Option<flat::StructuredEquationFamily>, FlattenError> {
    if equation.scalar_count <= 1 {
        return Ok(None);
    }
    let dims = normalized_family_dims(preferred_dims, equation.scalar_count, equation.span)?;
    let binders = dims
        .iter()
        .copied()
        .enumerate()
        .map(|(dimension, upper)| rumoca_core::StructuredIndexBinder {
            id: dimension,
            display_name: format!("__array_i{}", dimension + 1),
            lower: 1,
            upper,
            step: 1,
        })
        .collect();
    Ok(Some(flat::StructuredEquationFamily {
        domain: rumoca_core::StructuredIndexDomain { binders },
        first_equation_index: equation_index,
        equations_per_point: 1,
        span: equation.span,
        origin: equation.origin.clone(),
        regular: None,
        template: Some(rumoca_core::ComprehensionTemplate {
            body: vec![equation.residual.clone()],
            scalar_view: rumoca_core::ComprehensionScalarView::RowMajorProjection,
        }),
        interiors_materialized: true,
    }))
}

fn normalized_family_dims(
    preferred_dims: Option<&[i64]>,
    scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<Vec<i64>, FlattenError> {
    if let Some(preferred_dims) = preferred_dims {
        let preferred_count =
            preferred_dims
                .iter()
                .copied()
                .try_fold(1usize, |count, dimension| {
                    let dimension = usize::try_from(dimension).ok()?;
                    count.checked_mul(dimension)
                });
        if preferred_count == Some(scalar_count) && !preferred_dims.is_empty() {
            return Ok(preferred_dims.to_vec());
        }
    }
    let count = i64::try_from(scalar_count).map_err(|_| {
        FlattenError::unsupported_equation(
            "array equation scalar count exceeds structured-domain index range",
            span,
        )
    })?;
    Ok(vec![count])
}
