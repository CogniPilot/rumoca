//! One source-owner ordering for derived continuous scalar views.

use rumoca_ir_dae as dae;

use crate::StructuralError;

pub(crate) struct ScalarResidual<'dae, 'point> {
    pub expression: dae::ExprId<'dae>,
    pub scalar: usize,
    pub domain_point: Option<(dae::DomainId<'dae>, &'point [i64])>,
    pub provenance: dae::DaeProvenance,
}

pub(crate) fn visit_owner_rows<'dae>(
    view: dae::DaeView<'dae>,
    owner: dae::ContinuousOwnerView<'dae>,
    mut visit: impl FnMut(ScalarResidual<'dae, '_>) -> Result<(), StructuralError>,
) -> Result<(), StructuralError> {
    let family = match owner {
        dae::ContinuousOwnerView::Residual { equation, .. } => {
            return visit(ScalarResidual {
                expression: equation.residual(),
                scalar: 0,
                domain_point: None,
                provenance: equation.provenance(),
            });
        }
        dae::ContinuousOwnerView::Structured { family, .. } => family,
    };
    let domain = view
        .domain(family.domain())
        .expect("checked structured family domain resolves");
    for point in 0..domain.scalar_count() as usize {
        let values = domain
            .structured()
            .index_tuple_at(point)
            .expect("checked structured domain stays valid")
            .expect("point ordinal is inside checked domain");
        let scalar = family
            .scalar_view()
            .body_scalar(point, domain.extents())
            .expect("checked family view projects its domain point");
        for expression in family.bodies().iter() {
            visit(ScalarResidual {
                expression,
                scalar,
                domain_point: Some((family.domain(), &values)),
                provenance: family.provenance(),
            })?;
        }
    }
    Ok(())
}

pub(crate) fn projection_error(error: rumoca_eval_dae::ProjectionError) -> StructuralError {
    use rumoca_eval_dae::ProjectionError;
    let span = match &error {
        ProjectionError::ScalarOutOfBounds { span, .. }
        | ProjectionError::DynamicSubscript { span }
        | ProjectionError::IndexOutOfBounds { span, .. }
        | ProjectionError::IntegerOverflow { span }
        | ProjectionError::FunctionRecursion { span }
        | ProjectionError::UnsupportedRecordOperation { span, .. }
        | ProjectionError::ExternalFunction { span, .. } => *span,
    };
    StructuralError::Projection {
        reason: error.to_string(),
        span,
    }
}
