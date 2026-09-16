//! Matched source row views shared by derivative substitution and initialization.

use super::{
    ScalarRowSource, continuous_scalar_row_count, first_model_span, owner_provenance, scalar_count,
};
use crate::LowerError;
use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_phase_structural::UnknownId;
use std::collections::HashMap;

/// Source equations named by the structural matching. Tensor and structured
/// rows retain their exact component and enclosing domain point.
#[derive(Default)]
pub(super) struct ContinuousRowIndex<'dae> {
    rows: HashMap<UnknownId<'dae>, ScalarRowSource<'dae>>,
}

impl<'dae> ContinuousRowIndex<'dae> {
    pub(super) fn definition(
        &self,
        state: dae::StateId<'dae>,
        scalar: usize,
    ) -> Option<&ScalarRowSource<'dae>> {
        let scalar = u32::try_from(scalar).ok()?;
        self.rows.get(&UnknownId::Derivative { state, scalar })
    }

    pub(super) fn algebraic_definition(
        &self,
        variable: dae::AlgebraicId<'dae>,
        scalar: usize,
    ) -> Option<&ScalarRowSource<'dae>> {
        let scalar = u32::try_from(scalar).ok()?;
        self.rows.get(&UnknownId::Algebraic { variable, scalar })
    }
}

pub(super) fn index_continuous_rows<'dae>(
    view: dae::DaeView<'dae>,
    matching: &HashMap<usize, UnknownId<'dae>>,
) -> Result<ContinuousRowIndex<'dae>, LowerError> {
    let mut rows = HashMap::new();
    let mut row = 0usize;
    for owner in view.continuous_owners() {
        let span = owner_provenance(owner).span();
        match owner {
            dae::ContinuousOwnerView::Residual { equation, .. } => {
                for scalar in 0..scalar_count(view, equation.residual()) {
                    insert_matched_row(
                        matching,
                        row,
                        ScalarRowSource {
                            expression: equation.residual(),
                            scalar,
                            domain_point: None,
                        },
                        &mut rows,
                        span,
                    )?;
                    row += 1;
                }
            }
            dae::ContinuousOwnerView::Structured { family, .. } => {
                row = index_family_rows(view, matching, row, family, &mut rows, span)?;
            }
        }
    }
    if row != continuous_scalar_row_count(view)? {
        return Err(LowerError::contract(
            "continuous row enumeration disagrees with the checked row count",
            first_model_span(view),
        ));
    }
    Ok(ContinuousRowIndex { rows })
}

fn index_family_rows<'dae>(
    view: dae::DaeView<'dae>,
    matching: &HashMap<usize, UnknownId<'dae>>,
    mut row: usize,
    family: dae::StructuredFamilyView<'dae>,
    rows: &mut HashMap<UnknownId<'dae>, ScalarRowSource<'dae>>,
    span: Span,
) -> Result<usize, LowerError> {
    let domain = view
        .domain(family.domain())
        .expect("checked family domain resolves");
    for point in 0..domain.scalar_count() as usize {
        let values = domain
            .structured()
            .index_tuple_at(point)
            .expect("checked domain remains valid")
            .expect("checked point ordinal is in range");
        for body in family.bodies().iter() {
            let scalar = family
                .scalar_view()
                .body_scalar(point, domain.extents())
                .expect("checked family view projects its domain point");
            insert_matched_row(
                matching,
                row,
                ScalarRowSource {
                    expression: body,
                    scalar,
                    domain_point: Some((family.domain(), values.clone())),
                },
                rows,
                span,
            )?;
            row += 1;
        }
    }
    Ok(row)
}

fn insert_matched_row<'dae>(
    matching: &HashMap<usize, UnknownId<'dae>>,
    row: usize,
    source: ScalarRowSource<'dae>,
    rows: &mut HashMap<UnknownId<'dae>, ScalarRowSource<'dae>>,
    span: Span,
) -> Result<(), LowerError> {
    let Some(unknown) = matching.get(&row).copied() else {
        return Ok(());
    };
    if rows.insert(unknown, source).is_some() {
        return Err(LowerError::contract(
            "two continuous rows matched the same unknown",
            span,
        ));
    }
    Ok(())
}
