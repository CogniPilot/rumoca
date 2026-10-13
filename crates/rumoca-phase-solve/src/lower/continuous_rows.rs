//! Matched source row views shared by derivative substitution and initialization.

use super::{
    LoweredLayout, MatchedProjection, MatchingRows, ScalarRowSource, continuous_scalar_row_count,
    first_model_span, owner_provenance, scalar_count,
};
use crate::LowerError;
use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_phase_structural::UnknownId;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};

/// Source equations named by the structural matching. Tensor and structured
/// rows retain their exact component and enclosing domain point.
#[derive(Default)]
pub(super) struct ContinuousRowIndex<'dae> {
    rows: HashMap<UnknownId<'dae>, ScalarRowSource<'dae>>,
    compact_families: Vec<CompactFamilyDefinition<'dae>>,
}

struct CompactFamilyDefinition<'dae> {
    variable: dae::AlgebraicId<'dae>,
    family: dae::StructuredFamilyView<'dae>,
    domain: dae::DomainView<'dae>,
}

impl<'dae> ContinuousRowIndex<'dae> {
    pub(super) fn derivative_sources(
        &self,
    ) -> impl Iterator<Item = rumoca_phase_structural::MatchedDerivativeSource<'dae>> + '_ {
        self.rows.iter().filter_map(|(unknown, row)| match unknown {
            UnknownId::Derivative { state, scalar } => {
                Some(rumoca_phase_structural::MatchedDerivativeSource {
                    state: *state,
                    state_scalar: *scalar as usize,
                    expression: row.expression,
                    expression_scalar: row.scalar,
                    domain_point: row.domain_point.clone(),
                })
            }
            UnknownId::Algebraic { .. } | UnknownId::Solver(_) | UnknownId::Unmatched { .. } => {
                None
            }
        })
    }

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
    ) -> Option<Cow<'_, ScalarRowSource<'dae>>> {
        let scalar_index = u32::try_from(scalar).ok()?;
        if let Some(source) = self.rows.get(&UnknownId::Algebraic {
            variable,
            scalar: scalar_index,
        }) {
            return Some(Cow::Borrowed(source));
        }
        let recipe = self
            .compact_families
            .iter()
            .find(|recipe| recipe.variable == variable)?;
        let tuple = recipe
            .domain
            .structured()
            .index_tuple_at(scalar)
            .expect("retained checked domain remains valid")?;
        let component = recipe
            .family
            .scalar_view()
            .body_scalar(scalar, recipe.domain.extents())?;
        Some(Cow::Owned(ScalarRowSource {
            expression: recipe.family.bodies().get(0)?,
            scalar: component,
            domain_point: Some((recipe.family.domain(), tuple)),
        }))
    }

    #[cfg(test)]
    pub(super) fn storage_counts(&self) -> (usize, usize) {
        (self.rows.len(), self.compact_families.len())
    }
}

pub(super) fn index_continuous_rows<'dae>(
    view: dae::DaeView<'dae>,
    matching: &MatchingRows<'dae>,
    stages: &[MatchedProjection<'dae>],
    layout: &LoweredLayout<'dae>,
) -> Result<ContinuousRowIndex<'dae>, LowerError> {
    let mut rows = HashMap::new();
    let mut compact_families = Vec::new();
    let mut compact_variables = HashSet::new();
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
            dae::ContinuousOwnerView::Structured { id, family } => {
                if let Some(recipe) = compact_family_definition(view, layout, stages, id, family)? {
                    row = index_compact_family(
                        recipe,
                        family,
                        row,
                        &mut compact_variables,
                        &mut compact_families,
                        span,
                    )?;
                } else {
                    row = index_family_rows(view, matching, row, family, &mut rows, span)?;
                }
            }
        }
    }
    if row != continuous_scalar_row_count(view)? {
        return Err(LowerError::contract(
            "continuous row enumeration disagrees with the checked row count",
            first_model_span(view),
        ));
    }
    for (unknown, source) in &rows {
        if matches!(unknown, UnknownId::Algebraic { variable, .. } if compact_variables.contains(variable))
        {
            let span = view
                .expression(source.expression)
                .expect("indexed source expression resolves")
                .provenance()
                .span();
            return Err(LowerError::contract(
                "two continuous rows matched the same unknown",
                span,
            ));
        }
    }
    Ok(ContinuousRowIndex {
        rows,
        compact_families,
    })
}

fn index_compact_family<'dae>(
    recipe: CompactFamilyDefinition<'dae>,
    family: dae::StructuredFamilyView<'dae>,
    row: usize,
    variables: &mut HashSet<dae::AlgebraicId<'dae>>,
    families: &mut Vec<CompactFamilyDefinition<'dae>>,
    span: Span,
) -> Result<usize, LowerError> {
    if !variables.insert(recipe.variable) {
        return Err(LowerError::contract(
            "two continuous rows matched the same unknown",
            span,
        ));
    }
    families.push(recipe);
    row.checked_add(
        usize::try_from(family.scalar_rows())
            .map_err(|_| LowerError::contract("family row count exceeds host range", span))?,
    )
    .ok_or_else(|| LowerError::contract("continuous row ordinal overflow", span))
}

fn compact_family_definition<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    stages: &[MatchedProjection<'dae>],
    source: dae::ContinuousFamilyId<'dae>,
    family: dae::StructuredFamilyView<'dae>,
) -> Result<Option<CompactFamilyDefinition<'dae>>, LowerError> {
    for stage in stages {
        let MatchedProjection::Family(matched) = stage else {
            continue;
        };
        let Some(context) = matched.matching().context() else {
            continue;
        };
        if context.source() != source {
            continue;
        }
        let Some(
            [
                UnknownId::Algebraic {
                    variable,
                    scalar: 0,
                },
            ],
        ) = context.declaration_bases()
        else {
            continue;
        };
        let rows = matched.matching().row_range();
        if matched
            .declaration_row_run(view, layout, source, *variable, &rows)?
            .is_some()
        {
            return Ok(Some(CompactFamilyDefinition {
                variable: *variable,
                family,
                domain: view
                    .domain(context.domain())
                    .expect("checked matching source domain resolves"),
            }));
        }
    }
    Ok(None)
}

fn index_family_rows<'dae>(
    view: dae::DaeView<'dae>,
    matching: &MatchingRows<'dae>,
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
    matching: &MatchingRows<'dae>,
    row: usize,
    source: ScalarRowSource<'dae>,
    rows: &mut HashMap<UnknownId<'dae>, ScalarRowSource<'dae>>,
    span: Span,
) -> Result<(), LowerError> {
    let Some(unknown) = matching.get(&row) else {
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
