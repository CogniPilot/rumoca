use rumoca_ir_dae as dae;

use super::equalities::is_time_invariant;
use super::expressions::ExpressionRebuilder;

impl<'source, 'borrow, 'storage, 'target> ExpressionRebuilder<'source, 'borrow, 'storage, 'target> {
    /// Singleton equality classes equate scalar payloads. Construct the
    /// declared coordinate's shape when substituting its anchor, so a later
    /// projection still sees the aggregate its source expression promised.
    pub(super) fn shape_equality_anchor(
        &mut self,
        algebraic: dae::AlgebraicId<'source>,
        mut value: dae::ExprId<'target>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let expected = self
            .source
            .variable(algebraic.into())
            .unwrap()
            .value_type()
            .clone();
        let actual = self.target.value_type(value, provenance)?;
        if expected.dimensions() == actual.dimensions() {
            return Ok(value);
        }
        if expected.scalar_count() != Some(1)
            || actual.scalar_count() != Some(1)
            || expected.scalar_type() != dae::ScalarType::Real
            || !matches!(
                actual.scalar_type(),
                dae::ScalarType::Real | dae::ScalarType::Integer
            )
        {
            return Err(dae::DaeConstructionError::IncompleteDefinition {
                kind: "shape-compatible equality anchor",
                index: algebraic.index(),
                span: provenance.span(),
            });
        }
        if !actual.is_scalar() {
            let one = self
                .target
                .at(provenance)
                .literal(dae::DaeLiteral::Integer(1))?;
            let subscripts = actual
                .dimensions()
                .iter()
                .map(|_| dae::Subscript::Index {
                    expression: one,
                    provenance,
                })
                .collect::<Vec<_>>();
            value = self.target.at(provenance).index(value, subscripts)?;
        }
        for _ in expected.dimensions() {
            value = self.target.at(provenance).array([value])?;
        }
        Ok(value)
    }
}

/// Temporal differentiation commutes with a checked projection only while its
/// selected coordinates are invariant. Runtime/discrete indices are refused.
pub(super) fn has_invariant_subscripts<'dae>(
    view: dae::DaeView<'dae>,
    subscripts: dae::SubscriptsView<'dae>,
) -> bool {
    subscripts.iter().all(|subscript| match subscript {
        dae::SubscriptView::Whole { .. } => true,
        dae::SubscriptView::Index { expression, .. }
        | dae::SubscriptView::Slice { expression, .. } => is_time_invariant(view, expression),
    })
}
