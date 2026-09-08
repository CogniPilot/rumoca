//! One authority for GALEC causal-coordinate substitution.
//!
//! Expression emission asks for the exact definition selected by its concrete
//! projection indices. Shared-call scheduling may ask only for a definition it
//! can identify exactly without an inherited projection. Whole-`DoStep` call
//! ownership traces the real expression projection instead of approximating
//! that missing context.

use super::*;

#[derive(Clone, Copy)]
pub(super) struct CausalSubstitutionPlan<'a, 'dae> {
    view: dae::DaeView<'dae>,
    definitions: &'a rumoca_phase_structural::CausalDefinitions<'dae>,
    by_id: &'a HashMap<u32, ClassifiedVariable<'dae>>,
}

impl<'a, 'dae> CausalSubstitutionPlan<'a, 'dae> {
    pub(super) const fn new(
        view: dae::DaeView<'dae>,
        definitions: &'a rumoca_phase_structural::CausalDefinitions<'dae>,
        by_id: &'a HashMap<u32, ClassifiedVariable<'dae>>,
    ) -> Self {
        Self {
            view,
            definitions,
            by_id,
        }
    }

    /// The exact causal definition expression the emitter selects.
    pub(super) fn exact_definition(
        self,
        coordinate: dae::CoordinateView<'dae>,
        indices: &[gast::Expression],
        inline_causal_locals: bool,
    ) -> Option<dae::ExprId<'dae>> {
        let variable = self.substituted_algebraic(coordinate, inline_causal_locals)?;
        if let Some(definition) = self.definitions.definition(variable) {
            return Some(definition);
        }
        let variable = dae::VariableId::from(variable);
        let dimensions = self.view.variable(variable)?.value_type().dimensions();
        let scalar = literal_scalar_index(dimensions, indices)?;
        self.definitions
            .scalar_definition_for_variable(variable, scalar)
    }

    fn substituted_algebraic(
        self,
        coordinate: dae::CoordinateView<'dae>,
        inline_causal_locals: bool,
    ) -> Option<dae::AlgebraicId<'dae>> {
        let dae::CoordinateView::Algebraic(variable) = coordinate else {
            return None;
        };
        let materialized_local = self
            .by_id
            .get(&dae::VariableId::from(variable).index())
            .is_some_and(|variable| variable.class == VariableClass::Local)
            && !inline_causal_locals;
        (!materialized_local).then_some(variable)
    }
}
