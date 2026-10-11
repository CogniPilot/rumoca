//! Current algebraic reads project the structural engine's exact definition.

use super::*;

#[derive(Clone)]
pub(super) struct DefinitionOwner<'dae> {
    definitions: Rc<rumoca_phase_structural::SameTickDefinitions<'dae>>,
    targets: Rc<BTreeSet<u32>>,
}

impl PartialEq for DefinitionOwner<'_> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.definitions, &other.definitions)
            && Rc::ptr_eq(&self.targets, &other.targets)
    }
}
impl Eq for DefinitionOwner<'_> {}

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    pub(in crate::lower) fn with_same_tick_definitions(
        mut self,
        definitions: Rc<rumoca_phase_structural::SameTickDefinitions<'dae>>,
        targets: Rc<BTreeSet<u32>>,
    ) -> Self {
        self.same_tick_definitions = Some(DefinitionOwner {
            definitions,
            targets,
        });
        self
    }

    pub(super) fn inherit_current_definitions(&mut self, parent: &Self) {
        self.same_tick_definitions = parent.same_tick_definitions.clone();
        self.project_current_definitions = parent.project_current_definitions;
        self.active_current_definitions = parent.active_current_definitions.clone();
    }

    pub(super) fn current_definition(
        &self,
        coordinate: dae::CoordinateView<'dae>,
    ) -> Option<(u32, dae::ExprId<'dae>)> {
        if !self.project_current_definitions || self.sampled_source {
            return None;
        }
        let dae::CoordinateView::Algebraic(variable) = coordinate else {
            return None;
        };
        let variable = dae::VariableId::from(variable).index();
        let definition = self
            .same_tick_definitions
            .as_ref()?
            .0
            .definition(variable)?;
        Some((variable, definition))
    }

    fn begin_current_definition(&mut self, variable: u32, span: Span) -> Result<(), LowerError> {
        if self.active_current_definitions.contains(&variable) {
            return Err(LowerError::non_computable(
                "same-tick algebraic definitions require a coupled owner",
                span,
            ));
        }
        self.active_current_definitions.push(variable);
        Ok(())
    }

    pub(super) fn current_definition_value(
        &mut self,
        (variable, definition): (u32, dae::ExprId<'dae>),
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        self.begin_current_definition(variable, span)?;
        let value = self.expression(definition, scalar);
        self.active_current_definitions.pop();
        value
    }

    pub(super) fn pack_current_definition(
        &mut self,
        coordinate: dae::CoordinateView<'dae>,
        span: Span,
    ) -> Result<Option<solve::Reg>, LowerError> {
        let Some((variable, definition)) = self.current_definition(coordinate) else {
            return Ok(None);
        };
        self.begin_current_definition(variable, span)?;
        let value = self.pack_expression(definition);
        self.active_current_definitions.pop();
        value.map(Some)
    }

    pub(super) fn with_entry_definition_reads(
        &mut self,
        owner: ActivationCondition<'dae>,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        self.project_current_definitions = false;
        self.enter_context(ScalarContextFrame::EntryDefinitions {
            parent: self.context_id,
        });
        let value = self.materialize_activation_condition(owner, span);
        self.leave_context();
        self.project_current_definitions = true;
        value
    }
}
