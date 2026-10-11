//! Current algebraic reads project the structural engine's exact definition.

use super::*;
use std::collections::BTreeMap;

#[cfg(test)]
mod tests;

#[derive(Clone)]
pub(in crate::lower) struct DefinitionOwner<'dae> {
    definitions: Rc<rumoca_phase_structural::SameTickDefinitions<'dae>>,
    targets: Rc<BTreeSet<u32>>,
    facts: Rc<RefCell<BTreeMap<u32, (bool, bool)>>>,
}

impl PartialEq for DefinitionOwner<'_> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.definitions, &other.definitions)
            && Rc::ptr_eq(&self.targets, &other.targets)
    }
}
impl Eq for DefinitionOwner<'_> {}

impl<'dae> DefinitionOwner<'dae> {
    pub(in crate::lower) fn new(
        definitions: Rc<rumoca_phase_structural::SameTickDefinitions<'dae>>,
        targets: Rc<BTreeSet<u32>>,
    ) -> Self {
        Self {
            definitions,
            targets,
            facts: Rc::new(RefCell::new(BTreeMap::new())),
        }
    }

    fn facts(
        &self,
        view: dae::DaeView<'dae>,
        variable: u32,
        definition: dae::ExprId<'dae>,
    ) -> (bool, bool) {
        if let Some(facts) = self.facts.borrow().get(&variable) {
            return *facts;
        }
        let reads = self.definitions.read_closure(view, &[definition], &[]);
        let facts = (
            !reads.is_disjoint(&self.targets),
            !reads.contains(&variable),
        );
        self.facts.borrow_mut().insert(variable, facts);
        facts
    }
}

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    pub(in crate::lower) fn with_current_definitions(
        mut self,
        owner: DefinitionOwner<'dae>,
    ) -> Self {
        self.same_tick_definitions = Some(owner);
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
        let owner = self.same_tick_definitions.as_ref()?;
        let definition = owner.definitions.definition(variable)?;
        if !owner.facts(self.view, variable, definition).0 {
            return None;
        }
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

    pub(super) fn current_definition_is_acyclic(
        &self,
        variable: u32,
        definition: dae::ExprId<'dae>,
    ) -> bool {
        self.same_tick_definitions
            .as_ref()
            .is_some_and(|owner| owner.facts(self.view, variable, definition).1)
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
        let previous_mode = std::mem::replace(&mut self.project_current_definitions, false);
        self.enter_context(ScalarContextFrame::EntryDefinitions {
            parent: self.context_id,
        });
        let value = self.materialize_activation_condition(owner, span);
        self.leave_context();
        self.project_current_definitions = previous_mode;
        value
    }
}
