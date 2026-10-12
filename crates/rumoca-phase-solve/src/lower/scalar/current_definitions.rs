//! Current algebraic reads project the structural engine's exact definition.

use super::*;
use std::collections::BTreeMap;

#[cfg(test)]
mod tests;

type DefinitionFacts =
    BTreeMap<u32, Result<(bool, bool), rumoca_phase_structural::SameTickOrderError>>;

#[derive(Clone)]
pub(in crate::lower) struct DefinitionOwner<'dae> {
    definitions: Rc<rumoca_phase_structural::SameTickDefinitions<'dae>>,
    targets: Rc<BTreeSet<u32>>,
    facts: Rc<RefCell<DefinitionFacts>>,
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
    ) -> Result<(bool, bool), rumoca_phase_structural::SameTickOrderError> {
        if let Some(facts) = self.facts.borrow().get(&variable) {
            return facts.clone();
        }
        let facts = self
            .definitions
            .read_closure(view, &[definition], &[])
            .map(|reads| {
                (
                    !reads.is_disjoint(&self.targets),
                    !reads.contains(&variable),
                )
            });
        self.facts.borrow_mut().insert(variable, facts.clone());
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

    pub(in crate::lower) fn current_definition_program(
        mut self,
        owner: DefinitionOwner<'dae>,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        self = self.with_current_definitions(owner);
        self.project_current_definitions = true;
        self.program(expression, scalar)
    }

    pub(super) fn inherit_current_definitions(&mut self, parent: &Self) {
        self.same_tick_definitions = parent.same_tick_definitions.clone();
        self.project_current_definitions = parent.project_current_definitions;
        self.active_current_definitions = parent.active_current_definitions.clone();
    }

    pub(super) fn current_definition(
        &self,
        coordinate: dae::CoordinateView<'dae>,
    ) -> Result<Option<(u32, dae::ExprId<'dae>)>, LowerError> {
        if !self.project_current_definitions || self.sampled_source {
            return Ok(None);
        }
        let dae::CoordinateView::Algebraic(variable) = coordinate else {
            return Ok(None);
        };
        let variable = dae::VariableId::from(variable).index();
        let Some(owner) = self.same_tick_definitions.as_ref() else {
            return Ok(None);
        };
        let Some(definition) = owner.definitions.definition(variable) else {
            return Ok(None);
        };
        if !owner.facts(self.view, variable, definition)?.0 {
            return Ok(None);
        }
        Ok(Some((variable, definition)))
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
        self.same_tick_definitions.as_ref().is_some_and(|owner| {
            owner
                .facts(self.view, variable, definition)
                .is_ok_and(|facts| facts.1)
        })
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
        let Some((variable, definition)) = self.current_definition(coordinate)? else {
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
