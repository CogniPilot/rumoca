//! Total conditional statements lowered as selections in the enclosing scope.
//!
//! A conditional statement whose arms only assign values that cannot fail has
//! no observable effect besides those values, so evaluating every arm in the
//! enclosing scope and selecting is the same program as entering the selected
//! arm (SPEC_0040 SOLVE-C61). Two kinds of target qualify: a single-cell
//! target, whose arm values are selected, and an array target that every arm
//! updates at the same element, whose element values are selected and written
//! once, so no arm copies the array.

use super::super::total_conditionals;
use super::*;
use crate::lower::dae_faults::FaultMemo;

/// The element update every arm applies to one array target.
struct SharedUpdate<'dae> {
    target: dae::FunctionValueId<'dae>,
    base: dae::ExprId<'dae>,
    subscripts: dae::SubscriptsView<'dae>,
    value_type: dae::ValueTypeId<'dae>,
    array_type: dae::ValueTypeId<'dae>,
    values: Vec<dae::ExprId<'dae>>,
}

/// One arm's `base[subscripts] := value`.
struct ElementWrite<'dae> {
    base: dae::ExprId<'dae>,
    value: dae::ExprId<'dae>,
    subscripts: dae::SubscriptsView<'dae>,
    value_type: dae::ValueTypeId<'dae>,
    array_type: dae::ValueTypeId<'dae>,
}

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
    /// Whether every arm only assigns values that cannot fail, with no
    /// assertion or assigned-state interface.
    pub(super) fn is_total_selection(&mut self, plan: &ScopedConditionalPlan<'dae>) -> bool {
        if !plan.source.assertions.is_empty() || !plan.source.states.is_empty() {
            return false;
        }
        let shared = self.shared_updates(plan);
        self.outputs_are_selectable(plan, &shared)
            && self.guards_and_updates_are_total(plan, &shared)
            && arms(plan).all(|source| self.arm_is_total(source, &shared))
    }

    fn shared_updates(&self, plan: &ScopedConditionalPlan<'dae>) -> Vec<SharedUpdate<'dae>> {
        plan.outputs
            .iter()
            .filter_map(|output| self.shared_update(plan, output.target()))
            .collect()
    }

    /// Both arms are evaluated, so a target is selected only when it is a
    /// single cell or a shared element write; a large array is better left to
    /// the arm that is entered.
    fn outputs_are_selectable(
        &self,
        plan: &ScopedConditionalPlan<'dae>,
        shared: &[SharedUpdate<'dae>],
    ) -> bool {
        plan.outputs.iter().all(|output| {
            shared.iter().any(|update| update.target == output.target())
                || function_value_type(self.view, output.target(), plan.provenance)
                    .ok()
                    .and_then(|value_type| self.view.value_type(value_type))
                    .is_some_and(|value_type| {
                        !value_type.is_record() && value_type.dimensions().is_empty()
                    })
        })
    }

    fn guards_and_updates_are_total(
        &mut self,
        plan: &ScopedConditionalPlan<'dae>,
        shared: &[SharedUpdate<'dae>],
    ) -> bool {
        let view = self.view;
        let totality = &mut self.totality;
        plan.conditions
            .iter()
            .skip(1)
            .all(|condition| total_conditionals::is_total(view, totality, *condition))
            && shared.iter().all(|update| {
                std::iter::once(&update.base)
                    .chain(&update.values)
                    .all(|root| total_conditionals::is_total(view, totality, *root))
            })
    }

    fn arm_is_total(
        &mut self,
        source: &BranchStatements<'dae>,
        shared: &[SharedUpdate<'dae>],
    ) -> bool {
        let view = self.view;
        let totality = &mut self.totality;
        source.assertions.is_empty()
            && source.branch.statements().into_iter().all(|statement| {
                statement_definitions(statement).is_some_and(|definitions| {
                    definitions
                        .into_iter()
                        .all(|definition| definition_is_total(view, totality, shared, definition))
                })
            })
    }

    /// The common element update of `target`, when every arm defines it as
    /// `base[subscripts] := value` over the same base and the same scalar
    /// subscripts, and the updated cell is a single scalar.
    fn shared_update(
        &self,
        plan: &ScopedConditionalPlan<'dae>,
        target: dae::FunctionValueId<'dae>,
    ) -> Option<SharedUpdate<'dae>> {
        let mut shared: Option<SharedUpdate<'dae>> = None;
        for source in arms(plan) {
            let write = self.element_write(source, target, plan.provenance)?;
            match shared.as_mut() {
                None => shared = Some(SharedUpdate::first(target, write)?),
                Some(first) => self
                    .same_write(first, &write)
                    .then(|| first.values.push(write.value))?,
            }
        }
        shared
    }

    fn element_write(
        &self,
        source: &BranchStatements<'dae>,
        target: dae::FunctionValueId<'dae>,
        provenance: rumoca_core::Span,
    ) -> Option<ElementWrite<'dae>> {
        let definition = self
            .scoped_branch_definition(source, target, provenance)
            .ok()?;
        let rhs = self.view.expression(definition.rhs())?;
        let dae::ExpressionOperation::ArrayUpdate {
            base,
            value,
            subscripts,
        } = rhs.operation()
        else {
            return None;
        };
        let value_type = self.view.expression(value)?.value_type_id();
        let cell = self.view.value_type(value_type)?;
        (cell.dimensions().is_empty() && !cell.is_record()).then(|| ElementWrite {
            base,
            value,
            subscripts,
            value_type,
            array_type: rhs.value_type_id(),
        })
    }

    fn same_write(&self, first: &SharedUpdate<'dae>, write: &ElementWrite<'dae>) -> bool {
        self.same_base(first.base, write.base)
            && first.value_type == write.value_type
            && first.subscripts.len() == write.subscripts.len()
            && first
                .subscripts
                .iter()
                .zip(write.subscripts.iter())
                .all(|(a, b)| self.same_subscript(a, b))
    }

    fn same_subscript(
        &self,
        first: dae::SubscriptView<'dae>,
        second: dae::SubscriptView<'dae>,
    ) -> bool {
        match (first, second) {
            (
                dae::SubscriptView::Index { expression: a, .. },
                dae::SubscriptView::Index { expression: b, .. },
            ) => self.same_expression(a, b),
            _ => false,
        }
    }

    /// Whether two base expressions read the same value: one expression, or
    /// reads of the same earlier definition of the same function value.
    fn same_base(&self, first: dae::ExprId<'dae>, second: dae::ExprId<'dae>) -> bool {
        if first == second {
            return true;
        }
        let operation = |id| self.view.expression(id).map(|node| node.operation());
        matches!(
            (operation(first), operation(second)),
            (
                Some(dae::ExpressionOperation::FunctionValue { value: a, definition: da }),
                Some(dae::ExpressionOperation::FunctionValue { value: b, definition: db }),
            ) if a == b && da.id() == db.id()
        )
    }

    /// Whether two subscripts compute the same value: one expression, or the
    /// same binder, literal, or operator over the same operands.
    fn same_expression(&self, first: dae::ExprId<'dae>, second: dae::ExprId<'dae>) -> bool {
        if first == second {
            return true;
        }
        let operation = |id| self.view.expression(id).map(|node| node.operation());
        match (operation(first), operation(second)) {
            (
                Some(dae::ExpressionOperation::Literal(a)),
                Some(dae::ExpressionOperation::Literal(b)),
            ) => a == b,
            (
                Some(dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(a))),
                Some(dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(b))),
            ) => a.domain() == b.domain() && a.ordinal() == b.ordinal(),
            (
                Some(dae::ExpressionOperation::Binary {
                    operator: oa,
                    lhs: la,
                    rhs: ra,
                }),
                Some(dae::ExpressionOperation::Binary {
                    operator: ob,
                    lhs: lb,
                    rhs: rb,
                }),
            ) => oa == ob && self.same_expression(la, lb) && self.same_expression(ra, rb),
            _ => false,
        }
    }

    /// Lower a total conditional statement as selections in this scope: every
    /// arm is evaluated here and each output selects its arm's definition.
    pub(super) fn scoped_selection(
        &mut self,
        plan: &ScopedConditionalPlan<'dae>,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        let provenance = plan.provenance;
        let shared = self.shared_updates(plan);
        for arm in arms(plan) {
            self.lower_arm_definitions(arm, &shared, provenance)?;
        }
        let mut conditions = Vec::with_capacity(plan.conditions.len());
        for condition in &plan.conditions {
            conditions.push(self.expression(*condition)?.only_register(provenance)?);
        }
        for output in &plan.outputs {
            let selected = match shared
                .iter()
                .find(|update| update.target == output.target())
            {
                Some(update) => self.selected_update(update, &conditions, provenance)?,
                None => self.selected_definition(plan, output, &conditions)?,
            };
            self.function_values.insert(output.id(), selected);
        }
        Ok(())
    }

    /// Lower the definitions an arm issues, except the shared element writes,
    /// which are issued once after the selection.
    fn lower_arm_definitions(
        &mut self,
        arm: &BranchStatements<'dae>,
        shared: &[SharedUpdate<'dae>],
        provenance: rumoca_core::Span,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        for statement in arm.branch.statements() {
            let definitions = statement_definitions(statement)
                .ok_or(solve::SolveProgramConstructionError::InvalidRegion { provenance })?;
            let issued = definitions
                .into_iter()
                .filter(|definition| !is_shared(shared, definition.target()));
            for definition in issued {
                self.function_definition_value(definition)?;
            }
        }
        Ok(())
    }

    fn selected_definition(
        &mut self,
        plan: &ScopedConditionalPlan<'dae>,
        output: &dae::FunctionDefinitionView<'dae>,
        conditions: &[solve::ProgramRegister<'program>],
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        let provenance = plan.provenance;
        let value_type = function_value_type(self.view, output.target(), provenance)?;
        let mut values = Vec::new();
        for arm in arms(plan) {
            let definition = self.scoped_branch_definition(arm, output.target(), provenance)?;
            values.push(self.function_definition_value(definition)?);
        }
        self.select_arms(value_type, conditions, values, provenance)
    }

    /// The array after one element write of the value the taken arm computes.
    fn selected_update(
        &mut self,
        update: &SharedUpdate<'dae>,
        conditions: &[solve::ProgramRegister<'program>],
        provenance: rumoca_core::Span,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        let mut values = Vec::with_capacity(update.values.len());
        for value in &update.values {
            values.push(self.expression(*value)?);
        }
        let value = self.select_arms(update.value_type, conditions, values, provenance)?;
        self.array_update_with(
            update.array_type,
            update.base,
            value,
            update.subscripts,
            provenance,
        )
    }

    /// Chain `conditions` over the arm `values`, the last arm being the
    /// fallback.
    fn select_arms(
        &mut self,
        value_type: dae::ValueTypeId<'dae>,
        conditions: &[solve::ProgramRegister<'program>],
        mut values: Vec<LoweredValue<'program, 'dae>>,
        provenance: rumoca_core::Span,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        let mut selected = values
            .pop()
            .ok_or(solve::SolveProgramConstructionError::InvalidRegion { provenance })?;
        for (condition, value) in conditions.iter().zip(values).rev() {
            let leaves = value
                .leaves
                .iter()
                .zip(&selected.leaves)
                .map(|(then, otherwise)| {
                    self.builder
                        .select(*condition, *then, *otherwise, provenance)
                })
                .collect::<Result<Vec<_>, _>>()?;
            selected = LoweredValue { value_type, leaves };
        }
        Ok(selected)
    }
}

impl<'dae> SharedUpdate<'dae> {
    /// A subscripted write whose subscripts are all scalar indices.
    fn first(target: dae::FunctionValueId<'dae>, write: ElementWrite<'dae>) -> Option<Self> {
        let indexed = !write.subscripts.is_empty()
            && write
                .subscripts
                .iter()
                .all(|subscript| matches!(subscript, dae::SubscriptView::Index { .. }));
        indexed.then(|| Self {
            target,
            base: write.base,
            subscripts: write.subscripts,
            value_type: write.value_type,
            array_type: write.array_type,
            values: vec![write.value],
        })
    }
}

fn is_shared<'dae>(shared: &[SharedUpdate<'dae>], target: dae::FunctionValueId<'dae>) -> bool {
    shared.iter().any(|update| update.target == target)
}

fn definition_is_total<'dae>(
    view: dae::DaeView<'dae>,
    totality: &mut FaultMemo<'dae>,
    shared: &[SharedUpdate<'dae>],
    definition: dae::FunctionDefinitionView<'dae>,
) -> bool {
    is_shared(shared, definition.target())
        || total_conditionals::is_total(view, totality, definition.rhs())
}

/// The definitions an assignment statement issues; any other statement is not
/// part of a selection.
fn statement_definitions<'dae>(
    statement: dae::FunctionStatementView<'dae>,
) -> Option<Vec<dae::FunctionDefinitionView<'dae>>> {
    match statement {
        dae::FunctionStatementView::Assignment { definition } => Some(vec![definition]),
        dae::FunctionStatementView::AssignmentGroup { definitions, .. } => {
            Some(definitions.iter().collect())
        }
        _ => None,
    }
}

fn arms<'a, 'dae>(
    plan: &'a ScopedConditionalPlan<'dae>,
) -> impl Iterator<Item = &'a BranchStatements<'dae>> {
    plan.source
        .branches
        .iter()
        .chain(std::iter::once(&plan.source.fallback))
}
