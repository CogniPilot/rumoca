mod assigned;
mod selection;

use assigned::AssignedRegionInputs;
pub(super) use assigned::bind_states;
use std::collections::BTreeSet;

use super::assertions::{BranchStatements, ConditionalStatements, statement_roots};
use super::*;

#[derive(Clone)]
struct ScopedConditionalPlan<'dae> {
    owner: dae::FunctionConditionalId<'dae>,
    conditions: Vec<dae::ExprId<'dae>>,
    source: ConditionalStatements<'dae>,
    outputs: Vec<dae::FunctionDefinitionView<'dae>>,
    pending: Vec<usize>,
    provenance: rumoca_core::Span,
}

struct ScopedResults<'program> {
    values: Vec<solve::ProgramRegister<'program>>,
    states: Vec<solve::ProgramAssignedState<'program>>,
}

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
    pub(super) fn scoped_conditional(
        &mut self,
        conditional: dae::FunctionConditionalBodyView<'dae>,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        let provenance = conditional.declaration().span();
        let source = self
            .conditional_statements
            .get(&conditional.id())
            .cloned()
            .ok_or(solve::SolveProgramConstructionError::InvalidRegion { provenance })?;
        if self.next_direct_assertion != source.assertions.start {
            return Err(solve::SolveProgramConstructionError::InvalidCallOutput { provenance });
        }
        let mut plan = ScopedConditionalPlan {
            owner: conditional.id(),
            conditions: conditional.conditions().collect(),
            outputs: conditional.outputs().collect(),
            source,
            pending: Vec::new(),
            provenance,
        };
        if self.guard_is_implied(&plan) {
            let results = self.scoped_branch_values(&plan, &plan.source.branches[0])?;
            return self.publish_scoped(&plan, results);
        }
        if self.is_total_selection(&plan) {
            return self.scoped_selection(&plan);
        }
        // The first guard belongs to this source point; later guards stay in
        // the corresponding false regions.
        if let Some(condition) = plan.conditions.first() {
            self.expression(*condition)?;
        }
        let requirements = self.scoped_branch_requirements(&plan, 0)?;
        self.capture_environment_for_branches(&requirements)?;
        let mut pending = self
            .unissued_assertion_slots(
                requirements
                    .iter()
                    .flat_map(|(roots, _)| roots.iter().copied()),
            )
            .into_iter()
            .collect::<BTreeSet<_>>();
        for index in plan.source.assertions.clone() {
            let declaration = &self.direct_assertions[index];
            pending.insert(declaration.predicate_output - self.assertion_output_base);
            pending.extend(
                declaration
                    .message_values
                    .iter()
                    .map(|(_, output)| output - self.assertion_output_base),
            );
        }
        plan.pending = pending.into_iter().collect();
        let results = self.scoped_conditional_chain(&plan, 0)?;
        self.publish_scoped(&plan, results)
    }

    /// Whether the conditional is one guarded branch with nothing else whose
    /// guard consists of tests the early-exit predicate of the enclosing fold
    /// names: that branch runs on every pass.
    fn guard_is_implied(&self, plan: &ScopedConditionalPlan<'dae>) -> bool {
        let [guard] = plan.conditions.as_slice() else {
            return false;
        };
        plan.source.branches.len() == 1
            && plan.source.fallback.branch.statements().next().is_none()
            && plan.source.assertions.is_empty()
            && plan.source.branches[0].assertions.is_empty()
            && !self.implied_guards.is_empty()
            && super::implied_guards::implied(self.view, &self.implied_guards, *guard)
    }

    /// Record the values and assigned states one lowered conditional defines.
    fn publish_scoped(
        &mut self,
        plan: &ScopedConditionalPlan<'dae>,
        results: ScopedResults<'program>,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        let provenance = plan.provenance;
        let destinations = results.values;
        let mut state_offset = 0;
        for value in &plan.source.states {
            let value_type = function_value_type(self.view, *value, provenance)?;
            let count = lower_value_type_leaves(self.view, value_type, arithmetic_profile())?.len();
            let end = state_offset + count;
            let states = results
                .states
                .get(state_offset..end)
                .ok_or(solve::SolveProgramConstructionError::InvalidProjection { provenance })?
                .to_vec();
            self.assigned_values.insert((plan.owner, *value), states);
            state_offset = end;
        }
        if state_offset != results.states.len() {
            return Err(solve::SolveProgramConstructionError::InvalidProjection { provenance });
        }
        let mut offset = 0usize;
        for definition in &plan.outputs {
            let value_type = function_value_type(self.view, definition.target(), provenance)?;
            let count = lower_value_type_leaves(self.view, value_type, arithmetic_profile())?.len();
            let end = offset
                .checked_add(count)
                .ok_or(solve::SolveProgramConstructionError::InvalidCallOutput { provenance })?;
            let leaves = destinations
                .get(offset..end)
                .ok_or(solve::SolveProgramConstructionError::InvalidCallOutput { provenance })?
                .to_vec();
            self.function_values
                .insert(definition.id(), LoweredValue { value_type, leaves });
            offset = end;
        }
        if destinations.len() != offset + plan.pending.len() {
            return Err(solve::SolveProgramConstructionError::InvalidCallOutput { provenance });
        }
        for slot in plan.pending.iter().copied() {
            let value = self.inactive_slot(slot, provenance)?;
            if self.predicate_values[slot].replace(value).is_some() {
                return Err(solve::SolveProgramConstructionError::InvalidCallOutput { provenance });
            }
        }
        self.next_direct_assertion = plan.source.assertions.end;
        Ok(())
    }

    fn scoped_branch_requirements(
        &self,
        plan: &ScopedConditionalPlan<'dae>,
        first: usize,
    ) -> Result<
        Vec<(Vec<dae::ExprId<'dae>>, dae::FunctionScopeView<'dae>)>,
        solve::SolveProgramConstructionError,
    > {
        plan.source.branches[first..]
            .iter()
            .chain(std::iter::once(&plan.source.fallback))
            .map(|source| {
                let mut roots = statement_roots(source.branch.statements(), &|owner, value| {
                    self.assigned_values.contains_key(&(owner, value))
                });
                // This guard was evaluated in the parent. Only later guards
                // belong to the false continuation; arm reads remain roots.
                roots.extend(plan.conditions.iter().skip(first + 1).copied());
                for index in source.assertions.clone() {
                    roots.extend(
                        self.direct_assertions[index]
                            .message_values
                            .iter()
                            .map(|(value, _)| *value),
                    );
                }
                for definition in &plan.outputs {
                    roots.push(
                        self.scoped_branch_definition(
                            source,
                            definition.target(),
                            plan.provenance,
                        )?
                        .rhs(),
                    );
                }
                Ok((roots, source.branch.body_scope()))
            })
            .collect()
    }

    fn scoped_branch_definition(
        &self,
        source: &BranchStatements<'dae>,
        target: dae::FunctionValueId<'dae>,
        provenance: rumoca_core::Span,
    ) -> Result<dae::FunctionDefinitionView<'dae>, solve::SolveProgramConstructionError> {
        let Some(dae::FunctionAssignedState::Assigned(definition)) = source.branch.outgoing(target)
        else {
            return Err(solve::SolveProgramConstructionError::InvalidCallOutput { provenance });
        };
        self.view
            .function_definition(definition)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)
    }

    fn scoped_conditional_chain(
        &mut self,
        plan: &ScopedConditionalPlan<'dae>,
        first: usize,
    ) -> Result<ScopedResults<'program>, solve::SolveProgramConstructionError> {
        let Some(condition) = plan.conditions.get(first) else {
            return self.scoped_branch_values(plan, &plan.source.fallback);
        };
        let condition = self
            .expression(*condition)?
            .only_register(plan.provenance)?;
        let mut output_types = Vec::new();
        for definition in &plan.outputs {
            let ty = function_value_type(self.view, definition.target(), plan.provenance)?;
            output_types.extend(lower_value_type_leaves(
                self.view,
                ty,
                arithmetic_profile(),
            )?);
        }
        output_types.extend(self.pending_slot_types(&plan.pending));
        let requirements = self.scoped_branch_requirements(plan, first)?;
        let (captures, environment) = self.capture_environment_for_branches(&requirements)?;
        let context = self.region_context();
        let true_environment = environment.clone();
        let true_context = context.clone();
        let true_plan = plan.clone();
        let false_plan = plan.clone();
        let state_environment = self.assigned_capture_environment();
        if !plan.source.states.is_empty() || !state_environment.keys.is_empty() {
            return self.scoped_assigned_chain(
                plan,
                first,
                condition,
                AssignedRegionInputs {
                    captures: &captures,
                    output_types,
                    environment,
                    context,
                    state_environment,
                },
            );
        }
        let values = self.builder.conditional(
            condition,
            &captures,
            output_types,
            plan.provenance,
            move |builder, inputs, outputs| {
                let mut lowerer = load_region_lowerer(
                    builder,
                    inputs,
                    &true_environment,
                    &true_context,
                    true_plan.provenance,
                )?;
                let values =
                    lowerer.scoped_branch_values(&true_plan, &true_plan.source.branches[first])?;
                store_scoped_results(&mut lowerer, outputs, values.values, true_plan.provenance)
            },
            move |builder, inputs, outputs| {
                let mut lowerer = load_region_lowerer(
                    builder,
                    inputs,
                    &environment,
                    &context,
                    false_plan.provenance,
                )?;
                let values = lowerer.scoped_conditional_chain(&false_plan, first + 1)?;
                store_scoped_results(&mut lowerer, outputs, values.values, false_plan.provenance)
            },
        )?;
        Ok(ScopedResults {
            values,
            states: Vec::new(),
        })
    }

    fn scoped_branch_values(
        &mut self,
        plan: &ScopedConditionalPlan<'dae>,
        source: &BranchStatements<'dae>,
    ) -> Result<ScopedResults<'program>, solve::SolveProgramConstructionError> {
        self.next_direct_assertion = source.assertions.start;
        self.statements(source.branch.statements())?;
        if self.next_direct_assertion != source.assertions.end {
            return Err(solve::SolveProgramConstructionError::InvalidCallOutput {
                provenance: plan.provenance,
            });
        }
        let mut values = Vec::new();
        for output in &plan.outputs {
            let definition =
                self.scoped_branch_definition(source, output.target(), plan.provenance)?;
            values.extend(self.function_definition_value(definition)?.leaves);
        }
        for slot in &plan.pending {
            values.push(self.inactive_slot(*slot, plan.provenance)?);
        }
        let mut states = Vec::new();
        for value in &plan.source.states {
            states.extend(self.assigned_branch_value(source.branch, *value, plan.provenance)?);
        }
        Ok(ScopedResults { values, states })
    }
}

fn store_scoped_results<'program, 'dae>(
    lowerer: &mut ExpressionLowerer<'_, 'program, 'dae>,
    outputs: &[solve::ProgramSlot<'program>],
    values: Vec<solve::ProgramRegister<'program>>,
    provenance: rumoca_core::Span,
) -> Result<(), solve::SolveProgramConstructionError> {
    if outputs.len() != values.len() {
        return Err(solve::SolveProgramConstructionError::InvalidCallOutput { provenance });
    }
    for (output, value) in outputs.iter().zip(values) {
        lowerer.builder.store(*output, value, provenance)?;
    }
    Ok(())
}
