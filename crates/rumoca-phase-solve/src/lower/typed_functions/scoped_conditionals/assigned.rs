use super::super::regions::{EnvironmentLayout, RegionContext};
use super::*;

#[derive(Clone)]
pub(in crate::lower::typed_functions) struct AssignedCaptureEnvironment<'program, 'dae> {
    pub(in crate::lower::typed_functions) keys: Vec<(
        (dae::FunctionConditionalId<'dae>, dae::FunctionValueId<'dae>),
        std::ops::Range<usize>,
    )>,
    pub(in crate::lower::typed_functions) captures: Vec<solve::ProgramAssignedState<'program>>,
}

pub(super) struct AssignedRegionInputs<'inputs, 'program, 'dae> {
    pub(super) captures: &'inputs [solve::ProgramRegister<'program>],
    pub(super) output_types: Vec<solve::SolveValueType>,
    pub(super) environment: EnvironmentLayout<'dae>,
    pub(super) context: RegionContext<'dae>,
    pub(super) state_environment: AssignedCaptureEnvironment<'program, 'dae>,
}

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
    pub(in crate::lower::typed_functions) fn assigned_capture_environment(
        &self,
    ) -> AssignedCaptureEnvironment<'program, 'dae> {
        let mut entries = self.assigned_values.iter().collect::<Vec<_>>();
        entries.sort_by_key(|((conditional, value), _)| {
            (
                conditional.function().index(),
                conditional.ordinal(),
                value.ordinal(),
            )
        });
        let mut keys = Vec::new();
        let mut captures = Vec::new();
        for (key, states) in entries {
            let start = captures.len();
            captures.extend(states);
            keys.push((*key, start..captures.len()));
        }
        AssignedCaptureEnvironment { keys, captures }
    }

    pub(super) fn scoped_assigned_chain(
        &mut self,
        plan: &ScopedConditionalPlan<'dae>,
        first: usize,
        condition: solve::ProgramRegister<'program>,
        inputs: AssignedRegionInputs<'_, 'program, 'dae>,
    ) -> Result<ScopedResults<'program>, solve::SolveProgramConstructionError> {
        let AssignedRegionInputs {
            captures,
            output_types,
            environment,
            context,
            state_environment,
        } = inputs;
        let mut state_types = Vec::new();
        for value in &plan.source.states {
            let ty = function_value_type(self.view, *value, plan.provenance)?;
            state_types.extend(lower_value_type_leaves(
                self.view,
                ty,
                arithmetic_profile(),
            )?);
        }
        let true_environment = environment.clone();
        let true_context = context.clone();
        let true_plan = plan.clone();
        let false_plan = plan.clone();
        let true_state_keys = state_environment.keys.clone();
        let false_state_keys = state_environment.keys;
        let (values, states) = self.builder.conditional_assigned(
            condition,
            (captures, &state_environment.captures),
            (output_types, state_types),
            plan.provenance,
            move |builder, inputs, states, outputs| {
                let mut lowerer = load_region_lowerer(
                    builder,
                    inputs,
                    &true_environment,
                    &true_context,
                    true_plan.provenance,
                )?;
                bind_states(&mut lowerer, &true_state_keys, states, true_plan.provenance)?;
                let results =
                    lowerer.scoped_branch_values(&true_plan, &true_plan.source.branches[first])?;
                publish_results(&mut lowerer, outputs, results, true_plan.provenance)
            },
            move |builder, inputs, states, outputs| {
                let mut lowerer = load_region_lowerer(
                    builder,
                    inputs,
                    &environment,
                    &context,
                    false_plan.provenance,
                )?;
                bind_states(
                    &mut lowerer,
                    &false_state_keys,
                    states,
                    false_plan.provenance,
                )?;
                let results = lowerer.scoped_conditional_chain(&false_plan, first + 1)?;
                publish_results(&mut lowerer, outputs, results, false_plan.provenance)
            },
        )?;
        Ok(ScopedResults { values, states })
    }
}

pub(in crate::lower::typed_functions) fn bind_states<'program, 'dae>(
    lowerer: &mut ExpressionLowerer<'_, 'program, 'dae>,
    keys: &[(
        (dae::FunctionConditionalId<'dae>, dae::FunctionValueId<'dae>),
        std::ops::Range<usize>,
    )],
    captures: &[solve::ProgramAssignedState<'program>],
    provenance: rumoca_core::Span,
) -> Result<(), solve::SolveProgramConstructionError> {
    for (key, range) in keys {
        let states = captures
            .get(range.clone())
            .ok_or(solve::SolveProgramConstructionError::InvalidRegion { provenance })?
            .to_vec();
        lowerer.assigned_values.insert(*key, states);
    }
    Ok(())
}

fn publish_results<'program, 'dae>(
    lowerer: &mut ExpressionLowerer<'_, 'program, 'dae>,
    outputs: &[solve::ProgramSlot<'program>],
    results: ScopedResults<'program>,
    provenance: rumoca_core::Span,
) -> Result<(), solve::SolveProgramConstructionError> {
    store_scoped_results(lowerer, outputs, results.values, provenance)?;
    for (output, state) in results.states.into_iter().enumerate() {
        lowerer
            .builder
            .store_assigned_state(output, state, provenance)?;
    }
    Ok(())
}
