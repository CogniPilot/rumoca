//! Checked model-event transaction construction over the shared typed lowerer.

use std::{
    collections::{HashMap, HashSet},
    ops::Range,
};

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::{
    ExpressionLowerer, LoweredValue, PureCallRegistry, RegisteredAssertion, RegisteredCall,
    arithmetic_profile, lower_primitive_type, lower_value_type_leaves,
    regions::{RegionContext, RegionOutput, RegionValues, lower_region_values},
};
use crate::LowerError;
use crate::layout::LoweredLayout;
use crate::lower::call_scoped_actions::CallAssertionProjection;
use crate::lower::clocks::LoweredClocks;
use crate::lower::{
    delay_value_scalar_slot, pre_variable_scalar_slot, previous_value_scalar_slot,
    variable_scalar_slot,
};

pub(in crate::lower) struct PendingEventTransaction<'dae> {
    site: solve::SolvePureCallSite,
    inputs: Vec<(solve::ScalarSlot, solve::SolveValueType)>,
    targets: Vec<(
        solve::ScalarSlot,
        solve::SolveValueType,
        Option<solve::PeriodicClockId>,
    )>,
    target_variables: Vec<dae::VariableId<'dae>>,
    same_tick_value_reads: Vec<dae::ExprId<'dae>>,
    producer_owners: Vec<solve::EventTransactionProducerOwner>,
    assertions: Vec<(solve::SolveEventAction, CallAssertionProjection)>,
    statement_count: usize,
    clock_owners: Vec<solve::PeriodicClockId>,
    provenance: rumoca_core::ProvenanceSpan,
}

impl<'dae> PendingEventTransaction<'dae> {
    pub(in crate::lower) fn target_projections(
        &self,
    ) -> impl Iterator<
        Item = (
            dae::VariableId<'dae>,
            solve::ScalarSlot,
            &solve::SolveValueType,
        ),
    > {
        self.target_variables
            .iter()
            .copied()
            .zip(self.targets.iter())
            .map(|(variable, (base, value_type, _))| (variable, *base, value_type))
    }

    #[cfg(test)]
    pub(super) fn input_types(&self) -> impl Iterator<Item = &solve::SolveValueType> {
        self.inputs.iter().map(|(_, value_type)| value_type)
    }

    #[cfg(test)]
    pub(super) fn target_types(&self) -> impl Iterator<Item = &solve::SolveValueType> {
        self.targets.iter().map(|(_, value_type, _)| value_type)
    }

    #[cfg(test)]
    pub(super) fn target_clock_owners(
        &self,
    ) -> impl Iterator<Item = Option<solve::PeriodicClockId>> + '_ {
        self.targets.iter().map(|(_, _, clock)| *clock)
    }

    #[cfg(test)]
    pub(super) const fn statement_count(&self) -> usize {
        self.statement_count
    }

    pub(in crate::lower) fn clock_owners(&self) -> &[solve::PeriodicClockId] {
        &self.clock_owners
    }

    #[cfg(test)]
    pub(super) const fn site(&self) -> &solve::SolvePureCallSite {
        &self.site
    }

    pub(in crate::lower) fn push_producer_owner(
        &mut self,
        owner: solve::EventTransactionProducerOwner,
    ) {
        self.producer_owners.push(owner);
    }

    pub(in crate::lower) fn same_tick_value_reads(&self) -> &[dae::ExprId<'dae>] {
        &self.same_tick_value_reads
    }

    pub(in crate::lower) fn provenance(&self) -> rumoca_core::Span {
        self.provenance.span()
    }

    pub(in crate::lower) fn finish(
        self,
        action_indices: &HashMap<CallAssertionProjection, Vec<usize>>,
    ) -> Result<solve::EventTransactionProgram, LowerError> {
        let span = self.provenance.span();
        let mut assertions = Vec::with_capacity(self.assertions.len());
        let mut assertion_action_indices = Vec::with_capacity(self.assertions.len());
        for (action, projection) in self.assertions {
            let action_indices = action_indices.get(&projection).cloned().ok_or_else(|| {
                LowerError::contract(
                    "event transaction lost its compiler-issued assertion action projection",
                    span,
                )
            })?;
            assertions.push(action);
            assertion_action_indices.push(action_indices);
        }
        solve::EventTransactionProgram::checked(
            solve::EventTransactionConstruction {
                site: self.site,
                inputs: self.inputs,
                targets: self.targets,
                producer_owners: self.producer_owners,
                assertions,
                assertion_action_indices,
                statement_count: self.statement_count,
                clock_owners: self.clock_owners,
            },
            self.provenance,
        )
        .map_err(Into::into)
    }
}

#[derive(Clone)]
struct RegisteredTransactionAssertion<'dae> {
    assertion: RegisteredAssertion<'dae>,
    projection: CallAssertionProjection,
    clock: dae::ClockId<'dae>,
}

type RegisteredExpressionCalls<'dae> = (
    HashMap<dae::ExprId<'dae>, RegisteredCall<'dae>>,
    HashMap<dae::ExprId<'dae>, Range<usize>>,
    Vec<RegisteredTransactionAssertion<'dae>>,
);

#[derive(Clone, Copy)]
struct EligibleEventDefinition<'dae> {
    target: dae::ModelEventTarget<'dae>,
    value: dae::ExprId<'dae>,
    value_type: dae::ValueTypeId<'dae>,
    clock: dae::ClockId<'dae>,
}

struct EligibleEventTransaction<'dae> {
    clock_owners: Vec<dae::ClockId<'dae>>,
    definitions: Vec<EligibleEventDefinition<'dae>>,
    statement_count: usize,
}

/// One semantic model-storage coordinate captured by a typed owner.
///
/// This key deliberately retains the aggregate coordinate identity. A tensor
/// coordinate therefore owns one typed register regardless of its element
/// count; only the final execution/emission adapter materializes storage
/// elements.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) enum ModelCoordinateKey<'dae> {
    Parameter(dae::ParameterId<'dae>),
    Input(dae::InputId<'dae>),
    State(dae::StateId<'dae>),
    Derivative(dae::StateId<'dae>),
    Algebraic(dae::AlgebraicId<'dae>),
    DiscreteReal(dae::DiscreteRealId<'dae>),
    DiscreteValue(dae::DiscreteValueId<'dae>),
    PreDiscreteReal(dae::DiscreteRealId<'dae>),
    PreDiscreteValue(dae::DiscreteValueId<'dae>),
    PreState(dae::StateId<'dae>),
    PreAlgebraic(dae::AlgebraicId<'dae>),
    Time,
    ClockInterval(dae::PeriodicClockId<'dae>),
    Condition(dae::ConditionId<'dae>),
    Delay(dae::DelayId<'dae>),
    Previous(dae::PreviousId<'dae>),
    Terminal(dae::TerminalId<'dae>),
}

impl<'dae> ModelCoordinateKey<'dae> {
    pub(super) fn from_view(coordinate: dae::CoordinateView<'dae>) -> Option<Self> {
        Some(match coordinate {
            dae::CoordinateView::Parameter(id) => Self::Parameter(id),
            dae::CoordinateView::Input(id) => Self::Input(id),
            dae::CoordinateView::State(id) => Self::State(id),
            dae::CoordinateView::Derivative(id) => Self::Derivative(id),
            dae::CoordinateView::Algebraic(id) => Self::Algebraic(id),
            dae::CoordinateView::DiscreteReal(id) => Self::DiscreteReal(id),
            dae::CoordinateView::DiscreteValue(id) => Self::DiscreteValue(id),
            dae::CoordinateView::PreDiscreteReal(id) => Self::PreDiscreteReal(id),
            dae::CoordinateView::PreDiscreteValue(id) => Self::PreDiscreteValue(id),
            dae::CoordinateView::PreState(id) => Self::PreState(id),
            dae::CoordinateView::PreAlgebraic(id) => Self::PreAlgebraic(id),
            dae::CoordinateView::Time => Self::Time,
            dae::CoordinateView::ClockInterval(id) => Self::ClockInterval(id),
            dae::CoordinateView::Condition(id) => Self::Condition(id),
            dae::CoordinateView::Delay(id) => Self::Delay(id),
            dae::CoordinateView::Previous(id) => Self::Previous(id),
            dae::CoordinateView::Terminal(id) => Self::Terminal(id),
            dae::CoordinateView::Binder(_) | dae::CoordinateView::FunctionParameter(_) => {
                return None;
            }
        })
    }

    pub(super) const fn stable_key(self) -> (u8, u32) {
        match self {
            Self::Parameter(id) => (0, id.index()),
            Self::Input(id) => (1, id.index()),
            Self::State(id) => (2, id.index()),
            Self::Derivative(id) => (3, id.index()),
            Self::Algebraic(id) => (4, id.index()),
            Self::DiscreteReal(id) => (5, id.index()),
            Self::DiscreteValue(id) => (6, id.index()),
            Self::PreDiscreteReal(id) => (7, id.index()),
            Self::PreDiscreteValue(id) => (8, id.index()),
            Self::PreState(id) => (9, id.index()),
            Self::PreAlgebraic(id) => (10, id.index()),
            Self::Time => (11, 0),
            Self::ClockInterval(id) => (12, id.index()),
            Self::Condition(id) => (13, id.index()),
            Self::Delay(id) => (14, id.index()),
            Self::Previous(id) => (15, id.index()),
            Self::Terminal(id) => (16, id.index()),
        }
    }
}

impl<'dae> PureCallRegistry<'dae> {
    fn add_event_transaction_owner(
        &mut self,
        view: dae::DaeView<'dae>,
        transaction: &EligibleEventTransaction<'dae>,
        coordinate_types: &[(ModelCoordinateKey<'dae>, dae::ValueTypeId<'dae>)],
        provenance: rumoca_core::Span,
    ) -> Result<
        (
            solve::SolvePureCallSite,
            Vec<RegisteredTransactionAssertion<'dae>>,
        ),
        solve::SolveProgramConstructionError,
    > {
        let definitions = &transaction.definitions;
        let (callees, predicate_ranges, assertions) = self.register_expression_calls(
            view,
            definitions
                .iter()
                .map(|definition| (definition.value, definition.clock)),
        )?;
        let predicate_count = assertions.len();
        let (coordinate_inputs, inputs, outputs) =
            event_transaction_interface(view, transaction, coordinate_types, predicate_count)?;
        let identity = self.identities.issue(provenance)?;
        let owner = self.table.add_owner(
            identity,
            inputs,
            outputs,
            provenance,
            |builder, inputs, outputs| {
                let (coordinate_slots, activation_slots) = inputs.split_at(coordinate_inputs.len());
                let model_coordinates = coordinate_types
                    .iter()
                    .zip(coordinate_slots)
                    .map(|((key, value_type), input)| {
                        let register = builder.load(*input, provenance)?;
                        Ok((*key, LoweredValue::scalar(*value_type, register)))
                    })
                    .collect::<Result<HashMap<_, _>, solve::SolveProgramConstructionError>>()?;
                let activations = transaction
                    .clock_owners
                    .iter()
                    .copied()
                    .zip(activation_slots)
                    .map(|(clock, input)| {
                        builder.load(*input, provenance).map(|value| (clock, value))
                    })
                    .collect::<Result<HashMap<_, _>, _>>()?;
                let mut lowerer = ExpressionLowerer {
                    view,
                    builder,
                    model_coordinates,
                    parameters: HashMap::new(),
                    function_values: HashMap::new(),
                    conditional_groups: HashMap::new(),
                    fold_parameters: HashMap::new(),
                    fold_values: HashMap::new(),
                    binders: HashMap::new(),
                    callees,
                    predicate_ranges,
                    cache: HashMap::new(),
                    call_values: HashMap::new(),
                    predicate_values: vec![None; predicate_count],
                    next_direct_assertion: 0,
                    direct_assertion_count: 0,
                };
                let mut values = vec![None; definitions.len()];
                for clock in &transaction.clock_owners {
                    lower_clock_member_group(
                        &mut lowerer,
                        definitions,
                        *clock,
                        activations[clock],
                        provenance,
                        &mut values,
                    )?;
                }
                for (value, output) in values.into_iter().zip(outputs) {
                    let register = value
                        .ok_or(solve::SolveProgramConstructionError::InvalidCallOutput {
                            provenance,
                        })?
                        .only_register(provenance)?;
                    lowerer.builder.store(*output, register, provenance)?;
                }
                for (predicate, output) in lowerer
                    .predicate_values
                    .into_iter()
                    .zip(&outputs[definitions.len()..])
                {
                    let predicate = predicate.ok_or(
                        solve::SolveProgramConstructionError::InvalidCallOutput { provenance },
                    )?;
                    lowerer.builder.store(*output, predicate, provenance)?;
                }
                Ok(())
            },
        )?;
        let site = self
            .table
            .call_site(owner)
            .ok_or(solve::SolveProgramConstructionError::UnknownCallOwner { provenance })?;
        Ok((site, assertions))
    }

    // SPEC_0021: Exception - exhaustive expression-tree walk for nested call ownership.
    #[allow(clippy::excessive_nesting)]
    fn register_expression_calls(
        &mut self,
        view: dae::DaeView<'dae>,
        expressions: impl IntoIterator<Item = (dae::ExprId<'dae>, dae::ClockId<'dae>)>,
    ) -> Result<RegisteredExpressionCalls<'dae>, solve::SolveProgramConstructionError> {
        let mut roots = Vec::new();
        let mut seen = HashMap::new();
        let mut conflicts = HashSet::new();
        for (expression, clock) in expressions {
            dae::for_each_expression(view, expression, |projection, node| {
                let dae::ExpressionOperation::Call { owner, .. } = node.operation() else {
                    return;
                };
                match seen.insert(owner, clock) {
                    None => roots.push((owner, projection, clock)),
                    Some(previous) if previous != clock => {
                        conflicts.insert(owner);
                    }
                    Some(_) => {}
                }
            });
        }
        for (_, expression, clock) in &roots {
            let dae::ExpressionOperation::Call { owner, .. } = view
                .expression(*expression)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                .operation()
            else {
                return Err(solve::SolveProgramConstructionError::WireMismatch);
            };
            if conflicts.contains(&owner) || seen.get(&owner) != Some(clock) {
                return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                    provenance: view
                        .expression(*expression)
                        .expect("checked call projection resolves")
                        .provenance()
                        .span(),
                });
            }
        }
        let mut callees = HashMap::new();
        let mut predicate_ranges = HashMap::new();
        let mut predicate_count = 0usize;
        let mut assertions = Vec::new();
        for (owner, projection, clock) in roots {
            let registered = self.register_root(view, projection)?;
            let end = predicate_count
                .checked_add(registered.assertion_count)
                .ok_or(solve::SolveProgramConstructionError::IdentityOverflow {
                    provenance: view
                        .expression(projection)
                        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                        .provenance()
                        .span(),
                })?;
            predicate_ranges.insert(owner, predicate_count..end);
            predicate_count = end;
            assertions.extend(registered.assertions.iter().cloned().enumerate().map(
                |(output_offset, assertion)| RegisteredTransactionAssertion {
                    assertion,
                    projection: CallAssertionProjection {
                        owner: registered.owner,
                        output_offset,
                    },
                    clock,
                },
            ));
            callees.insert(owner, registered);
        }
        Ok((callees, predicate_ranges, assertions))
    }
}

/// Lower one exact clock region as a lazy typed conditional.
///
/// The true region evaluates every target definition together, retaining one
/// construction-issued call scope across mixed aggregate outputs. The false
/// region copies the aligned current target values and returns `true` for any
/// call-scoped assertion predicate, so an inactive clock cannot execute work
/// or fail an assertion. The outer transaction later commits the complete
/// tuple atomically.
/// Lower the definitions owned by one clock and place each result at its
/// original definition index.
///
/// The transaction commits atomically, so per-clock grouping only decides which
/// activation guards a definition, never the order results are stored in.
fn lower_clock_member_group<'program, 'dae>(
    lowerer: &mut ExpressionLowerer<'_, 'program, 'dae>,
    definitions: &[EligibleEventDefinition<'dae>],
    clock: dae::ClockId<'dae>,
    activation: solve::ProgramRegister<'program>,
    provenance: rumoca_core::Span,
    values: &mut [Option<LoweredValue<'program, 'dae>>],
) -> Result<(), solve::SolveProgramConstructionError> {
    let members = definitions
        .iter()
        .enumerate()
        .filter_map(|(index, definition)| (definition.clock == clock).then_some(index))
        .collect::<Vec<_>>();
    let lowered =
        activated_assignment_group(lowerer, definitions, &members, activation, provenance)?;
    for (index, value) in members.into_iter().zip(lowered) {
        values[index] = Some(value);
    }
    Ok(())
}

/// Append each member's fallback coordinate leaves to `captures`, returning the
/// capture range that belongs to each member.
///
/// The fallback is the value the target keeps when its activation is false, so
/// every member contributes one contiguous range even when its type is compound.
fn extend_with_fallback_captures<'program, 'dae>(
    lowerer: &ExpressionLowerer<'_, 'program, 'dae>,
    definitions: &[EligibleEventDefinition<'dae>],
    members: &[usize],
    captures: &mut Vec<solve::ProgramRegister<'program>>,
    provenance: rumoca_core::Span,
) -> Result<Vec<std::ops::Range<usize>>, solve::SolveProgramConstructionError> {
    let mut fallback_ranges = Vec::with_capacity(members.len());
    for &index in members {
        let definition = definitions[index];
        let fallback = lowerer
            .model_coordinates
            .get(&model_event_target_key(definition.target))
            .cloned()
            .ok_or(solve::SolveProgramConstructionError::InvalidCallInterface { provenance })?;
        let start = captures.len();
        captures.extend(fallback.leaves);
        fallback_ranges.push(start..captures.len());
    }
    Ok(fallback_ranges)
}

/// Flatten each value type into its scalar leaves, returning the leaf types and
/// the output range that belongs to each value.
fn lower_value_type_outputs<'dae>(
    lowerer: &ExpressionLowerer<'_, '_, 'dae>,
    value_types: &[dae::ValueTypeId<'dae>],
) -> Result<
    (Vec<solve::SolveValueType>, Vec<std::ops::Range<usize>>),
    solve::SolveProgramConstructionError,
> {
    let mut output_types = Vec::new();
    let mut output_ranges = Vec::with_capacity(value_types.len());
    for &value_type in value_types {
        let start = output_types.len();
        output_types.extend(lower_value_type_leaves(
            lowerer.view,
            value_type,
            arithmetic_profile(),
        )?);
        output_ranges.push(start..output_types.len());
    }
    Ok((output_types, output_ranges))
}

/// The call interface of an event transaction: coordinate inputs, the full
/// input list, and the output list.
type EventTransactionInterface = (
    Vec<solve::SolveValueType>,
    Vec<solve::SolveValueType>,
    Vec<solve::SolvePureCallOutput>,
);

/// Build the call interface of an event transaction: the coordinate inputs, the
/// full input list (coordinates then one activation per clock), and the outputs
/// (one result per definition then one predicate per assertion).
fn event_transaction_interface<'dae>(
    view: dae::DaeView<'dae>,
    transaction: &EligibleEventTransaction<'dae>,
    coordinate_types: &[(ModelCoordinateKey<'dae>, dae::ValueTypeId<'dae>)],
    predicate_count: usize,
) -> Result<EventTransactionInterface, solve::SolveProgramConstructionError> {
    let coordinate_inputs = coordinate_types
        .iter()
        .map(|(_, value_type)| lower_primitive_type(view, *value_type, arithmetic_profile()))
        .collect::<Result<Vec<_>, _>>()?;
    let mut inputs = coordinate_inputs.clone();
    inputs.extend(std::iter::repeat_n(
        solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
        transaction.clock_owners.len(),
    ));
    let mut outputs = transaction
        .definitions
        .iter()
        .map(|definition| {
            lower_primitive_type(view, definition.value_type, arithmetic_profile())
                .map(solve::SolvePureCallOutput::result)
        })
        .collect::<Result<Vec<_>, _>>()?;
    outputs.extend(
        std::iter::repeat_with(solve::SolvePureCallOutput::assertion_predicate)
            .take(predicate_count),
    );
    Ok((coordinate_inputs, inputs, outputs))
}

fn activated_assignment_group<'program, 'dae>(
    lowerer: &mut ExpressionLowerer<'_, 'program, 'dae>,
    definitions: &[EligibleEventDefinition<'dae>],
    members: &[usize],
    activation: solve::ProgramRegister<'program>,
    provenance: rumoca_core::Span,
) -> Result<Vec<LoweredValue<'program, 'dae>>, solve::SolveProgramConstructionError> {
    let expressions = members
        .iter()
        .map(|&index| definitions[index].value)
        .collect::<Vec<_>>();
    let pending = lowerer.pending_predicates(expressions.iter().copied());
    let (mut captures, environment) =
        lowerer.capture_environment_for(expressions.iter().copied())?;
    let fallback_ranges =
        extend_with_fallback_captures(lowerer, definitions, members, &mut captures, provenance)?;
    let value_types = members
        .iter()
        .map(|&index| definitions[index].value_type)
        .collect::<Vec<_>>();
    let (mut output_types, output_ranges) = lower_value_type_outputs(lowerer, &value_types)?;
    let value_output_count = output_types.len();
    output_types.extend(std::iter::repeat_n(
        solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
        pending.len(),
    ));
    let context = RegionContext {
        view: lowerer.view,
        callees: lowerer.callees.clone(),
        predicate_ranges: lowerer.predicate_ranges.clone(),
        conditional_groups: lowerer.conditional_groups.clone(),
        predicate_count: lowerer.predicate_values.len(),
        direct_assertion_count: lowerer.direct_assertion_count,
    };
    let true_environment = environment.clone();
    let true_context = context.clone();
    let true_pending = pending.clone();
    let results = value_types
        .iter()
        .copied()
        .zip(expressions.iter().copied())
        .collect::<Vec<RegionOutput<'dae>>>();
    let false_ranges = fallback_ranges.clone();
    let destinations = lowerer.builder.conditional(
        activation,
        &captures,
        output_types,
        provenance,
        move |builder, inputs, outputs| {
            lower_region_values(
                builder,
                inputs,
                outputs,
                &true_environment,
                &true_context,
                RegionValues {
                    results: &results,
                    pending_predicates: &true_pending,
                    provenance,
                },
            )
        },
        move |builder, inputs, outputs| {
            let mut output = 0usize;
            for range in &false_ranges {
                for input in &inputs[range.clone()] {
                    let value = builder.load(*input, provenance)?;
                    builder.store(outputs[output], value, provenance)?;
                    output += 1;
                }
            }
            for slot in &outputs[output..] {
                let value = builder.constant(solve::SolveValue::boolean(true), provenance)?;
                builder.store(*slot, value, provenance)?;
            }
            Ok(())
        },
    )?;
    for (&predicate, value) in pending
        .iter()
        .zip(destinations[value_output_count..].iter().copied())
    {
        lowerer.predicate_values[predicate] = Some(value);
    }
    Ok(value_types
        .into_iter()
        .zip(output_ranges)
        .map(|(value_type, range)| LoweredValue {
            value_type,
            leaves: destinations[range].to_vec(),
        })
        .collect())
}

const fn model_event_target_key(target: dae::ModelEventTarget<'_>) -> ModelCoordinateKey<'_> {
    match target {
        dae::ModelEventTarget::DiscreteReal(id) => ModelCoordinateKey::DiscreteReal(id),
        dae::ModelEventTarget::DiscreteValue(id) => ModelCoordinateKey::DiscreteValue(id),
    }
}

pub(in crate::lower) fn lower_model_event_transactions<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
) -> Result<Vec<PendingEventTransaction<'dae>>, LowerError> {
    let mut programs = Vec::new();
    let mut registry = layout.pure_calls.borrow_mut();
    for index in 0..view.model_event_transaction_count() {
        let id = view
            .model_event_transaction_id(index)
            .expect("dense checked model-event transaction identity resolves");
        let transaction = view
            .model_event_transaction(id)
            .expect("checked model-event transaction identity resolves");
        let provenance = transaction.provenance().span();
        let eligible = eligible_event_transaction(view, transaction).ok_or_else(|| {
                LowerError::non_computable(
                    "model-event transaction has no construction-issued final definition for every target under exact periodic-clock activation",
                    provenance,
                )
            })?;
        let coordinate_types = collect_model_coordinates(view, &eligible, provenance)?;
        let solve_clocks = eligible
            .clock_owners
            .iter()
            .copied()
            .map(|clock| clocks.clock(clock))
            .collect::<Result<Vec<_>, _>>()?;
        let (site, assertions) = registry
            .add_event_transaction_owner(view, &eligible, &coordinate_types, provenance)
            .map_err(|error| LowerError::contract(error.to_string(), provenance))?;
        let assertions = assertions
            .into_iter()
            .map(|registered| {
                let clock = clocks.clock(registered.clock)?;
                event_transaction_assertion(view, registered.assertion, clock, provenance)
                    .map(|action| (action, registered.projection))
            })
            .collect::<Result<Vec<_>, _>>()?;
        let inputs = transaction_inputs(
            view,
            layout,
            &coordinate_types,
            &eligible.clock_owners,
            provenance,
        )?;
        let targets = transaction_targets(view, layout, &eligible, clocks, provenance)?;
        let provenance_span = provenance
            .require_provenance("model-event transaction")
            .map_err(|error| LowerError::contract(error.to_string(), provenance))?;
        programs.push(PendingEventTransaction {
            site,
            inputs,
            target_variables: targets.iter().map(|target| target.0).collect(),
            same_tick_value_reads: eligible
                .definitions
                .iter()
                .map(|definition| definition.value)
                .collect(),
            targets: targets
                .into_iter()
                .map(|(_, base, value_type, clock)| (base, value_type, Some(clock)))
                .collect(),
            producer_owners: Vec::new(),
            assertions,
            statement_count: eligible.statement_count,
            clock_owners: solve_clocks,
            provenance: provenance_span,
        });
    }
    Ok(programs)
}

fn transaction_inputs<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    coordinate_types: &[(ModelCoordinateKey<'dae>, dae::ValueTypeId<'dae>)],
    clock_owners: &[dae::ClockId<'dae>],
    provenance: rumoca_core::Span,
) -> Result<Vec<(solve::ScalarSlot, solve::SolveValueType)>, LowerError> {
    let mut inputs = coordinate_types
        .iter()
        .map(|(key, value_type)| {
            Ok::<_, LowerError>((
                model_coordinate_source(view, layout, *key, provenance)?,
                lower_primitive_type(view, *value_type, arithmetic_profile())
                    .map_err(|error| LowerError::contract(error.to_string(), provenance))?,
            ))
        })
        .collect::<Result<Vec<_>, _>>()?;
    for clock in clock_owners {
        let index = layout
            .clock_activations
            .get(clock.index() as usize)
            .copied()
            .ok_or_else(|| {
                LowerError::contract(
                    "event-transaction activation clock has no Solve parameter lane",
                    provenance,
                )
            })?;
        inputs.push((
            solve::scalar_slot_p(index),
            solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
        ));
    }
    Ok(inputs)
}

fn transaction_targets<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    transaction: &EligibleEventTransaction<'dae>,
    clocks: &LoweredClocks<'dae>,
    provenance: rumoca_core::Span,
) -> Result<
    Vec<(
        dae::VariableId<'dae>,
        solve::ScalarSlot,
        solve::SolveValueType,
        solve::PeriodicClockId,
    )>,
    LowerError,
> {
    transaction
        .definitions
        .iter()
        .map(|definition| {
            let target = definition.target;
            let variable = view
                .variable_id(target.variable() as usize)
                .ok_or_else(|| {
                    LowerError::contract("transaction target variable is out of bounds", provenance)
                })?;
            Ok((
                variable,
                variable_scalar_slot(layout, target.variable(), 0, provenance)?,
                lower_primitive_type(view, definition.value_type, arithmetic_profile())
                    .map_err(|error| LowerError::contract(error.to_string(), provenance))?,
                clocks.clock(definition.clock)?,
            ))
        })
        .collect()
}

fn event_transaction_assertion<'dae>(
    view: dae::DaeView<'dae>,
    assertion: RegisteredAssertion<'dae>,
    clock: solve::PeriodicClockId,
    provenance: rumoca_core::Span,
) -> Result<solve::SolveEventAction, LowerError> {
    let message = view
        .expression(assertion.message)
        .and_then(|expression| match expression.operation() {
            dae::ExpressionOperation::Literal(dae::DaeLiteral::String(message)) => {
                Some(message.clone())
            }
            _ => None,
        })
        .ok_or_else(|| {
            LowerError::non_computable(
                "event-transaction assertion message is not a checked literal String",
                provenance,
            )
        })?;
    Ok(solve::SolveEventAction {
        kind: solve::SolveEventActionKind::Assert,
        message: solve::SolveEventMessage {
            parts: vec![solve::SolveEventMessagePart::Text(message)],
        },
        span: assertion.provenance.span(),
        origin: assertion.provenance.origin().to_string(),
        clock_owner: Some(clock),
    })
}

fn eligible_event_transaction<'dae>(
    view: dae::DaeView<'dae>,
    transaction: dae::ModelEventTransactionView<'dae>,
) -> Option<EligibleEventTransaction<'dae>> {
    let steps = transaction.steps().collect::<Vec<_>>();
    let mut final_values = HashMap::new();
    let mut clock_owners = Vec::new();
    for step in &steps {
        let clock = step.clock()?;
        let is_clock_level = step.trigger() == step.guard()
            && matches!(
                view.condition(step.trigger()).map(|condition| condition.operation()),
                Some(dae::ConditionOperation::Clock(owner)) if owner == clock
            );
        if !clock_owners.contains(&clock) {
            clock_owners.push(clock);
        }
        for definition in step.definitions() {
            final_values.insert(
                definition.target().variable(),
                is_clock_level.then_some((definition.target(), definition.value(), clock)),
            );
        }
    }
    clock_owners.sort_by_key(|clock| clock.index());
    let definitions = transaction
        .targets()
        .map(|target| {
            let variable = view.variable(view.variable_id(target.variable() as usize)?)?;
            let (defined_target, value, clock) =
                final_values.get(&target.variable()).copied().flatten()?;
            (defined_target == target).then_some(EligibleEventDefinition {
                target,
                value,
                value_type: variable.value_type_id(),
                clock,
            })
        })
        .collect::<Option<Vec<_>>>()?;
    Some(EligibleEventTransaction {
        clock_owners,
        definitions,
        statement_count: steps.len(),
    })
}

fn collect_model_coordinates<'dae>(
    view: dae::DaeView<'dae>,
    transaction: &EligibleEventTransaction<'dae>,
    provenance: rumoca_core::Span,
) -> Result<Vec<(ModelCoordinateKey<'dae>, dae::ValueTypeId<'dae>)>, LowerError> {
    let mut coordinates = HashMap::new();
    let mut mismatch = false;
    for definition in &transaction.definitions {
        coordinates.insert(
            model_event_target_key(definition.target),
            definition.value_type,
        );
        dae::for_each_expression(view, definition.value, |_, node| {
            let dae::ExpressionOperation::Coordinate(coordinate) = node.operation() else {
                return;
            };
            let Some(key) = ModelCoordinateKey::from_view(coordinate) else {
                return;
            };
            match coordinates.insert(key, node.value_type_id()) {
                Some(previous) if previous != node.value_type_id() => mismatch = true,
                _ => {}
            }
        });
    }
    if mismatch {
        return Err(LowerError::contract(
            "one event-transaction coordinate has incompatible aggregate types",
            provenance,
        ));
    }
    let mut coordinates = coordinates.into_iter().collect::<Vec<_>>();
    coordinates.sort_by_key(|(key, _)| key.stable_key());
    Ok(coordinates)
}

fn model_coordinate_source<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    coordinate: ModelCoordinateKey<'dae>,
    provenance: rumoca_core::Span,
) -> Result<solve::ScalarSlot, LowerError> {
    match coordinate {
        ModelCoordinateKey::Parameter(id) => {
            variable_scalar_slot(layout, id.index(), 0, provenance)
        }
        ModelCoordinateKey::Input(id) => variable_scalar_slot(layout, id.index(), 0, provenance),
        ModelCoordinateKey::State(id) => variable_scalar_slot(layout, id.index(), 0, provenance),
        ModelCoordinateKey::Algebraic(id) => {
            variable_scalar_slot(layout, id.index(), 0, provenance)
        }
        ModelCoordinateKey::DiscreteReal(id) => {
            variable_scalar_slot(layout, id.index(), 0, provenance)
        }
        ModelCoordinateKey::DiscreteValue(id) => {
            variable_scalar_slot(layout, id.index(), 0, provenance)
        }
        ModelCoordinateKey::PreDiscreteReal(id) => {
            pre_variable_scalar_slot(layout, id.index(), 0, provenance)
        }
        ModelCoordinateKey::PreDiscreteValue(id) => {
            pre_variable_scalar_slot(layout, id.index(), 0, provenance)
        }
        ModelCoordinateKey::PreState(id) => {
            pre_variable_scalar_slot(layout, id.index(), 0, provenance)
        }
        ModelCoordinateKey::PreAlgebraic(id) => {
            pre_variable_scalar_slot(layout, id.index(), 0, provenance)
        }
        ModelCoordinateKey::Time => Ok(solve::ScalarSlot::Time),
        ModelCoordinateKey::ClockInterval(id) => Ok(solve::ScalarSlot::Constant(
            view.periodic_clock(id).period_seconds(),
        )),
        ModelCoordinateKey::Delay(id) => delay_value_scalar_slot(layout, id.index(), 0, provenance),
        ModelCoordinateKey::Previous(id) => {
            previous_value_scalar_slot(layout, id.index(), 0, provenance)
        }
        ModelCoordinateKey::Terminal(_) => layout
            .solve_layout
            .terminal_event_parameter_index
            .map(solve::scalar_slot_p)
            .ok_or_else(|| {
                LowerError::contract("terminal coordinate has no Solve slot", provenance)
            }),
        ModelCoordinateKey::Derivative(_) => Err(LowerError::non_computable(
            "an event transaction reads a continuous derivative without an issued refresh input",
            provenance,
        )),
        ModelCoordinateKey::Condition(_) => Err(LowerError::non_computable(
            "an event transaction reads a condition level without an issued condition input",
            provenance,
        )),
    }
}
