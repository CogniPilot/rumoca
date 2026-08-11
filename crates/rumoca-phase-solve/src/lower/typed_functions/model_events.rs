//! Checked model-event transaction construction over the shared typed lowerer.

use std::{
    collections::{HashMap, HashSet},
    ops::Range,
};

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::{
    ExpressionLowerer, LoweredValue, PureCallRegistry, RegisteredAssertion, RegisteredCall,
    arithmetic_profile, lower_primitive_type,
};
use crate::LowerError;
use crate::layout::LoweredLayout;
use crate::lower::clocks::LoweredClocks;
use crate::lower::{
    delay_value_scalar_slot, pre_variable_scalar_slot, previous_value_scalar_slot,
    variable_scalar_slot,
};

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
        definitions: &[(dae::ExprId<'dae>, dae::ValueTypeId<'dae>)],
        coordinate_types: &[(ModelCoordinateKey<'dae>, dae::ValueTypeId<'dae>)],
        provenance: rumoca_core::Span,
    ) -> Result<
        (solve::SolvePureCallSite, Vec<RegisteredAssertion<'dae>>),
        solve::SolveProgramConstructionError,
    > {
        let (callees, predicate_ranges, assertions) =
            self.register_expression_calls(view, definitions.iter().map(|(value, _)| *value))?;
        let predicate_count = assertions.len();
        let inputs = coordinate_types
            .iter()
            .map(|(_, value_type)| lower_primitive_type(view, *value_type, arithmetic_profile()))
            .collect::<Result<Vec<_>, _>>()?;
        let mut outputs = definitions
            .iter()
            .map(|(_, value_type)| {
                lower_primitive_type(view, *value_type, arithmetic_profile())
                    .map(solve::SolvePureCallOutput::result)
            })
            .collect::<Result<Vec<_>, _>>()?;
        outputs.extend(
            std::iter::repeat_with(solve::SolvePureCallOutput::assertion_predicate)
                .take(predicate_count),
        );
        let identity = self.identities.issue(provenance)?;
        let owner = self.table.add_owner(
            identity,
            inputs,
            outputs,
            provenance,
            |builder, inputs, outputs| {
                let model_coordinates = coordinate_types
                    .iter()
                    .zip(inputs)
                    .map(|((key, value_type), input)| {
                        let register = builder.load(*input, provenance)?;
                        Ok((*key, LoweredValue::scalar(*value_type, register)))
                    })
                    .collect::<Result<HashMap<_, _>, solve::SolveProgramConstructionError>>()?;
                let mut lowerer = ExpressionLowerer {
                    view,
                    builder,
                    model_coordinates,
                    parameters: HashMap::new(),
                    function_values: HashMap::new(),
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
                for ((expression, value_type), output) in definitions.iter().zip(outputs) {
                    let value = lowerer.expression(*expression)?;
                    let value = lowerer.coerce_value(value, *value_type, provenance)?;
                    let register = value.only_register(provenance)?;
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

    fn register_expression_calls(
        &mut self,
        view: dae::DaeView<'dae>,
        expressions: impl IntoIterator<Item = dae::ExprId<'dae>>,
    ) -> Result<
        (
            HashMap<dae::ExprId<'dae>, RegisteredCall<'dae>>,
            HashMap<dae::ExprId<'dae>, Range<usize>>,
            Vec<RegisteredAssertion<'dae>>,
        ),
        solve::SolveProgramConstructionError,
    > {
        let mut roots = Vec::new();
        let mut seen = HashSet::new();
        for expression in expressions {
            dae::for_each_expression(view, expression, |projection, node| {
                if let dae::ExpressionOperation::Call { owner, .. } = node.operation()
                    && seen.insert(owner)
                {
                    roots.push((owner, projection));
                }
            });
        }
        let mut callees = HashMap::new();
        let mut predicate_ranges = HashMap::new();
        let mut predicate_count = 0usize;
        let mut assertions = Vec::new();
        for (owner, projection) in roots {
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
            assertions.extend(registered.assertions.iter().cloned());
            callees.insert(owner, registered);
        }
        Ok((callees, predicate_ranges, assertions))
    }
}

pub(in crate::lower) fn lower_model_event_transactions<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
) -> Result<Vec<solve::EventTransactionProgram>, LowerError> {
    let mut programs = Vec::new();
    let mut registry = layout.pure_calls.borrow_mut();
    for index in 0..view.model_event_transaction_count() {
        let id = view
            .model_event_transaction_id(index)
            .expect("dense checked model-event transaction identity resolves");
        let transaction = view
            .model_event_transaction(id)
            .expect("checked model-event transaction identity resolves");
        let Some((clock, definitions, statement_count)) =
            eligible_event_transaction(view, transaction)
        else {
            continue;
        };
        let provenance = transaction.provenance().span();
        let coordinate_types = collect_model_coordinates(view, &definitions, provenance)?;
        let solve_clock = clocks.clock(clock)?;
        let (site, assertions) = registry
            .add_event_transaction_owner(view, &definitions, &coordinate_types, provenance)
            .map_err(|error| LowerError::contract(error.to_string(), provenance))?;
        let assertions = assertions
            .into_iter()
            .map(|assertion| event_transaction_assertion(view, assertion, solve_clock, provenance))
            .collect::<Result<Vec<_>, _>>()?;
        let inputs = transaction_inputs(view, layout, &coordinate_types, provenance)?;
        let targets = transaction_targets(view, layout, transaction, &definitions, provenance)?;
        let provenance_span = provenance
            .require_provenance("model-event transaction")
            .map_err(|error| LowerError::contract(error.to_string(), provenance))?;
        programs.push(solve::EventTransactionProgram::checked(
            site,
            inputs,
            targets,
            assertions,
            statement_count,
            Some(solve_clock),
            provenance_span,
        )?);
    }
    Ok(programs)
}

fn transaction_inputs<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    coordinate_types: &[(ModelCoordinateKey<'dae>, dae::ValueTypeId<'dae>)],
    provenance: rumoca_core::Span,
) -> Result<Vec<(solve::ScalarSlot, solve::SolveValueType)>, LowerError> {
    coordinate_types
        .iter()
        .map(|(key, value_type)| {
            Ok((
                model_coordinate_source(view, layout, *key, provenance)?,
                lower_primitive_type(view, *value_type, arithmetic_profile())
                    .map_err(|error| LowerError::contract(error.to_string(), provenance))?,
            ))
        })
        .collect()
}

fn transaction_targets<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    transaction: dae::ModelEventTransactionView<'dae>,
    definitions: &[(dae::ExprId<'dae>, dae::ValueTypeId<'dae>)],
    provenance: rumoca_core::Span,
) -> Result<Vec<(solve::ScalarSlot, solve::SolveValueType)>, LowerError> {
    transaction
        .targets()
        .zip(definitions)
        .map(|(target, (_, value_type))| {
            Ok((
                variable_scalar_slot(layout, target.variable(), 0, provenance)?,
                lower_primitive_type(view, *value_type, arithmetic_profile())
                    .map_err(|error| LowerError::contract(error.to_string(), provenance))?,
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
) -> Option<(
    dae::ClockId<'dae>,
    Vec<(dae::ExprId<'dae>, dae::ValueTypeId<'dae>)>,
    usize,
)> {
    let steps = transaction.steps().collect::<Vec<_>>();
    let clock = steps.first()?.clock()?;
    if steps.iter().any(|step| {
        step.clock() != Some(clock)
            || step.trigger() != step.guard()
            || !matches!(
                view.condition(step.trigger()).map(|condition| condition.operation()),
                Some(dae::ConditionOperation::Clock(owner)) if owner == clock
            )
    }) {
        return None;
    }
    let mut final_values = HashMap::new();
    for step in &steps {
        for definition in step.definitions() {
            final_values.insert(definition.target().variable(), definition.value());
        }
    }
    let definitions = transaction
        .targets()
        .map(|target| {
            let variable = view.variable(view.variable_id(target.variable() as usize)?)?;
            Some((
                *final_values.get(&target.variable())?,
                variable.value_type_id(),
            ))
        })
        .collect::<Option<Vec<_>>>()?;
    Some((clock, definitions, steps.len()))
}

fn collect_model_coordinates<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &[(dae::ExprId<'dae>, dae::ValueTypeId<'dae>)],
    provenance: rumoca_core::Span,
) -> Result<Vec<(ModelCoordinateKey<'dae>, dae::ValueTypeId<'dae>)>, LowerError> {
    let mut coordinates = HashMap::new();
    let mut mismatch = false;
    for (expression, _) in definitions {
        dae::for_each_expression(view, *expression, |_, node| {
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
