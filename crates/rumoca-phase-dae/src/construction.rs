mod algorithm;
mod algorithm_lowering;
mod analysis;
mod clocks;
mod conditions;
mod discrete_values;
mod enumeration_conversion;
mod equation_lowering;
mod equation_systems;
mod expression;
mod function_array_assembly;
mod function_body;
mod function_construction;
mod function_external;
mod function_record_assembly;
mod function_seeds;
mod function_shapes;
mod initial_discrete_values;
mod model_algorithm;
mod model_events;
mod multi_output_equations;
mod record_equation;
mod structured_body;
#[cfg(test)]
mod tests;
mod variable_construction;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use rumoca_core::{
    BuiltinFunction, Causality, ClockLattice, ClockRational, Expression, InstanceId, Literal,
    OpBinary, OpUnary, PeriodicClockSchedule, SourceMap, Span, StructuredIndexBinder,
    StructuredIndexDomain, Subscript, VarName, Variability,
};
use rumoca_eval_flat::constant::{EvalContext, Value as EvalValue, eval_expr};
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

use crate::{DaeConstructionProduct, ToDaeError};
use algorithm::{
    AlgorithmAssignment as LoweredAlgorithmAssignment, AlgorithmFunctionCall,
    AlgorithmStatementContext, lower_algorithm_assignment, lower_algorithm_function_call,
    lower_algorithm_tensor_loop, own_clocked_algorithm_targets,
};
use algorithm_lowering::{AlgorithmBaseEnvironment, ModelAlgorithmsRequest, lower_algorithms};
use analysis::function_statement_products::FunctionLoweringPlan as FunctionStatementPlan;
use analysis::{
    AlgorithmConditionProduct, AlgorithmRelationOwner, Analysis, AnalyzedModel, ClockPlan,
    ClockTransferPlans, ComprehensionKey, ComprehensionPlans, DelayPlan, DerivativePlans,
    DerivedParameterPlan, DiscreteValueAssignmentPlan, DiscreteValueTopologyPlan,
    DynamicTimeEventOperand, EquationPartition, EventAssignmentRoute, EventBlockPlan,
    EventElseProduct, EventLoweringProduct, EventStatementPlan, ExpressionEventPlan,
    ExpressionEventPlans, ExternalArgumentPlan, ExternalFunctionPlan, FunctionArrayAssemblyPlan,
    FunctionAssignmentPlan, FunctionConditionalTarget, FunctionIntegerReduction,
    FunctionLoopLowering, FunctionPlan, FunctionRecordAssemblyPlan, FunctionRecordCallAssemblyPlan,
    FunctionRecordFieldAssembly, FunctionRecordFieldAssemblyPlan, FunctionRecordFieldIdentity,
    FunctionStatementProduct, FunctionStatementSequence, FunctionValueSeed, HistoryOperatorPlans,
    ModelAlgorithmPlan, ModelAlgorithmSequence, ModelEquationRow, ModelEquationSequence,
    ModelEventFunctionCallPlan, ModelEventFunctionOutputPlan, ModelEventTensorLoopPlan,
    MultiOutputEquationPlan, PlannedRole, RecordArrayFieldPlan, RecordArrayFieldPlans,
    RecordEquationAggregateSide, RecordEquationCoordinate, RecordEquationFieldPlan,
    RecordEquationFieldValue, RecordEquationPlan, ResolvedFunctionRecordField, RuntimeVariableRole,
    SemiLinearRules, StructuredSource, WhenOccurrenceId, analyze, assigned_function_targets,
    discrete_value_assignment, effective_function_scalar_type, effective_variable_scalar_type,
    empty_array_bound_to_declaration, function_record_field_name, is_event_condition,
    is_inferred_clock_condition, is_whole_clock_coordinate, model_algorithm_targets,
    record_field_projections, specialized_comprehension_plan,
};
use clocks::{LoweredClocks, lower_clocked_value_owners, lower_clocks};
use conditions::{combine_conditions, lower_condition, negate_condition};
use discrete_values::{DiscreteValueOwnerHandle, DiscreteValueStaging};
use enumeration_conversion::{
    enumeration_conversion, enumeration_range_ordinals, enumeration_range_type,
    has_enumeration_range_bound, is_flat_enumeration_literal,
};
use equation_lowering::{
    DeferredModelEquationRows, EquationRows, EquationSource, equation_generation,
    equation_owner_provenance, lower_equations,
};
use equation_systems::{EquationSystemInputs, lower_equation_expression, lower_equation_systems};
use expression::{
    FunctionArrayUpdate, FunctionCallLowering, FunctionExpressionValues, LoweringSymbols,
    all_model_expressions, classify_function_call, derivative_reference, expression_children,
    expression_span, lower_array_update, lower_call_operands, lower_clocked_expression,
    lower_clocked_model_algorithm_expression, lower_expression, lower_expression_scoped,
    lower_function_array_update, lower_function_expression, lower_function_expression_scoped,
    lower_model_algorithm_expression, lower_scoped_model_algorithm_expression,
    planned_input_variability, require_span, variable_attribute_expressions,
};
use function_array_assembly::lower_function_array_assembly;
use function_body::{
    FunctionConditional, FunctionFold, GuardedFunctionReturn, TotalArrayDefinition,
    function_assignment_coordinate, function_value_coordinate, lower_function_conditional,
    lower_function_fold, lower_function_value_seed, lower_generated_boolean_assignment,
    lower_guarded_function_return, lower_integer_reduction, lower_total_function_array_definition,
};
use function_construction::{
    FunctionRecordStagedValue, FunctionRecordStagingAvailability, FunctionRecordStagingScope,
    FunctionRecordStagingValues, FunctionRegistry, FunctionRegistryInput, FunctionSymbols,
    advance_function_lowering_record_staging as advance_function_record_staging,
    construct_functions, function_value_type,
};
use function_external::define_external_function;
use function_record_assembly::{
    lower_function_loop_record_assembly, lower_function_record_assembly,
    lower_function_record_field_assembly, lower_function_record_value,
};
use function_seeds::{
    collect_function_sequence_seeds, lower_function_sequence_seeds, lower_named_function_seeds,
};
use function_shapes::{
    FunctionShapeAnalysis, FunctionShapeCertificate, FunctionSpecializationKey, ShapeEnvironment,
    ValueShape, call_free_expression_shape, call_free_target_shape, evaluate_shape_integer,
    infer_function_integer_bounds, proven_conditional_branch,
};
use model_algorithm::{
    ModelAlgorithmLowering, lower_declarative_model_algorithm,
    lower_separated_array_sum_model_algorithm, lower_total_array_model_algorithm,
};
use model_events::{WhenChainsRequest, always_condition, lower_when_assignment, lower_when_chains};
use multi_output_equations::lower_multi_output_equation;
use record_equation::{RecordEquationLowering, lower_record_equation};
use structured_body::{lower_structured_body, normalize_conditional_residual};
use variable_construction::{
    VariableConstructionPlan, VariableDefinitionContext, define_reserved_variables,
    insert_variable_identities, plan_variable_construction,
};

#[derive(Clone, Copy, PartialEq, Eq)]
enum Coordinate<'dae> {
    Parameter(dae::ParameterId<'dae>),
    Input(dae::InputId<'dae>),
    State(dae::StateId<'dae>),
    Algebraic(dae::AlgebraicId<'dae>),
    DiscreteReal(dae::DiscreteRealId<'dae>),
    DiscreteValue(dae::DiscreteValueId<'dae>),
    FunctionParameter(dae::FunctionParameterId<'dae>),
    FunctionValue(dae::FunctionValueId<'dae>),
}

impl<'dae> Coordinate<'dae> {
    fn current(self) -> dae::CoordinateInput<'dae> {
        match self {
            Self::Parameter(id) => dae::CoordinateInput::Parameter(id),
            Self::Input(id) => dae::CoordinateInput::Input(id),
            Self::State(id) => dae::CoordinateInput::State(id),
            Self::Algebraic(id) => dae::CoordinateInput::Algebraic(id),
            Self::DiscreteReal(id) => dae::CoordinateInput::DiscreteReal(id),
            Self::DiscreteValue(id) => dae::CoordinateInput::DiscreteValue(id),
            Self::FunctionParameter(id) => dae::CoordinateInput::FunctionParameter(id),
            Self::FunctionValue(_) => {
                unreachable!("function values require their semantic body owner")
            }
        }
    }

    /// MLS §3.7.5 `pre(v)`: the left limit `v(t^pre)` at event entry.
    ///
    /// Discrete coordinates keep their event history in the discrete pre lane.
    /// A continuous state or algebraic gets its own event-entry snapshot lane,
    /// which is what makes `y = f*pre(x); reinit(x, 0)` in one when-body read
    /// the accumulated `x` rather than the reinitialized one. Analysis proves
    /// the read sits in a when-clause before this constructor runs.
    fn previous(self, span: Span) -> Result<dae::CoordinateInput<'dae>, ToDaeError> {
        match self {
            Self::DiscreteReal(id) => Ok(dae::CoordinateInput::PreDiscreteReal(id)),
            Self::DiscreteValue(id) => Ok(dae::CoordinateInput::PreDiscreteValue(id)),
            Self::State(id) => Ok(dae::CoordinateInput::PreState(id)),
            Self::Algebraic(id) => Ok(dae::CoordinateInput::PreAlgebraic(id)),
            Self::Parameter(_)
            | Self::Input(_)
            | Self::FunctionParameter(_)
            | Self::FunctionValue(_) => Err(ToDaeError::unsupported_flat(
                "pre expression",
                "pre(...) must name a discrete or continuous variable coordinate in canonical DAE",
                span,
            )),
        }
    }
}

/// A coordinate whose discrete event role was fixed when model coordinates
/// were issued.
///
/// This view is stored beside the general coordinate inventory. Event
/// lowering can therefore consume a closed carrier instead of repeatedly
/// checking that an arbitrary coordinate has one of the two event roles.
#[derive(Clone, Copy)]
enum EventCoordinate<'dae> {
    Real(dae::DiscreteRealId<'dae>),
    Value(dae::DiscreteValueId<'dae>),
}

#[derive(Clone, Copy)]
enum ReadableCoordinate<'dae> {
    Parameter(dae::ParameterId<'dae>),
    Input(dae::InputId<'dae>),
    State(dae::StateId<'dae>),
    Algebraic(dae::AlgebraicId<'dae>),
    DiscreteReal(dae::DiscreteRealId<'dae>),
    DiscreteValue(dae::DiscreteValueId<'dae>),
}

impl<'dae> ReadableCoordinate<'dae> {
    fn from_coordinate(coordinate: Coordinate<'dae>) -> Option<Self> {
        match coordinate {
            Coordinate::Parameter(id) => Some(Self::Parameter(id)),
            Coordinate::Input(id) => Some(Self::Input(id)),
            Coordinate::State(id) => Some(Self::State(id)),
            Coordinate::Algebraic(id) => Some(Self::Algebraic(id)),
            Coordinate::DiscreteReal(id) => Some(Self::DiscreteReal(id)),
            Coordinate::DiscreteValue(id) => Some(Self::DiscreteValue(id)),
            Coordinate::FunctionParameter(_) | Coordinate::FunctionValue(_) => None,
        }
    }

    fn current(self) -> dae::CoordinateInput<'dae> {
        match self {
            Self::Parameter(id) => dae::CoordinateInput::Parameter(id),
            Self::Input(id) => dae::CoordinateInput::Input(id),
            Self::State(id) => dae::CoordinateInput::State(id),
            Self::Algebraic(id) => dae::CoordinateInput::Algebraic(id),
            Self::DiscreteReal(id) => dae::CoordinateInput::DiscreteReal(id),
            Self::DiscreteValue(id) => dae::CoordinateInput::DiscreteValue(id),
        }
    }
}

impl<'dae> EventCoordinate<'dae> {
    fn from_coordinate(coordinate: Coordinate<'dae>) -> Option<Self> {
        match coordinate {
            Coordinate::DiscreteReal(id) => Some(Self::Real(id)),
            Coordinate::DiscreteValue(id) => Some(Self::Value(id)),
            _ => None,
        }
    }

    fn coordinate(self) -> Coordinate<'dae> {
        match self {
            Self::Real(id) => Coordinate::DiscreteReal(id),
            Self::Value(id) => Coordinate::DiscreteValue(id),
        }
    }

    fn previous(self) -> dae::CoordinateInput<'dae> {
        match self {
            Self::Real(id) => dae::CoordinateInput::PreDiscreteReal(id),
            Self::Value(id) => dae::CoordinateInput::PreDiscreteValue(id),
        }
    }

    fn target(self) -> dae::ModelEventTarget<'dae> {
        match self {
            Self::Real(id) => dae::ModelEventTarget::DiscreteReal(id),
            Self::Value(id) => dae::ModelEventTarget::DiscreteValue(id),
        }
    }
}

struct ModelCoordinates<'dae> {
    by_name: HashMap<VarName, Coordinate<'dae>>,
    by_instance: HashMap<rumoca_core::InstanceId, Coordinate<'dae>>,
    state_by_occurrence: HashMap<rumoca_core::SourceOccurrenceId, dae::StateId<'dae>>,
    by_occurrence: HashMap<rumoca_eval_flat::constant::ResolvedOccurrenceKey, Coordinate<'dae>>,
    event_by_name: HashMap<VarName, EventCoordinate<'dae>>,
    event_by_occurrence:
        HashMap<rumoca_eval_flat::constant::ResolvedOccurrenceKey, EventCoordinate<'dae>>,
    readable_by_occurrence:
        HashMap<rumoca_eval_flat::constant::ResolvedOccurrenceKey, ReadableCoordinate<'dae>>,
}

impl<'dae> ModelCoordinates<'dae> {
    fn new() -> Self {
        Self {
            by_name: HashMap::new(),
            by_instance: HashMap::new(),
            state_by_occurrence: HashMap::new(),
            by_occurrence: HashMap::new(),
            event_by_name: HashMap::new(),
            event_by_occurrence: HashMap::new(),
            readable_by_occurrence: HashMap::new(),
        }
    }

    fn insert(
        &mut self,
        variable: &flat::Variable,
        coordinate: Coordinate<'dae>,
    ) -> Result<(), dae::DaeConstructionError> {
        let reference = variable.component_ref.as_ref().ok_or(
            dae::DaeConstructionError::InvalidExpressionForm {
                span: variable.source_span,
            },
        )?;
        let occurrence = rumoca_eval_flat::constant::ResolvedOccurrenceKey {
            instance_id: variable.instance_id,
            root_def_id: reference.root_def_id(),
        };
        if self.by_occurrence.contains_key(&occurrence) {
            return Err(dae::DaeConstructionError::DuplicateKey {
                kind: "runtime variable occurrence",
                key: format!("{}:{}", occurrence.instance_id, occurrence.root_def_id),
                span: variable.source_span,
            });
        }
        if self.by_name.contains_key(&variable.name) {
            return Err(dae::DaeConstructionError::DuplicateKey {
                kind: "runtime variable name",
                key: variable.name.to_string(),
                span: variable.source_span,
            });
        }
        if self.by_instance.contains_key(&variable.instance_id) {
            return Err(dae::DaeConstructionError::DuplicateKey {
                kind: "runtime variable instance",
                key: variable.instance_id.to_string(),
                span: variable.source_span,
            });
        }
        let event = EventCoordinate::from_coordinate(coordinate);
        let readable = ReadableCoordinate::from_coordinate(coordinate);
        self.by_name.insert(variable.name.clone(), coordinate);
        self.by_instance.insert(variable.instance_id, coordinate);
        if let Coordinate::State(state) = coordinate {
            let source_occurrence = rumoca_core::SourceOccurrenceId::try_from(variable.instance_id)
                .map_err(|_| dae::DaeConstructionError::InvalidExpressionForm {
                    span: variable.source_span,
                })?;
            self.state_by_occurrence.insert(source_occurrence, state);
        }
        self.by_occurrence.insert(occurrence, coordinate);
        if let Some(event) = event {
            self.event_by_name.insert(variable.name.clone(), event);
            self.event_by_occurrence.insert(occurrence, event);
        }
        if let Some(readable) = readable {
            self.readable_by_occurrence.insert(occurrence, readable);
        }
        Ok(())
    }

    fn by_instance(&self) -> &HashMap<rumoca_core::InstanceId, Coordinate<'dae>> {
        &self.by_instance
    }

    fn state_by_occurrence(&self) -> &HashMap<rumoca_core::SourceOccurrenceId, dae::StateId<'dae>> {
        &self.state_by_occurrence
    }

    fn event(
        &self,
        name: &VarName,
        span: Span,
    ) -> Result<EventCoordinate<'dae>, dae::DaeConstructionError> {
        self.event_by_name.get(name).copied().ok_or_else(|| {
            dae::DaeConstructionError::InvalidVariableRole {
                name: name.clone(),
                span,
            }
        })
    }

    fn event_occurrence(
        &self,
        identity: rumoca_eval_flat::constant::ResolvedOccurrenceKey,
        name: &VarName,
        span: Span,
    ) -> Result<EventCoordinate<'dae>, dae::DaeConstructionError> {
        self.event_by_occurrence
            .get(&identity)
            .copied()
            .ok_or_else(|| dae::DaeConstructionError::InvalidVariableRole {
                name: name.clone(),
                span,
            })
    }

    fn readable_occurrence(
        &self,
        identity: rumoca_eval_flat::constant::ResolvedOccurrenceKey,
        name: &VarName,
        span: Span,
    ) -> Result<ReadableCoordinate<'dae>, dae::DaeConstructionError> {
        self.readable_by_occurrence
            .get(&identity)
            .copied()
            .ok_or_else(|| dae::DaeConstructionError::InvalidVariableRole {
                name: name.clone(),
                span,
            })
    }
}

impl<'dae> std::ops::Deref for ModelCoordinates<'dae> {
    type Target = HashMap<VarName, Coordinate<'dae>>;

    fn deref(&self) -> &Self::Target {
        &self.by_name
    }
}

struct ReservedVariable<'flat, 'dae> {
    flat: &'flat flat::Variable,
    role: RuntimeVariableRole,
    scalar_type: dae::ScalarType,
    value_type: dae::ValueTypeId<'dae>,
    definition: dae::VariableReservation<'dae>,
}

pub(crate) fn construct(
    flat: &flat::Model,
    source_map: SourceMap,
) -> Result<DaeConstructionProduct, ToDaeError> {
    let analyzed = analyze(flat)?.with_semi_linear_rules();
    if !flat.is_partial && !analyzed.analysis.balance.is_balanced() {
        return Err(ToDaeError::unbalanced_from_detail(
            analyzed.analysis.balance.clone(),
        ));
    }
    let balance_detail = analyzed.analysis.balance.clone();
    let variable_plan = plan_variable_construction(flat, &analyzed.analysis)?;

    let dae = dae::Dae::construct(source_map, move |construction| {
        build_checked(analyzed, variable_plan, construction)
    })
    .map_err(ToDaeError::from)?;
    Ok(DaeConstructionProduct::new(dae, balance_detail))
}

fn build_checked<'dae>(
    analyzed: AnalyzedModel<'_>,
    variable_plan: VariableConstructionPlan,
    construction: &mut dae::DaeConstruction<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let AnalyzedModel {
        analysis,
        model_algorithms,
        model_equations,
    } = analyzed;
    let flat = model_algorithms.flat();
    let analysis = &analysis;
    if let Some(declaration) = flat.predefined_string_declaration {
        construction.register_predefined_string(declaration)?;
    }
    let value_types = reserve_value_types(flat, analysis, construction)?;
    let clocks = lower_analysis_clocks(construction, flat, analysis)?;
    let no_function_ids = HashMap::new();
    let no_coordinate_instances = HashMap::new();
    let no_state_instances = HashMap::new();
    let analysis_functions = model_function_registry(
        flat,
        analysis,
        &no_function_ids,
        &no_coordinate_instances,
        &no_state_instances,
        &clocks,
    );
    let variable_identities = insert_variable_identities(
        flat,
        analysis,
        construction,
        &value_types,
        &analysis_functions,
        variable_plan,
    )?;
    let coordinates = variable_identities.coordinates;
    let function_ids = construct_functions(
        flat,
        &analysis.function_shapes,
        construction,
        &coordinates,
        FunctionRegistryInput {
            flat,
            comprehension_plans: &analysis.comprehension_plans,
            record_array_fields: &analysis.record_array_fields,
            constants: &analysis.constants,
            delay_plans: &analysis.delay_plans,
            derivatives: &analysis.derivatives,
            history_operators: &analysis.history_operators,
            coordinate_instances: coordinates.by_instance(),
            state_occurrences: coordinates.state_by_occurrence(),
            expression_events: &analysis.expression_events,
            sample_alias_schedules: &analysis.sample_alias_schedules,
            clock_transfer_plans: &analysis.clock_transfer_plans,
            clocks: &clocks,
        },
        &analysis.function_plans,
    )?;
    let functions = model_function_registry(
        flat,
        analysis,
        &function_ids,
        coordinates.by_instance(),
        coordinates.state_by_occurrence(),
        &clocks,
    );
    define_reserved_variables(
        construction,
        VariableDefinitionContext {
            flat,
            coordinates: &coordinates,
            functions: &functions,
            assigned_discrete_targets: &analysis.assigned_discrete_targets,
            derived_parameters: &analysis.derived_parameters,
            initial_parameters: &analysis.initial_parameters,
        },
        variable_identities.reserved,
    )?;
    lower_clocked_value_owners(
        construction,
        flat,
        &coordinates,
        &analysis.clocked_value_owners,
        &clocks,
    )?;
    lower_checked_model_content(
        construction,
        analysis,
        &model_algorithms,
        model_equations,
        &coordinates,
        &functions,
        &clocks,
    )
}

/// Lower the model's content once every identity it refers to exists.
///
/// Split out of `build_checked` so the preparation phase and the lowering phase
/// are separately readable. `flat` is derived from `model_algorithms` here
/// rather than passed alongside it, so the two cannot disagree about which
/// model is being lowered.
fn lower_checked_model_content<'scope, 'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    analysis: &'scope Analysis<'scope>,
    model_algorithms: &'scope ModelAlgorithmSequence<'scope>,
    model_equations: ModelEquationSequence<'scope>,
    coordinates: &ModelCoordinates<'dae>,
    functions: &FunctionRegistry<'scope, 'dae>,
    clocks: &LoweredClocks<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let flat = model_algorithms.flat();
    let mut discrete_values = DiscreteValueStaging::new();
    lower_bindings(
        construction,
        &mut discrete_values,
        coordinates,
        functions,
        BindingsRequest {
            roles: &analysis.roles,
            topology: &analysis.discrete_value_topology,
            flat,
            coordinate_owners: &analysis.clocked_coordinate_owners,
            clocks,
        },
    )?;
    lower_model_owners(
        construction,
        ModelOwnersRequest {
            flat,
            analysis,
            model_algorithms,
            model_equations,
            coordinates,
            functions,
            clocks,
            discrete_values,
        },
    )?;
    lower_scheduled_time_events(construction, &analysis.expression_events)
}

fn model_function_registry<'scope, 'dae>(
    flat: &'scope flat::Model,
    analysis: &'scope Analysis<'scope>,
    ids: &'scope HashMap<FunctionSpecializationKey, dae::FunctionId<'dae>>,
    coordinate_instances: &'scope HashMap<InstanceId, Coordinate<'dae>>,
    state_occurrences: &'scope HashMap<rumoca_core::SourceOccurrenceId, dae::StateId<'dae>>,
    clocks: &'scope LoweredClocks<'dae>,
) -> FunctionRegistry<'scope, 'dae> {
    FunctionRegistry {
        flat,
        shapes: &analysis.function_shapes,
        ids,
        comprehension_plans: &analysis.comprehension_plans,
        record_array_fields: &analysis.record_array_fields,
        constants: &analysis.constants,
        delay_plans: &analysis.delay_plans,
        derivatives: &analysis.derivatives,
        history_operators: &analysis.history_operators,
        coordinate_instances,
        state_occurrences,
        expression_events: &analysis.expression_events,
        sample_alias_schedules: &analysis.sample_alias_schedules,
        clock_transfer_plans: &analysis.clock_transfer_plans,
        clocks,
    }
}

fn lower_analysis_clocks<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    flat: &flat::Model,
    analysis: &Analysis<'_>,
) -> Result<LoweredClocks<'dae>, dae::DaeConstructionError> {
    lower_clocks(
        construction,
        flat,
        &analysis.clock_plans,
        &analysis.clocked_value_owners,
        &analysis.clock_transfer_plans,
        analysis
            .expression_events
            .ordered()
            .filter_map(|(span, plan)| {
                let ExpressionEventPlan::SampleClock(schedule) = plan else {
                    return None;
                };
                Some((schedule, span))
            }),
    )
}

/// Build the MLS §8.5 time events proven by expression analysis.
///
/// A relation over `time` alone has an exactly known crossing instant, so it
/// is scheduled rather than searched for by a root function.
fn lower_scheduled_time_events<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    events: &ExpressionEventPlans,
) -> Result<(), dae::DaeConstructionError> {
    for (span, plan) in events.ordered() {
        let ExpressionEventPlan::TimeEvent(instant) = plan else {
            continue;
        };
        let provenance = dae::DaeProvenance::source(span)?;
        construction.events(|owners| owners.time_event(instant, provenance))?;
    }
    Ok(())
}

struct ModelOwnersRequest<'scope, 'dae> {
    flat: &'scope flat::Model,
    analysis: &'scope Analysis<'scope>,
    model_algorithms: &'scope ModelAlgorithmSequence<'scope>,
    model_equations: ModelEquationSequence<'scope>,
    coordinates: &'scope ModelCoordinates<'dae>,
    functions: &'scope FunctionRegistry<'scope, 'dae>,
    clocks: &'scope LoweredClocks<'dae>,
    discrete_values: DiscreteValueStaging<'dae>,
}

fn lower_model_owners<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    request: ModelOwnersRequest<'_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let ModelOwnersRequest {
        flat,
        analysis,
        model_algorithms,
        model_equations,
        coordinates,
        functions,
        clocks,
        mut discrete_values,
    } = request;
    lower_equation_systems(
        construction,
        &mut discrete_values,
        &EquationSystemInputs::new(flat, analysis, coordinates, functions, clocks),
        model_equations,
    )?;
    initial_discrete_values::lower_initial_discrete_values(
        construction,
        coordinates,
        functions,
        analysis,
    )?;
    lower_assertions(
        construction,
        coordinates,
        functions,
        &analysis.sample_lattices,
        flat.assert_equations
            .iter()
            .chain(&flat.initial_assert_equations)
            .chain(&analysis.initial_algorithm_assertions),
    )?;
    lower_algorithms(
        construction,
        &mut discrete_values,
        ModelAlgorithmsRequest {
            environment: AlgorithmBaseEnvironment {
                coordinates,
                functions,
                sample_lattices: &analysis.sample_lattices,
            },
            algorithms: model_algorithms,
            topology: &analysis.discrete_value_topology,
        },
    )?;
    lower_when_chains(
        construction,
        &mut discrete_values,
        WhenChainsRequest::new(
            coordinates,
            functions,
            &analysis.sample_lattices,
            clocks,
            &flat.when_chains,
            &analysis.discrete_value_topology,
            &analysis.clocked_when_owners,
        ),
    )?;
    discrete_values.add_holds(construction, coordinates, &analysis.discrete_value_topology)?;
    discrete_values.finish(construction, &analysis.discrete_value_topology)
}

fn reserve_value_types<'dae>(
    flat: &flat::Model,
    analysis: &Analysis<'_>,
    construction: &mut dae::DaeConstruction<'dae>,
) -> Result<HashMap<VarName, dae::ValueTypeId<'dae>>, dae::DaeConstructionError> {
    let mut value_types = HashMap::new();
    for (name, variable) in &flat.variables {
        let role =
            analysis
                .roles
                .get(name)
                .ok_or(dae::DaeConstructionError::InvalidExpressionForm {
                    span: variable.source_span,
                })?;
        if matches!(role, PlannedRole::Clock) {
            continue;
        }
        let provenance = dae::DaeProvenance::source(variable.source_span)?;
        let scalar = effective_variable_scalar_type(flat, variable).ok_or(
            dae::DaeConstructionError::InvalidExpressionForm {
                span: variable.source_span,
            },
        )?;
        let dimensions = variable
            .dims
            .iter()
            .map(|extent| checked_source_array_extent(*extent, variable.source_span))
            .collect::<Result<Vec<_>, _>>()?;
        let value_type = construction.types(|types| {
            types.intern(
                variable.type_id,
                dae::ValueType::array(scalar, dimensions),
                provenance,
            )
        })?;
        value_types.insert(name.clone(), value_type);
    }
    Ok(value_types)
}

fn checked_source_array_extent(extent: i64, span: Span) -> Result<u32, dae::DaeConstructionError> {
    if extent < 0 {
        return Err(dae::DaeConstructionError::InvalidArrayExtent { span });
    }
    u32::try_from(extent)
        .map_err(|_| dae::DaeConstructionError::SourceArrayExtentOverflow { extent, span })
}

fn lower_function_statements<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    sequence: &FunctionStatementSequence,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let mut record_staging_available = symbols.record_staging_available.clone();
    for product in sequence.products() {
        let statements = product.source();
        let plan = product.plan();
        let current_symbols = symbols.with_record_staging_available(&record_staging_available);
        if let FunctionStatementPlan::ArrayAssembly(assembly) = plan {
            lower_function_array_assembly(
                construction,
                current_symbols,
                &mut body,
                statements,
                assembly,
            )?;
            record_staging_available.advance(plan);
            continue;
        }
        if let FunctionStatementPlan::RecordAssembly(assembly) = plan {
            lower_function_record_assembly(
                construction,
                current_symbols,
                &mut body,
                statements,
                assembly,
            )?;
            record_staging_available.advance(plan);
            continue;
        }
        if let FunctionStatementPlan::RecordFieldAssembly(assembly) = plan {
            lower_function_record_field_assembly(
                construction,
                current_symbols,
                &mut body,
                statements,
                assembly,
            )?;
            record_staging_available.advance(plan);
            continue;
        }
        body = lower_function_statement(construction, current_symbols, body, product)?;
        record_staging_available.advance(plan);
    }
    Ok(body)
}

fn lower_function_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    product: &FunctionStatementProduct,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let plan = product.plan();
    match (product.source(), plan) {
        (_, FunctionStatementPlan::ProvenAssertion) => Ok(body),
        ([_], FunctionStatementPlan::RuntimeAssertion) => {
            lower_issued_runtime_assertion(construction, symbols, body, product)
        }
        (
            [_],
            FunctionStatementPlan::GeneratedBooleanAssignment {
                target,
                value,
                span,
                ..
            },
        ) => lower_generated_boolean_assignment(construction, symbols, body, target, value, *span),
        (
            [rumoca_core::Statement::Assignment { value, span, .. }],
            FunctionStatementPlan::Assignment(plan),
        ) => {
            lower_function_assignment(
                construction,
                symbols,
                &mut body,
                FunctionAssignment {
                    value,
                    span: *span,
                    plan,
                },
            )?;
            Ok(body)
        }
        (
            [_],
            FunctionStatementPlan::For {
                domain,
                binder_spans,
                lowering,
                statements,
                indices,
                span,
            },
        ) => lower_function_loop(
            construction,
            symbols,
            body,
            FunctionLoop {
                indices,
                span: *span,
                domain,
                binder_spans,
                lowering,
                statements,
            },
        ),
        (
            [statement @ rumoca_core::Statement::If { .. }],
            FunctionStatementPlan::If { .. } | FunctionStatementPlan::ProvenBranch { .. },
        ) => lower_function_conditional_statement(
            construction,
            symbols,
            body,
            statement,
            plan,
            product.span(),
        ),
        (
            [
                rumoca_core::Statement::FunctionCall {
                    comp, args, span, ..
                },
            ],
            FunctionStatementPlan::MultiOutputCall { outputs },
        ) => lower_multi_output_statement(construction, symbols, body, comp, args, *span, outputs),
        (
            [
                rumoca_core::Statement::FunctionCall {
                    comp, args, span, ..
                },
            ],
            FunctionStatementPlan::RecordMultiOutputAssembly(plan),
        ) => lower_record_multi_output_statement(
            construction,
            symbols,
            body,
            comp,
            args,
            *span,
            plan,
        ),
        _ => Err(dae::DaeConstructionError::InvalidExpressionForm {
            span: product.span(),
        }),
    }
}

fn lower_issued_runtime_assertion<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: dae::FunctionBody<'dae>,
    product: &FunctionStatementProduct,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let assertion =
        product
            .runtime_assertion()
            .ok_or(dae::DaeConstructionError::InvalidExpressionForm {
                span: product.span(),
            })?;
    lower_runtime_function_assertion(
        construction,
        symbols,
        body,
        &assertion.condition,
        &assertion.message,
        assertion.span,
    )
}

fn lower_multi_output_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    callee: &rumoca_core::Reference,
    args: &[Expression],
    span: Span,
    outputs: &[Option<FunctionAssignmentPlan>],
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let call = FunctionMultiOutputCall {
        callee,
        args,
        span,
        outputs,
    };
    lower_function_multi_output_call(construction, symbols, &mut body, call)?;
    Ok(body)
}

fn lower_runtime_function_assertion<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    condition: &Expression,
    message: &Expression,
    span: Span,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let condition = lower_function_expression(
        construction,
        symbols.coordinates,
        Some(symbols.record_staging_scope()),
        symbols.functions,
        symbols.shapes,
        &body,
        condition,
    )?;
    let message = lower_function_expression(
        construction,
        symbols.coordinates,
        Some(symbols.record_staging_scope()),
        symbols.functions,
        symbols.shapes,
        &body,
        message,
    )?;
    let provenance = dae::DaeProvenance::source(span)?;
    construction
        .functions(|functions| functions.assertion(&mut body, condition, message, provenance))?;
    Ok(body)
}

fn lower_record_multi_output_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    callee: &rumoca_core::Reference,
    arguments: &[Expression],
    span: Span,
    plan: &FunctionRecordCallAssemblyPlan,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    lower_function_record_multi_output_assembly(
        construction,
        symbols,
        &mut body,
        callee,
        arguments,
        span,
        plan,
    )?;
    Ok(body)
}

/// Lower one MLS §11.5 conditional statement of a function body.
///
/// The conditional reaches the DAE either as its own branches, or — when
/// analysis settled every condition this specialization evaluates — as the
/// unconditional sequence the executed branch denotes, in which case no
/// condition reaches the DAE at all.
fn lower_function_conditional_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    statement: &rumoca_core::Statement,
    plan: &FunctionStatementPlan,
    source_span: Span,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let _ = statement;
    match plan {
        FunctionStatementPlan::If {
            conditions,
            branches,
            fallback,
            targets,
            span,
        } => {
            let binders = HashMap::new();
            lower_function_conditional(
                construction,
                &mut body,
                FunctionConditional {
                    symbols,
                    binders: &binders,
                    conditions,
                    branches,
                    fallback: fallback.as_ref(),
                    targets,
                    span: *span,
                },
            )?;
            Ok(body)
        }
        FunctionStatementPlan::ProvenBranch { statements, .. } => {
            lower_function_statements(construction, symbols, body, statements)
        }
        _ => Err(dae::DaeConstructionError::InvalidExpressionForm { span: source_span }),
    }
}

struct FunctionAssignment<'statement> {
    value: &'statement Expression,
    span: Span,
    plan: &'statement FunctionAssignmentPlan,
}

struct FunctionMultiOutputCall<'statement> {
    callee: &'statement rumoca_core::Reference,
    args: &'statement [Expression],
    span: Span,
    outputs: &'statement [Option<FunctionAssignmentPlan>],
}

/// Lower one MLS §11.2.1.1 multi-result call statement.
///
/// The call's arguments are lowered once and every received result is committed
/// as one atomic assignment group. The checked group retains the MLS §12.4.3
/// proof that all result projections belong to one call evaluation; a backend
/// may therefore materialize the call once without inferring atomicity from
/// spans or expression identity.
fn lower_function_multi_output_call<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    call: FunctionMultiOutputCall<'_>,
) -> Result<(), dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::source(call.span)?;
    let binders = HashMap::new();
    let operands = lower_call_operands(
        construction,
        LoweringSymbols {
            coordinates: symbols.coordinates,
            record_staging: Some(symbols.record_staging_scope()),
            functions: symbols.functions,
            shapes: symbols.shapes,
            function_body: Some(body),
            values: None,
            owner_clock: None,
        },
        &binders,
        call.callee,
        call.args,
        provenance,
    )?;
    let selected = call
        .outputs
        .iter()
        .enumerate()
        .filter_map(|(ordinal, plan)| plan.as_ref().map(|plan| (ordinal, plan)))
        .collect::<Vec<_>>();
    let results = operands.results(
        construction,
        selected.iter().map(|(ordinal, _)| *ordinal),
        provenance,
    )?;
    let mut assignments = Vec::with_capacity(selected.len());
    for ((_, plan), mut value) in selected.into_iter().zip(results) {
        let target = function_assignment_coordinate(symbols, plan, call.span)?;
        if !plan.subscripts().is_empty() {
            let base = plan
                .seed()
                .map(|seed| lower_function_value_seed(construction, seed, call.span))
                .transpose()?;
            value = lower_function_array_update(
                construction,
                FunctionArrayUpdate {
                    symbols: LoweringSymbols {
                        coordinates: symbols.coordinates,
                        record_staging: Some(symbols.record_staging_scope()),
                        functions: symbols.functions,
                        shapes: symbols.shapes,
                        function_body: Some(body),
                        values: None,
                        owner_clock: None,
                    },
                    binders: &binders,
                    base,
                    target,
                    subscripts: plan.subscripts(),
                    value,
                    provenance,
                },
            )?;
        }
        assignments.push((target, value));
    }
    construction.functions(|owner| owner.assign_all(body, &assignments, provenance))
}

fn lower_function_record_multi_output_assembly<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    callee: &rumoca_core::Reference,
    args: &[Expression],
    span: Span,
    plan: &FunctionRecordCallAssemblyPlan,
) -> Result<(), dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::source(span)?;
    let operands = lower_call_operands(
        construction,
        LoweringSymbols {
            coordinates: symbols.coordinates,
            record_staging: Some(symbols.record_staging_scope()),
            functions: symbols.functions,
            shapes: symbols.shapes,
            function_body: Some(body),
            values: None,
            owner_clock: None,
        },
        &HashMap::new(),
        callee,
        args,
        provenance,
    )?;
    let fields = plan
        .fields
        .iter()
        .map(|field| operands.result(construction, field.result_ordinal, provenance))
        .collect::<Result<Vec<_>, _>>()?;
    let target = function_value_coordinate(symbols.coordinates, &plan.target, span)?;
    let value_type =
        construction.functions(|functions| functions.value_type(target, provenance))?;
    construction.types(|types| {
        types.expect_record_layout(
            value_type,
            plan.fields.iter().map(|field| field.name.clone()),
            provenance,
        )
    })?;
    let record = construction
        .expressions(|expressions| expressions.at(provenance).record(value_type, fields))?;
    construction.functions(|functions| functions.assign(body, target, record, provenance))
}

fn lower_function_assignment<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    assignment: FunctionAssignment<'_>,
) -> Result<(), dae::DaeConstructionError> {
    let target = function_assignment_coordinate(symbols, assignment.plan, assignment.span)?;
    let mut value = lower_function_expression(
        construction,
        symbols.coordinates,
        Some(symbols.record_staging_scope()),
        symbols.functions,
        symbols.shapes,
        body,
        assignment.value,
    )?;
    let provenance = dae::DaeProvenance::source(assignment.span)?;
    let subscripts = assignment.plan.subscripts();
    if !subscripts.is_empty() {
        let binders = HashMap::new();
        let mut base = None;
        if let Some(seed) = assignment.plan.seed() {
            let seeded = lower_function_value_seed(construction, seed, assignment.span)?;
            base = Some(seeded);
        }
        value = lower_function_array_update(
            construction,
            FunctionArrayUpdate {
                symbols: LoweringSymbols {
                    coordinates: symbols.coordinates,
                    record_staging: Some(symbols.record_staging_scope()),
                    functions: symbols.functions,
                    shapes: symbols.shapes,
                    function_body: Some(body),
                    values: None,
                    owner_clock: None,
                },
                binders: &binders,
                base,
                target,
                subscripts,
                value,
                provenance,
            },
        )?;
    }
    construction.functions(|owner| owner.assign(body, target, value, provenance))
}

struct FunctionLoop<'statement> {
    indices: &'statement [rumoca_core::ForIndex],
    span: Span,
    domain: &'statement StructuredIndexDomain,
    binder_spans: &'statement [Span],
    lowering: &'statement FunctionLoopLowering,
    statements: &'statement FunctionStatementSequence,
}

fn lower_function_loop<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    input: FunctionLoop<'_>,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let owner = dae::DaeProvenance::source(input.span)?;
    let domain_provenance = match input.binder_spans {
        [span] => dae::DaeProvenance::source(*span)?,
        _ => owner,
    };
    let domain = construction
        .domains(|domains| domains.structured(input.domain.clone(), domain_provenance))?;
    let indices = input.indices.iter().collect::<Vec<_>>();
    let binders = lower_function_binders(construction, domain, &indices, input.binder_spans)?;
    let mut loop_shapes = symbols.shapes.clone();
    for binder in binders.keys() {
        // A loop binder is a scalar whose value varies over the iteration, so
        // it shadows any enclosing coordinate's proven value (MLS §11.2.2).
        loop_shapes.insert(binder.clone(), Vec::new());
    }
    let loop_symbols = FunctionSymbols {
        coordinates: symbols.coordinates,
        function_values: symbols.function_values,
        record_staging: symbols.record_staging,
        record_staging_available: symbols.record_staging_available,
        functions: symbols.functions,
        shapes: &loop_shapes,
    };
    match input.lowering {
        FunctionLoopLowering::TotalArrayDefinition => {
            body = lower_total_function_array_definition(
                construction,
                body,
                TotalArrayDefinition {
                    symbols: loop_symbols,
                    domain,
                    binders: &binders,
                    statements: input.statements,
                    owner,
                },
            )?;
            Ok(body)
        }
        FunctionLoopLowering::Fold {
            targets,
            iteration_locals,
        } => lower_function_fold(
            construction,
            loop_symbols,
            body,
            FunctionFold {
                domain,
                binders: &binders,
                statements: input.statements,
                targets,
                iteration_locals,
                owner,
            },
        ),
    }
}

fn lower_function_binders<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    domain: dae::DomainId<'dae>,
    indices: &[&rumoca_core::ForIndex],
    spans: &[Span],
) -> Result<HashMap<VarName, dae::DomainBinderId<'dae>>, dae::DaeConstructionError> {
    let mut binders = HashMap::with_capacity(indices.len());
    for (ordinal, (index, span)) in indices.iter().zip(spans).enumerate() {
        let provenance = dae::DaeProvenance::source(*span)?;
        let binder = construction.domains(|domains| domains.binder(domain, ordinal, provenance))?;
        binders.insert(VarName::new(&index.ident), binder);
    }
    Ok(binders)
}

fn lower_optional_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    expression: Option<&Expression>,
) -> Result<Option<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    expression
        .map(|expression| lower_expression(construction, coordinates, functions, expression, None))
        .transpose()
}

fn lower_attribute_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    expression: &Expression,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    lower_expression_scoped(
        construction,
        LoweringSymbols {
            coordinates,
            record_staging: None,
            functions,
            shapes: functions.shapes.model_values(),
            function_body: None,
            values: None,
            owner_clock: None,
        },
        &HashMap::new(),
        expression,
        None,
    )
}

struct BindingsRequest<'input, 'dae> {
    roles: &'input HashMap<VarName, PlannedRole>,
    topology: &'input DiscreteValueTopologyPlan,
    flat: &'input flat::Model,
    /// Clock owner of each coordinate in a clocked partition, so a declaration
    /// binding inside such a partition lowers `interval()`/`previous()` against
    /// the same clock its equations use (MLS §16.5.1).
    coordinate_owners: &'input HashMap<InstanceId, ClockPlan>,
    clocks: &'input LoweredClocks<'dae>,
}

fn lower_bindings<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    request: BindingsRequest<'_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let BindingsRequest {
        roles,
        topology,
        flat,
        coordinate_owners,
        clocks,
    } = request;
    for (name, variable) in &flat.variables {
        let Some(binding) = &variable.binding else {
            continue;
        };
        if matches!(roles[name], PlannedRole::Clock) {
            continue;
        }
        let coordinate = coordinates[name];
        if matches!(coordinate, Coordinate::Parameter(_) | Coordinate::Input(_)) {
            continue;
        }
        let Some(binding_span) = binding.span() else {
            return Err(dae::DaeConstructionError::MissingProvenance {
                origin: dae::DaeProvenanceOrigin::Source,
                attempted_span: None,
            });
        };
        let binding_source = dae::DaeProvenance::source(binding_span)?;
        let owner_span = binding_source.span();
        let owner = dae::DaeProvenance::generated(dae::DaeGeneration::BindingEquation, owner_span)?;
        let owner_clock = coordinate_owners
            .get(&variable.instance_id)
            .map(|plan| clocks.id(plan, binding_span))
            .transpose()?;
        let rhs = lower_equation_expression(
            construction,
            coordinates,
            functions,
            owner_clock,
            binding,
            None,
        )?;
        match coordinate {
            Coordinate::DiscreteValue(target) => {
                let semantic_owner = discrete_values
                    .owner(owner, [name.clone()], coordinates, topology)?
                    .expect("a discrete-value binding has one planned B.1c owner");
                discrete_values.always(semantic_owner, target, rhs, owner, binding_source)?;
            }
            Coordinate::Parameter(_)
            | Coordinate::Input(_)
            | Coordinate::FunctionParameter(_)
            | Coordinate::FunctionValue(_) => {
                unreachable!("non-equation binding coordinates were filtered before lowering")
            }
            coordinate => {
                lower_residual_binding(construction, coordinate, owner, rhs)?;
            }
        }
    }
    Ok(())
}

fn lower_residual_binding<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinate: Coordinate<'dae>,
    owner: dae::DaeProvenance,
    rhs: dae::ExprId<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let lhs = construction
        .expressions(|expressions| expressions.at(owner).coordinate(coordinate.current()))?;
    let residual = generated_residual(construction, owner, lhs, rhs)?;
    match coordinate {
        Coordinate::DiscreteReal(_) => {
            construction.discrete(|discrete| {
                discrete.real_equation(owner, |equation| equation.residual(residual))
            })?;
            Ok(())
        }
        Coordinate::State(_) | Coordinate::Algebraic(_) => {
            construction.continuous(|continuous| continuous.value_equation(owner, residual))
        }
        Coordinate::Parameter(_)
        | Coordinate::Input(_)
        | Coordinate::DiscreteValue(_)
        | Coordinate::FunctionParameter(_)
        | Coordinate::FunctionValue(_) => {
            unreachable!("caller passes only residual-defined coordinates")
        }
    }
}

fn generated_residual<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    owner: dae::DaeProvenance,
    lhs: dae::ExprId<'dae>,
    rhs: dae::ExprId<'dae>,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::SyntheticResidual, owner.span())?;
    construction.expressions(|expressions| {
        expressions
            .at(provenance)
            .binary(dae::BinaryOperator::Subtract, lhs, rhs)
    })
}

/// Lower the assertions an equation, initial-equation, or initial-algorithm
/// section owns.
///
/// The activation is a *level*, not an edge. MLS §8.3.7 violates an assertion
/// because its condition *is* false — *"assert(condition, message) ... the
/// assertion is violated if the condition is false"* — not because it became
/// false, and none of these three sections is a `when`, whose §8.3.5 "becomes
/// true" activation is what an edge encodes. An assertion written inside a
/// `when` body keeps its edge, because there the activation belongs to the
/// `when` (see `WhenLowering::lower_assert`).
///
/// The level is expressed by giving the action [`dae::ConditionInput::Always`]
/// as its *trigger*, which carries no §8.5 buffer, so `edge(trigger)` reads
/// `true` and the action's own guard — the negated assertion condition — is
/// what decides. Every assertion lowered here takes that path, whatever its
/// condition: `assert(x > 0, …)` is level-checked exactly like
/// `assert(false, …)`. Handing the assertion its own violation as the trigger
/// instead makes it an edge, and an assertion already violated at the
/// initialization instant then has no edge to report on — which silently
/// dropped the `initial algorithm` guard assertions this exists for.
fn lower_assertions<'dae, 'flat>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, PeriodicClockSchedule)],
    assertions: impl IntoIterator<Item = &'flat flat::AssertEquation>,
) -> Result<(), dae::DaeConstructionError> {
    for assertion in assertions {
        let (condition, _) = lower_condition(
            construction,
            coordinates,
            functions,
            sample_lattices,
            &assertion.condition,
        )?;
        let action_guard = negate_condition(construction, condition, assertion.span)?;
        let trigger = always_condition(construction, assertion.span)?;
        let message = lower_expression(
            construction,
            coordinates,
            functions,
            &assertion.message,
            None,
        )?;
        let level = lower_optional_expression(
            construction,
            coordinates,
            functions,
            assertion.level.as_ref(),
        )?;
        let provenance = dae::DaeProvenance::source(assertion.span)?;
        construction.events(|events| {
            events.assert_with_level(trigger, action_guard, message, level, provenance)
        })?;
    }
    Ok(())
}

#[derive(Clone, Copy)]
struct EventGuard<'dae> {
    trigger: dae::ConditionId<'dae>,
    condition: dae::ConditionId<'dae>,
    owner_clock: Option<dae::PeriodicClockId<'dae>>,
    branch_provenance: dae::DaeProvenance,
    always: bool,
    parent_activation: Option<(dae::ConditionId<'dae>, dae::ConditionId<'dae>)>,
}

#[derive(Clone, Copy)]
struct StructuredEquationEnvironment<'scope, 'dae> {
    roles: &'scope HashMap<VarName, PlannedRole>,
    topology: &'scope DiscreteValueTopologyPlan,
    clocked_owners: &'scope HashMap<usize, ClockPlan>,
    clocks: &'scope LoweredClocks<'dae>,
}

#[derive(Clone, Copy)]
struct StructuredEquationRows<'scope, 'dae> {
    equations: &'scope [flat::Equation],
    families: &'scope [flat::StructuredEquationFamily],
    family_rows: &'scope HashMap<usize, std::ops::Range<usize>>,
    excluded_families: &'scope HashSet<usize>,
    environment: Option<StructuredEquationEnvironment<'scope, 'dae>>,
    initialization: bool,
}

fn lower_structured_equations<'flat, 'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'flat, 'dae>,
    rows: StructuredEquationRows<'flat, 'dae>,
    mut deferred_equations: Option<&mut DeferredModelEquationRows<'flat>>,
) -> Result<(), dae::DaeConstructionError> {
    for (family_index, family) in rows.families.iter().enumerate() {
        if rows.excluded_families.contains(&family_index) {
            continue;
        }
        let owner = equation_owner_provenance(&family.origin, family.span)?;
        let row_range = rows
            .family_rows
            .get(&family_index)
            .cloned()
            .ok_or(dae::DaeConstructionError::InvalidExpressionForm { span: family.span })?;
        let generated_root = equation_generation(&family.origin);
        let domain =
            construction.domains(|domains| domains.structured(family.domain.clone(), owner))?;
        let bodies = if let Some(template) = &family.template {
            let mut binders = HashMap::with_capacity(family.domain.binders.len());
            for (ordinal, binder) in family.domain.binders.iter().enumerate() {
                let id = construction.domains(|domains| domains.binder(domain, ordinal, owner))?;
                binders.insert(VarName::new(&binder.display_name), id);
            }
            let mut scoped_shapes = functions.shapes.model_values().clone();
            for binder in binders.keys() {
                // A StructuredIndexDomain binder is a scalar Integer by
                // construction.  Carry that proof into function-call shape
                // selection while lowering the compact body.
                scoped_shapes.insert(binder.clone(), Vec::new());
            }
            if lower_partitioned_structured_template(
                construction,
                discrete_values,
                StructuredTemplatePartitionInput {
                    coordinates,
                    functions,
                    family,
                    row_range,
                    domain,
                    scalar_view: template.scalar_view,
                    binders: &binders,
                    shapes: &scoped_shapes,
                    environment: rows.environment,
                    deferred_equations: deferred_equations.as_deref_mut(),
                    owner,
                },
            )? {
                continue;
            }
            template
                .body
                .iter()
                .map(|body| {
                    let symbols = LoweringSymbols {
                        coordinates,
                        record_staging: None,
                        functions,
                        shapes: &scoped_shapes,
                        function_body: None,
                        values: None,
                        owner_clock: None,
                    };
                    lower_structured_body(
                        construction,
                        symbols,
                        &binders,
                        body,
                        generated_root,
                        owner.span(),
                    )
                })
                .collect::<Result<Vec<_>, _>>()?
        } else {
            lower_materialized_family_bodies(
                construction,
                coordinates,
                functions,
                rows.equations,
                family,
                owner,
            )?
        };
        let scalar_view = family
            .template
            .as_ref()
            .map(|template| template.scalar_view)
            .unwrap_or(rumoca_core::ComprehensionScalarView::RowMajorProjection);
        insert_structured_family(
            construction,
            rows.initialization,
            owner,
            domain,
            scalar_view,
            bodies,
        )?;
    }
    Ok(())
}

enum StructuredFamilyPartition<'flat> {
    Continuous,
    DiscreteValue(Vec<DiscreteValueAssignmentPlan<'flat>>),
    ConsumedDiscreteValue,
}

struct StructuredTemplatePartitionInput<'borrow, 'local, 'scope, 'flat, 'dae> {
    coordinates: &'scope HashMap<VarName, Coordinate<'dae>>,
    functions: &'scope FunctionRegistry<'flat, 'dae>,
    family: &'flat flat::StructuredEquationFamily,
    row_range: std::ops::Range<usize>,
    domain: dae::DomainId<'dae>,
    scalar_view: rumoca_core::ComprehensionScalarView,
    binders: &'local HashMap<VarName, dae::DomainBinderId<'dae>>,
    shapes: &'local ShapeEnvironment,
    environment: Option<StructuredEquationEnvironment<'scope, 'dae>>,
    deferred_equations: Option<&'borrow mut DeferredModelEquationRows<'flat>>,
    owner: dae::DaeProvenance,
}

fn lower_partitioned_structured_template<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    mut input: StructuredTemplatePartitionInput<'_, '_, '_, '_, 'dae>,
) -> Result<bool, dae::DaeConstructionError> {
    let Some(environment) = input.environment else {
        return Ok(false);
    };
    let template =
        input
            .family
            .template
            .as_ref()
            .ok_or(dae::DaeConstructionError::InvalidExpressionForm {
                span: input.family.span,
            })?;
    if input.family.interiors_materialized
        && template.body.len() != input.family.equations_per_point
    {
        return Err(dae::DaeConstructionError::InvalidExpressionForm {
            span: input.family.span,
        });
    }
    let partition = structured_family_partition(
        input.family,
        template,
        environment.roles,
        input.deferred_equations.as_deref_mut(),
    )?;
    consume_structured_family_claims(
        input.family,
        input.row_range,
        &partition,
        input.deferred_equations,
    )?;
    let assignments = match partition {
        StructuredFamilyPartition::Continuous => return Ok(false),
        StructuredFamilyPartition::ConsumedDiscreteValue => return Ok(true),
        StructuredFamilyPartition::DiscreteValue(assignments) => assignments,
    };
    lower_structured_discrete_family(
        construction,
        discrete_values,
        StructuredDiscreteFamilyInput {
            coordinates: input.coordinates,
            functions: input.functions,
            family: input.family,
            domain: input.domain,
            scalar_view: input.scalar_view,
            binders: input.binders,
            shapes: input.shapes,
            assignments: &assignments,
            environment,
            owner: input.owner,
        },
    )?;
    Ok(true)
}

fn consume_structured_family_claims(
    family: &flat::StructuredEquationFamily,
    rows: std::ops::Range<usize>,
    partition: &StructuredFamilyPartition<'_>,
    deferred_equations: Option<&mut DeferredModelEquationRows<'_>>,
) -> Result<(), dae::DaeConstructionError> {
    let Some(deferred_equations) = deferred_equations else {
        return Err(dae::DaeConstructionError::InvalidExpressionForm { span: family.span });
    };
    if rows.start != family.first_equation_index {
        return Err(dae::DaeConstructionError::InvalidExpressionForm { span: family.span });
    }
    let already_consumed = if family.interiors_materialized {
        family.equations_per_point
    } else {
        0
    };
    for row in rows.skip(already_consumed) {
        let row_partition = deferred_equations.take_partition(row, family.span)?;
        let compatible = matches!(
            (partition, row_partition),
            (
                StructuredFamilyPartition::Continuous,
                EquationPartition::Continuous
            ) | (
                StructuredFamilyPartition::ConsumedDiscreteValue,
                EquationPartition::ConsumedDiscreteValue
            ) | (
                StructuredFamilyPartition::DiscreteValue(_),
                EquationPartition::DiscreteValue(_) | EquationPartition::ConsumedDiscreteValue
            )
        );
        if !compatible {
            return Err(dae::DaeConstructionError::InvalidExpressionForm { span: family.span });
        }
    }
    Ok(())
}

fn structured_family_partition<'flat>(
    family: &'flat flat::StructuredEquationFamily,
    template: &'flat rumoca_core::ComprehensionTemplate,
    roles: &HashMap<VarName, PlannedRole>,
    mut deferred_equations: Option<&mut DeferredModelEquationRows<'flat>>,
) -> Result<StructuredFamilyPartition<'flat>, dae::DaeConstructionError> {
    let mut assignments = Vec::with_capacity(template.body.len());
    for (ordinal, body) in template.body.iter().enumerate() {
        // Materialized family rows and their compact template are two views of
        // one semantic owner. Move the already-issued role out of the deferred
        // sequence row; never classify the scalar occurrence again.
        let assignment = if family.interiors_materialized {
            let Some(deferred_equations) = deferred_equations.as_deref_mut() else {
                return Err(dae::DaeConstructionError::InvalidExpressionForm { span: family.span });
            };
            match deferred_equations
                .take_partition(family.first_equation_index + ordinal, family.span)?
            {
                EquationPartition::DiscreteValue(plan) => Some(Ok(plan)),
                EquationPartition::ConsumedDiscreteValue => Some(Err(())),
                EquationPartition::Continuous | EquationPartition::DiscreteReal { .. } => None,
            }
        } else {
            discrete_value_assignment(body, roles, family.span)
                .map_err(|_| dae::DaeConstructionError::InvalidExpressionForm {
                    span: family.span,
                })?
                .map(Ok)
        };
        assignments.push(assignment);
    }
    if assignments.iter().all(Option::is_none) {
        return Ok(StructuredFamilyPartition::Continuous);
    }
    if assignments
        .iter()
        .all(|assignment| matches!(assignment, Some(Err(()))))
    {
        return Ok(StructuredFamilyPartition::ConsumedDiscreteValue);
    }
    let mut plans = Vec::with_capacity(assignments.len());
    for assignment in assignments {
        let Some(Ok(plan)) = assignment else {
            return Err(dae::DaeConstructionError::InvalidExpressionForm { span: family.span });
        };
        plans.push(plan);
    }
    Ok(StructuredFamilyPartition::DiscreteValue(plans))
}

struct StructuredDiscreteFamilyInput<'assignments, 'scope, 'flat, 'dae> {
    coordinates: &'scope HashMap<VarName, Coordinate<'dae>>,
    functions: &'scope FunctionRegistry<'flat, 'dae>,
    family: &'scope flat::StructuredEquationFamily,
    domain: dae::DomainId<'dae>,
    scalar_view: rumoca_core::ComprehensionScalarView,
    binders: &'scope HashMap<VarName, dae::DomainBinderId<'dae>>,
    shapes: &'scope ShapeEnvironment,
    assignments: &'assignments [DiscreteValueAssignmentPlan<'flat>],
    environment: StructuredEquationEnvironment<'scope, 'dae>,
    owner: dae::DaeProvenance,
}

fn lower_structured_discrete_family<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    input: StructuredDiscreteFamilyInput<'_, '_, '_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let owner_clock = input
        .environment
        .clocked_owners
        .get(&input.family.first_equation_index)
        .map(|plan| input.environment.clocks.id(plan, input.family.span))
        .transpose()?;
    let semantic_owner = discrete_values
        .structured_owner(
            input.owner,
            input.domain,
            input.scalar_view,
            input.assignments.iter().map(|plan| plan.target.clone()),
            input.coordinates,
            input.environment.topology,
        )?
        .ok_or(dae::DaeConstructionError::InvalidExpressionForm {
            span: input.family.span,
        })?;
    for plan in input.assignments {
        let symbols = LoweringSymbols {
            coordinates: input.coordinates,
            record_staging: None,
            functions: input.functions,
            shapes: input.shapes,
            function_body: None,
            values: None,
            owner_clock,
        };
        let generation = plan
            .generated
            .then_some(dae::DaeGeneration::DiscreteUpdate)
            .or_else(|| equation_generation(&input.family.origin));
        let value = lower_structured_body(
            construction,
            symbols,
            input.binders,
            plan.value.as_ref(),
            generation,
            input.family.span,
        )?;
        let Some(Coordinate::DiscreteValue(target)) = input.coordinates.get(&plan.target).copied()
        else {
            return Err(dae::DaeConstructionError::InvalidVariableRole {
                name: plan.target.clone(),
                span: input.family.span,
            });
        };
        let action_span = plan.value.span().unwrap_or(input.family.span);
        discrete_values.always(
            semantic_owner,
            target,
            value,
            input.owner,
            dae::DaeProvenance::source(action_span)?,
        )?;
    }
    Ok(())
}

fn insert_structured_family<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    initialization: bool,
    owner: dae::DaeProvenance,
    domain: dae::DomainId<'dae>,
    scalar_view: rumoca_core::ComprehensionScalarView,
    bodies: Vec<dae::ExprId<'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    if initialization {
        construction.initialization(|system| {
            system.structured_family(owner, domain, scalar_view, |residuals| {
                insert_family_bodies(residuals, bodies)
            })?;
            Ok(())
        })
    } else {
        construction.continuous(|system| {
            system.structured_family(owner, domain, scalar_view, |residuals| {
                insert_family_bodies(residuals, bodies)
            })?;
            Ok(())
        })
    }
}

fn insert_family_bodies<'dae>(
    residuals: &mut dae::StructuredResiduals<'_, 'dae>,
    bodies: Vec<dae::ExprId<'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    for body in bodies {
        residuals.body(body)?;
    }
    Ok(())
}

fn lower_materialized_family_bodies<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    equations: &[flat::Equation],
    family: &flat::StructuredEquationFamily,
    owner: dae::DaeProvenance,
) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    let domain_count = family
        .domain
        .scalar_count()
        .expect("analysis validates the structured domain");
    let extents = family
        .domain
        .extents()
        .expect("analysis validates the structured domain");
    let mut bodies = Vec::with_capacity(family.equations_per_point);
    for body_ordinal in 0..family.equations_per_point {
        let mut scalar_bodies = Vec::with_capacity(domain_count);
        for point in 0..domain_count {
            let offset = point
                .checked_mul(family.equations_per_point)
                .and_then(|offset| offset.checked_add(body_ordinal))
                .expect("analysis validates the materialized family row range");
            let equation = &equations[family.first_equation_index + offset];
            let symbols = LoweringSymbols {
                coordinates,
                record_staging: None,
                functions,
                shapes: functions.shapes.model_values(),
                function_body: None,
                values: None,
                owner_clock: None,
            };
            scalar_bodies.push(lower_structured_body(
                construction,
                symbols,
                &HashMap::new(),
                &equation.residual,
                equation_generation(&equation.origin),
                equation.span,
            )?);
        }
        let provenance = dae::DaeProvenance::generated(
            dae::DaeGeneration::ArrayEquationProjection,
            owner.span(),
        )?;
        bodies.push(pack_row_major_body(
            construction,
            &scalar_bodies,
            &extents,
            provenance,
        )?);
    }
    Ok(bodies)
}

pub(super) fn pack_row_major_body<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    scalars: &[dae::ExprId<'dae>],
    extents: &[usize],
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let checked_extents = extents
        .iter()
        .copied()
        .map(|extent| {
            u32::try_from(extent).map_err(|_| dae::DaeConstructionError::ArrayExtentOverflow {
                extent,
                span: provenance.span(),
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    if let Some(base) = construction.expressions(|expressions| {
        expressions.exact_row_major_projection_base(scalars, &checked_extents, provenance)
    })? {
        return Ok(base);
    }
    let Some((&outer, inner_extents)) = extents.split_first() else {
        return Ok(scalars[0]);
    };
    let inner_count = inner_extents
        .iter()
        .try_fold(1usize, |count, extent| count.checked_mul(*extent))
        .expect("analysis validates the structured domain cardinality");
    let mut elements = Vec::with_capacity(outer);
    for chunk in scalars.chunks_exact(inner_count) {
        elements.push(pack_row_major_body(
            construction,
            chunk,
            inner_extents,
            provenance,
        )?);
    }
    construction.expressions(|expressions| expressions.at(provenance).array(elements))
}
