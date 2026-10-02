mod algorithm;
mod algorithm_lowering;
mod analysis;
pub use analysis::StructuralSelection;
mod clock_operator_hosts;
mod clocks;
mod conditions;
mod discrete_values;
mod enumeration_conversion;
mod equation_systems;
mod expression;
mod function_array_assembly;
mod function_body;
mod function_construction;
mod function_external;
mod function_record_assembly;
mod function_seeds;
mod function_shapes;
mod function_statements;
mod initial_discrete_values;
mod initial_parameter_values;
mod model_algorithm;
mod model_events;
mod multi_output_equations;
mod native_tables;
mod ordinary_equations;
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

use crate::ToDaeError;
use crate::balance::BalanceDetail;
use algorithm::{
    AlgorithmFunctionCall, AlgorithmStatementContext, lower_algorithm_assignment,
    lower_algorithm_function_call, lower_algorithm_tensor_loop, own_clocked_algorithm_targets,
};
use algorithm_lowering::{AlgorithmEnvironment, ModelAlgorithmsRequest, lower_algorithms};
use analysis::{
    AggregateDiscreteConnections, Analysis, ClockPlan, ComprehensionKey, ComprehensionPlans,
    DelayPlan, DerivedParameterPlan, DiscreteValueAssignmentPlan, DiscreteValueTopologyPlan,
    DynamicTimeEventOperand, EquationPartition, ExpressionEventPlan, ExpressionEventPlans,
    ExternalArgumentPlan, ExternalFunctionPlan, FunctionArrayAssemblyPlan, FunctionAssignmentPlan,
    FunctionDefinednessPlan, FunctionIntegerReduction, FunctionLoopLowering, FunctionPlan,
    FunctionRecordAssemblyPlan, FunctionRecordCallAssemblyPlan, FunctionRecordFieldAssembly,
    FunctionRecordFieldAssemblyPlan, FunctionStatementPlan, FunctionValueSeed,
    HistoryOperatorPlans, ModelAlgorithmPlan, ModelEventFunctionCallPlan,
    ModelEventFunctionOutputPlan, ModelEventTensorLoopPlan, MultiOutputEquationPlan,
    PartialJoinPlan, PlannedRole, RecordArrayFieldPlan, RecordArrayFieldPlans,
    RecordEquationFieldPlan, RecordEquationFieldValue, RecordEquationPlan, RuntimeVariableRole,
    SemiLinearRules, StructuredSource, WhenBranchKey, analyze, assigned_function_targets,
    branch_never_completes, discrete_value_assignment, effective_function_scalar_type,
    effective_variable_scalar_type, empty_array_bound_to_declaration, equation_partition,
    flattened_function_loop_source, function_assertion, function_record_field_name,
    inferred_clock_transfer, is_event_condition, is_inferred_clock_condition,
    is_whole_clock_coordinate, materialized_discrete_real_family, materialized_discrete_value_rows,
    model_algorithm_targets, record_field_projections, selected_conditional_statements,
    specialized_comprehension_plan, structured_assignment_names,
    when_conditional_selects_clock_structure,
};
use clock_operator_hosts::clock_operator_hosts;
use clocks::{LoweredClocks, lower_clocked_value_owners, lower_clocks};
use conditions::{combine_conditions, condition_owner_clock, lower_condition, negate_condition};
use discrete_values::{DiscreteValueOwnerHandle, DiscreteValueStaging};
use enumeration_conversion::{
    enumeration_conversion, enumeration_range_ordinals, enumeration_range_type,
    has_enumeration_range_bound, is_flat_enumeration_literal,
};
use equation_systems::{lower_equation_expression, lower_equation_systems};
use expression::{
    FunctionArrayUpdate, FunctionCallLowering, LoweringSymbols, all_model_expressions,
    classify_function_call, derivative_reference, expression_children, expression_span,
    lower_array_update, lower_call_operands, lower_clocked_expression,
    lower_clocked_model_algorithm_expression, lower_expression, lower_expression_scoped,
    lower_function_array_update, lower_function_expression, lower_function_expression_scoped,
    lower_model_algorithm_expression, lower_scoped_model_algorithm_expression,
    planned_input_variability, require_span, variable_attribute_expressions,
};
use function_array_assembly::lower_function_array_assembly;
use function_body::{
    DefinednessPredicates, FunctionConditional, FunctionFold, PartialTarget, TotalArrayDefinition,
    function_value_coordinate, lower_function_conditional, lower_function_fold,
    lower_function_value_seed, lower_generated_boolean_assignment, lower_guarded_function_return,
    lower_integer_reduction, lower_total_function_array_definition,
};
use function_construction::{
    FunctionRegistry, FunctionRegistryInput, construct_functions, function_value_type,
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
    FunctionShapeAnalysis, FunctionShapeCertificate, FunctionSpecializationKey, ProvenValue,
    ShapeEnvironment, ValueShape, call_free_expression_shape, call_free_target_shape,
    evaluate_shape_integer, infer_function_integer_bounds, proven_conditional_branch,
};
use function_statements::*;
use model_algorithm::{
    ModelAlgorithmLowering, lower_declarative_model_algorithm,
    lower_separated_array_sum_model_algorithm, lower_total_array_model_algorithm,
};
use model_events::{WhenChainsRequest, always_condition, lower_when_assignment, lower_when_chains};
use multi_output_equations::{MultiOutputDiscreteOwners, lower_multi_output_equation};
use ordinary_equations::{OrdinaryEquationRow, lower_ordinary_equation};
use record_equation::lower_record_equation;
use structured_body::{lower_structured_body, normalize_conditional_residual};
use variable_construction::{
    VariableConstructionPlan, VariableDefinitionContext, define_reserved_variables,
    insert_variable_identities, plan_variable_construction,
};

#[derive(Clone, Copy)]
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

    fn derivative(self, span: Span) -> Result<dae::CoordinateInput<'dae>, ToDaeError> {
        match self {
            Self::State(id) => Ok(dae::CoordinateInput::Derivative(id)),
            Self::Parameter(_)
            | Self::Input(_)
            | Self::Algebraic(_)
            | Self::DiscreteReal(_)
            | Self::DiscreteValue(_)
            | Self::FunctionParameter(_)
            | Self::FunctionValue(_) => Err(ToDaeError::unsupported_flat(
                "derivative target",
                "der(...) must name a coordinate classified as a continuous state",
                span,
            )),
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

struct ModelCoordinates<'dae> {
    by_name: HashMap<VarName, Coordinate<'dae>>,
    by_instance: HashMap<rumoca_core::InstanceId, Coordinate<'dae>>,
}

impl<'dae> ModelCoordinates<'dae> {
    fn new() -> Self {
        Self {
            by_name: HashMap::new(),
            by_instance: HashMap::new(),
        }
    }

    fn insert(&mut self, variable: &flat::Variable, coordinate: Coordinate<'dae>) {
        self.by_name.insert(variable.name.clone(), coordinate);
        let previous = self.by_instance.insert(variable.instance_id, coordinate);
        debug_assert!(
            previous.is_none(),
            "analysis rejects duplicate runtime variable instance identities"
        );
    }

    fn by_instance(&self) -> &HashMap<rumoca_core::InstanceId, Coordinate<'dae>> {
        &self.by_instance
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

pub(crate) fn construct(flat: &flat::Model, source_map: SourceMap) -> Result<dae::Dae, ToDaeError> {
    let analysis = analyze(flat)?.with_semi_linear_rules(flat);
    if !flat.is_partial && !analysis.balance.is_balanced() {
        return Err(ToDaeError::unbalanced_from_detail(analysis.balance));
    }
    let variable_plan = plan_variable_construction(flat, &analysis)?;
    let external_tables = native_tables::build_external_tables(flat, &analysis.constants)?;

    let dae = dae::Dae::construct(source_map, |construction| {
        build_checked(flat, &analysis, &variable_plan, construction)
    })
    .map_err(ToDaeError::from)?;
    Ok(dae.with_external_tables(external_tables))
}

/// The balance evidence and the structural guard selections of one analysis.
pub(crate) fn construction_evidence(
    flat: &flat::Model,
) -> Result<(BalanceDetail, Vec<analysis::StructuralSelection>), ToDaeError> {
    analyze(flat).map(|analysis| (analysis.balance, analysis.structural_selections))
}

fn build_checked<'dae>(
    flat: &flat::Model,
    analysis: &Analysis,
    variable_plan: &VariableConstructionPlan,
    construction: &mut dae::DaeConstruction<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    if let Some(declaration) = flat.predefined_string_declaration {
        construction.register_predefined_string(declaration)?;
    }
    let value_types = reserve_value_types(flat, analysis, construction)?;
    let mut clocks = lower_analysis_clocks(construction, flat, analysis)?;
    let no_function_ids = HashMap::new();
    let no_coordinate_instances = HashMap::new();
    let analysis_functions = model_function_registry(
        flat,
        analysis,
        &no_function_ids,
        &no_coordinate_instances,
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
    clocks.define_event_conditions(construction, &coordinates)?;
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
            history_operators: &analysis.history_operators,
            coordinate_instances: coordinates.by_instance(),
            expression_events: &analysis.expression_events,
            sample_alias_schedules: &analysis.sample_alias_schedules,
            clocked_coordinate_owners: &analysis.clocked_coordinate_owners,
            clocks: &clocks,
        },
        &analysis.function_plans,
    )?;
    let functions = model_function_registry(
        flat,
        analysis,
        &function_ids,
        coordinates.by_instance(),
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
            evaluable_parameters: &analysis.evaluable_parameters,
            native_table_ids: &analysis.native_table_ids,
        },
        variable_plan,
        variable_identities.reserved,
    )?;
    lower_clocked_value_owners(
        construction,
        flat,
        &coordinates,
        &analysis.clocked_value_owners,
        &clocks,
    )?;
    let mut discrete_values = DiscreteValueStaging::new();
    lower_bindings(
        construction,
        &mut discrete_values,
        &coordinates,
        &functions,
        BindingsRequest {
            roles: &analysis.roles,
            topology: &analysis.discrete_value_topology,
            flat,
            coordinate_owners: &analysis.clocked_coordinate_owners,
            clocks: &clocks,
        },
    )?;
    lower_model_owners(
        construction,
        flat,
        analysis,
        &coordinates,
        &functions,
        &clocks,
        discrete_values,
    )?;
    lower_scheduled_time_events(construction, &analysis.expression_events)
}

fn model_function_registry<'scope, 'dae>(
    flat: &'scope flat::Model,
    analysis: &'scope Analysis,
    ids: &'scope HashMap<FunctionSpecializationKey, dae::FunctionId<'dae>>,
    coordinate_instances: &'scope HashMap<InstanceId, Coordinate<'dae>>,
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
        history_operators: &analysis.history_operators,
        coordinate_instances,
        expression_events: &analysis.expression_events,
        sample_alias_schedules: &analysis.sample_alias_schedules,
        clocked_coordinate_owners: &analysis.clocked_coordinate_owners,
        clocks,
    }
}

fn lower_analysis_clocks<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    flat: &flat::Model,
    analysis: &Analysis,
) -> Result<LoweredClocks<'dae>, dae::DaeConstructionError> {
    let mut clocks = lower_clocks(
        construction,
        flat,
        &analysis.clock_plans,
        &analysis.event_clocks,
        &analysis.clocked_value_owners,
        analysis
            .expression_events
            .ordered()
            .filter_map(|(span, plan)| {
                let ExpressionEventPlan::SampleClock(schedule) = plan else {
                    return None;
                };
                Some((schedule, span))
            }),
    )?;
    // MLS §16.10: `interval()` of an event clock reads the clock's first tick
    // and the time of its previous tick.
    let event_intervals = clock_operator_hosts(flat, analysis, BuiltinFunction::Interval)
        .into_iter()
        .filter(|(plan, _)| plan.lattice().is_none())
        .collect::<Vec<_>>();
    clocks.issue_first_ticks(
        construction,
        clock_operator_hosts(flat, analysis, BuiltinFunction::FirstTick)
            .into_iter()
            .chain(event_intervals.iter().copied()),
    )?;
    clocks.issue_event_intervals(construction, event_intervals)?;
    clocks.issue_tick_counters(construction)?;
    Ok(clocks)
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

fn lower_model_owners<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    flat: &flat::Model,
    analysis: &Analysis,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    clocks: &LoweredClocks<'dae>,
    mut discrete_values: DiscreteValueStaging<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    lower_equation_systems(
        construction,
        &mut discrete_values,
        flat,
        analysis,
        coordinates,
        functions,
        clocks,
    )?;
    initial_discrete_values::lower_initial_discrete_values(
        construction,
        coordinates,
        functions,
        analysis,
    )?;
    initial_parameter_values::lower(construction, coordinates, functions, analysis)?;
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
            flat,
            environment: AlgorithmEnvironment {
                coordinates,
                functions,
                sample_lattices: &analysis.sample_lattices,
                tensor_loops: None,
                function_calls: None,
                transaction_steps: None,
            },
            plans: &analysis.model_algorithm_plans,
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
    analysis: &Analysis,
    construction: &mut dae::DaeConstruction<'dae>,
) -> Result<HashMap<VarName, dae::ValueTypeId<'dae>>, dae::DaeConstructionError> {
    let mut value_types = HashMap::new();
    for (name, variable) in &flat.variables {
        if matches!(analysis.roles[name], PlannedRole::Clock) {
            continue;
        }
        let provenance = dae::DaeProvenance::source(variable.source_span)?;
        let scalar = effective_variable_scalar_type(flat, variable)
            .expect("analysis accepts only primitive value types");
        let dimensions = variable
            .dims
            .iter()
            .map(|extent| {
                u32::try_from(*extent).map_err(|_| dae::DaeConstructionError::CapacityExceeded {
                    arena: "variable dimension",
                    attempted_index: usize::MAX,
                    span: variable.source_span,
                })
            })
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
            functions,
            // A variable's attribute and binding values keep their MLS §3.6.5
            // conditionals so a tunable-parameter guard re-selects the branch in
            // the eFMI `Recalibrate` step instead of freezing at the parameter's
            // translation-time value.
            shapes: functions.shapes.model_attribute_values(),
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
/// The MLS §8.3.7 level of one assertion whose level flattening settled.
pub(super) fn settled_assertion_level(
    level: Option<&Expression>,
    span: Span,
) -> Result<dae::AssertionLevel, dae::DaeConstructionError> {
    match flat::AssertionLevel::of_settled(level) {
        Some(flat::AssertionLevel::Error) => Ok(dae::AssertionLevel::Error),
        Some(flat::AssertionLevel::Warning) => Ok(dae::AssertionLevel::Warning),
        None => Err(dae::DaeConstructionError::UnsupportedAssertionLevel { span }),
    }
}

fn lower_assertions<'dae, 'flat>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, PeriodicClockSchedule)],
    assertions: impl IntoIterator<Item = &'flat flat::AssertEquation>,
) -> Result<(), dae::DaeConstructionError> {
    for assertion in assertions {
        let level = settled_assertion_level(assertion.level.as_ref(), assertion.span)?;
        let provenance = dae::DaeProvenance::source(assertion.span)?;
        if level == dae::AssertionLevel::Warning {
            let holds = lower_expression(
                construction,
                coordinates,
                functions,
                &assertion.condition,
                None,
            )?;
            let message = lower_expression(
                construction,
                coordinates,
                functions,
                &assertion.message,
                None,
            )?;
            let always = always_condition(construction, assertion.span)?;
            construction
                .events(|events| events.warning(always, always, holds, message, provenance))?;
            continue;
        }
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
        construction.events(|events| events.assert(trigger, action_guard, message, provenance))?;
    }
    Ok(())
}

/// The B.1c branch one guarded branch nests under.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum ParentActivation<'dae> {
    /// The unconditional statements of the enclosing algorithm section: a
    /// top-level `when` or `if` of an event algorithm starts from the values
    /// those statements have defined (MLS §11.1.2).
    Section,
    When {
        trigger: dae::ConditionId<'dae>,
        guard: dae::ConditionId<'dae>,
    },
}

#[derive(Clone, Copy)]
struct EventGuard<'dae> {
    trigger: dae::ConditionId<'dae>,
    condition: dae::ConditionId<'dae>,
    owner_clock: Option<dae::ClockId<'dae>>,
    branch_provenance: dae::DaeProvenance,
    always: bool,
    parent_activation: Option<ParentActivation<'dae>>,
    /// The top-level algorithm statement this branch belongs to. Separate
    /// statements of one section run in source order, so a later statement
    /// overrides an earlier one (MLS §11.1.2); the branches of one
    /// `when`/`elsewhen` or `if`/`elseif` chain keep their textual priority.
    statement: Option<Span>,
}

#[derive(Clone, Copy)]
struct StructuredEquationEnvironment<'scope, 'dae> {
    flat: &'scope flat::Model,
    roles: &'scope HashMap<VarName, PlannedRole>,
    topology: &'scope DiscreteValueTopologyPlan,
    connection_ranks: &'scope HashMap<VarName, usize>,
    aggregate_connections: &'scope AggregateDiscreteConnections,
    clocked_owners: &'scope HashMap<usize, ClockPlan>,
    clocks: &'scope LoweredClocks<'dae>,
}

#[derive(Clone, Copy)]
struct StructuredEquationRows<'scope, 'dae> {
    equations: &'scope [flat::Equation],
    families: &'scope [flat::StructuredEquationFamily],
    excluded_families: &'scope HashSet<usize>,
    environment: Option<StructuredEquationEnvironment<'scope, 'dae>>,
    initialization: bool,
}

fn lower_structured_equations<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    rows: StructuredEquationRows<'_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    for (family_index, family) in rows.families.iter().enumerate() {
        if rows.excluded_families.contains(&family_index)
            || rows.environment.is_some_and(|environment| {
                materialized_discrete_real_family(family, environment.roles)
                    || materialized_discrete_value_rows(family, rows.equations, environment.roles)
            })
        {
            continue;
        }
        let owner = equation_owner_provenance(&family.origin, family.span)?;
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
                    domain,
                    scalar_view: template.scalar_view,
                    binders: &binders,
                    shapes: &scoped_shapes,
                    environment: rows.environment,
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

struct StructuredTemplatePartitionInput<'scope, 'flat, 'dae> {
    coordinates: &'scope HashMap<VarName, Coordinate<'dae>>,
    functions: &'scope FunctionRegistry<'flat, 'dae>,
    family: &'scope flat::StructuredEquationFamily,
    domain: dae::DomainId<'dae>,
    scalar_view: rumoca_core::ComprehensionScalarView,
    binders: &'scope HashMap<VarName, dae::DomainBinderId<'dae>>,
    shapes: &'scope ShapeEnvironment,
    environment: Option<StructuredEquationEnvironment<'scope, 'dae>>,
    owner: dae::DaeProvenance,
}

fn lower_partitioned_structured_template<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    input: StructuredTemplatePartitionInput<'_, '_, 'dae>,
) -> Result<bool, dae::DaeConstructionError> {
    let Some(environment) = input.environment else {
        return Ok(false);
    };
    let template = input
        .family
        .template
        .as_ref()
        .expect("partitioned structured lowering receives a template family");
    let assignments = match structured_family_partition(input.family, template, environment) {
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

fn structured_family_partition<'flat>(
    family: &'flat flat::StructuredEquationFamily,
    template: &'flat rumoca_core::ComprehensionTemplate,
    environment: StructuredEquationEnvironment<'flat, '_>,
) -> StructuredFamilyPartition<'flat> {
    let assignments = template
        .body
        .iter()
        .enumerate()
        .map(|(ordinal, body)| {
            // Materialized family rows and their compact template are two
            // views of one semantic owner. Consult the authoritative row
            // claim for every origin, so an aggregate owner constructed from
            // exact element coverage consumes the template view as well.
            if family.interiors_materialized {
                let row = family.first_equation_index + ordinal;
                let equation = &environment.flat.equations[row];
                return match equation_partition(
                    environment.flat,
                    row,
                    equation,
                    environment.roles,
                    environment.connection_ranks,
                    environment.aggregate_connections,
                )
                .expect("analysis validates structured connection ownership")
                {
                    EquationPartition::DiscreteValue(plan) => Some(Ok(plan)),
                    EquationPartition::ConsumedDiscreteValue => Some(Err(())),
                    EquationPartition::Continuous
                    | EquationPartition::DiscreteReal { .. }
                    | EquationPartition::MultiOutput { .. }
                    | EquationPartition::DiscreteElements(_) => None,
                };
            }
            discrete_value_assignment(body, environment.roles, family.span)
                .expect("analysis validates structured equation partition ownership")
                .map(Ok)
        })
        .collect::<Vec<_>>();
    if assignments.iter().all(Option::is_none) {
        return StructuredFamilyPartition::Continuous;
    }
    if assignments
        .iter()
        .all(|assignment| matches!(assignment, Some(Err(()))))
    {
        return StructuredFamilyPartition::ConsumedDiscreteValue;
    }
    StructuredFamilyPartition::DiscreteValue(
        assignments
            .into_iter()
            .map(|assignment| {
                assignment
                    .expect("analysis prohibits a mixed structured equation partition")
                    .expect("analysis prohibits mixed consumed and owning discrete families")
            })
            .collect(),
    )
}

struct StructuredDiscreteFamilyInput<'scope, 'flat, 'dae> {
    coordinates: &'scope HashMap<VarName, Coordinate<'dae>>,
    functions: &'scope FunctionRegistry<'flat, 'dae>,
    family: &'scope flat::StructuredEquationFamily,
    domain: dae::DomainId<'dae>,
    scalar_view: rumoca_core::ComprehensionScalarView,
    binders: &'scope HashMap<VarName, dae::DomainBinderId<'dae>>,
    shapes: &'scope ShapeEnvironment,
    assignments: &'scope [DiscreteValueAssignmentPlan<'flat>],
    environment: StructuredEquationEnvironment<'scope, 'dae>,
    owner: dae::DaeProvenance,
}

/// Select the scalar view a structured discrete-value family lowers with.
///
/// A source `for` equation over a discrete array carries the default
/// `BinderSubstitution` view: each domain point owns one scalar body. Its
/// element rows (`x[i] = e`) are claimed by the aggregate discrete owner, which
/// packs them into one row-major aggregate value spanning the whole coordinate
/// and records that packing with `scalar_count`. A packed aggregate value is a
/// whole tensor, not a scalar body, so it must be projected row-major over the
/// domain rather than substituted point by point. When every assignment carries
/// a packed aggregate value the family lowers as `RowMajorProjection`; a genuine
/// per-point scalar plan keeps the declared view.
fn discrete_family_scalar_view(
    declared: rumoca_core::ComprehensionScalarView,
    assignments: &[DiscreteValueAssignmentPlan<'_>],
) -> rumoca_core::ComprehensionScalarView {
    if !assignments.is_empty() && assignments.iter().all(|plan| plan.scalar_count.is_some()) {
        return rumoca_core::ComprehensionScalarView::RowMajorProjection;
    }
    declared
}

fn lower_structured_discrete_family<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    input: StructuredDiscreteFamilyInput<'_, '_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let owner_clock = input
        .environment
        .clocked_owners
        .get(&input.family.first_equation_index)
        .map(|plan| input.environment.clocks.id(plan, input.family.span))
        .transpose()?;
    let scalar_view = discrete_family_scalar_view(input.scalar_view, input.assignments);
    let semantic_owner = discrete_values
        .structured_owner(
            input.owner,
            input.domain,
            scalar_view,
            input.assignments.iter().map(|plan| plan.target.clone()),
            input.coordinates,
            input.environment.topology,
        )?
        .expect("a structured discrete family has one planned B.1c owner");
    for plan in input.assignments {
        let symbols = LoweringSymbols {
            coordinates: input.coordinates,
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
        let Coordinate::DiscreteValue(target) = input.coordinates[plan.target] else {
            unreachable!("analysis classifies the family target as discrete-valued")
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
        .map(u32::try_from)
        .collect::<Result<Vec<_>, _>>()
        .ok();
    if let Some(base) = checked_extents
        .as_deref()
        .map(|extents| {
            construction.expressions(|expressions| {
                expressions.exact_row_major_projection_base(scalars, extents, provenance)
            })
        })
        .transpose()?
        .flatten()
    {
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

struct EquationRows<'scope, 'dae> {
    flat: &'scope flat::Model,
    equations: &'scope [flat::Equation],
    excluded: &'scope HashSet<usize>,
    records: &'scope HashMap<usize, RecordEquationPlan>,
    multi_output: &'scope HashMap<usize, MultiOutputEquationPlan>,
    roles: &'scope HashMap<VarName, PlannedRole>,
    connection_ranks: &'scope HashMap<VarName, usize>,
    aggregate_connections: &'scope AggregateDiscreteConnections,
    topology: &'scope DiscreteValueTopologyPlan,
    clocked_owners: &'scope HashMap<usize, ClockPlan>,
    clocks: &'scope LoweredClocks<'dae>,
    /// MLS §3.7.4.5 Rule 1 / Rule 2 replacements proven by analysis. A row
    /// listed here is lowered from the rule's residual instead of the source
    /// one; the row count, its owner, and its balance contribution are
    /// unchanged, which is why the rule needs no separate equation identity.
    semi_linear: &'scope SemiLinearRules,
    initialization: bool,
}

impl<'scope> EquationRows<'scope, '_> {
    fn partition(
        &'scope self,
        row: usize,
        equation: &'scope flat::Equation,
    ) -> EquationPartition<'scope> {
        equation_partition(
            self.flat,
            row,
            equation,
            self.roles,
            self.connection_ranks,
            self.aggregate_connections,
        )
        .expect("analysis already validates equation ownership")
    }
}

fn lower_equations<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    input: EquationRows<'_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    for (index, equation) in input.equations.iter().enumerate() {
        if input.excluded.contains(&index) {
            continue;
        }
        let owner = equation_owner_provenance(&equation.origin, equation.span)?;
        let generation = equation_generation(&equation.origin);
        let owner_clock = input
            .clocked_owners
            .get(&index)
            .map(|plan| input.clocks.id(plan, equation.span))
            .transpose()?;
        if let Some(plan) = input.multi_output.get(&index) {
            lower_multi_output_equation(
                construction,
                coordinates,
                functions,
                equation,
                plan,
                owner,
                (!input.initialization).then_some(MultiOutputDiscreteOwners {
                    discrete_values: &mut *discrete_values,
                    topology: input.topology,
                    owner_clock,
                }),
            )?;
            continue;
        }
        if let Some(plan) = input.records.get(&index) {
            lower_record_equation(
                construction,
                coordinates,
                functions,
                (equation, plan),
                owner,
                (!input.initialization).then_some(MultiOutputDiscreteOwners {
                    discrete_values: &mut *discrete_values,
                    topology: input.topology,
                    owner_clock,
                }),
            )?;
            continue;
        }
        lower_ordinary_equation(
            construction,
            discrete_values,
            coordinates,
            functions,
            OrdinaryEquationRow {
                input: &input,
                index,
                equation,
                owner,
                generation,
                owner_clock,
            },
        )?;
    }
    Ok(())
}

fn equation_generation(origin: &flat::EquationOrigin) -> Option<dae::DaeGeneration> {
    match origin {
        flat::EquationOrigin::ComponentEquation { .. } => None,
        flat::EquationOrigin::Connection { .. } => Some(dae::DaeGeneration::ConnectionEquation),
        flat::EquationOrigin::FlowSum { .. } | flat::EquationOrigin::UnconnectedFlow { .. } => {
            Some(dae::DaeGeneration::FlowBalanceEquation)
        }
        flat::EquationOrigin::Algorithm { .. } => Some(dae::DaeGeneration::AlgorithmEquation),
        flat::EquationOrigin::Reinit { .. } => Some(dae::DaeGeneration::EventActionLowering),
        flat::EquationOrigin::WhenAssignment { .. } => Some(dae::DaeGeneration::DiscreteUpdate),
        flat::EquationOrigin::Binding { .. } => Some(dae::DaeGeneration::BindingEquation),
    }
}

fn equation_owner_provenance(
    origin: &flat::EquationOrigin,
    span: Span,
) -> Result<dae::DaeProvenance, dae::DaeConstructionError> {
    match equation_generation(origin) {
        Some(generation) => dae::DaeProvenance::generated(generation, span),
        None => dae::DaeProvenance::source(span),
    }
}
