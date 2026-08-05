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

mod algorithm;
mod algorithm_lowering;
mod analysis;
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
mod initial_discrete_values;
mod model_algorithm;
mod model_events;
mod record_equation;
mod structured_body;
mod variable_construction;
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
    FunctionIntegerReduction, FunctionLoopLowering, FunctionPlan, FunctionRecordAssemblyPlan,
    FunctionRecordCallAssemblyPlan, FunctionRecordFieldAssembly, FunctionRecordFieldAssemblyPlan,
    FunctionStatementPlan, FunctionValueSeed, ModelAlgorithmPlan, ModelEventTensorLoopPlan,
    MultiOutputEquationPlan, PlannedRole, RecordArrayFieldPlan, RecordArrayFieldPlans,
    RecordEquationFieldValue, RecordEquationPlan, SemiLinearRules, analyze,
    assigned_function_targets, discrete_value_assignment, effective_function_scalar_type,
    effective_variable_scalar_type, empty_array_bound_to_declaration, equation_partition,
    function_assertion, function_record_field_name, is_event_condition,
    is_inferred_clock_condition, is_whole_clock_coordinate, model_algorithm_targets,
    record_field_projections, selected_conditional_statements, specialized_comprehension_plan,
    structured_assignment_names,
};
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
    lower_clocked_model_algorithm_expression, lower_coordinate_reference, lower_expression,
    lower_expression_scoped, lower_function_array_update, lower_function_expression,
    lower_function_expression_scoped, lower_model_algorithm_expression,
    lower_scoped_model_algorithm_expression, planned_input_variability, require_span,
    variable_attribute_expressions,
};
use function_array_assembly::lower_function_array_assembly;
use function_body::{
    FunctionConditional, FunctionFold, TotalArrayDefinition, flattened_function_loop,
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
    FunctionShapeAnalysis, FunctionShapeCertificate, FunctionSpecializationKey, ShapeEnvironment,
    ValueShape, call_free_expression_shape, call_free_target_shape, evaluate_shape_integer,
    infer_function_integer_bounds, proven_conditional_branch,
};
use model_algorithm::{
    ModelAlgorithmLowering, lower_declarative_model_algorithm,
    lower_separated_array_sum_model_algorithm, lower_total_array_model_algorithm,
};
use model_events::{WhenChainsRequest, always_condition, lower_when_assignment, lower_when_chains};
use record_equation::lower_record_equation;
use structured_body::lower_structured_body;
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
    role: PlannedRole,
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

    dae::Dae::construct(source_map, |construction| {
        build_checked(flat, &analysis, &variable_plan, construction)
    })
    .map_err(ToDaeError::from)
}

pub(crate) fn balance_detail(flat: &flat::Model) -> Result<BalanceDetail, ToDaeError> {
    analyze(flat).map(|analysis| analysis.balance)
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
    let clocks = lower_analysis_clocks(construction, flat, analysis)?;
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
            coordinate_instances: coordinates.by_instance(),
            expression_events: &analysis.expression_events,
            sample_alias_schedules: &analysis.sample_alias_schedules,
            clocked_coordinate_owners: &analysis.clocked_coordinate_owners,
            clocks: &clocks,
        },
        &analysis.function_plans,
    )?;
    let functions = FunctionRegistry {
        flat,
        shapes: &analysis.function_shapes,
        ids: &function_ids,
        comprehension_plans: &analysis.comprehension_plans,
        record_array_fields: &analysis.record_array_fields,
        constants: &analysis.constants,
        delay_plans: &analysis.delay_plans,
        coordinate_instances: coordinates.by_instance(),
        expression_events: &analysis.expression_events,
        sample_alias_schedules: &analysis.sample_alias_schedules,
        clocked_coordinate_owners: &analysis.clocked_coordinate_owners,
        clocks: &clocks,
    };
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
    lower_clocks(
        construction,
        flat,
        &analysis.clock_plans,
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

#[derive(Clone, Copy)]
struct FunctionSymbols<'symbols, 'dae> {
    coordinates: &'symbols HashMap<VarName, Coordinate<'dae>>,
    functions: &'symbols FunctionRegistry<'symbols, 'dae>,
    shapes: &'symbols ShapeEnvironment,
}

fn lower_function_statements<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    debug_assert_eq!(statements.len(), plans.len());
    let mut index = 0usize;
    while index < statements.len() {
        let statement = &statements[index];
        let plan = &plans[index];
        if let FunctionStatementPlan::ArrayAssembly(assembly) = plan {
            let statement_count = assembly.direct_count + usize::from(assembly.loop_plan.is_some());
            lower_function_array_assembly(
                construction,
                symbols,
                &mut body,
                &statements[index..index + statement_count],
                assembly,
            )?;
            index += statement_count;
            continue;
        }
        if let FunctionStatementPlan::RecordAssembly(assembly) = plan {
            lower_function_record_assembly(
                construction,
                symbols,
                &mut body,
                &statements[index..index + assembly.statement_count],
                assembly,
            )?;
            index += assembly.statement_count;
            continue;
        }
        if let FunctionStatementPlan::RecordFieldAssembly(assembly) = plan {
            lower_function_record_field_assembly(
                construction,
                symbols,
                &mut body,
                &statements[index..index + assembly.statement_count],
                assembly,
            )?;
            index += assembly.statement_count;
            continue;
        }
        body = lower_function_statement(construction, symbols, body, statement, plan)?;
        index += 1;
    }
    Ok(body)
}

fn lower_function_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    statement: &rumoca_core::Statement,
    plan: &FunctionStatementPlan,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    match (statement, plan) {
        (_, FunctionStatementPlan::ProvenAssertion) => Ok(body),
        (statement, FunctionStatementPlan::RuntimeAssertion) => {
            lower_runtime_function_assertion(construction, symbols, body, statement)
        }
        (
            _,
            FunctionStatementPlan::GeneratedBooleanAssignment {
                target,
                value,
                span,
                ..
            },
        ) => lower_generated_boolean_assignment(construction, symbols, body, target, value, *span),
        (
            rumoca_core::Statement::Assignment { value, span, .. },
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
            rumoca_core::Statement::For {
                indices,
                equations,
                span,
            },
            FunctionStatementPlan::For {
                domain,
                binder_spans,
                lowering,
                statements,
                source_depth,
            },
        ) => lower_function_loop(
            construction,
            symbols,
            body,
            FunctionLoop {
                indices,
                source_statements: equations,
                span: *span,
                domain,
                binder_spans,
                lowering,
                plans: statements,
                source_depth: *source_depth,
            },
        ),
        (
            statement @ rumoca_core::Statement::If { .. },
            FunctionStatementPlan::If { .. } | FunctionStatementPlan::ProvenBranch { .. },
        ) => lower_function_conditional_statement(construction, symbols, body, statement, plan),
        (
            rumoca_core::Statement::FunctionCall {
                comp, args, span, ..
            },
            FunctionStatementPlan::MultiOutputCall { outputs },
        ) => lower_multi_output_statement(construction, symbols, body, comp, args, *span, outputs),
        (
            rumoca_core::Statement::FunctionCall {
                comp, args, span, ..
            },
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
        (_, FunctionStatementPlan::ArrayAssemblyMember) => {
            unreachable!("array assembly members are consumed by their leading owner")
        }
        (_, FunctionStatementPlan::RecordAssemblyMember) => {
            unreachable!("record assembly members are consumed by their leading owner")
        }
        (_, FunctionStatementPlan::RecordFieldAssemblyMember) => {
            unreachable!("record field members are consumed by their staged owner")
        }
        (_, FunctionStatementPlan::RecordFieldAssembly(_)) => {
            unreachable!("record field assemblies lower with their source run")
        }
        _ => unreachable!("function analysis and construction plans remain aligned"),
    }
}

fn lower_multi_output_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    callee: &rumoca_core::ComponentReference,
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
    statement: &rumoca_core::Statement,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let assertion = function_assertion(statement, symbols.functions.flat)
        .expect("analysis already validates the assertion statement")
        .expect("a runtime assertion plan owns an assertion statement");
    let condition = lower_function_expression(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        &body,
        assertion.condition,
    )?;
    let message = lower_function_expression(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        &body,
        assertion.message,
    )?;
    let provenance = dae::DaeProvenance::source(assertion.span)?;
    construction
        .functions(|functions| functions.assertion(&mut body, condition, message, provenance))?;
    Ok(body)
}

fn lower_record_multi_output_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    callee: &rumoca_core::ComponentReference,
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
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let rumoca_core::Statement::If {
        cond_blocks,
        else_block,
        span,
    } = statement
    else {
        unreachable!("a conditional plan owns a conditional statement")
    };
    match plan {
        FunctionStatementPlan::If {
            branches,
            fallback,
            targets,
        } => {
            let binders = HashMap::new();
            lower_function_conditional(
                construction,
                &mut body,
                FunctionConditional {
                    symbols,
                    binders: &binders,
                    blocks: cond_blocks,
                    fallback: else_block.as_deref(),
                    branch_plans: branches,
                    fallback_plans: fallback.as_deref(),
                    targets,
                    span: *span,
                },
            )?;
            Ok(body)
        }
        FunctionStatementPlan::ProvenBranch {
            selected,
            statements,
        } => {
            let selected =
                selected_conditional_statements(cond_blocks, else_block.as_deref(), *selected);
            lower_function_statements(construction, symbols, body, selected, statements)
        }
        _ => unreachable!("function analysis and construction plans remain aligned"),
    }
}

struct FunctionAssignment<'statement> {
    value: &'statement Expression,
    span: Span,
    plan: &'statement FunctionAssignmentPlan,
}

struct FunctionMultiOutputCall<'statement> {
    callee: &'statement rumoca_core::ComponentReference,
    args: &'statement [Expression],
    span: Span,
    outputs: &'statement [Option<FunctionAssignmentPlan>],
}

/// Lower one MLS §11.2.1.1 multi-result call statement.
///
/// The call's *arguments* are lowered once and each read result ordinal becomes
/// the `call(function, ordinal, ..)` node that defines its receiving variable —
/// the same owner an MLS §11.2.1 single-result assignment builds at ordinal 0.
/// An omitted receiver reads no result, so it mints no expression.
///
/// Only the arguments are shared: reading k results denotes k evaluations of
/// the callee body, not the one evaluation MLS §12.4.3 describes. That is
/// observationally equal only for a pure callee, which is why an impure
/// external callee is refused by name during planning. See
/// [`expression::LoweredCallOperands`] for the cost this leaves behind.
fn lower_function_multi_output_call<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    call: FunctionMultiOutputCall<'_>,
) -> Result<(), dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::source(call.span)?;
    let reference = rumoca_core::Reference::from_component_reference(call.callee.clone());
    let binders = HashMap::new();
    let operands = lower_call_operands(
        construction,
        LoweringSymbols {
            coordinates: symbols.coordinates,
            functions: symbols.functions,
            shapes: symbols.shapes,
            function_body: Some(body),
            values: None,
            owner_clock: None,
        },
        &binders,
        &reference,
        call.args,
        provenance,
    )?;
    for (ordinal, plan) in call.outputs.iter().enumerate() {
        let Some(plan) = plan else {
            continue;
        };
        let target = function_value_coordinate(symbols.coordinates, plan.target());
        let mut value = operands.result(construction, ordinal, provenance)?;
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
        construction.functions(|owner| owner.assign(body, target, value, provenance))?;
    }
    Ok(())
}

fn lower_function_record_multi_output_assembly<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    callee: &rumoca_core::ComponentReference,
    args: &[Expression],
    span: Span,
    plan: &FunctionRecordCallAssemblyPlan,
) -> Result<(), dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::source(span)?;
    let reference = rumoca_core::Reference::from_component_reference(callee.clone());
    let operands = lower_call_operands(
        construction,
        LoweringSymbols {
            coordinates: symbols.coordinates,
            functions: symbols.functions,
            shapes: symbols.shapes,
            function_body: Some(body),
            values: None,
            owner_clock: None,
        },
        &HashMap::new(),
        &reference,
        args,
        provenance,
    )?;
    let fields = plan
        .fields
        .iter()
        .map(|field| operands.result(construction, field.result_ordinal, provenance))
        .collect::<Result<Vec<_>, _>>()?;
    let target = function_value_coordinate(symbols.coordinates, &plan.target);
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
    let target = function_value_coordinate(symbols.coordinates, assignment.plan.target());
    let mut value = lower_function_expression(
        construction,
        symbols.coordinates,
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
    source_statements: &'statement [rumoca_core::Statement],
    span: Span,
    domain: &'statement StructuredIndexDomain,
    binder_spans: &'statement [Span],
    lowering: &'statement FunctionLoopLowering,
    plans: &'statement [FunctionStatementPlan],
    source_depth: usize,
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
    let (indices, statements) =
        flattened_function_loop(input.indices, input.source_statements, input.source_depth);
    let binders = lower_function_binders(construction, domain, &indices, input.binder_spans)?;
    let mut loop_shapes = symbols.shapes.clone();
    for binder in binders.keys() {
        // A loop binder is a scalar whose value varies over the iteration, so
        // it shadows any enclosing coordinate's proven value (MLS §11.2.2).
        loop_shapes.insert(binder.clone(), Vec::new());
    }
    let loop_symbols = FunctionSymbols {
        coordinates: symbols.coordinates,
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
                    statements,
                    plans: input.plans,
                    owner,
                },
            )?;
            Ok(body)
        }
        FunctionLoopLowering::Fold { targets } => lower_function_fold(
            construction,
            loop_symbols,
            body,
            FunctionFold {
                domain,
                binders: &binders,
                statements,
                plans: input.plans,
                targets,
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
        if rows.excluded_families.contains(&family_index) {
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
                    EquationPartition::Continuous | EquationPartition::DiscreteReal { .. } => None,
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
    let semantic_owner = discrete_values
        .structured_owner(
            input.owner,
            input.domain,
            input.scalar_view,
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

struct OrdinaryEquationRow<'input, 'scope, 'dae> {
    input: &'input EquationRows<'scope, 'dae>,
    index: usize,
    equation: &'scope flat::Equation,
    owner: dae::DaeProvenance,
    generation: Option<dae::DaeGeneration>,
    owner_clock: Option<dae::PeriodicClockId<'dae>>,
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
            )?;
            continue;
        }
        if let Some(plan) = input.records.get(&index) {
            lower_record_equation(
                construction,
                coordinates,
                functions,
                equation,
                plan,
                owner,
                input.initialization,
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

fn lower_ordinary_equation<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    row: OrdinaryEquationRow<'_, '_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let OrdinaryEquationRow {
        input,
        index,
        equation,
        owner,
        generation,
        owner_clock,
    } = row;
    if input.initialization {
        let residual = lower_expression(
            construction,
            coordinates,
            functions,
            &equation.residual,
            generation,
        )?;
        construction.initialization(|system| system.value_equation(owner, residual))?;
        return Ok(());
    }
    match input.partition(index, equation) {
        EquationPartition::Continuous => {
            let (source, generation) = match input.semi_linear.residual(index) {
                Some(replacement) => (replacement, Some(dae::DaeGeneration::SemiLinearLowering)),
                None => (&equation.residual, generation),
            };
            let residual = lower_equation_expression(
                construction,
                coordinates,
                functions,
                owner_clock,
                source,
                generation,
            )?;
            construction.continuous(|system| system.value_equation(owner, residual))?;
        }
        EquationPartition::DiscreteReal { .. } => {
            let residual = lower_equation_expression(
                construction,
                coordinates,
                functions,
                owner_clock,
                &equation.residual,
                generation,
            )?;
            construction.discrete(|system| {
                system.real_equation(owner, |equation| equation.residual(residual))
            })?;
        }
        EquationPartition::DiscreteValue(plan) => {
            let generation = if plan.generated {
                Some(dae::DaeGeneration::DiscreteUpdate)
            } else {
                generation
            };
            let value = lower_equation_expression(
                construction,
                coordinates,
                functions,
                owner_clock,
                plan.value.as_ref(),
                generation,
            )?;
            let Coordinate::DiscreteValue(target) = coordinates[plan.target] else {
                unreachable!("analysis classifies the equation target as discrete-valued")
            };
            let semantic_owner = discrete_values
                .owner(owner, [plan.target.clone()], coordinates, input.topology)?
                .expect("a discrete equation has one planned B.1c owner");
            discrete_values.always(
                semantic_owner,
                target,
                value,
                owner,
                dae::DaeProvenance::source(equation.span)?,
            )?;
        }
        EquationPartition::ConsumedDiscreteValue => {}
    }
    Ok(())
}

fn lower_multi_output_equation<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    equation: &flat::Equation,
    plan: &MultiOutputEquationPlan,
    owner: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &equation.residual
    else {
        unreachable!("a multi-output equation plan owns a subtraction residual")
    };
    let Expression::Tuple { elements, .. } = lhs.as_ref() else {
        unreachable!("a multi-output equation plan owns a receiving tuple")
    };
    let Expression::FunctionCall {
        name,
        args,
        is_constructor: false,
        span,
    } = rhs.as_ref()
    else {
        unreachable!("a multi-output equation plan owns a function call")
    };
    let provenance = dae::DaeProvenance::source(*span)?;
    let symbols = LoweringSymbols {
        coordinates,
        functions,
        shapes: functions.shapes.model_values(),
        function_body: None,
        values: None,
        owner_clock: None,
    };
    let call = lower_call_operands(
        construction,
        symbols,
        &HashMap::new(),
        name,
        args,
        provenance,
    )?;
    for (ordinal, target) in plan.outputs.iter().enumerate() {
        if target.is_none() {
            continue;
        }
        let lhs = lower_expression(
            construction,
            coordinates,
            functions,
            &elements[ordinal],
            None,
        )?;
        let rhs = call.result(construction, ordinal, provenance)?;
        let residual = generated_residual(construction, owner, lhs, rhs)?;
        construction.continuous(|system| system.value_equation(owner, residual))?;
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

#[cfg(test)]
mod tests;
