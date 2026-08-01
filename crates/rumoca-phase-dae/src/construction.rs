use std::collections::{HashMap, HashSet};

use rumoca_core::{
    BuiltinFunction, Causality, ClockLattice, ClockRational, Expression, InstanceId, Literal,
    OpBinary, OpUnary, SourceMap, Span, StructuredIndexBinder, StructuredIndexDomain, Subscript,
    VarName, Variability,
};
use rumoca_eval_flat::constant::{EvalContext, Value as EvalValue, eval_expr};
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

use crate::ToDaeError;
use crate::balance::BalanceDetail;

mod algorithm;
mod analysis;
mod clocks;
mod discrete_values;
mod enumeration_conversion;
mod equation_systems;
mod expression;
mod function_array_assembly;
mod function_body;
mod function_construction;
mod function_external;
mod function_record_assembly;
mod function_shapes;
mod initial_discrete_values;
mod model_algorithm;
mod model_events;
mod record_equation;
mod structured_body;
mod variable_construction;
use algorithm::{
    AlgorithmFunctionCall, AlgorithmStatementContext, lower_algorithm_assignment,
    lower_algorithm_function_call, own_clocked_algorithm_targets,
};
use analysis::{
    Analysis, ClockPlan, ComprehensionKey, ComprehensionPlan, DelayPlan, DerivedParameterPlan,
    DiscreteValueTopologyPlan, EquationPartition, ExpressionEventPlan, ExpressionEventPlans,
    ExternalArgumentPlan, ExternalFunctionPlan, FunctionArrayAssemblyPlan, FunctionAssignmentPlan,
    FunctionIntegerReduction, FunctionLoopLowering, FunctionPlan, FunctionRecordAssemblyPlan,
    FunctionStatementPlan, FunctionValueSeed, ModelAlgorithmPlan, PlannedRole,
    RecordArrayFieldPlan, RecordArrayFieldPlans, RecordEquationPlan, SemiLinearRules, analyze,
    effective_function_scalar_type, effective_variable_scalar_type,
    empty_array_bound_to_declaration, equation_partition, is_inferred_clock_condition,
    is_whole_clock_coordinate, model_algorithm_targets, record_field_projections,
    structured_assignment_names,
};
use clocks::{LoweredClocks, lower_clocked_value_owners, lower_clocks};
use discrete_values::{DiscreteValueOwnerHandle, DiscreteValueStaging};
use enumeration_conversion::{
    enumeration_conversion, enumeration_range_ordinals, enumeration_range_type,
    has_enumeration_range_bound, is_flat_enumeration_literal,
};
use equation_systems::{lower_equation_expression, lower_equation_systems};
use expression::{
    FunctionArrayUpdate, FunctionCallLowering, LoweringSymbols, all_model_expressions,
    classify_function_call, derivative_reference, expression_children, expression_span,
    lower_clocked_expression, lower_coordinate_reference, lower_expression,
    lower_expression_scoped, lower_function_array_update, lower_function_expression,
    lower_function_expression_scoped, lower_model_algorithm_expression, planned_input_variability,
    require_span, variable_attribute_expressions,
};
use function_array_assembly::lower_function_array_assembly;
use function_body::{
    FunctionConditional, FunctionFold, TotalArrayDefinition, flattened_function_loop,
    function_value_coordinate, lower_function_conditional, lower_function_fold,
    lower_function_value_seed, lower_guarded_function_return, lower_integer_reduction,
    lower_total_function_array_definition,
};
use function_construction::{
    FunctionRegistry, FunctionRegistryInput, construct_functions, function_value_type,
};
use function_external::define_external_function;
use function_record_assembly::lower_function_record_assembly;
use function_shapes::{
    FunctionShapeAnalysis, FunctionSpecializationKey, ShapeEnvironment, ValueShape,
    evaluate_shape_integer,
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

    fn previous(self, span: Span) -> Result<dae::CoordinateInput<'dae>, ToDaeError> {
        match self {
            Self::DiscreteReal(id) => Ok(dae::CoordinateInput::PreDiscreteReal(id)),
            Self::DiscreteValue(id) => Ok(dae::CoordinateInput::PreDiscreteValue(id)),
            Self::Parameter(_)
            | Self::Input(_)
            | Self::State(_)
            | Self::Algebraic(_)
            | Self::FunctionParameter(_)
            | Self::FunctionValue(_) => Err(ToDaeError::unsupported_flat(
                "pre expression",
                "pre(...) must name a discrete coordinate in canonical DAE",
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
    let no_function_ids = HashMap::new();
    let no_coordinate_instances = HashMap::new();
    let analysis_functions = FunctionRegistry {
        flat,
        shapes: &analysis.function_shapes,
        ids: &no_function_ids,
        comprehension_plans: &analysis.comprehension_plans,
        record_array_fields: &analysis.record_array_fields,
        constants: &analysis.constants,
        delay_plans: &analysis.delay_plans,
        reinit_state_pre: &analysis.reinit_state_pre,
        coordinate_instances: &no_coordinate_instances,
        expression_events: &analysis.expression_events,
    };
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
            reinit_state_pre: &analysis.reinit_state_pre,
            coordinate_instances: coordinates.by_instance(),
            expression_events: &analysis.expression_events,
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
        reinit_state_pre: &analysis.reinit_state_pre,
        coordinate_instances: coordinates.by_instance(),
        expression_events: &analysis.expression_events,
    };
    define_reserved_variables(
        construction,
        VariableDefinitionContext {
            coordinates: &coordinates,
            functions: &functions,
            assigned_discrete_targets: &analysis.assigned_discrete_targets,
            derived_parameters: &analysis.derived_parameters,
            initial_parameters: &analysis.initial_parameters,
        },
        variable_plan,
        variable_identities.reserved,
    )?;
    let clocks = lower_clocks(construction, flat, &analysis.clock_plans)?;
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
            let statement_count = assembly.direct_count + 1;
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
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                span,
            },
            FunctionStatementPlan::If {
                branches,
                fallback,
                targets,
            },
        ) => {
            lower_function_conditional(
                construction,
                &mut body,
                FunctionConditional {
                    symbols,
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
        (_, FunctionStatementPlan::ArrayAssemblyMember) => {
            unreachable!("array assembly members are consumed by their leading owner")
        }
        (_, FunctionStatementPlan::RecordAssemblyMember) => {
            unreachable!("record assembly members are consumed by their leading owner")
        }
        _ => unreachable!("function analysis and construction plans remain aligned"),
    }
}

struct FunctionAssignment<'statement> {
    value: &'statement Expression,
    span: Span,
    plan: &'statement FunctionAssignmentPlan,
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
    loop_shapes.extend(binders.keys().cloned().map(|binder| (binder, Vec::new())));
    let loop_symbols = FunctionSymbols {
        coordinates: symbols.coordinates,
        functions: symbols.functions,
        shapes: &loop_shapes,
    };
    match input.lowering {
        FunctionLoopLowering::TotalArrayDefinition => {
            lower_total_function_array_definition(
                construction,
                &mut body,
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

fn lower_assertions<'dae, 'flat>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
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
            events.assert_with_level(action_guard, action_guard, message, level, provenance)
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
struct AlgorithmEnvironment<'scope, 'shape, 'dae> {
    coordinates: &'scope HashMap<VarName, Coordinate<'dae>>,
    functions: &'scope FunctionRegistry<'shape, 'dae>,
    sample_lattices: &'scope [(Span, ClockLattice)],
}

#[derive(Clone, Copy)]
struct AlgorithmOwner<'dae> {
    discrete_owner: Option<DiscreteValueOwnerHandle>,
    parent: Option<EventGuard<'dae>>,
    span: Span,
}

struct ModelAlgorithmsRequest<'scope, 'shape, 'dae> {
    flat: &'scope flat::Model,
    environment: AlgorithmEnvironment<'scope, 'shape, 'dae>,
    plans: &'scope [ModelAlgorithmPlan],
    topology: &'scope DiscreteValueTopologyPlan,
}

fn lower_algorithms<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    request: ModelAlgorithmsRequest<'_, '_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    debug_assert_eq!(request.flat.algorithms.len(), request.plans.len());
    for (algorithm, plan) in request.flat.algorithms.iter().zip(request.plans) {
        let owner_provenance =
            dae::DaeProvenance::generated(dae::DaeGeneration::AlgorithmEquation, algorithm.span)?;
        let discrete_owner = discrete_values.owner(
            owner_provenance,
            model_algorithm_targets(request.flat, algorithm),
            request.environment.coordinates,
            request.topology,
        )?;
        let mut lowering = ModelAlgorithmLowering {
            construction,
            discrete_values,
            discrete_owner,
            coordinates: request.environment.coordinates,
            functions: request.environment.functions,
        };
        match plan {
            ModelAlgorithmPlan::Declarative { target } => {
                lower_declarative_model_algorithm(&mut lowering, algorithm, target)?;
            }
            ModelAlgorithmPlan::TotalArrayDefinition {
                target,
                domain,
                binder_spans,
            } => {
                lower_total_array_model_algorithm(
                    &mut lowering,
                    algorithm,
                    target,
                    domain,
                    binder_spans,
                )?;
            }
            ModelAlgorithmPlan::SeparatedArraySum {
                array_target,
                scalar_target,
                domain,
                binder_spans,
            } => {
                lower_separated_array_sum_model_algorithm(
                    &mut lowering,
                    algorithm,
                    array_target,
                    scalar_target,
                    domain,
                    binder_spans,
                )?;
            }
            ModelAlgorithmPlan::Event => {
                lower_algorithm_statements(
                    lowering.construction,
                    lowering.discrete_values,
                    request.environment,
                    AlgorithmOwner {
                        discrete_owner,
                        parent: None,
                        span: algorithm.span,
                    },
                    &algorithm.statements,
                )?;
            }
        }
    }
    Ok(())
}

fn lower_algorithm_statements<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    environment: AlgorithmEnvironment<'_, '_, 'dae>,
    owner: AlgorithmOwner<'dae>,
    statements: &[rumoca_core::Statement],
) -> Result<(), dae::DaeConstructionError> {
    for statement in statements {
        lower_algorithm_statement(construction, discrete_values, environment, owner, statement)?;
    }
    Ok(())
}

fn lower_algorithm_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    environment: AlgorithmEnvironment<'_, '_, 'dae>,
    owner: AlgorithmOwner<'dae>,
    statement: &rumoca_core::Statement,
) -> Result<(), dae::DaeConstructionError> {
    let context = AlgorithmStatementContext {
        coordinates: environment.coordinates,
        functions: environment.functions,
        parent: owner.parent,
        owner_span: owner.span,
    };
    match statement {
        rumoca_core::Statement::Assignment { comp, value, span } => lower_algorithm_assignment(
            construction,
            discrete_values,
            owner.discrete_owner,
            context,
            comp,
            value,
            *span,
        ),
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            span,
        } => lower_algorithm_if(
            construction,
            discrete_values,
            environment,
            owner,
            cond_blocks,
            else_block.as_deref().unwrap_or_default(),
            *span,
        ),
        rumoca_core::Statement::When { blocks, span } => lower_algorithm_when(
            construction,
            discrete_values,
            environment,
            owner,
            blocks,
            *span,
        ),
        rumoca_core::Statement::FunctionCall {
            comp,
            args,
            outputs,
            span,
        } => lower_algorithm_function_call(
            construction,
            discrete_values,
            owner.discrete_owner,
            context,
            AlgorithmFunctionCall {
                component: comp,
                arguments: args,
                outputs,
                span: *span,
            },
        ),
        _ => unreachable!("algorithm analysis restricts the checked statement grammar"),
    }
}

fn lower_algorithm_if<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    environment: AlgorithmEnvironment<'_, '_, 'dae>,
    owner: AlgorithmOwner<'dae>,
    blocks: &[rumoca_core::StatementBlock],
    else_block: &[rumoca_core::Statement],
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    let mut previous = None;
    for block in blocks {
        let (condition, owner_clock) = lower_condition(
            construction,
            environment.coordinates,
            environment.functions,
            environment.sample_lattices,
            &block.cond,
        )?;
        let available = match previous {
            Some(previous) => {
                let not_previous = negate_condition(construction, previous, span)?;
                combine_conditions(construction, condition, not_previous, false, span)?
            }
            None => condition,
        };
        let condition_span = block
            .cond
            .span()
            .expect("analysis proves algorithm condition provenance");
        let guard = algorithm_if_guard(
            construction,
            owner.parent,
            available,
            owner_clock,
            condition_span,
            span,
        )?;
        if let Some(clock) = guard.owner_clock {
            own_clocked_algorithm_targets(
                construction,
                environment.coordinates,
                clock.into(),
                &block.stmts,
            )?;
        }
        lower_algorithm_statements(
            construction,
            discrete_values,
            environment,
            AlgorithmOwner {
                parent: Some(guard),
                span,
                ..owner
            },
            &block.stmts,
        )?;
        previous = Some(match previous {
            Some(previous) => combine_conditions(construction, previous, condition, true, span)?,
            None => condition,
        });
    }
    if !else_block.is_empty() {
        lower_algorithm_else(
            construction,
            discrete_values,
            environment,
            owner,
            previous,
            else_block,
            span,
        )?;
    }
    Ok(())
}

fn algorithm_if_guard<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    parent: Option<EventGuard<'dae>>,
    available: dae::ConditionId<'dae>,
    owner_clock: Option<dae::PeriodicClockId<'dae>>,
    provenance_span: Span,
    span: Span,
) -> Result<EventGuard<'dae>, dae::DaeConstructionError> {
    let branch_provenance = dae::DaeProvenance::source(provenance_span)?;
    match parent {
        Some(parent) => Ok(EventGuard {
            trigger: parent.trigger,
            condition: combine_conditions(construction, parent.condition, available, false, span)?,
            owner_clock: parent.owner_clock.or(owner_clock),
            branch_provenance,
            always: false,
            parent_activation: Some((parent.trigger, parent.condition)),
        }),
        None => Ok(EventGuard {
            trigger: available,
            condition: available,
            owner_clock,
            branch_provenance,
            always: false,
            parent_activation: None,
        }),
    }
}

fn lower_algorithm_else<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    environment: AlgorithmEnvironment<'_, '_, 'dae>,
    owner: AlgorithmOwner<'dae>,
    previous: Option<dae::ConditionId<'dae>>,
    statements: &[rumoca_core::Statement],
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    let available = match previous {
        Some(previous) => negate_condition(construction, previous, span)?,
        None => always_condition(construction, span)?,
    };
    let guard = algorithm_if_guard(construction, owner.parent, available, None, span, span)?;
    lower_algorithm_statements(
        construction,
        discrete_values,
        environment,
        AlgorithmOwner {
            parent: Some(guard),
            span,
            ..owner
        },
        statements,
    )
}

fn lower_algorithm_when<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    environment: AlgorithmEnvironment<'_, '_, 'dae>,
    owner: AlgorithmOwner<'dae>,
    blocks: &[rumoca_core::StatementBlock],
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    let mut previous = None;
    let mut guarded_blocks = Vec::with_capacity(blocks.len());
    for block in blocks {
        let (condition, owner_clock) = lower_condition(
            construction,
            environment.coordinates,
            environment.functions,
            environment.sample_lattices,
            &block.cond,
        )?;
        let available = match previous {
            Some(previous) => {
                let not_previous = negate_condition(construction, previous, span)?;
                combine_conditions(construction, condition, not_previous, false, span)?
            }
            None => condition,
        };
        let guard = match owner.parent {
            Some(parent) => EventGuard {
                trigger: available,
                condition: combine_conditions(
                    construction,
                    parent.condition,
                    available,
                    false,
                    span,
                )?,
                owner_clock: parent.owner_clock.or(owner_clock),
                branch_provenance: dae::DaeProvenance::source(
                    block
                        .cond
                        .span()
                        .expect("analysis proves algorithm condition provenance"),
                )?,
                always: false,
                parent_activation: Some((parent.trigger, parent.condition)),
            },
            None => EventGuard {
                trigger: available,
                condition: available,
                owner_clock,
                branch_provenance: dae::DaeProvenance::source(
                    block
                        .cond
                        .span()
                        .expect("analysis proves algorithm condition provenance"),
                )?,
                always: false,
                parent_activation: None,
            },
        };
        guarded_blocks.push((block, guard));
        previous = Some(match previous {
            Some(previous) => combine_conditions(construction, previous, condition, true, span)?,
            None => condition,
        });
    }
    for (block, guard) in &guarded_blocks {
        if let Some(clock) = guard.owner_clock {
            own_clocked_algorithm_targets(
                construction,
                environment.coordinates,
                clock.into(),
                &block.stmts,
            )?;
        }
    }
    for (block, guard) in guarded_blocks {
        lower_algorithm_statements(
            construction,
            discrete_values,
            environment,
            AlgorithmOwner {
                parent: Some(guard),
                span,
                ..owner
            },
            &block.stmts,
        )?;
    }
    Ok(())
}

fn lower_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    expression: &Expression,
) -> Result<(dae::ConditionId<'dae>, Option<dae::PeriodicClockId<'dae>>), dae::DaeConstructionError>
{
    let (condition, relations, owner_clock) = lower_condition_tree(
        construction,
        coordinates,
        functions,
        sample_lattices,
        expression,
    )?;
    let provenance = dae::DaeProvenance::generated(
        dae::DaeGeneration::ConditionLowering,
        expression
            .span()
            .expect("analysis proves condition provenance"),
    )?;
    for relation in relations {
        construction.conditions(|conditions| conditions.root(relation, condition, provenance))?;
    }
    Ok((condition, owner_clock))
}

type LoweredCondition<'dae> = (
    dae::ConditionId<'dae>,
    Vec<dae::RelationId<'dae>>,
    Option<dae::PeriodicClockId<'dae>>,
);
type LoweredConditionNode<'dae> = (
    dae::ConditionInput<'dae>,
    Vec<dae::RelationId<'dae>>,
    Option<dae::PeriodicClockId<'dae>>,
);

fn lower_condition_tree<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    expression: &Expression,
) -> Result<LoweredCondition<'dae>, dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::source(
        expression
            .span()
            .expect("analysis proves condition provenance"),
    )?;
    let (input, relations, owner_clock) = match expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Initial,
            ..
        } => (dae::ConditionInput::Initial, Vec::new(), None),
        Expression::Unary {
            op: OpUnary::Not,
            rhs,
            ..
        } => {
            let (condition, relations, _) =
                lower_condition_tree(construction, coordinates, functions, sample_lattices, rhs)?;
            (dae::ConditionInput::Not(condition), relations, None)
        }
        Expression::Binary {
            op: OpBinary::And,
            lhs,
            rhs,
            ..
        }
        | Expression::Binary {
            op: OpBinary::Or,
            lhs,
            rhs,
            ..
        } => lower_binary_condition(
            construction,
            coordinates,
            functions,
            sample_lattices,
            (lhs, rhs),
            matches!(
                expression,
                Expression::Binary {
                    op: OpBinary::Or,
                    ..
                }
            ),
            provenance,
        )?,
        Expression::Array { elements, .. } => {
            return lower_vector_condition(
                construction,
                coordinates,
                functions,
                sample_lattices,
                elements,
                provenance.span(),
            );
        }
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            ..
        } => lower_sample_condition(construction, sample_lattices, provenance)?,
        Expression::BuiltinCall {
            function: BuiltinFunction::Change,
            args,
            ..
        } => {
            let expression = lower_change_expression(
                construction,
                coordinates,
                functions,
                args,
                provenance.span(),
            )?;
            (dae::ConditionInput::Discrete(expression), Vec::new(), None)
        }
        Expression::Binary {
            op:
                OpBinary::Eq | OpBinary::Neq | OpBinary::Lt | OpBinary::Le | OpBinary::Gt | OpBinary::Ge,
            ..
        } => {
            let expression =
                lower_expression(construction, coordinates, functions, expression, None)?;
            let relation = construction
                .conditions(|conditions| conditions.relation(expression, provenance))?;
            (
                dae::ConditionInput::Relation(relation),
                vec![relation],
                None,
            )
        }
        _ => {
            let expression =
                lower_expression(construction, coordinates, functions, expression, None)?;
            (dae::ConditionInput::Discrete(expression), Vec::new(), None)
        }
    };
    let condition = construction.conditions(|conditions| conditions.reserve(provenance))?;
    construction.conditions(|conditions| conditions.define(condition, input, provenance))?;
    Ok((condition, relations, owner_clock))
}

fn lower_sample_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    provenance: dae::DaeProvenance,
) -> Result<LoweredConditionNode<'dae>, dae::DaeConstructionError> {
    let lattice = *sample_lattices
        .iter()
        .find_map(|(span, lattice)| (*span == provenance.span()).then_some(lattice))
        .expect("analysis proves every sample condition has an exact clock lattice");
    let clock = construction.clocks(|clocks| clocks.periodic(lattice, provenance))?;
    Ok((
        dae::ConditionInput::Clock(clock.into()),
        Vec::new(),
        Some(clock),
    ))
}

fn lower_binary_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    operands: (&Expression, &Expression),
    disjunction: bool,
    provenance: dae::DaeProvenance,
) -> Result<LoweredConditionNode<'dae>, dae::DaeConstructionError> {
    let (lhs, rhs) = operands;
    let (lhs, mut relations, lhs_clock) =
        lower_condition_tree(construction, coordinates, functions, sample_lattices, lhs)?;
    let (rhs, rhs_relations, rhs_clock) =
        lower_condition_tree(construction, coordinates, functions, sample_lattices, rhs)?;
    relations.extend(rhs_relations);
    let input = if disjunction {
        dae::ConditionInput::Or(lhs, rhs)
    } else {
        dae::ConditionInput::And(lhs, rhs)
    };
    let owner_clock = merge_condition_clock(lhs_clock, rhs_clock, disjunction, provenance)?;
    Ok((input, relations, owner_clock))
}

fn lower_vector_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    elements: &[Expression],
    span: Span,
) -> Result<
    (
        dae::ConditionId<'dae>,
        Vec<dae::RelationId<'dae>>,
        Option<dae::PeriodicClockId<'dae>>,
    ),
    dae::DaeConstructionError,
> {
    let generated = dae::DaeProvenance::generated(dae::DaeGeneration::ConditionLowering, span)?;
    let Some(first) = elements.first() else {
        let expression = construction.expressions(|expressions| {
            expressions
                .at(generated)
                .literal(dae::DaeLiteral::Boolean(false))
        })?;
        let condition = construction.conditions(|conditions| conditions.reserve(generated))?;
        construction.conditions(|conditions| {
            conditions.define(
                condition,
                dae::ConditionInput::Discrete(expression),
                generated,
            )
        })?;
        return Ok((condition, Vec::new(), None));
    };
    let (mut condition, mut relations, mut owner_clock) =
        lower_condition_tree(construction, coordinates, functions, sample_lattices, first)?;
    for element in &elements[1..] {
        let (rhs, rhs_relations, rhs_clock) = lower_condition_tree(
            construction,
            coordinates,
            functions,
            sample_lattices,
            element,
        )?;
        condition = combine_conditions(construction, condition, rhs, true, span)?;
        relations.extend(rhs_relations);
        owner_clock = merge_condition_clock(owner_clock, rhs_clock, true, generated)?;
    }
    Ok((condition, relations, owner_clock))
}

fn lower_change_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    arguments: &[Expression],
    span: Span,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let (name, subscripts) =
        derivative_reference(&arguments[0]).expect("algorithm analysis proves change target");
    let current_provenance = dae::DaeProvenance::source(
        arguments[0]
            .span()
            .expect("algorithm analysis proves change operand provenance"),
    )?;
    let generated = dae::DaeProvenance::generated(dae::DaeGeneration::ConditionLowering, span)?;
    let symbols = LoweringSymbols {
        coordinates,
        functions,
        shapes: functions.shapes.model_values(),
        function_body: None,
        values: None,
        owner_clock: None,
    };
    let current = lower_coordinate_reference(
        construction,
        symbols,
        &HashMap::new(),
        coordinates[name.var_name()].current(),
        subscripts,
        current_provenance,
    )?;
    let previous_coordinate = coordinates[name.var_name()]
        .previous(span)
        .expect("algorithm analysis proves a discrete change target");
    let previous = lower_coordinate_reference(
        construction,
        symbols,
        &HashMap::new(),
        previous_coordinate,
        subscripts,
        generated,
    )?;
    construction.expressions(|expressions| {
        expressions
            .at(generated)
            .binary(dae::BinaryOperator::NotEqual, current, previous)
    })
}

fn merge_condition_clock<'dae>(
    lhs: Option<dae::PeriodicClockId<'dae>>,
    rhs: Option<dae::PeriodicClockId<'dae>>,
    disjunction: bool,
    provenance: dae::DaeProvenance,
) -> Result<Option<dae::PeriodicClockId<'dae>>, dae::DaeConstructionError> {
    match (lhs, rhs) {
        (Some(lhs), Some(rhs)) if lhs != rhs => Err(dae::DaeConstructionError::DuplicateKey {
            kind: "condition clock owner",
            key: format!("{} and {}", lhs.index(), rhs.index()),
            span: provenance.span(),
        }),
        (Some(clock), Some(_)) => Ok(Some(clock)),
        (Some(clock), None) | (None, Some(clock)) if !disjunction => Ok(Some(clock)),
        _ => Ok(None),
    }
}

fn negate_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    condition: dae::ConditionId<'dae>,
    span: Span,
) -> Result<dae::ConditionId<'dae>, dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::generated(dae::DaeGeneration::ConditionLowering, span)?;
    let negated = construction.conditions(|conditions| conditions.reserve(provenance))?;
    construction.conditions(|conditions| {
        conditions.define(negated, dae::ConditionInput::Not(condition), provenance)
    })?;
    Ok(negated)
}

fn combine_conditions<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    lhs: dae::ConditionId<'dae>,
    rhs: dae::ConditionId<'dae>,
    disjunction: bool,
    span: Span,
) -> Result<dae::ConditionId<'dae>, dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::generated(dae::DaeGeneration::ConditionLowering, span)?;
    let combined = construction.conditions(|conditions| conditions.reserve(provenance))?;
    let input = if disjunction {
        dae::ConditionInput::Or(lhs, rhs)
    } else {
        dae::ConditionInput::And(lhs, rhs)
    };
    construction.conditions(|conditions| conditions.define(combined, input, provenance))?;
    Ok(combined)
}

fn lower_structured_equations<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    equations: &[flat::Equation],
    families: &[flat::StructuredEquationFamily],
    excluded_families: &HashSet<usize>,
    initialization: bool,
) -> Result<(), dae::DaeConstructionError> {
    for (family_index, family) in families.iter().enumerate() {
        if excluded_families.contains(&family_index) {
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
            template
                .body
                .iter()
                .map(|body| {
                    let symbols = LoweringSymbols {
                        coordinates,
                        functions,
                        shapes: functions.shapes.model_values(),
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
                equations,
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
            initialization,
            owner,
            domain,
            scalar_view,
            bodies,
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
    roles: &'scope HashMap<VarName, PlannedRole>,
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
        if input.initialization {
            let residual = lower_expression(
                construction,
                coordinates,
                functions,
                &equation.residual,
                generation,
            )?;
            construction.initialization(|system| system.value_equation(owner, residual))?;
            continue;
        }
        match equation_partition(input.flat, equation, input.roles)
            .expect("analysis already validates equation ownership")
        {
            EquationPartition::Continuous => {
                let (source, generation) = match input.semi_linear.residual(index) {
                    Some(replacement) => {
                        (replacement, Some(dae::DaeGeneration::SemiLinearLowering))
                    }
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
        }
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
