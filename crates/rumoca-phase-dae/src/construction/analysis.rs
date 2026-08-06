mod clocks;
mod comprehensions;
mod delays;
mod derived_parameters;
mod discrete_values;
mod equation_partitions;
mod event_conditions;
mod expression_events;
mod expression_semi_linear;
mod expression_validation;
mod function_array_assemblies;
mod function_bodies;
mod function_conditionals;
mod function_definitions;
mod function_externals;
mod function_impurity;
mod function_loops;
mod function_ranges;
mod function_record_assemblies;
mod function_reductions;
mod function_returns;
mod function_value_types;
mod initial_algorithms;
mod loop_compaction;
mod model_algorithm_calls;
mod model_algorithm_statements;
mod model_algorithms;
mod model_roles;
mod multi_output_equations;
mod record_array_fields;
mod record_equations;
mod sample_aliases;
mod source_balance;
mod structured_families;
mod unexecuted_branches;
mod when_chains;

use super::*;
use clocks::SampledTarget;
use clocks::{ClockAnalysis, ClockDomainAnalysis, analyze_clocks};
pub(super) use clocks::{
    ClockPlan, ClockedValuePlan, is_inferred_clock_condition, is_whole_clock_coordinate,
};
use comprehensions::analyze_comprehensions;
pub(super) use comprehensions::{
    ComprehensionKey, ComprehensionPlans, specialized_comprehension_plan,
};
pub(super) use delays::DelayPlan;
use delays::analyze_delays;
pub(super) use derived_parameters::DerivedParameterPlan;
use derived_parameters::analyze_derived_parameters;
pub(super) use discrete_values::DiscreteValueTopologyPlan;
use discrete_values::analyze_discrete_value_topology;
pub(super) use equation_partitions::{
    AggregateDiscreteConnections, DiscreteValueAssignmentPlan, EquationPartition,
    discrete_value_assignment, equation_partition, structured_discrete_assignments,
    structured_discrete_element_assignments,
};
use equation_partitions::{
    aggregate_discrete_connections, defined_discrete_targets, discrete_connection_ranks,
};
use event_conditions::{
    evaluate_clock_seconds, evaluate_sample_schedule, validate_algorithm_condition,
    validate_condition_expression, validate_when_condition_expression,
};
use expression_events::analyze_expression_events;
pub(super) use expression_events::{
    DynamicTimeEventOperand, ExpressionEventPlan, ExpressionEventPlans,
};
use expression_semi_linear::analyze_semi_linear_rules;
pub(super) use expression_semi_linear::{SemiLinearRowFilter, SemiLinearRules};
use expression_validation::{
    PreContext, validate_expression, validate_expression_in_context,
    validate_expression_scoped_with_record_array_fields,
    validate_model_expression_with_record_array_fields, validate_specialized_expression,
    validate_specialized_subscripts, validate_subscripts_scoped, validate_when_expression,
    when_body_context,
};
use function_array_assemblies::coalesce_function_array_assemblies;
pub(super) use function_bodies::function_assertion;
use function_bodies::{
    plan_function_statements, resolve_function_definitions,
    validate_function_expression_with_roles, validate_function_statements,
    validate_function_subscripts, validate_functions,
};
pub(super) use function_conditionals::selected_conditional_statements;
use function_conditionals::{plan_function_conditional, resolve_function_conditional};
use function_definitions::FunctionDefinitions;
pub(super) use function_definitions::FunctionValueSeed;
use function_externals::validate_external_function;
pub(super) use function_externals::{ExternalArgumentPlan, ExternalFunctionPlan};
use function_impurity::validate_impure_call_contexts;
use function_loops::{flattened_function_loop_source, subscript_is_binder, validate_function_loop};
pub(super) use function_ranges::assigned_function_targets;
use function_ranges::static_shape_integer_expression;
use function_ranges::{
    immutable_integer_defaults, static_function_range, validate_function_range_expression,
};
use function_record_assemblies::{
    plan_staged_record_assemblies, record_constructor, validate_record_output_assembly,
};
use function_reductions::validate_integer_reduction;
use function_returns::{
    certify_nonleading_return_branches, nonreturn_path, normalize_function_returns,
    validate_guarded_function_return,
};
pub(super) use function_value_types::record_field_projections;
use function_value_types::validate_function_value_type;
pub(super) use initial_algorithms::InitialDiscreteValue;
use initial_algorithms::{
    InitialAlgorithmAnalysis, analyze_initial_algorithms, claim_initial_discrete_equations,
    reject_unsupported_initial_algorithm_statements,
};
use loop_compaction::compact_function_loops;
use model_algorithm_calls::analyze_event_function_calls;
pub(super) use model_algorithm_calls::{ModelEventFunctionCallPlan, ModelEventFunctionOutputPlan};
use model_algorithm_statements::validate_model_algorithm;
use model_algorithms::analyze_model_algorithm;
pub(super) use model_algorithms::{ModelAlgorithmPlan, ModelEventTensorLoopPlan};
pub(super) use model_algorithms::{
    algorithm_targets, event_targets, is_event_condition, model_algorithm_targets,
    when_chain_targets,
};
use model_roles::{
    ModelRoles, analyze_model_roles, apply_clocked_partition_roles, is_predefined_clock_variable,
};
pub(in crate::construction) use multi_output_equations::MultiOutputEquationPlan;
use multi_output_equations::analyze_multi_output_equations;
pub(super) use record_array_fields::{RecordArrayFieldPlan, RecordArrayFieldPlans};
use record_array_fields::{
    analyze_record_array_fields, validate_record_array_field_runtime_coordinates,
};
use record_equations::analyze_record_equations;
use sample_aliases::analyze_sample_aliases;
use source_balance::{SourceBalanceInput, source_balance};
use structured_families::validate_structured_families;
use unexecuted_branches::{check_function_assignment_shapes, check_unexecuted_branches};
use when_chains::validate_when_chains;

pub(super) struct Analysis {
    pub(super) constants: EvalContext,
    pub(super) delay_plans: HashMap<Span, DelayPlan>,
    pub(super) roles: HashMap<VarName, PlannedRole>,
    pub(super) balance: BalanceDetail,
    pub(super) continuous_family_rows: HashSet<usize>,
    pub(super) initialization_family_rows: HashSet<usize>,
    /// Scalar initial-equation rows represented by typed initial discrete-value
    /// definitions rather than numeric initialization residuals.
    pub(super) initial_discrete_equation_rows: HashSet<usize>,
    pub(super) sample_lattices: Vec<(Span, PeriodicClockSchedule)>,
    pub(super) expression_events: ExpressionEventPlans,
    /// Exact scalar Boolean aliases of MLS §3.7.5 `sample(start, interval)`.
    ///
    /// Conditions consume this proof so a source `when sample_alias` retains
    /// the typed periodic clock instead of buffering a held B.1c coordinate.
    pub(super) sample_alias_schedules: HashMap<VarName, PeriodicClockSchedule>,
    pub(super) clock_plans: HashMap<InstanceId, ClockPlan>,
    pub(super) clock_equation_rows: HashSet<usize>,
    pub(super) clocked_equation_owners: HashMap<usize, ClockPlan>,
    pub(super) clocked_value_owners: HashMap<InstanceId, ClockedValuePlan>,
    /// Owning clock of every `when Clock()` branch, keyed by the branch span.
    pub(super) clocked_when_owners: HashMap<Span, ClockPlan>,
    /// Owning clock of every runtime coordinate in a clocked partition.
    pub(super) clocked_coordinate_owners: HashMap<InstanceId, ClockPlan>,
    pub(super) model_algorithm_plans: Vec<ModelAlgorithmPlan>,
    /// `fixed = false` parameters an initial algorithm determines (MLS §8.6).
    pub(super) initial_parameters: HashMap<VarName, Expression>,
    /// Discrete coordinates whose initialization-instant value an initial
    /// algorithm determines (MLS §8.6).
    pub(super) initial_discrete_values: HashMap<VarName, InitialDiscreteValue>,
    /// Assertions an initial algorithm owns, with enclosing guards folded in.
    pub(super) initial_algorithm_assertions: Vec<flat::AssertEquation>,
    pub(super) function_plans: HashMap<FunctionSpecializationKey, FunctionPlan>,
    pub(super) function_shapes: FunctionShapeAnalysis,
    pub(super) comprehension_plans: ComprehensionPlans,
    pub(super) record_array_fields: Arc<RecordArrayFieldPlans>,
    pub(super) derived_parameters: HashMap<VarName, DerivedParameterPlan>,
    pub(super) derived_parameter_families: HashSet<usize>,
    pub(super) derived_parameter_rows: HashSet<usize>,
    pub(super) record_equations: HashMap<usize, RecordEquationPlan>,
    /// Continuous MLS §12.4.3 tuple equations lowered by result ordinal.
    pub(super) multi_output_equations: HashMap<usize, MultiOutputEquationPlan>,
    pub(super) initial_record_equations: HashMap<usize, RecordEquationPlan>,
    pub(super) discrete_value_topology: DiscreteValueTopologyPlan,
    pub(super) discrete_connection_ranks: HashMap<VarName, usize>,
    pub(super) aggregate_discrete_connections: AggregateDiscreteConnections,
    pub(super) assigned_discrete_targets: HashSet<VarName>,
    /// MLS §3.7.4.5 Rule 1 / Rule 2 replacement residuals, keyed by the model
    /// equation row they replace. Empty until
    /// [`Analysis::with_semi_linear_rules`] proves them.
    pub(super) semi_linear_rules: SemiLinearRules,
}

struct SourceBalanceAnalysis {
    detail: BalanceDetail,
    assigned_discrete_targets: HashSet<VarName>,
}

pub(super) enum FunctionPlan {
    Statements {
        /// The source statements the plans were built from. Exact tensor-native
        /// loop rewrites may replace the shared body, so construction lowers this
        /// aligned sequence rather than rediscovering those rewrites.
        source: Vec<rumoca_core::Statement>,
        statements: Vec<FunctionStatementPlan>,
        generated_booleans: Vec<(VarName, Span)>,
        certified_output_seeds: Vec<(VarName, FunctionValueSeed)>,
    },
    GuardedReturn {
        branches: Vec<Vec<FunctionStatementPlan>>,
        tail: Vec<FunctionStatementPlan>,
        targets: Vec<VarName>,
    },
    IntegerReduction {
        initial: Vec<FunctionStatementPlan>,
        result: VarName,
        reduction: FunctionIntegerReduction,
    },
    /// MLS §12.9 external interface; the function has no Modelica body.
    External(ExternalFunctionPlan),
}

pub(super) enum FunctionIntegerReduction {
    WhileExclusive,
    ForInclusiveCapped,
}

pub(super) enum FunctionStatementPlan {
    Assignment(FunctionAssignmentPlan),
    /// An MLS §8.3.7 assertion whose condition this exact value-proven
    /// specialization establishes as `true`.
    ///
    /// The plan is the proof that construction may erase the flow action. An
    /// unsettled assertion is never represented by this variant: it needs a
    /// call-scoped runtime owner.
    ProvenAssertion,
    /// A default-level MLS §8.3.7 assertion owned by the top-level function
    /// statement sequence and evaluated once for every call.
    RuntimeAssertion,
    /// One compiler-owned immutable Boolean that captures a return predicate
    /// at its source statement before later mutable values can change.
    GeneratedBooleanAssignment {
        target: VarName,
        value: Expression,
        span: Span,
    },
    For {
        domain: StructuredIndexDomain,
        binder_spans: Vec<Span>,
        lowering: FunctionLoopLowering,
        statements: Vec<FunctionStatementPlan>,
        source_depth: usize,
    },
    If {
        branches: Vec<Vec<FunctionStatementPlan>>,
        fallback: Option<Vec<FunctionStatementPlan>>,
        targets: Vec<VarName>,
    },
    /// An MLS §11.5 conditional whose executed branch this specialization
    /// proves, planned as the unconditional statement sequence it denotes.
    ///
    /// `selected` names the condition branch that holds, or `None` for the else
    /// part; `statements` is the plan of exactly those statements.
    ProvenBranch {
        selected: Option<usize>,
        statements: Vec<FunctionStatementPlan>,
    },
    /// MLS §11.2.1.1 assignment from a call with multiple results.
    ///
    /// One entry per *receiving slot* written at the call site, in source
    /// order, so entry `i` names the target of result ordinal `i`. `None` is an
    /// omitted receiver — the `(out1, , out3)` spelling — whose result the DAE
    /// never reads and therefore never lowers.
    MultiOutputCall {
        outputs: Vec<Option<FunctionAssignmentPlan>>,
    },
    /// A pure multi-result call whose receiving list defines every field of
    /// one record-valued function output or local.
    RecordMultiOutputAssembly(FunctionRecordCallAssemblyPlan),
    ArrayAssembly(FunctionArrayAssemblyPlan),
    ArrayAssemblyMember,
    RecordAssembly(FunctionRecordAssemblyPlan),
    RecordAssemblyMember,
    /// One field of a record result assembled at its source position and
    /// stored independently until every field has a checked value.
    RecordFieldAssembly(FunctionRecordFieldAssemblyPlan),
    RecordFieldAssemblyMember,
}

pub(super) struct FunctionAssignmentPlan {
    target: VarName,
    subscripts: Box<[Subscript]>,
    /// Aggregate seed this element write starts from, proven dead by the
    /// definedness certificate that every declared element is written.
    seed: Option<FunctionValueSeed>,
}

impl FunctionAssignmentPlan {
    pub(super) fn target(&self) -> &VarName {
        &self.target
    }

    pub(super) fn subscripts(&self) -> &[Subscript] {
        &self.subscripts
    }

    pub(super) fn is_whole(&self) -> bool {
        self.subscripts.is_empty()
    }

    pub(super) fn seed(&self) -> Option<&FunctionValueSeed> {
        self.seed.as_ref()
    }
}

pub(super) struct FunctionArrayAssemblyPlan {
    pub(super) target: VarName,
    pub(super) direct_count: usize,
    pub(super) loop_plan: Option<Box<FunctionStatementPlan>>,
    pub(super) seed: Option<FunctionValueSeed>,
}

pub(super) struct FunctionRecordAssemblyPlan {
    pub(super) target: VarName,
    pub(super) statement_count: usize,
    pub(super) fields: Vec<FunctionRecordFieldAssembly>,
    pub(super) seed: Option<FunctionValueSeed>,
}

pub(super) struct FunctionRecordFieldAssemblyPlan {
    pub(super) target: VarName,
    pub(super) statement_count: usize,
    pub(super) field: FunctionRecordFieldAssembly,
    /// Earlier field definitions this field's expressions may read directly.
    pub(super) available_fields: Vec<VarName>,
    /// Constructor-order field names when this field completes the record.
    pub(super) finalize_fields: Option<Vec<VarName>>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(super) struct FunctionRecordFieldCoordinate {
    pub(super) target: VarName,
    pub(super) field: VarName,
}

pub(super) fn function_record_field_name(target: &VarName, field: &VarName) -> VarName {
    VarName::new(format!("{target}.{field}"))
}

pub(super) struct FunctionRecordFieldAssembly {
    pub(super) name: VarName,
    pub(super) scalar_type: Option<dae::ScalarType>,
    pub(super) dimensions: Vec<u32>,
    pub(super) scalars: Vec<FunctionRecordScalarSource>,
    pub(super) aggregate_statement: Option<usize>,
}

pub(super) struct FunctionRecordCallAssemblyPlan {
    pub(super) target: VarName,
    pub(super) fields: Vec<FunctionRecordCallField>,
}

pub(super) struct FunctionRecordCallField {
    pub(super) name: VarName,
    pub(super) result_ordinal: usize,
}

#[derive(Clone)]
pub(super) struct FunctionRecordScalarSource {
    pub(super) statement_offset: usize,
    pub(super) value_field: Option<VarName>,
    pub(super) value_coordinates: Vec<u32>,
}

pub(super) enum FunctionLoopLowering {
    Fold { targets: Vec<VarName> },
    TotalArrayDefinition,
}

#[derive(Clone, Copy)]
struct FunctionValidationContext<'scope> {
    function: &'scope rumoca_core::Function,
    flat: &'scope flat::Model,
    roles: &'scope HashMap<VarName, PlannedRole>,
    static_integers: &'scope HashMap<VarName, i64>,
    shapes: &'scope ShapeEnvironment,
    shape_analysis: &'scope FunctionShapeAnalysis,
    generated_booleans: &'scope [function_returns::GeneratedBooleanDefinition],
    /// Record-field coordinates already constructed by an enclosing staged
    /// assembly at this exact source position.
    staged_record_fields: &'scope HashSet<FunctionRecordFieldCoordinate>,
    /// Whether this source sequence maps directly to the call-scoped action
    /// sequence rather than a loop or runtime-conditional value owner.
    call_scoped_actions: bool,
}

/// Name the statement form, so a report says which owner is missing.
pub(super) fn statement_kind(statement: &rumoca_core::Statement) -> &'static str {
    match statement {
        rumoca_core::Statement::Empty { .. } => "empty",
        rumoca_core::Statement::Assignment { .. } => "assignment",
        rumoca_core::Statement::Return { .. } => "return",
        rumoca_core::Statement::Break { .. } => "break",
        rumoca_core::Statement::For { .. } => "for",
        rumoca_core::Statement::While { .. } => "while",
        rumoca_core::Statement::If { .. } => "if",
        rumoca_core::Statement::When { .. } => "when",
        rumoca_core::Statement::FunctionCall { .. } => "function-call",
        rumoca_core::Statement::Reinit { .. } => "reinit",
        rumoca_core::Statement::Assert { .. } => "assert",
    }
}

pub(super) fn required_statement_span(
    statement: &rumoca_core::Statement,
    owner: impl Into<String>,
) -> Result<Span, ToDaeError> {
    let kind = statement_kind(statement);
    statement
        .source_span()
        .ok_or_else(|| ToDaeError::MissingProvenance {
            owner: format!("{} ({kind} statement occurrence)", owner.into()),
        })
}

#[derive(Clone, Copy, Debug)]
pub(super) enum PlannedRole {
    /// An MLS §9.1.3 member that has no connection and no binding. It is
    /// retained in Flat IR for source identity, but has no runtime coordinate.
    UnusedExpandable,
    Parameter,
    Constant,
    Input,
    State,
    Algebraic,
    Output,
    DiscreteReal,
    DiscreteValue,
    Clock,
    EnumerationLiteral,
    Aggregate,
}

pub(super) struct RecordEquationPlan {
    pub(super) fields: Vec<RecordEquationFieldPlan>,
}

pub(super) struct RecordEquationFieldPlan {
    pub(super) target: VarName,
    pub(super) value: RecordEquationFieldValue,
}

pub(super) enum RecordEquationFieldValue {
    AggregateProjection(Box<[usize]>),
    Coordinate(VarName),
}

pub(super) fn analyze(flat: &flat::Model) -> Result<Analysis, ToDaeError> {
    validate_source_model(flat)?;
    // The parameter fixed point is folded before any shape is proven: MLS §12.2
    // lets a function's array dimensions be parameter expressions, so the
    // settled values of the model's evaluable parameters (MLS §4.5) are part of
    // what proves a function's declared extents, not a later consequence of it.
    let constants = constant_context(flat)?;
    let function_shapes = FunctionShapeAnalysis::analyze(flat, &constants)?;
    let record_array_fields = Arc::clone(function_shapes.record_array_fields());
    let function_plans = validate_functions(flat, &function_shapes)?;
    let record_equations = analyze_record_equations(flat, &flat.equations)?;
    let initial_record_equations = analyze_record_equations(flat, &flat.initial_equations)?;
    let comprehension_plans = analyze_comprehensions(all_model_expressions(flat), &constants)?;
    let delay_plans = analyze_delays(flat, &constants)?;
    let clocks = analyze_clocks(flat, &constants)?;
    let ModelRoles {
        states,
        variables: mut roles,
        expressions: mut expression_roles,
    } = analyze_model_roles(flat, &clocks.sampled_targets)?;
    validate_runtime_coordinates(flat, &roles, &record_array_fields)?;
    let derived_parameters = analyze_derived_parameters(flat, &roles)?;
    apply_derived_parameter_roles(&derived_parameters.plans, &mut roles, &mut expression_roles);
    let clock_domains =
        analyze_clocked_partitions(flat, &clocks, &constants, &mut roles, &mut expression_roles)?;
    let multi_output_equations =
        analyze_multi_output_equations(flat, &expression_roles, &states, &function_shapes)?;
    let (continuous_family_rows, initialization_family_rows) =
        validate_expressions_and_structured_rows(
            flat,
            &roles,
            &expression_roles,
            &states,
            &record_array_fields,
            function_shapes.model_values(),
            &multi_output_equations,
        )?;
    let (mut sample_lattices, model_algorithm_plans) = analyze_event_algorithms(
        flat,
        &roles,
        &expression_roles,
        &states,
        &constants,
        &function_shapes,
    )?;
    let (discrete_connection_ranks, aggregate_discrete_connections, discrete_value_topology) =
        analyze_discrete_connections(flat, &roles)?;
    let mut initial_algorithms =
        analyze_initial_algorithm_owners(flat, &roles, &states, &constants, &mut sample_lattices)?;
    let initial_discrete_equation_rows =
        claim_initial_discrete_equations(flat, &roles, &mut initial_algorithms.discrete_values)?;
    let balance = analyze_source_balance(SourceBalanceAnalysisInput {
        flat,
        roles: &roles,
        clock_equation_rows: &clocks.equation_rows,
        derived_parameter_rows: &derived_parameters.rows,
        record_equations: &record_equations,
        multi_output_equations: &multi_output_equations,
        connection_ranks: &discrete_connection_ranks,
        aggregate_connections: &aggregate_discrete_connections,
    })?;
    let (expression_events, sample_alias_schedules) = analyze_expression_event_ownership(
        flat,
        &roles,
        &constants,
        &discrete_connection_ranks,
        &aggregate_discrete_connections,
    )?;
    Ok(Analysis {
        constants,
        delay_plans,
        roles,
        balance: balance.detail,
        continuous_family_rows,
        initialization_family_rows,
        initial_discrete_equation_rows,
        sample_lattices,
        expression_events,
        sample_alias_schedules,
        clock_plans: clocks.plans,
        clock_equation_rows: clocks.equation_rows,
        clocked_equation_owners: clock_domains.equation_owners,
        clocked_value_owners: clock_domains.value_owners,
        clocked_when_owners: clock_domains.when_owners,
        clocked_coordinate_owners: clock_domains.coordinate_owners,
        model_algorithm_plans,
        initial_parameters: initial_algorithms.parameters,
        initial_discrete_values: initial_algorithms.discrete_values,
        initial_algorithm_assertions: initial_algorithms.assertions,
        function_plans,
        function_shapes,
        comprehension_plans,
        record_array_fields,
        derived_parameters: derived_parameters.plans,
        derived_parameter_families: derived_parameters.families,
        derived_parameter_rows: derived_parameters.rows,
        record_equations,
        multi_output_equations,
        initial_record_equations,
        discrete_value_topology,
        discrete_connection_ranks,
        aggregate_discrete_connections,
        assigned_discrete_targets: balance.assigned_discrete_targets,
        semi_linear_rules: SemiLinearRules::default(),
    })
}

fn validate_expressions_and_structured_rows(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    expression_roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    record_array_fields: &RecordArrayFieldPlans,
    values: &ShapeEnvironment,
    multi_output_equations: &HashMap<usize, MultiOutputEquationPlan>,
) -> Result<(HashSet<usize>, HashSet<usize>), ToDaeError> {
    validate_model_expressions(
        flat,
        expression_roles,
        states,
        record_array_fields,
        values,
        multi_output_equations,
    )?;
    analyze_structured_family_rows(
        flat,
        roles,
        expression_roles,
        states,
        record_array_fields,
        values,
    )
}

fn validate_runtime_coordinates(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    record_array_fields: &RecordArrayFieldPlans,
) -> Result<(), ToDaeError> {
    validate_runtime_coordinate_instances(flat, roles)?;
    validate_record_array_field_runtime_coordinates(flat, record_array_fields, roles)
}

fn analyze_structured_family_rows(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    expression_roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    record_array_fields: &RecordArrayFieldPlans,
    values: &ShapeEnvironment,
) -> Result<(HashSet<usize>, HashSet<usize>), ToDaeError> {
    let continuous = validate_structured_families(
        &flat.structured_equations,
        flat.equations.len(),
        roles,
        expression_roles,
        states,
        record_array_fields,
        values,
    )?;
    let initialization = validate_structured_families(
        &flat.initial_structured_equations,
        flat.initial_equations.len(),
        roles,
        expression_roles,
        states,
        record_array_fields,
        values,
    )?;
    Ok((continuous, initialization))
}

fn analyze_expression_event_ownership(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    constants: &EvalContext,
    connection_ranks: &HashMap<VarName, usize>,
    aggregate_connections: &AggregateDiscreteConnections,
) -> Result<
    (
        ExpressionEventPlans,
        HashMap<VarName, PeriodicClockSchedule>,
    ),
    ToDaeError,
> {
    let events = analyze_expression_events(flat, roles, constants)?;
    let aliases = analyze_sample_aliases(
        flat,
        roles,
        &events,
        connection_ranks,
        aggregate_connections,
    )?;
    Ok((events, aliases))
}

type EventAlgorithmAnalysis = (Vec<(Span, PeriodicClockSchedule)>, Vec<ModelAlgorithmPlan>);

fn analyze_event_algorithms(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    expression_roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
    function_shapes: &FunctionShapeAnalysis,
) -> Result<EventAlgorithmAnalysis, ToDaeError> {
    let mut sample_lattices = Vec::new();
    validate_when_chains(
        &flat.when_chains,
        roles,
        states,
        constants,
        &mut sample_lattices,
    )?;
    let plans = analyze_model_algorithms(
        flat,
        roles,
        expression_roles,
        states,
        constants,
        function_shapes,
        &mut sample_lattices,
    )?;
    Ok((sample_lattices, plans))
}

fn validate_source_model(flat: &flat::Model) -> Result<(), ToDaeError> {
    validate_flat_shape(flat)?;
    // Prove the initial-algorithm grammar before another analysis reports a
    // consequence of its missing owner, such as an `assert` read as a callee.
    reject_unsupported_initial_algorithm_statements(flat)?;
    validate_impure_call_contexts(flat)
}

fn analyze_discrete_connections(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<
    (
        HashMap<VarName, usize>,
        AggregateDiscreteConnections,
        DiscreteValueTopologyPlan,
    ),
    ToDaeError,
> {
    let ranks = discrete_connection_ranks(flat, roles);
    let aggregates = aggregate_discrete_connections(flat, roles, &ranks)?;
    let topology = analyze_discrete_value_topology(flat, roles, &ranks, &aggregates)?;
    Ok((ranks, aggregates, topology))
}

impl Analysis {
    /// Prove the MLS §3.7.4.5 Rule 1 / Rule 2 replacements over the model
    /// equation rows, completing the plan [`analyze`] leaves empty.
    ///
    /// The rules read every other owner's row claims, so they can only be
    /// proven once the rest of the analysis exists. Construction is the caller;
    /// `balance_detail` deliberately is not, because the source balance the
    /// rules preserve is counted on the untransformed rows.
    pub(super) fn with_semi_linear_rules(mut self, flat: &flat::Model) -> Self {
        let mut claimed = self.continuous_family_rows.clone();
        claimed.extend(&self.clock_equation_rows);
        claimed.extend(&self.derived_parameter_rows);
        self.semi_linear_rules = analyze_semi_linear_rules(
            flat,
            &self.roles,
            &self.discrete_connection_ranks,
            &self.aggregate_discrete_connections,
            &SemiLinearRowFilter {
                excluded: &claimed,
                records: &self.record_equations,
                clocked: &self.clocked_equation_owners,
            },
        );
        self
    }
}

/// Proves the clocked partitions and corrects the role plan they contradict.
///
/// The proven partition owners are what turn a continuous role plan into the
/// clocked discrete-time role MLS §16.5.1 requires, and the corrected roles are
/// in turn what let `assign_value_owners` prove clock ownership for those
/// coordinates. The replay is exact rather than iterative: partition membership
/// reads roles only through `is_clock_runtime_role`, which both the old and the
/// new role satisfy, so the second pass proves the same partitions and only
/// widens the ownership relation over them.
fn analyze_clocked_partitions(
    flat: &flat::Model,
    clocks: &ClockAnalysis,
    constants: &EvalContext,
    roles: &mut HashMap<VarName, PlannedRole>,
    expression_roles: &mut HashMap<VarName, PlannedRole>,
) -> Result<ClockDomainAnalysis, ToDaeError> {
    let domains = clocks::analyze_clock_domains(
        flat,
        roles,
        &clocks.plans,
        &clocks.equation_rows,
        &clocks.sampled_targets,
        constants,
    )?;
    if !apply_clocked_partition_roles(flat, &domains.coordinate_owners, roles, expression_roles)? {
        return Ok(domains);
    }
    clocks::analyze_clock_domains(
        flat,
        roles,
        &clocks.plans,
        &clocks.equation_rows,
        &clocks.sampled_targets,
        constants,
    )
}

fn analyze_model_algorithms(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    expression_roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
    function_shapes: &FunctionShapeAnalysis,
    sample_lattices: &mut Vec<(Span, PeriodicClockSchedule)>,
) -> Result<Vec<ModelAlgorithmPlan>, ToDaeError> {
    flat.algorithms
        .iter()
        .map(|algorithm| {
            validate_model_algorithm(
                algorithm,
                expression_roles,
                states,
                constants,
                sample_lattices,
            )?;
            analyze_model_algorithm(flat, algorithm, roles, function_shapes)
        })
        .collect()
}

fn validate_runtime_coordinate_instances(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<(), ToDaeError> {
    let mut instances = HashMap::new();
    for variable in flat.variables.values() {
        if !matches!(
            roles.get(&variable.name),
            Some(
                PlannedRole::Parameter
                    | PlannedRole::Constant
                    | PlannedRole::Input
                    | PlannedRole::State
                    | PlannedRole::Algebraic
                    | PlannedRole::Output
                    | PlannedRole::DiscreteReal
                    | PlannedRole::DiscreteValue
            )
        ) {
            continue;
        }
        if let Some(previous) = instances.insert(variable.instance_id, &variable.name) {
            return Err(ToDaeError::unsupported_flat(
                "runtime coordinate identity",
                format!(
                    "Flat variables `{previous}` and `{}` share exact instance {}",
                    variable.name, variable.instance_id
                ),
                variable.source_span,
            ));
        }
    }
    Ok(())
}

fn apply_derived_parameter_roles(
    plans: &HashMap<VarName, DerivedParameterPlan>,
    roles: &mut HashMap<VarName, PlannedRole>,
    expression_roles: &mut HashMap<VarName, PlannedRole>,
) {
    for name in plans.keys() {
        roles.insert(name.clone(), PlannedRole::Parameter);
        expression_roles.insert(name.clone(), PlannedRole::Parameter);
    }
}

fn validate_model_expressions(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    record_array_fields: &RecordArrayFieldPlans,
    model_values: &ShapeEnvironment,
    multi_output_equations: &HashMap<usize, MultiOutputEquationPlan>,
) -> Result<(), ToDaeError> {
    for variable in flat.variables.values() {
        for expression in variable_attribute_expressions(variable) {
            if let Some(span) = empty_array_bound_to_declaration(variable, expression) {
                // The owning declaration proves the element type and extent, so
                // the literal carries no operand that needs validating.
                require_span(span, "empty array attribute")?;
            } else {
                validate_model_expression_with_record_array_fields(
                    expression,
                    roles,
                    states,
                    record_array_fields,
                    model_values,
                )?;
            }
            validate_known_function_calls(expression, flat)?;
        }
    }
    for (row, equation) in flat.equations.iter().enumerate() {
        if multi_output_equations.contains_key(&row) {
            continue;
        }
        let expression = &equation.residual;
        validate_model_expression_with_record_array_fields(
            expression,
            roles,
            states,
            record_array_fields,
            model_values,
        )?;
        validate_known_function_calls(expression, flat)?;
    }
    for equation in &flat.initial_equations {
        let expression = &equation.residual;
        validate_model_expression_with_record_array_fields(
            expression,
            roles,
            states,
            record_array_fields,
            model_values,
        )?;
        validate_known_function_calls(expression, flat)?;
    }
    Ok(())
}

/// MLS §10.4: an empty array literal has no element from which to derive a
/// type, so its element type and trailing extents come from the declaration it
/// is bound to. A variable attribute may be an empty array exactly when its
/// own declaration proves a zero outer extent.
pub(super) fn empty_array_bound_to_declaration(
    variable: &flat::Variable,
    expression: &Expression,
) -> Option<Span> {
    let Expression::Array { elements, span, .. } = expression else {
        return None;
    };
    (elements.is_empty() && variable.dims.first() == Some(&0)).then_some(*span)
}

fn validate_flat_shape(flat: &flat::Model) -> Result<(), ToDaeError> {
    if !flat.predefined_types.is_complete() {
        return Err(ToDaeError::MissingSemanticIdentity {
            identity: "predefined scalar canonical TypeIds".to_string(),
        });
    }
    flat.validate().map_err(|error| {
        ToDaeError::unsupported_flat("Flat shape contract", format!("{error:?}"), error.span())
    })
}

struct SourceBalanceAnalysisInput<'scope> {
    flat: &'scope flat::Model,
    roles: &'scope HashMap<VarName, PlannedRole>,
    clock_equation_rows: &'scope HashSet<usize>,
    derived_parameter_rows: &'scope HashSet<usize>,
    record_equations: &'scope HashMap<usize, RecordEquationPlan>,
    multi_output_equations: &'scope HashMap<usize, MultiOutputEquationPlan>,
    connection_ranks: &'scope HashMap<VarName, usize>,
    aggregate_connections: &'scope AggregateDiscreteConnections,
}

fn analyze_source_balance(
    input: SourceBalanceAnalysisInput<'_>,
) -> Result<SourceBalanceAnalysis, ToDaeError> {
    let SourceBalanceAnalysisInput {
        flat,
        roles,
        clock_equation_rows,
        derived_parameter_rows,
        record_equations,
        multi_output_equations,
        connection_ranks,
        aggregate_connections,
    } = input;
    let assigned_discrete_targets =
        defined_discrete_targets(flat, roles, connection_ranks, aggregate_connections)?;
    let mut non_runtime_rows = clock_equation_rows.clone();
    non_runtime_rows.extend(derived_parameter_rows);
    let detail = source_balance(SourceBalanceInput {
        flat,
        roles,
        assigned_targets: &assigned_discrete_targets,
        excluded_equation_rows: &non_runtime_rows,
        record_equations,
        multi_output_equations,
        connection_ranks,
        aggregate_connections,
    })?;
    Ok(SourceBalanceAnalysis {
        detail,
        assigned_discrete_targets,
    })
}

/// Replay the initial algorithms, then validate every assertion the model owns
/// — the ones an equation section declares and the ones a replayed section
/// produced — against one condition grammar.
fn analyze_initial_algorithm_owners(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, PeriodicClockSchedule)>,
) -> Result<InitialAlgorithmAnalysis, ToDaeError> {
    let initial_algorithms = analyze_initial_algorithms(flat, roles, states, constants)?;
    validate_assertions(
        flat.assert_equations
            .iter()
            .chain(&flat.initial_assert_equations)
            .chain(&initial_algorithms.assertions),
        roles,
        states,
        constants,
        sample_lattices,
    )?;
    Ok(initial_algorithms)
}

fn validate_assertions<'flat>(
    assertions: impl IntoIterator<Item = &'flat flat::AssertEquation>,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, PeriodicClockSchedule)>,
) -> Result<(), ToDaeError> {
    for assertion in assertions {
        require_span(assertion.span, "assert equation")?;
        validate_condition_expression(
            &assertion.condition,
            roles,
            states,
            constants,
            sample_lattices,
        )?;
        validate_expression(&assertion.message, roles, states)?;
        if let Some(level) = &assertion.level {
            validate_expression(level, roles, states)?;
        }
    }
    Ok(())
}

pub(super) fn analyze_record_array_field_plans(
    flat: &flat::Model,
) -> Result<RecordArrayFieldPlans, ToDaeError> {
    analyze_record_array_fields(
        flat,
        all_model_expressions(flat)
            .chain(structured_template_expressions(&flat.structured_equations))
            .chain(structured_template_expressions(
                &flat.initial_structured_equations,
            ))
            .chain(
                flat.functions
                    .values()
                    .flat_map(function_shapes::function_expressions),
            ),
    )
}

fn structured_template_expressions(
    families: &[flat::StructuredEquationFamily],
) -> impl Iterator<Item = &Expression> {
    families
        .iter()
        .filter_map(|family| family.template.as_ref())
        .flat_map(|template| &template.body)
}

/// Fold every `constant`/`parameter` binding the Flat model settles at
/// translation time.
///
/// The fixed point is keyed on the exact occurrence identity of each
/// declaration — `validate_flat_shape` has already proven every Flat variable
/// carries a distinct allocated [`InstanceId`], so "has this declaration been
/// settled" is a question about the identity and never about a rendered name
/// two occurrences could share.
///
/// A binding that does not fold is only skipped when the evaluator says *why*
/// in typed terms: MLS §4.4 permits a parameter value to be established during
/// initialization instead, and those failures carry a
/// `RuntimeDependentReason`. Any other failure proves the model or the
/// evaluator wrong and is reported at the binding.
fn constant_context(flat: &flat::Model) -> Result<EvalContext, ToDaeError> {
    let mut context = EvalContext::with_capacity(flat.variables.len(), 0, flat.functions.len() * 2);
    for function in flat.functions.values() {
        context.add_function(function.clone());
    }
    for (name, variable) in &flat.variables {
        context.add_array_dimensions(name.to_string(), variable.dims.clone());
    }
    // MLS §4.8.5.2: an enumeration literal's semantic identity is its ordinal,
    // and both `Integer(...)` and the relational operators are defined on that
    // ordinal. Seeding the constant table with the model's exact ordinals is
    // what lets a parameter expression over an enumeration — such as the
    // `resolution < Resolution.s` guard of a periodic clock — evaluate.
    for (literal, ordinal) in &flat.enum_literal_ordinals {
        context.add_parameter(literal.clone(), EvalValue::Integer(*ordinal));
    }
    for _ in 0..flat.variables.len() {
        let mut progress = false;
        for (name, variable) in &flat.variables {
            if context.instance_value(variable.instance_id).is_some()
                || !matches!(
                    variable.variability,
                    Variability::Constant(_) | Variability::Parameter(_)
                )
            {
                continue;
            }
            let Some(binding) = &variable.binding else {
                continue;
            };
            match eval_expr(binding, &context) {
                Ok(value) => {
                    context.add_instance_parameter(variable.instance_id, name.to_string(), value);
                    progress = true;
                }
                Err(error) if error.runtime_dependent_reason().is_some() => {}
                Err(error) => {
                    return Err(ToDaeError::unsupported_flat(
                        "parameter binding",
                        format!("`{name}` cannot be evaluated: {error}"),
                        error.span().unwrap_or(variable.source_span),
                    ));
                }
            }
        }
        if !progress {
            break;
        }
    }
    register_deferred_parameters(flat, &mut context);
    Ok(context)
}

/// Name every `fixed = false` parameter the initialization system settles, and
/// what settles it.
///
/// MLS §4.4 lets a parameter declaration defer its value and MLS §8.6 gives the
/// initial section the job of establishing it. Such a parameter is absent from
/// the fold above for a *reason*, not by accident: it has no binding to fold.
/// A later consumer that asks the fold for its value — a `sample(start,
/// interval)` phase, a `delay` bound, a `Clock` argument — otherwise learns
/// only that the name has no value and reports it as unknown, which reads as a
/// name-resolution defect and hides the construct that was actually hit.
///
/// The distinction recorded here is the one that matters to those consumers:
/// a determining expression that reads `time` is settled by the *simulation
/// start instant*, because MLS §8.6 evaluates the initial section at the
/// initialization instant. That instant is chosen when the model is simulated,
/// not when it is translated, so no translation-time value exists for it at
/// all — as opposed to a value that merely waits for the initialization system
/// to run.
///
/// The two tiers are ordered, not symmetric: [`DeferredParameterSource::
/// StartInstant`] is a *proof* about the shape it recognizes, while
/// [`DeferredParameterSource::InitializationSystem`] is the conservative floor
/// every other deferred parameter falls to and asserts nothing about the start
/// instant either way. See [`deferred_parameter_source`] for exactly what is
/// proven and which start-instant-dependent shapes land in the weaker tier.
///
/// # Known remaining
///
/// A parameter that *has* a binding reading a deferred parameter is not
/// registered here — the guard below skips binding-holders, and the fold at
/// [`constant_context`] discards its failure through the
/// `runtime_dependent_reason` arm — so such a parameter is still reported as an
/// unknown name. This is real MSL, not a hypothetical:
/// `Modelica.Blocks.Math.ContinuousMean` (`Blocks/Math.mo:2349-2352`) declares
/// `parameter Real t_0(fixed = false)` with `initial equation t_0 = time` and
/// then `parameter Real actualStartTime = max(t_0, startTime)`, so
/// `actualStartTime` is start-instant-dependent through its binding. Closing it
/// needs one of: propagating the deferred source through the binding fold, or
/// registering a failed-fold binding over deferred free names as deferred
/// itself. Both belong with the start-relative-schedule work that would let
/// these sample starts construct at all, rather than with this diagnostic.
fn register_deferred_parameters(flat: &flat::Model, context: &mut EvalContext) {
    for (name, variable) in &flat.variables {
        if !matches!(variable.variability, Variability::Parameter(_))
            || variable.fixed != Some(false)
            || variable.binding.is_some()
            || context.instance_value(variable.instance_id).is_some()
        {
            continue;
        }
        context.add_deferred_parameter(name.to_string(), deferred_parameter_source(flat, name));
    }
}

/// What settles `target`: the start instant, or the initialization system.
///
/// This proves exactly one shape and claims nothing beyond it. `StartInstant`
/// is returned when some initial equation's residual is a *top-level*
/// subtraction with `target` itself as one direct operand, and the other
/// operand *syntactically* reads `time`. That is the shape flat lowering gives
/// `t0 = time`, and it is the shape both MSL sample-start sites are written in.
///
/// Everything else falls to `InitializationSystem`, which is therefore a floor
/// — "this parameter has no translation-time value" — and **not** a claim that
/// the value is independent of the start instant. Reachable shapes that do
/// depend on the start instant and still land in the weaker tier:
///
/// - indirectly, through another deferred parameter (`a = time; t0 = a`);
/// - through an initial *algorithm* target (`t0 := time`), which is owned by
///   [`initial_algorithms`] and never appears in `flat.initial_equations`;
/// - through a residual that is not a top-level subtraction on `target`
///   (`t0*t0 = time + 1.0`).
///
/// Each is refused either way, and the weaker label is true of all of them, so
/// the floor costs precision in the message and never correctness. Widening the
/// rule is deliberately left to the start-relative-schedule work, where a
/// start-instant dependency has to be *represented* rather than only named.
fn deferred_parameter_source(
    flat: &flat::Model,
    target: &VarName,
) -> rumoca_eval_flat::constant::DeferredParameterSource {
    use rumoca_eval_flat::constant::DeferredParameterSource;
    let determined_by_time = flat
        .initial_equations
        .iter()
        .filter_map(|equation| determining_value(&equation.residual, target))
        .any(reads_time);
    if determined_by_time {
        DeferredParameterSource::StartInstant
    } else {
        DeferredParameterSource::InitializationSystem
    }
}

/// The side of a `target - value` residual that is not `target`.
///
/// Flat lowering writes `t0 = time` as the residual `t0 - time`, so this
/// recognizes a residual that is a top-level subtraction with `target` as one
/// *direct* operand, and returns the other operand. It is a syntactic match on
/// that one shape, not a solve: a residual that merely constrains `target`
/// (`t0*t0 - (time + 1.0)`) yields `None`, and its caller then labels the
/// parameter by the weaker tier rather than inspecting it further.
fn determining_value<'a>(residual: &'a Expression, target: &VarName) -> Option<&'a Expression> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = residual
    else {
        return None;
    };
    if names_variable(lhs, target) {
        return Some(rhs);
    }
    if names_variable(rhs, target) {
        return Some(lhs);
    }
    None
}

fn names_variable(expression: &Expression, target: &VarName) -> bool {
    matches!(
        expression,
        Expression::VarRef { name, subscripts, .. }
            if subscripts.is_empty() && name.var_name() == target
    )
}

/// True when the expression reads `time`, whose value MLS §8.6 fixes at the
/// initialization instant.
fn reads_time(expression: &Expression) -> bool {
    if let Expression::VarRef { name, .. } = expression
        && name.as_str() == "time"
    {
        return true;
    }
    expression_children(expression).into_iter().any(reads_time)
}

fn validate_known_function_calls(
    expression: &Expression,
    flat: &flat::Model,
) -> Result<(), ToDaeError> {
    if let Expression::FunctionCall {
        name,
        args,
        is_constructor,
        span,
    } = expression
    {
        if *is_constructor && name.as_str().starts_with("__rumoca_named_arg__.") {
            if args.len() != 1 {
                return Err(ToDaeError::unsupported_flat(
                    "named function argument",
                    "a resolved named argument must contain exactly one value",
                    *span,
                ));
            }
        } else if enumeration_conversion(flat, name, args, *span)?.is_none() {
            let function = flat
                .functions
                .get(name.var_name())
                .ok_or_else(|| ToDaeError::unresolved_reference(name.as_str(), *span))?;
            if args.len() != function.inputs.len() {
                return Err(ToDaeError::unsupported_flat(
                    "function call arity",
                    format!(
                        "`{}` expects {} inputs but receives {}",
                        function.name,
                        function.inputs.len(),
                        args.len()
                    ),
                    *span,
                ));
            }
        }
    }
    for child in expression_children(expression) {
        validate_known_function_calls(child, flat)?;
    }
    Ok(())
}

pub(super) fn structured_assignment_names<'a>(
    target: &VarName,
    value: &Expression,
    names: impl IntoIterator<Item = &'a VarName>,
) -> Option<Vec<(VarName, VarName)>> {
    let Expression::VarRef {
        name, subscripts, ..
    } = value
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let target_prefix = format!("{target}.");
    let source_prefix = format!("{}.", name.var_name());
    let names = names.into_iter().cloned().collect::<HashSet<_>>();
    let target_leaves = names
        .iter()
        .filter(|candidate| candidate.as_str().starts_with(&target_prefix))
        .cloned()
        .collect::<Vec<_>>();
    let source_leaves = names
        .iter()
        .filter_map(|candidate| {
            candidate
                .as_str()
                .strip_prefix(&source_prefix)
                .map(str::to_owned)
        })
        .collect::<HashSet<_>>();
    if target_leaves.is_empty() || target_leaves.len() != source_leaves.len() {
        return None;
    }
    let mut pairs = Vec::with_capacity(target_leaves.len());
    for target_leaf in target_leaves {
        let suffix = target_leaf
            .as_str()
            .strip_prefix(&target_prefix)
            .expect("target leaves were selected by the same prefix");
        let source_leaf = VarName::new(format!("{source_prefix}{suffix}"));
        if !source_leaves.contains(suffix) || !names.contains(&source_leaf) {
            return None;
        }
        pairs.push((target_leaf, source_leaf));
    }
    pairs.sort_by(|(lhs, _), (rhs, _)| lhs.as_str().cmp(rhs.as_str()));
    Some(pairs)
}

pub(super) fn effective_function_scalar_type(
    flat: &flat::Model,
    value: &rumoca_core::FunctionParam,
) -> Option<dae::ScalarType> {
    let canonical = value.effective_type.canonical_type();
    if canonical == flat.predefined_types.real {
        Some(dae::ScalarType::Real)
    } else if canonical == flat.predefined_types.integer {
        Some(dae::ScalarType::Integer)
    } else if canonical == flat.predefined_types.boolean {
        Some(dae::ScalarType::Boolean)
    } else if canonical == flat.predefined_types.string {
        Some(dae::ScalarType::String)
    } else if flat.enumeration_type_roots.contains(&canonical) {
        Some(dae::ScalarType::Enumeration)
    } else {
        None
    }
}

pub(super) fn effective_variable_scalar_type(
    flat: &flat::Model,
    variable: &flat::Variable,
) -> Option<dae::ScalarType> {
    let canonical = flat
        .effective_types
        .get(&variable.type_id)?
        .canonical_type();
    if canonical == flat.predefined_types.real {
        Some(dae::ScalarType::Real)
    } else if canonical == flat.predefined_types.integer {
        Some(dae::ScalarType::Integer)
    } else if canonical == flat.predefined_types.boolean {
        Some(dae::ScalarType::Boolean)
    } else if canonical == flat.predefined_types.string {
        Some(dae::ScalarType::String)
    } else if flat.enumeration_types.contains(&variable.type_id) {
        Some(dae::ScalarType::Enumeration)
    } else {
        None
    }
}
