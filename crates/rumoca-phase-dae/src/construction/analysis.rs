use super::*;

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
mod model_algorithm_statements;
mod model_algorithms;
mod model_roles;
mod record_array_fields;
mod record_equations;
mod source_balance;
mod structured_families;
mod when_chains;
use clocks::SampledTarget;
use clocks::{ClockAnalysis, ClockDomainAnalysis, analyze_clocks};
pub(super) use clocks::{
    ClockPlan, ClockedValuePlan, is_inferred_clock_condition, is_whole_clock_coordinate,
};
use comprehensions::analyze_comprehensions;
pub(super) use comprehensions::{ComprehensionKey, ComprehensionPlan};
pub(super) use delays::DelayPlan;
use delays::analyze_delays;
pub(super) use derived_parameters::DerivedParameterPlan;
use derived_parameters::analyze_derived_parameters;
pub(super) use discrete_values::DiscreteValueTopologyPlan;
use discrete_values::analyze_discrete_value_topology;
use equation_partitions::defined_discrete_targets;
pub(super) use equation_partitions::{EquationPartition, equation_partition};
use event_conditions::{
    evaluate_clock_seconds, evaluate_sample_lattice, validate_algorithm_condition,
    validate_condition_expression,
};
pub(super) use expression_events::{ExpressionEventPlan, ExpressionEventPlans};
use expression_events::{analyze_expression_events, is_rescheduling_time_relation};
use expression_semi_linear::analyze_semi_linear_rules;
pub(super) use expression_semi_linear::{SemiLinearRowFilter, SemiLinearRules};
use expression_validation::{
    validate_expression, validate_expression_scoped_with_record_array_fields,
    validate_expression_with_record_array_fields, validate_specialized_expression,
    validate_specialized_subscripts, validate_subscripts_scoped,
};
use function_array_assemblies::coalesce_function_array_assemblies;
use function_bodies::{
    plan_function_statements, resolve_function_definitions,
    validate_function_expression_with_roles, validate_function_statements,
    validate_function_subscripts, validate_functions,
};
use function_conditionals::{plan_function_conditional, resolve_function_conditional};
use function_definitions::FunctionDefinitions;
pub(super) use function_definitions::FunctionValueSeed;
use function_externals::validate_external_function;
pub(super) use function_externals::{ExternalArgumentPlan, ExternalFunctionPlan};
use function_impurity::validate_impure_call_contexts;
use function_loops::{subscript_is_binder, validate_function_loop};
pub(super) use function_ranges::assigned_function_targets;
use function_ranges::{
    immutable_integer_defaults, static_function_range, validate_function_range_expression,
};
use function_record_assemblies::validate_record_output_assembly;
use function_reductions::validate_integer_reduction;
use function_returns::validate_guarded_function_return;
pub(super) use function_value_types::record_field_projections;
use function_value_types::validate_function_value_type;
pub(super) use initial_algorithms::InitialDiscreteValue;
use initial_algorithms::{
    InitialAlgorithmAnalysis, analyze_initial_algorithms,
    reject_unsupported_initial_algorithm_statements,
};
use model_algorithm_statements::validate_model_algorithm;
pub(super) use model_algorithms::ModelAlgorithmPlan;
use model_algorithms::analyze_model_algorithm;
pub(super) use model_algorithms::{
    algorithm_targets, event_targets, model_algorithm_targets, when_chain_targets,
};
use model_roles::{
    ModelRoles, analyze_model_roles, apply_clocked_partition_roles, is_predefined_clock_variable,
};
use record_array_fields::analyze_record_array_fields;
pub(super) use record_array_fields::{RecordArrayFieldPlan, RecordArrayFieldPlans};
use record_equations::analyze_record_equations;
use source_balance::source_balance;
use structured_families::validate_structured_families;
use when_chains::validate_when_chains;

pub(super) struct Analysis {
    pub(super) constants: EvalContext,
    pub(super) delay_plans: HashMap<Span, DelayPlan>,
    pub(super) roles: HashMap<VarName, PlannedRole>,
    pub(super) balance: BalanceDetail,
    pub(super) continuous_family_rows: HashSet<usize>,
    pub(super) initialization_family_rows: HashSet<usize>,
    pub(super) sample_lattices: Vec<(Span, ClockLattice)>,
    pub(super) expression_events: ExpressionEventPlans,
    pub(super) reinit_state_pre: HashSet<Span>,
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
    pub(super) comprehension_plans: HashMap<ComprehensionKey, ComprehensionPlan>,
    pub(super) record_array_fields: RecordArrayFieldPlans,
    pub(super) derived_parameters: HashMap<VarName, DerivedParameterPlan>,
    pub(super) derived_parameter_families: HashSet<usize>,
    pub(super) derived_parameter_rows: HashSet<usize>,
    pub(super) record_equations: HashMap<usize, RecordEquationPlan>,
    pub(super) initial_record_equations: HashMap<usize, RecordEquationPlan>,
    pub(super) discrete_value_topology: DiscreteValueTopologyPlan,
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
        statements: Vec<FunctionStatementPlan>,
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
    ArrayAssembly(FunctionArrayAssemblyPlan),
    ArrayAssemblyMember,
    RecordAssembly(FunctionRecordAssemblyPlan),
    RecordAssemblyMember,
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
    pub(super) loop_plan: Box<FunctionStatementPlan>,
}

pub(super) struct FunctionRecordAssemblyPlan {
    pub(super) target: VarName,
    pub(super) statement_count: usize,
    pub(super) fields: Vec<FunctionRecordFieldAssembly>,
}

pub(super) struct FunctionRecordFieldAssembly {
    pub(super) name: VarName,
    pub(super) dimensions: Vec<u32>,
    pub(super) scalars: Vec<FunctionRecordScalarSource>,
}

#[derive(Clone)]
pub(super) struct FunctionRecordScalarSource {
    pub(super) statement_offset: usize,
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
    pub(super) coordinate: VarName,
    pub(super) ordinal: usize,
}

pub(super) fn analyze(flat: &flat::Model) -> Result<Analysis, ToDaeError> {
    validate_flat_shape(flat)?;
    // The initial-algorithm statement grammar is proven before anything else
    // analyzes those statements. Analyzing them first reports a consequence of
    // an absent owner — a statement-form `assert` read as an unresolved callee,
    // for one — instead of the capability that is missing.
    reject_unsupported_initial_algorithm_statements(flat)?;
    validate_impure_call_contexts(flat)?;
    // The parameter fixed point is folded before any shape is proven: MLS §12.2
    // lets a function's array dimensions be parameter expressions, so the
    // settled values of the model's evaluable parameters (MLS §4.5) are part of
    // what proves a function's declared extents, not a later consequence of it.
    let constants = constant_context(flat)?;
    let function_shapes = FunctionShapeAnalysis::analyze(flat, &constants)?;
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
    validate_runtime_coordinate_instances(flat, &roles)?;
    let record_array_fields = analyze_record_array_field_plans(flat, &roles)?;
    let derived_parameters = analyze_derived_parameters(flat, &roles)?;
    apply_derived_parameter_roles(&derived_parameters.plans, &mut roles, &mut expression_roles);
    let clock_domains =
        analyze_clocked_partitions(flat, &clocks, &mut roles, &mut expression_roles)?;
    validate_model_expressions(flat, &expression_roles, &states, &record_array_fields)?;
    let continuous_family_rows = validate_structured_families(
        &flat.structured_equations,
        flat.equations.len(),
        &roles,
        &states,
        &record_array_fields,
    )?;
    let initialization_family_rows = validate_structured_families(
        &flat.initial_structured_equations,
        flat.initial_equations.len(),
        &roles,
        &states,
        &record_array_fields,
    )?;
    let mut sample_lattices = Vec::new();
    let reinit_state_pre = validate_when_chains(
        &flat.when_chains,
        &roles,
        &states,
        &constants,
        &mut sample_lattices,
    )?;
    let model_algorithm_plans = analyze_model_algorithms(
        flat,
        &roles,
        &expression_roles,
        &states,
        &constants,
        &mut sample_lattices,
    )?;
    let discrete_value_topology = analyze_discrete_value_topology(flat, &roles)?;
    let initial_algorithms =
        analyze_initial_algorithm_owners(flat, &roles, &states, &constants, &mut sample_lattices)?;
    let balance = analyze_source_balance(
        flat,
        &roles,
        &clocks.equation_rows,
        &derived_parameters.rows,
        &record_equations,
    )?;
    let expression_events = analyze_expression_events(flat, &roles, &constants)?;
    Ok(Analysis {
        constants,
        delay_plans,
        roles,
        balance: balance.detail,
        continuous_family_rows,
        initialization_family_rows,
        sample_lattices,
        expression_events,
        reinit_state_pre,
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
        initial_record_equations,
        discrete_value_topology,
        assigned_discrete_targets: balance.assigned_discrete_targets,
        semi_linear_rules: SemiLinearRules::default(),
    })
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
    roles: &mut HashMap<VarName, PlannedRole>,
    expression_roles: &mut HashMap<VarName, PlannedRole>,
) -> Result<ClockDomainAnalysis, ToDaeError> {
    let domains = clocks::analyze_clock_domains(
        flat,
        roles,
        &clocks.plans,
        &clocks.equation_rows,
        &clocks.sampled_targets,
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
    )
}

fn analyze_model_algorithms(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    expression_roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, ClockLattice)>,
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
            analyze_model_algorithm(flat, algorithm, roles)
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
) -> Result<(), ToDaeError> {
    for variable in flat.variables.values() {
        for expression in variable_attribute_expressions(variable) {
            if let Some(span) = empty_array_bound_to_declaration(variable, expression) {
                // The owning declaration proves the element type and extent, so
                // the literal carries no operand that needs validating.
                require_span(span, "empty array attribute")?;
            } else {
                validate_expression_with_record_array_fields(
                    expression,
                    roles,
                    states,
                    record_array_fields,
                )?;
            }
            validate_known_function_calls(expression, flat)?;
        }
    }
    for expression in flat
        .equations
        .iter()
        .chain(flat.initial_equations.iter())
        .map(|equation| &equation.residual)
    {
        validate_expression_with_record_array_fields(
            expression,
            roles,
            states,
            record_array_fields,
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

fn analyze_source_balance(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    clock_equation_rows: &HashSet<usize>,
    derived_parameter_rows: &HashSet<usize>,
    record_equations: &HashMap<usize, RecordEquationPlan>,
) -> Result<SourceBalanceAnalysis, ToDaeError> {
    let assigned_discrete_targets = defined_discrete_targets(flat, roles)?;
    let mut non_runtime_rows = clock_equation_rows.clone();
    non_runtime_rows.extend(derived_parameter_rows);
    let detail = source_balance(
        flat,
        roles,
        &assigned_discrete_targets,
        &non_runtime_rows,
        record_equations,
    )?;
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
    sample_lattices: &mut Vec<(Span, ClockLattice)>,
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
    sample_lattices: &mut Vec<(Span, ClockLattice)>,
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

fn analyze_record_array_field_plans(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<RecordArrayFieldPlans, ToDaeError> {
    analyze_record_array_fields(
        flat,
        all_model_expressions(flat)
            .chain(structured_template_expressions(&flat.structured_equations))
            .chain(structured_template_expressions(
                &flat.initial_structured_equations,
            )),
        roles,
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
    Ok(context)
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
