use super::*;

mod clocks;
mod comprehensions;
mod delays;
mod derived_parameters;
mod discrete_values;
mod equation_partitions;
mod event_conditions;
mod expression_validation;
mod function_array_assemblies;
mod function_bodies;
mod function_conditionals;
mod function_loops;
mod function_ranges;
mod function_record_assemblies;
mod function_reductions;
mod function_returns;
mod function_value_types;
mod model_algorithm_statements;
mod model_algorithms;
mod model_roles;
mod record_array_fields;
mod record_equations;
mod source_balance;
mod structured_families;
mod when_chains;
use clocks::analyze_clocks;
pub(super) use clocks::{ClockPlan, ClockedValuePlan};
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
    evaluate_clock_seconds, validate_algorithm_condition, validate_condition_expression,
};
use expression_validation::{
    validate_expression, validate_expression_scoped_with_record_array_fields,
    validate_expression_with_record_array_fields, validate_subscripts_scoped,
};
use function_array_assemblies::coalesce_function_array_assemblies;
use function_bodies::{
    validate_function_expression_with_roles, validate_function_statements,
    validate_function_subscripts, validate_functions,
};
use function_conditionals::validate_function_conditional;
use function_loops::{subscript_is_binder, validate_function_loop};
use function_ranges::{
    immutable_integer_defaults, static_function_range, validate_function_range_expression,
};
use function_record_assemblies::validate_record_output_assembly;
use function_reductions::validate_integer_reduction;
use function_returns::validate_guarded_function_return;
use function_value_types::validate_function_value_type;
use model_algorithm_statements::validate_model_algorithm;
pub(super) use model_algorithms::ModelAlgorithmPlan;
use model_algorithms::analyze_model_algorithm;
pub(super) use model_algorithms::{
    algorithm_targets, event_targets, model_algorithm_targets, when_chain_targets,
};
use model_roles::{ModelRoles, analyze_model_roles, is_predefined_clock_variable};
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
    pub(super) reinit_state_pre: HashSet<Span>,
    pub(super) clock_plans: HashMap<InstanceId, ClockPlan>,
    pub(super) clock_equation_rows: HashSet<usize>,
    pub(super) clocked_equation_owners: HashMap<usize, ClockPlan>,
    pub(super) clocked_value_owners: HashMap<InstanceId, ClockedValuePlan>,
    pub(super) model_algorithm_plans: Vec<ModelAlgorithmPlan>,
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

pub(super) fn required_statement_span(
    statement: &rumoca_core::Statement,
    owner: impl Into<String>,
) -> Result<Span, ToDaeError> {
    let kind = match statement {
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
    };
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
    let function_shapes = FunctionShapeAnalysis::analyze(flat)?;
    let function_plans = validate_functions(flat, &function_shapes)?;
    let record_equations = analyze_record_equations(flat, &flat.equations)?;
    let initial_record_equations = analyze_record_equations(flat, &flat.initial_equations)?;
    let constants = constant_context(flat);
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
    let clock_domains = clocks::analyze_clock_domains(
        flat,
        &roles,
        &clocks.plans,
        &clocks.equation_rows,
        &clocks.sampled_targets,
    )?;
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
    let model_algorithm_plans = flat
        .algorithms
        .iter()
        .map(|algorithm| {
            validate_model_algorithm(
                algorithm,
                &expression_roles,
                &states,
                &constants,
                &mut sample_lattices,
            )?;
            analyze_model_algorithm(flat, algorithm, &roles)
        })
        .collect::<Result<Vec<_>, _>>()?;
    let discrete_value_topology = analyze_discrete_value_topology(flat, &roles)?;
    reject_initial_algorithm(flat)?;
    validate_assertions(flat, &roles, &states, &constants, &mut sample_lattices)?;
    let balance = analyze_source_balance(
        flat,
        &roles,
        &clocks.equation_rows,
        &derived_parameters.rows,
        &record_equations,
    )?;
    Ok(Analysis {
        constants,
        delay_plans,
        roles,
        balance: balance.detail,
        continuous_family_rows,
        initialization_family_rows,
        sample_lattices,
        reinit_state_pre,
        clock_plans: clocks.plans,
        clock_equation_rows: clocks.equation_rows,
        clocked_equation_owners: clock_domains.equation_owners,
        clocked_value_owners: clock_domains.value_owners,
        model_algorithm_plans,
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
    })
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
    for expression in all_model_expressions(flat) {
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

fn validate_assertions(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, ClockLattice)>,
) -> Result<(), ToDaeError> {
    for assertion in flat
        .assert_equations
        .iter()
        .chain(&flat.initial_assert_equations)
    {
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

fn reject_initial_algorithm(flat: &flat::Model) -> Result<(), ToDaeError> {
    flat.initial_algorithms.first().map_or(Ok(()), |algorithm| {
        Err(ToDaeError::unsupported_algorithm(
            "initial",
            &algorithm.origin,
            algorithm.span,
        ))
    })
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

fn constant_context(flat: &flat::Model) -> EvalContext {
    let mut context = EvalContext::with_capacity(flat.variables.len(), 0, flat.functions.len() * 2);
    context.enable_unique_suffix_lookup();
    for function in flat.functions.values() {
        context.add_function(function.clone());
    }
    for (name, variable) in &flat.variables {
        context.add_array_dimensions(name.to_string(), variable.dims.clone());
    }
    for _ in 0..flat.variables.len() {
        let mut progress = false;
        for (name, variable) in &flat.variables {
            if context.get(name.as_str()).is_some()
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
            let Ok(value) = eval_expr(binding, &context) else {
                continue;
            };
            context.add_parameter(name.to_string(), value);
            progress = true;
        }
        if !progress {
            break;
        }
    }
    context
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
        } else {
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
