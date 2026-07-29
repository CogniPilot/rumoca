use super::*;

mod clocks;
mod comprehensions;
mod derived_parameters;
mod expression_validation;
mod function_array_assemblies;
mod function_conditionals;
mod function_ranges;
mod function_record_assemblies;
mod function_value_types;
mod model_algorithms;
mod model_roles;
mod record_array_fields;
mod record_equations;
mod source_balance;
mod structured_families;
mod when_clauses;
use clocks::analyze_clocks;
pub(super) use clocks::{ClockPlan, SampledValuePlan};
use comprehensions::analyze_comprehensions;
pub(super) use comprehensions::{ComprehensionKey, ComprehensionPlan};
pub(super) use derived_parameters::DerivedParameterPlan;
use derived_parameters::analyze_derived_parameters;
use expression_validation::{
    require_integer_literal, validate_array, validate_array_comprehension,
    validate_binary_operator, validate_builtin, validate_conditional, validate_subscripts_scoped,
    validate_unary_operator,
};
use function_array_assemblies::coalesce_function_array_assemblies;
use function_conditionals::validate_function_conditional;
use function_ranges::{
    immutable_integer_defaults, static_function_range, validate_function_range_expression,
};
use function_record_assemblies::validate_record_output_assembly;
use function_value_types::validate_function_value_type;
pub(super) use model_algorithms::ModelAlgorithmPlan;
use model_algorithms::analyze_model_algorithm;
pub(super) use model_algorithms::{
    algorithm_targets, event_targets, model_algorithm_targets, when_clause_targets,
};
use model_roles::{ModelRoles, analyze_model_roles};
pub(super) use record_array_fields::RecordArrayFieldPlan;
use record_array_fields::{analyze_record_array_fields, expression_for_validation};
use record_equations::analyze_record_equations;
use source_balance::source_balance;
use structured_families::validate_structured_families;
use when_clauses::validate_when_clauses;

pub(super) struct Analysis {
    pub(super) constants: EvalContext,
    pub(super) roles: HashMap<VarName, PlannedRole>,
    pub(super) balance: BalanceDetail,
    pub(super) continuous_family_rows: HashSet<usize>,
    pub(super) initialization_family_rows: HashSet<usize>,
    pub(super) sample_lattices: Vec<(Span, ClockLattice)>,
    pub(super) reinit_state_pre: HashSet<Span>,
    pub(super) clock_plans: HashMap<VarName, ClockPlan>,
    pub(super) clock_equation_rows: HashSet<usize>,
    pub(super) sampled_values: HashMap<VarName, SampledValuePlan>,
    pub(super) model_algorithm_plans: Vec<ModelAlgorithmPlan>,
    pub(super) function_plans: HashMap<FunctionSpecializationKey, FunctionPlan>,
    pub(super) function_shapes: FunctionShapeAnalysis,
    pub(super) comprehension_plans: HashMap<ComprehensionKey, ComprehensionPlan>,
    pub(super) record_array_fields: HashMap<Span, RecordArrayFieldPlan>,
    pub(super) derived_parameters: HashMap<VarName, DerivedParameterPlan>,
    pub(super) derived_parameter_families: HashSet<usize>,
    pub(super) derived_parameter_rows: HashSet<usize>,
    pub(super) record_equations: HashMap<usize, RecordEquationPlan>,
    pub(super) initial_record_equations: HashMap<usize, RecordEquationPlan>,
}

pub(super) struct FunctionPlan {
    pub(super) statements: Vec<FunctionStatementPlan>,
}

pub(super) enum FunctionStatementPlan {
    Assignment {
        target: VarName,
        subscript_count: usize,
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
    ArrayAssembly(FunctionArrayAssemblyPlan),
    ArrayAssemblyMember,
    RecordAssembly(FunctionRecordAssemblyPlan),
    RecordAssemblyMember,
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
    TotalArrayDefinition { target: VarName },
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

#[derive(Clone, Copy)]
pub(super) enum EquationPartition<'flat> {
    Continuous,
    DiscreteReal {
        target: &'flat VarName,
    },
    DiscreteValue {
        target: &'flat VarName,
        value: &'flat Expression,
    },
}

pub(super) fn analyze(flat: &flat::Model) -> Result<Analysis, ToDaeError> {
    flat.validate().map_err(|error| {
        ToDaeError::unsupported_flat("Flat shape contract", format!("{error:?}"), error.span())
    })?;
    let function_shapes = FunctionShapeAnalysis::analyze(flat)?;
    let function_plans = validate_functions(flat, &function_shapes)?;
    let record_equations = analyze_record_equations(flat, &flat.equations)?;
    let initial_record_equations = analyze_record_equations(flat, &flat.initial_equations)?;
    let constants = constant_context(flat);
    let comprehension_plans = analyze_comprehensions(all_model_expressions(flat), &constants)?;
    let record_array_fields = analyze_record_array_field_plans(flat)?;
    let clocks = analyze_clocks(flat, &constants)?;
    let ModelRoles {
        states,
        variables: mut roles,
        expressions: mut expression_roles,
    } = analyze_model_roles(flat, &clocks.sampled_values)?;
    let derived_parameters = analyze_derived_parameters(flat, &roles)?;
    for name in derived_parameters.plans.keys() {
        roles.insert(name.clone(), PlannedRole::Parameter);
        expression_roles.insert(name.clone(), PlannedRole::Parameter);
    }
    for expression in all_model_expressions(flat) {
        let validation_expression = expression_for_validation(expression, &record_array_fields);
        validate_expression(&validation_expression, &expression_roles, &states)?;
        validate_known_function_calls(expression, flat)?;
    }
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
    let reinit_state_pre = validate_when_clauses(
        &flat.when_clauses,
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
    reject_initial_algorithm(flat)?;
    validate_assertions(flat, &roles, &states, &constants, &mut sample_lattices)?;
    let mut non_runtime_rows = clocks.equation_rows.clone();
    non_runtime_rows.extend(&derived_parameters.rows);
    let balance = source_balance(flat, &roles, &non_runtime_rows, &record_equations)?;
    Ok(Analysis {
        constants,
        roles,
        balance,
        continuous_family_rows,
        initialization_family_rows,
        sample_lattices,
        reinit_state_pre,
        clock_plans: clocks.plans,
        clock_equation_rows: clocks.equation_rows,
        sampled_values: clocks.sampled_values,
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
) -> Result<HashMap<Span, RecordArrayFieldPlan>, ToDaeError> {
    analyze_record_array_fields(
        flat,
        all_model_expressions(flat)
            .chain(structured_template_expressions(&flat.structured_equations))
            .chain(structured_template_expressions(
                &flat.initial_structured_equations,
            )),
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

fn validate_condition_expression(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, ClockLattice)>,
) -> Result<(), ToDaeError> {
    match expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args,
            span,
        } => {
            let lattice = evaluate_sample_lattice(args, constants, *span)?;
            if !sample_lattices
                .iter()
                .any(|(existing, _)| *existing == *span)
            {
                sample_lattices.push((*span, lattice));
            }
            Ok(())
        }
        Expression::Unary {
            op: OpUnary::Not,
            rhs,
            ..
        } => validate_condition_expression(rhs, roles, states, constants, sample_lattices),
        Expression::Binary {
            op: OpBinary::And | OpBinary::Or,
            lhs,
            rhs,
            ..
        } => {
            validate_condition_expression(lhs, roles, states, constants, sample_lattices)?;
            validate_condition_expression(rhs, roles, states, constants, sample_lattices)
        }
        _ => validate_expression(expression, roles, states),
    }
}

fn evaluate_sample_lattice(
    arguments: &[Expression],
    constants: &EvalContext,
    span: Span,
) -> Result<ClockLattice, ToDaeError> {
    let [start, interval] = arguments else {
        return Err(ToDaeError::unsupported_runtime_operator(
            "sample",
            "sample(start, interval) requires exactly two scalar parameter arguments",
            span,
        ));
    };
    let start = evaluate_clock_seconds(start, constants, "sample start", span)?;
    let interval = evaluate_clock_seconds(interval, constants, "sample interval", span)?;
    let phase = ClockRational::from_seconds(start).map_err(|error| {
        ToDaeError::unsupported_runtime_operator("sample", error.to_string(), span)
    })?;
    let period = ClockRational::from_seconds(interval).map_err(|error| {
        ToDaeError::unsupported_runtime_operator("sample", error.to_string(), span)
    })?;
    ClockLattice::new(period, phase).map_err(|error| {
        ToDaeError::unsupported_runtime_operator("sample", error.to_string(), span)
    })
}

fn evaluate_clock_seconds(
    expression: &Expression,
    constants: &EvalContext,
    owner: &'static str,
    span: Span,
) -> Result<f64, ToDaeError> {
    let value = eval_expr(expression, constants).map_err(|error| {
        ToDaeError::unsupported_runtime_operator(
            "sample",
            format!("{owner} is not parameter-evaluable: {error}"),
            span,
        )
    })?;
    value
        .to_real()
        .filter(|value| value.is_finite())
        .ok_or_else(|| {
            ToDaeError::unsupported_runtime_operator(
                "sample",
                format!("{owner} must evaluate to a finite scalar Real"),
                span,
            )
        })
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

fn validate_model_algorithm(
    algorithm: &flat::Algorithm,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, ClockLattice)>,
) -> Result<(), ToDaeError> {
    require_span(algorithm.span, "model algorithm")?;
    validate_algorithm_statements(
        &algorithm.statements,
        roles,
        states,
        constants,
        sample_lattices,
        algorithm.span,
    )
}

// SPEC_0021 exception: exhaustive statement-grammar validation keeps each
// accepted form and its provenance checks visible at one boundary.
#[allow(clippy::too_many_lines)]
fn validate_algorithm_statements(
    statements: &[rumoca_core::Statement],
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, ClockLattice)>,
    owner_span: Span,
) -> Result<(), ToDaeError> {
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, value, span } => {
                require_span(*span, "algorithm assignment")?;
                if comp.parts.is_empty() {
                    return Err(ToDaeError::unsupported_algorithm(
                        "model",
                        "empty assignment target",
                        *span,
                    ));
                }
                let target = comp.to_var_name();
                let target_role = roles.get(&target);
                if comp.parts.iter().any(|part| !part.subs.is_empty()) {
                    return Err(ToDaeError::unsupported_algorithm(
                        "model",
                        format!(
                            "algorithm assignment target `{target}` is not a whole writable \
                             coordinate (resolved role: {target_role:?})"
                        ),
                        *span,
                    ));
                }
                if matches!(
                    target_role,
                    Some(
                        PlannedRole::Algebraic
                            | PlannedRole::Output
                            | PlannedRole::DiscreteReal
                            | PlannedRole::DiscreteValue
                    )
                ) {
                    validate_expression(value, roles, states)?;
                } else if structured_assignment_pairs(&target, value, roles).is_none() {
                    return Err(ToDaeError::unsupported_algorithm(
                        "model",
                        format!(
                            "algorithm assignment target `{target}` is not a whole writable \
                             coordinate (resolved role: {target_role:?})"
                        ),
                        *span,
                    ));
                }
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                span,
            } => {
                require_span(*span, "algorithm if statement")?;
                for block in cond_blocks {
                    validate_algorithm_condition(
                        &block.cond,
                        roles,
                        states,
                        constants,
                        sample_lattices,
                    )?;
                    validate_algorithm_statements(
                        &block.stmts,
                        roles,
                        states,
                        constants,
                        sample_lattices,
                        owner_span,
                    )?;
                }
                if let Some(statements) = else_block {
                    validate_algorithm_statements(
                        statements,
                        roles,
                        states,
                        constants,
                        sample_lattices,
                        owner_span,
                    )?;
                }
            }
            rumoca_core::Statement::When { blocks, span } => {
                require_span(*span, "algorithm when statement")?;
                if blocks.is_empty() {
                    return Err(ToDaeError::unsupported_algorithm(
                        "model",
                        "when statement must contain at least one guarded block",
                        *span,
                    ));
                }
                for block in blocks {
                    validate_algorithm_condition(
                        &block.cond,
                        roles,
                        states,
                        constants,
                        sample_lattices,
                    )?;
                    validate_algorithm_statements(
                        &block.stmts,
                        roles,
                        states,
                        constants,
                        sample_lattices,
                        owner_span,
                    )?;
                }
            }
            rumoca_core::Statement::FunctionCall {
                comp,
                args,
                outputs,
                span,
            } => {
                require_span(*span, "algorithm function-call assignment")?;
                if comp.parts.is_empty() || comp.parts.iter().any(|part| !part.subs.is_empty()) {
                    return Err(ToDaeError::unsupported_algorithm(
                        "model",
                        "function-call assignment requires one resolved, unsubscripted function",
                        *span,
                    ));
                }
                if outputs.is_empty() || outputs.iter().all(Option::is_none) {
                    return Err(ToDaeError::unsupported_algorithm(
                        "model",
                        "function-call assignment must retain at least one output",
                        *span,
                    ));
                }
                for argument in args {
                    validate_expression(argument, roles, states)?;
                }
                for output in outputs.iter().flatten() {
                    validate_function_call_output(output, roles)?;
                }
            }
            _ => {
                return Err(ToDaeError::unsupported_algorithm(
                    "model",
                    "statement must be an assignment, function-call assignment, or conditional \
                     discrete update",
                    statement.source_span().unwrap_or(owner_span),
                ));
            }
        }
    }
    Ok(())
}

fn validate_function_call_output(
    output: &rumoca_core::ComponentReference,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<(), ToDaeError> {
    let target = output.to_var_name();
    let is_whole_coordinate =
        !output.parts.is_empty() && output.parts.iter().all(|part| part.subs.is_empty());
    let is_discrete = matches!(
        roles.get(&target),
        Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
    );
    if is_whole_coordinate && is_discrete {
        return Ok(());
    }
    Err(ToDaeError::unsupported_algorithm(
        "model",
        format!("function-call output `{target}` is not a whole discrete coordinate"),
        output.span,
    ))
}

fn validate_algorithm_condition(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, ClockLattice)>,
) -> Result<(), ToDaeError> {
    match expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Change,
            args,
            span,
        } => {
            let [argument] = args.as_slice() else {
                return Err(ToDaeError::unsupported_algorithm(
                    "model",
                    "change(...) requires exactly one discrete coordinate",
                    *span,
                ));
            };
            let Some((name, _)) = derivative_reference(argument) else {
                return Err(ToDaeError::unsupported_algorithm(
                    "model",
                    "change(...) requires a discrete coordinate reference",
                    *span,
                ));
            };
            if !matches!(
                roles.get(name.var_name()),
                Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
            ) {
                return Err(ToDaeError::unsupported_algorithm(
                    "model",
                    "change(...) requires a discrete coordinate",
                    *span,
                ));
            }
            Ok(())
        }
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args,
            span,
        } => {
            let lattice = evaluate_sample_lattice(args, constants, *span)?;
            if !sample_lattices
                .iter()
                .any(|(existing, _)| *existing == *span)
            {
                sample_lattices.push((*span, lattice));
            }
            Ok(())
        }
        Expression::Unary {
            op: OpUnary::Not,
            rhs,
            ..
        } => validate_algorithm_condition(rhs, roles, states, constants, sample_lattices),
        Expression::Binary {
            op: OpBinary::And | OpBinary::Or,
            lhs,
            rhs,
            ..
        } => {
            validate_algorithm_condition(lhs, roles, states, constants, sample_lattices)?;
            validate_algorithm_condition(rhs, roles, states, constants, sample_lattices)
        }
        _ => validate_expression(expression, roles, states),
    }
}

fn validate_functions(
    flat: &flat::Model,
    shapes: &FunctionShapeAnalysis,
) -> Result<HashMap<FunctionSpecializationKey, FunctionPlan>, ToDaeError> {
    let mut plans = HashMap::with_capacity(shapes.certificates().len());
    for certificate in shapes.certificates() {
        let function = &flat.functions[&certificate.key.function];
        require_span(function.span, "function declaration")?;
        if function.external.is_some() || !function.pure || function.is_constructor {
            return Err(ToDaeError::unsupported_flat(
                "function lifecycle",
                format!("`{}` is not a pure Modelica function body", function.name),
                function.span,
            ));
        }
        for parameter in function
            .inputs
            .iter()
            .chain(&function.outputs)
            .chain(&function.locals)
        {
            require_span(parameter.span, "function parameter declaration")?;
            validate_function_value_type(parameter, function, flat, &mut HashSet::new())?;
            if let Some(default) = &parameter.default {
                validate_function_expression(default, function, flat)?;
            }
        }
        let static_integers = immutable_integer_defaults(function);
        let roles = function_expression_roles(function, flat);
        let context = FunctionValidationContext {
            function,
            flat,
            roles: &roles,
            static_integers: &static_integers,
            shapes: &certificate.values,
            shape_analysis: shapes,
        };
        let statements = validate_function_statements(&function.body, context)?;
        plans.insert(certificate.key.clone(), FunctionPlan { statements });
    }
    Ok(plans)
}

fn validate_function_expression(
    expression: &Expression,
    function: &rumoca_core::Function,
    flat: &flat::Model,
) -> Result<(), ToDaeError> {
    let roles = function_expression_roles(function, flat);
    validate_function_expression_with_roles(expression, &roles, flat)
}

fn function_expression_roles(
    function: &rumoca_core::Function,
    flat: &flat::Model,
) -> HashMap<VarName, PlannedRole> {
    let mut roles = function
        .inputs
        .iter()
        .map(|parameter| (VarName::new(&parameter.name), PlannedRole::Parameter))
        .collect::<HashMap<_, _>>();
    roles.extend(
        function
            .outputs
            .iter()
            .chain(&function.locals)
            .map(|value| (VarName::new(&value.name), PlannedRole::Parameter)),
    );
    for literal in flat.enum_literal_ordinals.keys() {
        roles.insert(VarName::new(literal), PlannedRole::EnumerationLiteral);
    }
    roles
}

fn validate_function_expression_with_roles(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    flat: &flat::Model,
) -> Result<(), ToDaeError> {
    validate_expression(expression, roles, &HashSet::new())?;
    validate_known_function_calls(expression, flat)
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

fn validate_function_statements(
    statements: &[rumoca_core::Statement],
    context: FunctionValidationContext<'_>,
) -> Result<Vec<FunctionStatementPlan>, ToDaeError> {
    let mut plans = Vec::with_capacity(statements.len());
    let mut index = 0usize;
    while index < statements.len() {
        if let Some((assembly, count)) =
            validate_record_output_assembly(statements, index, context)?
        {
            plans.push(FunctionStatementPlan::RecordAssembly(assembly));
            plans.extend(
                std::iter::repeat_with(|| FunctionStatementPlan::RecordAssemblyMember)
                    .take(count - 1),
            );
            index += count;
            continue;
        }
        let statement = &statements[index];
        match statement {
            rumoca_core::Statement::Assignment { comp, value, span } => {
                require_span(*span, "function assignment")?;
                let (target, subscript_count) =
                    validate_function_assignment_target(context.function, comp, *span)?;
                validate_function_assignment_subscripts(comp, context.roles, context.flat)?;
                validate_function_expression_with_roles(value, context.roles, context.flat)?;
                plans.push(FunctionStatementPlan::Assignment {
                    target,
                    subscript_count,
                });
            }
            rumoca_core::Statement::For { .. } => {
                plans.push(validate_function_loop(statement, context)?)
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                span,
            } => plans.push(validate_function_conditional(
                cond_blocks,
                else_block.as_deref(),
                *span,
                context,
            )?),
            _ => {
                return Err(ToDaeError::unsupported_flat(
                    "function statement",
                    format!(
                        "`{}` contains a statement without a checked DAE owner",
                        context.function.name
                    ),
                    statement.source_span().unwrap_or(context.function.span),
                ));
            }
        }
        index += 1;
    }
    coalesce_function_array_assemblies(statements, &mut plans, context)?;
    Ok(plans)
}

fn validate_function_loop(
    statement: &rumoca_core::Statement,
    context: FunctionValidationContext<'_>,
) -> Result<FunctionStatementPlan, ToDaeError> {
    let rumoca_core::Statement::For {
        indices,
        equations,
        span,
    } = statement
    else {
        unreachable!("function-loop validation receives a for statement")
    };
    require_span(*span, "function for statement")?;
    if indices.is_empty() {
        return Err(ToDaeError::unsupported_flat(
            "function loop domain",
            "a function for statement must declare at least one index",
            *span,
        ));
    }
    let validated = validate_function_loop_domain(indices, *span, context)?;
    if let Some(plan) =
        validate_nested_function_loop(indices, equations, *span, context, validated.clone())?
    {
        return Ok(plan);
    }
    validate_function_loop_body(indices, equations, *span, context, validated)
}

#[derive(Clone)]
struct ValidatedFunctionLoop {
    domain: StructuredIndexDomain,
    binder_spans: Vec<Span>,
    roles: HashMap<VarName, PlannedRole>,
}

fn validate_function_loop_domain(
    indices: &[rumoca_core::ForIndex],
    span: Span,
    context: FunctionValidationContext<'_>,
) -> Result<ValidatedFunctionLoop, ToDaeError> {
    let mut loop_roles = context.roles.clone();
    let mut binders = Vec::with_capacity(indices.len());
    let mut binder_spans = Vec::with_capacity(indices.len());
    for (ordinal, index) in indices.iter().enumerate() {
        let range_span = expression_span(&index.range)?;
        validate_function_range_expression(&index.range, context.roles, context.flat)?;
        let Some((lower, step, upper)) =
            static_function_range(&index.range, context.static_integers, context.shapes)?
        else {
            return Err(ToDaeError::unsupported_flat(
                "function loop domain",
                format!(
                    "`{}.{}` does not have a finite statically proven Integer range",
                    context.function.name, index.ident
                ),
                range_span,
            ));
        };
        binders.push(StructuredIndexBinder {
            id: ordinal,
            display_name: index.ident.clone(),
            lower,
            upper,
            step,
        });
        binder_spans.push(range_span);
        loop_roles.insert(VarName::new(&index.ident), PlannedRole::Parameter);
    }
    let domain = StructuredIndexDomain { binders };
    domain.scalar_count().map_err(|error| {
        ToDaeError::unsupported_flat(
            "function loop domain",
            format!(
                "`{}` has an invalid compact domain: {error}",
                context.function.name
            ),
            span,
        )
    })?;
    Ok(ValidatedFunctionLoop {
        domain,
        binder_spans,
        roles: loop_roles,
    })
}

fn validate_nested_function_loop(
    indices: &[rumoca_core::ForIndex],
    equations: &[rumoca_core::Statement],
    span: Span,
    context: FunctionValidationContext<'_>,
    mut validated: ValidatedFunctionLoop,
) -> Result<Option<FunctionStatementPlan>, ToDaeError> {
    if !equations
        .iter()
        .any(|statement| matches!(statement, rumoca_core::Statement::For { .. }))
    {
        return Ok(None);
    }
    let [nested @ rumoca_core::Statement::For { .. }] = equations else {
        return Err(ToDaeError::unsupported_flat(
            "nested function loop",
            format!(
                "`{}` needs a perfect loop nest before product-domain lowering",
                context.function.name
            ),
            span,
        ));
    };
    let nested_context = FunctionValidationContext {
        roles: &validated.roles,
        ..context
    };
    let FunctionStatementPlan::For {
        domain: nested_domain,
        binder_spans: nested_spans,
        statements,
        source_depth,
        ..
    } = validate_function_loop(nested, nested_context)?
    else {
        unreachable!("recursive function-loop validation returns a loop")
    };
    let offset = validated.domain.binders.len();
    validated
        .domain
        .binders
        .extend(nested_domain.binders.into_iter().map(|mut binder| {
            binder.id += offset;
            binder
        }));
    validated.binder_spans.extend(nested_spans);
    validated.domain.scalar_count().map_err(|error| {
        ToDaeError::unsupported_flat(
            "function loop domain",
            format!(
                "`{}` has an invalid product domain: {error}",
                context.function.name
            ),
            span,
        )
    })?;
    let source_depth = source_depth + 1;
    let (loop_indices, loop_statements) =
        flattened_function_loop_source(indices, equations, source_depth);
    let lowering = classify_function_loop(
        &validated.domain,
        &loop_indices,
        loop_statements,
        &statements,
        context.function,
    );
    Ok(Some(FunctionStatementPlan::For {
        domain: validated.domain,
        binder_spans: validated.binder_spans,
        lowering,
        statements,
        source_depth,
    }))
}

fn validate_function_loop_body(
    indices: &[rumoca_core::ForIndex],
    equations: &[rumoca_core::Statement],
    span: Span,
    context: FunctionValidationContext<'_>,
    validated: ValidatedFunctionLoop,
) -> Result<FunctionStatementPlan, ToDaeError> {
    let body_context = FunctionValidationContext {
        roles: &validated.roles,
        ..context
    };
    let statements = validate_function_statements(equations, body_context)?;
    let targets = function_loop_targets(&statements);
    if targets.is_empty() {
        return Err(ToDaeError::unsupported_flat(
            "function loop transition",
            format!(
                "`{}` has no loop-carried function value",
                context.function.name
            ),
            span,
        ));
    }
    let loop_indices = indices.iter().collect::<Vec<_>>();
    let lowering = classify_function_loop(
        &validated.domain,
        &loop_indices,
        equations,
        &statements,
        context.function,
    );
    Ok(FunctionStatementPlan::For {
        domain: validated.domain,
        binder_spans: validated.binder_spans,
        lowering,
        statements,
        source_depth: 1,
    })
}

fn classify_function_loop(
    domain: &StructuredIndexDomain,
    indices: &[&rumoca_core::ForIndex],
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
    function: &rumoca_core::Function,
) -> FunctionLoopLowering {
    let targets = function_loop_targets(plans);
    let (
        [rumoca_core::Statement::Assignment { comp, value, .. }],
        [
            FunctionStatementPlan::Assignment {
                target,
                subscript_count,
            },
        ],
    ) = (statements, plans)
    else {
        return FunctionLoopLowering::Fold { targets };
    };
    let Some(declaration) = function
        .outputs
        .iter()
        .chain(&function.locals)
        .find(|declaration| declaration.name == target.as_str())
    else {
        return FunctionLoopLowering::Fold { targets };
    };
    let Ok(extents) = domain.extents() else {
        return FunctionLoopLowering::Fold { targets };
    };
    let dimensions = declaration
        .dims
        .iter()
        .map(|dimension| usize::try_from(*dimension))
        .collect::<Result<Vec<_>, _>>();
    let Some(subscripts) = comp.parts.last().map(|part| part.subs.as_slice()) else {
        return FunctionLoopLowering::Fold { targets };
    };
    let exact_unit_domain = dimensions.as_ref().is_ok_and(|dimensions| {
        dimensions == &extents
            && domain
                .binders
                .iter()
                .zip(dimensions)
                .all(|(binder, dimension)| {
                    binder.lower == 1
                        && binder.step == 1
                        && usize::try_from(binder.upper).ok() == Some(*dimension)
                })
    });
    let exact_subscripts = *subscript_count == indices.len()
        && subscripts.len() == indices.len()
        && subscripts
            .iter()
            .zip(indices)
            .all(|(subscript, index)| subscript_is_binder(subscript, &index.ident));
    let mut references = Vec::new();
    value.collect_var_refs(&mut references);
    if exact_unit_domain
        && exact_subscripts
        && !references.iter().any(|reference| reference == target)
    {
        FunctionLoopLowering::TotalArrayDefinition {
            target: target.clone(),
        }
    } else {
        FunctionLoopLowering::Fold { targets }
    }
}

fn subscript_is_binder(subscript: &rumoca_core::Subscript, binder: &str) -> bool {
    matches!(
        subscript,
        rumoca_core::Subscript::Expr {
            expr,
            ..
        } if matches!(
            expr.as_ref(),
            Expression::VarRef {
                name,
                subscripts,
                ..
            } if name.as_str() == binder && subscripts.is_empty()
        )
    )
}

fn flattened_function_loop_source<'statement>(
    indices: &'statement [rumoca_core::ForIndex],
    equations: &'statement [rumoca_core::Statement],
    source_depth: usize,
) -> (
    Vec<&'statement rumoca_core::ForIndex>,
    &'statement [rumoca_core::Statement],
) {
    let mut flattened = indices.iter().collect::<Vec<_>>();
    let mut statements = equations;
    for _ in 1..source_depth {
        let [
            rumoca_core::Statement::For {
                indices, equations, ..
            },
        ] = statements
        else {
            unreachable!("function analysis accepts only perfect nested loops")
        };
        flattened.extend(indices);
        statements = equations;
    }
    (flattened, statements)
}

fn validate_function_assignment_target(
    function: &rumoca_core::Function,
    component: &rumoca_core::ComponentReference,
    span: Span,
) -> Result<(VarName, usize), ToDaeError> {
    let Some(target) = component.parts.last() else {
        return Err(ToDaeError::unsupported_flat(
            "function assignment target",
            "empty function result reference",
            span,
        ));
    };
    if !function
        .outputs
        .iter()
        .chain(&function.locals)
        .any(|value| value.name == target.ident)
    {
        return Err(ToDaeError::unsupported_flat(
            "function assignment target",
            format!(
                "`{}.{}` is not a whole mutable function value",
                function.name, target.ident
            ),
            span,
        ));
    }
    Ok((VarName::new(&target.ident), target.subs.len()))
}

fn validate_function_assignment_subscripts(
    component: &rumoca_core::ComponentReference,
    roles: &HashMap<VarName, PlannedRole>,
    flat: &flat::Model,
) -> Result<(), ToDaeError> {
    let Some(target) = component.parts.last() else {
        return Ok(());
    };
    for subscript in &target.subs {
        if let rumoca_core::Subscript::Expr { expr, .. } = subscript {
            validate_function_expression_with_roles(expr, roles, flat)?;
        }
    }
    Ok(())
}

fn function_loop_targets(plans: &[FunctionStatementPlan]) -> Vec<VarName> {
    let mut targets = Vec::new();
    for plan in plans {
        let FunctionStatementPlan::Assignment { target, .. } = plan else {
            continue;
        };
        if !targets.contains(target) {
            targets.push(target.clone());
        }
    }
    targets
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
        } else if name.as_str() == "Clock" {
            if args.len() != 1 {
                return Err(ToDaeError::unsupported_runtime_operator(
                    "Clock",
                    "the canonical clock proof currently requires Clock(interval)",
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

fn validate_variable(
    flat: &flat::Model,
    name: &VarName,
    variable: &flat::Variable,
    states: &HashSet<VarName>,
    assigned_discrete: &HashSet<VarName>,
) -> Result<PlannedRole, ToDaeError> {
    let external_input = is_external_input(flat, name, variable)?;
    if flat
        .variable_type_names
        .get(name)
        .is_some_and(|ty| ty == "Clock")
    {
        require_span(variable.source_span, format!("clock declaration `{name}`"))?;
        if !variable.dims.is_empty() {
            return Err(ToDaeError::unsupported_flat(
                "clock ownership proof",
                format!("clock coordinate `{name}` must be scalar"),
                variable.source_span,
            ));
        }
        return Ok(PlannedRole::Clock);
    }
    let scalar_type = validate_variable_header(flat, name, variable, external_input)?;
    let role = classify_variable_role(
        name,
        variable,
        states,
        assigned_discrete,
        scalar_type,
        external_input,
    );
    validate_variable_role(name, variable, role, scalar_type)?;
    Ok(role)
}

fn validate_variable_header(
    flat: &flat::Model,
    name: &VarName,
    variable: &flat::Variable,
    external_input: bool,
) -> Result<dae::ScalarType, ToDaeError> {
    require_span(
        variable.source_span,
        format!("variable declaration `{name}`"),
    )?;
    let Some(type_name) = flat.variable_type_names.get(name) else {
        return Err(ToDaeError::unsupported_flat(
            "effective variable type",
            format!("`{name}` has no resolved Flat type name"),
            variable.source_span,
        ));
    };
    let scalar_type = effective_variable_scalar_type(type_name, variable).ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "non-primitive coordinate",
            format!("`{name}` has unsupported effective type `{type_name}`"),
            variable.source_span,
        )
    })?;
    if !variable.is_primitive {
        return Err(ToDaeError::unsupported_flat(
            "non-primitive coordinate",
            format!("`{name}` must be expanded or enter a typed aggregate arena"),
            variable.source_span,
        ));
    }
    if variable.type_id.is_unknown() {
        return Err(ToDaeError::unsupported_flat(
            "effective variable type",
            format!("`{name}` retains TypeId::UNKNOWN"),
            variable.source_span,
        ));
    }
    for &extent in &variable.dims {
        if u32::try_from(extent).is_err() {
            return Err(ToDaeError::unsupported_flat(
                "array extent",
                format!("`{name}` has an extent outside the DAE u32 domain"),
                variable.source_span,
            ));
        }
    }
    if external_input && variable.binding.is_some() {
        return Err(ToDaeError::unsupported_flat(
            "input binding",
            "an externally supplied input cannot silently become a local equation",
            variable.source_span,
        ));
    }
    Ok(scalar_type)
}

fn classify_variable_role(
    name: &VarName,
    variable: &flat::Variable,
    states: &HashSet<VarName>,
    assigned_discrete: &HashSet<VarName>,
    scalar_type: dae::ScalarType,
    external_input: bool,
) -> PlannedRole {
    if external_input {
        PlannedRole::Input
    } else if matches!(variable.variability, Variability::Constant(_)) {
        PlannedRole::Constant
    } else if matches!(variable.variability, Variability::Parameter(_)) {
        PlannedRole::Parameter
    } else if states.contains(name) {
        PlannedRole::State
    } else if assigned_discrete.contains(name)
        || matches!(variable.variability, Variability::Discrete(_))
        || variable.is_discrete_type
    {
        if scalar_type == dae::ScalarType::Real {
            PlannedRole::DiscreteReal
        } else {
            PlannedRole::DiscreteValue
        }
    } else if matches!(variable.causality, Causality::Output(_)) {
        PlannedRole::Output
    } else {
        PlannedRole::Algebraic
    }
}

fn is_external_input(
    flat: &flat::Model,
    name: &VarName,
    variable: &flat::Variable,
) -> Result<bool, ToDaeError> {
    if !matches!(variable.causality, Causality::Input(_)) {
        return Ok(false);
    }
    let root = variable
        .component_ref
        .as_ref()
        .and_then(|reference| reference.parts.first())
        .map(|part| part.ident.as_str())
        .ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "input ownership",
                format!("input `{name}` has no resolved component-reference root"),
                variable.source_span,
            )
        })?;
    Ok(flat.top_level_input_components.contains(root))
}

fn validate_variable_role(
    name: &VarName,
    variable: &flat::Variable,
    role: PlannedRole,
    scalar_type: dae::ScalarType,
) -> Result<(), ToDaeError> {
    if matches!(
        role,
        PlannedRole::State | PlannedRole::Algebraic | PlannedRole::Output
    ) && scalar_type != dae::ScalarType::Real
    {
        return Err(ToDaeError::unsupported_flat(
            "continuous non-Real coordinate",
            format!("`{name}` must be classified as a discrete value"),
            variable.source_span,
        ));
    }
    if matches!(role, PlannedRole::State)
        && !matches!(
            variable.variability,
            Variability::Empty | Variability::Continuous(_)
        )
    {
        return Err(ToDaeError::unsupported_flat(
            "derivative target",
            format!("`{name}` is not a continuous Real coordinate"),
            variable.source_span,
        ));
    }
    if matches!(role, PlannedRole::DiscreteReal)
        && !variable.dims.is_empty()
        && variable.binding.is_some()
    {
        return Err(ToDaeError::unsupported_flat(
            "array discrete-Real definition",
            "B.1b structured families are not yet part of the canonical DAE grammar",
            variable.source_span,
        ));
    }
    Ok(())
}

pub(super) fn equation_partition<'flat>(
    equation: &'flat flat::Equation,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<EquationPartition<'flat>, ToDaeError> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &equation.residual
    else {
        return Ok(EquationPartition::Continuous);
    };
    let targets = assignment_target_names(lhs);
    let discrete_targets = targets
        .iter()
        .filter(|target| {
            matches!(
                roles.get(*target),
                Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
            )
        })
        .collect::<Vec<_>>();
    if discrete_targets.is_empty() {
        return Ok(EquationPartition::Continuous);
    }
    if discrete_targets.len() != targets.len() || targets.len() != 1 {
        return Err(ToDaeError::unsupported_flat(
            "mixed discrete equation",
            "one equation owner cannot mix continuous and discrete targets or define multiple discrete coordinates",
            equation.span,
        ));
    }
    let target = discrete_targets[0];
    match roles[target] {
        PlannedRole::DiscreteReal => Ok(EquationPartition::DiscreteReal { target }),
        PlannedRole::DiscreteValue => {
            let Expression::VarRef {
                name, subscripts, ..
            } = lhs.as_ref()
            else {
                return Err(ToDaeError::unsupported_flat(
                    "discrete-value equation",
                    "a discrete-value equation must have one resolved variable as its left-hand side",
                    equation.span,
                ));
            };
            if !subscripts.is_empty() {
                return Err(ToDaeError::unsupported_flat(
                    "indexed discrete-value equation",
                    "indexed discrete-value updates require an owned array-update operation",
                    equation.span,
                ));
            }
            Ok(EquationPartition::DiscreteValue {
                target: name.var_name(),
                value: rhs,
            })
        }
        PlannedRole::Parameter
        | PlannedRole::Constant
        | PlannedRole::Input
        | PlannedRole::State
        | PlannedRole::Algebraic
        | PlannedRole::Output
        | PlannedRole::Clock
        | PlannedRole::EnumerationLiteral
        | PlannedRole::Aggregate => {
            unreachable!("the target was selected as a discrete coordinate")
        }
    }
}

fn assignment_target_names(expression: &Expression) -> Vec<&VarName> {
    let mut targets = Vec::new();
    collect_assignment_target_names(expression, &mut targets);
    targets
}

fn collect_assignment_target_names<'flat>(
    expression: &'flat Expression,
    targets: &mut Vec<&'flat VarName>,
) {
    match expression {
        Expression::VarRef { name, .. } => targets.push(name.var_name()),
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            ..
        } if args.len() == 1 => collect_assignment_target_names(&args[0], targets),
        Expression::Unary { rhs, .. } | Expression::Index { base: rhs, .. } => {
            collect_assignment_target_names(rhs, targets);
        }
        Expression::Tuple { elements, .. } | Expression::Array { elements, .. } => {
            for element in elements {
                collect_assignment_target_names(element, targets);
            }
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (_, value) in branches {
                collect_assignment_target_names(value, targets);
            }
            collect_assignment_target_names(else_branch, targets);
        }
        Expression::Literal { .. }
        | Expression::Binary { .. }
        | Expression::BuiltinCall { .. }
        | Expression::FunctionCall { .. }
        | Expression::ArrayComprehension { .. }
        | Expression::Range { .. }
        | Expression::FieldAccess { .. }
        | Expression::Empty { .. } => {}
    }
}

pub(super) fn defined_discrete_targets(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<HashSet<VarName>, ToDaeError> {
    let mut targets = event_targets(flat);
    targets.extend(algorithm_targets(flat).into_iter().filter(|target| {
        matches!(
            roles.get(target),
            Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
        )
    }));
    for equation in &flat.equations {
        match equation_partition(equation, roles)? {
            EquationPartition::Continuous => {}
            EquationPartition::DiscreteReal { target }
            | EquationPartition::DiscreteValue { target, .. } => {
                targets.insert(target.clone());
            }
        }
    }
    Ok(targets)
}

fn structured_assignment_pairs(
    target: &VarName,
    value: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
) -> Option<Vec<(VarName, VarName)>> {
    let pairs = structured_assignment_names(target, value, roles.keys())?;
    pairs
        .iter()
        .all(|(target_leaf, _)| {
            matches!(
                roles.get(target_leaf),
                Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
            )
        })
        .then_some(pairs)
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

fn checked_shape_size(name: &VarName, variable: &flat::Variable) -> Result<usize, ToDaeError> {
    variable.shape_size().map_err(|_| {
        ToDaeError::unsupported_flat(
            "variable shape",
            format!("`{name}` has a shape whose scalar cardinality cannot be represented"),
            variable.source_span,
        )
    })
}

pub(super) fn primitive_scalar_type(type_name: &str) -> Option<dae::ScalarType> {
    match type_name {
        "Real" => Some(dae::ScalarType::Real),
        "Integer" => Some(dae::ScalarType::Integer),
        "Boolean" => Some(dae::ScalarType::Boolean),
        "String" => Some(dae::ScalarType::String),
        _ => None,
    }
}

pub(super) fn effective_variable_scalar_type(
    type_name: &str,
    variable: &flat::Variable,
) -> Option<dae::ScalarType> {
    primitive_scalar_type(type_name).or_else(|| {
        variable
            .is_discrete_type
            .then_some(dae::ScalarType::Integer)
    })
}

fn collect_derivative_targets(
    expression: &Expression,
    states: &mut HashSet<VarName>,
) -> Result<(), ToDaeError> {
    if let Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args,
        span,
    } = expression
    {
        require_span(*span, "derivative expression")?;
        let [argument] = args.as_slice() else {
            return Err(ToDaeError::unsupported_flat(
                "derivative expression",
                "der(...) must have exactly one resolved variable-reference operand",
                *span,
            ));
        };
        let Some((name, _)) = derivative_reference(argument) else {
            return Err(ToDaeError::unsupported_flat(
                "derivative expression",
                "der(...) must have exactly one resolved variable-reference operand",
                *span,
            ));
        };
        states.insert(name.var_name().clone());
    }
    for child in expression_children(expression) {
        collect_derivative_targets(child, states)?;
    }
    Ok(())
}

fn validate_expression(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
) -> Result<(), ToDaeError> {
    validate_expression_scoped(expression, roles, states, &HashSet::new())
}

fn validate_expression_scoped(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    binders: &HashSet<VarName>,
) -> Result<(), ToDaeError> {
    let span = expression_span(expression)?;
    match expression {
        Expression::Binary { op, lhs, rhs, .. } => {
            validate_binary_operator(op, span)?;
            validate_expression_scoped(lhs, roles, states, binders)?;
            validate_expression_scoped(rhs, roles, states, binders)
        }
        Expression::Unary { op, rhs, .. } => {
            validate_unary_operator(op, span)?;
            validate_expression_scoped(rhs, roles, states, binders)
        }
        Expression::VarRef {
            name, subscripts, ..
        } => {
            if name.as_str() != "time"
                && !roles.contains_key(name.var_name())
                && !binders.contains(name.var_name())
            {
                return Err(ToDaeError::unresolved_reference(name.as_str(), span));
            }
            if binders.contains(name.var_name()) && !subscripts.is_empty() {
                return Err(ToDaeError::unsupported_flat(
                    "structured-domain binder",
                    "a domain binder is a scalar Integer coordinate and cannot be subscripted",
                    span,
                ));
            }
            validate_subscripts_scoped(subscripts, roles, states, binders)
        }
        Expression::BuiltinCall { function, args, .. } => {
            validate_builtin(*function, args, roles, states, binders, span)
        }
        Expression::Literal { .. } => Ok(()),
        Expression::If {
            branches,
            else_branch,
            ..
        } => validate_conditional(branches, else_branch, roles, states, binders, span),
        Expression::FunctionCall { args, .. } => {
            for argument in args {
                if matches!(
                    argument,
                    Expression::Array {
                        elements,
                        ..
                    } if elements.is_empty()
                ) {
                    require_span(expression_span(argument)?, "empty function argument")?;
                    continue;
                }
                validate_expression_scoped(argument, roles, states, binders)?;
            }
            Ok(())
        }
        Expression::Array { elements, .. } => {
            validate_array(elements, roles, states, binders, span)
        }
        Expression::Range {
            start, step, end, ..
        } => {
            require_integer_literal(start, "range start")?;
            if let Some(step) = step {
                require_integer_literal(step, "range step")?;
            }
            require_integer_literal(end, "range end")?;
            Ok(())
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            validate_expression_scoped(base, roles, states, binders)?;
            validate_subscripts_scoped(subscripts, roles, states, binders)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => validate_array_comprehension(
            expr,
            indices,
            filter.as_deref(),
            roles,
            states,
            binders,
            span,
        ),
        Expression::Tuple { .. } | Expression::FieldAccess { .. } => {
            Err(ToDaeError::unsupported_flat(
                "aggregate expression",
                "tuple and record-field lowering require their typed semantic owner",
                span,
            ))
        }
        Expression::Empty { .. } => Err(ToDaeError::unsupported_flat(
            "empty expression",
            "an absent semantic value cannot enter canonical DAE",
            span,
        )),
    }
}
