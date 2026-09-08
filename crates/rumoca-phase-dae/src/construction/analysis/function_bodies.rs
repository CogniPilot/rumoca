mod guarded_loop_analysis;

use super::*;
use crate::construction::function_shapes::ProvenValue;
use guarded_loop_analysis::{seed_guarded_sequence_scratch, statement_reads_target};

pub(super) fn validate_functions(
    flat: &flat::Model,
    shapes: &FunctionShapeAnalysis,
) -> Result<HashMap<FunctionSpecializationKey, FunctionPlan>, ToDaeError> {
    let mut plans = HashMap::with_capacity(shapes.certificates().len());
    for certificate in shapes.certificates() {
        let plan = validate_function_certificate(flat, shapes, certificate)?;
        plans.insert(certificate.key.clone(), plan);
    }
    Ok(plans)
}

fn validate_function_certificate(
    flat: &flat::Model,
    shapes: &FunctionShapeAnalysis,
    certificate: &FunctionShapeCertificate,
) -> Result<FunctionPlan, ToDaeError> {
    let function = &flat.functions[&certificate.key.function];
    validate_function_declaration(function, flat, &certificate.values)?;
    let static_integers = immutable_integer_defaults(function, flat, &certificate.values)?;
    let roles = function_expression_roles(function, flat);
    let staged_record_fields = HashSet::new();
    let context = FunctionValidationContext {
        function,
        flat,
        roles: &roles,
        static_integers: &static_integers,
        shapes: &certificate.values,
        shape_analysis: shapes,
        generated_booleans: &[],
        staged_record_fields: &staged_record_fields,
        call_scoped_actions: true,
    };
    if function.external.is_some() {
        return validate_external_body(function, context);
    }
    if let Some(plan) = validate_guarded_function_return(function, context)? {
        return Ok(plan);
    }
    if let Some(plan) = validate_integer_reduction(function, context)? {
        return Ok(plan);
    }
    validate_statement_function(function, context)
}

fn validate_function_declaration(
    function: &rumoca_core::Function,
    flat: &flat::Model,
    values: &ShapeEnvironment,
) -> Result<(), ToDaeError> {
    require_span(function.span, "function declaration")?;
    if function.is_constructor {
        return Err(ToDaeError::unsupported_flat(
            "function lifecycle",
            format!("`{}` is not a pure Modelica function body", function.name),
            function.span,
        ));
    }
    if function.external.is_none() && !function.pure {
        return Err(ToDaeError::unsupported_flat(
            "function lifecycle",
            format!(
                "`{}` declares an impure Modelica body, which MLS §12.3 does not permit",
                function.name
            ),
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
            validate_function_expression(default, function, flat, values)?;
        }
    }
    Ok(())
}

fn validate_external_body(
    function: &rumoca_core::Function,
    context: FunctionValidationContext<'_>,
) -> Result<FunctionPlan, ToDaeError> {
    if !function.body.is_empty() {
        return Err(ToDaeError::unsupported_flat(
            "function lifecycle",
            format!(
                "`{}` declares both an algorithm body and an external interface",
                function.name
            ),
            function.span,
        ));
    }
    Ok(FunctionPlan::External(validate_external_function(
        function, context,
    )?))
}

fn validate_statement_function(
    function: &rumoca_core::Function,
    context: FunctionValidationContext<'_>,
) -> Result<FunctionPlan, ToDaeError> {
    // Normalize only semantics-preserving compact loop rewrites. A dependent
    // domain is refused rather than expanded into scalar statements.
    let returned = normalize_function_returns(&function.body)?;
    let returned_roles = roles_with_guards(context.roles, &returned.guards);
    let returned_shapes = shapes_with_guards(context.shapes, &returned.guards);
    let returned_context = FunctionValidationContext {
        roles: &returned_roles,
        shapes: &returned_shapes,
        generated_booleans: &returned.guards,
        ..context
    };
    check_function_assignment_shapes(&returned.statements, returned_context)?;
    if returned.has_returns {
        validate_nonreturn_path(function, context)?;
    }
    let source = compact_function_loops(
        &returned.statements,
        returned_context.static_integers,
        returned_context.shapes,
        function,
        context.flat,
        returned.has_returns,
    )?;
    let mut definitions = FunctionDefinitions::new(function);
    let certified_output_seeds = certified_return_output_seeds(
        function,
        returned.has_returns,
        returned_context,
        &mut definitions,
    )?;
    let plans = validate_function_statements(&source, returned_context, &mut definitions)?;
    require_total_outputs(function, &definitions)?;
    let statements = issue_function_statement_sequence(source, plans, function.span)?;
    Ok(FunctionPlan::Statements {
        statements,
        generated_booleans: returned
            .guards
            .iter()
            .map(|guard| (guard.target.clone(), guard.span))
            .collect(),
        certified_output_seeds,
    })
}

fn validate_nonreturn_path(
    function: &rumoca_core::Function,
    context: FunctionValidationContext<'_>,
) -> Result<(), ToDaeError> {
    certify_nonleading_return_branches(function, context)?;
    let nonreturn = normalize_function_returns(&nonreturn_path(&function.body))?;
    let roles = roles_with_guards(context.roles, &nonreturn.guards);
    let shapes = shapes_with_guards(context.shapes, &nonreturn.guards);
    let nonreturn_context = FunctionValidationContext {
        roles: &roles,
        shapes: &shapes,
        generated_booleans: &nonreturn.guards,
        ..context
    };
    let source = compact_function_loops(
        &nonreturn.statements,
        context.static_integers,
        &shapes,
        function,
        context.flat,
        false,
    )?;
    let mut definitions = FunctionDefinitions::new(function);
    validate_function_statements(&source, nonreturn_context, &mut definitions)?;
    require_total_outputs(function, &definitions)
}

fn roles_with_guards(
    roles: &HashMap<VarName, PlannedRole>,
    guards: &[function_returns::GeneratedBooleanDefinition],
) -> HashMap<VarName, PlannedRole> {
    let mut guarded = roles.clone();
    guarded.extend(
        guards
            .iter()
            .map(|guard| (guard.target.clone(), PlannedRole::Parameter)),
    );
    guarded
}

fn shapes_with_guards(
    shapes: &ShapeEnvironment,
    guards: &[function_returns::GeneratedBooleanDefinition],
) -> ShapeEnvironment {
    let mut guarded = shapes.clone();
    for guard in guards {
        guarded.insert(guard.target.clone(), Vec::new());
    }
    guarded
}

fn certified_return_output_seeds(
    function: &rumoca_core::Function,
    has_returns: bool,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<Vec<(VarName, FunctionValueSeed)>, ToDaeError> {
    if !has_returns {
        return Ok(Vec::new());
    }
    let mut seeds = Vec::with_capacity(function.outputs.len());
    for output in &function.outputs {
        let target = VarName::new(&output.name);
        // MLS §12.4.4 gives a declaration binding the status of an initial
        // value the algorithm may keep. `FunctionDefinitions::new` already
        // certified exactly those outputs, so seeding one here would emit a
        // dead-value store *over* the declared default at function entry —
        // observable on every path that keeps the default, and silent.
        if definitions.is_defined(&target) {
            continue;
        }
        let seed = definitions.whole_loop_seed(&target, context, output.span)?;
        seeds.push((target, seed));
    }
    definitions.assume_certified_outputs(function);
    Ok(seeds)
}

fn validate_function_expression(
    expression: &Expression,
    function: &rumoca_core::Function,
    flat: &flat::Model,
    values: &ShapeEnvironment,
) -> Result<(), ToDaeError> {
    let roles = function_expression_roles(function, flat);
    validate_function_expression_with_roles(expression, &roles, flat, values)
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
    // MLS §12.2: a record-typed formal, result, or local also names each of its
    // declared fields, which Flat renders as one joined reference identity.
    for value in function
        .inputs
        .iter()
        .chain(&function.outputs)
        .chain(&function.locals)
    {
        roles.extend(
            record_field_projections(value, flat)
                .into_iter()
                .map(|(path, _, _)| (path, PlannedRole::Parameter)),
        );
    }
    for literal in flat.enum_literal_ordinals.keys() {
        roles.insert(VarName::new(literal), PlannedRole::EnumerationLiteral);
    }
    roles
}

/// Validate one expression of a value-proven function specialization.
///
/// `values` is the specialization's proven environment: MLS §12.2 lets a
/// function body be written over its inputs, so a construct that must be static
/// — the compact range of MLS §10.4.1 — is static here exactly when this
/// specialization settles its operands.
pub(super) fn validate_function_expression_with_roles(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    flat: &flat::Model,
    values: &ShapeEnvironment,
) -> Result<(), ToDaeError> {
    validate_specialized_expression(expression, roles, values)?;
    validate_known_function_calls(expression, flat)
}

/// Prove every function output owns a definition of every declared element.
///
/// MLS §12.4.4 leaves an unwritten function value undefined, so a body that
/// returns one has no checked DAE denotation. The definedness certificate also
/// carries the totality proof that keeps a generated aggregate seed dead.
fn require_total_outputs(
    function: &rumoca_core::Function,
    definitions: &FunctionDefinitions,
) -> Result<(), ToDaeError> {
    for output in &function.outputs {
        let name = VarName::new(&output.name);
        if !definitions.is_total(&name) {
            return Err(ToDaeError::unsupported_flat(
                "function output definition",
                format!(
                    "`{}` returns `{name}` without defining every declared element",
                    function.name
                ),
                output.span,
            ));
        }
    }
    Ok(())
}

/// Plan one statement sequence and prove its definedness certificate.
pub(super) fn validate_function_statements(
    statements: &[rumoca_core::Statement],
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<Vec<FunctionStatementPlan>, ToDaeError> {
    let mut plans = plan_function_statements(statements, context)?;
    definitions.track_record_staging(&plans);
    annotate_iteration_locals(statements, &mut plans, context, &[], &[])?;
    resolve_function_definitions(statements, &mut plans, context, definitions)?;
    Ok(plans)
}

/// Separate nonescaping per-iteration scratch from the fold's carried tuple.
///
/// This is deliberately conservative. A candidate must be a declared local,
/// receive a dominating whole definition on every iteration before any read,
/// and have no read after the loop or across an enclosing back edge. Anything
/// not proved here remains carried and keeps the older, more general semantics.
fn annotate_iteration_locals<'a>(
    statements: &'a [rumoca_core::Statement],
    plans: &mut [FunctionStatementPlan],
    context: FunctionValidationContext<'_>,
    enclosing_suffix: &[&'a [rumoca_core::Statement]],
    back_edges: &[&'a [rumoca_core::Statement]],
) -> Result<(), ToDaeError> {
    if statements.len() != plans.len() {
        return Err(function_statement_product_error(
            "iteration-local analysis received a mismatched source sequence",
            context.function.span,
        ));
    }
    let locals = context
        .function
        .locals
        .iter()
        .map(|local| VarName::new(&local.name))
        .collect::<HashSet<_>>();
    for (index, plan) in plans.iter_mut().enumerate() {
        let (source_prefix, source_suffix) = statements.split_at(index + 1);
        let Some(statement) = source_prefix.last() else {
            return Err(function_statement_product_error(
                "iteration-local analysis lost its exact source statement",
                context.function.span,
            ));
        };
        let mut suffix = Vec::with_capacity(enclosing_suffix.len() + 1);
        suffix.push(source_suffix);
        suffix.extend_from_slice(enclosing_suffix);
        match (statement, plan) {
            (
                rumoca_core::Statement::For {
                    indices, equations, ..
                },
                FunctionStatementPlan::For {
                    lowering,
                    statements: body_plans,
                    source_depth,
                    ..
                },
            ) => {
                let (_, body) = flattened_function_loop_source(indices, equations, *source_depth);
                classify_iteration_local_targets(
                    lowering, body, body_plans, &locals, &suffix, back_edges,
                );
                let mut nested_back_edges = Vec::with_capacity(back_edges.len() + 1);
                nested_back_edges.push(body);
                nested_back_edges.extend_from_slice(back_edges);
                annotate_iteration_locals(body, body_plans, context, &suffix, &nested_back_edges)?;
            }
            (
                rumoca_core::Statement::If {
                    cond_blocks,
                    else_block,
                    ..
                },
                FunctionStatementPlan::If {
                    branches, fallback, ..
                },
            ) => annotate_conditional_iteration_locals(
                cond_blocks,
                else_block.as_deref(),
                branches,
                fallback.as_mut(),
                context,
                &suffix,
                back_edges,
            )?,
            _ => {}
        }
    }
    Ok(())
}

fn annotate_conditional_iteration_locals<'a>(
    source_blocks: &'a [rumoca_core::StatementBlock],
    source_fallback: Option<&'a [rumoca_core::Statement]>,
    branches: &mut [Vec<FunctionStatementPlan>],
    fallback: Option<&mut Vec<FunctionStatementPlan>>,
    context: FunctionValidationContext<'_>,
    suffix: &[&'a [rumoca_core::Statement]],
    back_edges: &[&'a [rumoca_core::Statement]],
) -> Result<(), ToDaeError> {
    if source_blocks.len() != branches.len() || source_fallback.is_some() != fallback.is_some() {
        return Err(function_statement_product_error(
            "conditional iteration analysis received mismatched branches",
            context.function.span,
        ));
    }
    for (source, branch) in source_blocks.iter().zip(branches) {
        annotate_iteration_locals(&source.stmts, branch, context, suffix, back_edges)?;
    }
    if let (Some(source), Some(branch)) = (source_fallback, fallback) {
        annotate_iteration_locals(source, branch, context, suffix, back_edges)?;
    }
    Ok(())
}

fn classify_iteration_local_targets(
    lowering: &mut FunctionLoopLowering,
    body: &[rumoca_core::Statement],
    body_plans: &[FunctionStatementPlan],
    locals: &HashSet<VarName>,
    suffix: &[&[rumoca_core::Statement]],
    back_edges: &[&[rumoca_core::Statement]],
) {
    let FunctionLoopLowering::Fold {
        targets,
        iteration_locals,
    } = lowering
    else {
        return;
    };
    let mut retained = Vec::with_capacity(targets.len());
    for target in std::mem::take(targets) {
        let local = locals.contains(&target)
            && loop_has_dominating_whole_definition(body, body_plans, &target)
            && !loop_compaction::statements_read_incoming_name(body, &target)
            && !loop_compaction::statements_partially_assign_name(body, &target)
            && !suffix
                .iter()
                .any(|segment| loop_compaction::statements_read_name(segment, &target))
            && !loop_compaction::statement_segments_read_incoming_name(back_edges, &target);
        if local {
            iteration_locals.push(target);
        } else {
            retained.push(target);
        }
    }
    *targets = retained;
}

/// Prove that one iteration defines `target` unconditionally before any
/// control-flow-owned definition or read can reach it.
///
/// This intentionally recognizes only a direct whole assignment in the loop's
/// statement sequence.  A definition nested under a rectangularization guard
/// does not dominate the inactive domain points, even when all of its active
/// uses are locally dominated; such a value still needs the fold's seeded
/// carried slot rather than an iteration-local slot with no entry value.
fn loop_has_dominating_whole_definition(
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
    target: &VarName,
) -> bool {
    let mut statements = statements.iter();
    let mut plans = plans.iter();
    loop {
        let (statement, plan) = match (statements.next(), plans.next()) {
            (Some(statement), Some(plan)) => (statement, plan),
            (None, None) => return false,
            _ => return false,
        };
        let defines_whole = match plan {
            FunctionStatementPlan::Assignment(assignment) => {
                assignment.target() == target && assignment.is_whole()
            }
            FunctionStatementPlan::RecordAssembly(assembly) => &assembly.target == target,
            FunctionStatementPlan::MultiOutputCall { outputs } => outputs
                .iter()
                .flatten()
                .any(|output| output.target() == target && output.is_whole()),
            _ => false,
        };
        if defines_whole {
            return true;
        }
        if statement_reads_target(statement, target) || statement_assigns_target(statement, target)
        {
            return false;
        }
    }
}

fn statement_assigns_target(statement: &rumoca_core::Statement, target: &VarName) -> bool {
    match statement {
        rumoca_core::Statement::Assignment { comp, .. } => &comp.to_var_name() == target,
        rumoca_core::Statement::FunctionCall { outputs, .. } => outputs
            .iter()
            .flatten()
            .any(|output| &output.to_var_name() == target),
        rumoca_core::Statement::For { equations, .. } => equations
            .iter()
            .any(|statement| statement_assigns_target(statement, target)),
        rumoca_core::Statement::While { block, .. } => block
            .stmts
            .iter()
            .any(|statement| statement_assigns_target(statement, target)),
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks.iter().any(|block| {
                block
                    .stmts
                    .iter()
                    .any(|statement| statement_assigns_target(statement, target))
            }) || else_block.as_ref().is_some_and(|statements| {
                statements
                    .iter()
                    .any(|statement| statement_assigns_target(statement, target))
            })
        }
        rumoca_core::Statement::When { blocks, .. } => blocks.iter().any(|block| {
            block
                .stmts
                .iter()
                .any(|statement| statement_assigns_target(statement, target))
        }),
        rumoca_core::Statement::Reinit { variable, .. } => &variable.to_var_name() == target,
        rumoca_core::Statement::Assert { .. }
        | rumoca_core::Statement::Empty { .. }
        | rumoca_core::Statement::Return { .. }
        | rumoca_core::Statement::Break { .. } => false,
    }
}

/// Prove the checked owner shape of one statement sequence.
///
/// Definedness is a separate pass because the array-assembly coalescing below
/// rewrites whole runs of element writes into one aggregate owner, and only the
/// coalesced plan says which value each statement actually defines.
pub(super) fn plan_function_statements(
    statements: &[rumoca_core::Statement],
    context: FunctionValidationContext<'_>,
) -> Result<Vec<FunctionStatementPlan>, ToDaeError> {
    let (mut staged_records, staged_members) = plan_staged_record_assemblies(statements, context)?;
    let mut staged_record_fields = context.staged_record_fields.clone();
    let mut plans = Vec::with_capacity(statements.len());
    let mut index = 0usize;
    while index < statements.len() {
        let Some(statement) = statements.get(index) else {
            return Err(function_statement_product_error(
                "function planning lost its exact source statement",
                context.function.span,
            ));
        };
        let statement_context = FunctionValidationContext {
            staged_record_fields: &staged_record_fields,
            ..context
        };
        if let rumoca_core::Statement::Empty { span } = statement
            && let Some(guard) = context
                .generated_booleans
                .iter()
                .find(|guard| guard.span == *span)
        {
            validate_function_expression_with_roles(
                &guard.value,
                statement_context.roles,
                statement_context.flat,
                statement_context.shapes,
            )?;
            plans.push(FunctionStatementPlan::GeneratedBooleanAssignment {
                target: guard.target.clone(),
                value: guard.value.clone(),
                span: guard.span,
            });
            index += 1;
            continue;
        }
        if let Some(assertion) = function_assertion(statement, statement_context.flat)? {
            plans.push(plan_proven_function_assertion(
                assertion,
                statement_context,
            )?);
            index += 1;
            continue;
        }
        if let Some(assembly) = staged_records.remove(&index) {
            let count = assembly.statement_count;
            let Some(member_count) = count.checked_sub(1) else {
                return Err(function_statement_product_error(
                    "staged record field assembly has an empty source run",
                    statement.source_span().unwrap_or(context.function.span),
                ));
            };
            let plan = FunctionStatementPlan::RecordFieldAssembly(assembly);
            advance_function_record_staging(&plan, &mut staged_record_fields);
            plans.push(plan);
            plans.extend(
                std::iter::repeat_with(|| FunctionStatementPlan::RecordFieldAssemblyMember)
                    .take(member_count),
            );
            index += count;
            continue;
        }
        if staged_members.contains(&index) {
            return Err(function_statement_product_error(
                "staged record member has no exact leading owner",
                statement.source_span().unwrap_or(context.function.span),
            ));
        }
        if let Some((assembly, count)) =
            validate_record_output_assembly(statements, index, statement_context)?
        {
            let plan = FunctionStatementPlan::RecordAssembly(assembly);
            advance_function_record_staging(&plan, &mut staged_record_fields);
            plans.push(plan);
            plans.extend(
                std::iter::repeat_with(|| FunctionStatementPlan::RecordAssemblyMember)
                    .take(count - 1),
            );
            index += count;
            continue;
        }
        let plan = plan_one_function_statement(statement, statement_context)?;
        advance_function_record_staging(&plan, &mut staged_record_fields);
        plans.push(plan);
        index += 1;
    }
    coalesce_function_array_assemblies(statements, &mut plans, context)?;
    Ok(plans)
}

fn plan_one_function_statement(
    statement: &rumoca_core::Statement,
    context: FunctionValidationContext<'_>,
) -> Result<FunctionStatementPlan, ToDaeError> {
    match statement {
        rumoca_core::Statement::Assignment { comp, value, span } => {
            require_span(*span, "function assignment")?;
            let assignment = validate_function_assignment_target(context, comp, *span)?;
            validate_function_expression_with_roles(
                value,
                context.roles,
                context.flat,
                context.shapes,
            )?;
            Ok(FunctionStatementPlan::Assignment(assignment))
        }
        rumoca_core::Statement::For { .. } => validate_function_loop(statement, context),
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            span,
        } => plan_function_conditional(cond_blocks, else_block.as_deref(), *span, context),
        rumoca_core::Statement::FunctionCall {
            comp,
            args,
            outputs,
            span,
        } => plan_function_multi_output_call(
            MultiOutputCallStatement {
                callee: comp,
                args,
                outputs,
                span: *span,
            },
            context,
        ),
        _ => {
            let span = required_statement_span(statement, "unsupported function body statement")?;
            Err(ToDaeError::unsupported_flat(
                "function statement",
                format!(
                    "`{}` contains a statement without a checked DAE owner",
                    context.function.name
                ),
                span,
            ))
        }
    }
}

#[derive(Clone, Copy)]
pub(in crate::construction) struct FunctionAssertion<'statement> {
    pub(in crate::construction) condition: &'statement Expression,
    pub(in crate::construction) message: &'statement Expression,
    pub(in crate::construction) level: Option<&'statement Expression>,
    pub(in crate::construction) span: Span,
}

/// Recognize MLS §8.3.7 `assert` in both statement shapes Flat retains.
///
/// Algorithm syntax reaches Flat as a zero-output call to the predefined
/// operator, while some producers use the dedicated statement. A declared
/// function named `assert` remains an ordinary user call: exact Flat function
/// identity wins over the predefined short name.
pub(in crate::construction) fn function_assertion<'statement>(
    statement: &'statement rumoca_core::Statement,
    flat: &flat::Model,
) -> Result<Option<FunctionAssertion<'statement>>, ToDaeError> {
    match statement {
        rumoca_core::Statement::Assert {
            condition,
            message,
            level,
            span,
        } => Ok(Some(FunctionAssertion {
            condition,
            message,
            level: level.as_deref(),
            span: *span,
        })),
        rumoca_core::Statement::FunctionCall {
            comp,
            args,
            outputs,
            span,
        } if outputs.iter().all(Option::is_none) => {
            let name = comp.as_str();
            if comp
                .resolved_function()
                .and_then(|resolved| flat.get_function_instance(resolved.instance_id))
                .is_some()
                || rumoca_core::runtime_flow_action_function_short_name(name) != Some("assert")
            {
                return Ok(None);
            }
            let (condition, message, level) = match args.as_slice() {
                [condition, message] => (condition, message, None),
                [condition, message, level] => (condition, message, Some(level)),
                _ => {
                    return Err(ToDaeError::unsupported_flat(
                        "function assertion",
                        "the predefined `assert` statement requires condition, message, and an optional level",
                        *span,
                    ));
                }
            };
            Ok(Some(FunctionAssertion {
                condition,
                message,
                level,
                span: *span,
            }))
        }
        _ => Ok(None),
    }
}

/// Prove that one function assertion has no executable failure path.
///
/// Function bodies lower to pure result DAGs, so an assertion that is not
/// settled needs a call-scoped flow-action owner rather than a value-expression
/// substitute. The specialization environment is exact for the Integer,
/// enumeration, and Boolean inputs carried by its key; only an exact `true`
/// proof permits semantic erasure.
fn plan_proven_function_assertion(
    assertion: FunctionAssertion<'_>,
    context: FunctionValidationContext<'_>,
) -> Result<FunctionStatementPlan, ToDaeError> {
    require_span(assertion.span, "function body assertion")?;
    validate_function_expression_with_roles(
        assertion.condition,
        context.roles,
        context.flat,
        context.shapes,
    )?;
    validate_function_expression_with_roles(
        assertion.message,
        context.roles,
        context.flat,
        context.shapes,
    )?;
    if let Some(level) = assertion.level {
        validate_function_expression_with_roles(
            level,
            context.roles,
            context.flat,
            context.shapes,
        )?;
    }
    if matches!(
        context.shapes.proven_value(assertion.condition),
        Some(ProvenValue::Settled(
            crate::construction::function_shapes::ProvenSettledValue::Boolean(true),
        ))
    ) {
        return Ok(FunctionStatementPlan::ProvenAssertion);
    }
    if assertion.level.is_some() {
        return Err(ToDaeError::unsupported_flat(
            "function assertion",
            format!(
                "`{}` contains a non-default assertion level without a checked severity owner",
                context.function.name
            ),
            assertion.span,
        ));
    }
    if !context.call_scoped_actions {
        return Err(ToDaeError::unsupported_flat(
            "function assertion",
            format!(
                "`{}` contains an assertion inside a runtime branch or loop without a nested flow-action owner",
                context.function.name
            ),
            assertion.span,
        ));
    }
    Ok(FunctionStatementPlan::RuntimeAssertion)
}

fn validate_function_assignment_target(
    context: FunctionValidationContext<'_>,
    component: &rumoca_core::ComponentReference,
    span: Span,
) -> Result<FunctionAssignmentPlan, ToDaeError> {
    let (target, target_def_id, record_field, record_root_name, record_field_name, subscripts) =
        match component.parts() {
            [target] => {
                let Some(value) = resolved_record_value(target, context.function)? else {
                    return Err(ToDaeError::unsupported_flat(
                        "function assignment target",
                        format!("`{}` is not an exact mutable function value", target.ident),
                        target.span,
                    ));
                };
                (
                    VarName::new(&value.name),
                    function_value_def_id(value, context.function)?,
                    None,
                    VarName::new(&value.name),
                    None,
                    target.subs.as_slice(),
                )
            }
            [root, field_part] if root.subs.is_empty() => {
                let Some(value) = resolved_record_value(root, context.function)? else {
                    return Err(ToDaeError::unsupported_flat(
                        "function assignment target",
                        format!("`{}` is not an exact mutable record value", root.ident),
                        root.span,
                    ));
                };
                if value.type_class != Some(rumoca_core::ClassType::Record) {
                    return Err(ToDaeError::unsupported_flat(
                        "function assignment target",
                        format!("`{}` is not a record value", value.name),
                        root.span,
                    ));
                }
                let target_def_id = function_value_def_id(value, context.function)?;
                let constructor = record_constructor(value, context)?;
                let fields = resolved_constructor_fields(&value.name, constructor)?;
                let field = require_constructor_field(&value.name, field_part, &fields)?;
                (
                    component.to_var_name(),
                    target_def_id,
                    Some(FunctionRecordFieldIdentity {
                        target: target_def_id,
                        field: field.def_id,
                    }),
                    VarName::new(&value.name),
                    Some(field.name.clone()),
                    field_part.subs.as_slice(),
                )
            }
            _ => {
                return Err(ToDaeError::unsupported_flat(
                    "function assignment target",
                    "a mutable function value must resolve to one value or one exact record field",
                    span,
                ));
            }
        };
    if context.shapes.get(&target).is_none() {
        return Err(ToDaeError::unsupported_flat(
            "function assignment target",
            format!(
                "`{}.{}` is not a whole mutable function value",
                context.function.name, target
            ),
            span,
        ));
    }
    validate_function_subscripts(subscripts, context)?;
    Ok(FunctionAssignmentPlan {
        target,
        target_def_id,
        record_field,
        record_root_name,
        record_field_name,
        subscripts: subscripts.to_vec().into_boxed_slice(),
        seed: None,
    })
}

pub(super) struct MultiOutputCallStatement<'statement> {
    pub(super) callee: &'statement rumoca_core::Reference,
    pub(super) args: &'statement [Expression],
    pub(super) outputs: &'statement [Option<rumoca_core::ComponentReference>],
    pub(super) span: Span,
}

/// Prove the exact left-to-right receivers of an MLS §11.2.1.1 multi-result call.
fn plan_function_multi_output_call(
    call: MultiOutputCallStatement<'_>,
    context: FunctionValidationContext<'_>,
) -> Result<FunctionStatementPlan, ToDaeError> {
    require_span(call.span, "function multi-result call statement")?;
    let Some(component) = call.callee.component_ref() else {
        return Err(ToDaeError::unsupported_flat(
            "function call statement",
            "a multi-result call statement requires one structured function reference",
            call.span,
        ));
    };
    if component.parts().iter().any(|part| !part.subs.is_empty()) {
        return Err(ToDaeError::unsupported_flat(
            "function call statement",
            "a multi-result call statement requires one resolved, unsubscripted function",
            call.span,
        ));
    }
    // The DAE has no effect-only owner for a call whose receivers are all holes.
    if call.outputs.iter().all(Option::is_none) {
        return Err(ToDaeError::unsupported_flat(
            "function call statement",
            format!(
                "`{}` calls `{}` as a statement without reading a result, which the canonical \
                 DAE has no owner for",
                context.function.name,
                call.callee.var_name()
            ),
            call.span,
        ));
    }
    let callee_name = call.callee.var_name();
    let Some(resolved) = call.callee.resolved_function() else {
        return Err(ToDaeError::unsupported_flat(
            "function call statement",
            format!("`{callee_name}` has no exact Flat function instance"),
            call.span,
        ));
    };
    let Some(callee) = context.flat.get_function_instance(resolved.instance_id) else {
        return Err(ToDaeError::unsupported_flat(
            "function call statement",
            format!("`{callee_name}` is not a declared function of the flat model"),
            call.span,
        ));
    };
    // MLS §12.6 record constructors are expression-only callables.
    if callee.is_constructor {
        return Err(ToDaeError::unsupported_flat(
            "function call statement",
            format!(
                "`{callee_name}` is a record constructor, which owns no multi-result call statement"
            ),
            call.span,
        ));
    }
    // The scalar-call DAE can preserve single-evaluation semantics only for pure callees.
    if !callee.body_is_pure() {
        return Err(ToDaeError::unsupported_flat(
            "function call statement",
            format!(
                "MLS §12.4.3 evaluates a multi-result call once, but the canonical DAE reads each \
                 result as its own call: `{callee_name}` is an impure external function, whose \
                 repeated invocation is not its single evaluation"
            ),
            call.span,
        ));
    }
    if call.outputs.len() > callee.outputs.len() {
        return Err(ToDaeError::unsupported_flat(
            "function call statement",
            format!(
                "MLS §11.2.1.1 admits at most one receiving variable per result: `{callee_name}` declares {} but the call site writes {}",
                callee.outputs.len(),
                call.outputs.len()
            ),
            call.span,
        ));
    }
    for argument in call.args {
        validate_function_expression_with_roles(
            argument,
            context.roles,
            context.flat,
            context.shapes,
        )?;
    }
    let key = context
        .shape_analysis
        .call_key(call.callee, call.args, context.shapes, call.span)?;
    let certificate = context.shape_analysis.certificate(&key).ok_or_else(|| {
        function_statement_product_error(
            "function call key has no exact specialization certificate",
            call.span,
        )
    })?;
    plan_multi_output_call_receivers(call, callee_name, certificate, context)
}

fn plan_multi_output_call_receivers(
    call: MultiOutputCallStatement<'_>,
    callee_name: &VarName,
    certificate: &FunctionShapeCertificate,
    context: FunctionValidationContext<'_>,
) -> Result<FunctionStatementPlan, ToDaeError> {
    if let Some(assembly) =
        plan_record_multi_output_assembly(call.outputs, certificate, call.span, context)?
    {
        return Ok(FunctionStatementPlan::RecordMultiOutputAssembly(assembly));
    }
    let mut outputs = Vec::with_capacity(call.outputs.len());
    for (ordinal, target) in call.outputs.iter().enumerate() {
        let Some(target) = target else {
            outputs.push(None);
            continue;
        };
        outputs.push(Some(plan_multi_output_receiver(
            target,
            ordinal,
            callee_name,
            certificate,
            call.span,
            context,
        )?));
    }
    Ok(FunctionStatementPlan::MultiOutputCall { outputs })
}

fn plan_record_multi_output_assembly(
    outputs: &[Option<rumoca_core::ComponentReference>],
    certificate: &FunctionShapeCertificate,
    span: Span,
    context: FunctionValidationContext<'_>,
) -> Result<Option<FunctionRecordCallAssemblyPlan>, ToDaeError> {
    let Some(Some(first)) = outputs.first() else {
        return Ok(None);
    };
    let [root, _] = first.parts() else {
        return Ok(None);
    };
    let Some(target) = resolved_record_value(root, context.function)? else {
        return Ok(None);
    };
    if target.type_class != Some(rumoca_core::ClassType::Record) {
        return Ok(None);
    }
    let target_def_id = function_value_def_id(target, context.function)?;
    let constructor = record_constructor(target, context)?;
    let resolved_fields = resolved_constructor_fields(&target.name, constructor)?;
    require_complete_record_call_receivers(outputs, &target.name, constructor.inputs.len(), span)?;

    let receivers = exact_record_call_receivers(
        outputs,
        target,
        target_def_id,
        &resolved_fields,
        context.function,
    )?;
    let fields = record_call_fields(
        target,
        constructor,
        &resolved_fields,
        &receivers,
        certificate,
        span,
    )?;
    Ok(Some(FunctionRecordCallAssemblyPlan {
        target: VarName::new(&target.name),
        target_def_id,
        fields,
    }))
}

pub(super) fn require_complete_record_call_receivers(
    outputs: &[Option<rumoca_core::ComponentReference>],
    target: &str,
    field_count: usize,
    span: Span,
) -> Result<(), ToDaeError> {
    if outputs.len() != field_count || outputs.iter().any(Option::is_none) {
        return Err(ToDaeError::unsupported_flat(
            "record output assembly",
            format!(
                "`{}` requires exactly one receiver for each of its {} constructor fields",
                target, field_count
            ),
            span,
        ));
    }
    Ok(())
}

pub(super) fn exact_record_call_receivers(
    outputs: &[Option<rumoca_core::ComponentReference>],
    target: &rumoca_core::FunctionParam,
    target_def_id: rumoca_core::DefId,
    resolved_fields: &[ResolvedFunctionRecordField],
    function: &rumoca_core::Function,
) -> Result<HashMap<rumoca_core::DefId, usize>, ToDaeError> {
    let mut receivers = HashMap::with_capacity(outputs.len());
    for (ordinal, receiver) in outputs.iter().enumerate() {
        let Some(receiver) = receiver.as_ref() else {
            return Err(function_statement_product_error(
                "complete record call receiver set contains an omitted field",
                function.span,
            ));
        };
        let [candidate_root, candidate_field] = receiver.parts() else {
            return Err(ToDaeError::unsupported_flat(
                "record output assembly",
                "a record call receiver must be one exact root and one exact field",
                receiver.span(),
            ));
        };
        let Some(candidate_target) = resolved_record_value(candidate_root, function)? else {
            return Err(ToDaeError::unsupported_flat(
                "record output assembly",
                format!(
                    "receiver root `{}` identity {} is not a mutable function value",
                    candidate_root.ident,
                    candidate_root.def_id.index()
                ),
                candidate_root.span,
            ));
        };
        if function_value_def_id(candidate_target, function)? != target_def_id
            || candidate_target.name != target.name
        {
            return Err(ToDaeError::unsupported_flat(
                "record output assembly",
                format!(
                    "receiver root `{}` does not match exact aggregate target `{}`",
                    candidate_target.name, target.name
                ),
                candidate_root.span,
            ));
        }
        if !candidate_root.subs.is_empty() || !candidate_field.subs.is_empty() {
            return Err(ToDaeError::unsupported_flat(
                "record output assembly",
                "record call receivers must be whole exact fields",
                candidate_field.span,
            ));
        }
        let resolved = require_constructor_field(&target.name, candidate_field, resolved_fields)?;
        if receivers.insert(resolved.def_id, ordinal).is_some() {
            return Err(ToDaeError::unsupported_flat(
                "record output assembly",
                format!(
                    "`{}.{}` is received more than once",
                    target.name, resolved.name
                ),
                candidate_field.span,
            ));
        }
    }
    Ok(receivers)
}

fn record_call_fields(
    target: &rumoca_core::FunctionParam,
    constructor: &rumoca_core::Function,
    resolved_fields: &[ResolvedFunctionRecordField],
    receivers: &HashMap<rumoca_core::DefId, usize>,
    certificate: &FunctionShapeCertificate,
    span: Span,
) -> Result<Vec<FunctionRecordCallField>, ToDaeError> {
    let mut fields = Vec::with_capacity(constructor.inputs.len());
    let mut fields_source = constructor.inputs.iter();
    let mut resolved = resolved_fields.iter();
    loop {
        let (field, resolved_field) = match (fields_source.next(), resolved.next()) {
            (Some(field), Some(resolved_field)) => (field, resolved_field),
            (None, None) => break,
            _ => {
                return Err(function_statement_product_error(
                    "record constructor field certificate count differs from its source",
                    span,
                ));
            }
        };
        let Some(ordinal) = receivers.get(&resolved_field.def_id) else {
            return Err(function_statement_product_error(
                "record constructor field has no exact call receiver",
                span,
            ));
        };
        let expected = field
            .dimensions()
            .iter()
            .map(|extent| u32::try_from(*extent))
            .collect::<Result<Vec<_>, _>>()
            .map_err(|_| {
                ToDaeError::unsupported_flat(
                    "record output assembly",
                    format!("`{}.{}` has an invalid extent", target.name, field.name),
                    field.span,
                )
            })?;
        let result = certificate.results.get(*ordinal).ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "record output assembly",
                format!(
                    "the call proves no result shape for ordinal {}",
                    ordinal + 1
                ),
                span,
            )
        })?;
        if *result != expected {
            return Err(ToDaeError::unsupported_flat(
                "record output assembly",
                format!(
                    "`{}.{}` has shape {:?}, but result {} has shape {:?}",
                    target.name,
                    field.name,
                    expected,
                    ordinal + 1,
                    result
                ),
                span,
            ));
        }
        fields.push(FunctionRecordCallField {
            name: VarName::new(&field.name),
            def_id: resolved_field.def_id,
            result_ordinal: *ordinal,
        });
    }
    Ok(fields)
}

/// Prove one receiving variable of an MLS §11.2.1.1 multi-result call.
fn plan_multi_output_receiver(
    target: &rumoca_core::ComponentReference,
    ordinal: usize,
    callee_name: &VarName,
    certificate: &FunctionShapeCertificate,
    span: Span,
    context: FunctionValidationContext<'_>,
) -> Result<FunctionAssignmentPlan, ToDaeError> {
    let plan = validate_function_assignment_target(context, target, span)?;
    // MLS §12.4.3: "The type of each component reference in the list must agree
    // with the type of the corresponding output component." (SPEC_0022
    // FUNC-025). The proven result shape is the one the constructed callee
    // actually returns.
    let Some(declared) = call_free_target_shape(target, context.shapes) else {
        return Err(ToDaeError::unsupported_flat(
            "function call statement",
            format!(
                "receiving variable `{}` has no proven shape in this specialization",
                plan.target()
            ),
            span,
        ));
    };
    let result = certificate.results.get(ordinal).ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "function call statement",
            format!("`{callee_name}` proves no result shape for ordinal {ordinal}"),
            span,
        )
    })?;
    if &declared != result {
        return Err(ToDaeError::unsupported_flat(
            "function call statement",
            format!(
                "receiving variable `{}` has shape {declared:?} but result {ordinal} of \
                 `{callee_name}` has shape {result:?}",
                plan.target()
            ),
            span,
        ));
    }
    Ok(plan)
}

/// Prove the MLS §12.4.4 definedness certificate of one planned sequence.
///
/// Every statement reads only values whose elements already have a definition,
/// every element write names the aggregate seed it starts from, and every
/// conditional joins exactly the values it can define on all of its paths.
pub(super) fn resolve_function_definitions(
    statements: &[rumoca_core::Statement],
    plans: &mut [FunctionStatementPlan],
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    if statements.len() != plans.len() {
        return Err(function_statement_product_error(
            "definedness analysis received a mismatched source sequence",
            context.function.span,
        ));
    }
    seed_guarded_sequence_scratch(statements, plans, context, definitions)?;
    let mut remaining_source = statements;
    let mut remaining_plans = plans;
    while let Some((plan, plan_tail)) = remaining_plans.split_first_mut() {
        let Some((statement, source_tail)) = remaining_source.split_first() else {
            return Err(function_statement_product_error(
                "definedness analysis lost its exact source statement",
                context.function.span,
            ));
        };
        if let FunctionStatementPlan::RecordFieldAssembly(assembly) = plan {
            let count = assembly.statement_count;
            let Some(member_count) = count.checked_sub(1) else {
                return Err(function_statement_product_error(
                    "record field assembly has no exact source run",
                    statement.source_span().unwrap_or(context.function.span),
                ));
            };
            if count > remaining_source.len() || member_count > plan_tail.len() {
                return Err(function_statement_product_error(
                    "record field assembly has no exact source run",
                    statement.source_span().unwrap_or(context.function.span),
                ));
            }
            let (source_run, source_tail) = remaining_source.split_at(count);
            let (_, plan_tail) = plan_tail.split_at_mut(member_count);
            resolve_record_field_assembly_definitions(source_run, assembly, context, definitions)?;
            remaining_source = source_tail;
            remaining_plans = plan_tail;
        } else {
            resolve_function_definition(statement, plan, context, definitions)?;
            remaining_source = source_tail;
            remaining_plans = plan_tail;
        }
    }
    if remaining_source.is_empty() {
        Ok(())
    } else {
        Err(function_statement_product_error(
            "definedness analysis left source statements unconsumed",
            context.function.span,
        ))
    }
}

fn resolve_record_field_assembly_definitions(
    statements: &[rumoca_core::Statement],
    assembly: &FunctionRecordFieldAssemblyPlan,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    for statement in statements {
        let rumoca_core::Statement::Assignment { value, span, .. } = statement else {
            return Err(function_statement_product_error(
                "record field assembly contains a foreign source statement",
                statement.source_span().unwrap_or(context.function.span),
            ));
        };
        definitions.require_readable(value, context, *span)?;
    }
    definitions.define_record_field(FunctionRecordFieldIdentity {
        target: assembly.target_def_id,
        field: assembly.field.def_id,
    });
    if assembly.finalize_fields.is_some() {
        definitions.define_function_value(&assembly.target, assembly.target_def_id);
    }
    Ok(())
}

fn resolve_function_definition(
    statement: &rumoca_core::Statement,
    plan: &mut FunctionStatementPlan,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    match (statement, plan) {
        (statement, FunctionStatementPlan::ProvenAssertion) => {
            resolve_function_assertion_definition(statement, false, context, definitions)?
        }
        (statement, FunctionStatementPlan::RuntimeAssertion) => {
            resolve_function_assertion_definition(statement, true, context, definitions)?
        }
        (
            _,
            FunctionStatementPlan::GeneratedBooleanAssignment {
                target,
                value,
                span,
                ..
            },
        ) => resolve_generated_boolean_definition(value, target, *span, context, definitions)?,
        (
            rumoca_core::Statement::Assignment { value, span, .. },
            FunctionStatementPlan::Assignment(assignment),
        ) => {
            resolve_function_assignment_definition(value, *span, assignment, context, definitions)?
        }
        (statement, plan) => {
            return resolve_structured_function_definition(statement, plan, context, definitions);
        }
    }
    Ok(())
}

fn resolve_structured_function_definition(
    statement: &rumoca_core::Statement,
    plan: &mut FunctionStatementPlan,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    match (statement, plan) {
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
            *targets = resolve_planned_conditional(
                (cond_blocks, else_block.as_deref(), *span),
                (branches, fallback.as_mut()),
                context,
                definitions,
            )?;
        }
        (
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            },
            FunctionStatementPlan::ProvenBranch {
                selected,
                statements: branch,
            },
        ) => {
            // MLS §11.5 executes exactly these statements, so they carry
            // the definedness certificate straight into the enclosing
            // sequence: nothing about the values they own is conditional.
            let selected =
                selected_conditional_statements(cond_blocks, else_block.as_deref(), *selected);
            resolve_function_definitions(selected, branch, context, definitions)?;
        }
        (
            rumoca_core::Statement::For {
                indices,
                equations,
                span,
            },
            FunctionStatementPlan::For {
                domain,
                lowering,
                statements: body,
                source_depth,
                ..
            },
        ) => resolve_function_loop_definitions(
            (indices, equations, *span),
            (domain, *source_depth, lowering, body),
            context,
            definitions,
        )?,
        (
            rumoca_core::Statement::FunctionCall { args, span, .. },
            FunctionStatementPlan::MultiOutputCall { outputs },
        ) => resolve_multi_output_definitions(args, *span, outputs, context, definitions)?,
        (
            rumoca_core::Statement::FunctionCall { args, span, .. },
            FunctionStatementPlan::RecordMultiOutputAssembly(assembly),
        ) => resolve_record_multi_output(args, *span, assembly, context, definitions)?,
        (_, FunctionStatementPlan::ArrayAssembly(assembly)) => {
            definitions.define_whole(&assembly.target)
        }
        (_, FunctionStatementPlan::RecordAssembly(assembly)) => {
            definitions.define_function_value(&assembly.target, assembly.target_def_id)
        }
        (_, FunctionStatementPlan::ArrayAssemblyMember)
        | (_, FunctionStatementPlan::RecordAssemblyMember)
        | (_, FunctionStatementPlan::RecordFieldAssemblyMember) => {}
        (_, FunctionStatementPlan::RecordFieldAssembly(_)) => {
            return Err(function_statement_product_error(
                "record field assembly bypassed its source-run owner",
                statement.source_span().unwrap_or(context.function.span),
            ));
        }
        _ => {
            return Err(function_statement_product_error(
                "definedness certificate owns a foreign source statement",
                statement.source_span().unwrap_or(context.function.span),
            ));
        }
    }
    Ok(())
}

fn resolve_planned_conditional(
    source: (
        &[rumoca_core::StatementBlock],
        Option<&[rumoca_core::Statement]>,
        Span,
    ),
    plan: (
        &mut [Vec<FunctionStatementPlan>],
        Option<&mut Vec<FunctionStatementPlan>>,
    ),
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<Vec<FunctionConditionalTarget>, ToDaeError> {
    resolve_function_conditional(
        source.0,
        source.1,
        plan.0,
        plan.1,
        source.2,
        context,
        definitions,
    )
}

fn resolve_generated_boolean_definition(
    value: &Expression,
    target: &VarName,
    span: Span,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    definitions.require_readable(value, context, span)?;
    definitions.define_whole(target);
    Ok(())
}

fn resolve_record_multi_output(
    arguments: &[Expression],
    span: Span,
    assembly: &FunctionRecordCallAssemblyPlan,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    for argument in arguments {
        definitions.require_readable(argument, context, span)?;
    }
    definitions.define_function_value(&assembly.target, assembly.target_def_id);
    Ok(())
}

fn resolve_function_assertion_definition(
    statement: &rumoca_core::Statement,
    reads_message: bool,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    let Some(assertion) = function_assertion(statement, context.flat)? else {
        return Err(function_statement_product_error(
            "assertion lowering certificate owns a foreign source statement",
            statement.source_span().unwrap_or(context.function.span),
        ));
    };
    definitions.require_readable(assertion.condition, context, assertion.span)?;
    if reads_message {
        definitions.require_readable(assertion.message, context, assertion.span)?;
    }
    Ok(())
}

fn resolve_multi_output_definitions(
    arguments: &[Expression],
    span: Span,
    outputs: &mut [Option<FunctionAssignmentPlan>],
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    // MLS §11.2.1.1 reads every argument before defining receivers.
    for argument in arguments {
        definitions.require_readable(argument, context, span)?;
    }
    for plan in outputs.iter_mut().flatten() {
        for expression in plan.subscripts().iter().filter_map(subscript_expression) {
            definitions.require_readable(expression, context, span)?;
        }
        if plan.is_whole() {
            match plan.record_field() {
                Some(identity) => definitions.define_record_field(identity),
                None => definitions.define_function_value(plan.target(), plan.target_def_id()),
            }
        } else {
            let seed =
                definitions.write_elements(plan.target(), plan.subscripts(), context, span)?;
            plan.seed = plan.seed.take().or(seed);
        }
    }
    Ok(())
}

/// Prove what one assignment reads and record what it defines.
fn resolve_function_assignment_definition(
    value: &Expression,
    span: Span,
    assignment: &mut FunctionAssignmentPlan,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    definitions.require_readable(value, context, span)?;
    for expression in assignment
        .subscripts
        .iter()
        .filter_map(subscript_expression)
    {
        definitions.require_readable(expression, context, span)?;
    }
    if assignment.is_whole() {
        match assignment.record_field {
            Some(identity) => definitions.define_record_field(identity),
            None => definitions.define_function_value(&assignment.target, assignment.target_def_id),
        }
        return Ok(());
    }
    let seed =
        definitions.write_elements(&assignment.target, &assignment.subscripts, context, span)?;
    assignment.seed = assignment.seed.take().or(seed);
    Ok(())
}

fn subscript_expression(subscript: &Subscript) -> Option<&Expression> {
    match subscript {
        rumoca_core::Subscript::Expr { expr, .. } => Some(expr),
        rumoca_core::Subscript::Index { .. } | rumoca_core::Subscript::Colon { .. } => None,
    }
}

/// A loop transition may only carry values whose elements already have a
/// definition, because MLS §12.4.4 gives the carried value no other owner.
fn resolve_function_loop_definitions(
    source: (&[rumoca_core::ForIndex], &[rumoca_core::Statement], Span),
    planned: (
        &StructuredIndexDomain,
        usize,
        &mut FunctionLoopLowering,
        &mut [FunctionStatementPlan],
    ),
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    let (indices, statements, span) = source;
    let (domain, source_depth, lowering, body) = planned;
    match lowering {
        FunctionLoopLowering::TotalArrayDefinition => {
            for plan in body {
                if let FunctionStatementPlan::Assignment(assignment) = plan {
                    match assignment.record_field {
                        Some(identity) => definitions.define_record_field(identity),
                        None => definitions
                            .define_function_value(&assignment.target, assignment.target_def_id),
                    }
                }
            }
        }
        FunctionLoopLowering::Fold {
            targets,
            iteration_locals,
        } => {
            resolve_fold_definitions(
                (indices, statements, span),
                (domain, source_depth, body, targets, iteration_locals),
                context,
                definitions,
            )?;
        }
    }
    Ok(())
}

fn resolve_fold_definitions(
    source: (&[rumoca_core::ForIndex], &[rumoca_core::Statement], Span),
    planned: (
        &StructuredIndexDomain,
        usize,
        &mut [FunctionStatementPlan],
        &mut Vec<VarName>,
        &mut Vec<VarName>,
    ),
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    let (indices, statements, span) = source;
    let (domain, source_depth, plans, targets, iteration_locals) = planned;
    let enclosing_definitions = definitions.clone();
    let (indices, statements) = flattened_function_loop_source(indices, statements, source_depth);
    seed_guarded_sequence_scratch(statements, plans, context, definitions)?;
    let domain = domain.validated().map_err(|error| {
        ToDaeError::unsupported_flat(
            "function loop transition",
            format!(
                "`{}` has an invalid compact domain: {error}",
                context.function.name
            ),
            span,
        )
    })?;
    for ordinal in 0..domain.scalar_count() {
        definitions.clear_names(iteration_locals);
        let point = domain.index_tuple_at(ordinal).ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "function loop transition",
                format!(
                    "`{}` has a missing compact-domain point",
                    context.function.name
                ),
                span,
            )
        })?;
        let mut integers = context.static_integers.clone();
        integers.extend(
            indices
                .iter()
                .zip(point)
                .map(|(index, value)| (VarName::new(&index.ident), value)),
        );
        let point_context = FunctionValidationContext {
            static_integers: &integers,
            ..context
        };
        resolve_fold_iteration(statements, plans, point_context, definitions)?;
        definitions.forget_varying_guard_paths(context.generated_booleans);
    }
    targets.retain(|target| {
        definitions.is_defined(target) || definitions.has_total_guarded_definition(target)
    });
    definitions.restore_names(&enclosing_definitions, iteration_locals);
    Ok(())
}

fn resolve_fold_iteration(
    statements: &[rumoca_core::Statement],
    plans: &mut [FunctionStatementPlan],
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    if statements.len() != plans.len() {
        return Err(function_statement_product_error(
            "fold analysis received a mismatched source sequence",
            context.function.span,
        ));
    }
    let mut remaining_source = statements;
    let mut remaining_plans = plans;
    while let Some((plan, plan_tail)) = remaining_plans.split_first_mut() {
        let Some((statement, source_tail)) = remaining_source.split_first() else {
            return Err(function_statement_product_error(
                "fold analysis lost its exact source statement",
                context.function.span,
            ));
        };
        if let FunctionStatementPlan::RecordAssembly(assembly) = plan {
            let count = assembly.statement_count;
            let Some(member_count) = count.checked_sub(1) else {
                return Err(function_statement_product_error(
                    "fold record assembly has no exact source run",
                    statement.source_span().unwrap_or(context.function.span),
                ));
            };
            if count > remaining_source.len() || member_count > plan_tail.len() {
                return Err(function_statement_product_error(
                    "fold record assembly has no exact source run",
                    statement.source_span().unwrap_or(context.function.span),
                ));
            }
            let (source_run, source_tail) = remaining_source.split_at(count);
            let (_, plan_tail) = plan_tail.split_at_mut(member_count);
            resolve_fold_record_assembly(source_run, assembly, context, definitions)?;
            remaining_source = source_tail;
            remaining_plans = plan_tail;
            continue;
        }
        resolve_fold_statement(statement, plan, context, definitions)?;
        remaining_source = source_tail;
        remaining_plans = plan_tail;
    }
    if remaining_source.is_empty() {
        Ok(())
    } else {
        Err(function_statement_product_error(
            "fold analysis left source statements unconsumed",
            context.function.span,
        ))
    }
}

fn resolve_fold_statement(
    statement: &rumoca_core::Statement,
    plan: &mut FunctionStatementPlan,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    match (statement, plan) {
        (statement, FunctionStatementPlan::ProvenAssertion) => {
            let Some(assertion) = function_assertion(statement, context.flat)? else {
                return Err(function_statement_product_error(
                    "proven fold assertion owns a foreign source statement",
                    statement.source_span().unwrap_or(context.function.span),
                ));
            };
            definitions.require_readable(assertion.condition, context, assertion.span)?;
        }
        (statement, FunctionStatementPlan::RuntimeAssertion) => {
            let Some(assertion) = function_assertion(statement, context.flat)? else {
                return Err(function_statement_product_error(
                    "runtime fold assertion owns a foreign source statement",
                    statement.source_span().unwrap_or(context.function.span),
                ));
            };
            definitions.require_readable(assertion.condition, context, assertion.span)?;
            definitions.require_readable(assertion.message, context, assertion.span)?;
        }
        (
            rumoca_core::Statement::Assignment { value, span, .. },
            FunctionStatementPlan::Assignment(assignment),
        ) => resolve_fold_assignment(value, *span, assignment, context, definitions)?,
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
            let point_targets = resolve_function_conditional(
                cond_blocks,
                else_block.as_deref(),
                branches,
                fallback.as_mut(),
                *span,
                context,
                definitions,
            )?;
            union_targets(targets, point_targets);
        }
        (
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            },
            FunctionStatementPlan::ProvenBranch {
                selected,
                statements,
            },
        ) => {
            let selected =
                selected_conditional_statements(cond_blocks, else_block.as_deref(), *selected);
            resolve_fold_iteration(selected, statements, context, definitions)?;
        }
        (
            rumoca_core::Statement::For {
                indices,
                equations,
                span,
            },
            FunctionStatementPlan::For {
                domain,
                lowering,
                statements,
                source_depth,
                ..
            },
        ) => resolve_function_loop_definitions(
            (indices, equations, *span),
            (domain, *source_depth, lowering, statements),
            context,
            definitions,
        )?,
        (
            rumoca_core::Statement::FunctionCall { args, span, .. },
            FunctionStatementPlan::MultiOutputCall { outputs },
        ) => resolve_multi_output_definitions(args, *span, outputs, context, definitions)?,
        _ => {
            return Err(function_statement_product_error(
                "fold certificate owns a foreign source statement",
                statement.source_span().unwrap_or(context.function.span),
            ));
        }
    }
    Ok(())
}

fn resolve_fold_record_assembly(
    statements: &[rumoca_core::Statement],
    assembly: &mut FunctionRecordAssemblyPlan,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    for statement in statements {
        let rumoca_core::Statement::Assignment { value, span, .. } = statement else {
            return Err(function_statement_product_error(
                "fold record assembly contains a foreign source statement",
                statement.source_span().unwrap_or(context.function.span),
            ));
        };
        definitions.require_readable(value, context, *span)?;
    }
    if !definitions.is_defined(&assembly.target) && assembly.seed.is_none() {
        let Some(first) = statements.first() else {
            return Err(function_statement_product_error(
                "fold record assembly has an empty source run",
                context.function.span,
            ));
        };
        let span = required_statement_span(first, "function loop record assembly")?;
        assembly.seed = Some(definitions.whole_loop_seed(&assembly.target, context, span)?);
    }
    definitions.define_function_value(&assembly.target, assembly.target_def_id);
    Ok(())
}

fn union_targets(
    targets: &mut Vec<FunctionConditionalTarget>,
    candidates: Vec<FunctionConditionalTarget>,
) {
    for target in candidates {
        if !targets.contains(&target) {
            targets.push(target);
        }
    }
}

fn resolve_fold_assignment(
    value: &Expression,
    span: Span,
    assignment: &mut FunctionAssignmentPlan,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    definitions.require_readable(value, context, span)?;
    for expression in assignment
        .subscripts()
        .iter()
        .filter_map(subscript_expression)
    {
        definitions.require_readable(expression, context, span)?;
    }
    if assignment.subscripts().is_empty() {
        seed_undefined_whole_loop_value(assignment, context, definitions, span)?;
        match assignment.record_field() {
            Some(identity) => definitions.define_record_field(identity),
            None => {
                definitions.define_function_value(assignment.target(), assignment.target_def_id())
            }
        }
        return Ok(());
    }
    let seed =
        definitions.write_elements(assignment.target(), assignment.subscripts(), context, span)?;
    assignment.seed = assignment.seed.take().or(seed);
    Ok(())
}

fn seed_undefined_whole_loop_value(
    assignment: &mut FunctionAssignmentPlan,
    context: FunctionValidationContext<'_>,
    definitions: &FunctionDefinitions,
    span: Span,
) -> Result<(), ToDaeError> {
    if !definitions.is_defined(assignment.target()) && assignment.seed.is_none() {
        assignment.seed = Some(definitions.whole_loop_seed(assignment.target(), context, span)?);
    }
    Ok(())
}

pub(super) fn validate_function_subscripts(
    subscripts: &[Subscript],
    context: FunctionValidationContext<'_>,
) -> Result<(), ToDaeError> {
    validate_specialized_subscripts(subscripts, context.roles, context.shapes)?;
    for subscript in subscripts {
        if let rumoca_core::Subscript::Expr { expr, .. } = subscript {
            validate_known_function_calls(expr, context.flat)?;
        }
    }
    Ok(())
}
