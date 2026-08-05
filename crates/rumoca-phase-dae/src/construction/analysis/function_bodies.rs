use super::*;
use crate::construction::function_shapes::ProvenValue;

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
    let statements = validate_function_statements(&source, returned_context, &mut definitions)?;
    require_total_outputs(function, &definitions)?;
    Ok(FunctionPlan::Statements {
        source,
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
    resolve_function_definitions(statements, &mut plans, context, definitions)?;
    Ok(plans)
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
        let statement_context = FunctionValidationContext {
            staged_record_fields: &staged_record_fields,
            ..context
        };
        if let rumoca_core::Statement::Empty { span } = &statements[index]
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
        if let Some(assertion) = function_assertion(&statements[index], statement_context.flat)? {
            plans.push(plan_proven_function_assertion(
                assertion,
                statement_context,
            )?);
            index += 1;
            continue;
        }
        if let Some(assembly) = staged_records.remove(&index) {
            let count = assembly.statement_count;
            let staged_field = FunctionRecordFieldCoordinate {
                target: assembly.target.clone(),
                field: assembly.field.name.clone(),
            };
            let finalizes_record = assembly.finalize_fields.is_some();
            let target = assembly.target.clone();
            plans.push(FunctionStatementPlan::RecordFieldAssembly(assembly));
            plans.extend(
                std::iter::repeat_with(|| FunctionStatementPlan::RecordFieldAssemblyMember)
                    .take(count - 1),
            );
            if finalizes_record {
                staged_record_fields.retain(|field| field.target != target);
            } else {
                staged_record_fields.insert(staged_field);
            }
            index += count;
            continue;
        }
        if staged_members.contains(&index) {
            unreachable!("a staged record member follows its owning field assembly")
        }
        if let Some((assembly, count)) =
            validate_record_output_assembly(statements, index, statement_context)?
        {
            plans.push(FunctionStatementPlan::RecordAssembly(assembly));
            plans.extend(
                std::iter::repeat_with(|| FunctionStatementPlan::RecordAssemblyMember)
                    .take(count - 1),
            );
            index += count;
            continue;
        }
        plans.push(plan_one_function_statement(
            &statements[index],
            statement_context,
        )?);
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
            let name = comp.to_var_name();
            if flat.functions.contains_key(&name)
                || rumoca_core::runtime_flow_action_function_short_name(name.as_str())
                    != Some("assert")
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
        Some(ProvenValue::Boolean(true))
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
    let (target, subscripts) = match component.parts() {
        [target] => (VarName::new(&target.ident), &target.subs),
        [root, field]
            if root.subs.is_empty()
                && context
                    .function
                    .outputs
                    .iter()
                    .chain(&context.function.locals)
                    .any(|value| {
                        value.name == root.ident
                            && value.type_class == Some(rumoca_core::ClassType::Record)
                    }) =>
        {
            (component.to_var_name(), &field.subs)
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
        subscripts: subscripts.clone().into_boxed_slice(),
        seed: None,
    })
}

pub(super) struct MultiOutputCallStatement<'statement> {
    pub(super) callee: &'statement rumoca_core::ComponentReference,
    pub(super) args: &'statement [Expression],
    pub(super) outputs: &'statement [Option<rumoca_core::ComponentReference>],
    pub(super) span: Span,
}

/// Prove the checked owner of an MLS §11.2.1.1 multi-result call statement.
///
/// MLS §11.2.1.1 writes the statement as
/// `"(" output-expression-list ")" ":=" component-reference function-call-args`
/// and states: "A function with n results needs m≤n receiving variables on the
/// left-hand side, and the variables are assigned from left to right." An
/// omitted receiver — `(out1, , out3)` — is a hole in that list, not a value.
///
/// The DAE owns one call expression per *read* result ordinal (the same
/// `call(function, ordinal, ..)` node an MLS §11.2.1 single-result call builds
/// at ordinal 0), so a receiving slot becomes an ordinary whole-value
/// definition of its target and an omitted slot becomes nothing at all.
fn plan_function_multi_output_call(
    call: MultiOutputCallStatement<'_>,
    context: FunctionValidationContext<'_>,
) -> Result<FunctionStatementPlan, ToDaeError> {
    require_span(call.span, "function multi-result call statement")?;
    if call.callee.parts().is_empty()
        || call.callee.parts().iter().any(|part| !part.subs.is_empty())
    {
        return Err(ToDaeError::unsupported_flat(
            "function call statement",
            "a multi-result call statement requires one resolved, unsubscripted function",
            call.span,
        ));
    }
    let callee_name = call.callee.to_var_name();
    let Some(callee) = context.flat.functions.get(&callee_name) else {
        return Err(ToDaeError::unsupported_flat(
            "function call statement",
            format!("`{callee_name}` is not a declared function of the flat model"),
            call.span,
        ));
    };
    // MLS §12.6 makes a record constructor an expression-only callable: it has
    // one result and no statement form, so it never owns a receiving list.
    if callee.is_constructor {
        return Err(ToDaeError::unsupported_flat(
            "function call statement",
            format!(
                "`{callee_name}` is a record constructor, which owns no multi-result call statement"
            ),
            call.span,
        ));
    }
    // MLS §12.4.3 and §11.2.1.1 evaluate the right-hand call *once* and then
    // assign the receiving variables. The canonical DAE has no multi-result
    // node: it owns one `call(function, ordinal, ..)` per result read, so a
    // statement that reads k results denotes k invocations. That is
    // indistinguishable from one evaluation only for a function whose result
    // depends on nothing but its arguments. `body_is_pure` is exactly that
    // predicate: MLS 3.7 §12.3 treats an MLS §12.9 external body that declared
    // no purity as impure, which is the form rumoca already reports as
    // deprecated (WR001). Such callees are refused by name rather than being
    // silently invoked once per receiver.
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
    // A statement call that reads no result defines nothing, so no DAE owner
    // observes it. MLS §12.3 admits such a call for its effect, but only an
    // impure context has an effect to keep; this body has no owner for one
    // either way, so the rejection names the missing owner and not a purity
    // the caller may not have.
    if call.outputs.iter().all(Option::is_none) {
        return Err(ToDaeError::unsupported_flat(
            "function call statement",
            format!(
                "`{}` calls `{callee_name}` as a statement without reading a result, which the \
                 canonical DAE has no owner for",
                context.function.name
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
    let reference = rumoca_core::Reference::from_component_reference(call.callee.clone());
    let key = context
        .shape_analysis
        .call_key(&reference, call.args, context.shapes, call.span)?;
    let certificate = context
        .shape_analysis
        .certificate(&key)
        .expect("call_key proves the certificate it returns a key for");
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
            &callee_name,
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
    let Some(target) = context
        .function
        .outputs
        .iter()
        .chain(&context.function.locals)
        .find(|value| {
            value.name == root.ident && value.type_class == Some(rumoca_core::ClassType::Record)
        })
    else {
        return Ok(None);
    };
    let constructor = record_constructor(target, context)?;
    if outputs.len() != constructor.inputs.len() || outputs.iter().any(Option::is_none) {
        return Ok(None);
    }

    let mut fields = Vec::with_capacity(constructor.inputs.len());
    for field in &constructor.inputs {
        let field_def_id = field.def_id.ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "record output assembly",
                format!(
                    "`{}.{}` has no exact field identity",
                    target.name, field.name
                ),
                field.span,
            )
        })?;
        let Some((ordinal, receiver)) =
            outputs.iter().enumerate().find_map(|(ordinal, receiver)| {
                let receiver = receiver.as_ref()?;
                let [candidate_root, candidate_field] = receiver.parts() else {
                    return None;
                };
                (candidate_root.ident == target.name
                    && candidate_field.ident == field.name
                    && candidate_field.def_id == field_def_id
                    && candidate_root.subs.is_empty()
                    && candidate_field.subs.is_empty())
                .then_some((ordinal, candidate_field))
            })
        else {
            return Ok(None);
        };
        let expected = field
            .dimensions()
            .iter()
            .map(|extent| u32::try_from(*extent))
            .collect::<Result<Vec<_>, _>>()
            .map_err(|_| {
                ToDaeError::unsupported_flat(
                    "record output assembly",
                    format!("`{}.{}` has an invalid extent", target.name, receiver.ident),
                    span,
                )
            })?;
        let Some(result) = certificate.results.get(ordinal) else {
            return Ok(None);
        };
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
            result_ordinal: ordinal,
        });
    }
    Ok(Some(FunctionRecordCallAssemblyPlan {
        target: VarName::new(&target.name),
        fields,
    }))
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
    debug_assert_eq!(statements.len(), plans.len());
    seed_guarded_sequence_scratch(statements, plans, context, definitions)?;
    let mut index = 0usize;
    while index < statements.len() {
        if let FunctionStatementPlan::RecordFieldAssembly(assembly) = &plans[index] {
            resolve_record_field_assembly_definitions(
                &statements[index..index + assembly.statement_count],
                assembly,
                context,
                definitions,
            )?;
            index += assembly.statement_count;
            continue;
        }
        resolve_function_definition(&statements[index], &mut plans[index], context, definitions)?;
        index += 1;
    }
    Ok(())
}

fn resolve_record_field_assembly_definitions(
    statements: &[rumoca_core::Statement],
    assembly: &FunctionRecordFieldAssemblyPlan,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    for statement in statements {
        let rumoca_core::Statement::Assignment { value, span, .. } = statement else {
            unreachable!("record field assembly contains assignments")
        };
        definitions.require_readable(value, context, *span)?;
    }
    let staged = function_record_field_name(&assembly.target, &assembly.field.name);
    definitions.define_whole(&staged);
    if assembly.finalize_fields.is_some() {
        definitions.define_whole(&assembly.target);
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
            )?
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
        ) => resolve_record_multi_output(args, *span, &assembly.target, context, definitions)?,
        (_, FunctionStatementPlan::ArrayAssembly(assembly)) => {
            definitions.define_whole(&assembly.target)
        }
        (_, FunctionStatementPlan::RecordAssembly(assembly)) => {
            definitions.define_whole(&assembly.target)
        }
        (_, FunctionStatementPlan::ArrayAssemblyMember)
        | (_, FunctionStatementPlan::RecordAssemblyMember)
        | (_, FunctionStatementPlan::RecordFieldAssemblyMember) => {}
        (_, FunctionStatementPlan::RecordFieldAssembly(_)) => {
            unreachable!("record field assemblies are resolved with their source run")
        }
        _ => unreachable!("function planning aligns statement and plan shapes"),
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
) -> Result<Vec<VarName>, ToDaeError> {
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
    target: &VarName,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    for argument in arguments {
        definitions.require_readable(argument, context, span)?;
    }
    definitions.define_whole(target);
    Ok(())
}

fn resolve_function_assertion_definition(
    statement: &rumoca_core::Statement,
    reads_message: bool,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    let assertion = function_assertion(statement, context.flat)?
        .expect("an assertion plan owns an assertion statement");
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
            definitions.define_whole(plan.target());
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
        definitions.define_whole(&assignment.target);
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
                    definitions.define_whole(&assignment.target);
                }
            }
        }
        FunctionLoopLowering::Fold { targets } => {
            resolve_fold_definitions(
                (indices, statements, span),
                (domain, source_depth, body, targets),
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
    ),
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    let (indices, statements, span) = source;
    let (domain, source_depth, plans, targets) = planned;
    let (indices, statements) = flattened_function_loop_source(indices, statements, source_depth);
    seed_guarded_sequence_scratch(statements, plans, context, definitions)?;
    let point_count = domain.scalar_count().map_err(|error| {
        ToDaeError::unsupported_flat(
            "function loop transition",
            format!(
                "`{}` has an invalid compact domain: {error}",
                context.function.name
            ),
            span,
        )
    })?;
    for ordinal in 0..point_count {
        let point = domain
            .index_tuple_at(ordinal)
            .map_err(|error| {
                ToDaeError::unsupported_flat(
                    "function loop transition",
                    format!(
                        "`{}` cannot project its compact domain: {error}",
                        context.function.name
                    ),
                    span,
                )
            })?
            .ok_or_else(|| {
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
    Ok(())
}

fn seed_guarded_sequence_scratch(
    statements: &[rumoca_core::Statement],
    plans: &mut [FunctionStatementPlan],
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    for outer_index in 0..plans.len() {
        let Some((guard, branch)) = guarded_sequence(&statements[outer_index]) else {
            continue;
        };
        let Some(inner_len) = guarded_plan_len(&plans[outer_index]) else {
            continue;
        };
        for inner_index in 0..inner_len {
            let candidates = guarded_plan_targets(&plans[outer_index], inner_index);
            for target in candidates {
                seed_guarded_candidate(
                    (statements, branch),
                    (outer_index, inner_index),
                    &target,
                    guard,
                    context,
                    definitions,
                    &mut plans[outer_index],
                )?;
            }
        }
    }
    Ok(())
}

fn seed_guarded_candidate(
    sources: (&[rumoca_core::Statement], &[rumoca_core::Statement]),
    indices: (usize, usize),
    target: &VarName,
    guard: &Expression,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
    plan: &mut FunctionStatementPlan,
) -> Result<(), ToDaeError> {
    let (statements, branch) = sources;
    if guarded_scratch_is_observable(
        statements,
        branch,
        indices,
        target,
        guard,
        context,
        definitions,
    ) {
        return Ok(());
    }
    let span = required_statement_span(&branch[indices.1], "guarded function-loop scratch")?;
    let seed = definitions.whole_loop_seed(target, context, span)?;
    attach_guarded_seed(plan, indices.1, target, seed);
    definitions.define_whole(target);
    Ok(())
}

fn guarded_sequence(
    statement: &rumoca_core::Statement,
) -> Option<(&Expression, &[rumoca_core::Statement])> {
    let rumoca_core::Statement::If {
        cond_blocks,
        else_block: None,
        ..
    } = statement
    else {
        return None;
    };
    let [block] = cond_blocks.as_slice() else {
        return None;
    };
    Some((&block.cond, &block.stmts))
}

fn guarded_plan_len(plan: &FunctionStatementPlan) -> Option<usize> {
    match plan {
        FunctionStatementPlan::If {
            branches,
            fallback: None,
            ..
        } if branches.len() == 1 => Some(branches[0].len()),
        _ => None,
    }
}

fn guarded_plan_targets(plan: &FunctionStatementPlan, inner_index: usize) -> Vec<VarName> {
    let FunctionStatementPlan::If { branches, .. } = plan else {
        unreachable!("uniform guard proof aligns source and plan conditionals")
    };
    whole_definition_targets(&branches[0][inner_index])
}

fn guarded_scratch_is_observable(
    statements: &[rumoca_core::Statement],
    branch: &[rumoca_core::Statement],
    indices: (usize, usize),
    target: &VarName,
    guard: &Expression,
    context: FunctionValidationContext<'_>,
    definitions: &FunctionDefinitions,
) -> bool {
    definitions.is_defined(target)
        || context
            .function
            .outputs
            .iter()
            .any(|output| output.name == target.as_str())
        || guarded_prefix_reads_name(statements, indices.0, branch, indices.1, target)
        || statements
            .iter()
            .any(|statement| has_unguarded_target_read(statement, target, guard, false))
}

fn attach_guarded_seed(
    plan: &mut FunctionStatementPlan,
    inner_index: usize,
    target: &VarName,
    seed: FunctionValueSeed,
) {
    let FunctionStatementPlan::If { branches, .. } = plan else {
        unreachable!("uniform guard proof aligns source and plan conditionals")
    };
    attach_whole_definition_seed(&mut branches[0][inner_index], target, seed);
}

fn whole_definition_targets(plan: &FunctionStatementPlan) -> Vec<VarName> {
    match plan {
        FunctionStatementPlan::Assignment(assignment) if assignment.is_whole() => {
            vec![assignment.target().clone()]
        }
        FunctionStatementPlan::MultiOutputCall { outputs } => outputs
            .iter()
            .flatten()
            .filter(|output| output.is_whole())
            .map(|output| output.target().clone())
            .collect(),
        FunctionStatementPlan::RecordAssembly(assembly) => vec![assembly.target.clone()],
        FunctionStatementPlan::ArrayAssembly(assembly) => vec![assembly.target.clone()],
        _ => Vec::new(),
    }
}

fn attach_whole_definition_seed(
    plan: &mut FunctionStatementPlan,
    target: &VarName,
    seed: FunctionValueSeed,
) {
    match plan {
        FunctionStatementPlan::Assignment(assignment) if assignment.target() == target => {
            assignment.seed = Some(seed);
        }
        FunctionStatementPlan::MultiOutputCall { outputs } => {
            let output = outputs
                .iter_mut()
                .flatten()
                .find(|output| output.target() == target)
                .expect("whole-definition candidate remains in its plan");
            output.seed = Some(seed);
        }
        FunctionStatementPlan::RecordAssembly(assembly) if &assembly.target == target => {
            assembly.seed = Some(seed);
        }
        FunctionStatementPlan::ArrayAssembly(assembly) if &assembly.target == target => {
            assembly.seed = Some(seed);
        }
        _ => unreachable!("whole-definition candidate remains in its plan"),
    }
}

fn guarded_prefix_reads_name(
    guarded: &[rumoca_core::Statement],
    outer_index: usize,
    branch: &[rumoca_core::Statement],
    inner_index: usize,
    target: &VarName,
) -> bool {
    guarded[..outer_index]
        .iter()
        .any(|statement| statement_reads_target(statement, target))
        || branch[..=inner_index]
            .iter()
            .any(|statement| statement_reads_target(statement, target))
}

fn has_unguarded_target_read(
    statement: &rumoca_core::Statement,
    target: &VarName,
    guard: &Expression,
    guarded: bool,
) -> bool {
    match statement {
        rumoca_core::Statement::For {
            indices, equations, ..
        } => {
            (!guarded
                && indices
                    .iter()
                    .any(|index| expression_reads_target(&index.range, target)))
                || equations
                    .iter()
                    .any(|statement| has_unguarded_target_read(statement, target, guard, guarded))
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks.iter().any(|block| {
                (!guarded && expression_reads_target(&block.cond, target))
                    || block.stmts.iter().any(|statement| {
                        let branch_guarded = guarded || condition_implies_guard(&block.cond, guard);
                        has_unguarded_target_read(statement, target, guard, branch_guarded)
                    })
            }) || else_block.as_ref().is_some_and(|statements| {
                statements
                    .iter()
                    .any(|statement| has_unguarded_target_read(statement, target, guard, guarded))
            })
        }
        _ => !guarded && statement_reads_target(statement, target),
    }
}

fn condition_implies_guard(condition: &Expression, guard: &Expression) -> bool {
    if rumoca_core::expressions_semantically_equal(condition, guard) {
        return true;
    }
    matches!(
        condition,
        Expression::Binary {
            op: OpBinary::And,
            lhs,
            rhs,
            ..
        } if condition_implies_guard(lhs, guard) || condition_implies_guard(rhs, guard)
    )
}

fn statement_reads_target(statement: &rumoca_core::Statement, target: &VarName) -> bool {
    match statement {
        rumoca_core::Statement::Assignment { comp, value, .. } => {
            expression_reads_target(value, target) || component_subscripts_read_target(comp, target)
        }
        rumoca_core::Statement::FunctionCall { args, outputs, .. } => {
            args.iter()
                .any(|argument| expression_reads_target(argument, target))
                || outputs
                    .iter()
                    .flatten()
                    .any(|output| component_subscripts_read_target(output, target))
        }
        rumoca_core::Statement::For {
            indices, equations, ..
        } => {
            indices
                .iter()
                .any(|index| expression_reads_target(&index.range, target))
                || equations
                    .iter()
                    .any(|statement| statement_reads_target(statement, target))
        }
        rumoca_core::Statement::While { block, .. } => {
            expression_reads_target(&block.cond, target)
                || block
                    .stmts
                    .iter()
                    .any(|statement| statement_reads_target(statement, target))
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks.iter().any(|block| {
                expression_reads_target(&block.cond, target)
                    || block
                        .stmts
                        .iter()
                        .any(|statement| statement_reads_target(statement, target))
            }) || else_block.as_ref().is_some_and(|statements| {
                statements
                    .iter()
                    .any(|statement| statement_reads_target(statement, target))
            })
        }
        rumoca_core::Statement::When { blocks, .. } => blocks.iter().any(|block| {
            expression_reads_target(&block.cond, target)
                || block
                    .stmts
                    .iter()
                    .any(|statement| statement_reads_target(statement, target))
        }),
        rumoca_core::Statement::Reinit {
            variable, value, ..
        } => {
            component_subscripts_read_target(variable, target)
                || expression_reads_target(value, target)
        }
        rumoca_core::Statement::Assert {
            condition,
            message,
            level,
            ..
        } => {
            expression_reads_target(condition, target)
                || expression_reads_target(message, target)
                || level
                    .as_deref()
                    .is_some_and(|level| expression_reads_target(level, target))
        }
        rumoca_core::Statement::Empty { .. }
        | rumoca_core::Statement::Return { .. }
        | rumoca_core::Statement::Break { .. } => false,
    }
}

fn component_subscripts_read_target(
    component: &rumoca_core::ComponentReference,
    target: &VarName,
) -> bool {
    component.parts().iter().any(|part| {
        part.subs.iter().any(|subscript| match subscript {
            Subscript::Expr { expr, .. } => expression_reads_target(expr, target),
            Subscript::Index { .. } | Subscript::Colon { .. } => false,
        })
    })
}

fn expression_reads_target(expression: &Expression, target: &VarName) -> bool {
    let mut references = Vec::new();
    expression.collect_var_refs(&mut references);
    references.iter().any(|reference| reference == target)
}

fn resolve_fold_iteration(
    statements: &[rumoca_core::Statement],
    plans: &mut [FunctionStatementPlan],
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    let mut index = 0usize;
    while index < statements.len() {
        if let FunctionStatementPlan::RecordAssembly(assembly) = &mut plans[index] {
            let count = assembly.statement_count;
            resolve_fold_record_assembly(
                &statements[index..index + count],
                assembly,
                context,
                definitions,
            )?;
            index += count;
            continue;
        }
        let statement = &statements[index];
        let plan = &mut plans[index];
        match (statement, plan) {
            (statement, FunctionStatementPlan::ProvenAssertion) => {
                let assertion = function_assertion(statement, context.flat)?
                    .expect("a proven loop assertion owns an assertion statement");
                definitions.require_readable(assertion.condition, context, assertion.span)?;
            }
            (statement, FunctionStatementPlan::RuntimeAssertion) => {
                let assertion = function_assertion(statement, context.flat)?
                    .expect("a runtime loop assertion owns an assertion statement");
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
            ) => {
                resolve_multi_output_definitions(args, *span, outputs, context, definitions)?;
            }
            _ => unreachable!("analysis admits only checked transition statements in a fold"),
        }
        index += 1;
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
            unreachable!("record assembly certificate contains assignments")
        };
        definitions.require_readable(value, context, *span)?;
    }
    if !definitions.is_defined(&assembly.target) && assembly.seed.is_none() {
        let span = required_statement_span(&statements[0], "function loop record assembly")?;
        assembly.seed = Some(definitions.whole_loop_seed(&assembly.target, context, span)?);
    }
    definitions.define_whole(&assembly.target);
    Ok(())
}

fn union_targets(targets: &mut Vec<VarName>, candidates: Vec<VarName>) {
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
        definitions.define_whole(assignment.target());
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
