use super::*;

pub(super) fn validate_functions(
    flat: &flat::Model,
    shapes: &FunctionShapeAnalysis,
) -> Result<HashMap<FunctionSpecializationKey, FunctionPlan>, ToDaeError> {
    let mut plans = HashMap::with_capacity(shapes.certificates().len());
    for certificate in shapes.certificates() {
        let function = &flat.functions[&certificate.key.function];
        require_span(function.span, "function declaration")?;
        if function.is_constructor {
            return Err(ToDaeError::unsupported_flat(
                "function lifecycle",
                format!("`{}` is not a pure Modelica function body", function.name),
                function.span,
            ));
        }
        // MLS §12.3: a Modelica body is pure. Only an MLS §12.9 external body
        // may be impure, and it reaches the DAE through its checked interface.
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
                validate_function_expression(default, function, flat, &certificate.values)?;
            }
        }
        let static_integers = immutable_integer_defaults(function, flat, &certificate.values)?;
        let roles = function_expression_roles(function, flat);
        let context = FunctionValidationContext {
            function,
            flat,
            roles: &roles,
            static_integers: &static_integers,
            shapes: &certificate.values,
            shape_analysis: shapes,
        };
        let plan = if function.external.is_some() {
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
            FunctionPlan::External(validate_external_function(function, context)?)
        } else if let Some(plan) = validate_guarded_function_return(function, context)? {
            plan
        } else if let Some(plan) = validate_integer_reduction(function, context)? {
            plan
        } else {
            let mut definitions = FunctionDefinitions::new(function);
            let statements =
                validate_function_statements(&function.body, context, &mut definitions)?;
            require_total_outputs(function, &definitions)?;
            FunctionPlan::Statements { statements }
        };
        plans.insert(certificate.key.clone(), plan);
    }
    Ok(plans)
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
                .map(|(path, _)| (path, PlannedRole::Parameter)),
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
                let assignment = validate_function_assignment_target(context, comp, *span)?;
                validate_function_expression_with_roles(
                    value,
                    context.roles,
                    context.flat,
                    context.shapes,
                )?;
                plans.push(FunctionStatementPlan::Assignment(assignment));
            }
            rumoca_core::Statement::For { .. } => {
                plans.push(validate_function_loop(statement, context)?)
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                span,
            } => plans.push(plan_function_conditional(
                cond_blocks,
                else_block.as_deref(),
                *span,
                context,
            )?),
            rumoca_core::Statement::FunctionCall {
                comp,
                args,
                outputs,
                span,
            } => plans.push(plan_function_multi_output_call(
                MultiOutputCallStatement {
                    callee: comp,
                    args,
                    outputs,
                    span: *span,
                },
                context,
            )?),
            _ => {
                let span =
                    required_statement_span(statement, "unsupported function body statement")?;
                return Err(ToDaeError::unsupported_flat(
                    "function statement",
                    format!(
                        "`{}` contains a statement without a checked DAE owner",
                        context.function.name
                    ),
                    span,
                ));
            }
        }
        index += 1;
    }
    coalesce_function_array_assemblies(statements, &mut plans, context)?;
    Ok(plans)
}

fn validate_function_assignment_target(
    context: FunctionValidationContext<'_>,
    component: &rumoca_core::ComponentReference,
    span: Span,
) -> Result<FunctionAssignmentPlan, ToDaeError> {
    let [target] = component.parts() else {
        return Err(ToDaeError::unsupported_flat(
            "function assignment target",
            "a mutable function value must have one resolved target part",
            span,
        ));
    };
    if !context
        .function
        .outputs
        .iter()
        .chain(&context.function.locals)
        .any(|value| value.name == target.ident)
    {
        return Err(ToDaeError::unsupported_flat(
            "function assignment target",
            format!(
                "`{}.{}` is not a whole mutable function value",
                context.function.name, target.ident
            ),
            span,
        ));
    }
    validate_function_subscripts(&target.subs, context)?;
    Ok(FunctionAssignmentPlan {
        target: VarName::new(&target.ident),
        subscripts: target.subs.clone().into_boxed_slice(),
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
    // A receiving variable takes one whole result. An element write would need
    // the aggregate seed MLS §12.4.4 definedness proves for the single-result
    // form, which this statement shape does not carry.
    if !plan.is_whole() {
        return Err(ToDaeError::unsupported_flat(
            "function call statement",
            format!(
                "receiving variable `{}` is subscripted; a multi-result call statement defines \
                 whole function values",
                plan.target()
            ),
            span,
        ));
    }
    // MLS §12.4.3: "Left-hand side references must agree with type of
    // corresponding output component" (SPEC_0022 FUNC-025). The proven result
    // shape is the one the constructed callee actually returns.
    let Some(declared) = context.shapes.get(plan.target()) else {
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
    if declared != result {
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
    for (statement, plan) in statements.iter().zip(plans.iter_mut()) {
        match (statement, plan) {
            (
                rumoca_core::Statement::Assignment { value, span, .. },
                FunctionStatementPlan::Assignment(assignment),
            ) => resolve_function_assignment_definition(
                value,
                *span,
                assignment,
                context,
                definitions,
            )?,
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
                *targets = resolve_function_conditional(
                    cond_blocks,
                    else_block.as_deref(),
                    branches,
                    fallback.as_mut(),
                    *span,
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
                rumoca_core::Statement::For { span, .. },
                FunctionStatementPlan::For {
                    lowering,
                    statements: body,
                    ..
                },
            ) => resolve_function_loop_definitions(lowering, body, context, definitions, *span)?,
            (
                rumoca_core::Statement::FunctionCall { args, span, .. },
                FunctionStatementPlan::MultiOutputCall { outputs },
            ) => {
                // MLS §11.2.1.1 evaluates the arguments, then assigns the
                // receiving variables left to right, so every argument is read
                // before any receiver is defined.
                for argument in args.iter() {
                    definitions.require_readable(argument, context, *span)?;
                }
                for plan in outputs.iter().flatten() {
                    definitions.define_whole(plan.target());
                }
            }
            (_, FunctionStatementPlan::ArrayAssembly(assembly)) => {
                definitions.define_whole(&assembly.target);
            }
            (_, FunctionStatementPlan::RecordAssembly(assembly)) => {
                definitions.define_whole(&assembly.target);
            }
            (_, FunctionStatementPlan::ArrayAssemblyMember)
            | (_, FunctionStatementPlan::RecordAssemblyMember) => {}
            _ => unreachable!("function planning aligns statement and plan shapes"),
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
    assignment.seed =
        definitions.write_elements(&assignment.target, &assignment.subscripts, context, span)?;
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
    lowering: &FunctionLoopLowering,
    body: &[FunctionStatementPlan],
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
    span: Span,
) -> Result<(), ToDaeError> {
    match lowering {
        FunctionLoopLowering::TotalArrayDefinition => {
            let [FunctionStatementPlan::Assignment(assignment)] = body else {
                unreachable!("a total array definition owns one element assignment")
            };
            definitions.define_whole(&assignment.target);
        }
        FunctionLoopLowering::Fold { targets } => {
            for target in targets {
                if !definitions.is_total(target) {
                    return Err(ToDaeError::unsupported_flat(
                        "function loop transition",
                        format!(
                            "`{}` carries `{target}` through a loop before every element of `{target}` has a definition",
                            context.function.name
                        ),
                        span,
                    ));
                }
            }
        }
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
