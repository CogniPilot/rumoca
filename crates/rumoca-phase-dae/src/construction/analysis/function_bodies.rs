use super::*;

pub(super) fn validate_functions(
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
        let plan = if let Some(plan) = validate_guarded_function_return(function, context)? {
            plan
        } else if let Some(plan) = validate_integer_reduction(function, context)? {
            plan
        } else {
            FunctionPlan::Statements {
                statements: validate_function_statements(&function.body, context)?,
            }
        };
        plans.insert(certificate.key.clone(), plan);
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

pub(super) fn validate_function_expression_with_roles(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    flat: &flat::Model,
) -> Result<(), ToDaeError> {
    validate_expression(expression, roles, &HashSet::new())?;
    validate_known_function_calls(expression, flat)
}

pub(super) fn validate_function_statements(
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
                validate_function_expression_with_roles(value, context.roles, context.flat)?;
                plans.push(FunctionStatementPlan::Assignment(assignment));
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
    })
}

pub(super) fn validate_function_subscripts(
    subscripts: &[Subscript],
    context: FunctionValidationContext<'_>,
) -> Result<(), ToDaeError> {
    validate_subscripts_scoped(subscripts, context.roles, &HashSet::new(), &HashSet::new())?;
    for subscript in subscripts {
        if let rumoca_core::Subscript::Expr { expr, .. } = subscript {
            validate_known_function_calls(expr, context.flat)?;
        }
    }
    Ok(())
}
