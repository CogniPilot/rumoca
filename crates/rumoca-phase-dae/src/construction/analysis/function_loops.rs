use super::*;

pub(super) fn validate_function_loop(
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
    /// The specialization environment with this nest's binders bound.
    ///
    /// MLS §11.2.2 makes a for-index a fresh scalar of the loop, so it shadows
    /// any enclosing coordinate of the same flat name. Binding it here — with a
    /// shape and *no* proven value — is what stops an inner bound written over
    /// the binder from folding an outer coordinate's value, and it is the same
    /// environment the lowering builds, so the analysis that admits a bound and
    /// the lowering that folds it read one scope.
    shapes: ShapeEnvironment,
}

fn validate_function_loop_domain(
    indices: &[rumoca_core::ForIndex],
    span: Span,
    context: FunctionValidationContext<'_>,
) -> Result<ValidatedFunctionLoop, ToDaeError> {
    let mut loop_roles = context.roles.clone();
    let mut loop_shapes = context.shapes.clone();
    let mut binders = Vec::with_capacity(indices.len());
    let mut binder_spans = Vec::with_capacity(indices.len());
    for (ordinal, index) in indices.iter().enumerate() {
        let range_span = expression_span(&index.range)?;
        // Each index is proven in the scope of the ones before it: MLS §11.2.2
        // opens the binders left to right, so `for i in 1:n, j in 1:i` reads `j`
        // in a scope where `i` is already a binder rather than an outer value.
        validate_function_range_expression(
            &index.range,
            context.roles,
            context.flat,
            &loop_shapes,
        )?;
        let Some((lower, step, upper)) =
            static_function_range(&index.range, context.static_integers, &loop_shapes)?
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
        loop_shapes.insert(VarName::new(&index.ident), Vec::new());
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
        shapes: loop_shapes,
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
        shapes: &validated.shapes,
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
        &validated.shapes,
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
        shapes: &validated.shapes,
        ..context
    };
    let statements = plan_function_statements(equations, body_context)?;
    // A compact loop transition owns one assignment per carried value. A nested
    // loop or conditional inside the body would need its own owner, which the
    // fold has no place for, so reject it here instead of at lowering.
    for (statement, plan) in equations.iter().zip(&statements) {
        if matches!(plan, FunctionStatementPlan::Assignment(_)) {
            continue;
        }
        let statement_span = required_statement_span(statement, "function loop body statement")?;
        return Err(ToDaeError::unsupported_flat(
            "function loop transition",
            format!(
                "`{}` requires direct value assignments in a loop body",
                context.function.name
            ),
            statement_span,
        ));
    }
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
        &validated.shapes,
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
    shapes: &ShapeEnvironment,
) -> FunctionLoopLowering {
    let targets = function_loop_targets(plans);
    let (
        [rumoca_core::Statement::Assignment { value, .. }],
        [FunctionStatementPlan::Assignment(assignment)],
    ) = (statements, plans)
    else {
        return FunctionLoopLowering::Fold { targets };
    };
    let target = assignment.target();
    if !function
        .outputs
        .iter()
        .chain(&function.locals)
        .any(|declaration| declaration.name == target.as_str())
    {
        return FunctionLoopLowering::Fold { targets };
    }
    let Ok(extents) = domain.extents() else {
        return FunctionLoopLowering::Fold { targets };
    };
    let Some(shape) = shapes.get(target) else {
        return FunctionLoopLowering::Fold { targets };
    };
    let dimensions = shape
        .iter()
        .map(|dimension| usize::try_from(*dimension))
        .collect::<Result<Vec<_>, _>>();
    let subscripts = assignment.subscripts();
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
    let exact_subscripts = subscripts.len() == indices.len()
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
        FunctionLoopLowering::TotalArrayDefinition
    } else {
        FunctionLoopLowering::Fold { targets }
    }
}

pub(super) fn subscript_is_binder(subscript: &rumoca_core::Subscript, binder: &str) -> bool {
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

fn function_loop_targets(plans: &[FunctionStatementPlan]) -> Vec<VarName> {
    let mut targets = Vec::new();
    for plan in plans {
        let FunctionStatementPlan::Assignment(assignment) = plan else {
            continue;
        };
        let target = assignment.target();
        if !targets.contains(target) {
            targets.push(target.clone());
        }
    }
    targets
}
