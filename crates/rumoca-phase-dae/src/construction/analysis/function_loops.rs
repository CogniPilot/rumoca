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
    validate_function_loop_body(indices, equations, context, validated)
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
        let binder = VarName::new(&index.ident);
        loop_roles.insert(binder.clone(), PlannedRole::Parameter);
        loop_shapes.bind_integer_bounds(binder, lower.min(upper), lower.max(upper));
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
        // A non-perfect nest is a sequence inside the outer transition. Keep
        // the child as its own lexically nested fold; only a perfect nest can
        // collapse to one Cartesian product domain.
        return Ok(None);
    };
    let nested_context = FunctionValidationContext {
        roles: &validated.roles,
        shapes: &validated.shapes,
        call_scoped_actions: false,
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
    context: FunctionValidationContext<'_>,
    validated: ValidatedFunctionLoop,
) -> Result<FunctionStatementPlan, ToDaeError> {
    let body_context = FunctionValidationContext {
        roles: &validated.roles,
        shapes: &validated.shapes,
        call_scoped_actions: true,
        ..context
    };
    let statements = plan_function_statements(equations, body_context)?;
    // A compact loop transition owns one assignment per carried value. A nested
    // loop or conditional inside the body would need its own owner, which the
    // fold has no place for, so reject it here instead of at lowering.
    for (statement, plan) in equations.iter().zip(&statements) {
        if matches!(
            plan,
            FunctionStatementPlan::Assignment(_)
                | FunctionStatementPlan::RecordAssembly(_)
                | FunctionStatementPlan::RecordAssemblyMember
                | FunctionStatementPlan::ProvenAssertion
                | FunctionStatementPlan::RuntimeAssertion
                | FunctionStatementPlan::MultiOutputCall { .. }
                | FunctionStatementPlan::For { .. }
                | FunctionStatementPlan::If { .. }
                | FunctionStatementPlan::ProvenBranch { .. }
        ) {
            continue;
        }
        let statement_span = required_statement_span(statement, "function loop body statement")?;
        return Err(ToDaeError::unsupported_flat(
            "function loop transition",
            format!(
                "`{}` requires direct value assignments or assertions in a loop body",
                context.function.name
            ),
            statement_span,
        ));
    }
    // An assertion-only loop has no value transition, but it still owns one
    // checked call-scoped action per domain point. The DAE fold represents
    // that exact zero-carried transition; requiring a target here would reject
    // the valid result of substituting an unobserved loop-local scratch value
    // into its trailing assertion.
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
    if loop_is_ordered_total_array_definitions(
        domain, indices, statements, plans, function, shapes, &targets,
    ) {
        FunctionLoopLowering::TotalArrayDefinition
    } else {
        FunctionLoopLowering::Fold { targets }
    }
}

fn loop_is_ordered_total_array_definitions(
    domain: &StructuredIndexDomain,
    indices: &[&rumoca_core::ForIndex],
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
    function: &rumoca_core::Function,
    shapes: &ShapeEnvironment,
    targets: &[VarName],
) -> bool {
    let assignment_count = plans
        .iter()
        .filter(|plan| matches!(plan, FunctionStatementPlan::Assignment(_)))
        .count();
    if assignment_count == 0 || targets.len() != assignment_count {
        return false;
    }
    let mut defined_targets = Vec::with_capacity(targets.len());
    statements.iter().zip(plans).all(|(statement, plan)| {
        if matches!(plan, FunctionStatementPlan::ProvenAssertion) {
            return assertion_expressions(statement)
                .first()
                .is_some_and(|condition| {
                    expression_reads_only_prior_loop_elements(condition, targets, &[], indices)
                });
        }
        if matches!(plan, FunctionStatementPlan::RuntimeAssertion) {
            return assertion_expressions(statement).iter().all(|expression| {
                expression_reads_only_prior_loop_elements(expression, targets, &[], indices)
            });
        }
        let (
            rumoca_core::Statement::Assignment { value, .. },
            FunctionStatementPlan::Assignment(assignment),
        ) = (statement, plan)
        else {
            return false;
        };
        let declared = declared_function_value(function, assignment.target());
        let covers = assignment_covers_loop_domain(domain, indices, assignment, shapes);
        let dependencies =
            expression_reads_only_prior_loop_elements(value, targets, &defined_targets, indices);
        let valid = declared && covers && dependencies;
        defined_targets.push(assignment.target().clone());
        valid
    })
}

fn assertion_expressions(statement: &rumoca_core::Statement) -> Vec<&Expression> {
    match statement {
        rumoca_core::Statement::Assert {
            condition, message, ..
        } => vec![condition, message],
        rumoca_core::Statement::FunctionCall { args, .. } => args.iter().take(2).collect(),
        _ => Vec::new(),
    }
}

fn declared_function_value(function: &rumoca_core::Function, target: &VarName) -> bool {
    function
        .outputs
        .iter()
        .chain(&function.locals)
        .any(|declaration| declaration.name == target.as_str())
}

fn assignment_covers_loop_domain(
    domain: &StructuredIndexDomain,
    indices: &[&rumoca_core::ForIndex],
    assignment: &FunctionAssignmentPlan,
    shapes: &ShapeEnvironment,
) -> bool {
    let Ok(extents) = domain.extents() else {
        return false;
    };
    let Some(shape) = shapes.get(assignment.target()) else {
        return false;
    };
    let dimensions = shape
        .iter()
        .map(|dimension| usize::try_from(*dimension))
        .collect::<Result<Vec<_>, _>>();
    let subscripts = assignment.subscripts();
    dimensions.as_ref().is_ok_and(|dimensions| {
        dimensions.get(..extents.len()) == Some(extents.as_slice())
            && domain
                .binders
                .iter()
                .zip(&extents)
                .all(|(binder, dimension)| {
                    binder.lower == 1
                        && binder.step == 1
                        && usize::try_from(binder.upper).ok() == Some(*dimension)
                })
            && subscripts.len() == dimensions.len()
            && subscripts
                .iter()
                .take(indices.len())
                .zip(indices)
                .all(|(subscript, index)| subscript_is_binder(subscript, &index.ident))
            && subscripts
                .iter()
                .skip(indices.len())
                .all(|subscript| matches!(subscript, Subscript::Colon { .. }))
    })
}

fn expression_reads_only_prior_loop_elements(
    value: &Expression,
    targets: &[VarName],
    defined_targets: &[VarName],
    indices: &[&rumoca_core::ForIndex],
) -> bool {
    struct LoopDependencyProof<'a> {
        targets: &'a [VarName],
        defined_targets: &'a [VarName],
        indices: &'a [&'a rumoca_core::ForIndex],
        valid: bool,
    }

    impl LoopDependencyProof<'_> {
        fn prove_target_element(
            &mut self,
            target: &VarName,
            subscripts: &[rumoca_core::Subscript],
        ) {
            self.valid &= self.defined_targets.contains(target)
                && subscripts.len() == self.indices.len()
                && subscripts
                    .iter()
                    .zip(self.indices)
                    .all(|(subscript, index)| subscript_is_binder(subscript, &index.ident));
        }

        fn visit_subscripts(&mut self, subscripts: &[rumoca_core::Subscript]) {
            for subscript in subscripts {
                rumoca_core::ExpressionVisitor::visit_subscript(self, subscript);
            }
        }
    }

    impl rumoca_core::ExpressionVisitor for LoopDependencyProof<'_> {
        fn visit_var_ref(
            &mut self,
            name: &rumoca_core::Reference,
            subscripts: &[rumoca_core::Subscript],
        ) {
            let target = name.var_name();
            if self.targets.contains(target) {
                self.prove_target_element(target, subscripts);
            }
            self.walk_var_ref(name, subscripts);
        }

        fn visit_index(&mut self, base: &Expression, subscripts: &[rumoca_core::Subscript]) {
            if let Expression::VarRef {
                name,
                subscripts: base_subscripts,
                ..
            } = base
                && base_subscripts.is_empty()
                && self.targets.contains(name.var_name())
            {
                self.prove_target_element(name.var_name(), subscripts);
                self.visit_subscripts(subscripts);
            } else {
                self.visit_expression(base);
                self.visit_subscripts(subscripts);
            }
        }
    }

    let mut proof = LoopDependencyProof {
        targets,
        defined_targets,
        indices,
        valid: true,
    };
    rumoca_core::ExpressionVisitor::visit_expression(&mut proof, value);
    proof.valid
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

pub(super) fn flattened_function_loop_source<'statement>(
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
        match plan {
            FunctionStatementPlan::Assignment(assignment) => {
                extend_unique_targets(&mut targets, [assignment.target().clone()]);
            }
            FunctionStatementPlan::RecordAssembly(assembly) => {
                extend_unique_targets(&mut targets, [assembly.target.clone()]);
            }
            FunctionStatementPlan::MultiOutputCall { outputs } => {
                extend_unique_targets(
                    &mut targets,
                    outputs
                        .iter()
                        .flatten()
                        .map(|output| output.target().clone()),
                );
            }
            FunctionStatementPlan::If {
                branches, fallback, ..
            } => {
                let mut branch_targets = branches
                    .iter()
                    .flat_map(|branch| function_loop_targets(branch))
                    .collect::<Vec<_>>();
                if let Some(fallback) = fallback {
                    branch_targets.extend(function_loop_targets(fallback));
                }
                extend_unique_targets(&mut targets, branch_targets);
            }
            FunctionStatementPlan::ProvenBranch { statements, .. } => {
                extend_unique_targets(&mut targets, function_loop_targets(statements));
            }
            FunctionStatementPlan::For { statements, .. } => {
                extend_unique_targets(&mut targets, function_loop_targets(statements));
            }
            _ => {}
        }
    }
    targets
}

fn extend_unique_targets(targets: &mut Vec<VarName>, additions: impl IntoIterator<Item = VarName>) {
    for target in additions {
        if !targets.contains(&target) {
            targets.push(target);
        }
    }
}
