use super::*;

pub(super) fn validate_model_algorithm(
    flat: &flat::Model,
    algorithm: &flat::Algorithm,
    roles: &HashMap<VarName, PlannedRole>,
    model_values: &ShapeEnvironment,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, PeriodicClockSchedule)>,
) -> Result<(), ToDaeError> {
    require_span(algorithm.span, "model algorithm")?;
    validate_algorithm_statements(
        &algorithm.statements,
        ModelAlgorithmScope {
            flat,
            roles,
            model_values,
            constants,
        },
        sample_lattices,
    )?;
    reject_unrepresented_sequential_reads(
        &algorithm.statements,
        &mut HashSet::new(),
        &mut HashSet::new(),
        false,
    )
}

fn reject_unrepresented_sequential_reads(
    statements: &[rumoca_core::Statement],
    written: &mut HashSet<VarName>,
    unrepresented: &mut HashSet<VarName>,
    event_guarded: bool,
) -> Result<(), ToDaeError> {
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, value, span } => {
                reject_sequential_assignment(comp, value, *span, written, unrepresented)?;
            }
            rumoca_core::Statement::FunctionCall {
                args,
                outputs,
                span,
                ..
            } => {
                reject_sequential_call(args, outputs, *span, written, unrepresented)?;
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                reject_sequential_if(
                    cond_blocks,
                    else_block.as_deref(),
                    written,
                    unrepresented,
                    event_guarded,
                )?;
            }
            rumoca_core::Statement::When { blocks, .. } => {
                reject_sequential_when(blocks, written, unrepresented)?;
            }
            rumoca_core::Statement::For {
                indices, equations, ..
            } => {
                reject_sequential_tensor_loop(indices, equations, written, unrepresented)?;
            }
            rumoca_core::Statement::Assert {
                condition,
                message,
                level,
                ..
            } => {
                reject_reads_of_written(condition, written)?;
                reject_reads_of_written(message, written)?;
                if let Some(level) = level {
                    reject_reads_of_written(level, written)?;
                }
            }
            _ => {}
        }
    }
    Ok(())
}

fn reject_sequential_assignment(
    component: &rumoca_core::ComponentReference,
    value: &Expression,
    span: Span,
    written: &mut HashSet<VarName>,
    unrepresented: &HashSet<VarName>,
) -> Result<(), ToDaeError> {
    reject_reads_of_written(value, unrepresented)?;
    for expression in component
        .parts()
        .iter()
        .flat_map(|part| &part.subs)
        .filter_map(|subscript| match subscript {
            Subscript::Expr { expr, .. } => Some(expr.as_ref()),
            _ => None,
        })
    {
        reject_reads_of_written(expression, written)?;
    }
    written.insert(
        rumoca_core::component_ref_to_base_reference(component)
            .var_name()
            .clone(),
    );
    require_span(span, "sequential algorithm assignment")
}

fn reject_sequential_call(
    arguments: &[Expression],
    outputs: &[Option<rumoca_core::ComponentReference>],
    span: Span,
    written: &mut HashSet<VarName>,
    unrepresented: &HashSet<VarName>,
) -> Result<(), ToDaeError> {
    for argument in arguments {
        reject_reads_of_written(argument, unrepresented)?;
    }
    written.extend(outputs.iter().flatten().map(|output| output.to_var_name()));
    require_span(span, "sequential algorithm function call")
}

fn reject_sequential_if(
    blocks: &[rumoca_core::StatementBlock],
    fallback: Option<&[rumoca_core::Statement]>,
    written: &mut HashSet<VarName>,
    unrepresented: &mut HashSet<VarName>,
    event_guarded: bool,
) -> Result<(), ToDaeError> {
    let incoming = written.clone();
    let incoming_unrepresented = unrepresented.clone();
    let mut exits = Vec::with_capacity(blocks.len() + usize::from(fallback.is_some()));
    let mut unsupported_exits = Vec::with_capacity(exits.capacity());
    for block in blocks {
        let unavailable = if event_guarded {
            &incoming_unrepresented
        } else {
            &incoming
        };
        reject_reads_of_written(&block.cond, unavailable)?;
        let mut branch = incoming.clone();
        let mut unsupported = incoming_unrepresented.clone();
        reject_unrepresented_sequential_reads(
            &block.stmts,
            &mut branch,
            &mut unsupported,
            event_guarded,
        )?;
        exits.push(branch);
        unsupported_exits.push(unsupported);
    }
    if let Some(fallback) = fallback {
        let mut branch = incoming;
        let mut unsupported = incoming_unrepresented;
        reject_unrepresented_sequential_reads(
            fallback,
            &mut branch,
            &mut unsupported,
            event_guarded,
        )?;
        exits.push(branch);
        unsupported_exits.push(unsupported);
    }
    merge_sequential_exits(written, unrepresented, exits, unsupported_exits);
    Ok(())
}

fn reject_sequential_when(
    blocks: &[rumoca_core::StatementBlock],
    written: &mut HashSet<VarName>,
    unrepresented: &mut HashSet<VarName>,
) -> Result<(), ToDaeError> {
    let incoming = written.clone();
    let incoming_unrepresented = unrepresented.clone();
    let mut exits = Vec::with_capacity(blocks.len());
    let mut unsupported_exits = Vec::with_capacity(blocks.len());
    for block in blocks {
        reject_reads_of_written(&block.cond, &incoming)?;
        let mut branch = incoming.clone();
        let mut unsupported = incoming_unrepresented.clone();
        reject_unrepresented_sequential_reads(&block.stmts, &mut branch, &mut unsupported, true)?;
        unsupported.extend(branch.difference(&incoming).cloned());
        exits.push(branch);
        unsupported_exits.push(unsupported);
    }
    merge_sequential_exits(written, unrepresented, exits, unsupported_exits);
    Ok(())
}

fn merge_sequential_exits(
    written: &mut HashSet<VarName>,
    unrepresented: &mut HashSet<VarName>,
    exits: Vec<HashSet<VarName>>,
    unsupported_exits: Vec<HashSet<VarName>>,
) {
    written.extend(exits.into_iter().flatten());
    unrepresented.extend(unsupported_exits.into_iter().flatten());
}

fn reject_sequential_tensor_loop(
    indices: &[rumoca_core::ForIndex],
    statements: &[rumoca_core::Statement],
    written: &mut HashSet<VarName>,
    unrepresented: &HashSet<VarName>,
) -> Result<(), ToDaeError> {
    for index in indices {
        reject_reads_of_written(&index.range, written)?;
    }
    for statement in statements {
        let rumoca_core::Statement::Assignment { comp, value, .. } = statement else {
            continue;
        };
        reject_reads_of_written(value, unrepresented)?;
        written.insert(
            rumoca_core::component_ref_to_base_reference(comp)
                .var_name()
                .clone(),
        );
    }
    Ok(())
}

fn reject_reads_of_written(
    expression: &Expression,
    written: &HashSet<VarName>,
) -> Result<(), ToDaeError> {
    let mut references = Vec::new();
    expression.collect_var_refs(&mut references);
    let Some(target) = references
        .into_iter()
        .find(|target| written.contains(target))
    else {
        return Ok(());
    };
    Err(ToDaeError::unsupported_algorithm(
        "model",
        format!(
            "sequential read of `{target}` after an earlier write requires an SSA event transition"
        ),
        expression_span(expression)?,
    ))
}

/// The facts a model algorithm statement is validated against: the planned
/// role of every model coordinate, the shape environment, and the constant
/// evaluation context.
///
/// A `for` statement validates its body under an extended role map, so the
/// scope is a copyable view that can be rebound for one nested body rather
/// than four arguments threaded through every statement form.
#[derive(Clone, Copy)]
struct ModelAlgorithmScope<'a> {
    flat: &'a flat::Model,
    roles: &'a HashMap<VarName, PlannedRole>,
    model_values: &'a ShapeEnvironment,
    constants: &'a EvalContext,
}

fn validate_algorithm_statements(
    statements: &[rumoca_core::Statement],
    scope: ModelAlgorithmScope<'_>,
    sample_lattices: &mut Vec<(Span, PeriodicClockSchedule)>,
) -> Result<(), ToDaeError> {
    for statement in statements {
        validate_algorithm_statement(statement, scope, sample_lattices)?;
    }
    Ok(())
}

/// The accepted model algorithm statement grammar. Every form the DAE
/// construction admits has one arm here and one validator below; anything else
/// is refused with its own span.
fn validate_algorithm_statement(
    statement: &rumoca_core::Statement,
    scope: ModelAlgorithmScope<'_>,
    sample_lattices: &mut Vec<(Span, PeriodicClockSchedule)>,
) -> Result<(), ToDaeError> {
    match statement {
        rumoca_core::Statement::Assignment { comp, value, span } => {
            validate_algorithm_assignment(comp, value, *span, scope)
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            span,
        } => validate_algorithm_if(
            cond_blocks,
            else_block.as_deref(),
            *span,
            scope,
            sample_lattices,
        ),
        rumoca_core::Statement::For {
            indices,
            equations,
            span,
        } => validate_algorithm_for(indices, equations, *span, scope, sample_lattices),
        rumoca_core::Statement::When { blocks, span } => {
            validate_algorithm_when(blocks, *span, scope, sample_lattices)
        }
        rumoca_core::Statement::FunctionCall {
            comp,
            args,
            outputs,
            span,
        } => validate_algorithm_function_call(comp, args, outputs, *span, scope),
        rumoca_core::Statement::Assert {
            condition,
            message,
            level,
            span,
        } => validate_algorithm_assert(condition, message, level.as_deref(), *span, scope),
        _ => {
            let span = required_statement_span(statement, "unsupported model algorithm statement")?;
            Err(ToDaeError::unsupported_algorithm(
                "model",
                "statement must be an assignment, assertion, function-call assignment, or \
                 conditional discrete update",
                span,
            ))
        }
    }
}

/// An assignment must name a whole writable coordinate, or decompose into
/// structured discrete leaves that are each writable.
fn validate_algorithm_assignment(
    comp: &rumoca_core::ComponentReference,
    value: &Expression,
    span: Span,
    scope: ModelAlgorithmScope<'_>,
) -> Result<(), ToDaeError> {
    require_span(span, "algorithm assignment")?;
    if comp.parts().is_empty() {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            "empty assignment target",
            span,
        ));
    }
    let target = rumoca_core::component_ref_to_base_reference(comp)
        .var_name()
        .clone();
    let target_role = scope.roles.get(&target);
    for part in comp.parts() {
        validate_subscripts_scoped(&part.subs, scope.roles, &HashSet::new())?;
    }
    if matches!(
        target_role,
        Some(
            PlannedRole::Algebraic
                | PlannedRole::Output
                | PlannedRole::DiscreteReal
                | PlannedRole::DiscreteValue
        )
    ) || (matches!(target_role, Some(PlannedRole::Aggregate))
        && is_direct_record_call_assignment(comp, value))
    {
        validate_expression(value, scope.roles)?;
    } else if structured_assignment_plan(scope.flat, comp, value)
        .as_ref()
        .is_none_or(|plan| !structured_assignment_targets_are_writable(plan, scope.roles))
    {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            format!(
                "algorithm assignment target `{target}` is not a whole writable \
                 coordinate (resolved role: {target_role:?})"
            ),
            span,
        ));
    }
    Ok(())
}

fn validate_algorithm_if(
    cond_blocks: &[rumoca_core::StatementBlock],
    else_block: Option<&[rumoca_core::Statement]>,
    span: Span,
    scope: ModelAlgorithmScope<'_>,
    sample_lattices: &mut Vec<(Span, PeriodicClockSchedule)>,
) -> Result<(), ToDaeError> {
    require_span(span, "algorithm if statement")?;
    for block in cond_blocks {
        validate_algorithm_condition(&block.cond, scope.roles, scope.constants, sample_lattices)?;
        validate_algorithm_statements(&block.stmts, scope, sample_lattices)?;
    }
    if let Some(statements) = else_block {
        validate_algorithm_statements(statements, scope, sample_lattices)?;
    }
    Ok(())
}

/// The body of a `for` is validated with its loop indices bound as parameters,
/// so the extended role map is the only part of the scope that changes.
fn validate_algorithm_for(
    indices: &[rumoca_core::ForIndex],
    equations: &[rumoca_core::Statement],
    span: Span,
    scope: ModelAlgorithmScope<'_>,
    sample_lattices: &mut Vec<(Span, PeriodicClockSchedule)>,
) -> Result<(), ToDaeError> {
    require_span(span, "algorithm for statement")?;
    if indices.is_empty() {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            "for statement must declare at least one index",
            span,
        ));
    }
    let mut loop_roles = scope.roles.clone();
    for index in indices {
        validate_model_algorithm_range(&index.range, &loop_roles, scope.model_values)?;
        loop_roles.insert(VarName::new(&index.ident), PlannedRole::Parameter);
    }
    let loop_scope = ModelAlgorithmScope {
        roles: &loop_roles,
        ..scope
    };
    validate_algorithm_statements(equations, loop_scope, sample_lattices)
}

fn validate_algorithm_when(
    blocks: &[rumoca_core::StatementBlock],
    span: Span,
    scope: ModelAlgorithmScope<'_>,
    sample_lattices: &mut Vec<(Span, PeriodicClockSchedule)>,
) -> Result<(), ToDaeError> {
    require_span(span, "algorithm when statement")?;
    if blocks.is_empty() {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            "when statement must contain at least one guarded block",
            span,
        ));
    }
    for block in blocks {
        validate_algorithm_condition(&block.cond, scope.roles, scope.constants, sample_lattices)?;
        validate_algorithm_statements(&block.stmts, scope, sample_lattices)?;
    }
    Ok(())
}

fn validate_algorithm_function_call(
    comp: &rumoca_core::Reference,
    args: &[Expression],
    outputs: &[Option<rumoca_core::ComponentReference>],
    span: Span,
    scope: ModelAlgorithmScope<'_>,
) -> Result<(), ToDaeError> {
    require_span(span, "algorithm function-call assignment")?;
    if comp.parts().is_empty() || comp.parts().iter().any(|part| !part.subs.is_empty()) {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            "function-call assignment requires one resolved, unsubscripted function",
            span,
        ));
    }
    if outputs.is_empty() || outputs.iter().all(Option::is_none) {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            "function-call assignment must retain at least one output",
            span,
        ));
    }
    for argument in args {
        validate_expression(argument, scope.roles)?;
    }
    for output in outputs.iter().flatten() {
        validate_function_call_output(output, scope.roles)?;
    }
    Ok(())
}

fn validate_algorithm_assert(
    condition: &Expression,
    message: &Expression,
    level: Option<&Expression>,
    span: Span,
    scope: ModelAlgorithmScope<'_>,
) -> Result<(), ToDaeError> {
    require_span(span, "algorithm assertion")?;
    validate_expression(condition, scope.roles)?;
    validate_expression(message, scope.roles)?;
    if let Some(level) = level {
        validate_expression(level, scope.roles)?;
    }
    Ok(())
}

fn is_direct_record_call_assignment(
    target: &rumoca_core::ComponentReference,
    value: &Expression,
) -> bool {
    target.parts().iter().all(|part| part.subs.is_empty())
        && matches!(
            value,
            Expression::FunctionCall {
                is_constructor: false,
                call_kind: rumoca_core::FunctionCallKind::Invocation,
                ..
            }
        )
}

fn validate_function_call_output(
    output: &rumoca_core::ComponentReference,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<(), ToDaeError> {
    let target = output.to_var_name();
    let is_whole_coordinate =
        !output.parts().is_empty() && output.parts().iter().all(|part| part.subs.is_empty());
    let is_writable = matches!(
        roles.get(&target),
        Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue | PlannedRole::Aggregate)
    );
    if is_whole_coordinate && is_writable {
        return Ok(());
    }
    Err(ToDaeError::unsupported_algorithm(
        "model",
        format!("function-call output `{target}` is not a whole discrete coordinate or record"),
        output.span(),
    ))
}

fn structured_assignment_targets_are_writable(
    plan: &StructuredAssignmentPlan,
    roles: &HashMap<VarName, PlannedRole>,
) -> bool {
    plan.pairs.iter().all(|(target_leaf, _)| {
        matches!(
            roles.get(&target_leaf.name),
            Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
        )
    })
}
