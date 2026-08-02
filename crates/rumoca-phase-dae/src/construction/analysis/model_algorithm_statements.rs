use super::*;

pub(super) fn validate_model_algorithm(
    algorithm: &flat::Algorithm,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, PeriodicClockSchedule)>,
) -> Result<(), ToDaeError> {
    require_span(algorithm.span, "model algorithm")?;
    validate_algorithm_statements(
        &algorithm.statements,
        roles,
        states,
        constants,
        sample_lattices,
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
    sample_lattices: &mut Vec<(Span, PeriodicClockSchedule)>,
) -> Result<(), ToDaeError> {
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, value, span } => {
                require_span(*span, "algorithm assignment")?;
                if comp.parts().is_empty() {
                    return Err(ToDaeError::unsupported_algorithm(
                        "model",
                        "empty assignment target",
                        *span,
                    ));
                }
                let target = rumoca_core::component_ref_to_base_reference(comp)
                    .var_name()
                    .clone();
                let target_role = roles.get(&target);
                for part in comp.parts() {
                    validate_subscripts_scoped(&part.subs, roles, states, &HashSet::new())?;
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
                    )?;
                }
                if let Some(statements) = else_block {
                    validate_algorithm_statements(
                        statements,
                        roles,
                        states,
                        constants,
                        sample_lattices,
                    )?;
                }
            }
            rumoca_core::Statement::For {
                indices,
                equations,
                span,
            } => {
                require_span(*span, "algorithm for statement")?;
                if indices.is_empty() {
                    return Err(ToDaeError::unsupported_algorithm(
                        "model",
                        "for statement must declare at least one index",
                        *span,
                    ));
                }
                let mut loop_roles = roles.clone();
                for index in indices {
                    validate_expression(&index.range, &loop_roles, states)?;
                    loop_roles.insert(VarName::new(&index.ident), PlannedRole::Parameter);
                }
                validate_algorithm_statements(
                    equations,
                    &loop_roles,
                    states,
                    constants,
                    sample_lattices,
                )?;
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
                if comp.parts().is_empty() || comp.parts().iter().any(|part| !part.subs.is_empty())
                {
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
                let span =
                    required_statement_span(statement, "unsupported model algorithm statement")?;
                return Err(ToDaeError::unsupported_algorithm(
                    "model",
                    "statement must be an assignment, function-call assignment, or conditional \
                     discrete update",
                    span,
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
        !output.parts().is_empty() && output.parts().iter().all(|part| part.subs.is_empty());
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
        output.span(),
    ))
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
