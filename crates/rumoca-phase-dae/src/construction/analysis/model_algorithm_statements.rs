use super::*;
use crate::construction::terminal_print::terminal_print_message;

pub(super) fn validate_model_algorithm(
    algorithm: &flat::Algorithm,
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    model_values: &ShapeEnvironment,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, PeriodicClockSchedule)>,
) -> Result<(), ToDaeError> {
    require_span(algorithm.span, "model algorithm")?;
    validate_algorithm_statements(
        &algorithm.statements,
        flat,
        roles,
        states,
        model_values,
        constants,
        sample_lattices,
    )?;
    reject_unrepresented_sequential_reads(
        &algorithm.statements,
        &mut HashSet::new(),
        &mut HashSet::new(),
        SequentialScope {
            event_guarded: false,
            later_writes: &HashSet::new(),
        },
    )
}

/// Where one statement sequence of a model algorithm stands.
#[derive(Clone, Copy)]
struct SequentialScope<'scope> {
    /// The sequence runs inside a `when` body.
    event_guarded: bool,
    /// Values some statement after this sequence, in an enclosing sequence,
    /// may still write.
    later_writes: &'scope HashSet<VarName>,
}

fn reject_unrepresented_sequential_reads(
    statements: &[rumoca_core::Statement],
    written: &mut HashSet<VarName>,
    unrepresented: &mut HashSet<VarName>,
    scope: SequentialScope<'_>,
) -> Result<(), ToDaeError> {
    for (index, statement) in statements.iter().enumerate() {
        let later_writes = || {
            let mut later = scope.later_writes.clone();
            collect_algorithm_writes(&statements[index + 1..], &mut later);
            later
        };
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
                    SequentialScope {
                        later_writes: &later_writes(),
                        ..scope
                    },
                )?;
            }
            rumoca_core::Statement::When { blocks, .. } => {
                reject_sequential_when(blocks, written, unrepresented, &later_writes())?;
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
    scope: SequentialScope<'_>,
) -> Result<(), ToDaeError> {
    let incoming = written.clone();
    let incoming_unrepresented = unrepresented.clone();
    let mut exits = Vec::with_capacity(blocks.len() + usize::from(fallback.is_some()));
    let mut unsupported_exits = Vec::with_capacity(exits.capacity());
    for block in blocks {
        let unavailable = if scope.event_guarded {
            &incoming_unrepresented
        } else {
            &incoming
        };
        reject_reads_of_written(&block.cond, unavailable)?;
        let mut branch = incoming.clone();
        let mut unsupported = incoming_unrepresented.clone();
        reject_unrepresented_sequential_reads(&block.stmts, &mut branch, &mut unsupported, scope)?;
        exits.push(branch);
        unsupported_exits.push(unsupported);
    }
    if let Some(fallback) = fallback {
        let mut branch = incoming;
        let mut unsupported = incoming_unrepresented;
        reject_unrepresented_sequential_reads(fallback, &mut branch, &mut unsupported, scope)?;
        exits.push(branch);
        unsupported_exits.push(unsupported);
    }
    merge_sequential_exits(written, unrepresented, exits, unsupported_exits);
    Ok(())
}

/// A `when` condition reads the algorithm's values where the statement
/// stands (MLS §11.1.2), and the checked condition reads each variable's
/// current value. Those agree for a variable no later statement writes: an
/// earlier write is then its final one, and a variable the algorithm never
/// writes is its own value. A value a later statement writes, or an earlier
/// value the `when` body itself writes again, lacks a checked owner for the
/// condition. A value only the `when` body writes is read the way the
/// equation form of the same `when` reads it, through event iteration.
///
/// A statement after the `when` reads a value its body writes the same way:
/// when no later statement writes it, the value after the `when` is the
/// value after the algorithm, which is the variable's current value. A value
/// a later statement writes again has no such owner where it is read.
fn reject_sequential_when(
    blocks: &[rumoca_core::StatementBlock],
    written: &mut HashSet<VarName>,
    unrepresented: &mut HashSet<VarName>,
    later_writes: &HashSet<VarName>,
) -> Result<(), ToDaeError> {
    let incoming = written.clone();
    let incoming_unrepresented = unrepresented.clone();
    let mut body_writes = HashSet::new();
    for block in blocks {
        collect_algorithm_writes(&block.stmts, &mut body_writes);
    }
    let unavailable = later_writes
        .iter()
        .chain(&overlapping_names(&incoming, &body_writes))
        .chain(&incoming_unrepresented)
        .cloned()
        .collect::<HashSet<_>>();
    let mut exits = Vec::with_capacity(blocks.len());
    let mut unsupported_exits = Vec::with_capacity(blocks.len());
    for block in blocks {
        reject_reads_of_written(&block.cond, &unavailable)?;
        let mut branch = incoming.clone();
        let mut unsupported = incoming_unrepresented.clone();
        reject_unrepresented_sequential_reads(
            &block.stmts,
            &mut branch,
            &mut unsupported,
            SequentialScope {
                event_guarded: true,
                later_writes,
            },
        )?;
        let mut block_writes = HashSet::new();
        collect_algorithm_writes(&block.stmts, &mut block_writes);
        unsupported.extend(overlapping_names(&block_writes, later_writes));
        exits.push(branch);
        unsupported_exits.push(unsupported);
    }
    merge_sequential_exits(written, unrepresented, exits, unsupported_exits);
    Ok(())
}

/// Whether two variable names share storage: they are equal, or one names a
/// field (or element) of the record (or array) the other names, as a whole
/// record write `r := R(1, 2)` writes `r.a` and `r.b`.
pub(in crate::construction) fn names_overlap(lhs: &VarName, rhs: &VarName) -> bool {
    let (lhs, rhs) = (lhs.as_str(), rhs.as_str());
    lhs == rhs || is_component_of(lhs, rhs) || is_component_of(rhs, lhs)
}

fn is_component_of(name: &str, whole: &str) -> bool {
    name.strip_prefix(whole)
        .is_some_and(|rest| rest.starts_with('.') || rest.starts_with('['))
}

/// The storage two name sets share, as the more specific name of each
/// overlapping pair.
fn overlapping_names(lhs: &HashSet<VarName>, rhs: &HashSet<VarName>) -> HashSet<VarName> {
    let mut shared = HashSet::new();
    for left in lhs {
        for right in rhs.iter().filter(|right| names_overlap(left, right)) {
            let specific = if left.as_str().len() >= right.as_str().len() {
                left
            } else {
                right
            };
            shared.insert(specific.clone());
        }
    }
    shared
}

/// Every variable a statement sequence may write, at any nesting depth.
pub(in crate::construction) fn collect_algorithm_writes(
    statements: &[rumoca_core::Statement],
    writes: &mut HashSet<VarName>,
) {
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, .. } => {
                writes.insert(
                    rumoca_core::component_ref_to_base_reference(comp)
                        .var_name()
                        .clone(),
                );
            }
            rumoca_core::Statement::FunctionCall { outputs, .. } => {
                writes.extend(outputs.iter().flatten().map(|output| output.to_var_name()));
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    collect_algorithm_writes(&block.stmts, writes);
                }
                if let Some(block) = else_block {
                    collect_algorithm_writes(block, writes);
                }
            }
            rumoca_core::Statement::When { blocks, .. } => {
                for block in blocks {
                    collect_algorithm_writes(&block.stmts, writes);
                }
            }
            rumoca_core::Statement::For { equations, .. } => {
                collect_algorithm_writes(equations, writes);
            }
            rumoca_core::Statement::While { block, .. } => {
                collect_algorithm_writes(&block.stmts, writes);
            }
            _ => {}
        }
    }
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
        .find(|target| written.iter().any(|name| names_overlap(name, target)))
    else {
        return Ok(());
    };
    Err(ToDaeError::unsupported_algorithm(
        "model",
        format!(
            "sequential read of `{target}`, whose value where it is read differs from its value after the algorithm, requires an SSA event transition"
        ),
        expression_span(expression)?,
    ))
}

// SPEC_0021 exception: exhaustive statement-grammar validation keeps each
// accepted form and its provenance checks visible at one boundary.
// SPEC_0021: Exception - cohesive exhaustive flow stays contiguous so ordering remains auditable.
#[allow(clippy::too_many_lines)]
fn validate_algorithm_statements(
    statements: &[rumoca_core::Statement],
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    model_values: &ShapeEnvironment,
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
                ) || (matches!(target_role, Some(PlannedRole::Aggregate))
                    && is_direct_record_call_assignment(comp, value))
                {
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
                        flat,
                        roles,
                        states,
                        model_values,
                        constants,
                        sample_lattices,
                    )?;
                }
                if let Some(statements) = else_block {
                    validate_algorithm_statements(
                        statements,
                        flat,
                        roles,
                        states,
                        model_values,
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
                    validate_model_algorithm_range(
                        &index.range,
                        &loop_roles,
                        states,
                        model_values,
                    )?;
                    loop_roles.insert(VarName::new(&index.ident), PlannedRole::Parameter);
                }
                validate_algorithm_statements(
                    equations,
                    flat,
                    &loop_roles,
                    states,
                    model_values,
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
                        flat,
                        roles,
                        states,
                        model_values,
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
                if (outputs.is_empty() || outputs.iter().all(Option::is_none))
                    && terminal_print_message(flat, comp, args).is_none()
                {
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
            rumoca_core::Statement::Assert {
                condition,
                message,
                level,
                span,
            } => {
                require_span(*span, "algorithm assertion")?;
                validate_expression(condition, roles, states)?;
                validate_expression(message, roles, states)?;
                if let Some(level) = level {
                    validate_expression(level, roles, states)?;
                }
            }
            _ => {
                let span =
                    required_statement_span(statement, "unsupported model algorithm statement")?;
                return Err(ToDaeError::unsupported_algorithm(
                    "model",
                    "statement must be an assignment, assertion, function-call assignment, or \
                     conditional discrete update",
                    span,
                ));
            }
        }
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

fn structured_assignment_pairs(
    target: &VarName,
    value: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
) -> Option<Vec<VarName>> {
    let pairs = structured_assignment_names(target, value, roles.keys())?;
    let pairs = pairs
        .into_iter()
        .map(|(target_leaf, _)| target_leaf)
        .collect::<Vec<_>>();
    pairs
        .iter()
        .all(|target_leaf| {
            matches!(
                roles.get(target_leaf),
                Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
            )
        })
        .then_some(pairs)
}
