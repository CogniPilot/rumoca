use super::*;

#[derive(Default)]
struct ForLoopLowerState {
    assignments: IndexMap<VarName, Expression>,
    known_targets: HashSet<VarName>,
    touched_targets: IndexSet<VarName>,
    break_condition: Option<Expression>,
}

fn loop_active_guard(base_guard: Expression, break_condition: &Option<Expression>) -> Expression {
    if let Some(break_expr) = break_condition {
        and_expr(base_guard, not_expr(break_expr.clone()))
    } else {
        base_guard
    }
}

fn apply_guarded_loop_assignment(
    dae: &Dae,
    state: &mut ForLoopLowerState,
    target: VarName,
    value: Expression,
    guard: Expression,
) -> Result<(), String> {
    let rewritten =
        rewrite_algorithm_current_refs(dae, &value, &state.assignments, &state.known_targets);
    let fallback = state
        .assignments
        .shift_remove(&target)
        .unwrap_or_else(|| algorithm_if_fallback_expr(dae, &target));
    let merged = guarded_expr(guard, rewritten, fallback);
    if let Some(error) = expression_budget_error(&target, &merged) {
        return Err(error);
    }
    let normalized = normalize_algorithm_current_value(dae, &target, &merged);
    state.touched_targets.insert(target.clone());
    state.assignments.insert(target, normalized);
    Ok(())
}

fn expression_budget_error(target: &VarName, expr: &Expression) -> Option<String> {
    if !expression_exceeds_node_budget(expr, MAX_GUARDED_ALGORITHM_EXPR_NODES) {
        return None;
    }
    let node_count = expression_node_count(expr);
    Some(format!(
        "ForLoopGuardedExpressionTooLarge(target={target}, nodes={node_count}, max_nodes={MAX_GUARDED_ALGORITHM_EXPR_NODES})"
    ))
}

fn for_branch_state(entry_state: &ForLoopLowerState) -> ForLoopLowerState {
    ForLoopLowerState {
        assignments: entry_state.assignments.clone(),
        known_targets: entry_state.known_targets.clone(),
        touched_targets: IndexSet::new(),
        break_condition: None,
    }
}

fn mark_loop_break(state: &mut ForLoopLowerState, guard: Expression) {
    state.break_condition = Some(match state.break_condition.take() {
        Some(existing) => or_expr(existing, guard),
        None => guard,
    });
}

fn merge_break_condition(target: &mut Option<Expression>, candidate: Option<Expression>) {
    let Some(candidate) = candidate else {
        return;
    };
    *target = Some(match target.take() {
        Some(existing) => or_expr(existing, candidate),
        None => candidate,
    });
}

fn guard_break_condition(state: &mut ForLoopLowerState, guard: &Expression) {
    if let Some(condition) = state.break_condition.take() {
        state.break_condition = Some(and_expr(guard.clone(), condition));
    }
}

fn branch_target_value(dae: &Dae, state: &ForLoopLowerState, target: &VarName) -> Expression {
    state
        .assignments
        .get(target)
        .cloned()
        .unwrap_or_else(|| algorithm_if_fallback_expr(dae, target))
}

fn merge_for_if_branch_target(
    dae: &Dae,
    target: &VarName,
    entry_state: &ForLoopLowerState,
    branch_results: &[(Expression, ForLoopLowerState)],
    else_state: Option<&ForLoopLowerState>,
) -> Result<Expression, String> {
    let mut merged = else_state.map_or_else(
        || branch_target_value(dae, entry_state, target),
        |state| branch_target_value(dae, state, target),
    );
    for (guard, branch_state) in branch_results.iter().rev() {
        let value = branch_target_value(dae, branch_state, target);
        merged = guarded_expr(guard.clone(), value, merged);
        if let Some(error) = expression_budget_error(target, &merged) {
            return Err(error);
        }
    }
    Ok(normalize_algorithm_current_value(dae, target, &merged))
}

fn lower_for_statement_sequence_with_guard(
    dae: &Dae,
    flat: &Model,
    statements: &[Statement],
    guard: Expression,
    bindings: &HashMap<String, i64>,
    state: &mut ForLoopLowerState,
) -> Result<(), String> {
    for statement in statements {
        lower_for_statement_with_guard(dae, flat, statement, guard.clone(), bindings, state)?;
    }
    Ok(())
}

fn lower_for_if_with_guard(
    dae: &Dae,
    flat: &Model,
    cond_blocks: &[StatementBlock],
    else_block: &Option<Vec<Statement>>,
    guard: Expression,
    bindings: &HashMap<String, i64>,
    state: &mut ForLoopLowerState,
) -> Result<(), String> {
    let entry_state = ForLoopLowerState {
        assignments: state.assignments.clone(),
        known_targets: state.known_targets.clone(),
        touched_targets: IndexSet::new(),
        break_condition: state.break_condition.clone(),
    };
    let mut taken = bool_expr(false);
    let mut branch_results = Vec::new();
    for block in cond_blocks {
        let substituted_cond = substitute_expr_with_bindings(&block.cond, bindings);
        let cond = rewrite_algorithm_current_refs(
            dae,
            &substituted_cond,
            &entry_state.assignments,
            &entry_state.known_targets,
        );
        let branch_guard = and_expr(
            guard.clone(),
            and_expr(not_expr(taken.clone()), cond.clone()),
        );
        let mut branch_state = for_branch_state(&entry_state);
        lower_for_statement_sequence_with_guard(
            dae,
            flat,
            &block.stmts,
            bool_expr(true),
            bindings,
            &mut branch_state,
        )?;
        guard_break_condition(&mut branch_state, &branch_guard);
        branch_results.push((branch_guard, branch_state));
        taken = or_expr(taken, cond);
    }

    let else_state = if let Some(stmts) = else_block {
        let else_guard = and_expr(guard, not_expr(taken));
        let mut state = for_branch_state(&entry_state);
        lower_for_statement_sequence_with_guard(
            dae,
            flat,
            stmts,
            bool_expr(true),
            bindings,
            &mut state,
        )?;
        guard_break_condition(&mut state, &else_guard);
        Some(state)
    } else {
        None
    };

    let mut touched_targets = IndexSet::new();
    for (_, branch_state) in &branch_results {
        touched_targets.extend(branch_state.touched_targets.iter().cloned());
    }
    if let Some(state) = &else_state {
        touched_targets.extend(state.touched_targets.iter().cloned());
    }

    for target in touched_targets {
        let merged = merge_for_if_branch_target(
            dae,
            &target,
            &entry_state,
            &branch_results,
            else_state.as_ref(),
        )?;
        state.touched_targets.insert(target.clone());
        state.assignments.insert(target, merged);
    }

    state.known_targets = entry_state.known_targets;
    state.break_condition = entry_state.break_condition;
    for (_, branch_state) in branch_results {
        state.known_targets.extend(branch_state.known_targets);
        merge_break_condition(&mut state.break_condition, branch_state.break_condition);
    }
    if let Some(else_state) = else_state {
        state.known_targets.extend(else_state.known_targets);
        merge_break_condition(&mut state.break_condition, else_state.break_condition);
    }
    Ok(())
}

fn lower_nested_for_with_guard(
    dae: &Dae,
    flat: &Model,
    indices: &[rumoca_core::ForIndex],
    equations: &[Statement],
    guard: Expression,
    bindings: &HashMap<String, i64>,
    state: &mut ForLoopLowerState,
) -> Result<(), String> {
    let mut merged_bindings = bindings.clone();
    let iteration_bindings = expand_for_bindings(indices, flat, &merged_bindings)?;
    for iteration in iteration_bindings {
        merged_bindings = iteration;
        let iteration_guard = loop_active_guard(guard.clone(), &state.break_condition);
        lower_for_statement_sequence_with_guard(
            dae,
            flat,
            equations,
            iteration_guard,
            &merged_bindings,
            state,
        )?;
    }
    Ok(())
}

fn lower_for_statement_with_guard(
    dae: &Dae,
    flat: &Model,
    statement: &Statement,
    guard: Expression,
    bindings: &HashMap<String, i64>,
    state: &mut ForLoopLowerState,
) -> Result<(), String> {
    let active_guard = loop_active_guard(guard, &state.break_condition);
    if is_bool_expr(&active_guard, false) {
        return Ok(());
    }

    let substituted = substitute_statement_with_bindings(statement, bindings);
    match &substituted {
        Statement::Break { .. } => {
            mark_loop_break(state, active_guard);
            Ok(())
        }
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => lower_for_if_with_guard(
            dae,
            flat,
            cond_blocks,
            else_block,
            active_guard,
            bindings,
            state,
        ),
        Statement::For {
            indices, equations, ..
        } => lower_nested_for_with_guard(
            dae,
            flat,
            indices,
            equations,
            active_guard,
            bindings,
            state,
        ),
        Statement::While { .. } => Err("ForContainsWhile".to_string()),
        Statement::When { .. } => Err("ForContainsWhen".to_string()),
        _ => {
            let lowered = lower_statement_assignments(dae, flat, &substituted)?;
            for (target, value, _, _) in lowered {
                apply_guarded_loop_assignment(dae, state, target, value, active_guard.clone())?;
            }
            Ok(())
        }
    }
}

pub(super) fn lower_for_statement_assignments(
    dae: &Dae,
    flat: &Model,
    indices: &[rumoca_core::ForIndex],
    equations: &[Statement],
    outer_current_values: &IndexMap<VarName, Expression>,
    outer_known_targets: &HashSet<VarName>,
) -> Result<Vec<AlgorithmAssignment>, String> {
    // MLS §11.1 / §11.2: loop bodies execute in sequence within the enclosing
    // algorithm state, so they must observe earlier assignments in the same
    // algorithm block instead of restarting from the event-entry values.
    let mut state = ForLoopLowerState {
        assignments: outer_current_values.clone(),
        known_targets: outer_known_targets.clone(),
        touched_targets: IndexSet::new(),
        break_condition: None,
    };
    let mut loop_targets = HashSet::new();
    let bindings = HashMap::new();
    let iteration_bindings = expand_for_bindings(indices, flat, &bindings)?;
    for iteration in &iteration_bindings {
        for statement in equations {
            let substituted = substitute_statement_with_bindings(statement, iteration);
            for (target, _, _, _) in lower_statement_assignments_with_context(
                dae,
                flat,
                &substituted,
                &state.assignments,
                &state.known_targets,
            )? {
                state.known_targets.insert(target.clone());
                loop_targets.insert(target);
            }
        }
    }
    for iteration in iteration_bindings {
        let iteration_guard = loop_active_guard(bool_expr(true), &state.break_condition);
        lower_for_statement_sequence_with_guard(
            dae,
            flat,
            equations,
            iteration_guard,
            &iteration,
            &mut state,
        )?;
    }

    Ok(state
        .assignments
        .into_iter()
        .filter(|(target, _)| loop_targets.contains(target))
        .map(|(target, value)| {
            (
                target,
                value,
                Span::DUMMY,
                "algorithm for-assignment".to_string(),
            )
        })
        .collect())
}
