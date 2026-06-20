use super::*;

#[derive(Default)]
struct ForLoopLowerState {
    assignments: IndexMap<VarName, Expression>,
    assignment_spans: IndexMap<VarName, Span>,
    known_targets: HashSet<VarName>,
    touched_targets: IndexSet<VarName>,
    break_condition: Option<Expression>,
}

struct ForIfBranches<'a> {
    cond_blocks: &'a [StatementBlock],
    else_block: &'a Option<Vec<Statement>>,
    span: Span,
}

struct NestedForLoop<'a> {
    indices: &'a [rumoca_core::ForIndex],
    equations: &'a [Statement],
    span: Span,
}

fn require_source_span(span: Span, context: &str) -> Result<Span, String> {
    if span.is_dummy() {
        return Err(context.to_string());
    }
    Ok(span)
}

fn expression_or_owner_span(
    expr: &Expression,
    owner_span: Span,
    context: &str,
) -> Result<Span, String> {
    expr.span()
        .filter(|span| !span.is_dummy())
        .or_else(|| (!owner_span.is_dummy()).then_some(owner_span))
        .ok_or_else(|| context.to_string())
}

fn statement_or_owner_span(
    statement: &Statement,
    owner_span: Span,
    context: &str,
) -> Result<Span, String> {
    statement
        .source_span()
        .filter(|span| !span.is_dummy())
        .or_else(|| (!owner_span.is_dummy()).then_some(owner_span))
        .ok_or_else(|| context.to_string())
}

fn loop_active_guard(
    base_guard: Expression,
    break_condition: &Option<Expression>,
    owner_span: Span,
) -> Result<Expression, String> {
    let owner_span = expression_or_owner_span(&base_guard, owner_span, "ForLoopGuardMissingSpan")?;
    if let Some(break_expr) = break_condition {
        let break_span =
            expression_or_owner_span(break_expr, owner_span, "ForLoopBreakGuardMissingSpan")?;
        Ok(and_expr(
            base_guard,
            not_expr(break_expr.clone(), break_span),
            owner_span,
        ))
    } else {
        Ok(base_guard)
    }
}

fn apply_guarded_loop_assignment(
    dae: &Dae,
    state: &mut ForLoopLowerState,
    target: VarName,
    value: Expression,
    guard: Expression,
    assignment_span: Span,
) -> Result<(), String> {
    let rewritten =
        rewrite_algorithm_current_refs(dae, &value, &state.assignments, &state.known_targets)?;
    let fallback_span = value
        .span()
        .or_else(|| guard.span())
        .filter(|span| !span.is_dummy())
        .or_else(|| (!assignment_span.is_dummy()).then_some(assignment_span))
        .ok_or_else(|| format!("ForLoopAssignmentMissingSpan(target={})", target.as_str()))?;
    let fallback = state
        .assignments
        .shift_remove(&target)
        .map(Ok)
        .unwrap_or_else(|| algorithm_if_fallback_expr(dae, &target, fallback_span))?;
    let merged = guarded_expr(guard, rewritten, fallback, fallback_span);
    if let Some(error) = expression_budget_error(&target, &merged) {
        return Err(error);
    }
    let normalized = normalize_algorithm_current_value(dae, &target, &merged, fallback_span)
        .map_err(|err| err.to_string())?;
    state.touched_targets.insert(target.clone());
    state.assignment_spans.insert(target.clone(), fallback_span);
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
        assignment_spans: entry_state.assignment_spans.clone(),
        known_targets: entry_state.known_targets.clone(),
        touched_targets: IndexSet::new(),
        break_condition: None,
    }
}

fn mark_loop_break(
    state: &mut ForLoopLowerState,
    guard: Expression,
    owner_span: Span,
) -> Result<(), String> {
    let owner_span = expression_or_owner_span(&guard, owner_span, "ForLoopBreakGuardMissingSpan")?;
    state.break_condition = Some(match state.break_condition.take() {
        Some(existing) => or_expr(existing, guard, owner_span),
        None => guard,
    });
    Ok(())
}

fn merge_break_condition(
    target: &mut Option<Expression>,
    candidate: Option<Expression>,
    owner_span: Span,
) -> Result<(), String> {
    let Some(candidate) = candidate else {
        return Ok(());
    };
    let owner_span =
        expression_or_owner_span(&candidate, owner_span, "ForLoopMergedBreakMissingSpan")?;
    *target = Some(match target.take() {
        Some(existing) => or_expr(existing, candidate, owner_span),
        None => candidate,
    });
    Ok(())
}

fn guard_break_condition(
    state: &mut ForLoopLowerState,
    guard: &Expression,
    owner_span: Span,
) -> Result<(), String> {
    if let Some(condition) = state.break_condition.take() {
        let owner_span =
            expression_or_owner_span(guard, owner_span, "ForLoopGuardedBreakMissingSpan")?;
        state.break_condition = Some(and_expr(guard.clone(), condition, owner_span));
    }
    Ok(())
}

fn branch_target_value(
    dae: &Dae,
    state: &ForLoopLowerState,
    target: &VarName,
    span: Span,
) -> Result<Expression, String> {
    let span = require_source_span(span, "ForBranchTargetMissingSpan")?;
    match state.assignments.get(target) {
        Some(value) => Ok(value.clone()),
        None => algorithm_if_fallback_expr(dae, target, span),
    }
}

fn merge_for_if_branch_target(
    dae: &Dae,
    target: &VarName,
    entry_state: &ForLoopLowerState,
    branch_results: &[(Expression, ForLoopLowerState)],
    else_state: Option<&ForLoopLowerState>,
    if_span: Span,
) -> Result<Expression, String> {
    let mut merged = match else_state {
        Some(state) => branch_target_value(dae, state, target, if_span)?,
        None => branch_target_value(dae, entry_state, target, if_span)?,
    };
    for (guard, branch_state) in branch_results.iter().rev() {
        let guard_span = expression_or_owner_span(guard, if_span, "ForIfBranchGuardMissingSpan")?;
        let value = branch_target_value(dae, branch_state, target, guard_span)?;
        merged = guarded_expr(guard.clone(), value, merged, guard_span);
        if let Some(error) = expression_budget_error(target, &merged) {
            return Err(error);
        }
    }
    let owner_span = expression_or_owner_span(&merged, if_span, "ForIfMergedMissingSpan")?;
    normalize_algorithm_current_value(dae, target, &merged, owner_span)
        .map_err(|err| err.to_string())
}

fn lower_for_statement_sequence_with_guard(
    dae: &Dae,
    flat: &Model,
    statements: &[Statement],
    guard: Expression,
    bindings: &HashMap<String, i64>,
    state: &mut ForLoopLowerState,
    owner_span: Span,
) -> Result<(), String> {
    for statement in statements {
        lower_for_statement_with_guard(
            dae,
            flat,
            statement,
            guard.clone(),
            bindings,
            state,
            owner_span,
        )?;
    }
    Ok(())
}

fn lower_for_if_with_guard(
    dae: &Dae,
    flat: &Model,
    branches: ForIfBranches<'_>,
    guard: Expression,
    bindings: &HashMap<String, i64>,
    state: &mut ForLoopLowerState,
) -> Result<(), String> {
    let if_span = require_source_span(branches.span, "ForIfMissingSpan")?;
    let entry_state = ForLoopLowerState {
        assignments: state.assignments.clone(),
        assignment_spans: state.assignment_spans.clone(),
        known_targets: state.known_targets.clone(),
        touched_targets: IndexSet::new(),
        break_condition: state.break_condition.clone(),
    };
    let mut taken = bool_expr(false, if_span);
    let mut branch_results = Vec::new();
    for block in branches.cond_blocks {
        let substituted_cond = substitute_expr_with_bindings(&block.cond, bindings);
        let cond = rewrite_algorithm_current_refs(
            dae,
            &substituted_cond,
            &entry_state.assignments,
            &entry_state.known_targets,
        )?;
        let cond_span = expression_or_owner_span(&cond, if_span, "ForIfConditionMissingSpan")?;
        let branch_guard = and_expr(
            guard.clone(),
            and_expr(not_expr(taken.clone(), cond_span), cond.clone(), cond_span),
            cond_span,
        );
        let mut branch_state = for_branch_state(&entry_state);
        lower_for_statement_sequence_with_guard(
            dae,
            flat,
            &block.stmts,
            bool_expr(true, cond_span),
            bindings,
            &mut branch_state,
            cond_span,
        )?;
        guard_break_condition(&mut branch_state, &branch_guard, cond_span)?;
        branch_results.push((branch_guard, branch_state));
        taken = or_expr(taken, cond, cond_span);
    }

    let else_state = if let Some(stmts) = branches.else_block {
        let else_guard = and_expr(guard, not_expr(taken, if_span), if_span);
        let mut state = for_branch_state(&entry_state);
        lower_for_statement_sequence_with_guard(
            dae,
            flat,
            stmts,
            bool_expr(true, if_span),
            bindings,
            &mut state,
            if_span,
        )?;
        guard_break_condition(&mut state, &else_guard, if_span)?;
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
            if_span,
        )?;
        state.touched_targets.insert(target.clone());
        state.assignment_spans.insert(target.clone(), if_span);
        state.assignments.insert(target, merged);
    }

    state.known_targets = entry_state.known_targets;
    state.break_condition = entry_state.break_condition;
    for (_, branch_state) in branch_results {
        state.known_targets.extend(branch_state.known_targets);
        merge_break_condition(
            &mut state.break_condition,
            branch_state.break_condition,
            if_span,
        )?;
    }
    if let Some(else_state) = else_state {
        state.known_targets.extend(else_state.known_targets);
        merge_break_condition(
            &mut state.break_condition,
            else_state.break_condition,
            if_span,
        )?;
    }
    Ok(())
}

fn lower_nested_for_with_guard(
    dae: &Dae,
    flat: &Model,
    nested: NestedForLoop<'_>,
    guard: Expression,
    bindings: &HashMap<String, i64>,
    state: &mut ForLoopLowerState,
) -> Result<(), String> {
    let owner_span = require_source_span(nested.span, "NestedForMissingSpan")?;
    let mut merged_bindings = bindings.clone();
    let iteration_bindings = expand_for_bindings(nested.indices, flat, &merged_bindings)?;
    for iteration in iteration_bindings {
        merged_bindings = iteration;
        let iteration_guard = loop_active_guard(guard.clone(), &state.break_condition, owner_span)?;
        lower_for_statement_sequence_with_guard(
            dae,
            flat,
            nested.equations,
            iteration_guard,
            &merged_bindings,
            state,
            owner_span,
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
    owner_span: Span,
) -> Result<(), String> {
    let statement_span = statement_or_owner_span(statement, owner_span, "ForStatementMissingSpan")?;
    let active_guard = loop_active_guard(guard, &state.break_condition, statement_span)?;
    if is_bool_expr(&active_guard, false) {
        return Ok(());
    }

    let substituted = substitute_statement_with_bindings(statement, bindings);
    match &substituted {
        Statement::Break { .. } => {
            mark_loop_break(state, active_guard, statement_span)?;
            Ok(())
        }
        Statement::If {
            cond_blocks,
            else_block,
            span,
        } => lower_for_if_with_guard(
            dae,
            flat,
            ForIfBranches {
                cond_blocks,
                else_block,
                span: *span,
            },
            active_guard,
            bindings,
            state,
        ),
        Statement::For {
            indices,
            equations,
            span,
        } => lower_nested_for_with_guard(
            dae,
            flat,
            NestedForLoop {
                indices,
                equations,
                span: *span,
            },
            active_guard,
            bindings,
            state,
        ),
        Statement::While { .. } => Err("ForContainsWhile".to_string()),
        Statement::When { .. } => Err("ForContainsWhen".to_string()),
        _ => {
            let lowered = lower_statement_assignments(dae, flat, &substituted)?;
            for (target, value, span, _) in lowered {
                apply_guarded_loop_assignment(
                    dae,
                    state,
                    target,
                    value,
                    active_guard.clone(),
                    span,
                )?;
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
    owner_span: Span,
) -> Result<Vec<AlgorithmAssignment>, String> {
    let owner_span = require_source_span(owner_span, "ForLoopMissingSpan")?;
    // MLS §11.1 / §11.2: loop bodies execute in sequence within the enclosing
    // algorithm state, so they must observe earlier assignments in the same
    // algorithm block instead of restarting from the event-entry values.
    let mut state = ForLoopLowerState {
        assignments: outer_current_values.clone(),
        assignment_spans: IndexMap::new(),
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
        let iteration_guard = loop_active_guard(
            bool_expr(true, owner_span),
            &state.break_condition,
            owner_span,
        )?;
        lower_for_statement_sequence_with_guard(
            dae,
            flat,
            equations,
            iteration_guard,
            &iteration,
            &mut state,
            owner_span,
        )?;
    }

    let assignment_spans = state.assignment_spans;
    let mut lowered = Vec::new();
    for (target, value) in state.assignments {
        if !loop_targets.contains(&target) {
            continue;
        }
        let Some(span) = assignment_spans
            .get(&target)
            .copied()
            .or_else(|| value.span())
        else {
            return Err(format!(
                "ForLoopAssignmentMissingSpan(target={})",
                target.as_str()
            ));
        };
        lowered.push((target, value, span, "algorithm for-assignment".to_string()));
    }
    Ok(lowered)
}
