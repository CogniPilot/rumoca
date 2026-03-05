use rumoca_ir_dae as dae;
use rumoca_phase_structural::eliminate::{EliminationResult, try_solve_for_unknown};

#[derive(Clone)]
pub struct RuntimeAliasSubstitution {
    pub var_name: dae::VarName,
    pub expr: dae::Expression,
    pub equation_index: usize,
}

fn expr_contains_var_ref(expr: &dae::Expression, var_name: &dae::VarName) -> bool {
    let mut refs = std::collections::HashSet::new();
    expr.collect_var_refs(&mut refs);
    refs.contains(var_name)
}

fn expr_contains_any_event_or_clock_operator(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::BuiltinCall { function, args } => {
            matches!(
                function,
                dae::BuiltinFunction::Pre
                    | dae::BuiltinFunction::Edge
                    | dae::BuiltinFunction::Change
                    | dae::BuiltinFunction::Sample
                    | dae::BuiltinFunction::NoEvent
                    | dae::BuiltinFunction::Smooth
            ) || args.iter().any(expr_contains_any_event_or_clock_operator)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_contains_any_event_or_clock_operator(lhs)
                || expr_contains_any_event_or_clock_operator(rhs)
        }
        dae::Expression::Unary { rhs, .. } => expr_contains_any_event_or_clock_operator(rhs),
        dae::Expression::FunctionCall { args, .. } => {
            args.iter().any(expr_contains_any_event_or_clock_operator)
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, val)| {
                expr_contains_any_event_or_clock_operator(cond)
                    || expr_contains_any_event_or_clock_operator(val)
            }) || expr_contains_any_event_or_clock_operator(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => elements
            .iter()
            .any(expr_contains_any_event_or_clock_operator),
        dae::Expression::Range { start, step, end } => {
            expr_contains_any_event_or_clock_operator(start)
                || step
                    .as_deref()
                    .is_some_and(expr_contains_any_event_or_clock_operator)
                || expr_contains_any_event_or_clock_operator(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_contains_any_event_or_clock_operator(expr)
                || indices
                    .iter()
                    .any(|idx| expr_contains_any_event_or_clock_operator(&idx.range))
                || filter
                    .as_deref()
                    .is_some_and(expr_contains_any_event_or_clock_operator)
        }
        dae::Expression::Index { base, subscripts } => {
            expr_contains_any_event_or_clock_operator(base)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(expr) => expr_contains_any_event_or_clock_operator(expr),
                    _ => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => {
            expr_contains_any_event_or_clock_operator(base)
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

fn substitute_exact_var_exprs(
    exprs: &[dae::Expression],
    var_name: &dae::VarName,
    replacement: &dae::Expression,
) -> Vec<dae::Expression> {
    exprs
        .iter()
        .map(|expr| substitute_exact_var(expr, var_name, replacement))
        .collect()
}

fn substitute_exact_var_subscripts(
    subscripts: &[dae::Subscript],
    var_name: &dae::VarName,
    replacement: &dae::Expression,
) -> Vec<dae::Subscript> {
    subscripts
        .iter()
        .map(|sub| match sub {
            dae::Subscript::Expr(sub_expr) => dae::Subscript::Expr(Box::new(substitute_exact_var(
                sub_expr,
                var_name,
                replacement,
            ))),
            _ => sub.clone(),
        })
        .collect()
}

fn substitute_exact_var_if_branches(
    branches: &[(dae::Expression, dae::Expression)],
    var_name: &dae::VarName,
    replacement: &dae::Expression,
) -> Vec<(dae::Expression, dae::Expression)> {
    branches
        .iter()
        .map(|(cond, val)| {
            (
                substitute_exact_var(cond, var_name, replacement),
                substitute_exact_var(val, var_name, replacement),
            )
        })
        .collect()
}

fn substitute_exact_var_comprehension_indices(
    indices: &[dae::ComprehensionIndex],
    var_name: &dae::VarName,
    replacement: &dae::Expression,
) -> Vec<dae::ComprehensionIndex> {
    indices
        .iter()
        .map(|idx| dae::ComprehensionIndex {
            name: idx.name.clone(),
            range: substitute_exact_var(&idx.range, var_name, replacement),
        })
        .collect()
}

fn substitute_exact_var(
    expr: &dae::Expression,
    var_name: &dae::VarName,
    replacement: &dae::Expression,
) -> dae::Expression {
    match expr {
        dae::Expression::VarRef { name, subscripts }
            if name == var_name && subscripts.is_empty() =>
        {
            replacement.clone()
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            expr.clone()
        }
        dae::Expression::Binary { op, lhs, rhs } => dae::Expression::Binary {
            op: op.clone(),
            lhs: Box::new(substitute_exact_var(lhs, var_name, replacement)),
            rhs: Box::new(substitute_exact_var(rhs, var_name, replacement)),
        },
        dae::Expression::Unary { op, rhs } => dae::Expression::Unary {
            op: op.clone(),
            rhs: Box::new(substitute_exact_var(rhs, var_name, replacement)),
        },
        dae::Expression::BuiltinCall { function, args } => dae::Expression::BuiltinCall {
            function: *function,
            args: substitute_exact_var_exprs(args, var_name, replacement),
        },
        dae::Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => dae::Expression::FunctionCall {
            name: name.clone(),
            args: substitute_exact_var_exprs(args, var_name, replacement),
            is_constructor: *is_constructor,
        },
        dae::Expression::If {
            branches,
            else_branch,
        } => dae::Expression::If {
            branches: substitute_exact_var_if_branches(branches, var_name, replacement),
            else_branch: Box::new(substitute_exact_var(else_branch, var_name, replacement)),
        },
        dae::Expression::Array {
            elements,
            is_matrix,
        } => dae::Expression::Array {
            elements: substitute_exact_var_exprs(elements, var_name, replacement),
            is_matrix: *is_matrix,
        },
        dae::Expression::Tuple { elements } => dae::Expression::Tuple {
            elements: substitute_exact_var_exprs(elements, var_name, replacement),
        },
        dae::Expression::Range { start, step, end } => dae::Expression::Range {
            start: Box::new(substitute_exact_var(start, var_name, replacement)),
            step: step
                .as_ref()
                .map(|s| Box::new(substitute_exact_var(s, var_name, replacement))),
            end: Box::new(substitute_exact_var(end, var_name, replacement)),
        },
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => dae::Expression::ArrayComprehension {
            expr: Box::new(substitute_exact_var(expr, var_name, replacement)),
            indices: substitute_exact_var_comprehension_indices(indices, var_name, replacement),
            filter: filter
                .as_ref()
                .map(|pred| Box::new(substitute_exact_var(pred, var_name, replacement))),
        },
        dae::Expression::Index { base, subscripts } => dae::Expression::Index {
            base: Box::new(substitute_exact_var(base, var_name, replacement)),
            subscripts: substitute_exact_var_subscripts(subscripts, var_name, replacement),
        },
        dae::Expression::FieldAccess { base, field } => dae::Expression::FieldAccess {
            base: Box::new(substitute_exact_var(base, var_name, replacement)),
            field: field.clone(),
        },
    }
}

fn apply_runtime_alias_substitutions_expr(
    expr: &dae::Expression,
    substitutions: &[RuntimeAliasSubstitution],
) -> dae::Expression {
    let mut out = expr.clone();
    for sub in substitutions {
        if expr_contains_var_ref(&out, &sub.var_name) {
            out = substitute_exact_var(&out, &sub.var_name, &sub.expr);
        }
    }
    out
}

fn build_runtime_alias_substitutions(dae_model: &dae::Dae) -> Vec<RuntimeAliasSubstitution> {
    let mut substitutions = Vec::new();
    let runtime_defined_discrete_targets: std::collections::HashSet<String> = dae_model
        .f_m
        .iter()
        .chain(dae_model.f_z.iter())
        .filter_map(|eq| eq.lhs.as_ref())
        .map(|lhs| lhs.as_str().to_string())
        .collect();

    for (eq_idx, eq) in dae_model.f_x.iter().enumerate() {
        if eq.origin.starts_with("connection equation:")
            || eq.origin.starts_with("flow sum equation:")
        {
            continue;
        }
        let Some(target_name) = extract_runtime_assignment_target_name(&eq.rhs) else {
            continue;
        };
        let Some(_target_size) = dae_model.algebraics.get(&target_name).map(|var| var.size())
        else {
            continue;
        };
        if runtime_defined_discrete_targets.contains(target_name.as_str()) {
            continue;
        }
        if expr_contains_any_event_or_clock_operator(&eq.rhs) {
            continue;
        }
        let Some(solution) = try_solve_for_unknown(&eq.rhs, &target_name) else {
            continue;
        };
        if expr_contains_var_ref(&solution, &target_name) {
            continue;
        }
        substitutions.push(RuntimeAliasSubstitution {
            var_name: target_name,
            expr: solution,
            equation_index: eq_idx,
        });
    }
    substitutions
}

fn extract_runtime_assignment_target_name(expr: &dae::Expression) -> Option<dae::VarName> {
    let dae::Expression::Binary { op, lhs, rhs } = expr else {
        return None;
    };
    if !matches!(op, dae::OpBinary::Sub(_)) {
        return None;
    }
    if let dae::Expression::VarRef { name, subscripts } = lhs.as_ref()
        && subscripts.is_empty()
    {
        return Some(name.clone());
    }
    if let dae::Expression::VarRef { name, subscripts } = rhs.as_ref()
        && subscripts.is_empty()
    {
        return Some(name.clone());
    }
    None
}

fn apply_runtime_alias_substitutions(
    dae_model: &mut dae::Dae,
    substitutions: &[RuntimeAliasSubstitution],
) {
    if substitutions.is_empty() {
        return;
    }
    for eq in &mut dae_model.f_x {
        eq.rhs = apply_runtime_alias_substitutions_expr(&eq.rhs, substitutions);
    }
    for eq in &mut dae_model.f_z {
        eq.rhs = apply_runtime_alias_substitutions_expr(&eq.rhs, substitutions);
    }
    for eq in &mut dae_model.f_m {
        eq.rhs = apply_runtime_alias_substitutions_expr(&eq.rhs, substitutions);
    }
    for eq in &mut dae_model.f_c {
        eq.rhs = apply_runtime_alias_substitutions_expr(&eq.rhs, substitutions);
    }
    for expr in &mut dae_model.relation {
        *expr = apply_runtime_alias_substitutions_expr(expr, substitutions);
    }
    for expr in &mut dae_model.synthetic_root_conditions {
        *expr = apply_runtime_alias_substitutions_expr(expr, substitutions);
    }
    for expr in &mut dae_model.clock_constructor_exprs {
        *expr = apply_runtime_alias_substitutions_expr(expr, substitutions);
    }
}

fn expr_lists_reference_var(dae_model: &dae::Dae, var_name: &dae::VarName) -> bool {
    dae_model
        .f_x
        .iter()
        .any(|eq| expr_contains_var_ref(&eq.rhs, var_name))
        || dae_model
            .f_z
            .iter()
            .any(|eq| expr_contains_var_ref(&eq.rhs, var_name))
        || dae_model
            .f_m
            .iter()
            .any(|eq| expr_contains_var_ref(&eq.rhs, var_name))
        || dae_model
            .f_c
            .iter()
            .any(|eq| expr_contains_var_ref(&eq.rhs, var_name))
        || dae_model
            .relation
            .iter()
            .any(|expr| expr_contains_var_ref(expr, var_name))
        || dae_model
            .synthetic_root_conditions
            .iter()
            .any(|expr| expr_contains_var_ref(expr, var_name))
        || dae_model
            .clock_constructor_exprs
            .iter()
            .any(|expr| expr_contains_var_ref(expr, var_name))
}

/// Apply runtime alias substitutions to structural elimination substitutions.
pub fn apply_runtime_alias_substitutions_to_elimination(
    elim: &mut EliminationResult,
    substitutions: &[RuntimeAliasSubstitution],
) {
    if substitutions.is_empty() {
        return;
    }
    for sub in &mut elim.substitutions {
        sub.expr = apply_runtime_alias_substitutions_expr(&sub.expr, substitutions);
    }
}

/// Normalize eligible runtime aliases into state/core equations and prune now-unused aliases.
///
/// Returns `(removed_alias_count, substitutions_applied)`.
pub fn normalize_runtime_aliases_collect(
    dae_model: &mut dae::Dae,
) -> (usize, Vec<RuntimeAliasSubstitution>) {
    let substitutions = build_runtime_alias_substitutions(dae_model);
    if substitutions.is_empty() {
        return (0, substitutions);
    }
    apply_runtime_alias_substitutions(dae_model, &substitutions);

    let mut removed = 0usize;
    let mut removable_eq_indices = Vec::new();
    for sub in &substitutions {
        if expr_lists_reference_var(dae_model, &sub.var_name) {
            continue;
        }
        dae_model.algebraics.shift_remove(&sub.var_name);
        dae_model.outputs.shift_remove(&sub.var_name);
        removable_eq_indices.push(sub.equation_index);
        removed += 1;
    }
    removable_eq_indices.sort_unstable();
    removable_eq_indices.dedup();
    for idx in removable_eq_indices.into_iter().rev() {
        if idx < dae_model.f_x.len() {
            dae_model.f_x.remove(idx);
        }
    }

    (removed, substitutions)
}
