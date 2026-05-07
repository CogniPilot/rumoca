use crate::{
    Dae, MassMatrix, SimError, TimeoutBudget, VarName, build_parameter_values,
    debug_print_after_expand, debug_print_mass_matrix, debug_print_prepare_counts,
    dump_missing_state_equation_diagnostics, eliminate, inject_dummy_state, problem,
    run_timeout_step, run_timeout_step_result, sim_trace_enabled, trace_timer_elapsed_seconds,
    trace_timer_start_if,
};
use rumoca_sim_core::ir_dae as dae;
use rumoca_sim_core::phase_structural::eliminate::try_solve_for_unknown;
use rumoca_sim_core::phase_structural::scalarize::scalarize_equations;
use rumoca_sim_core::simulation::dae_prepare::{
    demote_alias_states_without_der, demote_coupled_derivative_states,
    demote_direct_assigned_states, demote_exact_alias_component_states,
    demote_orphan_states_without_equation_refs, demote_states_without_assignable_derivative_rows,
    demote_states_without_derivative_refs, demote_states_without_retained_derivative_rows,
    eliminate_derivative_aliases, expand_compound_derivatives,
    index_reduce_missing_state_derivatives, normalize_ode_equation_signs,
    promote_der_algebraics_to_states, substitute_standalone_state_derivatives_in_non_ode_rows,
};
use rumoca_sim_core::simulation::introspection::trace_flow_array_alias_watch;
use rumoca_sim_core::simulation::pipeline::{PreparedSimulation, run_logged_phase};
use rumoca_sim_core::{compute_mass_matrix, pin_orphaned_variables};

#[derive(Clone)]
struct RuntimeAliasSubstitution {
    var_name: VarName,
    expr: dae::Expression,
    equation_index: usize,
    preserve_defining_equation: bool,
}

fn parse_embedded_alias_indices(name: &str) -> Option<Vec<i64>> {
    let mut indices = Vec::new();
    let mut depth = 0usize;
    let mut current = String::new();
    let mut saw_subscript = false;

    for ch in name.chars() {
        match ch {
            '[' => {
                depth += 1;
                if depth == 1 {
                    current.clear();
                    saw_subscript = true;
                } else {
                    current.push(ch);
                }
            }
            ']' => {
                if depth == 1 {
                    let trimmed = current.trim();
                    indices.push(parse_integer_index_literal(trimmed)?);
                    current.clear();
                } else if depth > 1 {
                    current.push(ch);
                }
                depth = depth.saturating_sub(1);
            }
            _ if depth >= 1 => current.push(ch),
            _ => {}
        }
    }

    (saw_subscript && depth == 0 && !indices.is_empty()).then_some(indices)
}

fn parse_integer_index_literal(text: &str) -> Option<i64> {
    text.parse::<i64>().ok().or_else(|| {
        let value = text.parse::<f64>().ok()?;
        (value.is_finite() && value.fract() == 0.0).then_some(value as i64)
    })
}

fn embedded_alias_indices_for_exact_substitution(
    name: &VarName,
    subscripts: &[dae::Subscript],
    var_name: &VarName,
) -> Option<Vec<i64>> {
    if !subscripts.is_empty() || name == var_name {
        return None;
    }
    let name_base = dae::component_base_name(name.as_str())?;
    let var_base = dae::component_base_name(var_name.as_str())?;
    if name_base != var_base {
        return None;
    }
    parse_embedded_alias_indices(name.as_str())
}

fn runtime_alias_ref_matches(
    name: &VarName,
    subscripts: &[dae::Subscript],
    var_name: &VarName,
) -> bool {
    (name == var_name && subscripts.is_empty())
        || embedded_alias_indices_for_exact_substitution(name, subscripts, var_name).is_some()
}

fn expr_contains_var_ref(expr: &dae::Expression, var_name: &VarName) -> bool {
    match expr {
        dae::Expression::VarRef { name, subscripts } => {
            runtime_alias_ref_matches(name, subscripts, var_name)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(expr) => expr_contains_var_ref(expr, var_name),
                    _ => false,
                })
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_contains_var_ref(lhs, var_name) || expr_contains_var_ref(rhs, var_name)
        }
        dae::Expression::Unary { rhs, .. } => expr_contains_var_ref(rhs, var_name),
        dae::Expression::BuiltinCall { args, .. } | dae::Expression::FunctionCall { args, .. } => {
            args.iter().any(|arg| expr_contains_var_ref(arg, var_name))
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(condition, value)| {
                expr_contains_var_ref(condition, var_name) || expr_contains_var_ref(value, var_name)
            }) || expr_contains_var_ref(else_branch, var_name)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => elements
            .iter()
            .any(|element| expr_contains_var_ref(element, var_name)),
        dae::Expression::Range { start, step, end } => {
            expr_contains_var_ref(start, var_name)
                || step
                    .as_deref()
                    .is_some_and(|step| expr_contains_var_ref(step, var_name))
                || expr_contains_var_ref(end, var_name)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_contains_var_ref(expr, var_name)
                || indices
                    .iter()
                    .any(|idx| expr_contains_var_ref(&idx.range, var_name))
                || filter
                    .as_deref()
                    .is_some_and(|pred| expr_contains_var_ref(pred, var_name))
        }
        dae::Expression::Index { base, subscripts } => {
            expr_contains_var_ref(base, var_name)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(expr) => expr_contains_var_ref(expr, var_name),
                    _ => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => expr_contains_var_ref(base, var_name),
        dae::Expression::Literal(_) | dae::Expression::Empty => false,
    }
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

fn expr_contains_branch_local_analog_operator(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::BuiltinCall { function, args } => {
            matches!(
                function,
                dae::BuiltinFunction::Homotopy
                    | dae::BuiltinFunction::NoEvent
                    | dae::BuiltinFunction::Smooth
            ) || args.iter().any(expr_contains_branch_local_analog_operator)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_contains_branch_local_analog_operator(lhs)
                || expr_contains_branch_local_analog_operator(rhs)
        }
        dae::Expression::Unary { rhs, .. } => expr_contains_branch_local_analog_operator(rhs),
        dae::Expression::FunctionCall { args, .. } => {
            args.iter().any(expr_contains_branch_local_analog_operator)
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, val)| {
                expr_contains_branch_local_analog_operator(cond)
                    || expr_contains_branch_local_analog_operator(val)
            }) || expr_contains_branch_local_analog_operator(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => elements
            .iter()
            .any(expr_contains_branch_local_analog_operator),
        dae::Expression::Range { start, step, end } => {
            expr_contains_branch_local_analog_operator(start)
                || step
                    .as_deref()
                    .is_some_and(expr_contains_branch_local_analog_operator)
                || expr_contains_branch_local_analog_operator(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_contains_branch_local_analog_operator(expr)
                || indices
                    .iter()
                    .any(|idx| expr_contains_branch_local_analog_operator(&idx.range))
                || filter
                    .as_deref()
                    .is_some_and(expr_contains_branch_local_analog_operator)
        }
        dae::Expression::Index { base, subscripts } => {
            expr_contains_branch_local_analog_operator(base)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(expr) => expr_contains_branch_local_analog_operator(expr),
                    _ => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => {
            expr_contains_branch_local_analog_operator(base)
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

fn substitute_exact_var_exprs(
    exprs: &[dae::Expression],
    var_name: &VarName,
    replacement: &dae::Expression,
) -> Vec<dae::Expression> {
    exprs
        .iter()
        .map(|expr| substitute_exact_var(expr, var_name, replacement))
        .collect()
}

fn substitute_exact_var_subscripts(
    subscripts: &[dae::Subscript],
    var_name: &VarName,
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
    var_name: &VarName,
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
    var_name: &VarName,
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
    var_name: &VarName,
    replacement: &dae::Expression,
) -> dae::Expression {
    match expr {
        dae::Expression::VarRef { name, subscripts } => {
            if let Some(indices) =
                embedded_alias_indices_for_exact_substitution(name, subscripts, var_name)
            {
                index_runtime_alias_replacement(replacement, &indices)
            } else if name == var_name && subscripts.is_empty() {
                replacement.clone()
            } else {
                expr.clone()
            }
        }
        dae::Expression::Literal(_) | dae::Expression::Empty => expr.clone(),
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

fn index_runtime_alias_replacement(
    replacement: &dae::Expression,
    indices: &[i64],
) -> dae::Expression {
    if indices.is_empty() {
        return replacement.clone();
    }
    let extra_subscripts = indices
        .iter()
        .copied()
        .map(dae::Subscript::Index)
        .collect::<Vec<_>>();
    match replacement {
        dae::Expression::VarRef { name, subscripts } => {
            let mut projected_subscripts = subscripts.clone();
            projected_subscripts.extend(extra_subscripts);
            dae::Expression::VarRef {
                name: name.clone(),
                subscripts: projected_subscripts,
            }
        }
        _ => dae::Expression::Index {
            base: Box::new(replacement.clone()),
            subscripts: extra_subscripts,
        },
    }
}

fn apply_runtime_alias_substitutions_expr(
    expr: &dae::Expression,
    substitutions: &[RuntimeAliasSubstitution],
) -> dae::Expression {
    let mut out = expr.clone();
    let mut normalized_substitutions = Vec::with_capacity(substitutions.len());
    for sub in substitutions {
        let replacement =
            apply_runtime_alias_substitutions_expr(&sub.expr, &normalized_substitutions);
        if expr_contains_var_ref(&out, &sub.var_name) {
            out = substitute_exact_var(&out, &sub.var_name, &replacement);
        }
        let mut normalized = sub.clone();
        normalized.expr = replacement;
        normalized_substitutions.push(normalized);
    }
    out
}

fn dae_has_runtime_surfaces(dae: &Dae) -> bool {
    !dae.f_z.is_empty()
        || !dae.f_m.is_empty()
        || !dae.f_c.is_empty()
        || !dae.relation.is_empty()
        || !dae.synthetic_root_conditions.is_empty()
        || !dae.clock_constructor_exprs.is_empty()
}

fn runtime_alias_target_size(dae: &Dae, target_name: &VarName) -> Option<usize> {
    if let Some(var) = dae.algebraics.get(target_name) {
        return Some(var.size());
    }
    let var = dae.outputs.get(target_name)?;
    (!dae_has_runtime_surfaces(dae)).then_some(var.size())
}

fn build_runtime_alias_substitutions(dae: &Dae) -> Vec<RuntimeAliasSubstitution> {
    let mut substitutions = Vec::new();
    let runtime_defined_discrete_targets: std::collections::HashSet<String> = dae
        .f_m
        .iter()
        .chain(dae.f_z.iter())
        .filter_map(|eq| eq.lhs.as_ref())
        .map(|lhs| lhs.as_str().to_string())
        .collect();

    for (eq_idx, eq) in dae.f_x.iter().enumerate() {
        if eq.origin.starts_with("connection equation:")
            || eq.origin.starts_with("flow sum equation:")
        {
            continue;
        }
        let Some(target_name) = extract_runtime_assignment_target_name(&eq.rhs) else {
            continue;
        };
        let Some(_target_size) = runtime_alias_target_size(dae, &target_name) else {
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
            preserve_defining_equation: false,
        });
    }
    let snapshot = substitutions.clone();
    for sub in &mut substitutions {
        // MLS §3.7.4.4 and §3.7.5: branch-local analog helpers that remain
        // live inside homotopy/smooth/noEvent expressions must keep a real
        // defining constraint after alias normalization.
        sub.preserve_defining_equation =
            expr_lists_reference_var_in_branch_local_analog_context_after_substitution_excluding_f_x_equation(
                dae,
                &sub.var_name,
                sub.equation_index,
                &snapshot,
            );
    }
    substitutions
}

fn extract_runtime_assignment_target_name(expr: &dae::Expression) -> Option<VarName> {
    let dae::Expression::Binary { op, lhs, rhs } = expr else {
        return None;
    };
    if !matches!(op, rumoca_sim_core::ir_core::OpBinary::Sub(_)) {
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

fn apply_runtime_alias_substitutions(dae: &mut Dae, substitutions: &[RuntimeAliasSubstitution]) {
    if substitutions.is_empty() {
        return;
    }
    for (eq_idx, eq) in dae.f_x.iter_mut().enumerate() {
        let filtered = substitutions
            .iter()
            .filter(|sub| !(sub.preserve_defining_equation && sub.equation_index == eq_idx))
            .cloned()
            .collect::<Vec<_>>();
        eq.rhs = apply_runtime_alias_substitutions_expr(&eq.rhs, &filtered);
    }
    for eq in &mut dae.f_z {
        eq.rhs = apply_runtime_alias_substitutions_expr(&eq.rhs, substitutions);
    }
    for eq in &mut dae.f_m {
        eq.rhs = apply_runtime_alias_substitutions_expr(&eq.rhs, substitutions);
    }
    for eq in &mut dae.f_c {
        eq.rhs = apply_runtime_alias_substitutions_expr(&eq.rhs, substitutions);
    }
    for expr in &mut dae.relation {
        *expr = apply_runtime_alias_substitutions_expr(expr, substitutions);
    }
    for expr in &mut dae.synthetic_root_conditions {
        *expr = apply_runtime_alias_substitutions_expr(expr, substitutions);
    }
    for expr in &mut dae.clock_constructor_exprs {
        *expr = apply_runtime_alias_substitutions_expr(expr, substitutions);
    }
}

fn expr_lists_reference_var(dae: &Dae, var_name: &VarName) -> bool {
    dae.f_x
        .iter()
        .any(|eq| expr_contains_var_ref(&eq.rhs, var_name))
        || dae
            .f_z
            .iter()
            .any(|eq| expr_contains_var_ref(&eq.rhs, var_name))
        || dae
            .f_m
            .iter()
            .any(|eq| expr_contains_var_ref(&eq.rhs, var_name))
        || dae
            .f_c
            .iter()
            .any(|eq| expr_contains_var_ref(&eq.rhs, var_name))
        || dae
            .relation
            .iter()
            .any(|expr| expr_contains_var_ref(expr, var_name))
        || dae
            .synthetic_root_conditions
            .iter()
            .any(|expr| expr_contains_var_ref(expr, var_name))
        || dae
            .clock_constructor_exprs
            .iter()
            .any(|expr| expr_contains_var_ref(expr, var_name))
}

fn apply_runtime_alias_substitutions_preserving_var(
    expr: &dae::Expression,
    substitutions: &[RuntimeAliasSubstitution],
    preserved_var: &VarName,
) -> dae::Expression {
    let filtered = substitutions
        .iter()
        .filter(|sub| &sub.var_name != preserved_var)
        .cloned()
        .collect::<Vec<_>>();
    apply_runtime_alias_substitutions_expr(expr, &filtered)
}

fn expr_lists_reference_var_in_branch_local_analog_context_after_substitution_excluding_f_x_equation(
    dae: &Dae,
    var_name: &VarName,
    excluded_index: usize,
    substitutions: &[RuntimeAliasSubstitution],
) -> bool {
    dae.f_x.iter().enumerate().any(|(idx, eq)| {
        idx != excluded_index
            && expr_contains_branch_local_analog_operator(&eq.rhs)
            && expr_contains_var_ref(
                &apply_runtime_alias_substitutions_preserving_var(&eq.rhs, substitutions, var_name),
                var_name,
            )
    }) || dae.f_z.iter().any(|eq| {
        expr_contains_branch_local_analog_operator(&eq.rhs)
            && expr_contains_var_ref(
                &apply_runtime_alias_substitutions_preserving_var(&eq.rhs, substitutions, var_name),
                var_name,
            )
    }) || dae.f_m.iter().any(|eq| {
        expr_contains_branch_local_analog_operator(&eq.rhs)
            && expr_contains_var_ref(
                &apply_runtime_alias_substitutions_preserving_var(&eq.rhs, substitutions, var_name),
                var_name,
            )
    }) || dae.f_c.iter().any(|eq| {
        expr_contains_branch_local_analog_operator(&eq.rhs)
            && expr_contains_var_ref(
                &apply_runtime_alias_substitutions_preserving_var(&eq.rhs, substitutions, var_name),
                var_name,
            )
    }) || dae.relation.iter().any(|expr| {
        expr_contains_branch_local_analog_operator(expr)
            && expr_contains_var_ref(
                &apply_runtime_alias_substitutions_preserving_var(expr, substitutions, var_name),
                var_name,
            )
    }) || dae.synthetic_root_conditions.iter().any(|expr| {
        expr_contains_branch_local_analog_operator(expr)
            && expr_contains_var_ref(
                &apply_runtime_alias_substitutions_preserving_var(expr, substitutions, var_name),
                var_name,
            )
    }) || dae.clock_constructor_exprs.iter().any(|expr| {
        expr_contains_branch_local_analog_operator(expr)
            && expr_contains_var_ref(
                &apply_runtime_alias_substitutions_preserving_var(expr, substitutions, var_name),
                var_name,
            )
    })
}

fn apply_runtime_alias_substitutions_to_elimination(
    elim: &mut eliminate::EliminationResult,
    substitutions: &[RuntimeAliasSubstitution],
) {
    if substitutions.is_empty() {
        return;
    }
    for sub in &mut elim.substitutions {
        sub.expr = apply_runtime_alias_substitutions_expr(&sub.expr, substitutions);
    }
}

fn runtime_alias_reconstruction_substitutions(
    substitutions: &[RuntimeAliasSubstitution],
) -> Vec<rumoca_sim_core::phase_structural::eliminate::Substitution> {
    substitutions
        .iter()
        .map(
            |sub| rumoca_sim_core::phase_structural::eliminate::Substitution {
                var_name: sub.var_name.clone(),
                expr: sub.expr.clone(),
                env_keys: vec![sub.var_name.as_str().to_string()],
            },
        )
        .collect()
}

fn normalize_runtime_aliases_collect(dae: &mut Dae) -> (usize, Vec<RuntimeAliasSubstitution>) {
    let substitutions = build_runtime_alias_substitutions(dae);
    if substitutions.is_empty() {
        return (0, substitutions);
    }
    apply_runtime_alias_substitutions(dae, &substitutions);

    let mut removed = 0usize;
    let mut removable_eq_indices = Vec::new();
    for sub in &substitutions {
        if expr_lists_reference_var(dae, &sub.var_name) {
            continue;
        }
        dae.algebraics.shift_remove(&sub.var_name);
        removable_eq_indices.push(sub.equation_index);
        removed += 1;
    }
    removable_eq_indices.sort_unstable();
    removable_eq_indices.dedup();
    for idx in removable_eq_indices.into_iter().rev() {
        if idx < dae.f_x.len() {
            dae.f_x.remove(idx);
        }
    }

    (removed, substitutions)
}

pub(super) fn run_orphan_and_direct_state_demotion_phases(
    dae: &mut Dae,
    budget: &TimeoutBudget,
    trace: bool,
) -> Result<(), SimError> {
    let mut n_demoted_orphan_states = 0usize;
    run_logged_phase(
        trace,
        "demote_orphan_states_without_equation_refs(phase1h)",
        || {
            run_timeout_step(budget, || {
                n_demoted_orphan_states = demote_orphan_states_without_equation_refs(dae);
            })
        },
    )?;
    if n_demoted_orphan_states > 0 {
        eprintln!(
            "[prepare_dae] demoted {} orphan states without equation references",
            n_demoted_orphan_states
        );
    }

    let mut n_demoted_no_der_states = 0usize;
    run_logged_phase(
        trace,
        "demote_states_without_derivative_refs(phase1h2)",
        || {
            run_timeout_step(budget, || {
                n_demoted_no_der_states = demote_states_without_derivative_refs(dae);
            })
        },
    )?;
    if n_demoted_no_der_states > 0 {
        eprintln!(
            "[prepare_dae] demoted {} states without derivative references",
            n_demoted_no_der_states
        );
    }

    let mut n_demoted_unassignable_states = 0usize;
    run_logged_phase(
        trace,
        "demote_states_without_assignable_derivative_rows(phase1h3)",
        || {
            run_timeout_step(budget, || {
                n_demoted_unassignable_states =
                    demote_states_without_assignable_derivative_rows(dae);
            })
        },
    )?;
    if n_demoted_unassignable_states > 0 {
        eprintln!(
            "[prepare_dae] demoted {} states without assignable derivative rows",
            n_demoted_unassignable_states
        );
    }

    let n_states_before_repromote: usize = dae.states.values().map(|v| v.size()).sum();
    run_logged_phase(trace, "promote_der_algebraics_to_states(phase1h4)", || {
        run_timeout_step(budget, || promote_der_algebraics_to_states(dae))
    })?;
    let n_states_after_repromote: usize = dae.states.values().map(|v| v.size()).sum();
    let n_repromoted = n_states_after_repromote.saturating_sub(n_states_before_repromote);
    if n_repromoted > 0 {
        eprintln!(
            "[prepare_dae] re-promoted {} algebraics to states after demotion passes",
            n_repromoted
        );
    }

    let mut n_redemoted_unassignable_states = 0usize;
    run_logged_phase(
        trace,
        "demote_states_without_assignable_derivative_rows(phase1h5)",
        || {
            run_timeout_step(budget, || {
                n_redemoted_unassignable_states =
                    demote_states_without_assignable_derivative_rows(dae);
            })
        },
    )?;
    if n_redemoted_unassignable_states > 0 {
        eprintln!(
            "[prepare_dae] re-demoted {} states without assignable derivative rows after re-promotion",
            n_redemoted_unassignable_states
        );
    }

    let mut n_demoted_direct_assigned_states = 0usize;
    let disable_direct_state_demotion =
        std::env::var("RUMOCA_SIM_DISABLE_DIRECT_STATE_DEMOTION").is_ok();
    if !disable_direct_state_demotion {
        run_logged_phase(trace, "demote_direct_assigned_states(phase1i)", || {
            run_timeout_step(budget, || {
                n_demoted_direct_assigned_states = demote_direct_assigned_states(dae);
            })
        })?;
    }
    if n_demoted_direct_assigned_states > 0 {
        eprintln!(
            "[prepare_dae] demoted {} direct-assigned trajectory states",
            n_demoted_direct_assigned_states
        );
        run_logged_phase(trace, "expand_compound_derivatives(phase1i-post)", || {
            run_timeout_step(budget, || expand_compound_derivatives(dae))
        })?;
    }
    Ok(())
}

pub(super) fn run_prepare_structure_passes(
    dae: &mut Dae,
    budget: &TimeoutBudget,
) -> Result<(), SimError> {
    let trace = sim_trace_enabled();

    run_logged_phase(trace, "expand_compound_derivatives(phase1a2)", || {
        run_timeout_step(budget, || expand_compound_derivatives(dae))
    })?;
    debug_print_after_expand(dae);

    run_logged_phase(trace, "eliminate_derivative_aliases(phase1b2)", || {
        run_timeout_step(budget, || eliminate_derivative_aliases(dae))
    })?;

    let mut n_index_reduced = 0usize;
    run_logged_phase(
        trace,
        "index_reduce_missing_state_derivatives(phase1c)",
        || {
            run_timeout_step(budget, || {
                n_index_reduced = index_reduce_missing_state_derivatives(dae);
            })
        },
    )?;
    if n_index_reduced > 0 {
        eprintln!(
            "[prepare_dae] index-reduced {} missing state derivatives",
            n_index_reduced
        );
    }

    let mut n_demoted_coupled_states = 0usize;
    run_logged_phase(trace, "demote_coupled_derivative_states(phase1d)", || {
        run_timeout_step(budget, || {
            n_demoted_coupled_states = demote_coupled_derivative_states(dae);
        })
    })?;
    if n_demoted_coupled_states > 0 {
        eprintln!(
            "[prepare_dae] demoted {} coupled-derivative states",
            n_demoted_coupled_states
        );
    }

    let mut n_demoted_alias_states = 0usize;
    run_logged_phase(trace, "demote_alias_states_without_der(phase1e)", || {
        run_timeout_step(budget, || {
            n_demoted_alias_states = demote_alias_states_without_der(dae);
        })
    })?;
    if n_demoted_alias_states > 0 || n_demoted_coupled_states > 0 {
        eprintln!(
            "[prepare_dae] demoted {} alias-states without derivative rows",
            n_demoted_alias_states
        );
        run_logged_phase(trace, "expand_compound_derivatives(phase1e-post)", || {
            run_timeout_step(budget, || expand_compound_derivatives(dae))
        })?;
    }

    let n_states_before_promote: usize = dae.states.values().map(|v| v.size()).sum();
    run_logged_phase(trace, "promote_der_algebraics_to_states(phase1f)", || {
        run_timeout_step(budget, || promote_der_algebraics_to_states(dae))
    })?;
    let n_x_after: usize = dae.states.values().map(|v| v.size()).sum();
    let n_promoted = n_x_after - n_states_before_promote;
    if n_promoted > 0 {
        eprintln!(
            "[prepare_dae] promoted {} algebraics to states ({} → {} states)",
            n_promoted, n_states_before_promote, n_x_after
        );
    }

    run_orphan_and_direct_state_demotion_phases(dae, budget, trace)?;
    Ok(())
}

pub(super) fn reorder_equations_for_prepare(
    dae: &mut Dae,
    budget: &TimeoutBudget,
) -> Result<(), SimError> {
    match run_timeout_step_result(budget, || problem::reorder_equations_for_solver(dae)) {
        Ok(()) => Ok(()),
        Err(SimError::MissingStateEquation(name)) => {
            dump_missing_state_equation_diagnostics(dae, &name);
            Err(SimError::MissingStateEquation(name))
        }
        Err(e) => Err(e),
    }
}

pub(super) fn build_ic_plan_or_empty(
    dae: &Dae,
    n_x: usize,
    budget: &TimeoutBudget,
) -> Result<Vec<rumoca_sim_core::phase_structural::IcBlock>, SimError> {
    budget.check()?;
    if let Some(hint) = rumoca_sim_core::phase_structural::build_ic_relaxation_hint(dae, n_x)
        && relaxed_ic_hint_has_disjoint_drop_row(dae, &hint)
    {
        if sim_trace_enabled() {
            eprintln!(
                "[sim-trace] IC plan build: skipping relaxed BLT hint with disjoint dropped rows/unknowns; using full-Newton IC solve"
            );
        }
        budget.check()?;
        return Ok(Vec::new());
    }
    let ic_blocks = match rumoca_sim_core::phase_structural::build_ic_plan(dae, n_x) {
        Ok(blocks) => blocks,
        Err(err) => {
            if sim_trace_enabled() {
                eprintln!(
                    "[sim-trace] IC plan build failed, using empty plan: {:?}",
                    err
                );
            }
            Vec::new()
        }
    };
    budget.check()?;
    Ok(ic_blocks)
}

pub(super) fn relaxed_ic_hint_has_disjoint_drop_row(
    dae: &Dae,
    hint: &rumoca_sim_core::phase_structural::IcRelaxationHint,
) -> bool {
    if hint.dropped_eq_global.is_empty() || hint.dropped_unknown_names.is_empty() {
        return false;
    }
    let dropped_unknowns: Vec<VarName> = hint
        .dropped_unknown_names
        .iter()
        .map(VarName::new)
        .collect();
    hint.dropped_eq_global.iter().copied().any(|eq_idx| {
        let Some(eq) = dae.f_x.get(eq_idx) else {
            return true;
        };
        !dropped_unknowns.iter().any(|name| {
            rumoca_sim_core::phase_structural::eliminate::expr_contains_var(&eq.rhs, name)
        })
    })
}

pub(super) fn build_prepare_mass_matrix(
    dae: &Dae,
    n_x: usize,
    has_dummy: bool,
    budget: &TimeoutBudget,
) -> Result<MassMatrix, SimError> {
    if has_dummy {
        return Ok(Vec::new());
    }
    budget.check()?;
    let trace = sim_trace_enabled();
    if trace {
        eprintln!("[sim-trace] mass_diag start: build_parameter_values");
    }
    let t_params = trace_timer_start_if(trace);
    let param_values = build_parameter_values(dae, budget)?;
    if trace {
        eprintln!(
            "[sim-trace] mass_diag done: build_parameter_values elapsed={:.3}s",
            trace_timer_elapsed_seconds(t_params)
        );
    }
    if trace {
        eprintln!("[sim-trace] mass_matrix start: compute_mass_matrix");
    }
    let t_compute = trace_timer_start_if(trace);
    let mass_matrix = compute_mass_matrix(dae, n_x, &param_values, budget)?;
    if trace {
        eprintln!(
            "[sim-trace] mass_matrix done: compute_mass_matrix elapsed={:.3}s",
            trace_timer_elapsed_seconds(t_compute)
        );
    }
    budget.check()?;
    Ok(mass_matrix)
}

pub(super) fn eliminate_trivial_with_trace(
    dae: &mut Dae,
    trace: bool,
    phase_name: &str,
    watch_name: &str,
) -> eliminate::EliminationResult {
    if trace {
        eprintln!("[sim-trace] prepare phase start: {phase_name}");
    }
    let t_elim = trace_timer_start_if(trace);
    let elim = eliminate::eliminate_trivial(dae);
    trace_flow_array_alias_watch(watch_name, dae, trace);
    if trace {
        eprintln!(
            "[sim-trace] prepare phase done: {phase_name} elapsed={:.3}s eliminated_eqs={} substitutions={}",
            trace_timer_elapsed_seconds(t_elim),
            elim.n_eliminated,
            elim.substitutions.len()
        );
    }
    elim
}

fn run_trivial_elimination_phase(
    dae: &mut Dae,
    trace: bool,
    disable_trivial_elim: bool,
) -> eliminate::EliminationResult {
    if disable_trivial_elim {
        if trace {
            eprintln!("[sim-trace] prepare phase skipped: eliminate_trivial (disabled)");
        }
        return eliminate::EliminationResult::default();
    }
    eliminate_trivial_with_trace(dae, trace, "eliminate_trivial", "after_eliminate_trivial")
}

fn run_post_structure_elimination_phase(
    dae: &mut Dae,
    trace: bool,
    disable_trivial_elim: bool,
    elim: &mut eliminate::EliminationResult,
) {
    if disable_trivial_elim {
        if trace {
            eprintln!(
                "[sim-trace] prepare phase skipped: eliminate_trivial(post-structure) (disabled)"
            );
        }
        return;
    }
    let elim_post = eliminate_trivial_with_trace(
        dae,
        trace,
        "eliminate_trivial(post-structure)",
        "after_eliminate_trivial_post",
    );
    elim.substitutions.extend(elim_post.substitutions);
    elim.n_eliminated += elim_post.n_eliminated;
}

fn run_post_scalarize_elimination_phase(
    dae: &mut Dae,
    trace: bool,
    _scalarize: bool,
    disable_trivial_elim: bool,
    elim: &mut eliminate::EliminationResult,
) {
    if disable_trivial_elim {
        trace_flow_array_alias_watch("after_eliminate_trivial_post_scalarize", dae, trace);
        if trace {
            eprintln!(
                "[sim-trace] prepare phase done: eliminate_trivial(post-scalarize) elapsed=0.000s eliminated_eqs=0 substitutions=0 (skipped: trivial elimination disabled)"
            );
        }
        return;
    }
    let elim_post_scalar = eliminate_trivial_with_trace(
        dae,
        trace,
        "eliminate_trivial(post-scalarize)",
        "after_eliminate_trivial_post_scalarize",
    );
    elim.substitutions.extend(elim_post_scalar.substitutions);
    elim.n_eliminated += elim_post_scalar.n_eliminated;
}

pub(super) fn build_ic_plan_with_trace(
    dae: &Dae,
    n_x: usize,
    budget: &TimeoutBudget,
    trace: bool,
) -> Result<Vec<rumoca_sim_core::phase_structural::IcBlock>, SimError> {
    if trace {
        eprintln!("[sim-trace] prepare step start: build_ic_plan");
    }
    let t_ic = trace_timer_start_if(trace);
    let ic_blocks = build_ic_plan_or_empty(dae, n_x, budget)?;
    if trace {
        eprintln!(
            "[sim-trace] prepare step done: build_ic_plan elapsed={:.3}s",
            trace_timer_elapsed_seconds(t_ic)
        );
    }
    Ok(ic_blocks)
}

pub(super) fn build_mass_matrix_with_trace(
    dae: &Dae,
    n_x: usize,
    has_dummy: bool,
    budget: &TimeoutBudget,
    trace: bool,
) -> Result<MassMatrix, SimError> {
    if trace {
        eprintln!("[sim-trace] prepare step start: build_mass_matrix");
    }
    let t_mass = trace_timer_start_if(trace);
    let mass_matrix = build_prepare_mass_matrix(dae, n_x, has_dummy, budget)?;
    if trace {
        eprintln!(
            "[sim-trace] prepare step done: build_mass_matrix elapsed={:.3}s",
            trace_timer_elapsed_seconds(t_mass)
        );
    }
    Ok(mass_matrix)
}

fn substitute_non_ode_state_derivatives_with_trace(
    dae: &mut Dae,
    budget: &TimeoutBudget,
    trace: bool,
    trace_prefix: &str,
) -> Result<(), SimError> {
    if trace {
        eprintln!("[sim-trace] {trace_prefix} step start: substitute_standalone_state_derivatives");
    }
    let t0 = trace_timer_start_if(trace);
    let mut n_rewritten = 0usize;
    run_timeout_step(budget, || {
        n_rewritten = substitute_standalone_state_derivatives_in_non_ode_rows(dae);
    })?;
    if trace {
        eprintln!(
            "[sim-trace] {trace_prefix} step done: substitute_standalone_state_derivatives elapsed={:.3}s",
            trace_timer_elapsed_seconds(t0)
        );
    }
    if n_rewritten > 0 {
        eprintln!(
            "[prepare_dae] substituted der(state) in {} non-ODE rows",
            n_rewritten
        );
    }
    Ok(())
}

pub(super) fn log_prepare_substitutions_if_introspect(
    elim: &eliminate::EliminationResult,
    trace: bool,
) {
    if !(trace && std::env::var("RUMOCA_SIM_INTROSPECT").is_ok()) {
        return;
    }
    let expr_limit = std::env::var("RUMOCA_SIM_INTROSPECT_EXPR_CHARS")
        .ok()
        .and_then(|raw| raw.parse::<usize>().ok())
        .filter(|n| *n > 0)
        .unwrap_or(160);
    let truncate = |s: String| -> String {
        if s.len() <= expr_limit {
            s
        } else {
            format!("{}...", &s[..expr_limit])
        }
    };
    let limit = std::env::var("RUMOCA_SIM_INTROSPECT_SUB_LIMIT")
        .ok()
        .and_then(|raw| raw.parse::<usize>().ok())
        .filter(|n| *n > 0)
        .unwrap_or(40);
    eprintln!(
        "[sim-introspect] substitutions count={} (showing {})",
        elim.substitutions.len(),
        limit.min(elim.substitutions.len())
    );
    for (i, sub) in elim.substitutions.iter().take(limit).enumerate() {
        let expr = truncate(format!("{:?}", sub.expr));
        eprintln!(
            "[sim-introspect] sub[{i}] var={} env_keys={:?} expr={}",
            sub.var_name.as_str(),
            sub.env_keys,
            expr
        );
    }
    if elim.substitutions.len() > limit {
        eprintln!(
            "[sim-introspect] ... omitted {} substitutions (set RUMOCA_SIM_INTROSPECT_SUB_LIMIT to increase)",
            elim.substitutions.len() - limit
        );
    }
}

fn normalize_runtime_aliases_and_update_elim(
    dae: &mut Dae,
    elim: &mut eliminate::EliminationResult,
    trace: bool,
) {
    let (n_normalized, runtime_alias_substitutions) = normalize_runtime_aliases_collect(dae);
    apply_runtime_alias_substitutions_to_elimination(elim, &runtime_alias_substitutions);
    elim.substitutions
        .extend(runtime_alias_reconstruction_substitutions(
            &runtime_alias_substitutions,
        ));
    if trace && n_normalized > 0 {
        eprintln!(
            "[sim-trace] normalized {} runtime alias variables into core-state equations/events",
            n_normalized
        );
    }
}

fn log_final_orphan_state_cleanup(n_no_derivative_refs: usize, n_unassignable_rows: usize) {
    if n_no_derivative_refs > 0 {
        eprintln!(
            "[prepare_dae] final pass: demoted {} states whose derivative rows were removed",
            n_no_derivative_refs
        );
    }
    if n_unassignable_rows > 0 {
        eprintln!(
            "[prepare_dae] final pass: demoted {} states without assignable derivative rows",
            n_unassignable_rows
        );
    }
}

fn run_late_orphan_state_cleanup_if_needed(
    dae: &mut Dae,
    budget: &TimeoutBudget,
    trace: bool,
    late_continuous_rows_before: usize,
) -> Result<(), SimError> {
    if dae.f_x.len() >= late_continuous_rows_before {
        return Ok(());
    }

    let mut n_no_derivative_refs = 0usize;
    let mut n_unassignable_rows = 0usize;
    // MLS Appendix B / SPEC_0003: retained states require derivative rows.
    // Runtime alias normalization and post-structure elimination can remove
    // derivative-alias rows after the earlier demotion pass has already run.
    run_logged_phase(
        trace,
        "demote_states_without_retained_derivative_rows",
        || {
            run_timeout_step(budget, || {
                (n_no_derivative_refs, n_unassignable_rows) =
                    demote_states_without_retained_derivative_rows(dae);
            })
        },
    )?;
    log_final_orphan_state_cleanup(n_no_derivative_refs, n_unassignable_rows);
    Ok(())
}

/// Core DAE preparation pipeline shared by both simulation and template codegen.
///
/// Runs all structural passes (elimination, scalarization, equation reordering,
/// sign normalization, orphaned variable pinning). The `normalize_aliases` flag
/// controls whether runtime alias normalization runs.
fn prepare_dae_core(
    dae: &Dae,
    scalarize: bool,
    budget: &TimeoutBudget,
    normalize_aliases: bool,
) -> Result<(Dae, eliminate::EliminationResult, bool /* has_dummy */), SimError> {
    let trace = sim_trace_enabled();
    let trace_step = |name: &str, f: &mut dyn FnMut() -> Result<(), SimError>| {
        if trace {
            eprintln!("[sim-trace] prepare step start: {name}");
        }
        let t0 = trace_timer_start_if(trace);
        let res = f();
        if trace {
            eprintln!(
                "[sim-trace] prepare step done: {name} elapsed={:.3}s",
                trace_timer_elapsed_seconds(t0)
            );
        }
        res
    };

    budget.check()?;
    let n_x_orig: usize = dae.states.values().map(|v| v.size()).sum();
    let n_z_declared: usize = dae.algebraics.values().map(|v| v.size()).sum::<usize>()
        + dae.outputs.values().map(|v| v.size()).sum::<usize>();
    let n_discrete_declared: usize = dae.discrete_reals.values().map(|v| v.size()).sum::<usize>()
        + dae
            .discrete_valued
            .values()
            .map(|v| v.size())
            .sum::<usize>();
    if n_x_orig + n_z_declared + n_discrete_declared == 0 {
        return Err(SimError::EmptySystem);
    }

    let mut dae = dae.clone();
    let mut n_demoted_exact_alias_states = 0usize;
    run_logged_phase(trace, "demote_exact_alias_component_states(phase0)", || {
        run_timeout_step(budget, || {
            // MLS §8 equations and connector-generated equalities are
            // exact alias constraints. Run duplicate-state demotion before
            // trivial elimination erases those alias edges.
            n_demoted_exact_alias_states = demote_exact_alias_component_states(&mut dae);
        })
    })?;
    if n_demoted_exact_alias_states > 0 {
        eprintln!(
            "[prepare_dae] demoted {} exact-alias duplicate states",
            n_demoted_exact_alias_states
        );
    }
    let disable_trivial_elim = std::env::var("RUMOCA_SIM_DISABLE_TRIVIAL_ELIM").is_ok();
    budget.check()?;
    let mut elim = run_trivial_elimination_phase(&mut dae, trace, disable_trivial_elim);
    budget.check()?;

    run_prepare_structure_passes(&mut dae, budget)?;
    trace_flow_array_alias_watch("after_structure_passes", &dae, trace);

    let late_continuous_rows_before = dae.f_x.len();
    if normalize_aliases {
        run_logged_phase(trace, "normalize_runtime_aliases", || {
            run_timeout_step(budget, || {
                normalize_runtime_aliases_and_update_elim(&mut dae, &mut elim, trace)
            })
        })?;
    }

    budget.check()?;
    run_post_structure_elimination_phase(&mut dae, trace, disable_trivial_elim, &mut elim);
    budget.check()?;

    run_late_orphan_state_cleanup_if_needed(&mut dae, budget, trace, late_continuous_rows_before)?;

    debug_print_prepare_counts(&dae);
    let has_dummy = dae.states.values().map(|v| v.size()).sum::<usize>() == 0;
    if has_dummy {
        trace_step("inject_dummy_state", &mut || {
            run_timeout_step(budget, || inject_dummy_state(&mut dae))
        })?;
    }

    if scalarize {
        trace_step("scalarize_equations", &mut || {
            run_timeout_step(budget, || scalarize_equations(&mut dae))
        })?;
        trace_flow_array_alias_watch("after_scalarize", &dae, trace);
    }

    trace_step("reorder_equations_for_solver", &mut || {
        reorder_equations_for_prepare(&mut dae, budget)
    })?;

    trace_step("normalize_ode_equation_signs", &mut || {
        run_timeout_step(budget, || normalize_ode_equation_signs(&mut dae))
    })?;

    substitute_non_ode_state_derivatives_with_trace(&mut dae, budget, trace, "prepare")?;

    budget.check()?;
    run_post_scalarize_elimination_phase(
        &mut dae,
        trace,
        scalarize,
        disable_trivial_elim,
        &mut elim,
    );
    budget.check()?;

    log_prepare_substitutions_if_introspect(&elim, trace);

    trace_step("pin_orphaned_variables", &mut || {
        run_timeout_step(budget, || pin_orphaned_variables(&mut dae, &elim))
    })?;

    Ok((dae, elim, has_dummy))
}

/// Prepare a DAE for numerical simulation.
///
/// Runs the shared structural pipeline (with runtime alias normalization),
/// then builds the IC plan and mass matrix needed by the solver.
pub(super) fn prepare_dae(
    dae: &Dae,
    scalarize: bool,
    budget: &TimeoutBudget,
) -> Result<PreparedSimulation, SimError> {
    let trace = sim_trace_enabled();
    let (dae, elim, has_dummy) = prepare_dae_core(dae, scalarize, budget, true)?;

    let n_x: usize = dae.states.values().map(|v| v.size()).sum();
    let ic_blocks = build_ic_plan_with_trace(&dae, n_x, budget, trace)?;

    let mass_matrix = build_mass_matrix_with_trace(&dae, n_x, has_dummy, budget, trace)?;

    debug_print_mass_matrix(&dae, &mass_matrix);
    Ok(PreparedSimulation {
        dae,
        has_dummy_state: has_dummy,
        elimination: elim,
        ic_blocks,
        mass_matrix,
    })
}

#[cfg(test)]
mod tests;
