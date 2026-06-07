use super::*;
use crate::when_guard::when_guard_activation_expr;
use crate::{
    dae_to_flat_expression, dae_to_flat_var_name, flat_to_dae_expression, flat_to_dae_var_name,
};
mod current_value_rewrite;
mod for_lowering;
mod guarded_expr;
mod overlapping_array_assignments;
mod rewrite_discrete;
mod slice_lowering;
mod substitution;
mod target_names;
use current_value_rewrite::{
    algorithm_variable_dims, current_value_subscript_expr, rewrite_algorithm_current_refs,
};
use for_lowering::lower_for_statement_assignments;
use guarded_expr::{
    MAX_GUARDED_ALGORITHM_EXPR_NODES, expression_exceeds_node_budget, expression_node_count,
    guarded_expr,
};
use overlapping_array_assignments::collapse_overlapping_array_assignments;
use rewrite_discrete::{discrete_assignment_rhs_var_name, rewrite_discrete_self_refs_to_pre};
use slice_lowering::lower_assignment_statement;
use substitution::*;
use target_names::{
    algorithm_assignment_base_with_subscripts, algorithm_assignment_target_name,
    algorithm_output_target_name, varname_with_subscripts, varref_with_subscripts,
};

enum DiscreteEquationBucket {
    DiscreteReal,
    DiscreteValued,
}

#[derive(Clone, Copy)]
enum WhenInactiveRhs {
    Current,
    Pre,
    InitialValueThenPre,
}

fn discrete_variable_equation_bucket_for_lhs(
    dae: &Dae,
    lhs: &VarName,
) -> Option<DiscreteEquationBucket> {
    if dae
        .variables
        .discrete_valued
        .contains_key(&flat_to_dae_var_name(lhs))
        || subscript_fallback_chain(lhs.as_str())
            .into_iter()
            .any(|candidate| {
                dae.variables
                    .discrete_valued
                    .contains_key(&flat_to_dae_var_name(&candidate))
            })
    {
        return Some(DiscreteEquationBucket::DiscreteValued);
    }
    if dae
        .variables
        .discrete_reals
        .contains_key(&flat_to_dae_var_name(lhs))
        || subscript_fallback_chain(lhs.as_str())
            .into_iter()
            .any(|candidate| {
                dae.variables
                    .discrete_reals
                    .contains_key(&flat_to_dae_var_name(&candidate))
            })
    {
        return Some(DiscreteEquationBucket::DiscreteReal);
    }
    None
}

fn discrete_equation_bucket_for_lhs(dae: &Dae, lhs: &VarName) -> Option<DiscreteEquationBucket> {
    // MLS Appendix B notes reinit as a special case outside B.1b/B.1c.
    // Solver-facing DAE stores state resets in the event partition (`f_z`) so
    // runtime event updates can apply them without high-level when clauses.
    if dae
        .variables
        .states
        .contains_key(&flat_to_dae_var_name(lhs))
        || subscript_fallback_chain(lhs.as_str())
            .into_iter()
            .any(|candidate| {
                dae.variables
                    .states
                    .contains_key(&flat_to_dae_var_name(&candidate))
            })
    {
        return Some(DiscreteEquationBucket::DiscreteReal);
    }
    discrete_variable_equation_bucket_for_lhs(dae, lhs)
}

fn guarded_when_rhs_with_guard(
    guard: &Expression,
    lhs: &VarName,
    rhs: &Expression,
    inactive_rhs: WhenInactiveRhs,
) -> Expression {
    let else_expr = when_inactive_rhs(lhs, inactive_rhs);
    Expression::If {
        branches: vec![(guard.clone(), rhs.clone())],
        else_branch: Box::new(else_expr),
        span: rumoca_core::Span::DUMMY,
    }
}

fn when_inactive_rhs(lhs: &VarName, inactive_rhs: WhenInactiveRhs) -> Expression {
    match inactive_rhs {
        WhenInactiveRhs::Current => Expression::VarRef {
            name: lhs.clone().into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        },
        // MLS §8.3.5.1 / §8.6: inactive ordinary when-equations hold the
        // previous event value, including during initialization.
        WhenInactiveRhs::Pre => pre_target_expr(lhs),
        // Initial algorithms can assign discrete targets before runtime event
        // iteration, so algorithm when-statements preserve that initial value
        // when their runtime guard is false.
        WhenInactiveRhs::InitialValueThenPre => Expression::If {
            branches: vec![(
                Expression::BuiltinCall {
                    function: BuiltinFunction::Initial,
                    args: vec![],
                    span: rumoca_core::Span::DUMMY,
                },
                Expression::VarRef {
                    name: lhs.clone().into(),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                },
            )],
            else_branch: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Pre,
                args: vec![Expression::VarRef {
                    name: lhs.clone().into(),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                }],
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
    }
}

pub(super) fn route_discrete_event_equations(
    dae: &mut Dae,
    when_clause: &rumoca_ir_dae::WhenClause,
) -> Result<(), ToDaeError> {
    dae.events
        .event_actions
        .extend(when_clause.actions.iter().cloned());
    let when_condition = dae_to_flat_expression(&when_clause.condition);
    let guard = when_guard_activation_expr(dae, &when_condition);
    if when_clause.equations.len() != when_clause.equation_inactive_rhs.len() {
        return Err(ToDaeError::internal(format!(
            "when-clause equation metadata must stay aligned: {} equations, {} inactive RHS entries",
            when_clause.equations.len(),
            when_clause.equation_inactive_rhs.len()
        )));
    }
    for (eq, equation_inactive_rhs) in when_clause
        .equations
        .iter()
        .zip(when_clause.equation_inactive_rhs.iter())
    {
        let Some(lhs) = &eq.lhs else {
            continue;
        };
        let lhs = dae_to_flat_var_name(lhs);
        let rhs = dae_to_flat_expression(&eq.rhs);
        let inactive_rhs = match equation_inactive_rhs {
            rumoca_ir_dae::WhenEquationInactiveRhs::Current => WhenInactiveRhs::Current,
            rumoca_ir_dae::WhenEquationInactiveRhs::Pre => WhenInactiveRhs::Pre,
        };
        let guarded = rumoca_ir_dae::Equation::explicit_with_scalar_count(
            flat_to_dae_var_name(&lhs),
            flat_to_dae_expression(&guarded_when_rhs_with_guard(
                &guard,
                &lhs,
                &rhs,
                inactive_rhs,
            )),
            eq.span,
            format!("guarded {}", eq.origin),
            eq.scalar_count,
        );
        match discrete_equation_bucket_for_lhs(dae, &lhs) {
            Some(DiscreteEquationBucket::DiscreteValued) => {
                dae.discrete.valued_updates.push(guarded)
            }
            Some(DiscreteEquationBucket::DiscreteReal) => dae.discrete.real_updates.push(guarded),
            None => {}
        }
    }
    Ok(())
}

fn is_connection_equation_origin(origin: &str) -> bool {
    origin.contains("connection equation:")
}

fn anchored_collision_targets(
    grouped: &IndexMap<VarName, Vec<rumoca_ir_dae::Equation>>,
) -> std::collections::HashSet<VarName> {
    grouped
        .iter()
        .filter(|(_, equations)| {
            equations
                .iter()
                .any(|equation| !is_connection_equation_origin(&equation.origin))
        })
        .map(|(lhs, _)| lhs.clone())
        .collect()
}

fn flip_edge_key(origin: &str, lhs: &VarName, rhs: &VarName) -> (String, String, String) {
    (
        origin.to_string(),
        lhs.as_str().to_string(),
        rhs.as_str().to_string(),
    )
}

fn choose_collision_keep_index(
    equations: &[rumoca_ir_dae::Equation],
    lhs: &VarName,
    anchored_targets: &std::collections::HashSet<VarName>,
    flipped_once: &std::collections::HashSet<(String, String, String)>,
) -> Option<usize> {
    for (idx, equation) in equations.iter().enumerate() {
        let rhs_expr = dae_to_flat_expression(&equation.rhs);
        let Some(rhs_target) = discrete_assignment_rhs_var_name(&rhs_expr) else {
            continue;
        };
        let reverse_key = flip_edge_key(&equation.origin, &rhs_target, lhs);
        if flipped_once.contains(&reverse_key) {
            return Some(idx);
        }
    }

    for (idx, equation) in equations.iter().enumerate() {
        let rhs_expr = dae_to_flat_expression(&equation.rhs);
        let Some(rhs_target) = discrete_assignment_rhs_var_name(&rhs_expr) else {
            continue;
        };
        if anchored_targets.contains(&rhs_target) {
            return Some(idx);
        }
    }
    None
}

fn has_reverse_connection_alias(
    grouped: &IndexMap<VarName, Vec<rumoca_ir_dae::Equation>>,
    lhs: &VarName,
    rhs_target: &VarName,
) -> bool {
    grouped.get(rhs_target).is_some_and(|rhs_equations| {
        rhs_equations.iter().any(|rhs_equation| {
            if !is_connection_equation_origin(&rhs_equation.origin) {
                return false;
            }
            let rhs_expr = dae_to_flat_expression(&rhs_equation.rhs);
            discrete_assignment_rhs_var_name(&rhs_expr)
                .as_ref()
                .is_some_and(|candidate| candidate == lhs)
        })
    })
}

fn keep_collision_equation_without_flip(dae: &Dae, lhs: &VarName, rhs_target: &VarName) -> bool {
    rhs_target == lhs || discrete_equation_bucket_for_lhs(dae, rhs_target).is_none()
}

fn resolve_collision_equation(
    dae: &Dae,
    grouped: &IndexMap<VarName, Vec<rumoca_ir_dae::Equation>>,
    lhs: &VarName,
    equation: &rumoca_ir_dae::Equation,
    rhs_target: &VarName,
    flipped_once: &std::collections::HashSet<(String, String, String)>,
) -> Option<VarName> {
    if keep_collision_equation_without_flip(dae, lhs, rhs_target) {
        return None;
    }
    if has_reverse_connection_alias(grouped, lhs, rhs_target) {
        return Some(lhs.clone());
    }
    let reverse_flip_key = flip_edge_key(&equation.origin, rhs_target, lhs);
    if flipped_once.contains(&reverse_flip_key) {
        return Some(lhs.clone());
    }
    Some(rhs_target.clone())
}

fn process_collision_equation(
    dae: &Dae,
    grouped: &mut IndexMap<VarName, Vec<rumoca_ir_dae::Equation>>,
    lhs: &VarName,
    equation: rumoca_ir_dae::Equation,
    flipped_once: &mut std::collections::HashSet<(String, String, String)>,
    debug_canonicalize: bool,
) -> bool {
    let rhs_expr = dae_to_flat_expression(&equation.rhs);
    let Some(rhs_target) = discrete_assignment_rhs_var_name(&rhs_expr) else {
        if debug_canonicalize {
            crate::log_fm_canon_debug(format!(
                "f_m canonicalize no-rhs-var lhs={} origin={}",
                lhs, equation.origin
            ));
        }
        grouped.entry(lhs.clone()).or_default().push(equation);
        return false;
    };

    let Some(resolved_target) =
        resolve_collision_equation(dae, grouped, lhs, &equation, &rhs_target, flipped_once)
    else {
        if debug_canonicalize {
            crate::log_fm_canon_debug(format!(
                "f_m canonicalize keep-collision lhs={} rhs={} origin={}",
                lhs, rhs_target, equation.origin
            ));
        }
        grouped.entry(lhs.clone()).or_default().push(equation);
        return false;
    };

    if resolved_target == *lhs {
        if debug_canonicalize {
            crate::log_fm_canon_debug(format!(
                "f_m canonicalize drop-redundant-edge lhs={} rhs={} origin={}",
                lhs, rhs_target, equation.origin
            ));
        }
        return true;
    }

    flipped_once.insert(flip_edge_key(&equation.origin, lhs, &rhs_target));
    let mut flipped = equation;
    flipped.lhs = Some(flat_to_dae_var_name(&resolved_target));
    flipped.rhs = flat_to_dae_expression(&Expression::VarRef {
        name: lhs.clone().into(),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    });
    if debug_canonicalize {
        crate::log_fm_canon_debug(format!(
            "f_m canonicalize flip lhs={} -> lhs={} origin={}",
            lhs, resolved_target, flipped.origin
        ));
    }
    grouped.entry(resolved_target).or_default().push(flipped);
    true
}

fn resolve_connection_alias_target_collisions(
    grouped: &mut IndexMap<VarName, Vec<rumoca_ir_dae::Equation>>,
    dae: &Dae,
) -> Result<(), ToDaeError> {
    let debug_canonicalize = crate::fm_canon_debug_enabled();
    let anchored_targets = anchored_collision_targets(grouped);

    let mut iteration = 0usize;
    let max_iterations = 16usize;
    let mut flipped_once: std::collections::HashSet<(String, String, String)> =
        std::collections::HashSet::new();
    loop {
        iteration += 1;
        if iteration > max_iterations {
            return Err(ToDaeError::internal(format!(
                "f_m canonicalize collision-pass exceeded {max_iterations} iterations; \
                 this indicates unresolved discrete assignment target collisions"
            )));
        }

        let mut changed = false;
        let keys: Vec<VarName> = grouped.keys().cloned().collect();

        for lhs in keys {
            let Some(equations) = grouped.get(&lhs) else {
                continue;
            };
            if equations.len() <= 1 {
                continue;
            }
            if equations
                .iter()
                .any(|equation| !is_connection_equation_origin(&equation.origin))
            {
                continue;
            }

            let candidate_keep_idx =
                choose_collision_keep_index(equations, &lhs, &anchored_targets, &flipped_once);

            let Some(mut current_equations) = grouped.swap_remove(&lhs) else {
                continue;
            };
            if current_equations.len() <= 1 {
                grouped.insert(lhs.clone(), current_equations);
                continue;
            }

            let Some(keep_idx) = candidate_keep_idx else {
                return Err(ToDaeError::discrete_solved_form_violation(
                    format!("cannot resolve discrete connection assignment collision for `{lhs}`"),
                    current_equations
                        .first()
                        .map(|equation| equation.span)
                        .unwrap_or(rumoca_core::Span::DUMMY),
                ));
            };
            let keep_idx = keep_idx.min(current_equations.len() - 1);
            let keep_equation = current_equations.remove(keep_idx);
            grouped.insert(lhs.clone(), vec![keep_equation]);

            for equation in current_equations {
                changed |= process_collision_equation(
                    dae,
                    grouped,
                    &lhs,
                    equation,
                    &mut flipped_once,
                    debug_canonicalize,
                );
            }
        }

        if !changed {
            break;
        }
    }
    Ok(())
}

pub(super) fn canonicalize_discrete_assignment_equations(dae: &mut Dae) -> Result<(), ToDaeError> {
    let debug_canonicalize = crate::fm_canon_debug_enabled();
    {
        let (mut grouped, passthrough) =
            drain_grouped_discrete_assignments(&mut dae.discrete.valued_updates);
        reroute_connection_aliases_for_defined_targets(&mut grouped, dae);
        resolve_connection_alias_target_collisions(&mut grouped, dae)?;
        debug_log_grouped_target_collisions(debug_canonicalize, "f_m", &grouped);
        dae.discrete.valued_updates =
            rebuild_canonicalized_discrete_assignments(grouped, passthrough);
        debug_log_final_target_collisions(debug_canonicalize, "f_m", &dae.discrete.valued_updates);
    }
    {
        let (grouped, passthrough) =
            drain_grouped_discrete_assignments(&mut dae.discrete.real_updates);
        debug_log_grouped_target_collisions(debug_canonicalize, "f_z", &grouped);
        dae.discrete.real_updates =
            rebuild_canonicalized_discrete_assignments(grouped, passthrough);
        debug_log_final_target_collisions(debug_canonicalize, "f_z", &dae.discrete.real_updates);
    }
    Ok(())
}

fn drain_grouped_discrete_assignments(
    equations: &mut Vec<rumoca_ir_dae::Equation>,
) -> (
    IndexMap<VarName, Vec<rumoca_ir_dae::Equation>>,
    Vec<rumoca_ir_dae::Equation>,
) {
    let mut grouped: IndexMap<VarName, Vec<rumoca_ir_dae::Equation>> = IndexMap::new();
    let mut passthrough = Vec::new();
    for equation in equations.drain(..) {
        if let Some(lhs) = equation.lhs.as_ref() {
            grouped
                .entry(dae_to_flat_var_name(lhs))
                .or_default()
                .push(equation);
        } else {
            passthrough.push(equation);
        }
    }
    (grouped, passthrough)
}

fn rebuild_canonicalized_discrete_assignments(
    grouped: IndexMap<VarName, Vec<rumoca_ir_dae::Equation>>,
    passthrough: Vec<rumoca_ir_dae::Equation>,
) -> Vec<rumoca_ir_dae::Equation> {
    let mut rebuilt = Vec::with_capacity(passthrough.len() + grouped.len());
    rebuilt.extend(passthrough);
    for (lhs, equations) in grouped {
        rebuilt.extend(canonicalize_assignment_group(lhs, equations));
    }
    rebuilt
}

fn reroute_connection_aliases_for_defined_targets(
    grouped: &mut IndexMap<VarName, Vec<rumoca_ir_dae::Equation>>,
    dae: &Dae,
) {
    let mut rerouted: IndexMap<VarName, Vec<rumoca_ir_dae::Equation>> = IndexMap::new();
    for (lhs, equations) in grouped.iter_mut() {
        if equations.len() <= 1
            || !equations
                .iter()
                .any(|equation| !is_connection_equation_origin(&equation.origin))
        {
            continue;
        }

        for equation in equations
            .iter()
            .filter(|equation| is_connection_equation_origin(&equation.origin))
        {
            let rhs_expr = dae_to_flat_expression(&equation.rhs);
            let Some(rhs_target) = discrete_assignment_rhs_var_name(&rhs_expr) else {
                continue;
            };
            if keep_collision_equation_without_flip(dae, lhs, &rhs_target) {
                continue;
            }
            let mut flipped = equation.clone();
            flipped.lhs = Some(flat_to_dae_var_name(&rhs_target));
            flipped.rhs = flat_to_dae_expression(&Expression::VarRef {
                name: lhs.clone().into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            });
            rerouted.entry(rhs_target).or_default().push(flipped);
        }
    }
    for (lhs, aliases) in rerouted {
        grouped.entry(lhs).or_default().extend(aliases);
    }
}

fn canonicalize_assignment_group(
    lhs: VarName,
    mut equations: Vec<rumoca_ir_dae::Equation>,
) -> Vec<rumoca_ir_dae::Equation> {
    if equations.len() == 1 {
        let mut equation = equations.remove(0);
        if rewrites_discrete_self_refs(&equation) {
            let rewritten =
                rewrite_discrete_self_refs_to_pre(&dae_to_flat_expression(&equation.rhs), &lhs);
            equation.rhs = flat_to_dae_expression(&rewritten);
        }
        return vec![equation];
    }

    if let Some(merged) = merge_branch_split_discrete_assignments(&lhs, &equations) {
        return vec![merged];
    }

    if equations
        .iter()
        .any(|equation| !is_connection_equation_origin(&equation.origin))
    {
        equations.retain(|equation| !is_connection_equation_origin(&equation.origin));
    }

    let mut rewritten_equations = Vec::with_capacity(equations.len());
    for mut equation in equations {
        if rewrites_discrete_self_refs(&equation) {
            let rewritten =
                rewrite_discrete_self_refs_to_pre(&dae_to_flat_expression(&equation.rhs), &lhs);
            equation.rhs = flat_to_dae_expression(&rewritten);
        }
        rewritten_equations.push(equation);
    }
    rewritten_equations
}

fn rewrites_discrete_self_refs(equation: &rumoca_ir_dae::Equation) -> bool {
    !equation.origin.starts_with("guarded reinit")
}

fn merge_branch_split_discrete_assignments(
    lhs: &VarName,
    equations: &[rumoca_ir_dae::Equation],
) -> Option<rumoca_ir_dae::Equation> {
    let first = equations.first()?;
    if !rewrites_discrete_self_refs(first) {
        return None;
    }
    if equations
        .iter()
        .any(|equation| is_connection_equation_origin(&equation.origin))
        || equations
            .iter()
            .any(|equation| equation.origin != first.origin)
    {
        return None;
    }

    let inactive = pre_target_expr(lhs);
    let mut merged_branches = Vec::new();
    for equation in equations {
        let rhs = dae_to_flat_expression(&equation.rhs);
        let Expression::If {
            branches,
            else_branch,
            ..
        } = rhs
        else {
            return None;
        };
        let else_guard = negated_branch_guard(&branches);
        merged_branches.extend(
            branches
                .into_iter()
                .filter(|(_, value)| !is_pre_target_expr(value, lhs)),
        );
        if !is_pre_target_expr(&else_branch, lhs) {
            merged_branches.push((else_guard, *else_branch));
        }
    }
    if merged_branches.is_empty() {
        return None;
    }

    let mut merged = first.clone();
    // MLS §8.3.4: if-equation branches contribute one equation for each
    // branch-local equation. After flattening, branch-split discrete updates
    // for the same target are the same Appendix B assignment with guarded RHSs.
    merged.rhs = flat_to_dae_expression(&Expression::If {
        branches: merged_branches,
        else_branch: Box::new(inactive),
        span: rumoca_core::Span::DUMMY,
    });
    Some(merged)
}

fn is_pre_target_expr(expr: &Expression, target: &VarName) -> bool {
    matches!(
        expr,
        Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            args, ..
        } if matches!(
            args.as_slice(),
            [Expression::VarRef { name, subscripts, .. }]
                if name.var_name() == target && subscripts.is_empty()
        )
    )
}

fn negated_branch_guard(branches: &[(Expression, Expression)]) -> Expression {
    let active_guard = branches
        .iter()
        .map(|(condition, _)| condition.clone())
        .reduce(or_expr)
        .unwrap_or_else(|| bool_expr(false));
    not_expr(active_guard)
}

fn debug_log_grouped_target_collisions(
    debug_canonicalize: bool,
    partition: &str,
    grouped: &IndexMap<VarName, Vec<rumoca_ir_dae::Equation>>,
) {
    if !debug_canonicalize {
        return;
    }
    for (lhs, equations) in grouped.iter().filter(|(_, eqs)| eqs.len() > 1) {
        crate::log_fm_canon_debug(format!(
            "{} canonicalize grouped-collision lhs={} count={} origins={:?}",
            partition,
            lhs,
            equations.len(),
            equations
                .iter()
                .map(|eq| eq.origin.clone())
                .collect::<Vec<_>>()
        ));
    }
}

fn debug_log_final_target_collisions(
    debug_canonicalize: bool,
    partition: &str,
    equations: &[rumoca_ir_dae::Equation],
) {
    if !debug_canonicalize {
        return;
    }
    let mut counts: IndexMap<String, usize> = IndexMap::new();
    for equation in equations {
        if let Some(lhs) = equation.lhs.as_ref() {
            *counts.entry(lhs.as_str().to_string()).or_default() += 1;
        }
    }
    for (lhs, count) in counts.into_iter().filter(|(_, count)| *count > 1) {
        crate::log_fm_canon_debug(format!(
            "{} canonicalize final-target-collision lhs={} count={}",
            partition, lhs, count
        ));
    }
}

fn lookup_algorithm_target_scalar_count(
    dae: &Dae,
    target: &VarName,
    span: Span,
    allow_parameter_target: bool,
) -> Result<usize, String> {
    let target_key = flat_to_dae_var_name(target);
    if let Some(var) = dae
        .variables
        .states
        .get(&target_key)
        .or_else(|| dae.variables.algebraics.get(&target_key))
        .or_else(|| dae.variables.outputs.get(&target_key))
        .or_else(|| dae.variables.inputs.get(&target_key))
        .or_else(|| dae.variables.discrete_reals.get(&target_key))
        .or_else(|| dae.variables.discrete_valued.get(&target_key))
    {
        return Ok(var.size().max(1));
    }

    if allow_parameter_target && let Some(var) = dae.variables.parameters.get(&target_key) {
        return Ok(var.size().max(1));
    }

    for candidate in subscript_fallback_chain(target.as_str()) {
        let candidate_key = flat_to_dae_var_name(&candidate);
        if dae.variables.states.contains_key(&candidate_key)
            || dae.variables.algebraics.contains_key(&candidate_key)
            || dae.variables.outputs.contains_key(&candidate_key)
            || dae.variables.inputs.contains_key(&candidate_key)
            || dae.variables.discrete_reals.contains_key(&candidate_key)
            || dae.variables.discrete_valued.contains_key(&candidate_key)
            || (allow_parameter_target && dae.variables.parameters.contains_key(&candidate_key))
        {
            return Ok(1);
        }
    }

    Err(format!(
        "algorithm assignment target `{target}` is missing from DAE variables at {:?}",
        rumoca_core::span_to_source_span(span)
    ))
}

fn bool_expr(value: bool) -> Expression {
    Expression::Literal {
        value: Literal::Boolean(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn is_bool_expr(expr: &Expression, expected: bool) -> bool {
    matches!(expr, Expression::Literal { value: Literal::Boolean(v), .. } if *v == expected)
}

fn not_expr(expr: Expression) -> Expression {
    if let Expression::Literal {
        value: Literal::Boolean(flag),
        span: _,
    } = expr
    {
        return bool_expr(!flag);
    }
    Expression::Unary {
        op: rumoca_core::OpUnary::Not,
        rhs: Box::new(expr),
        span: rumoca_core::Span::DUMMY,
    }
}

fn and_expr(lhs: Expression, rhs: Expression) -> Expression {
    if is_bool_expr(&lhs, false) || is_bool_expr(&rhs, false) {
        return bool_expr(false);
    }
    if is_bool_expr(&lhs, true) {
        return rhs;
    }
    if is_bool_expr(&rhs, true) {
        return lhs;
    }
    Expression::Binary {
        op: rumoca_core::OpBinary::And,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn or_expr(lhs: Expression, rhs: Expression) -> Expression {
    if is_bool_expr(&lhs, true) || is_bool_expr(&rhs, true) {
        return bool_expr(true);
    }
    if is_bool_expr(&lhs, false) {
        return rhs;
    }
    if is_bool_expr(&rhs, false) {
        return lhs;
    }
    Expression::Binary {
        op: rumoca_core::OpBinary::Or,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn eval_integer_expr(expr: &Expression, flat: &Model) -> Option<i64> {
    scalar_size::try_eval_flat_expr_i64(expr, flat, 0)
}

fn eval_integer_array_expr(expr: &Expression, flat: &Model, depth: u8) -> Option<Vec<i64>> {
    if depth > 8 {
        return None;
    }
    match expr {
        Expression::Array {
            elements,
            is_matrix: false,
            ..
        } => elements
            .iter()
            .map(|element| scalar_size::try_eval_flat_expr_i64(element, flat, depth + 1))
            .collect(),
        Expression::VarRef { name, .. } => {
            let var = flat.variables.get(name.var_name())?;
            let binding = var.binding.as_ref()?;
            eval_integer_array_expr(binding, flat, depth + 1)
        }
        _ => None,
    }
}

fn eval_for_range_values(range: &Expression, flat: &Model) -> Result<Vec<i64>, String> {
    const MAX_LOOP_EXPANSION: usize = 100_000;
    let (start, step, end) = match range {
        Expression::Range {
            start, step, end, ..
        } => {
            let start_value = eval_integer_expr(start, flat)
                .ok_or_else(|| format!("ForRangeStartNotConstant({start:?})"))?;
            let end_value = eval_integer_expr(end, flat)
                .ok_or_else(|| format!("ForRangeEndNotConstant({end:?})"))?;
            let step_value = if let Some(step_expr) = step {
                eval_integer_expr(step_expr, flat)
                    .ok_or_else(|| format!("ForRangeStepNotConstant({step_expr:?})"))?
            } else {
                1
            };
            (start_value, step_value, end_value)
        }
        _ => {
            if let Some(end_value) = eval_integer_expr(range, flat) {
                (1, 1, end_value)
            } else if let Some(values) = eval_integer_array_expr(range, flat, 0) {
                return Ok(values);
            } else {
                return Err(format!("ForRangeNotConstant({range:?})"));
            }
        }
    };
    if step == 0 {
        return Err("ForRangeZeroStep".to_string());
    }
    let mut values = Vec::new();
    if step > 0 {
        let mut value = start;
        while value <= end {
            values.push(value);
            if values.len() > MAX_LOOP_EXPANSION {
                return Err(format!("ForRangeTooLarge({MAX_LOOP_EXPANSION})"));
            }
            value += step;
        }
    } else {
        let mut value = start;
        while value >= end {
            values.push(value);
            if values.len() > MAX_LOOP_EXPANSION {
                return Err(format!("ForRangeTooLarge({MAX_LOOP_EXPANSION})"));
            }
            value += step;
        }
    }
    Ok(values)
}

fn expand_for_bindings(
    indices: &[rumoca_core::ForIndex],
    flat: &Model,
    bindings: &HashMap<String, i64>,
) -> Result<Vec<HashMap<String, i64>>, String> {
    if indices.is_empty() {
        return Ok(vec![bindings.clone()]);
    }

    let mut expanded = Vec::new();
    let first = &indices[0];
    let range = substitute_expr_with_bindings(&first.range, bindings);
    let values = eval_for_range_values(&range, flat)?;
    for value in values {
        let mut next_bindings = bindings.clone();
        next_bindings.insert(first.ident.clone(), value);
        expanded.extend(expand_for_bindings(&indices[1..], flat, &next_bindings)?);
    }
    Ok(expanded)
}

fn resolve_multi_output_selection_names(
    flat: &Model,
    function_name: &VarName,
    requested_outputs: usize,
) -> Result<Vec<VarName>, String> {
    let Some(function) = resolve_flat_function(function_name.as_str(), flat) else {
        return Err("FunctionCallMultiOutputUnresolved".to_string());
    };
    if function.outputs.len() < requested_outputs {
        return Err("FunctionCallMultiOutputArityMismatch".to_string());
    }
    Ok(function
        .outputs
        .iter()
        .take(requested_outputs)
        .map(|output| {
            VarName::new(format!(
                "{}.{}",
                function_name.as_str(),
                output.name.as_str()
            ))
        })
        .collect())
}

fn pre_target_expr(target: &VarName) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Pre,
        args: vec![Expression::VarRef {
            name: target.clone().into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }],
        span: rumoca_core::Span::DUMMY,
    }
}

fn current_target_expr(target: &VarName) -> Expression {
    Expression::VarRef {
        name: target.clone().into(),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    }
}

fn normalize_algorithm_current_value(
    dae: &Dae,
    target: &VarName,
    value: &Expression,
) -> Expression {
    if discrete_equation_bucket_for_lhs(dae, target).is_some() {
        rewrite_discrete_self_refs_to_pre(value, target)
    } else {
        value.clone()
    }
}

fn algorithm_if_fallback_expr(dae: &Dae, target: &VarName) -> Expression {
    let is_discrete_target = dae
        .variables
        .discrete_reals
        .contains_key(&flat_to_dae_var_name(target))
        || dae
            .variables
            .discrete_valued
            .contains_key(&flat_to_dae_var_name(target))
        || subscript_fallback_chain(target.as_str())
            .into_iter()
            .any(|candidate| {
                dae.variables
                    .discrete_reals
                    .contains_key(&flat_to_dae_var_name(&candidate))
                    || dae
                        .variables
                        .discrete_valued
                        .contains_key(&flat_to_dae_var_name(&candidate))
            });
    if is_discrete_target {
        pre_target_expr(target)
    } else {
        current_target_expr(target)
    }
}

fn algorithm_assignment_to_target_expr(
    dae: &Dae,
    statement: &Statement,
) -> Option<(VarName, Expression, Span, String)> {
    match statement {
        Statement::Assignment { comp, value, span } => Some((
            algorithm_assignment_target_name(comp)?,
            value.clone(),
            *span,
            "algorithm assignment".to_string(),
        )),
        Statement::FunctionCall {
            comp,
            args,
            outputs,
            span,
        } => {
            let [output] = outputs.as_slice() else {
                return None;
            };
            let target = algorithm_output_target_name(output)?;
            Some((
                target,
                Expression::FunctionCall {
                    name: comp.to_var_name().into(),
                    args: args.clone(),
                    is_constructor: false,
                    span: *span,
                },
                *span,
                "algorithm function call assignment".to_string(),
            ))
        }
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => algorithm_if_assignment_to_target_expr(dae, cond_blocks, else_block),
        Statement::Empty { .. } => None,
        Statement::For { .. }
        | Statement::While { .. }
        | Statement::When { .. }
        | Statement::Reinit { .. }
        | Statement::Assert { .. }
        | Statement::Return { .. }
        | Statement::Break { .. } => None,
    }
}

fn algorithm_if_assignment_to_target_expr(
    dae: &Dae,
    cond_blocks: &[StatementBlock],
    else_block: &Option<Vec<Statement>>,
) -> Option<AlgorithmAssignment> {
    let mut branches = Vec::new();
    let mut target: Option<VarName> = None;
    for block in cond_blocks {
        let [single] = block.stmts.as_slice() else {
            return None;
        };
        let (block_target, block_value, _, _) = algorithm_assignment_to_target_expr(dae, single)?;
        if target
            .as_ref()
            .is_some_and(|existing| existing != &block_target)
        {
            return None;
        }
        if target.is_none() {
            target = Some(block_target);
        }
        branches.push((block.cond.clone(), block_value));
    }

    let else_value = algorithm_if_else_assignment_expr(dae, target.as_ref(), else_block)?;
    Some((
        target?,
        Expression::If {
            branches,
            else_branch: Box::new(else_value),
            span: rumoca_core::Span::DUMMY,
        },
        Span::DUMMY,
        "algorithm if-assignment".to_string(),
    ))
}

fn algorithm_if_else_assignment_expr(
    dae: &Dae,
    target: Option<&VarName>,
    else_block: &Option<Vec<Statement>>,
) -> Option<Expression> {
    if let Some(else_stmts) = else_block.as_ref() {
        let [else_single] = else_stmts.as_slice() else {
            return None;
        };
        let (else_target, else_value, _, _) =
            algorithm_assignment_to_target_expr(dae, else_single)?;
        return (target == Some(&else_target)).then_some(else_value);
    }

    let target_name = target?;
    if dae
        .variables
        .discrete_reals
        .contains_key(&flat_to_dae_var_name(target_name))
        || dae
            .variables
            .discrete_valued
            .contains_key(&flat_to_dae_var_name(target_name))
    {
        Some(pre_target_expr(target_name))
    } else {
        None
    }
}

type AlgorithmAssignment = (VarName, Expression, Span, String);

fn is_noop_algorithm_statement(statement: &Statement) -> bool {
    match statement {
        Statement::Empty { .. }
        | Statement::Assert { .. }
        | Statement::Return { .. }
        | Statement::Break { .. } => true,
        Statement::FunctionCall { outputs, .. } => outputs.is_empty(),
        Statement::For { equations, .. } => equations.iter().all(is_noop_algorithm_statement),
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .all(|block| block.stmts.iter().all(is_noop_algorithm_statement))
                && else_block
                    .as_ref()
                    .is_none_or(|stmts| stmts.iter().all(is_noop_algorithm_statement))
        }
        Statement::When { blocks, .. } => blocks
            .iter()
            .all(|block| block.stmts.iter().all(is_noop_algorithm_statement)),
        Statement::Assignment { .. } | Statement::While { .. } | Statement::Reinit { .. } => false,
    }
}

fn collect_statement_targets(
    dae: &Dae,
    flat: &Model,
    statement: &Statement,
) -> Result<Vec<VarName>, String> {
    if is_noop_algorithm_statement(statement) {
        return Ok(Vec::new());
    }

    match statement {
        Statement::Assignment { comp, value, span } => {
            Ok(lower_assignment_statement(flat, comp, value, *span)?
                .into_iter()
                .map(|(target, _, _, _)| target)
                .collect())
        }
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            let mut targets = IndexSet::new();
            for block in cond_blocks {
                extend_statement_targets(dae, flat, &block.stmts, &mut targets)?;
            }
            if let Some(statements) = else_block {
                extend_statement_targets(dae, flat, statements, &mut targets)?;
            }
            Ok(targets.into_iter().collect())
        }
        Statement::For {
            indices, equations, ..
        } => {
            let mut targets = IndexSet::new();
            let bindings = HashMap::new();
            let iteration_bindings = expand_for_bindings(indices, flat, &bindings)?;
            for iteration in iteration_bindings {
                for statement in equations {
                    let substituted = substitute_statement_with_bindings(statement, &iteration);
                    extend_statement_targets(
                        dae,
                        flat,
                        std::slice::from_ref(&substituted),
                        &mut targets,
                    )?;
                }
            }
            Ok(targets.into_iter().collect())
        }
        Statement::FunctionCall {
            comp,
            args: _,
            outputs,
            ..
        } if outputs.len() > 1 => outputs
            .iter()
            .map(|output| {
                algorithm_output_target_name(output)
                    .ok_or_else(|| "FunctionCallMultiOutputTarget".to_string())
            })
            .collect(),
        _ => algorithm_assignment_to_target_expr(dae, statement)
            .map(|(target, _, _, _)| vec![target])
            .ok_or_else(|| {
                format!(
                    "{} {:?}",
                    unsupported_algorithm_statement_tag(statement),
                    statement
                )
            }),
    }
}

fn extend_statement_targets(
    dae: &Dae,
    flat: &Model,
    statements: &[Statement],
    targets: &mut IndexSet<VarName>,
) -> Result<(), String> {
    for statement in statements {
        for target in collect_statement_targets(dae, flat, statement)? {
            targets.insert(target);
        }
    }
    Ok(())
}

fn collect_algorithm_block_assignments(
    dae: &Dae,
    flat: &Model,
    statements: &[Statement],
    initial_values: &IndexMap<VarName, Expression>,
    initial_known_targets: &HashSet<VarName>,
) -> Result<IndexMap<VarName, AlgorithmAssignment>, String> {
    let mut assignments: IndexMap<VarName, AlgorithmAssignment> = IndexMap::new();
    let mut current_values = initial_values.clone();
    let mut known_targets = initial_known_targets.clone();
    for statement in statements {
        for target in collect_statement_targets(dae, flat, statement)? {
            known_targets.insert(target);
        }
    }
    for statement in statements {
        for (target, value, span, origin) in lower_statement_assignments_with_context(
            dae,
            flat,
            statement,
            &current_values,
            &known_targets,
        )? {
            let rewritten =
                rewrite_algorithm_current_refs(dae, &value, &current_values, &known_targets);
            let normalized = normalize_algorithm_current_value(dae, &target, &rewritten);
            current_values.insert(target.clone(), normalized.clone());
            assignments.insert(target.clone(), (target, normalized, span, origin));
        }
    }
    Ok(collapse_overlapping_array_assignments(dae, assignments))
}

fn lower_if_statement_assignments(
    dae: &Dae,
    flat: &Model,
    cond_blocks: &[StatementBlock],
    else_block: &Option<Vec<Statement>>,
    outer_current_values: &IndexMap<VarName, Expression>,
    outer_known_targets: &HashSet<VarName>,
) -> Result<Vec<AlgorithmAssignment>, String> {
    let mut branch_maps: Vec<(Expression, IndexMap<VarName, AlgorithmAssignment>)> = Vec::new();
    for block in cond_blocks {
        let assignments = collect_algorithm_block_assignments(
            dae,
            flat,
            &block.stmts,
            outer_current_values,
            outer_known_targets,
        )?;
        branch_maps.push((block.cond.clone(), assignments));
    }

    let else_assignments = match else_block.as_ref() {
        Some(statements) => collect_algorithm_block_assignments(
            dae,
            flat,
            statements,
            outer_current_values,
            outer_known_targets,
        )?,
        None => IndexMap::new(),
    };

    let mut targets: IndexSet<VarName> = IndexSet::new();
    for (_, assignments) in &branch_maps {
        for target in assignments.keys() {
            targets.insert(target.clone());
        }
    }
    for target in else_assignments.keys() {
        targets.insert(target.clone());
    }

    let mut lowered = Vec::new();
    for target in targets {
        let branches = branch_maps
            .iter()
            .map(|(condition, assignments)| {
                let rhs = assignments
                    .get(&target)
                    .map(|(_, value, _, _)| value.clone())
                    // MLS §11.1.3: if an if-branch does not assign a target variable,
                    // the variable retains its value — pre(x) for discrete, x for continuous.
                    .unwrap_or_else(|| algorithm_if_fallback_expr(dae, &target));
                (condition.clone(), rhs)
            })
            .collect();
        let else_rhs = else_assignments
            .get(&target)
            .map(|(_, value, _, _)| value.clone())
            // MLS §11.1.3: unassigned target in else-branch falls back to pre(x) for discrete,
            // x for continuous.
            .unwrap_or_else(|| algorithm_if_fallback_expr(dae, &target));
        lowered.push((
            target,
            Expression::If {
                branches,
                else_branch: Box::new(else_rhs),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "algorithm if-assignment".to_string(),
        ));
    }

    Ok(lowered)
}

fn lower_statement_assignments_with_context(
    dae: &Dae,
    flat: &Model,
    statement: &Statement,
    current_values: &IndexMap<VarName, Expression>,
    known_targets: &HashSet<VarName>,
) -> Result<Vec<AlgorithmAssignment>, String> {
    if is_noop_algorithm_statement(statement) {
        return Ok(Vec::new());
    }

    match statement {
        Statement::Assignment { comp, value, span } => {
            lower_assignment_statement(flat, comp, value, *span)
        }
        Statement::When { .. } => Err("When".to_string()),
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => lower_if_statement_assignments(
            dae,
            flat,
            cond_blocks,
            else_block,
            current_values,
            known_targets,
        ),
        Statement::For {
            indices, equations, ..
        } => lower_for_statement_assignments(
            dae,
            flat,
            indices,
            equations,
            current_values,
            known_targets,
        ),
        Statement::FunctionCall {
            comp,
            args,
            outputs,
            span,
        } if outputs.len() > 1 => {
            let function_name = comp.to_var_name();
            let selection_names =
                resolve_multi_output_selection_names(flat, &function_name, outputs.len())?;
            let mut lowered = Vec::with_capacity(outputs.len());
            for (output_expr, selection_name) in outputs.iter().zip(selection_names.iter()) {
                let Some(target) = algorithm_output_target_name(output_expr) else {
                    return Err("FunctionCallMultiOutputTarget".to_string());
                };
                lowered.push((
                    target,
                    Expression::FunctionCall {
                        name: selection_name.clone().into(),
                        args: args.clone(),
                        is_constructor: false,
                        span: *span,
                    },
                    *span,
                    "algorithm function call assignment".to_string(),
                ));
            }
            Ok(lowered)
        }
        _ => algorithm_assignment_to_target_expr(dae, statement)
            .map(|assignment| vec![assignment])
            .ok_or_else(|| {
                format!(
                    "{} {:?}",
                    unsupported_algorithm_statement_tag(statement),
                    statement
                )
            }),
    }
}

fn lower_statement_assignments(
    dae: &Dae,
    flat: &Model,
    statement: &Statement,
) -> Result<Vec<AlgorithmAssignment>, String> {
    let current_values = IndexMap::new();
    let known_targets = HashSet::new();
    lower_statement_assignments_with_context(dae, flat, statement, &current_values, &known_targets)
}

fn unsupported_algorithm_statement_tag(statement: &Statement) -> &'static str {
    match statement {
        Statement::Assignment { .. } => "Assignment",
        Statement::If { .. } => "If",
        Statement::For { .. } => "For",
        Statement::While { .. } => "While",
        Statement::When { .. } => "When",
        Statement::FunctionCall { .. } => "FunctionCall",
        Statement::Reinit { .. } => "Reinit",
        Statement::Assert { .. } => "Assert",
        Statement::Return { .. } => "Return",
        Statement::Break { .. } => "Break",
        Statement::Empty { .. } => "Empty",
    }
}

#[derive(Default)]
struct LoweredAlgorithmPartitions {
    main: Vec<rumoca_ir_dae::Equation>,
    f_z: Vec<rumoca_ir_dae::Equation>,
    f_m: Vec<rumoca_ir_dae::Equation>,
}

struct WhenAssignmentTarget {
    span: Span,
    branches: Vec<(Expression, Expression)>,
}

type WhenAssignmentBranches = IndexMap<VarName, WhenAssignmentTarget>;

fn collect_when_statement_target_branches(
    dae: &Dae,
    flat: &Model,
    blocks: &[StatementBlock],
    statement_span: Span,
) -> Result<WhenAssignmentBranches, String> {
    let mut targets: WhenAssignmentBranches = IndexMap::new();
    let empty_values = IndexMap::new();
    let empty_targets = HashSet::new();
    for block in blocks {
        let block_assignments = collect_algorithm_block_assignments(
            dae,
            flat,
            &block.stmts,
            &empty_values,
            &empty_targets,
        )?;
        for (_, (target, value, _, _)) in block_assignments {
            targets
                .entry(target)
                .or_insert_with(|| WhenAssignmentTarget {
                    span: statement_span,
                    branches: Vec::new(),
                })
                .branches
                .push((when_guard_activation_expr(dae, &block.cond), value));
        }
    }

    Ok(targets)
}

fn merge_algorithm_when_statement_branches(
    merged_targets: &mut WhenAssignmentBranches,
    statement_targets: WhenAssignmentBranches,
) {
    for (target, mut new_target) in statement_targets {
        if let Some(existing) = merged_targets.get_mut(&target) {
            // MLS §11.1 / §11.2: algorithm statements execute in source order,
            // so later when-statements must override earlier assignments to the
            // same target when both fire at the same event instant.
            new_target
                .branches
                .extend(std::mem::take(&mut existing.branches));
            *existing = new_target;
        } else {
            merged_targets.insert(target, new_target);
        }
    }
}

fn lower_when_target_branches_to_event_equations(
    dae: &Dae,
    targets: WhenAssignmentBranches,
    algorithm_span: Span,
    algorithm_origin: &str,
    allow_parameter_targets: bool,
) -> Result<Vec<rumoca_ir_dae::Equation>, String> {
    let mut lowered = Vec::with_capacity(targets.len());
    for (target, assignment) in targets {
        let span = if assignment.span == Span::DUMMY {
            algorithm_span
        } else {
            assignment.span
        };
        let eq = rumoca_ir_dae::Equation::explicit_with_scalar_count(
            flat_to_dae_var_name(&target),
            flat_to_dae_expression(&Expression::If {
                branches: assignment.branches,
                else_branch: Box::new(when_inactive_rhs(
                    &target,
                    WhenInactiveRhs::InitialValueThenPre,
                )),
                span: rumoca_core::Span::DUMMY,
            }),
            span,
            format!("algorithm when-assignment ({algorithm_origin})"),
            lookup_algorithm_target_scalar_count(dae, &target, span, allow_parameter_targets)?,
        );
        lowered.push(eq);
    }

    Ok(lowered)
}

fn lower_algorithm_to_equations(
    dae: &Dae,
    flat: &Model,
    algorithm: &rumoca_ir_flat::Algorithm,
    allow_parameter_targets: bool,
) -> Result<LoweredAlgorithmPartitions, String> {
    if algorithm.statements.is_empty() {
        return Ok(LoweredAlgorithmPartitions::default());
    }

    let mut lowered = LoweredAlgorithmPartitions::default();
    let mut when_assignments: WhenAssignmentBranches = IndexMap::new();
    let mut main_statements = Vec::new();

    for statement in &algorithm.statements {
        let bare_statement = statement.as_unspanned();
        if let Statement::When { blocks, .. } = bare_statement {
            let statement_targets = collect_when_statement_target_branches(
                dae,
                flat,
                blocks,
                statement.source_span().unwrap_or(algorithm.span),
            )?;
            merge_algorithm_when_statement_branches(&mut when_assignments, statement_targets);
            continue;
        }
        main_statements.push(statement.clone());
    }

    for eq in lower_when_target_branches_to_event_equations(
        dae,
        when_assignments,
        algorithm.span,
        &algorithm.origin,
        allow_parameter_targets,
    )? {
        route_lowered_when_equation(dae, &mut lowered, eq);
    }

    let main_assignments = collect_algorithm_block_assignments(
        dae,
        flat,
        &main_statements,
        &IndexMap::new(),
        &HashSet::new(),
    )?;

    for (target, (_, value, span, origin)) in main_assignments {
        // Flat statements currently do not carry their own spans. Use the
        // enclosing algorithm span for generated DAE equations so diagnostics
        // remain anchored to source instead of Span::DUMMY (SPEC_0008).
        let span = if span == Span::DUMMY {
            algorithm.span
        } else {
            span
        };
        route_lowered_main_algorithm_assignment(
            dae,
            &mut lowered,
            LoweredMainAlgorithmAssignment {
                target,
                value,
                span,
                origin,
            },
            &algorithm.origin,
            allow_parameter_targets,
        )?;
    }

    Ok(lowered)
}

fn route_lowered_when_equation(
    dae: &Dae,
    lowered: &mut LoweredAlgorithmPartitions,
    eq: rumoca_ir_dae::Equation,
) {
    let Some(lhs) = eq.lhs.as_ref() else {
        return;
    };
    let lhs = dae_to_flat_var_name(lhs);
    match discrete_equation_bucket_for_lhs(dae, &lhs) {
        Some(DiscreteEquationBucket::DiscreteValued) => lowered.f_m.push(eq),
        Some(DiscreteEquationBucket::DiscreteReal) => lowered.f_z.push(eq),
        None => lowered.main.push(eq),
    }
}

struct LoweredMainAlgorithmAssignment {
    target: VarName,
    value: Expression,
    span: Span,
    origin: String,
}

fn route_lowered_main_algorithm_assignment(
    dae: &Dae,
    lowered: &mut LoweredAlgorithmPartitions,
    assignment: LoweredMainAlgorithmAssignment,
    algorithm_origin: &str,
    allow_parameter_target: bool,
) -> Result<(), String> {
    let LoweredMainAlgorithmAssignment {
        target,
        value,
        span,
        origin,
    } = assignment;
    if let Some(bucket) = discrete_variable_equation_bucket_for_lhs(dae, &target) {
        // MLS §11.1 algorithm assignments are equation-like model behavior.
        // MLS Appendix B stores discrete Real and discrete-valued variables in
        // B.1b/B.1c update partitions, so keep these out of continuous f_x.
        let eq = rumoca_ir_dae::Equation::explicit_with_scalar_count(
            flat_to_dae_var_name(&target),
            flat_to_dae_expression(&value),
            span,
            format!("{} ({})", origin, algorithm_origin),
            lookup_algorithm_target_scalar_count(dae, &target, span, allow_parameter_target)?,
        );
        match bucket {
            DiscreteEquationBucket::DiscreteValued => lowered.f_m.push(eq),
            DiscreteEquationBucket::DiscreteReal => lowered.f_z.push(eq),
        }
        return Ok(());
    }

    if allow_parameter_target {
        let target_key = flat_to_dae_var_name(&target);
        if let Some(var) = dae.variables.parameters.get(&target_key) {
            if var.fixed != Some(false) {
                return Err(format!(
                    "initial algorithm assignment target `{target}` is a fixed parameter; only parameter fixed=false may be solved during initialization at {:?}",
                    rumoca_core::span_to_source_span(span)
                ));
            }
            lowered
                .main
                .push(rumoca_ir_dae::Equation::explicit_with_scalar_count(
                    target_key,
                    flat_to_dae_expression(&value),
                    span,
                    format!("{} ({})", origin, algorithm_origin),
                    lookup_algorithm_target_scalar_count(dae, &target, span, true)?,
                ));
            return Ok(());
        }
    }

    let lhs = Expression::VarRef {
        name: target.clone().into(),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    };
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(value),
        span: rumoca_core::Span::DUMMY,
    };

    lowered.main.push(rumoca_ir_dae::Equation::residual_array(
        flat_to_dae_expression(&residual),
        span,
        format!("{} ({})", origin, algorithm_origin),
        lookup_algorithm_target_scalar_count(dae, &target, span, allow_parameter_target)?,
    ));
    Ok(())
}

pub(super) fn lower_algorithms_to_equations(dae: &mut Dae, flat: &Model) -> Result<(), ToDaeError> {
    for algorithm in &flat.algorithms {
        match lower_algorithm_to_equations(dae, flat, algorithm, false) {
            Ok(lowered) => {
                dae.continuous.equations.extend(lowered.main);
                dae.discrete.real_updates.extend(lowered.f_z);
                dae.discrete.valued_updates.extend(lowered.f_m);
            }
            Err(kind) => {
                return Err(ToDaeError::unsupported_algorithm(
                    "model".to_string(),
                    format!("{} (statement={kind})", algorithm.origin),
                    algorithm.span,
                ));
            }
        }
    }

    for algorithm in &flat.initial_algorithms {
        match lower_algorithm_to_equations(dae, flat, algorithm, true) {
            Ok(lowered) => {
                dae.initialization.equations.extend(lowered.main);
                // MLS §8.6 and §11.1: initial algorithms contribute equations
                // to the initialization problem. Discrete targets still use
                // the same Appendix B solved forms as model algorithms, but
                // they must initialize here rather than populate runtime event
                // update partitions.
                dae.initialization.equations.extend(lowered.f_z);
                dae.initialization.equations.extend(lowered.f_m);
            }
            Err(kind) => {
                return Err(ToDaeError::unsupported_algorithm(
                    "initial".to_string(),
                    format!("{} (statement={kind})", algorithm.origin),
                    algorithm.span,
                ));
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests;
