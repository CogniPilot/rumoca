use super::*;
use crate::when_guard::when_guard_activation_expr;
use crate::{
    dae_to_flat_expression, dae_to_flat_var_name, flat_to_dae_expression, flat_to_dae_var_name,
};
mod rewrite_discrete;
mod slice_lowering;
mod substitution;
mod target_names;
use rewrite_discrete::{discrete_assignment_rhs_var_name, rewrite_discrete_self_refs_to_pre};
use slice_lowering::lower_assignment_statement;
use substitution::*;
use target_names::{
    algorithm_assignment_base_with_subscripts, algorithm_assignment_target_name,
    algorithm_output_target_name, varref_with_subscripts,
};

enum DiscreteEquationBucket {
    DiscreteReal,
    DiscreteValued,
}

fn discrete_equation_bucket_for_lhs(dae: &Dae, lhs: &VarName) -> Option<DiscreteEquationBucket> {
    // MLS Appendix B notes reinit as a special case outside B.1b/B.1c.
    // Solver-facing DAE stores state resets in the event partition (`f_z`) so
    // runtime event updates can apply them without high-level when clauses.
    if dae.states.contains_key(&flat_to_dae_var_name(lhs))
        || subscript_fallback_chain(lhs)
            .into_iter()
            .any(|candidate| dae.states.contains_key(&flat_to_dae_var_name(&candidate)))
    {
        return Some(DiscreteEquationBucket::DiscreteReal);
    }
    if dae.discrete_valued.contains_key(&flat_to_dae_var_name(lhs))
        || subscript_fallback_chain(lhs).into_iter().any(|candidate| {
            dae.discrete_valued
                .contains_key(&flat_to_dae_var_name(&candidate))
        })
    {
        return Some(DiscreteEquationBucket::DiscreteValued);
    }
    if dae.discrete_reals.contains_key(&flat_to_dae_var_name(lhs))
        || subscript_fallback_chain(lhs).into_iter().any(|candidate| {
            dae.discrete_reals
                .contains_key(&flat_to_dae_var_name(&candidate))
        })
    {
        return Some(DiscreteEquationBucket::DiscreteReal);
    }
    None
}

fn guarded_when_rhs(
    dae: &Dae,
    when_condition: &Expression,
    lhs: &VarName,
    rhs: &Expression,
    use_pre_else: bool,
) -> Expression {
    let guard = when_guard_activation_expr(dae, when_condition);
    let else_expr = when_inactive_rhs(lhs, use_pre_else);
    Expression::If {
        branches: vec![(guard, rhs.clone())],
        else_branch: Box::new(else_expr),
    }
}

fn when_inactive_rhs(lhs: &VarName, use_pre_else: bool) -> Expression {
    if use_pre_else {
        // MLS §8.3.5.1 / §8.6: an ordinary when-equation is inactive at the
        // initial event unless explicitly driven by initial equations or
        // initial algorithms, so the current initial-section value must win
        // over the startup left-limit pre-store when the guard is false.
        return Expression::If {
            branches: vec![(
                Expression::BuiltinCall {
                    function: BuiltinFunction::Initial,
                    args: vec![],
                },
                Expression::VarRef {
                    name: lhs.clone(),
                    subscripts: vec![],
                },
            )],
            else_branch: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Pre,
                args: vec![Expression::VarRef {
                    name: lhs.clone(),
                    subscripts: vec![],
                }],
            }),
        };
    }

    Expression::VarRef {
        name: lhs.clone(),
        subscripts: vec![],
    }
}

pub(super) fn route_discrete_event_equations(
    dae: &mut Dae,
    when_clause: &rumoca_ir_dae::WhenClause,
) {
    let when_condition = dae_to_flat_expression(&when_clause.condition);
    for eq in &when_clause.equations {
        let Some(lhs) = &eq.lhs else {
            continue;
        };
        let lhs = dae_to_flat_var_name(lhs);
        let rhs = dae_to_flat_expression(&eq.rhs);
        let use_pre_else = !eq.origin.starts_with("reinit");
        let guarded = rumoca_ir_dae::Equation::explicit_with_scalar_count(
            flat_to_dae_var_name(&lhs),
            flat_to_dae_expression(&guarded_when_rhs(
                dae,
                &when_condition,
                &lhs,
                &rhs,
                use_pre_else,
            )),
            eq.span,
            format!("guarded {}", eq.origin),
            eq.scalar_count,
        );
        match discrete_equation_bucket_for_lhs(dae, &lhs) {
            Some(DiscreteEquationBucket::DiscreteValued) => dae.f_m.push(guarded),
            Some(DiscreteEquationBucket::DiscreteReal) => dae.f_z.push(guarded),
            None => {}
        }
    }
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
            eprintln!(
                "f_m canonicalize no-rhs-var lhs={} origin={}",
                lhs, equation.origin
            );
        }
        grouped.entry(lhs.clone()).or_default().push(equation);
        return false;
    };

    let Some(resolved_target) =
        resolve_collision_equation(dae, grouped, lhs, &equation, &rhs_target, flipped_once)
    else {
        if debug_canonicalize {
            eprintln!(
                "f_m canonicalize keep-collision lhs={} rhs={} origin={}",
                lhs, rhs_target, equation.origin
            );
        }
        grouped.entry(lhs.clone()).or_default().push(equation);
        return false;
    };

    if resolved_target == *lhs {
        if debug_canonicalize {
            eprintln!(
                "f_m canonicalize drop-redundant-edge lhs={} rhs={} origin={}",
                lhs, rhs_target, equation.origin
            );
        }
        return true;
    }

    flipped_once.insert(flip_edge_key(&equation.origin, lhs, &rhs_target));
    let mut flipped = equation;
    flipped.lhs = Some(flat_to_dae_var_name(&resolved_target));
    flipped.rhs = flat_to_dae_expression(&Expression::VarRef {
        name: lhs.clone(),
        subscripts: vec![],
    });
    if debug_canonicalize {
        eprintln!(
            "f_m canonicalize flip lhs={} -> lhs={} origin={}",
            lhs, resolved_target, flipped.origin
        );
    }
    grouped.entry(resolved_target).or_default().push(flipped);
    true
}

fn resolve_connection_alias_target_collisions(
    grouped: &mut IndexMap<VarName, Vec<rumoca_ir_dae::Equation>>,
    dae: &Dae,
) {
    let debug_canonicalize = std::env::var("RUMOCA_DEBUG_FM_CANON").is_ok();
    let anchored_targets = anchored_collision_targets(grouped);

    let mut iteration = 0usize;
    let max_iterations = 16usize;
    let mut flipped_once: std::collections::HashSet<(String, String, String)> =
        std::collections::HashSet::new();
    loop {
        iteration += 1;
        if iteration > max_iterations {
            if debug_canonicalize {
                eprintln!("f_m canonicalize collision-pass reached max iterations");
            }
            break;
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

            let keep_idx = candidate_keep_idx
                .unwrap_or(0)
                .min(current_equations.len() - 1);
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
}

pub(super) fn canonicalize_discrete_assignment_equations(dae: &mut Dae) {
    let debug_canonicalize = std::env::var("RUMOCA_DEBUG_FM_CANON").is_ok();
    let (mut grouped, passthrough) = drain_grouped_discrete_assignments(dae);
    reroute_connection_aliases_for_defined_targets(&mut grouped, dae);
    resolve_connection_alias_target_collisions(&mut grouped, dae);
    debug_log_grouped_duplicates(debug_canonicalize, &grouped);

    let mut rebuilt = Vec::with_capacity(passthrough.len() + grouped.len());
    rebuilt.extend(passthrough);
    for (lhs, equations) in grouped {
        rebuilt.extend(canonicalize_assignment_group(lhs, equations));
    }
    dae.f_m = rebuilt;
    debug_log_final_duplicates(debug_canonicalize, &dae.f_m);
}

fn drain_grouped_discrete_assignments(
    dae: &mut Dae,
) -> (
    IndexMap<VarName, Vec<rumoca_ir_dae::Equation>>,
    Vec<rumoca_ir_dae::Equation>,
) {
    let mut grouped: IndexMap<VarName, Vec<rumoca_ir_dae::Equation>> = IndexMap::new();
    let mut passthrough = Vec::new();
    for equation in dae.f_m.drain(..) {
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
                name: lhs.clone(),
                subscripts: vec![],
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
        let rewritten =
            rewrite_discrete_self_refs_to_pre(&dae_to_flat_expression(&equation.rhs), &lhs);
        equation.rhs = flat_to_dae_expression(&rewritten);
        return vec![equation];
    }

    if equations
        .iter()
        .any(|equation| !is_connection_equation_origin(&equation.origin))
    {
        equations.retain(|equation| !is_connection_equation_origin(&equation.origin));
    }

    let mut seen_origin = HashSet::<String>::default();
    equations.retain(|equation| seen_origin.insert(equation.origin.clone()));

    let mut seen_rhs = HashSet::<String>::default();
    let mut deduped = Vec::new();
    for mut equation in equations {
        let rewritten =
            rewrite_discrete_self_refs_to_pre(&dae_to_flat_expression(&equation.rhs), &lhs);
        equation.rhs = flat_to_dae_expression(&rewritten);
        let rhs_key = format!("{:?}", equation.rhs);
        if seen_rhs.insert(rhs_key) {
            deduped.push(equation);
        }
    }
    deduped
}

fn debug_log_grouped_duplicates(
    debug_canonicalize: bool,
    grouped: &IndexMap<VarName, Vec<rumoca_ir_dae::Equation>>,
) {
    if !debug_canonicalize {
        return;
    }
    for (lhs, equations) in grouped.iter().filter(|(_, eqs)| eqs.len() > 1) {
        eprintln!(
            "f_m canonicalize grouped-dup lhs={} count={} origins={:?}",
            lhs,
            equations.len(),
            equations
                .iter()
                .map(|eq| eq.origin.clone())
                .collect::<Vec<_>>()
        );
    }
}

fn debug_log_final_duplicates(debug_canonicalize: bool, equations: &[rumoca_ir_dae::Equation]) {
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
        eprintln!("f_m canonicalize final-dup lhs={} count={}", lhs, count);
    }
}

fn lookup_algorithm_target_scalar_count(dae: &Dae, target: &VarName) -> usize {
    let target_key = flat_to_dae_var_name(target);
    dae.states
        .get(&target_key)
        .or_else(|| dae.algebraics.get(&target_key))
        .or_else(|| dae.outputs.get(&target_key))
        .or_else(|| dae.inputs.get(&target_key))
        .or_else(|| dae.discrete_reals.get(&target_key))
        .or_else(|| dae.discrete_valued.get(&target_key))
        .or_else(|| dae.derivative_aliases.get(&target_key))
        .map(|var| var.size())
        .unwrap_or(1)
        .max(1)
}

fn bool_expr(value: bool) -> Expression {
    Expression::Literal(Literal::Boolean(value))
}

fn is_bool_expr(expr: &Expression, expected: bool) -> bool {
    matches!(expr, Expression::Literal(Literal::Boolean(v)) if *v == expected)
}

fn not_expr(expr: Expression) -> Expression {
    if let Expression::Literal(Literal::Boolean(flag)) = expr {
        return bool_expr(!flag);
    }
    Expression::Unary {
        op: rumoca_ir_core::OpUnary::Not(Default::default()),
        rhs: Box::new(expr),
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
        op: rumoca_ir_core::OpBinary::And(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
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
        op: rumoca_ir_core::OpBinary::Or(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn guarded_expr(guard: Expression, value: Expression, fallback: Expression) -> Expression {
    if is_bool_expr(&guard, true) {
        return value;
    }
    if is_bool_expr(&guard, false) {
        return fallback;
    }
    Expression::If {
        branches: vec![(guard, value)],
        else_branch: Box::new(fallback),
    }
}

fn eval_integer_expr(expr: &Expression, flat: &Model) -> Option<i64> {
    scalar_size::try_eval_flat_expr_i64(expr, flat, 0)
}

fn eval_for_range_values(range: &Expression, flat: &Model) -> Result<Vec<i64>, String> {
    const MAX_LOOP_EXPANSION: usize = 100_000;
    let (start, step, end) = match range {
        Expression::Range { start, step, end } => {
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
            let end_value = eval_integer_expr(range, flat)
                .ok_or_else(|| format!("ForRangeNotConstant({range:?})"))?;
            (1, 1, end_value)
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
    indices: &[flat::ForIndex],
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

fn resolve_multi_output_projection_names(
    flat: &Model,
    function_name: &VarName,
    requested_outputs: usize,
) -> Result<Vec<VarName>, String> {
    let Some(function) = resolve_flat_function(function_name, flat) else {
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
            name: target.clone(),
            subscripts: vec![],
        }],
    }
}

fn current_target_expr(target: &VarName) -> Expression {
    Expression::VarRef {
        name: target.clone(),
        subscripts: vec![],
    }
}

fn rewrite_algorithm_current_refs(
    dae: &Dae,
    expr: &Expression,
    current_values: &IndexMap<VarName, Expression>,
    known_targets: &HashSet<VarName>,
) -> Expression {
    match expr {
        Expression::VarRef { name, subscripts } => {
            rewrite_algorithm_current_var_ref(dae, name, subscripts, current_values, known_targets)
        }
        Expression::Binary { op, lhs, rhs } => Expression::Binary {
            op: op.clone(),
            lhs: rewrite_algorithm_current_box(dae, lhs, current_values, known_targets),
            rhs: rewrite_algorithm_current_box(dae, rhs, current_values, known_targets),
        },
        Expression::Unary { op, rhs } => Expression::Unary {
            op: op.clone(),
            rhs: rewrite_algorithm_current_box(dae, rhs, current_values, known_targets),
        },
        Expression::BuiltinCall { function, args } => {
            if matches!(function, BuiltinFunction::Pre) {
                return Expression::BuiltinCall {
                    function: *function,
                    args: args.clone(),
                };
            }
            Expression::BuiltinCall {
                function: *function,
                args: rewrite_algorithm_expr_vec(dae, args, current_values, known_targets),
            }
        }
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => Expression::FunctionCall {
            name: name.clone(),
            args: rewrite_algorithm_expr_vec(dae, args, current_values, known_targets),
            is_constructor: *is_constructor,
        },
        Expression::If {
            branches,
            else_branch,
        } => rewrite_algorithm_current_if_expr(
            dae,
            branches,
            else_branch,
            current_values,
            known_targets,
        ),
        Expression::Array {
            elements,
            is_matrix,
        } => Expression::Array {
            elements: rewrite_algorithm_expr_vec(dae, elements, current_values, known_targets),
            is_matrix: *is_matrix,
        },
        Expression::Tuple { elements } => Expression::Tuple {
            elements: rewrite_algorithm_expr_vec(dae, elements, current_values, known_targets),
        },
        Expression::Range { start, step, end } => rewrite_algorithm_current_range_expr(
            dae,
            start,
            step.as_deref(),
            end,
            current_values,
            known_targets,
        ),
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => rewrite_algorithm_current_comprehension_expr(
            dae,
            expr,
            indices,
            filter.as_deref(),
            current_values,
            known_targets,
        ),
        Expression::Index { base, subscripts } => Expression::Index {
            base: rewrite_algorithm_current_box(dae, base, current_values, known_targets),
            subscripts: subscripts.to_vec(),
        },
        Expression::FieldAccess { base, field } => Expression::FieldAccess {
            base: rewrite_algorithm_current_box(dae, base, current_values, known_targets),
            field: field.clone(),
        },
        Expression::Literal(literal) => Expression::Literal(literal.clone()),
        Expression::Empty => Expression::Empty,
    }
}

fn rewrite_algorithm_current_box(
    dae: &Dae,
    expr: &Expression,
    current_values: &IndexMap<VarName, Expression>,
    known_targets: &HashSet<VarName>,
) -> Box<Expression> {
    Box::new(rewrite_algorithm_current_refs(
        dae,
        expr,
        current_values,
        known_targets,
    ))
}

fn rewrite_algorithm_current_var_ref(
    dae: &Dae,
    name: &VarName,
    subscripts: &[Subscript],
    current_values: &IndexMap<VarName, Expression>,
    known_targets: &HashSet<VarName>,
) -> Expression {
    let target = varref_with_subscripts(name, subscripts);
    current_values.get(&target).cloned().unwrap_or_else(|| {
        if known_targets.contains(&target) {
            algorithm_if_fallback_expr(dae, &target)
        } else {
            Expression::VarRef {
                name: name.clone(),
                subscripts: subscripts.to_vec(),
            }
        }
    })
}

fn rewrite_algorithm_expr_vec(
    dae: &Dae,
    exprs: &[Expression],
    current_values: &IndexMap<VarName, Expression>,
    known_targets: &HashSet<VarName>,
) -> Vec<Expression> {
    exprs
        .iter()
        .map(|expr| rewrite_algorithm_current_refs(dae, expr, current_values, known_targets))
        .collect()
}

fn rewrite_algorithm_current_if_expr(
    dae: &Dae,
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    current_values: &IndexMap<VarName, Expression>,
    known_targets: &HashSet<VarName>,
) -> Expression {
    Expression::If {
        branches: branches
            .iter()
            .map(|(condition, value)| {
                (
                    rewrite_algorithm_current_refs(dae, condition, current_values, known_targets),
                    rewrite_algorithm_current_refs(dae, value, current_values, known_targets),
                )
            })
            .collect(),
        else_branch: Box::new(rewrite_algorithm_current_refs(
            dae,
            else_branch,
            current_values,
            known_targets,
        )),
    }
}

fn rewrite_algorithm_current_range_expr(
    dae: &Dae,
    start: &Expression,
    step: Option<&Expression>,
    end: &Expression,
    current_values: &IndexMap<VarName, Expression>,
    known_targets: &HashSet<VarName>,
) -> Expression {
    Expression::Range {
        start: Box::new(rewrite_algorithm_current_refs(
            dae,
            start,
            current_values,
            known_targets,
        )),
        step: step.map(|value| {
            Box::new(rewrite_algorithm_current_refs(
                dae,
                value,
                current_values,
                known_targets,
            ))
        }),
        end: Box::new(rewrite_algorithm_current_refs(
            dae,
            end,
            current_values,
            known_targets,
        )),
    }
}

fn rewrite_algorithm_current_comprehension_expr(
    dae: &Dae,
    expr: &Expression,
    indices: &[ComprehensionIndex],
    filter: Option<&Expression>,
    current_values: &IndexMap<VarName, Expression>,
    known_targets: &HashSet<VarName>,
) -> Expression {
    Expression::ArrayComprehension {
        expr: Box::new(rewrite_algorithm_current_refs(
            dae,
            expr,
            current_values,
            known_targets,
        )),
        indices: indices
            .iter()
            .map(|index| ComprehensionIndex {
                name: index.name.clone(),
                range: rewrite_algorithm_current_refs(
                    dae,
                    &index.range,
                    current_values,
                    known_targets,
                ),
            })
            .collect(),
        filter: filter.map(|value| {
            Box::new(rewrite_algorithm_current_refs(
                dae,
                value,
                current_values,
                known_targets,
            ))
        }),
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
        .discrete_reals
        .contains_key(&flat_to_dae_var_name(target))
        || dae
            .discrete_valued
            .contains_key(&flat_to_dae_var_name(target))
        || subscript_fallback_chain(target)
            .into_iter()
            .any(|candidate| {
                dae.discrete_reals
                    .contains_key(&flat_to_dae_var_name(&candidate))
                    || dae
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
        Statement::Assignment { comp, value } => Some((
            algorithm_assignment_target_name(comp)?,
            value.clone(),
            Span::DUMMY,
            "algorithm assignment".to_string(),
        )),
        Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => {
            let [output] = outputs.as_slice() else {
                return None;
            };
            let target = algorithm_output_target_name(output)?;
            Some((
                target,
                Expression::FunctionCall {
                    name: component_reference_to_var_name(comp),
                    args: args.clone(),
                    is_constructor: false,
                },
                Span::DUMMY,
                "algorithm function call assignment".to_string(),
            ))
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            let mut branches = Vec::new();
            let mut target: Option<VarName> = None;
            for block in cond_blocks {
                let [single] = block.stmts.as_slice() else {
                    return None;
                };
                let (block_target, block_value, _, _) =
                    algorithm_assignment_to_target_expr(dae, single)?;
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
            let else_value = if let Some(else_stmts) = else_block.as_ref() {
                let [else_single] = else_stmts.as_slice() else {
                    return None;
                };
                let (else_target, else_value, _, _) =
                    algorithm_assignment_to_target_expr(dae, else_single)?;
                if target.as_ref() != Some(&else_target) {
                    return None;
                }
                else_value
            } else {
                let target_name = target.as_ref()?;
                if !dae
                    .discrete_reals
                    .contains_key(&flat_to_dae_var_name(target_name))
                    && !dae
                        .discrete_valued
                        .contains_key(&flat_to_dae_var_name(target_name))
                {
                    return None;
                }
                pre_target_expr(target_name)
            };
            Some((
                target?,
                Expression::If {
                    branches,
                    else_branch: Box::new(else_value),
                },
                Span::DUMMY,
                "algorithm if-assignment".to_string(),
            ))
        }
        Statement::Empty => None,
        Statement::For { .. }
        | Statement::While(_)
        | Statement::When(_)
        | Statement::Reinit { .. }
        | Statement::Assert { .. }
        | Statement::Return
        | Statement::Break => None,
    }
}

type AlgorithmAssignment = (VarName, Expression, Span, String);

fn is_noop_algorithm_statement(statement: &Statement) -> bool {
    match statement {
        Statement::Empty | Statement::Assert { .. } | Statement::Return | Statement::Break => true,
        Statement::FunctionCall { outputs, .. } => outputs.is_empty(),
        Statement::For { equations, .. } => equations.iter().all(is_noop_algorithm_statement),
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            cond_blocks
                .iter()
                .all(|block| block.stmts.iter().all(is_noop_algorithm_statement))
                && else_block
                    .as_ref()
                    .is_none_or(|stmts| stmts.iter().all(is_noop_algorithm_statement))
        }
        Statement::When(blocks) => blocks
            .iter()
            .all(|block| block.stmts.iter().all(is_noop_algorithm_statement)),
        Statement::Assignment { .. } | Statement::While(_) | Statement::Reinit { .. } => false,
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
        Statement::Assignment { comp, value } => Ok(lower_assignment_statement(flat, comp, value)?
            .into_iter()
            .map(|(target, _, _, _)| target)
            .collect()),
        Statement::If {
            cond_blocks,
            else_block,
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
        Statement::For { indices, equations } => {
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
    Ok(assignments)
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

    let else_assignments = else_block
        .as_ref()
        .map(|statements| {
            collect_algorithm_block_assignments(
                dae,
                flat,
                statements,
                outer_current_values,
                outer_known_targets,
            )
        })
        .transpose()?
        .unwrap_or_default();

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
                    .unwrap_or_else(|| algorithm_if_fallback_expr(dae, &target));
                (condition.clone(), rhs)
            })
            .collect();
        let else_rhs = else_assignments
            .get(&target)
            .map(|(_, value, _, _)| value.clone())
            .unwrap_or_else(|| algorithm_if_fallback_expr(dae, &target));
        lowered.push((
            target,
            Expression::If {
                branches,
                else_branch: Box::new(else_rhs),
            },
            Span::DUMMY,
            "algorithm if-assignment".to_string(),
        ));
    }

    Ok(lowered)
}

#[derive(Default)]
struct ForLoopLowerState {
    assignments: IndexMap<VarName, Expression>,
    known_targets: HashSet<VarName>,
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
) {
    let rewritten =
        rewrite_algorithm_current_refs(dae, &value, &state.assignments, &state.known_targets);
    let fallback = state
        .assignments
        .get(&target)
        .cloned()
        .unwrap_or_else(|| algorithm_if_fallback_expr(dae, &target));
    let merged = guarded_expr(guard, rewritten, fallback);
    let normalized = normalize_algorithm_current_value(dae, &target, &merged);
    state.assignments.insert(target, normalized);
}

fn mark_loop_break(state: &mut ForLoopLowerState, guard: Expression) {
    state.break_condition = Some(match state.break_condition.take() {
        Some(existing) => or_expr(existing, guard),
        None => guard,
    });
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
    let mut taken = bool_expr(false);
    for block in cond_blocks {
        let cond = substitute_expr_with_bindings(&block.cond, bindings);
        let branch_guard = and_expr(
            guard.clone(),
            and_expr(not_expr(taken.clone()), cond.clone()),
        );
        lower_for_statement_sequence_with_guard(
            dae,
            flat,
            &block.stmts,
            branch_guard,
            bindings,
            state,
        )?;
        taken = or_expr(taken, cond);
    }

    if let Some(stmts) = else_block {
        let else_guard = and_expr(guard, not_expr(taken));
        lower_for_statement_sequence_with_guard(dae, flat, stmts, else_guard, bindings, state)?;
    }
    Ok(())
}

fn lower_nested_for_with_guard(
    dae: &Dae,
    flat: &Model,
    indices: &[flat::ForIndex],
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
        Statement::Break => {
            mark_loop_break(state, active_guard);
            Ok(())
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => lower_for_if_with_guard(
            dae,
            flat,
            cond_blocks,
            else_block,
            active_guard,
            bindings,
            state,
        ),
        Statement::For { indices, equations } => lower_nested_for_with_guard(
            dae,
            flat,
            indices,
            equations,
            active_guard,
            bindings,
            state,
        ),
        Statement::While(_) => Err("ForContainsWhile".to_string()),
        Statement::When(_) => Err("ForContainsWhen".to_string()),
        _ => {
            let lowered = lower_statement_assignments(dae, flat, &substituted)?;
            for (target, value, _, _) in lowered {
                apply_guarded_loop_assignment(dae, state, target, value, active_guard.clone());
            }
            Ok(())
        }
    }
}

fn lower_for_statement_assignments(
    dae: &Dae,
    flat: &Model,
    indices: &[flat::ForIndex],
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
        Statement::Assignment { comp, value } => lower_assignment_statement(flat, comp, value),
        Statement::When(_) => Err("When".to_string()),
        Statement::If {
            cond_blocks,
            else_block,
        } => lower_if_statement_assignments(
            dae,
            flat,
            cond_blocks,
            else_block,
            current_values,
            known_targets,
        ),
        Statement::For { indices, equations } => lower_for_statement_assignments(
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
        } if outputs.len() > 1 => {
            let function_name = component_reference_to_var_name(comp);
            let projection_names =
                resolve_multi_output_projection_names(flat, &function_name, outputs.len())?;
            let mut lowered = Vec::with_capacity(outputs.len());
            for (output_expr, projection_name) in outputs.iter().zip(projection_names.iter()) {
                let Some(target) = algorithm_output_target_name(output_expr) else {
                    return Err("FunctionCallMultiOutputTarget".to_string());
                };
                lowered.push((
                    target,
                    Expression::FunctionCall {
                        name: projection_name.clone(),
                        args: args.clone(),
                        is_constructor: false,
                    },
                    Span::DUMMY,
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
        Statement::While(_) => "While",
        Statement::When(_) => "When",
        Statement::FunctionCall { .. } => "FunctionCall",
        Statement::Reinit { .. } => "Reinit",
        Statement::Assert { .. } => "Assert",
        Statement::Return => "Return",
        Statement::Break => "Break",
        Statement::Empty => "Empty",
    }
}

#[derive(Default)]
struct LoweredAlgorithmPartitions {
    main: Vec<rumoca_ir_dae::Equation>,
    f_z: Vec<rumoca_ir_dae::Equation>,
    f_m: Vec<rumoca_ir_dae::Equation>,
}

type WhenAssignmentBranches = IndexMap<VarName, Vec<(Expression, Expression)>>;

fn collect_when_statement_target_branches(
    dae: &Dae,
    flat: &Model,
    blocks: &[StatementBlock],
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
                .or_default()
                .push((when_guard_activation_expr(dae, &block.cond), value));
        }
    }

    Ok(targets)
}

fn merge_algorithm_when_statement_branches(
    merged_targets: &mut WhenAssignmentBranches,
    statement_targets: WhenAssignmentBranches,
) {
    for (target, mut new_branches) in statement_targets {
        if let Some(existing) = merged_targets.get_mut(&target) {
            // MLS §11.1 / §11.2: algorithm statements execute in source order,
            // so later when-statements must override earlier assignments to the
            // same target when both fire at the same event instant.
            new_branches.extend(std::mem::take(existing));
            *existing = new_branches;
        } else {
            merged_targets.insert(target, new_branches);
        }
    }
}

fn lower_when_target_branches_to_event_equations(
    dae: &Dae,
    targets: WhenAssignmentBranches,
    algorithm_origin: &str,
) -> Vec<rumoca_ir_dae::Equation> {
    let mut lowered = Vec::with_capacity(targets.len());
    for (target, branches) in targets {
        let eq = rumoca_ir_dae::Equation::explicit_with_scalar_count(
            flat_to_dae_var_name(&target),
            flat_to_dae_expression(&Expression::If {
                branches,
                else_branch: Box::new(when_inactive_rhs(&target, true)),
            }),
            Span::DUMMY,
            format!("algorithm when-assignment ({algorithm_origin})"),
            lookup_algorithm_target_scalar_count(dae, &target),
        );
        lowered.push(eq);
    }

    lowered
}

fn lower_algorithm_to_equations(
    dae: &Dae,
    flat: &Model,
    algorithm: &rumoca_ir_flat::Algorithm,
) -> Result<LoweredAlgorithmPartitions, String> {
    if algorithm.statements.is_empty() {
        return Ok(LoweredAlgorithmPartitions::default());
    }

    let mut lowered = LoweredAlgorithmPartitions::default();
    let mut when_assignments: WhenAssignmentBranches = IndexMap::new();
    let mut main_statements = Vec::new();

    for statement in &algorithm.statements {
        if let Statement::When(blocks) = statement {
            let statement_targets = collect_when_statement_target_branches(dae, flat, blocks)?;
            merge_algorithm_when_statement_branches(&mut when_assignments, statement_targets);
            continue;
        }
        main_statements.push(statement.clone());
    }

    for eq in
        lower_when_target_branches_to_event_equations(dae, when_assignments, &algorithm.origin)
    {
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
        let lhs = Expression::VarRef {
            name: target.clone(),
            subscripts: vec![],
        };
        let residual = Expression::Binary {
            op: rumoca_ir_flat::OpBinary::Sub(Default::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(value),
        };

        lowered.main.push(rumoca_ir_dae::Equation::residual_array(
            flat_to_dae_expression(&residual),
            span,
            format!("{} ({})", origin, algorithm.origin),
            lookup_algorithm_target_scalar_count(dae, &target),
        ));
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

pub(super) fn lower_algorithms_to_equations(dae: &mut Dae, flat: &Model) -> Result<(), ToDaeError> {
    for algorithm in &flat.algorithms {
        match lower_algorithm_to_equations(dae, flat, algorithm) {
            Ok(lowered) => {
                dae.f_x.extend(lowered.main);
                dae.f_z.extend(lowered.f_z);
                dae.f_m.extend(lowered.f_m);
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
        match lower_algorithm_to_equations(dae, flat, algorithm) {
            Ok(lowered) => {
                if !lowered.f_z.is_empty() || !lowered.f_m.is_empty() {
                    return Err(ToDaeError::unsupported_algorithm(
                        "initial".to_string(),
                        format!("{} (statement=WhenInInitial)", algorithm.origin),
                        algorithm.span,
                    ));
                }
                dae.initial_equations.extend(lowered.main);
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
