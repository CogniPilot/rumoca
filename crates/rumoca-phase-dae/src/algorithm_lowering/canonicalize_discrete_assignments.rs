use super::rewrite_discrete::{
    discrete_assignment_rhs_var_name, rewrite_discrete_self_refs_to_pre,
};
use super::*;

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
    rhs_target == lhs || super::discrete_equation_bucket_for_lhs(dae, rhs_target).is_none()
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
            if equations.len() <= 1
                || equations
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
