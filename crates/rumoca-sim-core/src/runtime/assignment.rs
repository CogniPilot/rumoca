use rumoca_eval_runtime::eval::{VarEnv, eval_expr};
use rumoca_eval_runtime::sim_float::SimFloat;
use rumoca_ir_dae as dae;
use std::collections::{HashMap, HashSet};

pub fn canonical_var_ref_key(name: &dae::VarName, subscripts: &[dae::Subscript]) -> Option<String> {
    if subscripts.is_empty() {
        return Some(name.as_str().to_string());
    }

    let mut index_parts = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        let idx = match sub {
            dae::Subscript::Index(i) => *i,
            dae::Subscript::Expr(expr) => match expr.as_ref() {
                dae::Expression::Literal(dae::Literal::Integer(i)) => *i,
                dae::Expression::Literal(dae::Literal::Real(v))
                    if v.is_finite() && v.fract() == 0.0 =>
                {
                    *v as i64
                }
                _ => return None,
            },
            _ => return None,
        };
        index_parts.push(idx.to_string());
    }

    Some(format!("{}[{}]", name.as_str(), index_parts.join(",")))
}

pub fn extract_direct_assignment(rhs: &dae::Expression) -> Option<(String, &dae::Expression)> {
    match rhs {
        dae::Expression::Binary {
            op: dae::OpBinary::Sub(_),
            lhs,
            rhs,
        } => {
            if let dae::Expression::VarRef { name, subscripts } = lhs.as_ref()
                && let Some(target) = canonical_var_ref_key(name, subscripts)
            {
                return Some((target, rhs.as_ref()));
            }
            if let dae::Expression::VarRef { name, subscripts } = rhs.as_ref()
                && let Some(target) = canonical_var_ref_key(name, subscripts)
            {
                return Some((target, lhs.as_ref()));
            }
            None
        }
        dae::Expression::Unary {
            op: dae::OpUnary::Minus(_),
            rhs,
        } => extract_direct_assignment(rhs),
        _ => None,
    }
}

pub fn direct_assignment_from_equation(eq: &dae::Equation) -> Option<(String, &dae::Expression)> {
    if let Some(lhs) = &eq.lhs {
        return Some((lhs.as_str().to_string(), &eq.rhs));
    }
    extract_direct_assignment(&eq.rhs)
}

fn is_zero_literal(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::Literal(dae::Literal::Integer(0)) => true,
        dae::Expression::Literal(dae::Literal::Real(v)) => v.abs() <= f64::EPSILON,
        _ => false,
    }
}

pub fn extract_active_assignment_from_expr<'a>(
    expr: &'a dae::Expression,
    env: &VarEnv<f64>,
) -> Option<(String, &'a dae::Expression)> {
    if let Some(assignment) = extract_direct_assignment(expr) {
        return Some(assignment);
    }
    match expr {
        dae::Expression::Unary {
            op: dae::OpUnary::Minus(_),
            rhs,
        } => extract_active_assignment_from_expr(rhs, env),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            for (condition, value) in branches {
                if rumoca_eval_runtime::eval::eval_expr::<f64>(condition, env).to_bool() {
                    return extract_active_assignment_from_expr(value, env);
                }
            }
            extract_active_assignment_from_expr(else_branch, env)
        }
        _ => None,
    }
}

pub fn extract_active_discrete_assignment<'a>(
    residual: &'a dae::Expression,
    env: &VarEnv<f64>,
) -> Option<(String, &'a dae::Expression)> {
    if let Some(assignment) = extract_direct_assignment(residual) {
        return Some(assignment);
    }
    if let Some(assignment) = extract_active_assignment_from_expr(residual, env) {
        return Some(assignment);
    }
    let dae::Expression::Binary {
        op: dae::OpBinary::Sub(_),
        lhs,
        rhs,
    } = residual
    else {
        return None;
    };
    if is_zero_literal(lhs.as_ref()) {
        return extract_active_assignment_from_expr(rhs, env);
    }
    if is_zero_literal(rhs.as_ref()) {
        return extract_active_assignment_from_expr(lhs, env);
    }
    None
}

pub fn discrete_assignment_from_equation<'a>(
    eq: &'a dae::Equation,
    env: &VarEnv<f64>,
) -> Option<(String, &'a dae::Expression)> {
    if let Some(lhs) = eq.lhs.as_ref() {
        return Some((lhs.as_str().to_string(), &eq.rhs));
    }
    extract_active_discrete_assignment(&eq.rhs, env)
}

pub fn extract_pre_assignment_target(expr: &dae::Expression) -> Option<String> {
    let dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Pre,
        args,
    } = expr
    else {
        return None;
    };
    let arg = args.first()?;
    let dae::Expression::VarRef { name, subscripts } = arg else {
        return None;
    };
    canonical_var_ref_key(name, subscripts)
}

pub fn extract_pre_assignment(rhs: &dae::Expression) -> Option<(String, &dae::Expression)> {
    match rhs {
        dae::Expression::Binary {
            op: dae::OpBinary::Sub(_),
            lhs,
            rhs,
        } => {
            if let Some(target) = extract_pre_assignment_target(lhs.as_ref()) {
                return Some((target, rhs.as_ref()));
            }
            if let Some(target) = extract_pre_assignment_target(rhs.as_ref()) {
                return Some((target, lhs.as_ref()));
            }
            None
        }
        dae::Expression::Unary {
            op: dae::OpUnary::Minus(_),
            rhs,
        } => extract_pre_assignment(rhs),
        _ => None,
    }
}

pub fn pre_assignment_from_initial_equation(
    eq: &dae::Equation,
) -> Option<(String, &dae::Expression)> {
    if eq.lhs.is_some() {
        return None;
    }
    extract_pre_assignment(&eq.rhs)
}

pub fn is_known_assignment_name(dae: &dae::Dae, raw: &str) -> bool {
    let key = dae::VarName::new(raw);
    if contains_assignment_name(dae, &key) {
        return true;
    }

    let Some(base) = dae::component_base_name(raw) else {
        return false;
    };
    let base_key = dae::VarName::new(base);
    contains_assignment_name(dae, &base_key)
}

pub fn is_runtime_unknown_name(dae: &dae::Dae, raw: &str) -> bool {
    let key = dae::VarName::new(raw);
    if contains_runtime_unknown_name(dae, &key) {
        return true;
    }

    let Some(base) = dae::component_base_name(raw) else {
        return false;
    };
    let base_key = dae::VarName::new(base);
    contains_runtime_unknown_name(dae, &base_key)
}

fn contains_assignment_name(dae: &dae::Dae, key: &dae::VarName) -> bool {
    dae.states.contains_key(key)
        || dae.algebraics.contains_key(key)
        || dae.outputs.contains_key(key)
        || dae.inputs.contains_key(key)
        || dae.parameters.contains_key(key)
        || dae.constants.contains_key(key)
        || dae.discrete_reals.contains_key(key)
        || dae.discrete_valued.contains_key(key)
        || dae.derivative_aliases.contains_key(key)
}

fn contains_runtime_unknown_name(dae: &dae::Dae, key: &dae::VarName) -> bool {
    dae.states.contains_key(key)
        || dae.algebraics.contains_key(key)
        || dae.outputs.contains_key(key)
        || dae.discrete_reals.contains_key(key)
        || dae.discrete_valued.contains_key(key)
}

pub fn variable_size_for_assignment_name(dae: &dae::Dae, name: &str) -> Option<usize> {
    if name.contains('[') {
        return Some(1);
    }
    let key = dae::VarName::new(name);
    dae.states
        .get(&key)
        .or_else(|| dae.algebraics.get(&key))
        .or_else(|| dae.outputs.get(&key))
        .or_else(|| dae.inputs.get(&key))
        .or_else(|| dae.parameters.get(&key))
        .or_else(|| dae.constants.get(&key))
        .or_else(|| dae.discrete_reals.get(&key))
        .or_else(|| dae.discrete_valued.get(&key))
        .or_else(|| dae.derivative_aliases.get(&key))
        .map(|var| var.size())
}

pub fn assignment_solution_is_alias_varref(dae: &dae::Dae, solution: &dae::Expression) -> bool {
    if let dae::Expression::VarRef { name, subscripts } = solution
        && let Some(source_key) = canonical_var_ref_key(name, subscripts)
    {
        return is_known_assignment_name(dae, source_key.as_str());
    }
    false
}

pub fn should_defer_alias_varref_assignment(
    dae: &dae::Dae,
    target: &str,
    solution: &dae::Expression,
) -> bool {
    let dae::Expression::VarRef { name, subscripts } = solution else {
        return false;
    };
    let Some(source_key) = canonical_var_ref_key(name, subscripts) else {
        return false;
    };
    if !is_known_assignment_name(dae, source_key.as_str()) {
        return false;
    }
    let target_size = variable_size_for_assignment_name(dae, target).unwrap_or(1);
    let source_size = variable_size_for_assignment_name(dae, source_key.as_str()).unwrap_or(1);
    target_size <= 1 && source_size <= 1
}

pub fn is_discrete_name(dae: &dae::Dae, name: &str) -> bool {
    let key = dae::VarName::new(name);
    dae.discrete_reals.contains_key(&key) || dae.discrete_valued.contains_key(&key)
}

pub fn extract_alias_pair(dae: &dae::Dae, rhs: &dae::Expression) -> Option<(String, String)> {
    let dae::Expression::Binary {
        op: dae::OpBinary::Sub(_),
        lhs,
        rhs,
    } = rhs
    else {
        return None;
    };
    let dae::Expression::VarRef {
        name: lhs_name,
        subscripts: lhs_subscripts,
    } = lhs.as_ref()
    else {
        return None;
    };
    let dae::Expression::VarRef {
        name: rhs_name,
        subscripts: rhs_subscripts,
    } = rhs.as_ref()
    else {
        return None;
    };
    let lhs_key = canonical_var_ref_key(lhs_name, lhs_subscripts)?;
    let rhs_key = canonical_var_ref_key(rhs_name, rhs_subscripts)?;
    if !is_known_assignment_name(dae, lhs_key.as_str())
        || !is_known_assignment_name(dae, rhs_key.as_str())
    {
        return None;
    }
    Some((lhs_key, rhs_key))
}

pub fn extract_alias_pair_from_equation(
    dae: &dae::Dae,
    eq: &dae::Equation,
) -> Option<(String, String)> {
    if let Some(lhs) = eq.lhs.as_ref()
        && let dae::Expression::VarRef {
            name: rhs_name,
            subscripts: rhs_subscripts,
        } = &eq.rhs
    {
        let rhs_key = canonical_var_ref_key(rhs_name, rhs_subscripts)?;
        let lhs_key = lhs.as_str().to_string();
        if is_known_assignment_name(dae, lhs_key.as_str())
            && is_known_assignment_name(dae, rhs_key.as_str())
        {
            return Some((lhs_key, rhs_key));
        }
    }
    extract_alias_pair(dae, &eq.rhs)
}

#[derive(Clone, Copy, Debug, Default)]
pub struct DirectAssignmentTargetStats {
    pub total: usize,
    pub non_alias: usize,
}

pub fn collect_direct_assignment_target_stats(
    dae: &dae::Dae,
    n_x: usize,
    skip_alias_pairs: bool,
) -> HashMap<String, DirectAssignmentTargetStats> {
    let mut stats: HashMap<String, DirectAssignmentTargetStats> = HashMap::new();
    for eq in dae.f_x.iter().skip(n_x) {
        if eq.origin == "orphaned_variable_pin" {
            continue;
        }
        let Some((target, solution)) = direct_assignment_from_equation(eq) else {
            continue;
        };
        let is_alias_solution = assignment_solution_is_alias_varref(dae, solution);
        if skip_alias_pairs && is_alias_solution {
            continue;
        }
        let entry = stats.entry(target).or_default();
        entry.total += 1;
        if !is_alias_solution {
            entry.non_alias += 1;
        }
    }
    stats
}

pub fn direct_assignment_source_is_known(
    dae: &dae::Dae,
    solution: &dae::Expression,
    n_x: usize,
    n_total: usize,
    mut solver_idx_for_target: impl FnMut(&str) -> Option<usize>,
) -> bool {
    let mut refs = HashSet::new();
    solution.collect_var_refs(&mut refs);
    refs.into_iter().all(|name| {
        let source = name.as_str();
        if source == "time" {
            return true;
        }
        if !is_known_assignment_name(dae, source) {
            return false;
        }

        // Runtime/IC direct-assignment seeding is only valid when RHS inputs
        // are already known at the current solve stage. If an RHS depends on
        // another unsolved solver unknown, directional seeding can pin stale
        // values and force the wrong algebraic branch.
        let unknown_idx = solver_idx_for_target(source).or_else(|| {
            let base = dae::component_base_name(source)?;
            solver_idx_for_target(base.as_str())
        });
        !unknown_idx.is_some_and(|idx| idx >= n_x && idx < n_total)
    })
}

fn indexed_values_with<F>(expected_len: usize, mut get_value: F) -> Option<Vec<f64>>
where
    F: FnMut(usize) -> Option<f64>,
{
    if expected_len <= 1 {
        return None;
    }
    let mut values = Vec::with_capacity(expected_len);
    for index in 1..=expected_len {
        values.push(get_value(index)?);
    }
    Some(values)
}

fn indexed_history_values_from_runtime(
    name: &dae::VarName,
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Option<Vec<f64>> {
    indexed_values_with(expected_len, |index| {
        let key = format!("{}[{}]", name.as_str(), index);
        rumoca_eval_runtime::eval::get_pre_value(&key)
            .or_else(|| env.vars.get(key.as_str()).copied())
    })
}

fn indexed_varref_values_from_env(
    name: &dae::VarName,
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Option<Vec<f64>> {
    indexed_values_with(expected_len, |index| {
        let key = format!("{}[{}]", name.as_str(), index);
        env.vars.get(key.as_str()).copied()
    })
}

fn eval_array_or_scalar_assignment(expr: &dae::Expression, env: &VarEnv<f64>) -> Vec<f64> {
    let values = rumoca_eval_runtime::eval::eval_array_values::<f64>(expr, env);
    if values.is_empty() {
        vec![rumoca_eval_runtime::eval::eval_expr::<f64>(expr, env)]
    } else {
        values
    }
}

fn active_if_branch<'a>(
    expr: &'a dae::Expression,
    env: &VarEnv<f64>,
) -> Option<&'a dae::Expression> {
    let dae::Expression::If {
        branches,
        else_branch,
    } = expr
    else {
        return None;
    };
    if let Some((_, value)) = branches
        .iter()
        .find(|(cond, _)| rumoca_eval_runtime::eval::eval_expr::<f64>(cond, env).to_bool())
    {
        return Some(value);
    }
    Some(else_branch)
}

fn eval_assignment_raw_values(
    expr: &dae::Expression,
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Vec<f64> {
    if let Some(branch_expr) = active_if_branch(expr, env) {
        return eval_assignment_raw_values(branch_expr, env, expected_len);
    }
    match expr {
        dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            if let Some(values) = indexed_varref_values_from_env(name, env, expected_len) {
                return values;
            }
            eval_array_or_scalar_assignment(expr, env)
        }
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Pre,
            args,
        } if args.len() == 1 => {
            if let dae::Expression::VarRef { name, subscripts } = &args[0]
                && subscripts.is_empty()
                && let Some(values) = indexed_history_values_from_runtime(name, env, expected_len)
            {
                return values;
            }
            eval_array_or_scalar_assignment(expr, env)
        }
        dae::Expression::FunctionCall { name, args, .. }
            if args.len() == 1
                && name
                    .as_str()
                    .rsplit('.')
                    .next()
                    .is_some_and(|short| short == "previous") =>
        {
            if let dae::Expression::VarRef {
                name: arg_name,
                subscripts,
            } = &args[0]
                && subscripts.is_empty()
                && let Some(values) =
                    indexed_history_values_from_runtime(arg_name, env, expected_len)
            {
                return values;
            }
            eval_array_or_scalar_assignment(expr, env)
        }
        _ => eval_array_or_scalar_assignment(expr, env),
    }
}

fn expand_values_to_size(raw: Vec<f64>, size: usize) -> Vec<f64> {
    if size == 0 {
        return Vec::new();
    }
    if raw.len() == size {
        return raw;
    }
    if raw.is_empty() {
        return vec![0.0; size];
    }
    if raw.len() == 1 {
        return vec![raw[0]; size];
    }
    let last = *raw.last().unwrap_or(&0.0);
    let mut out = Vec::with_capacity(size);
    for index in 0..size {
        out.push(raw.get(index).copied().unwrap_or(last));
    }
    out
}

pub fn evaluate_direct_assignment_values(
    solution: &dae::Expression,
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Vec<f64> {
    let raw = eval_assignment_raw_values(solution, env, expected_len);
    expand_values_to_size(raw, expected_len)
}

fn clamp_finite(v: f64) -> f64 {
    if v.is_finite() { v } else { 0.0 }
}

fn sync_solver_slot_value(y: &mut [f64], idx: usize, value: f64, updates: &mut usize) -> bool {
    if idx >= y.len() || (y[idx] - value).abs() <= 1e-12 {
        return false;
    }
    y[idx] = value;
    *updates += 1;
    true
}

fn sync_env_slot_value(env: &mut VarEnv<f64>, names: &[String], idx: usize, value: f64) -> bool {
    let Some(name) = names.get(idx) else {
        return false;
    };
    match env.vars.get(name.as_str()) {
        Some(existing) if (*existing - value).abs() <= 1e-12 => false,
        _ => {
            env.set(name, value);
            true
        }
    }
}

pub fn apply_values_to_indices(
    y: &mut [f64],
    env: &mut VarEnv<f64>,
    names: &[String],
    indices: &[usize],
    values: &[f64],
) -> (bool, usize) {
    let mut changed = false;
    let mut updates = 0usize;
    for (slot, idx) in indices.iter().copied().enumerate() {
        let value = clamp_finite(values.get(slot).copied().unwrap_or(0.0));
        changed |= sync_solver_slot_value(y, idx, value, &mut updates);
        changed |= sync_env_slot_value(env, names, idx, value);
    }
    (changed, updates)
}

pub fn apply_seeded_values_to_indices(
    y: &mut [f64],
    env: &mut VarEnv<f64>,
    names: &[String],
    indices: &[usize],
    values: &[f64],
    n_x: usize,
    mut on_seed_value: impl FnMut(&str, f64),
) -> (bool, usize) {
    let mut changed = false;
    let mut updates = 0usize;
    for (slot, idx_ref) in indices.iter().enumerate() {
        let var_idx = *idx_ref;
        if var_idx < n_x || var_idx >= y.len() {
            continue;
        }
        let value = clamp_finite(*values.get(slot).unwrap_or(&0.0));
        if (y[var_idx] - value).abs() <= 1e-12 {
            continue;
        }
        y[var_idx] = value;
        if let Some(name) = names.get(var_idx) {
            env.set(name, value);
            on_seed_value(name, value);
        }
        changed = true;
        updates += 1;
    }
    (changed, updates)
}

pub fn apply_runtime_values_to_indices(
    y: &mut [f64],
    env: &mut VarEnv<f64>,
    names: &[String],
    indices: &[usize],
    values: &[f64],
    n_x: usize,
) -> (bool, usize) {
    let mut changed = false;
    let mut updates = 0usize;
    for (slot, idx_ref) in indices.iter().enumerate() {
        let value = clamp_finite(*values.get(slot).unwrap_or(&0.0));
        let idx = *idx_ref;
        if idx >= n_x && idx < y.len() && (y[idx] - value).abs() > 1e-12 {
            y[idx] = value;
            changed = true;
            updates += 1;
        }
        if let Some(name) = names.get(idx)
            && env
                .vars
                .get(name)
                .is_none_or(|existing| (existing - value).abs() > 1e-12)
        {
            env.set(name, value);
            changed = true;
            updates += 1;
        }
    }
    (changed, updates)
}

pub fn propagate_runtime_direct_assignments_from_env(
    dae: &dae::Dae,
    y: &mut [f64],
    n_x: usize,
    env: &mut VarEnv<f64>,
) -> usize {
    if dae.f_x.len() <= n_x || y.is_empty() {
        return 0;
    }

    let crate::runtime::layout::SolverNameIndexMaps {
        names,
        name_to_idx,
        base_to_indices,
    } = crate::runtime::layout::build_solver_name_index_maps(dae, y.len());
    let target_assignment_stats = collect_direct_assignment_target_stats(dae, n_x, true);

    let mut updates = 0usize;
    let max_passes = y.len().max(4);
    for _ in 0..max_passes {
        let mut changed = false;
        for eq in dae.f_x.iter().skip(n_x) {
            if eq.origin == "orphaned_variable_pin" {
                continue;
            }
            let Some((target, solution)) = direct_assignment_from_equation(eq) else {
                continue;
            };
            // Alias equalities are solved via runtime alias-component propagation.
            if assignment_solution_is_alias_varref(dae, solution) {
                continue;
            }
            let target_stats = target_assignment_stats
                .get(target.as_str())
                .copied()
                .unwrap_or_default();
            if target_stats.total > 1 && target_stats.non_alias != 1 {
                continue;
            }

            if !target.contains('[')
                && let Some(indices) = base_to_indices.get(target.as_str())
                && indices.len() > 1
            {
                let values = evaluate_direct_assignment_values(solution, env, indices.len());
                let (branch_changed, branch_updates) =
                    apply_runtime_values_to_indices(y, env, &names, indices, &values, n_x);
                changed |= branch_changed;
                updates += branch_updates;
                continue;
            }

            let value = clamp_finite(eval_expr::<f64>(solution, env));
            if let Some(var_idx) =
                crate::runtime::layout::solver_idx_for_target(target.as_str(), &name_to_idx)
                && var_idx >= n_x
                && var_idx < y.len()
                && (y[var_idx] - value).abs() > 1e-12
            {
                y[var_idx] = value;
                changed = true;
                updates += 1;
            }
            if env
                .vars
                .get(target.as_str())
                .is_none_or(|existing| (existing - value).abs() > 1e-12)
            {
                env.set(target.as_str(), value);
                changed = true;
                updates += 1;
            }
        }
        if !changed {
            break;
        }
    }

    updates
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;

    fn test_scalar_var(name: &str) -> dae::Variable {
        dae::Variable {
            name: dae::VarName::new(name),
            ..Default::default()
        }
    }

    fn test_dae_with_vars() -> dae::Dae {
        let mut dae = dae::Dae::default();
        dae.algebraics
            .insert(dae::VarName::new("x"), test_scalar_var("x"));
        dae.algebraics
            .insert(dae::VarName::new("y"), test_scalar_var("y"));
        dae
    }

    #[test]
    fn canonical_var_ref_key_formats_indexed_refs() {
        let key = canonical_var_ref_key(
            &dae::VarName::new("x"),
            &[dae::Subscript::Index(2), dae::Subscript::Index(1)],
        )
        .expect("indexed key");
        assert_eq!(key, "x[2,1]");
    }

    #[test]
    fn extract_direct_assignment_handles_residual_orientation() {
        let expr = dae::Expression::Binary {
            op: dae::OpBinary::Sub(Default::default()),
            lhs: Box::new(dae::Expression::Literal(dae::Literal::Real(5.0))),
            rhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("y"),
                subscripts: vec![],
            }),
        };
        let (target, source) = extract_direct_assignment(&expr).expect("direct assignment");
        assert_eq!(target, "y");
        assert!(
            matches!(source, dae::Expression::Literal(dae::Literal::Real(v)) if (*v - 5.0).abs() < 1.0e-12)
        );
    }

    #[test]
    fn direct_assignment_from_equation_prefers_explicit_lhs() {
        let eq = dae::Equation::explicit(
            dae::VarName::new("z"),
            dae::Expression::Literal(dae::Literal::Integer(1)),
            Span::DUMMY,
            "test",
        );
        let (target, source) = direct_assignment_from_equation(&eq).expect("direct assignment");
        assert_eq!(target, "z");
        assert!(matches!(
            source,
            dae::Expression::Literal(dae::Literal::Integer(1))
        ));
    }

    #[test]
    fn apply_seeded_values_to_indices_updates_solver_slice() {
        let mut y = vec![0.0, 0.0, 0.0];
        let mut env = VarEnv::<f64>::new();
        let names = vec!["x".to_string(), "y".to_string(), "z".to_string()];
        let indices = vec![1usize, 2usize];
        let values = vec![4.0, 5.0];
        let mut seeded = Vec::new();
        let (changed, updates) = apply_seeded_values_to_indices(
            &mut y,
            &mut env,
            &names,
            &indices,
            &values,
            1,
            |name, value| seeded.push((name.to_string(), value)),
        );
        assert!(changed);
        assert_eq!(updates, 2);
        assert_eq!(y[1], 4.0);
        assert_eq!(y[2], 5.0);
        assert_eq!(seeded.len(), 2);
    }

    #[test]
    fn apply_values_to_indices_updates_solver_and_env_without_partition_gate() {
        let mut y = vec![0.0, 0.0, 0.0];
        let mut env = VarEnv::<f64>::new();
        let names = vec!["x".to_string(), "y".to_string(), "z".to_string()];
        let indices = vec![0usize, 2usize];
        let values = vec![4.0, 5.0];
        let (changed, updates) =
            apply_values_to_indices(&mut y, &mut env, &names, &indices, &values);
        assert!(changed);
        assert_eq!(updates, 2);
        assert_eq!(y[0], 4.0);
        assert_eq!(y[2], 5.0);
        assert_eq!(env.vars.get("x").copied().unwrap_or(0.0), 4.0);
        assert_eq!(env.vars.get("z").copied().unwrap_or(0.0), 5.0);
    }

    #[test]
    fn apply_runtime_values_to_indices_updates_env_even_without_solver_slot() {
        let mut y = vec![0.0, 0.0];
        let mut env = VarEnv::<f64>::new();
        let names = vec!["x".to_string(), "y".to_string(), "z".to_string()];
        let indices = vec![2usize];
        let values = vec![7.0];
        let (changed, updates) =
            apply_runtime_values_to_indices(&mut y, &mut env, &names, &indices, &values, 1);
        assert!(changed);
        assert_eq!(updates, 1);
        assert_eq!(env.vars.get("z").copied().unwrap_or(0.0), 7.0);
    }

    #[test]
    fn pre_assignment_from_initial_equation_extracts_pre_target() {
        let pre_x = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Pre,
            args: vec![dae::Expression::VarRef {
                name: dae::VarName::new("x"),
                subscripts: vec![],
            }],
        };
        let rhs = dae::Expression::Binary {
            op: dae::OpBinary::Sub(Default::default()),
            lhs: Box::new(pre_x),
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(2.0))),
        };
        let eq = dae::Equation::residual(rhs, Span::DUMMY, "test");
        let (target, source) =
            pre_assignment_from_initial_equation(&eq).expect("pre-assignment must resolve");
        assert_eq!(target, "x");
        assert!(matches!(
            source,
            dae::Expression::Literal(dae::Literal::Real(v)) if (*v - 2.0).abs() < 1.0e-12
        ));
    }

    #[test]
    fn is_known_assignment_name_accepts_indexed_names_for_known_bases() {
        let dae = test_dae_with_vars();
        assert!(is_known_assignment_name(&dae, "x"));
        assert!(is_known_assignment_name(&dae, "x[2]"));
        assert!(!is_known_assignment_name(&dae, "missing"));
    }

    #[test]
    fn collect_direct_assignment_target_stats_counts_alias_and_non_alias_rows() {
        let mut dae = test_dae_with_vars();
        dae.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: dae::OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("x"),
                    subscripts: vec![],
                }),
                rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(3.0))),
            },
            Span::DUMMY,
            "non_alias",
        ));
        dae.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: dae::OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("x"),
                    subscripts: vec![],
                }),
                rhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("y"),
                    subscripts: vec![],
                }),
            },
            Span::DUMMY,
            "alias",
        ));
        let stats = collect_direct_assignment_target_stats(&dae, 0, false);
        let x_stats = stats.get("x").copied().expect("x target stats");
        assert_eq!(x_stats.total, 2);
        assert_eq!(x_stats.non_alias, 1);
    }

    #[test]
    fn direct_assignment_source_is_known_rejects_unsolved_solver_unknown_refs() {
        let dae = test_dae_with_vars();
        let rhs = dae::Expression::VarRef {
            name: dae::VarName::new("y"),
            subscripts: vec![],
        };
        let known = direct_assignment_source_is_known(&dae, &rhs, 1, 3, |target| {
            if target == "y" { Some(1) } else { None }
        });
        assert!(!known);
    }

    #[test]
    fn extract_alias_pair_from_equation_detects_known_var_equalities() {
        let dae = test_dae_with_vars();
        let eq = dae::Equation::residual(
            dae::Expression::Binary {
                op: dae::OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("x"),
                    subscripts: vec![],
                }),
                rhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("y"),
                    subscripts: vec![],
                }),
            },
            Span::DUMMY,
            "alias",
        );
        let (lhs, rhs) = extract_alias_pair_from_equation(&dae, &eq).expect("alias pair");
        assert_eq!(lhs, "x");
        assert_eq!(rhs, "y");
    }

    #[test]
    fn is_discrete_name_only_matches_discrete_partitions() {
        let mut dae = test_dae_with_vars();
        dae.discrete_reals
            .insert(dae::VarName::new("d"), test_scalar_var("d"));
        assert!(is_discrete_name(&dae, "d"));
        assert!(!is_discrete_name(&dae, "x"));
    }

    #[test]
    fn evaluate_direct_assignment_values_expands_scalar_varref_to_indexed_values() {
        let mut env = VarEnv::<f64>::new();
        env.set("a[1]", 1.0);
        env.set("a[2]", 2.0);
        let expr = dae::Expression::VarRef {
            name: dae::VarName::new("a"),
            subscripts: vec![],
        };
        let values = evaluate_direct_assignment_values(&expr, &env, 2);
        assert_eq!(values, vec![1.0, 2.0]);
    }

    #[test]
    fn evaluate_direct_assignment_values_reads_pre_history_for_arrays() {
        rumoca_eval_runtime::eval::clear_pre_values();
        rumoca_eval_runtime::eval::set_pre_value("hist[1]", 3.0);
        rumoca_eval_runtime::eval::set_pre_value("hist[2]", 4.0);
        let env = VarEnv::<f64>::new();
        let expr = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Pre,
            args: vec![dae::Expression::VarRef {
                name: dae::VarName::new("hist"),
                subscripts: vec![],
            }],
        };
        let values = evaluate_direct_assignment_values(&expr, &env, 2);
        assert_eq!(values, vec![3.0, 4.0]);
        rumoca_eval_runtime::eval::clear_pre_values();
    }

    #[test]
    fn extract_active_discrete_assignment_selects_true_if_branch() {
        let mut env = VarEnv::<f64>::new();
        env.set("flag", 1.0);
        let residual = dae::Expression::Binary {
            op: dae::OpBinary::Sub(Default::default()),
            lhs: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
            rhs: Box::new(dae::Expression::If {
                branches: vec![(
                    dae::Expression::VarRef {
                        name: dae::VarName::new("flag"),
                        subscripts: vec![],
                    },
                    dae::Expression::Binary {
                        op: dae::OpBinary::Sub(Default::default()),
                        lhs: Box::new(dae::Expression::VarRef {
                            name: dae::VarName::new("x"),
                            subscripts: vec![],
                        }),
                        rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(2.0))),
                    },
                )],
                else_branch: Box::new(dae::Expression::Binary {
                    op: dae::OpBinary::Sub(Default::default()),
                    lhs: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new("x"),
                        subscripts: vec![],
                    }),
                    rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(3.0))),
                }),
            }),
        };

        let (target, source) =
            extract_active_discrete_assignment(&residual, &env).expect("active assignment");
        assert_eq!(target, "x");
        assert!(matches!(
            source,
            dae::Expression::Literal(dae::Literal::Real(v)) if (*v - 2.0).abs() < 1.0e-12
        ));
    }

    #[test]
    fn discrete_assignment_from_equation_prefers_explicit_lhs() {
        let mut env = VarEnv::<f64>::new();
        env.set("cond", 0.0);
        let eq = dae::Equation::explicit(
            dae::VarName::new("z"),
            dae::Expression::If {
                branches: vec![(
                    dae::Expression::VarRef {
                        name: dae::VarName::new("cond"),
                        subscripts: vec![],
                    },
                    dae::Expression::Literal(dae::Literal::Real(1.0)),
                )],
                else_branch: Box::new(dae::Expression::Literal(dae::Literal::Real(2.0))),
            },
            Span::DUMMY,
            "test",
        );

        let (target, source) =
            discrete_assignment_from_equation(&eq, &env).expect("explicit assignment");
        assert_eq!(target, "z");
        assert!(matches!(source, dae::Expression::If { .. }));
    }
}
