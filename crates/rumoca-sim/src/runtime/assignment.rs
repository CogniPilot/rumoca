use rumoca_eval_dae::runtime::VarEnv;
use rumoca_ir_dae as dae;
use std::collections::{HashMap, HashSet};

use crate::runtime::scalar_eval::{eval_scalar_bool_expr_fast, eval_scalar_expr_fast};

mod fast_eval;

pub(crate) use fast_eval::eval_assignment_scalar_fast;
pub use fast_eval::evaluate_direct_assignment_values;

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

fn integer_like_subscript_index(value: f64) -> Option<i64> {
    if !value.is_finite() {
        return None;
    }
    let rounded = value.round();
    let tol = 1.0e-9 * rounded.abs().max(1.0);
    ((value - rounded).abs() <= tol).then_some(rounded as i64)
}

fn canonical_var_ref_key_with_env(
    name: &dae::VarName,
    subscripts: &[dae::Subscript],
    env: &VarEnv<f64>,
) -> Option<String> {
    if subscripts.is_empty() {
        return Some(name.as_str().to_string());
    }

    let mut index_parts = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        let idx = match sub {
            dae::Subscript::Index(i) => *i,
            dae::Subscript::Expr(expr) => eval_scalar_expr_fast(expr, env)
                .or_else(|| Some(rumoca_eval_dae::runtime::eval_expr::<f64>(expr, env)))
                .and_then(integer_like_subscript_index)?,
            dae::Subscript::Colon => return None,
        };
        index_parts.push(idx.to_string());
    }

    Some(format!("{}[{}]", name.as_str(), index_parts.join(",")))
}

pub fn extract_direct_assignment(rhs: &dae::Expression) -> Option<(String, &dae::Expression)> {
    match rhs {
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(_),
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
            op: rumoca_ir_core::OpUnary::Minus(_),
            rhs,
        } => extract_direct_assignment(rhs),
        _ => None,
    }
}

fn extract_direct_assignment_with_guard_env<'a>(
    rhs: &'a dae::Expression,
    guard_env: &VarEnv<f64>,
) -> Option<(String, &'a dae::Expression)> {
    match rhs {
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(_),
            lhs,
            rhs,
        } => {
            if let dae::Expression::VarRef { name, subscripts } = lhs.as_ref()
                && let Some(target) = canonical_var_ref_key_with_env(name, subscripts, guard_env)
            {
                return Some((target, rhs.as_ref()));
            }
            if let dae::Expression::VarRef { name, subscripts } = rhs.as_ref()
                && let Some(target) = canonical_var_ref_key_with_env(name, subscripts, guard_env)
            {
                return Some((target, lhs.as_ref()));
            }
            None
        }
        dae::Expression::Unary {
            op: rumoca_ir_core::OpUnary::Minus(_),
            rhs,
        } => extract_direct_assignment_with_guard_env(rhs, guard_env),
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
    extract_active_assignment_from_expr_with_guard_env(expr, env)
}

pub fn extract_active_assignment_from_expr_with_guard_env<'a>(
    expr: &'a dae::Expression,
    guard_env: &VarEnv<f64>,
) -> Option<(String, &'a dae::Expression)> {
    if let Some(assignment) = extract_direct_assignment_with_guard_env(expr, guard_env) {
        return Some(assignment);
    }
    match expr {
        dae::Expression::Unary {
            op: rumoca_ir_core::OpUnary::Minus(_),
            rhs,
        } => extract_active_assignment_from_expr_with_guard_env(rhs, guard_env),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            for (condition, value) in branches {
                match eval_scalar_bool_expr_fast(condition, guard_env) {
                    Some(true) => {
                        return extract_active_assignment_from_expr_with_guard_env(
                            value, guard_env,
                        );
                    }
                    Some(false) => continue,
                    None => return None,
                }
            }
            extract_active_assignment_from_expr_with_guard_env(else_branch, guard_env)
        }
        _ => None,
    }
}

pub fn extract_active_discrete_assignment<'a>(
    residual: &'a dae::Expression,
    env: &VarEnv<f64>,
) -> Option<(String, &'a dae::Expression)> {
    extract_active_discrete_assignment_with_guard_env(residual, env)
}

pub fn extract_active_discrete_assignment_with_guard_env<'a>(
    residual: &'a dae::Expression,
    guard_env: &VarEnv<f64>,
) -> Option<(String, &'a dae::Expression)> {
    if let Some(assignment) = extract_direct_assignment_with_guard_env(residual, guard_env) {
        return Some(assignment);
    }
    if let Some(assignment) =
        extract_active_assignment_from_expr_with_guard_env(residual, guard_env)
    {
        return Some(assignment);
    }
    let dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(_),
        lhs,
        rhs,
    } = residual
    else {
        return None;
    };
    if is_zero_literal(lhs.as_ref()) {
        return extract_active_assignment_from_expr_with_guard_env(rhs, guard_env);
    }
    if is_zero_literal(rhs.as_ref()) {
        return extract_active_assignment_from_expr_with_guard_env(lhs, guard_env);
    }
    None
}

pub fn discrete_assignment_from_equation<'a>(
    eq: &'a dae::Equation,
    env: &VarEnv<f64>,
) -> Option<(String, &'a dae::Expression)> {
    discrete_assignment_from_equation_with_guard_env(eq, env)
}

pub fn discrete_assignment_from_equation_with_guard_env<'a>(
    eq: &'a dae::Equation,
    guard_env: &VarEnv<f64>,
) -> Option<(String, &'a dae::Expression)> {
    if let Some(lhs) = eq.lhs.as_ref() {
        return Some((lhs.as_str().to_string(), &eq.rhs));
    }
    extract_active_discrete_assignment_with_guard_env(&eq.rhs, guard_env)
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
            op: rumoca_ir_core::OpBinary::Sub(_),
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
            op: rumoca_ir_core::OpUnary::Minus(_),
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
        op: rumoca_ir_core::OpBinary::Sub(_),
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

#[derive(Clone, Debug)]
struct RuntimeStateDerivativeSource {
    alias_name: String,
    state_backed: bool,
}

fn extract_derivative_state_key(expr: &dae::Expression) -> Option<String> {
    let dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Der,
        args,
    } = expr
    else {
        return None;
    };
    let dae::Expression::VarRef { name, subscripts } = args.first()? else {
        return None;
    };
    canonical_var_ref_key(name, subscripts)
}

fn unwrap_runtime_derivative_residual(expr: &dae::Expression) -> &dae::Expression {
    match expr {
        dae::Expression::Unary {
            op: rumoca_ir_core::OpUnary::Minus(_),
            rhs,
        } => unwrap_runtime_derivative_residual(rhs),
        _ => expr,
    }
}

fn extract_runtime_state_derivative_alias(eq: &dae::Equation) -> Option<(String, String)> {
    if let Some(lhs) = eq.lhs.as_ref()
        && let Some(state_key) = extract_derivative_state_key(&eq.rhs)
    {
        return Some((state_key, lhs.as_str().to_string()));
    }

    let residual = unwrap_runtime_derivative_residual(&eq.rhs);
    let dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(_),
        lhs,
        rhs,
    } = residual
    else {
        return None;
    };

    if let dae::Expression::VarRef {
        name: alias_name,
        subscripts,
    } = lhs.as_ref()
        && let Some(state_key) = extract_derivative_state_key(rhs)
    {
        return Some((state_key, canonical_var_ref_key(alias_name, subscripts)?));
    }

    if let dae::Expression::VarRef {
        name: alias_name,
        subscripts,
    } = rhs.as_ref()
        && let Some(state_key) = extract_derivative_state_key(lhs)
    {
        return Some((state_key, canonical_var_ref_key(alias_name, subscripts)?));
    }

    None
}

fn is_state_assignment_name(dae: &dae::Dae, raw: &str) -> bool {
    let key = dae::VarName::new(raw);
    if dae.states.contains_key(&key) {
        return true;
    }

    dae::component_base_name(raw)
        .map(|base| dae.states.contains_key(&dae::VarName::new(base)))
        .unwrap_or(false)
}

fn build_runtime_state_derivative_sources(
    dae: &dae::Dae,
) -> HashMap<String, Vec<RuntimeStateDerivativeSource>> {
    let mut sources: HashMap<String, Vec<RuntimeStateDerivativeSource>> = HashMap::new();
    for eq in &dae.f_x {
        let Some((state_key, alias_name)) = extract_runtime_state_derivative_alias(eq) else {
            continue;
        };
        if !is_state_assignment_name(dae, state_key.as_str())
            || !is_known_assignment_name(dae, alias_name.as_str())
        {
            continue;
        }
        sources
            .entry(state_key)
            .or_default()
            .push(RuntimeStateDerivativeSource {
                state_backed: is_state_assignment_name(dae, alias_name.as_str()),
                alias_name,
            });
    }
    sources
}

fn state_derivative_env_key(state_key: &str) -> String {
    format!("der({state_key})")
}

fn set_state_derivative_env_value(env: &mut VarEnv<f64>, state_key: &str, value: f64) -> usize {
    let key = state_derivative_env_key(state_key);
    if env
        .vars
        .get(key.as_str())
        .is_some_and(|existing| (existing - value).abs() <= 1.0e-12)
    {
        return 0;
    }
    env.set(key.as_str(), value);
    1
}

fn consistent_values(values: &[f64]) -> Option<f64> {
    let anchor = *values.first()?;
    let tol = 1.0e-9 * (1.0 + anchor.abs());
    values
        .iter()
        .all(|value| value.is_finite() && (value - anchor).abs() <= tol)
        .then_some(anchor)
}

fn consistent_derivative_source_value(
    sources: &[RuntimeStateDerivativeSource],
    env: &VarEnv<f64>,
    require_state_backed: bool,
) -> Option<f64> {
    let values: Vec<f64> = sources
        .iter()
        .filter(|source| !require_state_backed || source.state_backed)
        .filter_map(|source| env.vars.get(source.alias_name.as_str()).copied())
        .collect();
    consistent_values(&values)
}

fn preferred_component_derivative_value(
    component_states: &[String],
    env: &VarEnv<f64>,
    preferred_states: &HashSet<String>,
) -> Option<f64> {
    let preferred_values: Vec<f64> = component_states
        .iter()
        .filter(|state| preferred_states.contains(state.as_str()))
        .filter_map(|state| {
            env.vars
                .get(state_derivative_env_key(state.as_str()).as_str())
                .copied()
        })
        .collect();
    consistent_values(&preferred_values).or_else(|| {
        let all_values: Vec<f64> = component_states
            .iter()
            .filter_map(|state| {
                env.vars
                    .get(state_derivative_env_key(state.as_str()).as_str())
                    .copied()
            })
            .collect();
        consistent_values(&all_values)
    })
}

pub(crate) fn propagate_runtime_derivative_aliases_from_env(
    dae: &dae::Dae,
    _n_x: usize,
    env: &mut VarEnv<f64>,
) -> usize {
    let derivative_sources = build_runtime_state_derivative_sources(dae);
    if derivative_sources.is_empty() {
        return 0;
    }

    let mut updates = 0usize;
    let mut preferred_states = HashSet::new();
    for (state_key, sources) in &derivative_sources {
        let preferred = consistent_derivative_source_value(sources, env, true);
        if let Some(value) =
            preferred.or_else(|| consistent_derivative_source_value(sources, env, false))
        {
            updates += set_state_derivative_env_value(env, state_key.as_str(), value);
        }
        if preferred.is_some() {
            preferred_states.insert(state_key.clone());
        }
    }

    let adjacency =
        crate::runtime::alias::build_runtime_alias_adjacency_with_known_assignments(dae, 0);
    if adjacency.is_empty() {
        return updates;
    }

    let state_names: HashSet<String> = dae
        .states
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();
    let mut visited = HashSet::new();
    for state_name in &state_names {
        if !adjacency.contains_key(state_name.as_str()) || visited.contains(state_name.as_str()) {
            continue;
        }
        let component = crate::runtime::alias::collect_alias_component(
            state_name.as_str(),
            &adjacency,
            &mut visited,
        );
        let component_states: Vec<String> = component
            .into_iter()
            .filter(|name| state_names.contains(name.as_str()))
            .collect();
        if component_states.len() < 2 {
            continue;
        }
        let Some(anchor) =
            preferred_component_derivative_value(&component_states, env, &preferred_states)
        else {
            continue;
        };
        for state_key in component_states {
            updates += set_state_derivative_env_value(env, state_key.as_str(), anchor);
        }
    }

    updates
}

#[derive(Clone, Copy, Debug, Default)]
pub struct DirectAssignmentTargetStats {
    pub total: usize,
    pub non_alias: usize,
}

fn collect_assignment_target_dependencies<'a>(
    dae: &dae::Dae,
    eqs: impl Iterator<Item = &'a dae::Equation>,
    include_alias_assignments: bool,
) -> HashMap<String, Vec<String>> {
    let mut deps: HashMap<String, HashSet<String>> = HashMap::new();
    for eq in eqs {
        if eq.origin == "orphaned_variable_pin" {
            continue;
        }
        let Some((target, solution)) = direct_assignment_from_equation(eq) else {
            continue;
        };
        if !include_alias_assignments && assignment_solution_is_alias_varref(dae, solution) {
            continue;
        }

        let mut refs = HashSet::new();
        solution.collect_var_refs(&mut refs);
        let target_deps = deps.entry(target.clone()).or_default();
        for name in refs {
            let source = name.as_str();
            if source == target || !is_known_assignment_name(dae, source) {
                continue;
            }
            target_deps.insert(source.to_string());
        }
    }

    deps.into_iter()
        .map(|(target, deps)| {
            let mut deps = deps.into_iter().collect::<Vec<_>>();
            deps.sort();
            (target, deps)
        })
        .collect()
}

fn visit_ordered_assignment_target(
    target: &str,
    deps: &HashMap<String, Vec<String>>,
    visiting: &mut HashSet<String>,
    visited: &mut HashSet<String>,
    ordered: &mut Vec<String>,
) {
    if visited.contains(target) || !visiting.insert(target.to_string()) {
        return;
    }
    if let Some(target_deps) = deps.get(target) {
        for dep in target_deps {
            if deps.contains_key(dep.as_str()) {
                visit_ordered_assignment_target(dep, deps, visiting, visited, ordered);
            }
        }
    }
    visiting.remove(target);
    if visited.insert(target.to_string()) {
        ordered.push(target.to_string());
    }
}

fn ordered_assignment_targets_from_roots(
    deps: &HashMap<String, Vec<String>>,
    mut roots: Vec<String>,
) -> Vec<String> {
    let mut ordered = Vec::new();
    let mut visited = HashSet::new();
    let mut visiting = HashSet::new();
    roots.sort_unstable();
    roots.dedup();
    for root in roots {
        if deps.contains_key(root.as_str()) {
            visit_ordered_assignment_target(
                root.as_str(),
                deps,
                &mut visiting,
                &mut visited,
                &mut ordered,
            );
        }
    }
    ordered
}

fn collect_assignment_target_stats<'a>(
    dae: &dae::Dae,
    eqs: impl Iterator<Item = &'a dae::Equation>,
    skip_alias_pairs: bool,
) -> HashMap<String, DirectAssignmentTargetStats> {
    let mut stats: HashMap<String, DirectAssignmentTargetStats> = HashMap::new();
    for eq in eqs {
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

pub fn collect_direct_assignment_target_stats(
    dae: &dae::Dae,
    n_x: usize,
    skip_alias_pairs: bool,
) -> HashMap<String, DirectAssignmentTargetStats> {
    collect_assignment_target_stats(dae, dae.f_x.iter().skip(n_x), skip_alias_pairs)
}

pub fn collect_discrete_assignment_target_stats(
    dae: &dae::Dae,
    skip_alias_pairs: bool,
) -> HashMap<String, DirectAssignmentTargetStats> {
    collect_assignment_target_stats(dae, dae.f_z.iter().chain(dae.f_m.iter()), skip_alias_pairs)
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

pub(crate) struct RuntimeDirectAssignmentContext {
    solver_maps: crate::runtime::layout::SolverNameIndexMaps,
    target_assignment_stats: HashMap<String, DirectAssignmentTargetStats>,
    target_dependencies: HashMap<String, Vec<String>>,
    #[cfg(test)]
    ordered_runtime_target_dependencies: HashMap<String, Vec<String>>,
}

pub(crate) fn build_runtime_direct_assignment_context(
    dae: &dae::Dae,
    y_len: usize,
    n_x: usize,
) -> RuntimeDirectAssignmentContext {
    RuntimeDirectAssignmentContext {
        solver_maps: crate::runtime::layout::build_solver_name_index_maps(dae, y_len),
        target_assignment_stats: collect_direct_assignment_target_stats(dae, n_x, false),
        target_dependencies: collect_assignment_target_dependencies(
            dae,
            dae.f_x.iter().skip(n_x),
            false,
        ),
        #[cfg(test)]
        ordered_runtime_target_dependencies: collect_assignment_target_dependencies(
            dae,
            dae.f_x
                .iter()
                .skip(n_x)
                .chain(dae.f_z.iter())
                .chain(dae.f_m.iter()),
            true,
        ),
    }
}

fn push_dependency_and_base(worklist: &mut Vec<String>, dep: &str) {
    worklist.push(dep.to_string());
    if let Some(base) = dae::component_base_name(dep)
        && base != dep
    {
        worklist.push(base);
    }
}

fn runtime_direct_assignment_debug_target(target: &str) -> bool {
    std::env::var_os("RUMOCA_DEBUG_DIGITAL_START").is_some()
        && matches!(
            target,
            "a.y"
                | "b.y"
                | "FF.RS1.q"
                | "FF.RS1.r"
                | "FF.RS1.s"
                | "Enable.y"
                | "FF.j"
                | "FF.k"
                | "MUX.d"
        )
}

fn debug_runtime_assignment_skip_alias(debug_digital: bool, target: &str) {
    if debug_digital {
        eprintln!("DEBUG assign skip alias target={target}");
    }
}

fn debug_runtime_assignment_skip_stats(
    debug_digital: bool,
    target: &str,
    target_stats: DirectAssignmentTargetStats,
) {
    if debug_digital {
        eprintln!(
            "DEBUG assign skip stats target={target} total={} non_alias={}",
            target_stats.total, target_stats.non_alias
        );
    }
}

fn debug_runtime_assignment_vector(
    debug_digital: bool,
    target: &str,
    values: &[f64],
    branch_changed: bool,
    branch_updates: usize,
) {
    if debug_digital {
        eprintln!(
            "DEBUG assign vector target={target} values={values:?} changed={branch_changed} updates={branch_updates}"
        );
    }
}

fn apply_runtime_direct_assignment_vector(
    base_to_indices: &HashMap<String, Vec<usize>>,
    target: &str,
    solution: &dae::Expression,
    y: &mut [f64],
    n_x: usize,
    env: &mut VarEnv<f64>,
    names: &[String],
) -> Option<(bool, usize)> {
    if target.contains('[') {
        return None;
    }
    let indices = base_to_indices.get(target)?;
    if indices.len() <= 1 {
        return None;
    }

    let values = evaluate_direct_assignment_values(solution, env, indices.len());
    let (branch_changed, branch_updates) =
        apply_runtime_values_to_indices(y, env, names, indices, &values, n_x);
    debug_runtime_assignment_vector(
        runtime_direct_assignment_debug_target(target),
        target,
        &values,
        branch_changed,
        branch_updates,
    );
    Some((branch_changed, branch_updates))
}

fn insert_dependency_name_and_base(names: &mut HashSet<String>, name: &str) -> bool {
    let mut changed = names.insert(name.to_string());
    if let Some(base) = dae::component_base_name(name)
        && base != name
    {
        changed |= names.insert(base);
    }
    changed
}

pub(crate) fn extend_runtime_direct_assignment_dependency_closure(
    ctx: &RuntimeDirectAssignmentContext,
    names: &mut HashSet<String>,
) -> bool {
    let mut changed_any = false;
    let mut worklist: Vec<String> = names.iter().cloned().collect();
    let mut visited = HashSet::new();

    while let Some(target) = worklist.pop() {
        if !visited.insert(target.clone()) {
            continue;
        }
        let Some(deps) = ctx.target_dependencies.get(target.as_str()) else {
            continue;
        };
        for dep in deps {
            if insert_dependency_name_and_base(names, dep) {
                changed_any = true;
                push_dependency_and_base(&mut worklist, dep);
            }
        }
    }

    changed_any
}

#[cfg(test)]
pub(crate) fn ordered_runtime_assignment_targets_for_seeds(
    ctx: &RuntimeDirectAssignmentContext,
    seeds: &HashSet<String>,
) -> Vec<String> {
    ordered_assignment_targets_from_roots(
        &ctx.ordered_runtime_target_dependencies,
        seeds.iter().cloned().collect(),
    )
}

pub(crate) fn ordered_discrete_assignment_targets(dae: &dae::Dae) -> Vec<String> {
    let deps =
        collect_assignment_target_dependencies(dae, dae.f_z.iter().chain(dae.f_m.iter()), true);
    let roots = deps.keys().cloned().collect();
    ordered_assignment_targets_from_roots(&deps, roots)
}

pub(crate) fn propagate_runtime_direct_assignments_from_env_with_context(
    ctx: &RuntimeDirectAssignmentContext,
    dae: &dae::Dae,
    y: &mut [f64],
    n_x: usize,
    env: &mut VarEnv<f64>,
) -> usize {
    if dae.f_x.len() <= n_x {
        return 0;
    }

    let names = &ctx.solver_maps.names;
    let name_to_idx = &ctx.solver_maps.name_to_idx;
    let base_to_indices = &ctx.solver_maps.base_to_indices;
    let target_assignment_stats = &ctx.target_assignment_stats;

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
            let debug_digital = runtime_direct_assignment_debug_target(target.as_str());
            // Alias equalities are solved via runtime alias-component propagation.
            if assignment_solution_is_alias_varref(dae, solution) {
                debug_runtime_assignment_skip_alias(debug_digital, target.as_str());
                continue;
            }
            let target_stats = target_assignment_stats
                .get(target.as_str())
                .copied()
                .unwrap_or_default();
            if std::env::var_os("RUMOCA_DEBUG_COUNTER_ENABLE").is_some() && target == "Enable.y" {
                eprintln!(
                    "DEBUG assign target={target} total={} non_alias={} before={} after={} stepTime={} time={}",
                    target_stats.total,
                    target_stats.non_alias,
                    env.get("Enable.before"),
                    env.get("Enable.after"),
                    env.get("Enable.stepTime"),
                    env.get("time"),
                );
            }
            if target_stats.total > 1 && target_stats.non_alias != 1 {
                debug_runtime_assignment_skip_stats(debug_digital, target.as_str(), target_stats);
                continue;
            }

            if let Some((branch_changed, branch_updates)) = apply_runtime_direct_assignment_vector(
                base_to_indices,
                target.as_str(),
                solution,
                y,
                n_x,
                env,
                names,
            ) {
                changed |= branch_changed;
                updates += branch_updates;
                continue;
            }

            let value = clamp_finite(
                evaluate_direct_assignment_values(solution, env, 1)
                    .into_iter()
                    .next()
                    .unwrap_or(0.0),
            );
            if debug_digital {
                eprintln!(
                    "DEBUG assign scalar target={target} value={value} before={} time={}",
                    env.get(target.as_str()),
                    env.get("time"),
                );
            }
            if std::env::var_os("RUMOCA_DEBUG_COUNTER_ENABLE").is_some() && target == "Enable.y" {
                eprintln!("DEBUG assign target={target} value={value}");
            }
            if let Some(var_idx) =
                crate::runtime::layout::solver_idx_for_target(target.as_str(), name_to_idx)
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

pub fn propagate_runtime_direct_assignments_from_env(
    dae: &dae::Dae,
    y: &mut [f64],
    n_x: usize,
    env: &mut VarEnv<f64>,
) -> usize {
    let ctx = build_runtime_direct_assignment_context(dae, y.len(), n_x);
    propagate_runtime_direct_assignments_from_env_with_context(&ctx, dae, y, n_x, env)
}

#[cfg(test)]
mod tests;
