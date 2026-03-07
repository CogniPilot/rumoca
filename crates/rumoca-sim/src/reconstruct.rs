use std::sync::Arc;

use rumoca_eval_dae::runtime::{self as eval, eval_expr};
use rumoca_ir_dae as dae;

use rumoca_phase_solve::eliminate;

/// Build the static part of the reconstruction environment.
fn build_reconstruction_env_template(
    existing_names: &[String],
    dae: &dae::Dae,
    param_values: &[f64],
) -> eval::VarEnv<f64> {
    let mut env = eval::VarEnv::new();
    env.set("time", 0.0);

    if !dae.functions.is_empty() {
        env.functions = Arc::new(eval::collect_user_functions(dae));
    }
    env.dims = Arc::new(eval::collect_var_dims(dae));
    env.start_exprs = Arc::new(eval::collect_var_starts(dae));
    env.enum_literal_ordinals = Arc::new(dae.enum_literal_ordinals.clone());

    for &(fqn, value) in eval::MODELICA_CONSTANTS {
        if !env.vars.contains_key(fqn) {
            env.set(fqn, value);
        }
    }

    // Seed reconstructed/simulated names with zero so nested expressions can
    // always resolve a value even before the first time-point update.
    for name in existing_names {
        env.set(name, 0.0);
    }

    let mut pidx = 0usize;
    for (pname, pvar) in &dae.parameters {
        eval::map_var_to_env(&mut env, pname.as_str(), pvar, param_values, &mut pidx);
    }

    for (cname, cvar) in &dae.constants {
        let Some(ref start) = cvar.start else {
            continue;
        };
        env.set(cname.as_str(), eval_expr(start, &env));
    }

    env
}

/// Update time-varying values in-place for one output sample.
fn update_reconstruction_env_for_time(
    env: &mut eval::VarEnv<f64>,
    t: f64,
    t_idx: usize,
    existing_names: &[String],
    existing_data: &[Vec<f64>],
) {
    env.set("time", t);
    for (name_idx, name) in existing_names.iter().enumerate() {
        let value = existing_data
            .get(name_idx)
            .and_then(|series| series.get(t_idx))
            .copied()
            .unwrap_or(0.0);
        env.set(name, value);
    }
}

/// Reconstruct eliminated variables by evaluating substitution expressions
/// at each time point. Returns (names, data) for the eliminated variables.
pub fn reconstruct_eliminated(
    elim: &eliminate::EliminationResult,
    dae: &dae::Dae,
    param_values: &[f64],
    times: &[f64],
    existing_names: &[String],
    existing_data: &[Vec<f64>],
) -> (Vec<String>, Vec<Vec<f64>>) {
    let n_subs = elim.substitutions.len();
    let mut by_sub = vec![vec![0.0; times.len()]; n_subs];
    let (ordered_subs, cyclic_subs) = substitution_eval_order(elim);
    if reconstruct_introspect_enabled() {
        eprintln!(
            "[sim-introspect] reconstruct substitutions={} ordered={} cyclic={}",
            n_subs,
            ordered_subs.len(),
            cyclic_subs.len()
        );
        if reconstruct_introspect_subs_enabled() {
            for (idx, sub) in elim.substitutions.iter().enumerate() {
                let expr_dbg = truncate_debug(
                    format!("{:?}", sub.expr),
                    reconstruct_introspect_expr_chars(),
                );
                eprintln!(
                    "[sim-introspect] reconstruct sub[{idx}] var={} env_keys={:?} expr={}",
                    sub.var_name.as_str(),
                    sub.env_keys,
                    expr_dbg
                );
            }
        }
    }
    let mut env = build_reconstruction_env_template(existing_names, dae, param_values);
    eval::clear_pre_values();

    for (t_idx, &t) in times.iter().enumerate() {
        update_reconstruction_env_for_time(&mut env, t, t_idx, existing_names, existing_data);
        let mut vals = vec![0.0_f64; n_subs];

        // Evaluate acyclic substitutions exactly once in dependency order.
        for &sub_idx in &ordered_subs {
            let sub = &elim.substitutions[sub_idx];
            let v = eval_expr(&sub.expr, &env);
            maybe_log_non_finite_reconstruction(sub_idx, sub, v, t_idx, &env);
            apply_substitution_value(&mut env, &mut vals[sub_idx], &sub.env_keys, v);
        }

        // Iterate cycles to a local fixed point.
        if !cyclic_subs.is_empty() {
            evaluate_cyclic_substitutions_to_fixpoint(
                &cyclic_subs,
                elim,
                t_idx,
                &mut env,
                &mut vals,
            );
        }

        for (sub_idx, v) in vals.into_iter().enumerate() {
            by_sub[sub_idx][t_idx] = v;
        }
        eval::seed_pre_values_from_env(&env);
    }

    let mut extra_names: Vec<String> = Vec::new();
    let mut extra_data: Vec<Vec<f64>> = Vec::new();
    for (sub_idx, sub) in elim.substitutions.iter().enumerate() {
        for key in &sub.env_keys {
            extra_names.push(key.clone());
            extra_data.push(by_sub[sub_idx].clone());
        }
    }

    (extra_names, extra_data)
}

/// Apply eliminated substitutions into a runtime environment.
///
/// This keeps substitution aliases (e.g. helper variables like time-scaled
/// signals) available during runtime algorithm and equation evaluation.
pub fn apply_eliminated_substitutions_to_env(
    elim: &eliminate::EliminationResult,
    env: &mut eval::VarEnv<f64>,
) {
    if elim.substitutions.is_empty() {
        return;
    }

    let n_subs = elim.substitutions.len();
    let (ordered_subs, cyclic_subs) = substitution_eval_order(elim);
    let mut vals = vec![0.0_f64; n_subs];

    for &sub_idx in &ordered_subs {
        let sub = &elim.substitutions[sub_idx];
        let value = eval_expr(&sub.expr, env);
        apply_substitution_value(env, &mut vals[sub_idx], &sub.env_keys, value);
    }

    if !cyclic_subs.is_empty() {
        evaluate_cyclic_substitutions_to_fixpoint(&cyclic_subs, elim, 0, env, &mut vals);
    }
}

#[cfg(test)]
fn expr_has_intrinsic_time_dependency(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::VarRef { name, .. } => name.as_str() == "time",
        dae::Expression::BuiltinCall { .. } | dae::Expression::FunctionCall { .. } => true,
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_has_intrinsic_time_dependency(lhs) || expr_has_intrinsic_time_dependency(rhs)
        }
        dae::Expression::Unary { rhs, .. } => expr_has_intrinsic_time_dependency(rhs),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(condition, value)| {
                expr_has_intrinsic_time_dependency(condition)
                    || expr_has_intrinsic_time_dependency(value)
            }) || expr_has_intrinsic_time_dependency(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(expr_has_intrinsic_time_dependency)
        }
        dae::Expression::Range { start, step, end } => {
            expr_has_intrinsic_time_dependency(start)
                || step
                    .as_deref()
                    .is_some_and(expr_has_intrinsic_time_dependency)
                || expr_has_intrinsic_time_dependency(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_has_intrinsic_time_dependency(expr)
                || indices
                    .iter()
                    .any(|idx| expr_has_intrinsic_time_dependency(&idx.range))
                || filter
                    .as_deref()
                    .is_some_and(expr_has_intrinsic_time_dependency)
        }
        dae::Expression::Index { base, subscripts } => {
            expr_has_intrinsic_time_dependency(base)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(expr) => expr_has_intrinsic_time_dependency(expr),
                    _ => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => expr_has_intrinsic_time_dependency(base),
        dae::Expression::Literal(_) | dae::Expression::Empty => false,
    }
}

#[cfg(test)]
fn can_reconstruct_eliminated_as_time_invariant(elim: &eliminate::EliminationResult) -> bool {
    use std::collections::{HashMap, HashSet};

    let n_subs = elim.substitutions.len();
    if n_subs == 0 {
        return true;
    }

    let mut name_to_sub_idx: HashMap<String, usize> = HashMap::with_capacity(n_subs);
    for (sub_idx, sub) in elim.substitutions.iter().enumerate() {
        name_to_sub_idx.insert(sub.var_name.as_str().to_string(), sub_idx);
    }

    let mut dynamic = vec![false; n_subs];
    for (sub_idx, sub) in elim.substitutions.iter().enumerate() {
        dynamic[sub_idx] = expr_has_intrinsic_time_dependency(&sub.expr);
    }

    let mut changed = true;
    while changed {
        changed = false;
        for (sub_idx, sub) in elim.substitutions.iter().enumerate() {
            if dynamic[sub_idx] {
                continue;
            }
            let mut refs = HashSet::new();
            sub.expr.collect_var_refs(&mut refs);
            let depends_on_dynamic_sub = refs.into_iter().any(|name| {
                name_to_sub_idx
                    .get(name.as_str())
                    .copied()
                    .is_some_and(|dep_idx| dynamic[dep_idx])
            });
            if depends_on_dynamic_sub {
                dynamic[sub_idx] = true;
                changed = true;
            }
        }
    }

    !dynamic.into_iter().any(|is_dynamic| is_dynamic)
}

#[cfg(test)]
fn reconstruct_eliminated_constant(
    elim: &eliminate::EliminationResult,
    dae: &dae::Dae,
    param_values: &[f64],
    n_times: usize,
    t_start: f64,
    existing_names: &[String],
    existing_values: &[f64],
) -> (Vec<String>, Vec<Vec<f64>>) {
    let n_subs = elim.substitutions.len();
    if n_subs == 0 {
        return (Vec::new(), Vec::new());
    }

    let (ordered_subs, cyclic_subs) = substitution_eval_order(elim);
    if reconstruct_introspect_enabled() {
        eprintln!(
            "[sim-introspect] reconstruct constant substitutions={} ordered={} cyclic={}",
            n_subs,
            ordered_subs.len(),
            cyclic_subs.len()
        );
    }

    let mut env = build_reconstruction_env_template(existing_names, dae, param_values);
    env.set("time", t_start);
    for (name_idx, name) in existing_names.iter().enumerate() {
        let value = existing_values.get(name_idx).copied().unwrap_or(0.0);
        env.set(name, value);
    }

    let mut vals = vec![0.0_f64; n_subs];
    for &sub_idx in &ordered_subs {
        let sub = &elim.substitutions[sub_idx];
        let v = eval_expr(&sub.expr, &env);
        maybe_log_non_finite_reconstruction(sub_idx, sub, v, 0, &env);
        apply_substitution_value(&mut env, &mut vals[sub_idx], &sub.env_keys, v);
    }

    if !cyclic_subs.is_empty() {
        evaluate_cyclic_substitutions_to_fixpoint(&cyclic_subs, elim, 0, &mut env, &mut vals);
    }

    let mut extra_names: Vec<String> = Vec::new();
    let mut extra_data: Vec<Vec<f64>> = Vec::new();
    for (sub_idx, sub) in elim.substitutions.iter().enumerate() {
        for key in &sub.env_keys {
            extra_names.push(key.clone());
            extra_data.push(vec![vals[sub_idx]; n_times]);
        }
    }

    (extra_names, extra_data)
}

fn evaluate_cyclic_substitutions_once(
    cyclic_subs: &[usize],
    elim: &eliminate::EliminationResult,
    t_idx: usize,
    env: &mut eval::VarEnv<f64>,
    vals: &mut [f64],
) -> bool {
    let mut changed = false;
    for &sub_idx in cyclic_subs {
        let sub = &elim.substitutions[sub_idx];
        let v = eval_expr(&sub.expr, env);
        maybe_log_non_finite_reconstruction(sub_idx, sub, v, t_idx, env);
        changed |= apply_substitution_value(env, &mut vals[sub_idx], &sub.env_keys, v);
    }
    changed
}

fn evaluate_cyclic_substitutions_to_fixpoint(
    cyclic_subs: &[usize],
    elim: &eliminate::EliminationResult,
    t_idx: usize,
    env: &mut eval::VarEnv<f64>,
    vals: &mut [f64],
) {
    let max_cycle_passes = (cyclic_subs.len() * 4).clamp(1, 128);
    for _ in 0..max_cycle_passes {
        if !evaluate_cyclic_substitutions_once(cyclic_subs, elim, t_idx, env, vals) {
            break;
        }
    }
}

fn substitution_eval_order(elim: &eliminate::EliminationResult) -> (Vec<usize>, Vec<usize>) {
    use std::cmp::Reverse;
    use std::collections::{BinaryHeap, HashMap, HashSet};

    let n_subs = elim.substitutions.len();
    if n_subs == 0 {
        return (Vec::new(), Vec::new());
    }

    let mut name_to_sub_idx: HashMap<String, usize> = HashMap::with_capacity(n_subs);
    let mut record_base_to_sub_idxs: HashMap<String, Vec<usize>> = HashMap::new();
    for (sub_idx, sub) in elim.substitutions.iter().enumerate() {
        let name = sub.var_name.as_str().to_string();
        name_to_sub_idx.insert(name.clone(), sub_idx);
        if let Some((base, _field)) = name.split_once('.') {
            record_base_to_sub_idxs
                .entry(base.to_string())
                .or_default()
                .push(sub_idx);
        }
    }

    // deps[i] = substitutions that substitution i depends on.
    let mut deps: Vec<Vec<usize>> = vec![Vec::new(); n_subs];
    // dependents[i] = substitutions that depend on substitution i.
    let mut dependents: Vec<Vec<usize>> = vec![Vec::new(); n_subs];
    let mut in_degree = vec![0usize; n_subs];

    for (sub_idx, sub) in elim.substitutions.iter().enumerate() {
        let mut refs = HashSet::new();
        sub.expr.collect_var_refs(&mut refs);

        let mut unique_deps: Vec<usize> = Vec::new();
        for name in refs {
            if let Some(dep_idx) = name_to_sub_idx.get(name.as_str()).copied()
                && dep_idx != sub_idx
            {
                unique_deps.push(dep_idx);
            }
            if let Some(children) = record_base_to_sub_idxs.get(name.as_str()) {
                unique_deps.extend(
                    children
                        .iter()
                        .copied()
                        .filter(|&dep_idx| dep_idx != sub_idx),
                );
            }
        }
        unique_deps.sort_unstable();
        unique_deps.dedup();

        in_degree[sub_idx] = unique_deps.len();
        deps[sub_idx] = unique_deps;
        for &dep_idx in &deps[sub_idx] {
            dependents[dep_idx].push(sub_idx);
        }
    }

    for dependent_idxs in &mut dependents {
        dependent_idxs.sort_unstable();
    }

    // Stable Kahn topological sort: smaller original index first.
    let mut queue: BinaryHeap<Reverse<usize>> = BinaryHeap::new();
    for (sub_idx, &deg) in in_degree.iter().enumerate() {
        if deg == 0 {
            queue.push(Reverse(sub_idx));
        }
    }

    let mut ordered = Vec::with_capacity(n_subs);
    while let Some(Reverse(sub_idx)) = queue.pop() {
        ordered.push(sub_idx);
        for &next_idx in &dependents[sub_idx] {
            in_degree[next_idx] = in_degree[next_idx].saturating_sub(1);
            if in_degree[next_idx] == 0 {
                queue.push(Reverse(next_idx));
            }
        }
    }

    let mut is_ordered = vec![false; n_subs];
    for &sub_idx in &ordered {
        is_ordered[sub_idx] = true;
    }
    let cyclic: Vec<usize> = (0..n_subs)
        .filter(|&sub_idx| !is_ordered[sub_idx])
        .collect();
    (ordered, cyclic)
}

fn reconstruct_introspect_enabled() -> bool {
    std::env::var("RUMOCA_SIM_INTROSPECT").is_ok()
}

fn reconstruct_introspect_subs_enabled() -> bool {
    std::env::var("RUMOCA_SIM_INTROSPECT_SUBSTITUTIONS").is_ok()
}

fn reconstruct_introspect_expr_chars() -> usize {
    std::env::var("RUMOCA_SIM_INTROSPECT_EXPR_CHARS")
        .ok()
        .and_then(|raw| raw.parse::<usize>().ok())
        .filter(|&n| n > 0)
        .unwrap_or(220)
}

fn truncate_debug(mut text: String, max_chars: usize) -> String {
    if text.chars().count() <= max_chars {
        return text;
    }
    text.truncate(max_chars);
    text.push('…');
    text
}

fn format_expr_ref_values(expr: &dae::Expression, env: &eval::VarEnv<f64>) -> String {
    use std::collections::HashSet;

    let mut refs = HashSet::new();
    expr.collect_var_refs(&mut refs);
    let mut names: Vec<String> = refs
        .into_iter()
        .map(|name| name.as_str().to_string())
        .collect();
    names.sort();

    const MAX_REFS: usize = 12;
    let omitted = names.len().saturating_sub(MAX_REFS);
    let shown = names.into_iter().take(MAX_REFS).map(|name| {
        let value = env.get(name.as_str());
        format!("{name}={value}")
    });
    let mut joined = shown.collect::<Vec<_>>().join(", ");
    if omitted > 0 {
        joined.push_str(&format!(", ... (+{omitted} refs)"));
    }
    joined
}

fn maybe_log_non_finite_reconstruction(
    sub_idx: usize,
    sub: &eliminate::Substitution,
    value: f64,
    t_idx: usize,
    env: &eval::VarEnv<f64>,
) {
    if t_idx != 0 || value.is_finite() || !reconstruct_introspect_enabled() {
        return;
    }
    let expr = truncate_debug(
        format!("{:?}", sub.expr),
        reconstruct_introspect_expr_chars(),
    );
    eprintln!(
        "[sim-introspect] reconstruct non-finite sub[{sub_idx}] var={} keys={:?} value={} expr={} refs=[{}]",
        sub.var_name,
        sub.env_keys,
        value,
        expr,
        format_expr_ref_values(&sub.expr, env)
    );
}

fn apply_substitution_value(
    env: &mut eval::VarEnv<f64>,
    slot: &mut f64,
    env_keys: &[String],
    value: f64,
) -> bool {
    let changed = slot.to_bits() != value.to_bits();
    if changed {
        *slot = value;
    }
    for key in env_keys {
        env.set(key.as_str(), value);
    }
    changed
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_dae as dae;
    type Function = dae::Function;
    type FunctionParam = dae::FunctionParam;
    type Literal = dae::Literal;
    type OpBinary = rumoca_ir_core::OpBinary;
    type Statement = dae::Statement;
    type VarName = dae::VarName;
    type Variable = dae::Variable;

    fn lit(v: f64) -> dae::Expression {
        dae::Expression::Literal(Literal::Real(v))
    }

    fn var(name: &str) -> dae::Expression {
        dae::Expression::VarRef {
            name: VarName::new(name),
            subscripts: vec![],
        }
    }

    #[test]
    fn test_reconstruct_param_mapping_handles_zero_sized_array_param_slots() {
        let mut dae = dae::Dae::new();

        let mut p0 = Variable::new(VarName::new("p0"));
        p0.dims = vec![0, 2];
        p0.start = Some(dae::Expression::Array {
            elements: vec![
                dae::Expression::Array {
                    elements: vec![lit(1.0), lit(2.0)],
                    is_matrix: false,
                },
                dae::Expression::Array {
                    elements: vec![lit(3.0), lit(4.0)],
                    is_matrix: false,
                },
            ],
            is_matrix: true,
        });
        dae.parameters.insert(VarName::new("p0"), p0);

        let mut p1 = Variable::new(VarName::new("p1"));
        p1.start = Some(lit(2.0));
        dae.parameters.insert(VarName::new("p1"), p1);

        let sub = eliminate::Substitution {
            var_name: VarName::new("x"),
            expr: dae::Expression::Binary {
                op: OpBinary::Div(Default::default()),
                lhs: Box::new(dae::Expression::VarRef {
                    name: VarName::new("time"),
                    subscripts: vec![],
                }),
                rhs: Box::new(dae::Expression::VarRef {
                    name: VarName::new("p1"),
                    subscripts: vec![],
                }),
            },
            env_keys: vec!["x".to_string()],
        };
        let elim = eliminate::EliminationResult {
            substitutions: vec![sub],
            n_eliminated: 1,
        };

        // p0 occupies one parameter slot in runtime mapping despite size()==0.
        let params = vec![99.0, 2.0];
        let times = vec![0.0, 1.0];
        let (names, data) = reconstruct_eliminated(&elim, &dae, &params, &times, &[], &[]);

        assert_eq!(names, vec!["x".to_string()]);
        assert_eq!(data.len(), 1);
        assert_eq!(data[0].len(), 2);
        assert!((data[0][0] - 0.0).abs() < 1e-12);
        assert!((data[0][1] - 0.5).abs() < 1e-12);
    }

    #[test]
    fn test_reconstruct_resolves_substitution_dependencies_out_of_order() {
        let dae = dae::Dae::new();
        let elim = eliminate::EliminationResult {
            substitutions: vec![
                eliminate::Substitution {
                    var_name: VarName::new("y"),
                    expr: dae::Expression::Binary {
                        op: OpBinary::Div(Default::default()),
                        lhs: Box::new(lit(1.0)),
                        rhs: Box::new(dae::Expression::VarRef {
                            name: VarName::new("x"),
                            subscripts: vec![],
                        }),
                    },
                    env_keys: vec!["y".to_string()],
                },
                eliminate::Substitution {
                    var_name: VarName::new("x"),
                    expr: lit(2.0),
                    env_keys: vec!["x".to_string()],
                },
            ],
            n_eliminated: 2,
        };

        let (names, data) = reconstruct_eliminated(&elim, &dae, &[], &[0.0], &[], &[]);
        assert_eq!(names, vec!["y".to_string(), "x".to_string()]);
        assert_eq!(data.len(), 2);
        assert!((data[0][0] - 0.5).abs() < 1e-12);
        assert!((data[1][0] - 2.0).abs() < 1e-12);
    }

    #[test]
    fn test_reconstruct_handles_long_dependency_chains_without_fixed_pass_limit() {
        let dae = dae::Dae::new();
        let n = 40usize;
        let mut substitutions = Vec::with_capacity(n);

        // Intentionally out-of-order:
        // x1 = x2 + 1, x2 = x3 + 1, ..., x39 = x40 + 1, x40 = 1.
        for i in 1..n {
            let lhs = format!("x{i}");
            let rhs = format!("x{}", i + 1);
            substitutions.push(eliminate::Substitution {
                var_name: VarName::new(&lhs),
                expr: dae::Expression::Binary {
                    op: OpBinary::Add(Default::default()),
                    lhs: Box::new(var(&rhs)),
                    rhs: Box::new(lit(1.0)),
                },
                env_keys: vec![lhs],
            });
        }
        substitutions.push(eliminate::Substitution {
            var_name: VarName::new("x40"),
            expr: lit(1.0),
            env_keys: vec!["x40".to_string()],
        });

        let elim = eliminate::EliminationResult {
            substitutions,
            n_eliminated: n,
        };
        let (names, data) = reconstruct_eliminated(&elim, &dae, &[], &[0.0], &[], &[]);
        let x1_idx = names
            .iter()
            .position(|name| name == "x1")
            .expect("x1 should be reconstructed");
        assert!((data[x1_idx][0] - 40.0).abs() < 1e-12);
    }

    #[test]
    fn test_reconstruct_orders_record_field_substitutions_before_base_ref_consumers() {
        let mut dae = dae::Dae::new();
        let mut state_metric = Function::new("Pkg.stateMetric", Default::default());
        state_metric.add_input(FunctionParam::new("st", "State"));
        state_metric.add_output(FunctionParam::new("y", "Real").with_default(
            dae::Expression::Binary {
                op: OpBinary::Add(Default::default()),
                lhs: Box::new(var("st.p")),
                rhs: Box::new(var("st.T")),
            },
        ));
        state_metric.body = vec![Statement::Empty];
        dae.functions
            .insert(VarName::new("Pkg.stateMetric"), state_metric);

        let elim = eliminate::EliminationResult {
            substitutions: vec![
                eliminate::Substitution {
                    var_name: VarName::new("h"),
                    expr: dae::Expression::FunctionCall {
                        name: VarName::new("Pkg.stateMetric"),
                        args: vec![var("state")],
                        is_constructor: false,
                    },
                    env_keys: vec!["h".to_string()],
                },
                eliminate::Substitution {
                    var_name: VarName::new("state.p"),
                    expr: lit(101325.0),
                    env_keys: vec!["state.p".to_string()],
                },
                eliminate::Substitution {
                    var_name: VarName::new("state.T"),
                    expr: lit(350.0),
                    env_keys: vec!["state.T".to_string()],
                },
            ],
            n_eliminated: 3,
        };

        let (names, data) = reconstruct_eliminated(&elim, &dae, &[], &[0.0], &[], &[]);
        let h_idx = names
            .iter()
            .position(|name| name == "h")
            .expect("h should be reconstructed");
        assert!((data[h_idx][0] - 101675.0).abs() < 1e-9);
    }

    #[test]
    fn test_can_reconstruct_eliminated_as_time_invariant_tracks_dependency_chains() {
        let elim_static = eliminate::EliminationResult {
            substitutions: vec![
                eliminate::Substitution {
                    var_name: VarName::new("y"),
                    expr: dae::Expression::Binary {
                        op: OpBinary::Add(Default::default()),
                        lhs: Box::new(var("x")),
                        rhs: Box::new(lit(1.0)),
                    },
                    env_keys: vec!["y".to_string()],
                },
                eliminate::Substitution {
                    var_name: VarName::new("x"),
                    expr: lit(2.0),
                    env_keys: vec!["x".to_string()],
                },
            ],
            n_eliminated: 2,
        };
        assert!(can_reconstruct_eliminated_as_time_invariant(&elim_static));

        let elim_dynamic = eliminate::EliminationResult {
            substitutions: vec![
                eliminate::Substitution {
                    var_name: VarName::new("y"),
                    expr: var("x"),
                    env_keys: vec!["y".to_string()],
                },
                eliminate::Substitution {
                    var_name: VarName::new("x"),
                    expr: var("time"),
                    env_keys: vec!["x".to_string()],
                },
            ],
            n_eliminated: 2,
        };
        assert!(!can_reconstruct_eliminated_as_time_invariant(&elim_dynamic));
    }

    #[test]
    fn test_reconstruct_constant_matches_full_for_time_invariant_substitutions() {
        let dae = dae::Dae::new();
        let elim = eliminate::EliminationResult {
            substitutions: vec![
                eliminate::Substitution {
                    var_name: VarName::new("y"),
                    expr: dae::Expression::Binary {
                        op: OpBinary::Add(Default::default()),
                        lhs: Box::new(var("x")),
                        rhs: Box::new(var("base")),
                    },
                    env_keys: vec!["y".to_string()],
                },
                eliminate::Substitution {
                    var_name: VarName::new("x"),
                    expr: dae::Expression::Binary {
                        op: OpBinary::Mul(Default::default()),
                        lhs: Box::new(var("base")),
                        rhs: Box::new(lit(2.0)),
                    },
                    env_keys: vec!["x".to_string()],
                },
            ],
            n_eliminated: 2,
        };
        assert!(can_reconstruct_eliminated_as_time_invariant(&elim));

        let times = vec![0.0, 0.5, 1.0];
        let existing_names = vec!["base".to_string()];
        let existing_data = vec![vec![3.0; times.len()]];
        let existing_values = vec![3.0];

        let (names_full, data_full) =
            reconstruct_eliminated(&elim, &dae, &[], &times, &existing_names, &existing_data);
        let (names_const, data_const) = reconstruct_eliminated_constant(
            &elim,
            &dae,
            &[],
            times.len(),
            0.0,
            &existing_names,
            &existing_values,
        );

        assert_eq!(names_full, names_const);
        assert_eq!(data_full, data_const);
    }

    #[test]
    fn test_reconstruct_eliminated_pre_tracks_previous_sample_value() {
        let dae = dae::Dae::new();
        let elim = eliminate::EliminationResult {
            substitutions: vec![eliminate::Substitution {
                var_name: VarName::new("y_prev"),
                expr: dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Pre,
                    args: vec![var("sig")],
                },
                env_keys: vec!["y_prev".to_string()],
            }],
            n_eliminated: 1,
        };

        let times = vec![0.0, 1.0, 2.0];
        let existing_names = vec!["sig".to_string()];
        let existing_data = vec![vec![1.0, 2.0, 3.0]];
        let (names, data) =
            reconstruct_eliminated(&elim, &dae, &[], &times, &existing_names, &existing_data);

        assert_eq!(names, vec!["y_prev".to_string()]);
        assert_eq!(data.len(), 1);
        assert_eq!(data[0].len(), 3);
        // `pre(sig)` falls back to current sample when no prior cache exists.
        assert!((data[0][0] - 1.0).abs() < 1e-12);
        assert!((data[0][1] - 1.0).abs() < 1e-12);
        assert!((data[0][2] - 2.0).abs() < 1e-12);
    }
}
