use super::*;

pub(super) fn sampled_target_held_value(
    target: &str,
    fallback_expr: Option<&dae::Expression>,
    env: &VarEnv<f64>,
) -> f64 {
    hotpath_stats::inc_held_value_read();
    let held = rumoca_eval_dae::runtime::get_pre_value(target)
        .or_else(|| env.vars.get(target).copied())
        .unwrap_or_else(|| {
            fallback_expr
                .map(|expr| eval_discrete_scalar_value(expr, env))
                .unwrap_or(0.0)
        });
    clamp_finite(held)
}

pub(super) fn array_target_held_values(
    target: &str,
    fallback_expr: Option<&dae::Expression>,
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Vec<f64> {
    let fallback_values = fallback_expr
        .map(|expr| evaluate_direct_assignment_values(expr, env, expected_len))
        .unwrap_or_else(|| vec![0.0; expected_len]);
    let mut values = Vec::with_capacity(expected_len);
    for (index, fallback_value) in fallback_values.iter().copied().enumerate() {
        let key = format!("{target}[{}]", index + 1);
        let base_fallback = base_target_hold_fallback(env, target, index);
        let held = rumoca_eval_dae::runtime::get_pre_value(&key)
            .or_else(|| env.vars.get(&key).copied())
            .or(base_fallback)
            .unwrap_or(fallback_value);
        values.push(clamp_finite(held));
    }
    values
}

pub(super) fn sample_source_prefers_pre_value(dae: &dae::Dae, key: &str) -> bool {
    key.starts_with("__pre__.")
        || dae.parameters.contains_key(&dae::VarName::new(key))
        || dae.constants.contains_key(&dae::VarName::new(key))
        || crate::runtime::assignment::is_discrete_name(dae, key)
}

pub(super) fn extract_sample_source_alias_pair(eq: &dae::Equation) -> Option<(String, String)> {
    if let Some(lhs) = eq.lhs.as_ref()
        && let dae::Expression::VarRef {
            name: rhs_name,
            subscripts: rhs_subscripts,
        } = &eq.rhs
    {
        let rhs_key = canonical_var_ref_key(rhs_name, rhs_subscripts)?;
        return Some((lhs.as_str().to_string(), rhs_key));
    }

    let dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(_),
        lhs,
        rhs,
    } = &eq.rhs
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
    Some((
        canonical_var_ref_key(lhs_name, lhs_subscripts)?,
        canonical_var_ref_key(rhs_name, rhs_subscripts)?,
    ))
}

pub(super) fn insert_sample_source_alias_edge(
    adjacency: &mut HashMap<String, Vec<String>>,
    lhs: &str,
    rhs: &str,
) {
    if lhs == rhs {
        return;
    }
    adjacency
        .entry(lhs.to_string())
        .or_default()
        .push(rhs.to_string());
    adjacency
        .entry(rhs.to_string())
        .or_default()
        .push(lhs.to_string());
}

pub(super) fn build_sample_source_alias_adjacency(
    dae: &dae::Dae,
    n_x: usize,
) -> HashMap<String, Vec<String>> {
    let mut adjacency = HashMap::new();
    for eq in dae
        .f_x
        .iter()
        .skip(n_x)
        .chain(dae.f_z.iter())
        .chain(dae.f_m.iter())
    {
        if eq.origin == "orphaned_variable_pin" {
            continue;
        }
        let Some((lhs, rhs)) = extract_sample_source_alias_pair(eq) else {
            continue;
        };
        if sample_source_prefers_pre_value(dae, lhs.as_str())
            || sample_source_prefers_pre_value(dae, rhs.as_str())
        {
            continue;
        }
        insert_sample_source_alias_edge(&mut adjacency, lhs.as_str(), rhs.as_str());
    }
    adjacency
}

pub(super) fn collect_sample_source_alias_component(
    seed: &str,
    adjacency: &HashMap<String, Vec<String>>,
    visited: &mut HashSet<String>,
) -> Vec<String> {
    let mut component = Vec::new();
    let mut stack = vec![seed.to_string()];
    while let Some(name) = stack.pop() {
        if !visited.insert(name.clone()) {
            continue;
        }
        component.push(name.clone());
        if let Some(neighbors) = adjacency.get(name.as_str()) {
            stack.extend(neighbors.iter().cloned());
        }
    }
    component
}

pub(super) fn propagate_sample_source_env_only_aliases(
    dae: &dae::Dae,
    expr: &dae::Expression,
    n_x: usize,
    env: &mut VarEnv<f64>,
) -> usize {
    let adjacency = build_sample_source_alias_adjacency(dae, n_x);
    if adjacency.is_empty() {
        return 0;
    }

    let mut refs = HashSet::new();
    expr.collect_var_refs(&mut refs);
    let mut seeds = Vec::new();
    for name in refs {
        let key = name.as_str();
        if key == "time" || sample_source_prefers_pre_value(dae, key) {
            continue;
        }
        if adjacency.contains_key(key) {
            seeds.push(key.to_string());
        }
    }
    if seeds.is_empty() {
        return 0;
    }

    let mut visited = HashSet::new();
    let mut updates = 0usize;
    for seed in seeds {
        if visited.contains(seed.as_str()) {
            continue;
        }
        let component =
            collect_sample_source_alias_component(seed.as_str(), &adjacency, &mut visited);
        let mut anchor_value: Option<f64> = None;
        let mut inconsistent = false;
        for name in &component {
            let Some(value) = env.vars.get(name.as_str()).copied() else {
                continue;
            };
            if let Some(anchor) = anchor_value {
                inconsistent = (anchor - value).abs() > 1.0e-12;
            } else {
                anchor_value = Some(value);
            }
            if inconsistent {
                break;
            }
        }
        if inconsistent {
            continue;
        }
        let Some(anchor) = anchor_value else {
            continue;
        };
        for name in component {
            if env
                .vars
                .get(name.as_str())
                .is_none_or(|existing| (existing - anchor).abs() > 1.0e-12)
            {
                env.set(name.as_str(), anchor);
                updates += 1;
            }
        }
    }

    updates
}

pub(super) fn expr_needs_sample_source_runtime_closure(
    dae: &dae::Dae,
    expr: &dae::Expression,
) -> bool {
    let mut refs = HashSet::new();
    expr.collect_var_refs(&mut refs);
    refs.into_iter().any(|name| {
        let key = name.as_str();
        key != "time"
            && crate::runtime::assignment::is_known_assignment_name(dae, key)
            && !sample_source_prefers_pre_value(dae, key)
    })
}

pub(super) fn expr_needs_sample_source_env_only_alias_closure(
    dae: &dae::Dae,
    expr: &dae::Expression,
    n_x: usize,
) -> bool {
    let adjacency = build_sample_source_alias_adjacency(dae, n_x);
    if adjacency.is_empty() {
        return false;
    }

    let mut refs = HashSet::new();
    expr.collect_var_refs(&mut refs);
    refs.into_iter().any(|name| {
        let key = name.as_str();
        key != "time" && !sample_source_prefers_pre_value(dae, key) && adjacency.contains_key(key)
    })
}

pub(super) fn build_sample_source_runtime_env(
    dae: &dae::Dae,
    expr: &dae::Expression,
    env: &VarEnv<f64>,
) -> Option<VarEnv<f64>> {
    let counts = dae.runtime_partition_scalar_counts();
    let solver_len = counts.x + counts.y;
    let n_x = counts.x;
    let needs_runtime_closure = expr_needs_sample_source_runtime_closure(dae, expr);
    let needs_env_only_alias_closure =
        expr_needs_sample_source_env_only_alias_closure(dae, expr, n_x);
    if !needs_runtime_closure && !needs_env_only_alias_closure {
        return None;
    }

    let mut recovered_env = env.clone();
    if needs_runtime_closure {
        let direct_assignment_ctx =
            crate::runtime::assignment::build_runtime_direct_assignment_context(
                dae, solver_len, n_x,
            );
        let alias_ctx =
            crate::runtime::alias::build_runtime_alias_propagation_context(dae, solver_len, n_x);
        let mut y_scratch = vec![0.0; solver_len];
        crate::runtime::layout::sync_solver_values_from_env(dae, &mut y_scratch, &recovered_env);

        let max_passes = (dae.f_x.len() + dae.f_z.len() + dae.f_m.len()).max(4);
        for _ in 0..max_passes {
            // MLS §16.5.1: sample() may read continuous-time derivative helpers
            // that prepare normalized away into der(state) aliases, so recover
            // those before direct-assignment and alias closure.
            let derivative_updates =
                crate::runtime::assignment::propagate_runtime_derivative_aliases_from_env(
                    dae,
                    n_x,
                    &mut recovered_env,
                );
            let direct_updates = crate::runtime::assignment::propagate_runtime_direct_assignments_from_env_with_context(
                &direct_assignment_ctx,
                dae,
                &mut y_scratch,
                n_x,
                &mut recovered_env,
            );
            let alias_updates =
                crate::runtime::alias::propagate_runtime_alias_components_from_env_with_context(
                    &alias_ctx,
                    &mut y_scratch,
                    n_x,
                    &mut recovered_env,
                );
            if derivative_updates + direct_updates + alias_updates == 0 {
                break;
            }
        }
    }
    let _ = propagate_sample_source_env_only_aliases(dae, expr, n_x, &mut recovered_env);

    Some(recovered_env)
}

pub(super) fn build_sample_source_left_limit_env(
    dae: &dae::Dae,
    env: &VarEnv<f64>,
    use_full_left_limit: bool,
) -> VarEnv<f64> {
    let mut left_limit_env = env.clone();
    for (name, pre) in rumoca_eval_dae::runtime::snapshot_pre_values() {
        if name == "time"
            || (!use_full_left_limit && !sample_source_prefers_pre_value(dae, name.as_str()))
        {
            continue;
        }
        left_limit_env.set(name.as_str(), clamp_finite(pre));
    }
    left_limit_env
}

pub(super) fn expr_uses_only_parameter_like_refs(dae: &dae::Dae, expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::VarRef { name, .. } => {
            dae.parameters.contains_key(name) || dae.constants.contains_key(name)
        }
        dae::Expression::Literal(_) | dae::Expression::Empty => true,
        dae::Expression::Unary { rhs, .. } => expr_uses_only_parameter_like_refs(dae, rhs),
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_uses_only_parameter_like_refs(dae, lhs)
                && expr_uses_only_parameter_like_refs(dae, rhs)
        }
        dae::Expression::BuiltinCall { args, .. }
        | dae::Expression::FunctionCall { args, .. }
        | dae::Expression::Array { elements: args, .. }
        | dae::Expression::Tuple { elements: args } => args
            .iter()
            .all(|arg| expr_uses_only_parameter_like_refs(dae, arg)),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().all(|(condition, value)| {
                expr_uses_only_parameter_like_refs(dae, condition)
                    && expr_uses_only_parameter_like_refs(dae, value)
            }) && expr_uses_only_parameter_like_refs(dae, else_branch)
        }
        dae::Expression::Range { start, step, end } => {
            expr_uses_only_parameter_like_refs(dae, start)
                && step
                    .as_deref()
                    .is_none_or(|value| expr_uses_only_parameter_like_refs(dae, value))
                && expr_uses_only_parameter_like_refs(dae, end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_uses_only_parameter_like_refs(dae, expr)
                && indices
                    .iter()
                    .all(|index| expr_uses_only_parameter_like_refs(dae, &index.range))
                && filter
                    .as_deref()
                    .is_none_or(|value| expr_uses_only_parameter_like_refs(dae, value))
        }
        dae::Expression::Index { base, subscripts } => {
            expr_uses_only_parameter_like_refs(dae, base)
                && subscripts.iter().all(|subscript| match subscript {
                    dae::Subscript::Expr(expr) => expr_uses_only_parameter_like_refs(dae, expr),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => true,
                })
        }
        dae::Expression::FieldAccess { base, .. } => expr_uses_only_parameter_like_refs(dae, base),
    }
}

pub(super) fn resolved_clock_expr_uses_exact_timing(
    dae: &dae::Dae,
    clock_expr: &dae::Expression,
    env: &VarEnv<f64>,
    remaining_depth: usize,
) -> bool {
    if remaining_depth == 0 {
        return false;
    }
    let dae::Expression::FunctionCall { name, args, .. } = clock_expr else {
        return false;
    };
    let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
    match short {
        "Clock" => {
            if args.is_empty() {
                return false;
            }
            if args.len() == 1
                && crate::runtime::clock::sample_clock_arg_is_explicit_clock(dae, &args[0], env)
            {
                return resolved_clock_expr_uses_exact_timing(
                    dae,
                    &args[0],
                    env,
                    remaining_depth - 1,
                );
            }
            args.iter()
                .all(|arg| expr_uses_only_parameter_like_refs(dae, arg))
                && rumoca_eval_dae::runtime::infer_clock_timing_seconds(clock_expr, env).is_some()
        }
        "subSample" | "superSample" | "shiftSample" | "backSample" => {
            args.first().is_some_and(|source| {
                resolved_clock_expr_uses_exact_timing(dae, source, env, remaining_depth - 1)
            }) && args
                .iter()
                .skip(1)
                .all(|arg| expr_uses_only_parameter_like_refs(dae, arg))
                && rumoca_eval_dae::runtime::infer_clock_timing_seconds(clock_expr, env).is_some()
        }
        _ => false,
    }
}

pub(super) fn clock_expr_uses_exact_timing(
    dae: &dae::Dae,
    clock_expr: &dae::Expression,
    env: &VarEnv<f64>,
) -> bool {
    explicit_signal_clock_expr(dae, clock_expr, env, 8).is_some_and(|resolved_expr| {
        resolved_clock_expr_uses_exact_timing(dae, &resolved_expr, env, 8)
    })
}

pub(super) fn eval_sample_source_tick_value(
    dae: &dae::Dae,
    expr: &dae::Expression,
    clock_expr: Option<&dae::Expression>,
    env: &VarEnv<f64>,
) -> f64 {
    let recovered_env = build_sample_source_runtime_env(dae, expr, env);
    let base_env = recovered_env.as_ref().unwrap_or(env);
    let use_exact_timing_mix =
        clock_expr.is_some_and(|clock| clock_expr_uses_exact_timing(dae, clock, base_env));
    hotpath_stats::inc_left_limit_read();
    if let dae::Expression::VarRef { name, subscripts } = expr
        && name.as_str() == "time"
        && subscripts.is_empty()
    {
        return clamp_finite(base_env.get("time"));
    }

    if let dae::Expression::VarRef { name, subscripts } = expr
        && let Some(key) = canonical_var_ref_key(name, subscripts)
    {
        if sample_source_prefers_pre_value(dae, key.as_str())
            && let Some(pre) = rumoca_eval_dae::runtime::get_pre_value(&key)
        {
            return clamp_finite(pre);
        }
        if use_exact_timing_mix {
            return clamp_finite(eval_discrete_scalar_value(expr, base_env));
        }
    }

    let left_limit_env = build_sample_source_left_limit_env(dae, base_env, !use_exact_timing_mix);
    eval_discrete_scalar_value(expr, &left_limit_env)
}

pub(super) fn sampled_tick_value(
    dae: &dae::Dae,
    expr: &dae::Expression,
    clock_expr: Option<&dae::Expression>,
    env: &VarEnv<f64>,
) -> f64 {
    if let dae::Expression::VarRef { name, subscripts } = expr
        && name.as_str() == "time"
        && subscripts.is_empty()
    {
        // MLS §16.5.1: sample(time) at a tick yields the current event time,
        // not the previous representable left-limit of time.
        return clamp_finite(env.get("time"));
    }
    // MLS §16.5.1: exact periodic clocks can read the current continuous
    // event-time value at the tick, but event clocks and non-continuous
    // sources must still observe the event-entry left-limit.
    eval_sample_source_tick_value(dae, expr, clock_expr, env)
}

pub(super) fn implicit_sampled_tick_value(
    dae: &dae::Dae,
    expr: &dae::Expression,
    env: &VarEnv<f64>,
) -> f64 {
    let recovered_env = build_sample_source_runtime_env(dae, expr, env);
    let base_env = recovered_env.as_ref().unwrap_or(env);
    hotpath_stats::inc_left_limit_read();
    if let dae::Expression::VarRef { name, subscripts } = expr
        && name.as_str() == "time"
        && subscripts.is_empty()
    {
        return clamp_finite(base_env.get("time"));
    }

    let sample_env = build_sample_source_left_limit_env(dae, base_env, false);
    eval_discrete_scalar_value(expr, &sample_env)
}

pub(super) fn sampled_array_tick_values(
    dae: &dae::Dae,
    expr: &dae::Expression,
    clock_expr: Option<&dae::Expression>,
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Vec<f64> {
    let recovered_env = build_sample_source_runtime_env(dae, expr, env);
    let base_env = recovered_env.as_ref().unwrap_or(env);
    let use_exact_timing_mix =
        clock_expr.is_some_and(|clock| clock_expr_uses_exact_timing(dae, clock, base_env));
    if let dae::Expression::VarRef { name, subscripts } = expr
        && name.as_str() == "time"
        && subscripts.is_empty()
    {
        return vec![clamp_finite(base_env.get("time")); expected_len];
    }

    let left_limit_env = build_sample_source_left_limit_env(dae, base_env, !use_exact_timing_mix);
    evaluate_direct_assignment_values(expr, &left_limit_env, expected_len)
        .into_iter()
        .map(clamp_finite)
        .collect()
}

pub(super) fn implicit_sampled_array_tick_values(
    dae: &dae::Dae,
    expr: &dae::Expression,
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Vec<f64> {
    let recovered_env = build_sample_source_runtime_env(dae, expr, env);
    let base_env = recovered_env.as_ref().unwrap_or(env);
    if let dae::Expression::VarRef { name, subscripts } = expr
        && name.as_str() == "time"
        && subscripts.is_empty()
    {
        return vec![clamp_finite(base_env.get("time")); expected_len];
    }

    let sample_env = build_sample_source_left_limit_env(dae, base_env, false);
    evaluate_direct_assignment_values(expr, &sample_env, expected_len)
        .into_iter()
        .map(clamp_finite)
        .collect()
}

pub(super) fn inferred_clock_timing_active(expr: &dae::Expression, env: &VarEnv<f64>) -> bool {
    let Some((period, phase)) = rumoca_eval_dae::runtime::infer_clock_timing_seconds(expr, env)
    else {
        return false;
    };
    if !(period.is_finite() && phase.is_finite() && period > 0.0) {
        return false;
    }
    let t = env.get("time");
    if !t.is_finite() {
        return false;
    }
    let k = ((t - phase) / period).round();
    if !k.is_finite() || k < 0.0 {
        return false;
    }
    crate::timeline::sample_time_match_with_tol(phase + k * period, t)
}

pub(super) fn eval_sample_clock_active(
    dae: &dae::Dae,
    clock_expr: &dae::Expression,
    env: &VarEnv<f64>,
) -> bool {
    let active = eval_sample_clock_active_inner(dae, clock_expr, env, 8);
    hotpath_stats::inc_sample_active_check(active);
    active
}

pub(super) fn explicit_signal_clock_expr(
    dae: &dae::Dae,
    expr: &dae::Expression,
    env: &VarEnv<f64>,
    remaining_depth: usize,
) -> Option<dae::Expression> {
    if remaining_depth == 0 {
        return match expr {
            dae::Expression::FunctionCall { name, .. } => {
                let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
                matches!(short, "Clock" | "firstTick").then(|| expr.clone())
            }
            dae::Expression::VarRef { .. }
                if crate::runtime::clock::sample_clock_arg_is_explicit_clock(dae, expr, env) =>
            {
                Some(expr.clone())
            }
            _ => None,
        };
    }
    match expr {
        dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            if let Some(source) =
                crate::runtime::clock::sample_clock_alias_source_expr(dae, name, subscripts)
                && let Some(resolved) =
                    explicit_signal_clock_expr(dae, &source, env, remaining_depth - 1)
            {
                return Some(resolved);
            }
            // MLS §16.5.1/§16.5.2: sampled/derived value operators keep the
            // clock of their source signal, even when that clock is exposed via
            // a solver-backed alias such as `periodicClock.c - sample1.clock`.
            if crate::runtime::clock::sample_clock_arg_is_explicit_clock(dae, expr, env) {
                return Some(expr.clone());
            }
            rumoca_eval_dae::runtime::infer_clock_timing_seconds(expr, env).map(|_| expr.clone())
        }
        dae::Expression::BuiltinCall { function, args }
            if *function == dae::BuiltinFunction::Sample && args.len() >= 2 =>
        {
            let clock_expr = &args[1];
            if !crate::runtime::clock::sample_clock_arg_is_explicit_clock(dae, clock_expr, env) {
                return None;
            }
            explicit_signal_clock_expr(dae, clock_expr, env, remaining_depth - 1)
                .or_else(|| Some(clock_expr.clone()))
        }
        dae::Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            match short {
                "Clock" | "firstTick" => Some(expr.clone()),
                // MLS §16.5.1: hold/previous/noClock keep the input clock.
                "hold" | "previous" | "noClock" => {
                    explicit_signal_clock_expr(dae, args.first()?, env, remaining_depth - 1)
                }
                "subSample" | "superSample" | "shiftSample" | "backSample" => {
                    let base_clock =
                        explicit_signal_clock_expr(dae, args.first()?, env, remaining_depth - 1)?;
                    let mut derived_args = Vec::with_capacity(args.len());
                    derived_args.push(base_clock);
                    derived_args.extend(args.iter().skip(1).cloned());
                    Some(dae::Expression::FunctionCall {
                        name: dae::VarName::new(short),
                        args: derived_args,
                        is_constructor: *is_constructor,
                    })
                }
                _ => None,
            }
        }
        _ => None,
    }
}

pub(super) fn resolve_active_derived_clock_source_expr(
    dae: &dae::Dae,
    expr: &dae::Expression,
    env: &VarEnv<f64>,
    remaining_depth: usize,
) -> Option<dae::Expression> {
    if remaining_depth == 0 {
        return explicit_signal_clock_expr(dae, expr, env, remaining_depth);
    }
    match expr {
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            for (condition, value) in branches {
                let active = eval_scalar_bool_expr_fast(condition, env).or_else(|| {
                    Some(clock_bool(rumoca_eval_dae::runtime::eval_expr::<f64>(
                        condition, env,
                    )))
                })?;
                if active {
                    return resolve_active_derived_clock_source_expr(
                        dae,
                        value,
                        env,
                        remaining_depth - 1,
                    );
                }
            }
            resolve_active_derived_clock_source_expr(dae, else_branch, env, remaining_depth - 1)
        }
        dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            if let Some(source) =
                crate::runtime::clock::sample_clock_alias_source_expr(dae, name, subscripts)
            {
                return resolve_active_derived_clock_source_expr(
                    dae,
                    &source,
                    env,
                    remaining_depth - 1,
                );
            }
            explicit_signal_clock_expr(dae, expr, env, remaining_depth)
        }
        _ => explicit_signal_clock_expr(dae, expr, env, remaining_depth),
    }
}

pub(super) fn explicit_signal_clock_active(
    dae: &dae::Dae,
    expr: &dae::Expression,
    env: &VarEnv<f64>,
) -> bool {
    explicit_signal_clock_expr(dae, expr, env, 8)
        .is_some_and(|clock_expr| eval_sample_clock_active(dae, &clock_expr, env))
}

pub(super) fn eval_sample_clock_active_inner(
    dae: &dae::Dae,
    clock_expr: &dae::Expression,
    env: &VarEnv<f64>,
    remaining_depth: usize,
) -> bool {
    if let dae::Expression::VarRef { name, subscripts } = clock_expr
        && subscripts.is_empty()
        && remaining_depth > 0
        && let Some(source) =
            crate::runtime::clock::sample_clock_alias_source_expr(dae, name, subscripts)
    {
        return eval_sample_clock_active_inner(dae, &source, env, remaining_depth - 1);
    }

    if let dae::Expression::FunctionCall { name, .. } = clock_expr {
        let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
        if short == "Clock" {
            return eval_clock_edge_assignment(dae, clock_expr, env).is_some_and(clock_bool);
        }
        if matches!(
            short,
            "subSample" | "superSample" | "shiftSample" | "backSample"
        ) && let dae::Expression::FunctionCall {
            args,
            is_constructor,
            ..
        } = clock_expr
            && let Some(source_expr) = args.first()
            && let Some(resolved_source) =
                resolve_active_derived_clock_source_expr(dae, source_expr, env, remaining_depth)
        {
            let mut resolved_args = Vec::with_capacity(args.len());
            resolved_args.push(resolved_source);
            resolved_args.extend(args.iter().skip(1).cloned());
            let resolved_expr = dae::Expression::FunctionCall {
                name: dae::VarName::new(short),
                args: resolved_args,
                is_constructor: *is_constructor,
            };
            if rumoca_eval_dae::runtime::infer_clock_timing_seconds(&resolved_expr, env).is_some() {
                return fast_clock_scalar(&resolved_expr, env).is_some_and(clock_bool);
            }
        }
        return fast_clock_scalar(clock_expr, env).is_some_and(clock_bool);
    }

    fast_clock_scalar(clock_expr, env).is_some_and(clock_bool)
}

pub(super) fn is_clock_function_name(short: &str) -> bool {
    matches!(
        short,
        "Clock" | "subSample" | "superSample" | "shiftSample" | "backSample" | "firstTick"
    )
}

pub(super) fn eval_clocked_sample_assignment(
    dae: &dae::Dae,
    target: &str,
    solution: &dae::Expression,
    env: &VarEnv<f64>,
) -> Option<f64> {
    let dae::Expression::BuiltinCall { function, args } = solution else {
        return None;
    };
    if *function != dae::BuiltinFunction::Sample || args.len() < 2 {
        return None;
    }

    let value_expr = &args[0];
    let clock_expr = &args[1];
    // Disambiguate sample(value, clockExpr) from sample(start, interval):
    // only treat the 2nd argument as a clock when it is an explicit clock
    // constructor/derivation expression.
    if !crate::runtime::clock::sample_clock_arg_is_explicit_clock(dae, clock_expr, env) {
        return None;
    }

    let clock_active = eval_sample_clock_active(dae, clock_expr, env);
    if clock_active {
        return Some(sampled_tick_value(dae, value_expr, Some(clock_expr), env));
    }

    Some(sampled_target_held_value(target, None, env))
}

pub(super) fn eval_implicit_sample_assignment(
    dae: &dae::Dae,
    target: &str,
    solution: &dae::Expression,
    env: &VarEnv<f64>,
    implicit_clock_active: bool,
) -> Option<f64> {
    let dae::Expression::BuiltinCall { function, args } = solution else {
        return None;
    };
    if *function != dae::BuiltinFunction::Sample || args.len() != 1 {
        return None;
    }
    let value_expr = &args[0];
    if implicit_clock_active {
        Some(implicit_sampled_tick_value(dae, value_expr, env))
    } else {
        Some(sampled_target_held_value(target, Some(value_expr), env))
    }
}

pub(super) fn sampled_value_source_is_clock_expr(
    dae: &dae::Dae,
    expr: &dae::Expression,
    remaining_depth: usize,
) -> bool {
    if remaining_depth == 0 {
        return false;
    }
    match expr {
        dae::Expression::FunctionCall { name, .. } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            matches!(
                short,
                "Clock" | "subSample" | "superSample" | "shiftSample" | "backSample" | "firstTick"
            )
        }
        dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            crate::runtime::clock::sample_clock_alias_source_expr(dae, name, subscripts)
                .is_some_and(|source| {
                    sampled_value_source_is_clock_expr(dae, &source, remaining_depth - 1)
                })
        }
        _ => false,
    }
}

pub(super) fn discrete_value_source_expr<'a>(
    dae: &'a dae::Dae,
    target: &str,
    env: &VarEnv<f64>,
) -> Option<&'a dae::Expression> {
    dae.f_z.iter().chain(dae.f_m.iter()).find_map(|eq| {
        let (lhs, solution) =
            crate::runtime::assignment::discrete_assignment_from_equation_with_guard_env(eq, env)?;
        (lhs.as_str() == target).then_some(solution)
    })
}

pub(super) fn clocked_value_source_reads_current_tick_inner(
    dae: &dae::Dae,
    expr: &dae::Expression,
    env: &VarEnv<f64>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> bool {
    if remaining_depth == 0 {
        return false;
    }
    match expr {
        dae::Expression::BuiltinCall { function, args }
            if *function == dae::BuiltinFunction::Sample && args.len() >= 2 =>
        {
            crate::runtime::clock::sample_clock_arg_is_explicit_clock(dae, &args[1], env)
                && eval_sample_clock_active(dae, &args[1], env)
        }
        dae::Expression::BuiltinCall { function, args }
            if *function == dae::BuiltinFunction::Sample && args.len() == 1 =>
        {
            !args.is_empty()
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            for (condition, value) in branches {
                if eval_discrete_condition_bool(dae, condition, env).unwrap_or(false) {
                    return clocked_value_source_reads_current_tick_inner(
                        dae,
                        value,
                        env,
                        remaining_depth - 1,
                        visiting,
                    );
                }
            }
            clocked_value_source_reads_current_tick_inner(
                dae,
                else_branch,
                env,
                remaining_depth - 1,
                visiting,
            )
        }
        dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            let Some(key) = canonical_var_ref_key(name, subscripts) else {
                return false;
            };
            if !visiting.insert(key.clone()) {
                return false;
            }
            let reads_current =
                discrete_value_source_expr(dae, key.as_str(), env).is_some_and(|source| {
                    clocked_value_source_reads_current_tick_inner(
                        dae,
                        source,
                        env,
                        remaining_depth - 1,
                        visiting,
                    )
                });
            visiting.remove(&key);
            reads_current
        }
        _ => false,
    }
}

pub(super) fn clocked_value_source_reads_current_tick(
    dae: &dae::Dae,
    expr: &dae::Expression,
    env: &VarEnv<f64>,
) -> bool {
    clocked_value_source_reads_current_tick_inner(dae, expr, env, 8, &mut HashSet::new())
}

pub(super) fn sampled_value_operator_preserves_source_tick(
    solution: &dae::Expression,
    env: &VarEnv<f64>,
) -> bool {
    let dae::Expression::FunctionCall { name, args, .. } = solution else {
        return false;
    };
    let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
    match short {
        "shiftSample" | "backSample" => args
            .get(1)
            .and_then(|arg| eval_scalar_expr_fast(arg, env))
            .is_some_and(|shift| shift.abs() <= 1.0e-12),
        "subSample" | "superSample" => args
            .get(1)
            .and_then(|arg| eval_scalar_expr_fast(arg, env))
            .is_some_and(|factor| (factor.round() - 1.0).abs() <= 1.0e-12),
        _ => false,
    }
}

pub(super) fn eval_sampled_value_function_assignment(
    dae: &dae::Dae,
    target: &str,
    solution: &dae::Expression,
    env: &VarEnv<f64>,
    implicit_clock_active: bool,
) -> Option<f64> {
    let dae::Expression::FunctionCall { name, args, .. } = solution else {
        return None;
    };
    let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
    if !matches!(
        short,
        "subSample" | "superSample" | "shiftSample" | "backSample"
    ) {
        return None;
    }

    // MLS §16.5.1: value-form sampled-clock derivations read the sampled value
    // source on active ticks and otherwise hold the target's previous sample.
    let value_expr = args.first()?;
    if sampled_value_source_is_clock_expr(dae, value_expr, 8) {
        return None;
    }
    let explicit_clock_active = explicit_signal_clock_active(dae, solution, env)
        || inferred_clock_timing_active(solution, env);
    let source_reads_current_tick = sampled_value_operator_preserves_source_tick(solution, env)
        && clocked_value_source_reads_current_tick(dae, value_expr, env);
    if implicit_clock_active || explicit_clock_active {
        if source_reads_current_tick {
            return Some(eval_discrete_scalar_value(value_expr, env));
        }
        // MLS §16.5.1: sampled-clock value operators read the source at the
        // active tick. If the source is itself a clocked value updated on this
        // tick, read its current settled value; otherwise evaluate the source
        // with exact-clock vs. event-clock left-limit rules.
        Some(sampled_tick_value(dae, value_expr, Some(solution), env))
    } else {
        Some(sampled_target_held_value(target, None, env))
    }
}

pub(super) fn eval_hold_assignment(
    dae: &dae::Dae,
    target: &str,
    solution: &dae::Expression,
    env: &VarEnv<f64>,
    implicit_clock_active: bool,
) -> Option<f64> {
    let dae::Expression::FunctionCall { name, args, .. } = solution else {
        return None;
    };
    let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
    if short != "hold" {
        return None;
    }

    let value_expr = args.first()?;
    let explicit_clock_active = explicit_signal_clock_active(dae, value_expr, env)
        || inferred_clock_timing_active(value_expr, env);
    if implicit_clock_active || explicit_clock_active {
        Some(eval_discrete_scalar_value(value_expr, env))
    } else {
        Some(sampled_target_held_value(target, None, env))
    }
}

pub(super) fn eval_discrete_assignment_array_special_values(
    dae: &dae::Dae,
    target: &str,
    solution: &dae::Expression,
    env: &VarEnv<f64>,
    expected_len: usize,
    implicit_clock_active: bool,
) -> Option<Vec<f64>> {
    match solution {
        dae::Expression::BuiltinCall { function, args }
            if *function == dae::BuiltinFunction::Sample && args.len() >= 2 =>
        {
            let value_expr = &args[0];
            let clock_expr = &args[1];
            if !crate::runtime::clock::sample_clock_arg_is_explicit_clock(dae, clock_expr, env) {
                return None;
            }
            if eval_sample_clock_active(dae, clock_expr, env) {
                Some(sampled_array_tick_values(
                    dae,
                    value_expr,
                    Some(clock_expr),
                    env,
                    expected_len,
                ))
            } else {
                Some(array_target_held_values(target, None, env, expected_len))
            }
        }
        dae::Expression::BuiltinCall { function, args }
            if *function == dae::BuiltinFunction::Sample && args.len() == 1 =>
        {
            let value_expr = &args[0];
            if implicit_clock_active {
                Some(implicit_sampled_array_tick_values(
                    dae,
                    value_expr,
                    env,
                    expected_len,
                ))
            } else {
                Some(array_target_held_values(
                    target,
                    Some(value_expr),
                    env,
                    expected_len,
                ))
            }
        }
        dae::Expression::FunctionCall { name, args, .. } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            match short {
                "subSample" | "superSample" | "shiftSample" | "backSample" => {
                    let value_expr = args.first()?;
                    if sampled_value_source_is_clock_expr(dae, value_expr, 8) {
                        return None;
                    }
                    let explicit_clock_active = explicit_signal_clock_active(dae, solution, env)
                        || inferred_clock_timing_active(solution, env);
                    if !(implicit_clock_active || explicit_clock_active) {
                        return Some(array_target_held_values(target, None, env, expected_len));
                    }
                    let source_reads_current_tick =
                        sampled_value_operator_preserves_source_tick(solution, env)
                            && clocked_value_source_reads_current_tick(dae, value_expr, env);
                    if source_reads_current_tick {
                        return Some(evaluate_direct_assignment_values(
                            value_expr,
                            env,
                            expected_len,
                        ));
                    }
                    Some(sampled_array_tick_values(
                        dae,
                        value_expr,
                        Some(solution),
                        env,
                        expected_len,
                    ))
                }
                "hold" => {
                    let value_expr = args.first()?;
                    let explicit_clock_active = explicit_signal_clock_active(dae, value_expr, env)
                        || inferred_clock_timing_active(value_expr, env);
                    if implicit_clock_active || explicit_clock_active {
                        Some(evaluate_direct_assignment_values(
                            value_expr,
                            env,
                            expected_len,
                        ))
                    } else {
                        Some(array_target_held_values(target, None, env, expected_len))
                    }
                }
                _ => None,
            }
        }
        _ => None,
    }
}
