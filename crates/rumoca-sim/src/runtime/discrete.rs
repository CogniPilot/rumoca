use crate::runtime::assignment::{
    canonical_var_ref_key, eval_assignment_scalar_fast, evaluate_direct_assignment_values,
};
use crate::runtime::hotpath_stats;
use crate::runtime::scalar_eval::{
    eval_left_limit_scalar_expr_fast, eval_scalar_bool_expr_fast, eval_scalar_expr_fast,
};
use rumoca_eval_dae::runtime::VarEnv;
use rumoca_ir_dae as dae;
use std::collections::{HashMap, HashSet};

fn clamp_finite(v: f64) -> f64 {
    if v.is_finite() { v } else { 0.0 }
}

fn clock_bool(value: f64) -> bool {
    value.is_finite() && value > 0.5
}

fn fast_clock_scalar(expr: &dae::Expression, env: &VarEnv<f64>) -> Option<f64> {
    eval_scalar_expr_fast(expr, env).map(clamp_finite)
}

fn projected_user_function_output_name<'a>(
    name: &'a dae::VarName,
    env: &VarEnv<f64>,
) -> Option<&'a str> {
    let requested = name.as_str();
    let mut split_positions: Vec<usize> = requested.match_indices('.').map(|(i, _)| i).collect();
    split_positions.reverse();
    for split_idx in split_positions {
        let base_name = &requested[..split_idx];
        let suffix = &requested[split_idx + 1..];
        let output_name = suffix
            .split_once('[')
            .map(|(base, _)| base)
            .unwrap_or(suffix);
        let Some(func) = env.functions.get(base_name) else {
            continue;
        };
        if func.outputs.iter().any(|output| output.name == output_name) {
            return Some(output_name);
        }
    }
    None
}

fn prefers_guard_env_for_discrete_expr(expr: &dae::Expression, guard_env: &VarEnv<f64>) -> bool {
    match expr {
        dae::Expression::FunctionCall { name, .. } => {
            projected_user_function_output_name(name, guard_env).is_some()
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(condition, value)| {
                prefers_guard_env_for_discrete_expr(condition, guard_env)
                    || prefers_guard_env_for_discrete_expr(value, guard_env)
            }) || prefers_guard_env_for_discrete_expr(else_branch, guard_env)
        }
        dae::Expression::Unary { rhs, .. } => prefers_guard_env_for_discrete_expr(rhs, guard_env),
        dae::Expression::Binary { lhs, rhs, .. } => {
            prefers_guard_env_for_discrete_expr(lhs, guard_env)
                || prefers_guard_env_for_discrete_expr(rhs, guard_env)
        }
        dae::Expression::BuiltinCall { args, .. }
        | dae::Expression::Array { elements: args, .. }
        | dae::Expression::Tuple { elements: args } => args
            .iter()
            .any(|arg| prefers_guard_env_for_discrete_expr(arg, guard_env)),
        dae::Expression::Range { start, step, end } => {
            prefers_guard_env_for_discrete_expr(start, guard_env)
                || step
                    .as_deref()
                    .is_some_and(|value| prefers_guard_env_for_discrete_expr(value, guard_env))
                || prefers_guard_env_for_discrete_expr(end, guard_env)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            prefers_guard_env_for_discrete_expr(expr, guard_env)
                || indices
                    .iter()
                    .any(|index| prefers_guard_env_for_discrete_expr(&index.range, guard_env))
                || filter
                    .as_deref()
                    .is_some_and(|value| prefers_guard_env_for_discrete_expr(value, guard_env))
        }
        dae::Expression::Index { base, subscripts } => {
            prefers_guard_env_for_discrete_expr(base, guard_env)
                || subscripts.iter().any(|subscript| match subscript {
                    dae::Subscript::Expr(expr) => {
                        prefers_guard_env_for_discrete_expr(expr, guard_env)
                    }
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => {
            prefers_guard_env_for_discrete_expr(base, guard_env)
        }
        _ => false,
    }
}

fn eval_discrete_scalar_value(expr: &dae::Expression, env: &VarEnv<f64>) -> f64 {
    eval_discrete_scalar_value_in_env(expr, env)
}

fn eval_discrete_scalar_value_in_env(expr: &dae::Expression, env: &VarEnv<f64>) -> f64 {
    if let Some(value) = eval_assignment_scalar_fast(expr, env) {
        return clamp_finite(value);
    }
    match expr {
        dae::Expression::If { .. }
        | dae::Expression::BuiltinCall { .. }
        | dae::Expression::FunctionCall { .. } => {
            clamp_finite(rumoca_eval_dae::runtime::eval_expr::<f64>(expr, env))
        }
        _ => clamp_finite(
            evaluate_direct_assignment_values(expr, env, 1)
                .into_iter()
                .next()
                .unwrap_or(0.0),
        ),
    }
}

fn state_base_name(name: &str) -> String {
    dae::component_base_name(name).unwrap_or_else(|| name.to_string())
}

fn is_state_target(dae: &dae::Dae, target: &str) -> bool {
    let base = state_base_name(target);
    dae.states.contains_key(&dae::VarName::new(base))
}

fn expression_references_target(solution: &dae::Expression, target: &str) -> bool {
    let mut refs = HashSet::new();
    solution.collect_var_refs(&mut refs);
    let target_base = state_base_name(target);
    refs.contains(&dae::VarName::new(target_base))
}

fn eval_state_assignment_with_left_limit_target(
    target: &str,
    solution: &dae::Expression,
    env: &VarEnv<f64>,
) -> f64 {
    if let Some(value) = eval_left_limit_scalar_expr_fast(solution, env) {
        return clamp_finite(value);
    }
    let mut left_limit_env = env.clone();
    if let Some(pre) = rumoca_eval_dae::runtime::get_pre_value(target) {
        left_limit_env.set(target, clamp_finite(pre));
    }
    let target_base = state_base_name(target);
    if let Some(pre) = rumoca_eval_dae::runtime::get_pre_value(&target_base) {
        left_limit_env.set(&target_base, clamp_finite(pre));
    }
    eval_discrete_scalar_value(solution, &left_limit_env)
}

fn expr_has_var_refs(expr: &dae::Expression) -> bool {
    let mut refs = HashSet::new();
    expr.collect_var_refs(&mut refs);
    !refs.is_empty()
}

pub fn eval_clock_edge_assignment(
    dae: &dae::Dae,
    solution: &dae::Expression,
    env: &VarEnv<f64>,
) -> Option<f64> {
    let dae::Expression::FunctionCall { name, args, .. } = solution else {
        return None;
    };
    let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
    if short != "Clock" {
        return None;
    }
    hotpath_stats::inc_clock_edge_eval();
    if args.len() >= 2 {
        return fast_clock_scalar(solution, env);
    }
    let trigger = args.first()?;
    if let dae::Expression::VarRef {
        name: trigger_name,
        subscripts,
    } = trigger
        && subscripts.is_empty()
    {
        let key = dae::VarName::new(trigger_name.as_str());
        if dae.discrete_valued.contains_key(&key) {
            // Discrete-valued triggers (Boolean/Integer) use rising-edge semantics.
        } else if rumoca_eval_dae::runtime::infer_clock_timing_seconds(solution, env).is_some() {
            // Numeric aliases such as Clock(period) should keep periodic timing semantics.
            return fast_clock_scalar(solution, env);
        }
        if dae.parameters.contains_key(&key) || dae.constants.contains_key(&key) {
            return fast_clock_scalar(solution, env);
        }
    }
    if !expr_has_var_refs(trigger) {
        return fast_clock_scalar(solution, env);
    }

    let current = clock_bool(fast_clock_scalar(trigger, env)?);
    let previous = match trigger {
        dae::Expression::VarRef { name, subscripts } => canonical_var_ref_key(name, subscripts)
            .and_then(|key| rumoca_eval_dae::runtime::get_pre_value(&key))
            .is_some_and(clock_bool),
        _ => false,
    };
    Some(if current && !previous { 1.0 } else { 0.0 })
}

mod sampling;
use sampling::*;

pub fn expr_uses_previous(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::FunctionCall { name, args, .. } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            short == "previous" || args.iter().any(expr_uses_previous)
        }
        dae::Expression::BuiltinCall { args, .. } => args.iter().any(expr_uses_previous),
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_uses_previous(lhs.as_ref()) || expr_uses_previous(rhs.as_ref())
        }
        dae::Expression::Unary { rhs, .. } => expr_uses_previous(rhs.as_ref()),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches
                .iter()
                .any(|(cond, value)| expr_uses_previous(cond) || expr_uses_previous(value))
                || expr_uses_previous(else_branch.as_ref())
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(expr_uses_previous)
        }
        dae::Expression::Range { start, step, end } => {
            expr_uses_previous(start.as_ref())
                || step.as_ref().is_some_and(|value| expr_uses_previous(value))
                || expr_uses_previous(end.as_ref())
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_uses_previous(expr.as_ref())
                || indices.iter().any(|index| expr_uses_previous(&index.range))
                || filter
                    .as_ref()
                    .is_some_and(|value| expr_uses_previous(value))
        }
        dae::Expression::Index { base, subscripts } => {
            expr_uses_previous(base.as_ref())
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(value) => expr_uses_previous(value.as_ref()),
                    _ => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => expr_uses_previous(base.as_ref()),
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

fn eval_discrete_assignment_value_in_env(
    dae: &dae::Dae,
    target: &str,
    solution: &dae::Expression,
    env: &VarEnv<f64>,
    implicit_clock_active: bool,
) -> f64 {
    if let Some(value) = eval_clocked_sample_assignment(dae, target, solution, env) {
        return value;
    }
    if let Some(value) =
        eval_implicit_sample_assignment(dae, target, solution, env, implicit_clock_active)
    {
        return value;
    }
    if let Some(value) =
        eval_sampled_value_function_assignment(dae, target, solution, env, implicit_clock_active)
    {
        return value;
    }
    if !implicit_clock_active && expr_uses_previous(solution) {
        return sampled_target_held_value(target, Some(solution), env);
    }
    if let Some(value) = eval_hold_assignment(dae, target, solution, env, implicit_clock_active) {
        return value;
    }
    if let Some(value) = eval_clock_edge_assignment(dae, solution, env) {
        return value;
    }
    if is_state_target(dae, target) && expression_references_target(solution, target) {
        return eval_state_assignment_with_left_limit_target(target, solution, env);
    }
    eval_discrete_scalar_value(solution, env)
}

pub fn eval_discrete_assignment_value(
    dae: &dae::Dae,
    target: &str,
    solution: &dae::Expression,
    env: &VarEnv<f64>,
    implicit_clock_active: bool,
) -> f64 {
    eval_discrete_assignment_value_in_env(dae, target, solution, env, implicit_clock_active)
}

pub fn dims_total_size(dims: &[i64]) -> Option<usize> {
    if dims.is_empty() {
        return None;
    }
    let mut total = 1usize;
    for dim in dims {
        if *dim <= 0 {
            return None;
        }
        let Ok(dim_usize) = usize::try_from(*dim) else {
            return None;
        };
        total = total.checked_mul(dim_usize)?;
    }
    Some(total)
}

fn eval_discrete_condition_bool(
    dae: &dae::Dae,
    condition: &dae::Expression,
    env: &VarEnv<f64>,
) -> Option<bool> {
    if let dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } = condition
    {
        let mut any_active = false;
        for element in elements {
            any_active |= eval_discrete_condition_bool(dae, element, env)?;
        }
        return Some(any_active);
    }
    if let dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Edge,
        args,
    } = condition
        && let Some(signal) = args.first()
        // MLS §16.5.1 / Appendix B: within a clocked discrete partition,
        // edge(Clock()) is driven by the partition's active tick, not by the
        // generic fast scalar fallback. The implicit clock has no explicit
        // pre-store entry, so use the settle-round implicit-clock marker here.
        && is_implicit_clock_expr(signal)
    {
        return Some(clock_bool(
            env.get(rumoca_eval_dae::runtime::IMPLICIT_CLOCK_ACTIVE_ENV_KEY),
        ));
    }
    if let dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Edge,
        args,
    } = condition
        && let Some(signal) = args.first()
        // MLS §16.5.1: exact clock indicators are false on the left-limit and
        // true on the tick. Evaluate those through the clock-aware path before
        // the generic fast bool path, which otherwise treats missing pre(clock)
        // history as the current env value.
        && (explicit_signal_clock_active(dae, signal, env)
            || inferred_clock_timing_active(signal, env))
    {
        return Some(true);
    }
    if let Some(value) = eval_scalar_bool_expr_fast(condition, env) {
        return Some(value);
    }
    Some(clock_bool(rumoca_eval_dae::runtime::eval_expr::<f64>(
        condition, env,
    )))
}

fn is_implicit_clock_expr(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::FunctionCall { name, args, .. } => {
            name.as_str().rsplit('.').next().unwrap_or(name.as_str()) == "Clock" && args.is_empty()
        }
        _ => false,
    }
}

fn select_discrete_array_if_branch<'a>(
    dae: &dae::Dae,
    solution: &'a dae::Expression,
    env: &VarEnv<f64>,
) -> Option<(&'a dae::Expression, bool)> {
    let dae::Expression::If {
        branches,
        else_branch,
    } = solution
    else {
        return None;
    };

    for (condition, value) in branches {
        if eval_discrete_condition_bool(dae, condition, env)? {
            return Some((value, true));
        }
    }
    Some((else_branch.as_ref(), false))
}

fn condition_uses_event_entry_time_marker(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::VarRef { name, subscripts }
            if name.as_str() == "time" && subscripts.is_empty() =>
        {
            true
        }
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Initial,
            ..
        } => true,
        dae::Expression::BuiltinCall { args, .. } | dae::Expression::FunctionCall { args, .. } => {
            args.iter().any(condition_uses_event_entry_time_marker)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            condition_uses_event_entry_time_marker(lhs)
                || condition_uses_event_entry_time_marker(rhs)
        }
        dae::Expression::Unary { rhs, .. } => condition_uses_event_entry_time_marker(rhs),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(condition, value)| {
                condition_uses_event_entry_time_marker(condition)
                    || condition_uses_event_entry_time_marker(value)
            }) || condition_uses_event_entry_time_marker(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(condition_uses_event_entry_time_marker)
        }
        dae::Expression::Range { start, step, end } => {
            condition_uses_event_entry_time_marker(start)
                || step
                    .as_deref()
                    .is_some_and(condition_uses_event_entry_time_marker)
                || condition_uses_event_entry_time_marker(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            condition_uses_event_entry_time_marker(expr)
                || indices
                    .iter()
                    .any(|index| condition_uses_event_entry_time_marker(&index.range))
                || filter
                    .as_deref()
                    .is_some_and(condition_uses_event_entry_time_marker)
        }
        dae::Expression::Index { base, subscripts } => {
            condition_uses_event_entry_time_marker(base)
                || subscripts.iter().any(|subscript| match subscript {
                    dae::Subscript::Expr(expr) => condition_uses_event_entry_time_marker(expr),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => condition_uses_event_entry_time_marker(base),
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

fn condition_prefers_event_entry_guard(
    condition: &dae::Expression,
    guard_env: &VarEnv<f64>,
) -> bool {
    condition_uses_event_entry_time_marker(condition)
        || prefers_guard_env_for_discrete_expr(condition, guard_env)
}

fn select_discrete_scalar_if_branch<'a>(
    dae: &dae::Dae,
    solution: &'a dae::Expression,
    env: &VarEnv<f64>,
    guard_env: &VarEnv<f64>,
) -> Option<&'a dae::Expression> {
    let dae::Expression::If {
        branches,
        else_branch,
    } = solution
    else {
        return None;
    };

    for (condition, value) in branches {
        let condition_env = if condition_prefers_event_entry_guard(condition, guard_env) {
            guard_env
        } else {
            env
        };
        if eval_discrete_condition_bool(dae, condition, condition_env)? {
            return Some(value);
        }
    }
    Some(else_branch.as_ref())
}

fn evaluate_discrete_assignment_array_values(
    dae: &dae::Dae,
    target: &str,
    solution: &dae::Expression,
    env: &VarEnv<f64>,
    expected_len: usize,
    implicit_clock_active: bool,
) -> Vec<f64> {
    let (selected_expr, selected_active_branch) =
        select_discrete_array_if_branch(dae, solution, env).unwrap_or((solution, false));
    let mut values = eval_discrete_assignment_array_special_values(
        dae,
        target,
        selected_expr,
        env,
        expected_len,
        implicit_clock_active,
    )
    .unwrap_or_else(|| evaluate_direct_assignment_values(selected_expr, env, expected_len));
    let should_hold_previous = if matches!(solution, dae::Expression::If { .. }) {
        !selected_active_branch && expr_uses_previous(solution)
    } else {
        !implicit_clock_active && expr_uses_previous(solution)
    };
    if should_hold_previous {
        for (index, value) in values.iter_mut().enumerate() {
            let key = format!("{target}[{}]", index + 1);
            let base_fallback = base_target_hold_fallback(env, target, index);
            *value = rumoca_eval_dae::runtime::get_pre_value(&key)
                .or_else(|| env.vars.get(&key).copied())
                .or(base_fallback)
                .unwrap_or(*value);
        }
    }
    values
}

pub struct ScalarDiscreteEquationInput<'a> {
    pub dae: &'a dae::Dae,
    pub eq: &'a dae::Equation,
    pub target: &'a str,
    pub solution: &'a dae::Expression,
    pub env: &'a mut VarEnv<f64>,
    pub rhs_env: Option<&'a VarEnv<f64>>,
    pub implicit_clock_active: bool,
}

fn indexed_source_value_or_default(
    env: &VarEnv<f64>,
    source_name: &str,
    index: usize,
    default_value: f64,
) -> (f64, bool) {
    let indexed_key = format!("{source_name}[{index}]");
    if let Some(value) = env.vars.get(indexed_key.as_str()).copied() {
        return (value, true);
    }
    (default_value, false)
}

fn base_target_hold_fallback(env: &VarEnv<f64>, target: &str, index: usize) -> Option<f64> {
    if index == 0 {
        return env.vars.get(target).copied();
    }
    None
}

pub fn apply_scalar_discrete_partition_equation(
    input: ScalarDiscreteEquationInput<'_>,
    mut set_target_value: impl FnMut(&mut VarEnv<f64>, &str, f64) -> bool,
    mut on_scalar_eval: impl FnMut(&str, f64, f64),
) -> bool {
    let mut eval_override = |_eq: &dae::Equation,
                             _target: &str,
                             _solution: &dae::Expression,
                             _env: &VarEnv<f64>,
                             _implicit_clock_active: bool|
     -> Option<f64> { None };
    apply_scalar_discrete_partition_equation_with_override(
        input,
        &mut set_target_value,
        &mut on_scalar_eval,
        &mut eval_override,
    )
}

fn apply_scalar_discrete_partition_equation_with_override(
    input: ScalarDiscreteEquationInput<'_>,
    set_target_value: &mut impl FnMut(&mut VarEnv<f64>, &str, f64) -> bool,
    on_scalar_eval: &mut impl FnMut(&str, f64, f64),
    eval_override: &mut impl FnMut(
        &dae::Equation,
        &str,
        &dae::Expression,
        &VarEnv<f64>,
        bool,
    ) -> Option<f64>,
) -> bool {
    let dae = input.dae;
    let eq = input.eq;
    let target = input.target;
    let solution = input.solution;
    let env = input.env;
    let rhs_env = input.rhs_env.unwrap_or(env);
    let implicit_clock_active = input.implicit_clock_active;

    if std::env::var_os("RUMOCA_DEBUG_COUNTER_ENABLE").is_some() && target == "Enable.y" {
        eprintln!(
            "DEBUG discrete target={target} before={} after={} stepTime={} time={} implicit_clock_active={implicit_clock_active}",
            env.get("Enable.before"),
            env.get("Enable.after"),
            env.get("Enable.stepTime"),
            env.get("time"),
        );
    }

    if !target.contains('[')
        && let Some(dims) = env.dims.get(target).cloned()
        && let Some(size) = dims_total_size(&dims)
        && size > 1
    {
        let mut values = evaluate_discrete_assignment_array_values(
            dae,
            target,
            solution,
            env,
            size,
            implicit_clock_active,
        );
        if let dae::Expression::VarRef { name, subscripts } = solution
            && subscripts.is_empty()
        {
            let mut indexed_values = Vec::with_capacity(size);
            let mut has_indexed_value = false;
            for index in 1..=size {
                let (value, sourced_from_indexed) =
                    indexed_source_value_or_default(env, name.as_str(), index, values[index - 1]);
                has_indexed_value |= sourced_from_indexed;
                indexed_values.push(value);
            }
            if has_indexed_value {
                values = indexed_values;
            }
        }
        let mut changed_any = false;
        for (index, value) in values.iter().copied().enumerate() {
            let key = format!("{target}[{}]", index + 1);
            changed_any |= set_target_value(env, key.as_str(), value);
        }
        if let Some(first) = values.first().copied() {
            changed_any |= set_target_value(env, target, first);
        }
        rumoca_eval_dae::runtime::set_array_entries(env, target, &dims, &values);
        return changed_any;
    }

    if let Some((lhs, rhs)) = crate::runtime::assignment::extract_alias_pair_from_equation(dae, eq)
    {
        if eq.origin.contains("connection equation:") {
            return false;
        }
        let lhs_runtime_unknown = crate::runtime::assignment::is_runtime_unknown_name(dae, &lhs);
        let rhs_runtime_unknown = crate::runtime::assignment::is_runtime_unknown_name(dae, &rhs);
        // Alias-only equalities between runtime unknowns are resolved by alias
        // propagation. Keep direct evaluation for parameter/constant sourced
        // assignments (e.g. y = k) so discrete partitions materialize values.
        if lhs_runtime_unknown && rhs_runtime_unknown {
            return false;
        }
    }
    let new_value = eval_override(eq, target, solution, rhs_env, implicit_clock_active)
        .unwrap_or_else(|| {
            eval_discrete_assignment_value_in_env(
                dae,
                target,
                solution,
                rhs_env,
                implicit_clock_active,
            )
        });
    if std::env::var_os("RUMOCA_DEBUG_DIGITAL_START").is_some()
        && matches!(
            target,
            "a.y" | "b.y" | "Adder.a" | "Adder.b" | "Enable.y" | "FF.j" | "FF.k" | "MUX.d"
        )
    {
        eprintln!(
            "DEBUG discrete target={target} origin={} old={} new={} implicit_clock_active={implicit_clock_active} rhs_time={} env_time={}",
            eq.origin,
            env.vars.get(target).copied().unwrap_or(0.0),
            new_value,
            rhs_env.get("time"),
            env.get("time"),
        );
    }
    if std::env::var_os("RUMOCA_DEBUG_COUNTER_ENABLE").is_some() && target == "Enable.y" {
        eprintln!("DEBUG discrete target={target} value={new_value}");
    }
    let old_value = env.vars.get(target).copied().unwrap_or(0.0);
    on_scalar_eval(target, old_value, new_value);
    set_target_value(env, target, new_value)
}

type TupleFunctionAssignment<'a> = crate::runtime::tuple::TupleFunctionAssignment<'a>;
type DiscreteSourceMap<'a> = HashMap<String, &'a dae::Expression>;
type DiscreteTargetStats = HashMap<String, crate::runtime::assignment::DirectAssignmentTargetStats>;

fn should_skip_alias_discrete_target(
    dae: &dae::Dae,
    target: &str,
    solution: &dae::Expression,
    target_stats: &DiscreteTargetStats,
) -> bool {
    let stats = target_stats.get(target).copied().unwrap_or_default();
    // MLS §8 equation semantics: simple equality equations are symmetric.
    // When a discrete target has one real defining RHS plus plain alias
    // equalities, the runtime event settle must keep the defining RHS and let
    // alias propagation update the peer variables, not overwrite the target
    // from a stale alias value.
    crate::runtime::assignment::assignment_solution_is_alias_varref(dae, solution)
        && stats.total > 1
        && stats.non_alias == 1
}

fn discrete_tuple_function_assignment_from_equation<'a>(
    eq: &'a dae::Equation,
    env: &VarEnv<f64>,
) -> Option<TupleFunctionAssignment<'a>> {
    crate::runtime::tuple::discrete_tuple_function_assignment_from_equation_with_guard_env(
        eq,
        env,
        crate::runtime::alias::is_zero_literal,
    )
}

fn build_discrete_source_map_and_active_solutions<'a>(
    dae: &'a dae::Dae,
    branch_env: &'a VarEnv<f64>,
) -> (DiscreteSourceMap<'a>, Vec<&'a dae::Expression>) {
    let mut sources = HashMap::new();
    let mut candidate_solutions = Vec::new();
    for eq in dae.f_z.iter().chain(dae.f_m.iter()) {
        if let Some((lhs, solution)) =
            crate::runtime::assignment::discrete_assignment_from_equation_with_guard_env(
                eq, branch_env,
            )
        {
            sources.entry(lhs.to_string()).or_insert(solution);
            candidate_solutions.push(solution);
            continue;
        }
        if let Some(tuple_assignment) =
            discrete_tuple_function_assignment_from_equation(eq, branch_env)
        {
            candidate_solutions.push(tuple_assignment.solution);
        }
    }
    let active_solutions = candidate_solutions
        .into_iter()
        .filter(|solution| {
            crate::runtime::clock::expression_may_trigger_clock_event_from_sources(
                solution, &sources,
            )
        })
        .collect();
    (sources, active_solutions)
}

fn ordered_discrete_partition_equations(dae: &dae::Dae) -> Vec<&dae::Equation> {
    let equations: Vec<&dae::Equation> = dae.f_z.iter().chain(dae.f_m.iter()).collect();
    if equations.len() <= 1 {
        return equations;
    }

    let ordered_targets = crate::runtime::assignment::ordered_discrete_assignment_targets(dae);
    if ordered_targets.is_empty() {
        return equations;
    }

    let order_map: HashMap<&str, usize> = ordered_targets
        .iter()
        .enumerate()
        .map(|(idx, target)| (target.as_str(), idx))
        .collect();

    let mut ranked_equations: Vec<(usize, usize, &dae::Equation)> = equations
        .into_iter()
        .enumerate()
        .map(|(idx, eq)| {
            let rank = crate::runtime::assignment::direct_assignment_from_equation(eq)
                .and_then(|(target, _)| order_map.get(target.as_str()).copied())
                .unwrap_or(usize::MAX);
            (rank, idx, eq)
        })
        .collect();
    ranked_equations.sort_by_key(|(rank, idx, _)| (*rank, *idx));
    ranked_equations.into_iter().map(|(_, _, eq)| eq).collect()
}

fn discrete_clock_event_active(dae: &dae::Dae, env: &VarEnv<f64>) -> bool {
    if !crate::runtime::clock::dae_may_have_discrete_clock_activity(dae) {
        return false;
    }
    if let Some(active) = crate::runtime::clock::static_periodic_clock_event_active(dae, env) {
        return active;
    }
    let (sources, active_solutions) = build_discrete_source_map_and_active_solutions(dae, env);
    crate::runtime::clock::discrete_clock_event_active_from_sources(
        dae,
        env,
        &sources,
        &active_solutions,
        is_clock_function_name,
        eval_clock_edge_assignment,
        eval_sample_clock_active,
    )
}

fn set_discrete_target_value(
    env: &mut VarEnv<f64>,
    explicit_updates: &mut HashSet<String>,
    target: &str,
    new_value: f64,
) -> bool {
    let old_value = env.vars.get(target).copied().unwrap_or(0.0);
    if (old_value - new_value).abs() <= 1.0e-12 {
        return false;
    }
    env.set(target, new_value);
    crate::runtime::alias::insert_name_and_base(explicit_updates, target);
    true
}

fn solution_uses_post_event_observation(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::BuiltinCall {
            function:
                dae::BuiltinFunction::Pre
                | dae::BuiltinFunction::Sample
                | dae::BuiltinFunction::Edge
                | dae::BuiltinFunction::Change
                | dae::BuiltinFunction::Initial,
            ..
        } => true,
        dae::Expression::BuiltinCall { args, .. } => {
            args.iter().any(solution_uses_post_event_observation)
        }
        dae::Expression::FunctionCall { name, args, .. } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            matches!(
                short,
                "Clock"
                    | "previous"
                    | "hold"
                    | "subSample"
                    | "superSample"
                    | "shiftSample"
                    | "backSample"
                    | "firstTick"
                    | "noClock"
            ) || args.iter().any(solution_uses_post_event_observation)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            solution_uses_post_event_observation(lhs) || solution_uses_post_event_observation(rhs)
        }
        dae::Expression::Unary { rhs, .. } => solution_uses_post_event_observation(rhs),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                solution_uses_post_event_observation(cond)
                    || solution_uses_post_event_observation(value)
            }) || solution_uses_post_event_observation(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(solution_uses_post_event_observation)
        }
        dae::Expression::Range { start, step, end } => {
            solution_uses_post_event_observation(start)
                || step
                    .as_deref()
                    .is_some_and(solution_uses_post_event_observation)
                || solution_uses_post_event_observation(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            solution_uses_post_event_observation(expr)
                || indices
                    .iter()
                    .any(|index| solution_uses_post_event_observation(&index.range))
                || filter
                    .as_deref()
                    .is_some_and(solution_uses_post_event_observation)
        }
        dae::Expression::Index { base, subscripts } => {
            solution_uses_post_event_observation(base)
                || subscripts.iter().any(|subscript| match subscript {
                    dae::Subscript::Expr(expr) => solution_uses_post_event_observation(expr),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => solution_uses_post_event_observation(base),
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

fn refresh_post_event_observation_values_at_time_inner(
    dae: &dae::Dae,
    env: &mut VarEnv<f64>,
    t_eval: f64,
    excluded_targets: &HashSet<String>,
) -> bool {
    if dae.f_z.is_empty() && dae.f_m.is_empty() {
        return false;
    }

    let mut rhs_env = env.clone();
    rhs_env.set("time", t_eval);
    rhs_env.is_initial = env.is_initial;
    let target_stats =
        crate::runtime::assignment::collect_discrete_assignment_target_stats(dae, true);

    let mut explicit_updates = HashSet::new();
    let mut changed_any = false;
    for eq in dae.f_z.iter().chain(dae.f_m.iter()) {
        let Some((target, solution)) =
            crate::runtime::assignment::discrete_assignment_from_equation_with_guard_env(
                eq, &rhs_env,
            )
        else {
            continue;
        };
        if excluded_targets.contains(target.as_str()) {
            continue;
        }
        if should_skip_alias_discrete_target(dae, target.as_str(), solution, &target_stats) {
            continue;
        }
        if prefers_guard_env_for_discrete_expr(solution, env) {
            continue;
        }
        if !solution_uses_post_event_observation(solution) {
            continue;
        }
        let new_value =
            eval_discrete_assignment_value_in_env(dae, target.as_str(), solution, &rhs_env, false);
        changed_any |=
            set_discrete_target_value(env, &mut explicit_updates, target.as_str(), new_value);
    }

    if changed_any {
        let _ = crate::runtime::alias::propagate_discrete_alias_equalities(
            dae,
            env,
            &mut explicit_updates,
            |_| {},
        );
    }

    if std::env::var_os("RUMOCA_DEBUG_COUNTER_ENABLE").is_some()
        && env.vars.contains_key("Counter.FF[1].And1.y")
    {
        eprintln!(
            "DEBUG post_event t={t_eval:.6} changed={changed_any} And1.y={} And1.aux_n={} FF1.q={} RS1.TD1.y={} RS2.TD1.y={}",
            env.get("Counter.FF[1].And1.y"),
            env.get("Counter.FF[1].And1.auxiliary_n"),
            env.get("Counter.FF[1].q"),
            env.get("Counter.FF[1].RS1.TD1.y"),
            env.get("Counter.FF[1].RS2.TD1.y"),
        );
    }

    changed_any
}

mod updates;

#[cfg(test)]
pub(crate) use updates::refresh_post_event_observation_values_at_time;
pub(crate) use updates::refresh_post_event_observation_values_excluding_at_time;
pub use updates::{
    apply_discrete_partition_updates,
    apply_discrete_partition_updates_with_guard_env_and_scalar_override,
    apply_discrete_partition_updates_with_scalar_override,
};

#[cfg(test)]
mod tests;
