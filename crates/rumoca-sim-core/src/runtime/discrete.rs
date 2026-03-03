use crate::runtime::assignment::{canonical_var_ref_key, evaluate_direct_assignment_values};
use rumoca_eval_runtime::eval::{VarEnv, eval_expr};
use rumoca_eval_runtime::sim_float::SimFloat;
use rumoca_ir_dae as dae;
use std::collections::{HashMap, HashSet};

fn clamp_finite(v: f64) -> f64 {
    if v.is_finite() { v } else { 0.0 }
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
    let mut left_limit_env = env.clone();
    if let Some(pre) = rumoca_eval_runtime::eval::get_pre_value(target) {
        left_limit_env.set(target, clamp_finite(pre));
    }
    let target_base = state_base_name(target);
    if let Some(pre) = rumoca_eval_runtime::eval::get_pre_value(&target_base) {
        left_limit_env.set(&target_base, clamp_finite(pre));
    }
    clamp_finite(eval_expr::<f64>(solution, &left_limit_env))
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
    if args.len() >= 2 {
        return Some(clamp_finite(eval_expr::<f64>(solution, env)));
    }
    let trigger = args.first()?;
    if let dae::Expression::VarRef {
        name: trigger_name,
        subscripts,
    } = trigger
        && subscripts.is_empty()
    {
        let key = dae::VarName::new(trigger_name.as_str());
        if dae.parameters.contains_key(&key) || dae.constants.contains_key(&key) {
            return Some(clamp_finite(eval_expr::<f64>(solution, env)));
        }
    }
    if !expr_has_var_refs(trigger) {
        return Some(clamp_finite(eval_expr::<f64>(solution, env)));
    }

    let current = eval_expr::<f64>(trigger, env).to_bool();
    let previous = match trigger {
        dae::Expression::VarRef { name, subscripts } => canonical_var_ref_key(name, subscripts)
            .and_then(|key| rumoca_eval_runtime::eval::get_pre_value(&key))
            .is_some_and(|value| value != 0.0),
        _ => false,
    };
    Some(if current && !previous { 1.0 } else { 0.0 })
}

fn sampled_target_held_value(target: &str, value_expr: &dae::Expression, env: &VarEnv<f64>) -> f64 {
    let held = rumoca_eval_runtime::eval::get_pre_value(target)
        .or_else(|| env.vars.get(target).copied())
        .unwrap_or_else(|| clamp_finite(eval_expr::<f64>(value_expr, env)));
    clamp_finite(held)
}

fn eval_left_limit_value(expr: &dae::Expression, env: &VarEnv<f64>) -> f64 {
    if let dae::Expression::VarRef { name, subscripts } = expr {
        if name.as_str() == "time" && subscripts.is_empty() {
            return clamp_finite(env.get("time"));
        }
        if let Some(key) = canonical_var_ref_key(name, subscripts)
            && let Some(pre) = rumoca_eval_runtime::eval::get_pre_value(&key)
        {
            return clamp_finite(pre);
        }
    }
    clamp_finite(eval_expr::<f64>(expr, env))
}

pub fn eval_sample_clock_active(
    dae: &dae::Dae,
    clock_expr: &dae::Expression,
    env: &VarEnv<f64>,
) -> bool {
    eval_clock_edge_assignment(dae, clock_expr, env)
        .map(|value| value != 0.0)
        .unwrap_or_else(|| eval_expr::<f64>(clock_expr, env).to_bool())
}

pub fn is_clock_function_name(short: &str) -> bool {
    matches!(
        short,
        "Clock" | "subSample" | "superSample" | "shiftSample" | "backSample" | "firstTick"
    )
}

fn eval_clocked_sample_assignment(
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

    let clock_active = eval_sample_clock_active(dae, clock_expr, env);
    if clock_active {
        return Some(eval_left_limit_value(value_expr, env));
    }

    Some(sampled_target_held_value(target, value_expr, env))
}

fn eval_implicit_sample_assignment(
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
        Some(eval_left_limit_value(value_expr, env))
    } else {
        Some(sampled_target_held_value(target, value_expr, env))
    }
}

fn eval_hold_assignment(
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
    if implicit_clock_active {
        Some(clamp_finite(eval_expr::<f64>(value_expr, env)))
    } else {
        Some(sampled_target_held_value(target, value_expr, env))
    }
}

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

pub fn eval_discrete_assignment_value(
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
        eval_implicit_sample_assignment(target, solution, env, implicit_clock_active)
    {
        return value;
    }
    if !implicit_clock_active && expr_uses_previous(solution) {
        return sampled_target_held_value(target, solution, env);
    }
    if let Some(value) = eval_hold_assignment(target, solution, env, implicit_clock_active) {
        return value;
    }
    if let Some(value) = eval_clock_edge_assignment(dae, solution, env) {
        return value;
    }
    if is_state_target(dae, target) && expression_references_target(solution, target) {
        return eval_state_assignment_with_left_limit_target(target, solution, env);
    }
    clamp_finite(eval_expr::<f64>(solution, env))
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

pub struct ScalarDiscreteEquationInput<'a> {
    pub dae: &'a dae::Dae,
    pub eq: &'a dae::Equation,
    pub target: &'a str,
    pub solution: &'a dae::Expression,
    pub env: &'a mut VarEnv<f64>,
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
    let implicit_clock_active = input.implicit_clock_active;

    if !target.contains('[')
        && let Some(dims) = env.dims.get(target).cloned()
        && let Some(size) = dims_total_size(&dims)
        && size > 1
    {
        let mut values = evaluate_direct_assignment_values(solution, env, size);
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
        if !implicit_clock_active && expr_uses_previous(solution) {
            for (index, value) in values.iter_mut().enumerate() {
                let key = format!("{target}[{}]", index + 1);
                let base_fallback = base_target_hold_fallback(env, target, index);
                *value = rumoca_eval_runtime::eval::get_pre_value(&key)
                    .or_else(|| env.vars.get(&key).copied())
                    .or(base_fallback)
                    .unwrap_or(*value);
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
        rumoca_eval_runtime::eval::set_array_entries(env, target, &dims, &values);
        return changed_any;
    }

    if crate::runtime::assignment::extract_alias_pair_from_equation(dae, eq).is_some() {
        return false;
    }
    let new_value =
        eval_override(eq, target, solution, env, implicit_clock_active).unwrap_or_else(|| {
            eval_discrete_assignment_value(dae, target, solution, env, implicit_clock_active)
        });
    let old_value = env.vars.get(target).copied().unwrap_or(0.0);
    on_scalar_eval(target, old_value, new_value);
    set_target_value(env, target, new_value)
}

type TupleFunctionAssignment<'a> = crate::runtime::tuple::TupleFunctionAssignment<'a>;
type DiscreteSourceMap<'a> = HashMap<String, &'a dae::Expression>;

fn discrete_tuple_function_assignment_from_equation<'a>(
    eq: &'a dae::Equation,
    env: &VarEnv<f64>,
) -> Option<TupleFunctionAssignment<'a>> {
    crate::runtime::tuple::discrete_tuple_function_assignment_from_equation(
        eq,
        env,
        |condition, env| eval_expr::<f64>(condition, env).to_bool(),
        crate::runtime::alias::is_zero_literal,
    )
}

fn build_discrete_source_map_and_active_solutions<'a>(
    dae: &'a dae::Dae,
    env: &'a VarEnv<f64>,
) -> (DiscreteSourceMap<'a>, Vec<&'a dae::Expression>) {
    let mut sources = HashMap::new();
    let mut active_solutions = Vec::new();
    for eq in dae.f_z.iter().chain(dae.f_m.iter()) {
        if let Some((lhs, solution)) =
            crate::runtime::assignment::discrete_assignment_from_equation(eq, env)
        {
            sources.entry(lhs.to_string()).or_insert(solution);
            active_solutions.push(solution);
            continue;
        }
        if let Some(tuple_assignment) = discrete_tuple_function_assignment_from_equation(eq, env) {
            active_solutions.push(tuple_assignment.solution);
        }
    }
    (sources, active_solutions)
}

fn discrete_clock_event_active(dae: &dae::Dae, env: &VarEnv<f64>) -> bool {
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

fn apply_scalar_discrete_partition_equation_from_eq(
    dae: &dae::Dae,
    eq: &dae::Equation,
    env: &mut VarEnv<f64>,
    explicit_updates: &mut HashSet<String>,
    implicit_clock_active: bool,
    eval_override: &mut impl FnMut(
        &dae::Equation,
        &str,
        &dae::Expression,
        &VarEnv<f64>,
        bool,
    ) -> Option<f64>,
) -> bool {
    let Some((target, solution)) =
        crate::runtime::assignment::discrete_assignment_from_equation(eq, env)
    else {
        return false;
    };
    apply_scalar_discrete_partition_equation_with_override(
        ScalarDiscreteEquationInput {
            dae,
            eq,
            target: target.as_str(),
            solution,
            env,
            implicit_clock_active,
        },
        &mut |env, target, new_value| {
            set_discrete_target_value(env, explicit_updates, target, new_value)
        },
        &mut |_target, _old_value, _new_value| {},
        eval_override,
    )
}

fn apply_tuple_discrete_partition_equation_from_eq(
    eq: &dae::Equation,
    env: &mut VarEnv<f64>,
    explicit_updates: &mut HashSet<String>,
    implicit_clock_active: bool,
) -> bool {
    let Some(tuple_assignment) = discrete_tuple_function_assignment_from_equation(eq, env) else {
        return false;
    };
    crate::runtime::tuple::apply_discrete_tuple_function_assignment(
        &tuple_assignment,
        env,
        implicit_clock_active,
        expr_uses_previous,
        |env, target, new_value| {
            set_discrete_target_value(env, explicit_updates, target, new_value)
        },
        |_name| {},
    )
}

pub fn apply_discrete_partition_updates_with_scalar_override(
    dae: &dae::Dae,
    env: &mut VarEnv<f64>,
    mut eval_override: impl FnMut(
        &dae::Equation,
        &str,
        &dae::Expression,
        &VarEnv<f64>,
        bool,
    ) -> Option<f64>,
) -> bool {
    if dae.f_z.is_empty() && dae.f_m.is_empty() {
        return false;
    }

    let mut changed_any = false;
    let max_passes = (dae.f_z.len() + dae.f_m.len()).clamp(1, 16);
    for _ in 0..max_passes {
        let mut changed_pass = false;
        let mut explicit_updates: HashSet<String> = HashSet::new();
        let implicit_clock_active = discrete_clock_event_active(dae, env);
        env.set(
            rumoca_eval_runtime::eval::IMPLICIT_CLOCK_ACTIVE_ENV_KEY,
            if implicit_clock_active { 1.0 } else { 0.0 },
        );

        for eq in dae.f_z.iter().chain(dae.f_m.iter()) {
            let changed_eq = apply_scalar_discrete_partition_equation_from_eq(
                dae,
                eq,
                env,
                &mut explicit_updates,
                implicit_clock_active,
                &mut eval_override,
            );
            if changed_eq {
                changed_pass = true;
                changed_any = true;
                continue;
            }

            let changed_tuple = apply_tuple_discrete_partition_equation_from_eq(
                eq,
                env,
                &mut explicit_updates,
                implicit_clock_active,
            );
            if !changed_tuple {
                continue;
            }
            let _ = crate::runtime::alias::propagate_discrete_alias_equalities(
                dae,
                env,
                &mut explicit_updates,
                |_| {},
            );
            changed_pass = true;
            changed_any = true;
        }

        if crate::runtime::alias::propagate_discrete_alias_equalities(
            dae,
            env,
            &mut explicit_updates,
            |_| {},
        ) {
            changed_pass = true;
            changed_any = true;
        }
        if !changed_pass {
            break;
        }
    }

    changed_any
}

pub fn apply_discrete_partition_updates(dae: &dae::Dae, env: &mut VarEnv<f64>) -> bool {
    apply_discrete_partition_updates_with_scalar_override(
        dae,
        env,
        |_eq, _target, _solution, _env, _implicit_clock_active| None,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn var(name: &str) -> dae::Expression {
        dae::Expression::VarRef {
            name: dae::VarName::new(name),
            subscripts: vec![],
        }
    }

    #[test]
    fn implicit_sample_active_uses_left_limit_value() {
        let dae_model = dae::Dae::default();
        let mut env = VarEnv::<f64>::new();
        env.set("x", 5.0);
        rumoca_eval_runtime::eval::clear_pre_values();
        rumoca_eval_runtime::eval::set_pre_value("x", 4.0);

        let solution = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            args: vec![var("x")],
        };
        let value = eval_discrete_assignment_value(&dae_model, "y", &solution, &env, true);
        assert!((value - 4.0).abs() <= 1.0e-12);

        rumoca_eval_runtime::eval::clear_pre_values();
    }

    #[test]
    fn previous_expression_inactive_clock_holds_target_left_limit() {
        let dae_model = dae::Dae::default();
        let mut env = VarEnv::<f64>::new();
        env.set("x", 2.0);
        env.set("y", 1.0);
        rumoca_eval_runtime::eval::clear_pre_values();
        rumoca_eval_runtime::eval::set_pre_value("y", 7.0);

        let solution = dae::Expression::FunctionCall {
            name: dae::VarName::new("previous"),
            args: vec![var("x")],
            is_constructor: false,
        };
        let value = eval_discrete_assignment_value(&dae_model, "y", &solution, &env, false);
        assert!((value - 7.0).abs() <= 1.0e-12);

        rumoca_eval_runtime::eval::clear_pre_values();
    }

    #[test]
    fn state_target_assignment_uses_left_limit_state_value() {
        let mut dae_model = dae::Dae::default();
        dae_model.states.insert(
            dae::VarName::new("x"),
            dae::Variable::new(dae::VarName::new("x")),
        );
        let mut env = VarEnv::<f64>::new();
        env.set("x", 2.0);
        rumoca_eval_runtime::eval::clear_pre_values();
        rumoca_eval_runtime::eval::set_pre_value("x", -5.0);

        let solution = dae::Expression::Binary {
            op: dae::OpBinary::Mul(Default::default()),
            lhs: Box::new(dae::Expression::Literal(dae::Literal::Real(-0.8))),
            rhs: Box::new(var("x")),
        };
        let value = eval_discrete_assignment_value(&dae_model, "x", &solution, &env, true);
        assert!((value - 4.0).abs() <= 1.0e-12);

        rumoca_eval_runtime::eval::clear_pre_values();
    }

    #[test]
    fn discrete_partition_updates_allows_scalar_override_callback() {
        let mut dae_model = dae::Dae::default();
        dae_model.discrete_reals.insert(
            dae::VarName::new("z"),
            dae::Variable::new(dae::VarName::new("z")),
        );
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("z"),
            dae::Expression::Literal(dae::Literal::Real(1.0)),
            rumoca_core::Span::DUMMY,
            "z assignment",
        ));

        let mut env = VarEnv::<f64>::new();
        env.set("z", 0.0);
        let changed = apply_discrete_partition_updates_with_scalar_override(
            &dae_model,
            &mut env,
            |_eq, _target, _solution, _env, _implicit_clock_active| Some(3.0),
        );
        assert!(changed);
        assert!((env.vars.get("z").copied().unwrap_or(0.0) - 3.0).abs() <= 1e-12);
    }
}
