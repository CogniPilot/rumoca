use rumoca_eval_flat::eval::{self, build_env};
use rumoca_ir_dae as dae;

pub fn runtime_capture_target_names(dae_model: &dae::Dae) -> Vec<String> {
    crate::collect_discrete_channel_names(dae_model)
}

fn sample_clock_arg_is_explicit_clock(
    dae_model: &dae::Dae,
    clock_expr: &dae::Expression,
    env: &eval::VarEnv<f64>,
) -> bool {
    if let dae::Expression::FunctionCall { name, .. } = clock_expr {
        let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
        if matches!(
            short,
            "Clock" | "subSample" | "superSample" | "shiftSample" | "backSample" | "firstTick"
        ) {
            return true;
        }
    }
    if eval::infer_clock_timing_seconds(clock_expr, env).is_some() {
        return true;
    }
    if let dae::Expression::VarRef { name, subscripts } = clock_expr
        && subscripts.is_empty()
    {
        let key = dae::VarName::new(name.as_str());
        return dae_model.discrete_reals.contains_key(&key)
            || dae_model.discrete_valued.contains_key(&key);
    }
    false
}

fn expr_uses_implicit_sample_clock(
    dae_model: &dae::Dae,
    expr: &dae::Expression,
    env: &eval::VarEnv<f64>,
) -> bool {
    match expr {
        dae::Expression::BuiltinCall { function, args } => {
            let is_implicit_sample = if *function == dae::BuiltinFunction::Sample {
                if args.len() <= 1 {
                    true
                } else {
                    !sample_clock_arg_is_explicit_clock(dae_model, &args[1], env)
                }
            } else {
                false
            };
            is_implicit_sample
                || args
                    .iter()
                    .any(|arg| expr_uses_implicit_sample_clock(dae_model, arg, env))
        }
        dae::Expression::FunctionCall { args, .. } => args
            .iter()
            .any(|arg| expr_uses_implicit_sample_clock(dae_model, arg, env)),
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_uses_implicit_sample_clock(dae_model, lhs, env)
                || expr_uses_implicit_sample_clock(dae_model, rhs, env)
        }
        dae::Expression::Unary { rhs, .. } => expr_uses_implicit_sample_clock(dae_model, rhs, env),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expr_uses_implicit_sample_clock(dae_model, cond, env)
                    || expr_uses_implicit_sample_clock(dae_model, value, env)
            }) || expr_uses_implicit_sample_clock(dae_model, else_branch, env)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => elements
            .iter()
            .any(|item| expr_uses_implicit_sample_clock(dae_model, item, env)),
        dae::Expression::Range { start, step, end } => {
            expr_uses_implicit_sample_clock(dae_model, start, env)
                || step
                    .as_ref()
                    .is_some_and(|value| expr_uses_implicit_sample_clock(dae_model, value, env))
                || expr_uses_implicit_sample_clock(dae_model, end, env)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_uses_implicit_sample_clock(dae_model, expr, env)
                || indices
                    .iter()
                    .any(|idx| expr_uses_implicit_sample_clock(dae_model, &idx.range, env))
                || filter
                    .as_ref()
                    .is_some_and(|value| expr_uses_implicit_sample_clock(dae_model, value, env))
        }
        dae::Expression::Index { base, subscripts } => {
            expr_uses_implicit_sample_clock(dae_model, base, env)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(value) => {
                        expr_uses_implicit_sample_clock(dae_model, value, env)
                    }
                    _ => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => {
            expr_uses_implicit_sample_clock(dae_model, base, env)
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

pub fn settle_runtime_discrete_capture_env(
    dae_model: &dae::Dae,
    y: &mut [f64],
    p: &[f64],
    n_x: usize,
    t_eval: f64,
) -> eval::VarEnv<f64> {
    crate::settle_runtime_event_updates(
        crate::EventSettleInput {
            dae: dae_model,
            y,
            p,
            n_x,
            t_eval,
        },
        crate::runtime::assignment::propagate_runtime_direct_assignments_from_env,
        crate::runtime::alias::propagate_runtime_alias_components_from_env,
        |dae, env| {
            crate::runtime::discrete::apply_discrete_partition_updates_with_scalar_override(
                dae,
                env,
                |_eq, target, solution, env, implicit_clock_active| {
                    if implicit_clock_active
                        && expr_uses_implicit_sample_clock(dae_model, solution, env)
                    {
                        return Some(env.vars.get(target).copied().unwrap_or(0.0));
                    }
                    None
                },
            )
        },
        crate::runtime::layout::sync_solver_values_from_env,
    )
}

pub fn build_initialization_capture_env(
    dae_model: &dae::Dae,
    y: &mut [f64],
    p: &[f64],
    t_eval: f64,
) -> eval::VarEnv<f64> {
    build_env(dae_model, y, p, t_eval)
}
