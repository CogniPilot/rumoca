use super::*;

#[cfg(test)]
pub(crate) fn refresh_post_event_observation_values_at_time(
    dae: &dae::Dae,
    env: &mut VarEnv<f64>,
    t_eval: f64,
) -> bool {
    refresh_post_event_observation_values_at_time_inner(dae, env, t_eval, &HashSet::new())
}

pub(crate) fn refresh_post_event_observation_values_excluding_at_time(
    dae: &dae::Dae,
    env: &mut VarEnv<f64>,
    t_eval: f64,
    excluded_targets: &[String],
) -> bool {
    let excluded_targets = excluded_targets
        .iter()
        .cloned()
        .collect::<HashSet<String>>();
    refresh_post_event_observation_values_at_time_inner(dae, env, t_eval, &excluded_targets)
}

struct ScalarDiscretePartitionEval<'a, F> {
    dae: &'a dae::Dae,
    env: &'a mut VarEnv<f64>,
    guard_env: &'a VarEnv<f64>,
    target_stats: &'a DiscreteTargetStats,
    explicit_updates: &'a mut HashSet<String>,
    implicit_clock_active: bool,
    eval_override: &'a mut F,
}

fn apply_scalar_discrete_partition_equation_from_eq<F>(
    eq: &dae::Equation,
    ctx: &mut ScalarDiscretePartitionEval<'_, F>,
) -> bool
where
    F: FnMut(&dae::Equation, &str, &dae::Expression, &VarEnv<f64>, bool) -> Option<f64>,
{
    let Some((target, solution)) =
        // MLS §8.3.5 / §8.6: within an event iteration, later when/if branch
        // selection must observe discrete updates computed earlier in the same
        // settle round, while `pre(...)` remains anchored to the event
        // left-limit via the runtime pre-store.
        crate::runtime::assignment::discrete_assignment_from_equation_with_guard_env(eq, ctx.env)
    else {
        return false;
    };
    let selected_solution = if matches!(solution, dae::Expression::If { .. }) {
        // MLS §8.6 / Appendix B: event iteration evaluates the active branch
        // at the current event instant. A scalar `if change(x) then ... else
        // pre(y)` must therefore select the current branch before the generic
        // `pre(...)` hold logic decides whether to retain the previous value.
        select_discrete_scalar_if_branch(ctx.dae, solution, ctx.env, ctx.guard_env)
            .unwrap_or(solution)
    } else {
        solution
    };
    if should_skip_alias_discrete_target(
        ctx.dae,
        target.as_str(),
        selected_solution,
        ctx.target_stats,
    ) {
        return false;
    }
    let rhs_env = prefers_guard_env_for_discrete_expr(selected_solution, ctx.guard_env)
        .then_some(ctx.guard_env);
    apply_scalar_discrete_partition_equation_with_override(
        ScalarDiscreteEquationInput {
            dae: ctx.dae,
            eq,
            target: target.as_str(),
            solution: selected_solution,
            env: ctx.env,
            rhs_env,
            implicit_clock_active: ctx.implicit_clock_active,
        },
        &mut |env, target, new_value| {
            set_discrete_target_value(env, ctx.explicit_updates, target, new_value)
        },
        &mut |_target, _old_value, _new_value| {},
        ctx.eval_override,
    )
}

fn apply_tuple_discrete_partition_equation_from_eq(
    eq: &dae::Equation,
    env: &mut VarEnv<f64>,
    guard_env: &VarEnv<f64>,
    explicit_updates: &mut HashSet<String>,
    implicit_clock_active: bool,
) -> bool {
    let Some(tuple_assignment) = discrete_tuple_function_assignment_from_equation(eq, env) else {
        return false;
    };
    crate::runtime::tuple::apply_discrete_tuple_function_assignment(
        &tuple_assignment,
        env,
        guard_env,
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
    eval_override: impl FnMut(&dae::Equation, &str, &dae::Expression, &VarEnv<f64>, bool) -> Option<f64>,
) -> bool {
    let guard_env = env.clone();
    apply_discrete_partition_updates_with_guard_env_and_scalar_override(
        dae,
        env,
        &guard_env,
        eval_override,
    )
}

pub fn apply_discrete_partition_updates_with_guard_env_and_scalar_override(
    dae: &dae::Dae,
    env: &mut VarEnv<f64>,
    guard_env: &VarEnv<f64>,
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
    let target_stats =
        crate::runtime::assignment::collect_discrete_assignment_target_stats(dae, true);
    // MLS §8.6 / Appendix B: event-discrete iteration must converge to a
    // fixed point. Use a data-dependent cap so long dependency chains are not
    // truncated after an arbitrary small number of passes.
    let max_passes = (dae.f_z.len() + dae.f_m.len() + dae.f_c.len()).max(8);
    for _ in 0..max_passes {
        let mut changed_pass = false;
        let mut explicit_updates: HashSet<String> = HashSet::new();
        let implicit_clock_active = discrete_clock_event_active(dae, env);
        env.set(
            rumoca_eval_dae::runtime::IMPLICIT_CLOCK_ACTIVE_ENV_KEY,
            if implicit_clock_active { 1.0 } else { 0.0 },
        );

        for eq in ordered_discrete_partition_equations(dae) {
            let mut scalar_eval = ScalarDiscretePartitionEval {
                dae,
                env,
                guard_env,
                target_stats: &target_stats,
                explicit_updates: &mut explicit_updates,
                implicit_clock_active,
                eval_override: &mut eval_override,
            };
            let changed_eq = apply_scalar_discrete_partition_equation_from_eq(eq, &mut scalar_eval);
            if changed_eq {
                changed_pass = true;
                changed_any = true;
                continue;
            }

            let changed_tuple = apply_tuple_discrete_partition_equation_from_eq(
                eq,
                env,
                guard_env,
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
