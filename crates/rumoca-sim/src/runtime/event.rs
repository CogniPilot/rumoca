use rumoca_eval_dae::runtime::{VarEnv, build_env};
use rumoca_ir_dae as dae;

fn carry_discrete_values_from_env(dae: &dae::Dae, env: &mut VarEnv<f64>, prev: &VarEnv<f64>) {
    for (name, var) in dae.discrete_reals.iter().chain(dae.discrete_valued.iter()) {
        let base = name.as_str();
        if let Some(value) = prev.vars.get(base).copied() {
            env.set(base, value);
        }
        let size = var.size();
        if size <= 1 {
            continue;
        }
        for idx in 1..=size {
            let scalar_name = format!("{base}[{idx}]");
            if let Some(value) = prev.vars.get(scalar_name.as_str()).copied() {
                env.set(scalar_name.as_str(), value);
            }
        }
    }
}

pub fn integration_direction(t_start: f64, t_end: f64) -> f64 {
    if t_end >= t_start { 1.0 } else { -1.0 }
}

pub fn event_right_limit_time(t_start: f64, t_end: f64, t_event: f64) -> f64 {
    if !t_event.is_finite() {
        return t_event;
    }
    let delta = (1.0e-6 * (1.0 + t_event.abs())).max(f64::EPSILON * (1.0 + t_event.abs()));
    if integration_direction(t_start, t_end) >= 0.0 {
        t_event + delta
    } else {
        t_event - delta
    }
}

pub fn event_restart_time(t_start: f64, t_end: f64, t_event: f64) -> f64 {
    let t_right = event_right_limit_time(t_start, t_end, t_event);
    if integration_direction(t_start, t_end) >= 0.0 {
        t_right.min(t_end)
    } else {
        t_right.max(t_end)
    }
}

pub fn refresh_pre_values_from_state(dae_model: &dae::Dae, y: &[f64], p: &[f64], t: f64) {
    let env = build_env(dae_model, y, p, t);
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&env);
}

pub fn build_runtime_env(dae_model: &dae::Dae, y: &mut [f64], p: &[f64], t: f64) -> VarEnv<f64> {
    let env = build_env(dae_model, y, p, t);
    let _ = crate::runtime::layout::sync_solver_values_from_env(dae_model, y, &env);
    env
}

pub struct EventSettleInput<'a> {
    pub dae: &'a dae::Dae,
    pub y: &'a mut [f64],
    pub p: &'a [f64],
    pub n_x: usize,
    pub t_eval: f64,
}

pub fn settle_runtime_event_updates<PD, PA, AD, SS>(
    input: EventSettleInput<'_>,
    mut propagate_runtime_direct_assignments: PD,
    mut propagate_runtime_alias_components: PA,
    mut apply_discrete_partition_updates: AD,
    mut sync_solver_values_from_env: SS,
) -> VarEnv<f64>
where
    PD: FnMut(&dae::Dae, &mut [f64], usize, &mut VarEnv<f64>) -> usize,
    PA: FnMut(&dae::Dae, &mut [f64], usize, &mut VarEnv<f64>) -> usize,
    AD: FnMut(&dae::Dae, &mut VarEnv<f64>) -> bool,
    SS: FnMut(&dae::Dae, &mut [f64], &VarEnv<f64>) -> usize,
{
    let dae = input.dae;
    let y = input.y;
    let p = input.p;
    let n_x = input.n_x;
    let t_eval = input.t_eval;
    let mut env = build_env(dae, y, p, t_eval);

    for _ in 0..4 {
        let mut changed = false;

        changed |= propagate_runtime_direct_assignments(dae, y, n_x, &mut env) > 0;
        changed |= propagate_runtime_alias_components(dae, y, n_x, &mut env) > 0;
        changed |= apply_discrete_partition_updates(dae, &mut env);
        changed |= sync_solver_values_from_env(dae, y, &env) > 0;
        changed |= propagate_runtime_alias_components(dae, y, n_x, &mut env) > 0;

        if !changed {
            break;
        }

        let prev_env = env;
        // Keep pre(...) fixed to the event left-limit across settle passes.
        env = build_env(dae, y, p, t_eval);
        carry_discrete_values_from_env(dae, &mut env, &prev_env);
    }

    env
}

pub fn settle_runtime_event_updates_default(input: EventSettleInput<'_>) -> VarEnv<f64> {
    settle_runtime_event_updates(
        input,
        crate::runtime::assignment::propagate_runtime_direct_assignments_from_env,
        crate::runtime::alias::propagate_runtime_alias_components_from_env,
        crate::runtime::discrete::apply_discrete_partition_updates,
        crate::runtime::layout::sync_solver_values_from_env,
    )
}

pub fn settle_runtime_sample_updates_default(input: EventSettleInput<'_>) -> VarEnv<f64> {
    settle_runtime_event_updates(
        input,
        crate::runtime::assignment::propagate_runtime_direct_assignments_from_env,
        crate::runtime::alias::propagate_runtime_alias_components_from_env,
        |_dae, _env| false,
        crate::runtime::layout::sync_solver_values_from_env,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;
    use rumoca_ir_dae as dae;

    #[test]
    fn settle_runtime_event_updates_reaches_fixed_point() {
        let dae = dae::Dae::default();
        let mut y = vec![0.0];
        let p = vec![];
        let mut steps = 0usize;

        let _env = settle_runtime_event_updates(
            EventSettleInput {
                dae: &dae,
                y: &mut y,
                p: &p,
                n_x: 0,
                t_eval: 0.0,
            },
            |_dae, y, _n_x, env| {
                steps += 1;
                if steps == 1 {
                    y[0] = 2.0;
                    env.set("y0", 2.0);
                    1
                } else {
                    0
                }
            },
            |_dae, _y, _n_x, _env| 0,
            |_dae, _env| false,
            |_dae, _y, _env| 0,
        );

        assert_eq!(y[0], 2.0);
        assert!(steps >= 2);
    }

    #[test]
    fn settle_runtime_event_updates_preserves_discrete_values_across_rebuilds() {
        let mut dae = dae::Dae::default();
        dae.discrete_reals.insert(
            dae::VarName::new("d"),
            dae::Variable::new(dae::VarName::new("d")),
        );
        let mut y = vec![0.0];
        let p = vec![];
        let mut saw_discrete_on_second_pass = false;
        let mut pass = 0usize;

        let env = settle_runtime_event_updates(
            EventSettleInput {
                dae: &dae,
                y: &mut y,
                p: &p,
                n_x: 0,
                t_eval: 0.0,
            },
            |_dae, _y, _n_x, env| {
                pass += 1;
                if pass == 1 {
                    env.set("d", 3.0);
                    return 1;
                }
                let preserved = env.vars.get("d").copied().unwrap_or(0.0) == 3.0;
                if preserved {
                    saw_discrete_on_second_pass = true;
                }
                0
            },
            |_dae, _y, _n_x, _env| 0,
            |_dae, _env| false,
            |_dae, _y, _env| 0,
        );

        assert!(saw_discrete_on_second_pass);
        assert_eq!(env.vars.get("d").copied().unwrap_or(0.0), 3.0);
    }

    #[test]
    fn settle_runtime_event_updates_preserves_discrete_array_values_across_rebuilds() {
        let mut dae = dae::Dae::default();
        let mut z = dae::Variable::new(dae::VarName::new("z"));
        z.dims = vec![2];
        dae.discrete_valued.insert(dae::VarName::new("z"), z);
        let mut y = vec![0.0];
        let p = vec![];
        let mut pass = 0usize;
        let mut preserved_on_second_pass = false;

        let env = settle_runtime_event_updates(
            EventSettleInput {
                dae: &dae,
                y: &mut y,
                p: &p,
                n_x: 0,
                t_eval: 0.0,
            },
            |_dae, _y, _n_x, env| {
                pass += 1;
                if pass == 1 {
                    env.set("z[1]", 3.0);
                    env.set("z[2]", -2.0);
                    return 1;
                }
                if env.vars.get("z[1]").copied().unwrap_or(0.0) == 3.0
                    && env.vars.get("z[2]").copied().unwrap_or(0.0) == -2.0
                {
                    preserved_on_second_pass = true;
                }
                0
            },
            |_dae, _y, _n_x, _env| 0,
            |_dae, _env| false,
            |_dae, _y, _env| 0,
        );

        assert!(preserved_on_second_pass);
        assert_eq!(env.vars.get("z[1]").copied().unwrap_or(0.0), 3.0);
        assert_eq!(env.vars.get("z[2]").copied().unwrap_or(0.0), -2.0);
    }

    #[test]
    fn settle_runtime_event_updates_applies_discrete_partition_across_passes() {
        let mut dae = dae::Dae::default();
        dae.discrete_reals.insert(
            dae::VarName::new("z"),
            dae::Variable::new(dae::VarName::new("z")),
        );
        let mut y = vec![0.0];
        let p = vec![];
        let mut pass = 0usize;
        let mut discrete_calls = 0usize;

        let env = settle_runtime_event_updates(
            EventSettleInput {
                dae: &dae,
                y: &mut y,
                p: &p,
                n_x: 0,
                t_eval: 0.0,
            },
            |_dae, _y, _n_x, env| {
                // Force a second settle pass to ensure discrete equations are
                // not re-applied during convergence.
                pass += 1;
                if pass == 1 {
                    env.set("alias_seed", 1.0);
                    return 1;
                }
                0
            },
            |_dae, _y, _n_x, _env| 0,
            |_dae, env| {
                discrete_calls += 1;
                env.set("z", 42.0);
                true
            },
            |_dae, _y, _env| 0,
        );

        assert_eq!(
            discrete_calls, pass,
            "discrete partition should run on each settle pass while converging"
        );
        assert_eq!(env.vars.get("z").copied().unwrap_or(0.0), 42.0);
    }

    #[test]
    fn settle_runtime_event_updates_keeps_pre_values_fixed_across_passes() {
        let mut dae = dae::Dae::default();
        dae.discrete_valued.insert(
            dae::VarName::new("u"),
            dae::Variable::new(dae::VarName::new("u")),
        );
        dae.discrete_valued.insert(
            dae::VarName::new("y"),
            dae::Variable::new(dae::VarName::new("y")),
        );

        rumoca_eval_dae::runtime::clear_pre_values();
        rumoca_eval_dae::runtime::set_pre_value("u", 0.0);

        let mut y = vec![];
        let p = vec![];
        let mut pass = 0usize;

        let env = settle_runtime_event_updates(
            EventSettleInput {
                dae: &dae,
                y: &mut y,
                p: &p,
                n_x: 0,
                t_eval: 0.0,
            },
            |_dae, _y, _n_x, env| {
                pass += 1;
                let old = env.vars.get("u").copied().unwrap_or(0.0);
                env.set("u", 1.0);
                if (old - 1.0).abs() > 1.0e-12 { 1 } else { 0 }
            },
            |_dae, _y, _n_x, _env| 0,
            |_dae, env| {
                let pre_u = rumoca_eval_dae::runtime::get_pre_value("u").unwrap_or(0.0);
                let old = env.vars.get("y").copied().unwrap_or(0.0);
                env.set("y", pre_u);
                (old - pre_u).abs() > 1.0e-12
            },
            |_dae, _y, _env| 0,
        );

        assert!(pass >= 2, "event settle should iterate to a fixed point");
        assert!(
            (env.vars.get("y").copied().unwrap_or(0.0) - 0.0).abs() <= 1.0e-12,
            "y should observe pre(u) from event left-limit"
        );
    }

    #[test]
    fn event_restart_time_advances_right_limit_and_clamps_forward() {
        let t_restart = event_restart_time(0.0, 10.0, 2.0);
        assert!(t_restart > 2.0);
        assert!(t_restart <= 10.0);
    }

    #[test]
    fn event_right_limit_time_uses_meaningful_forward_stride() {
        let t_event = 0.5;
        let t_right = event_right_limit_time(0.0, 1.0, t_event);
        assert!(t_right > t_event);
        assert!((t_right - t_event) >= 1.0e-6);
    }

    #[test]
    fn event_restart_time_clamps_at_forward_horizon_end() {
        let t_restart = event_restart_time(0.0, 10.0, 10.0);
        assert_eq!(t_restart, 10.0);
    }

    #[test]
    fn event_restart_time_advances_right_limit_and_clamps_backward() {
        let t_restart = event_restart_time(10.0, 0.0, 8.0);
        assert!(t_restart < 8.0);
        assert!(t_restart >= 0.0);
    }

    #[test]
    fn settle_runtime_event_updates_applies_state_reset_from_discrete_partition() {
        let mut dae_model = dae::Dae::default();
        dae_model.states.insert(
            dae::VarName::new("x"),
            dae::Variable::new(dae::VarName::new("x")),
        );
        dae_model.f_z.push(dae::Equation {
            lhs: Some(dae::VarName::new("x")),
            rhs: dae::Expression::Literal(dae::Literal::Real(7.5)),
            span: Span::DUMMY,
            origin: "reinit(x, 7.5) lowered".to_string(),
            scalar_count: 1,
        });

        let mut y = vec![1.0];
        let p = Vec::new();
        let env = settle_runtime_event_updates_default(EventSettleInput {
            dae: &dae_model,
            y: &mut y,
            p: &p,
            n_x: 1,
            t_eval: 2.0,
        });

        assert!((y[0] - 7.5).abs() <= 1.0e-12);
        assert!((env.vars.get("x").copied().unwrap_or(f64::NAN) - 7.5).abs() <= 1.0e-12);
    }
}
