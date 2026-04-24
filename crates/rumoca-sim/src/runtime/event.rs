use rumoca_eval_dae::runtime::{
    VarEnv, build_runtime_parameter_tail_env, map_var_to_env,
    refresh_env_solver_and_parameter_values,
};
use rumoca_ir_dae as dae;

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

pub fn build_runtime_state_env(dae_model: &dae::Dae, y: &[f64], p: &[f64], t: f64) -> VarEnv<f64> {
    let mut env = build_runtime_parameter_tail_env(dae_model, p, t);
    let mut idx = 0usize;
    for (name, var) in dae_model
        .states
        .iter()
        .chain(dae_model.algebraics.iter())
        .chain(dae_model.outputs.iter())
    {
        map_var_to_env(&mut env, name.as_str(), var, y, &mut idx);
    }
    env
}

pub fn refresh_pre_values_from_state(dae_model: &dae::Dae, y: &[f64], p: &[f64], t: f64) {
    let env = build_runtime_state_env(dae_model, y, p, t);
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&env);
}

pub fn build_runtime_env(dae_model: &dae::Dae, y: &mut [f64], p: &[f64], t: f64) -> VarEnv<f64> {
    build_runtime_state_env(dae_model, y, p, t)
}

pub struct EventSettleInput<'a> {
    pub dae: &'a dae::Dae,
    pub y: &'a mut [f64],
    pub p: &'a [f64],
    pub n_x: usize,
    pub t_eval: f64,
    pub is_initial: bool,
}

fn event_settle_iteration_budget(dae: &dae::Dae) -> usize {
    // MLS §8.6 / Appendix B (SPEC_0022 SIM-001): event iteration must continue
    // until the discrete equations reach a fixed point. Use a generous
    // data-dependent cap to avoid silently truncating longer discrete chains.
    let work_items = dae.f_m.len()
        + dae.f_z.len()
        + dae.f_c.len()
        + dae.triggered_clock_conditions.len()
        + dae.synthetic_root_conditions.len();
    work_items.max(8)
}

pub(crate) fn apply_runtime_pre_discrete_phase<PD, PA>(
    dae: &dae::Dae,
    y: &mut [f64],
    n_x: usize,
    env: &mut VarEnv<f64>,
    propagate_runtime_direct_assignments: &mut PD,
    propagate_runtime_alias_components: &mut PA,
) -> bool
where
    PD: FnMut(&dae::Dae, &mut [f64], usize, &mut VarEnv<f64>) -> usize,
    PA: FnMut(&dae::Dae, &mut [f64], usize, &mut VarEnv<f64>) -> usize,
{
    let mut changed = false;
    changed |=
        crate::runtime::assignment::propagate_runtime_derivative_aliases_from_env(dae, n_x, env)
            > 0;
    changed |= propagate_runtime_direct_assignments(dae, y, n_x, env) > 0;
    changed |= propagate_runtime_alias_components(dae, y, n_x, env) > 0;
    changed
}

pub(crate) fn apply_runtime_post_discrete_phase<PA, SS>(
    dae: &dae::Dae,
    y: &mut [f64],
    n_x: usize,
    env: &mut VarEnv<f64>,
    propagate_runtime_alias_components: &mut PA,
    sync_solver_values_from_env: &mut SS,
) -> bool
where
    PA: FnMut(&dae::Dae, &mut [f64], usize, &mut VarEnv<f64>) -> usize,
    SS: FnMut(&dae::Dae, &mut [f64], &VarEnv<f64>) -> usize,
{
    let mut changed = false;
    changed |= sync_solver_values_from_env(dae, y, env) > 0;
    changed |= propagate_runtime_alias_components(dae, y, n_x, env) > 0;
    changed
}

pub fn settle_runtime_event_updates_with_base_env<PD, PA, AD, SS>(
    input: EventSettleInput<'_>,
    mut env: VarEnv<f64>,
    mut propagate_runtime_direct_assignments: PD,
    mut propagate_runtime_alias_components: PA,
    mut apply_discrete_partition_updates: AD,
    mut sync_solver_values_from_env: SS,
    advance_pre_between_passes: bool,
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
    env.is_initial = input.is_initial;

    for _ in 0..event_settle_iteration_budget(dae) {
        let mut changed = false;

        changed |= apply_runtime_pre_discrete_phase(
            dae,
            y,
            n_x,
            &mut env,
            &mut propagate_runtime_direct_assignments,
            &mut propagate_runtime_alias_components,
        );
        changed |= apply_discrete_partition_updates(dae, &mut env);
        changed |= apply_runtime_post_discrete_phase(
            dae,
            y,
            n_x,
            &mut env,
            &mut propagate_runtime_alias_components,
            &mut sync_solver_values_from_env,
        );

        if !changed {
            break;
        }

        if advance_pre_between_passes {
            // MLS Appendix B / SPEC_0022 SIM-001: ordinary event iteration
            // advances pre(z) and pre(m) to the previous event-iteration
            // result before the next solve pass.
            rumoca_eval_dae::runtime::seed_pre_values_from_env(&env);
        }
        refresh_env_solver_and_parameter_values(&mut env, dae, y, p, t_eval);
        env.is_initial = input.is_initial;
    }

    env
}

fn settle_runtime_event_updates_inner<PD, PA, AD, SS>(
    input: EventSettleInput<'_>,
    propagate_runtime_direct_assignments: PD,
    propagate_runtime_alias_components: PA,
    apply_discrete_partition_updates: AD,
    sync_solver_values_from_env: SS,
    advance_pre_between_passes: bool,
) -> VarEnv<f64>
where
    PD: FnMut(&dae::Dae, &mut [f64], usize, &mut VarEnv<f64>) -> usize,
    PA: FnMut(&dae::Dae, &mut [f64], usize, &mut VarEnv<f64>) -> usize,
    AD: FnMut(&dae::Dae, &mut VarEnv<f64>) -> bool,
    SS: FnMut(&dae::Dae, &mut [f64], &VarEnv<f64>) -> usize,
{
    let env = build_runtime_state_env(input.dae, input.y, input.p, input.t_eval);
    settle_runtime_event_updates_with_base_env(
        input,
        env,
        propagate_runtime_direct_assignments,
        propagate_runtime_alias_components,
        apply_discrete_partition_updates,
        sync_solver_values_from_env,
        advance_pre_between_passes,
    )
}

pub fn settle_runtime_event_updates<PD, PA, AD, SS>(
    input: EventSettleInput<'_>,
    propagate_runtime_direct_assignments: PD,
    propagate_runtime_alias_components: PA,
    apply_discrete_partition_updates: AD,
    sync_solver_values_from_env: SS,
) -> VarEnv<f64>
where
    PD: FnMut(&dae::Dae, &mut [f64], usize, &mut VarEnv<f64>) -> usize,
    PA: FnMut(&dae::Dae, &mut [f64], usize, &mut VarEnv<f64>) -> usize,
    AD: FnMut(&dae::Dae, &mut VarEnv<f64>) -> bool,
    SS: FnMut(&dae::Dae, &mut [f64], &VarEnv<f64>) -> usize,
{
    settle_runtime_event_updates_inner(
        input,
        propagate_runtime_direct_assignments,
        propagate_runtime_alias_components,
        apply_discrete_partition_updates,
        sync_solver_values_from_env,
        true,
    )
}

pub fn settle_runtime_event_updates_frozen_pre<PD, PA, AD, SS>(
    input: EventSettleInput<'_>,
    propagate_runtime_direct_assignments: PD,
    propagate_runtime_alias_components: PA,
    apply_discrete_partition_updates: AD,
    sync_solver_values_from_env: SS,
) -> VarEnv<f64>
where
    PD: FnMut(&dae::Dae, &mut [f64], usize, &mut VarEnv<f64>) -> usize,
    PA: FnMut(&dae::Dae, &mut [f64], usize, &mut VarEnv<f64>) -> usize,
    AD: FnMut(&dae::Dae, &mut VarEnv<f64>) -> bool,
    SS: FnMut(&dae::Dae, &mut [f64], &VarEnv<f64>) -> usize,
{
    // MLS §16.5.1 / §16.4: on a clock tick, previous()/hold() stay anchored to
    // the event-entry left limit for the full synchronous settle round. Do not
    // advance the runtime pre-store between passes.
    settle_runtime_event_updates_inner(
        input,
        propagate_runtime_direct_assignments,
        propagate_runtime_alias_components,
        apply_discrete_partition_updates,
        sync_solver_values_from_env,
        false,
    )
}

pub(crate) fn settle_runtime_event_updates_frozen_pre_from_env<PD, PA, AD, SS>(
    input: EventSettleInput<'_>,
    env: VarEnv<f64>,
    propagate_runtime_direct_assignments: PD,
    propagate_runtime_alias_components: PA,
    apply_discrete_partition_updates: AD,
    sync_solver_values_from_env: SS,
) -> VarEnv<f64>
where
    PD: FnMut(&dae::Dae, &mut [f64], usize, &mut VarEnv<f64>) -> usize,
    PA: FnMut(&dae::Dae, &mut [f64], usize, &mut VarEnv<f64>) -> usize,
    AD: FnMut(&dae::Dae, &mut VarEnv<f64>) -> bool,
    SS: FnMut(&dae::Dae, &mut [f64], &VarEnv<f64>) -> usize,
{
    settle_runtime_event_updates_with_base_env(
        input,
        env,
        propagate_runtime_direct_assignments,
        propagate_runtime_alias_components,
        apply_discrete_partition_updates,
        sync_solver_values_from_env,
        false,
    )
}

pub fn settle_runtime_event_updates_default(input: EventSettleInput<'_>) -> VarEnv<f64> {
    let mut guard_env: Option<VarEnv<f64>> = None;
    settle_runtime_event_updates(
        input,
        crate::runtime::assignment::propagate_runtime_direct_assignments_from_env,
        crate::runtime::alias::propagate_runtime_alias_components_from_env,
        |dae, env| {
            let guard_env = guard_env.get_or_insert_with(|| env.clone());
            crate::runtime::discrete::apply_discrete_partition_updates_with_guard_env_and_scalar_override(
                dae,
                env,
                guard_env,
                |_eq, _target, _solution, _env, _implicit_clock_active| None,
            )
        },
        crate::runtime::layout::sync_solver_values_from_env,
    )
}

pub fn settle_runtime_event_updates_default_frozen_pre(input: EventSettleInput<'_>) -> VarEnv<f64> {
    let mut guard_env: Option<VarEnv<f64>> = None;
    settle_runtime_event_updates_frozen_pre(
        input,
        crate::runtime::assignment::propagate_runtime_direct_assignments_from_env,
        crate::runtime::alias::propagate_runtime_alias_components_from_env,
        |dae, env| {
            let guard_env = guard_env.get_or_insert_with(|| env.clone());
            crate::runtime::discrete::apply_discrete_partition_updates_with_guard_env_and_scalar_override(
                dae,
                env,
                guard_env,
                |_eq, _target, _solution, _env, _implicit_clock_active| None,
            )
        },
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

    fn test_var(name: &str) -> dae::Expression {
        dae::Expression::VarRef {
            name: dae::VarName::new(name),
            subscripts: vec![],
        }
    }

    fn test_der(name: &str) -> dae::Expression {
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![test_var(name)],
        }
    }

    fn build_sampled_derivative_helper_chain_dae() -> dae::Dae {
        let mut dae = dae::Dae::default();
        for name in ["load.phi", "load.w", "speed.flange.phi"] {
            dae.states.insert(
                dae::VarName::new(name),
                dae::Variable::new(dae::VarName::new(name)),
            );
        }
        dae.outputs.insert(
            dae::VarName::new("speed.w"),
            dae::Variable::new(dae::VarName::new("speed.w")),
        );
        dae.algebraics.insert(
            dae::VarName::new("sample1.u"),
            dae::Variable::new(dae::VarName::new("sample1.u")),
        );
        for name in ["periodicClock.y", "sample1.clock", "sample1.y"] {
            dae.discrete_reals.insert(
                dae::VarName::new(name),
                dae::Variable::new(dae::VarName::new(name)),
            );
        }

        dae.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(test_var("load.w")),
                rhs: Box::new(test_der("load.phi")),
            },
            Span::DUMMY,
            "load.w = der(load.phi)",
        ));
        dae.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(test_var("speed.flange.phi")),
                rhs: Box::new(test_var("load.phi")),
            },
            Span::DUMMY,
            "speed.flange.phi = load.phi",
        ));
        dae.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(test_var("speed.w")),
                rhs: Box::new(test_der("speed.flange.phi")),
            },
            Span::DUMMY,
            "speed.w = der(speed.flange.phi)",
        ));
        dae.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(test_var("speed.w")),
                rhs: Box::new(test_var("sample1.u")),
            },
            Span::DUMMY,
            "speed.w = sample1.u",
        ));
        dae.f_z.push(dae::Equation::explicit(
            dae::VarName::new("periodicClock.y"),
            dae::Expression::FunctionCall {
                name: dae::VarName::new("Clock"),
                args: vec![dae::Expression::Literal(dae::Literal::Real(0.1))],
                is_constructor: false,
            },
            Span::DUMMY,
            "periodicClock.y = Clock(0.1)",
        ));
        dae.f_z.push(dae::Equation::explicit(
            dae::VarName::new("sample1.clock"),
            test_var("periodicClock.y"),
            Span::DUMMY,
            "sample1.clock = periodicClock.y",
        ));
        dae.f_m.push(dae::Equation::explicit(
            dae::VarName::new("sample1.y"),
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Sample,
                args: vec![test_var("sample1.u"), test_var("sample1.clock")],
            },
            Span::DUMMY,
            "sample1.y = sample(sample1.u, sample1.clock)",
        ));

        dae
    }

    fn sampled_derivative_pre_env() -> VarEnv<f64> {
        let mut pre_env = VarEnv::<f64>::new();
        pre_env.set("time", 0.09);
        pre_env.set("load.phi", 0.0);
        pre_env.set("load.w", 0.05);
        pre_env.set("speed.flange.phi", 0.0);
        pre_env.set("speed.w", 0.05);
        pre_env.set("sample1.u", 0.05);
        pre_env.set("periodicClock.y", 0.0);
        pre_env.set("sample1.clock", 0.0);
        pre_env.set("sample1.y", 0.05);
        pre_env
    }

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
                is_initial: false,
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
    fn settle_runtime_event_updates_preserves_discrete_values_across_passes() {
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
                is_initial: false,
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
    fn settle_runtime_event_updates_preserves_discrete_array_values_across_passes() {
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
                is_initial: false,
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
                is_initial: false,
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
    fn settle_runtime_event_updates_advances_pre_values_across_passes() {
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
                is_initial: false,
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
            (env.vars.get("y").copied().unwrap_or(0.0) - 1.0).abs() <= 1.0e-12,
            "y should observe pre(u) from the previous event-iteration pass"
        );
    }

    #[test]
    fn settle_runtime_event_updates_frozen_pre_keeps_clocked_previous_at_left_limit() {
        let dae = dae::Dae::default();
        let mut y = vec![];
        let p = vec![];

        rumoca_eval_dae::runtime::clear_pre_values();
        rumoca_eval_dae::runtime::set_pre_value("u", 0.0);

        let env = settle_runtime_event_updates_frozen_pre(
            EventSettleInput {
                dae: &dae,
                y: &mut y,
                p: &p,
                n_x: 0,
                t_eval: 0.0,
                is_initial: false,
            },
            |_dae, _y, _n_x, _env| 0,
            |_dae, _y, _n_x, _env| 0,
            |_dae, env| {
                let previous_u = rumoca_eval_dae::runtime::get_pre_value("u").unwrap_or(0.0);
                let old_y = env.vars.get("y").copied().unwrap_or(0.0);
                env.set("y", previous_u);
                let old_u = env.vars.get("u").copied().unwrap_or(0.0);
                env.set("u", env.get("y") + 1.0);
                (old_y - previous_u).abs() > 1.0e-12 || (old_u - env.get("u")).abs() > 1.0e-12
            },
            |_dae, _y, _env| 0,
        );

        assert!(
            (env.vars.get("y").copied().unwrap_or(f64::NAN) - 0.0).abs() <= 1.0e-12,
            "clocked previous() must stay on the event-entry left limit during settle"
        );
        assert!(
            (env.vars.get("u").copied().unwrap_or(f64::NAN) - 1.0).abs() <= 1.0e-12,
            "current-tick updates may depend on previous(u), but may not feed back into it"
        );
        rumoca_eval_dae::runtime::clear_pre_values();
    }

    #[test]
    fn build_runtime_env_preserves_runtime_tail_values_without_plain_env_rebuild() {
        let mut dae_model = dae::Dae::default();
        dae_model.states.insert(
            dae::VarName::new("x"),
            dae::Variable::new(dae::VarName::new("x")),
        );
        let mut p = dae::Variable::new(dae::VarName::new("p"));
        p.start = Some(dae::Expression::Literal(dae::Literal::Real(2.5)));
        dae_model.parameters.insert(dae::VarName::new("p"), p);
        let mut u = dae::Variable::new(dae::VarName::new("u"));
        u.start = Some(dae::Expression::Literal(dae::Literal::Real(3.5)));
        dae_model.inputs.insert(dae::VarName::new("u"), u);
        let mut d = dae::Variable::new(dae::VarName::new("d"));
        d.start = Some(dae::Expression::Literal(dae::Literal::Real(4.5)));
        dae_model.discrete_reals.insert(dae::VarName::new("d"), d);

        let mut y = vec![1.25];
        let env = build_runtime_env(&dae_model, &mut y, &[2.5], 0.75);

        assert_eq!(env.vars.get("x").copied(), Some(1.25));
        assert_eq!(env.vars.get("p").copied(), Some(2.5));
        assert_eq!(env.vars.get("u").copied(), Some(3.5));
        assert_eq!(env.vars.get("d").copied(), Some(4.5));
        assert_eq!(env.vars.get("time").copied(), Some(0.75));
    }

    #[test]
    fn refresh_pre_values_from_state_seeds_runtime_tail_values() {
        let mut dae_model = dae::Dae::default();
        dae_model.states.insert(
            dae::VarName::new("x"),
            dae::Variable::new(dae::VarName::new("x")),
        );
        let mut u = dae::Variable::new(dae::VarName::new("u"));
        u.start = Some(dae::Expression::Literal(dae::Literal::Real(6.0)));
        dae_model.inputs.insert(dae::VarName::new("u"), u);
        let mut d = dae::Variable::new(dae::VarName::new("d"));
        d.start = Some(dae::Expression::Literal(dae::Literal::Real(7.0)));
        dae_model.discrete_reals.insert(dae::VarName::new("d"), d);

        rumoca_eval_dae::runtime::clear_pre_values();
        refresh_pre_values_from_state(&dae_model, &[1.5], &[], 0.25);

        assert_eq!(rumoca_eval_dae::runtime::get_pre_value("x"), Some(1.5));
        assert_eq!(rumoca_eval_dae::runtime::get_pre_value("u"), Some(6.0));
        assert_eq!(rumoca_eval_dae::runtime::get_pre_value("d"), Some(7.0));
        assert_eq!(rumoca_eval_dae::runtime::get_pre_value("time"), Some(0.25));
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
            is_initial: false,
        });

        assert!((y[0] - 7.5).abs() <= 1.0e-12);
        assert!((env.vars.get("x").copied().unwrap_or(f64::NAN) - 7.5).abs() <= 1.0e-12);
    }

    #[test]
    fn settle_runtime_event_updates_refreshes_solver_values_across_passes() {
        let mut dae = dae::Dae::default();
        dae.outputs.insert(
            dae::VarName::new("y_out"),
            dae::Variable::new(dae::VarName::new("y_out")),
        );
        let mut y = vec![0.0];
        let p = vec![];
        let mut pass = 0usize;
        let mut saw_refreshed_solver_value = false;

        let env = settle_runtime_event_updates(
            EventSettleInput {
                dae: &dae,
                y: &mut y,
                p: &p,
                n_x: 0,
                t_eval: 0.0,
                is_initial: false,
            },
            |_dae, y, _n_x, env| {
                pass += 1;
                if pass == 1 {
                    y[0] = 2.0;
                    env.set("d", 7.0);
                    return 1;
                }
                if env.vars.get("y_out").copied().unwrap_or(0.0) == 2.0 {
                    saw_refreshed_solver_value = true;
                }
                0
            },
            |_dae, _y, _n_x, _env| 0,
            |_dae, _env| false,
            |_dae, _y, _env| 0,
        );

        assert!(saw_refreshed_solver_value);
        assert_eq!(env.vars.get("y_out").copied().unwrap_or(0.0), 2.0);
        assert_eq!(env.vars.get("d").copied().unwrap_or(0.0), 7.0);
    }

    #[test]
    fn settle_runtime_event_updates_handles_dependency_chains_longer_than_four_passes() {
        let dae = dae::Dae::default();
        let mut y = vec![0.0];
        let p = vec![];
        let mut pass = 0usize;

        let env = settle_runtime_event_updates(
            EventSettleInput {
                dae: &dae,
                y: &mut y,
                p: &p,
                n_x: 0,
                t_eval: 0.0,
                is_initial: false,
            },
            |_dae, _y, _n_x, env| {
                let key = format!("d{pass}");
                pass += 1;
                if pass <= 6 {
                    env.set(&key, pass as f64);
                    1
                } else {
                    0
                }
            },
            |_dae, _y, _n_x, _env| 0,
            |_dae, _env| false,
            |_dae, _y, _env| 0,
        );

        assert!(pass >= 7, "event settle should not stop at four passes");
        assert_eq!(env.vars.get("d5").copied(), Some(6.0));
    }

    #[test]
    fn settle_runtime_event_updates_refreshes_sampled_derivative_helper_chain() {
        let dae = build_sampled_derivative_helper_chain_dae();
        rumoca_eval_dae::runtime::clear_pre_values();
        rumoca_eval_dae::runtime::seed_pre_values_from_env(&sampled_derivative_pre_env());

        let mut y = vec![0.0, 0.11, 0.0, 0.0, 0.0];
        let env = settle_runtime_event_updates_frozen_pre(
            EventSettleInput {
                dae: &dae,
                y: &mut y,
                p: &[],
                n_x: 1,
                t_eval: 0.1,
                is_initial: false,
            },
            crate::runtime::assignment::propagate_runtime_direct_assignments_from_env,
            crate::runtime::alias::propagate_runtime_alias_components_from_env,
            crate::runtime::discrete::apply_discrete_partition_updates,
            crate::runtime::layout::sync_solver_values_from_env,
        );

        assert!((env.get("der(load.phi)") - 0.11).abs() <= 1.0e-12);
        assert!((env.get("der(speed.flange.phi)") - 0.11).abs() <= 1.0e-12);
        assert!((env.get("speed.w") - 0.11).abs() <= 1.0e-12);
        assert!((env.get("sample1.u") - 0.11).abs() <= 1.0e-12);
        assert!(
            (env.get("sample1.y") - 0.11).abs() <= 1.0e-12,
            "MLS §16.5.1: event-time sample() must observe the settled derivative-backed continuous source chain; der(load.phi)={} der(speed.flange.phi)={} speed.w={} sample1.u={} sample1.y={}",
            env.get("der(load.phi)"),
            env.get("der(speed.flange.phi)"),
            env.get("speed.w"),
            env.get("sample1.u"),
            env.get("sample1.y"),
        );

        rumoca_eval_dae::runtime::clear_pre_values();
    }
}
