use diffsol::{OdeEquations, OdeSolverMethod, VectorHost};

use super::{SolverStateOverwriteInput, overwrite_solver_state};
use crate::TimeoutBudget;
use crate::with_diffsol::{
    Dae, SimError, SimOptions, SolverStartupProfile, eval, problem, sim_trace_enabled,
};

pub(crate) use crate::CompiledDiscreteEventContext;

pub(crate) struct StartupSyncInput<'a> {
    pub(crate) dae: &'a Dae,
    pub(crate) opts: &'a SimOptions,
    pub(crate) startup_profile: SolverStartupProfile,
    pub(crate) compiled_runtime: &'a problem::CompiledRuntimeNewtonContext,
    pub(crate) param_values: &'a [f64],
    pub(crate) n_x: usize,
    pub(crate) budget: &'a TimeoutBudget,
}

pub(super) struct ScheduledEventProjectionInput<'a> {
    pub(super) dae: &'a Dae,
    pub(super) y_at_stop: &'a [f64],
    pub(super) p: &'a [f64],
    pub(super) n_x: usize,
    pub(super) t_stop: f64,
    pub(super) atol: f64,
    pub(super) budget: &'a TimeoutBudget,
    pub(super) compiled_runtime: &'a problem::CompiledRuntimeNewtonContext,
    pub(super) seed_env: &'a eval::VarEnv<f64>,
}

pub(super) fn maybe_project_scheduled_event_state(
    dae: &Dae,
    y_at_stop: &[f64],
    n_x: usize,
    t_stop: f64,
    atol: f64,
    budget: &TimeoutBudget,
) -> Result<Vec<f64>, SimError> {
    if n_x == 0 || n_x >= y_at_stop.len() {
        return Ok(y_at_stop.to_vec());
    }
    match problem::project_algebraics_with_fixed_states_at_time(
        dae, y_at_stop, n_x, t_stop, atol, budget,
    )? {
        Some(projected) => {
            if sim_trace_enabled() {
                let changed = projected
                    .iter()
                    .zip(y_at_stop.iter())
                    .any(|(lhs, rhs)| (lhs - rhs).abs() > 1.0e-12);
                eprintln!(
                    "[sim-trace] runtime projection at t={} changed={}",
                    t_stop, changed
                );
            }
            Ok(projected)
        }
        None => {
            if sim_trace_enabled() {
                eprintln!(
                    "[sim-trace] runtime projection at t={} failed; continuing without projection",
                    t_stop
                );
            }
            Ok(y_at_stop.to_vec())
        }
    }
}

pub(super) fn project_scheduled_event_state_with_seed_env(
    input: ScheduledEventProjectionInput<'_>,
) -> Result<Vec<f64>, SimError> {
    let ScheduledEventProjectionInput {
        dae,
        y_at_stop,
        p,
        n_x,
        t_stop,
        atol,
        budget,
        compiled_runtime,
        seed_env,
    } = input;
    if n_x == 0 || n_x >= y_at_stop.len() {
        return Ok(y_at_stop.to_vec());
    }

    let mut projected = y_at_stop.to_vec();
    let masks = problem::build_runtime_projection_masks(dae, n_x, projected.len());
    let direct_seed_ctx = problem::build_runtime_direct_seed_context(dae, projected.len(), n_x);
    let mut direct_seed_env_cache = Some(seed_env.clone());
    let mut scratch = problem::RuntimeProjectionScratch::default();
    let converged =
        problem::project_algebraics_with_fixed_states_at_time_with_context_and_cache_in_place(
            dae,
            projected.as_mut_slice(),
            problem::RuntimeProjectionContext {
                p,
                compiled_runtime: Some(compiled_runtime),
                fixed_cols: &masks.fixed_cols,
                ignored_rows: &masks.ignored_rows,
                branch_local_analog_cols: &masks.branch_local_analog_cols,
                direct_seed_ctx: Some(&direct_seed_ctx),
                direct_seed_env_cache: Some(&mut direct_seed_env_cache),
            },
            problem::RuntimeProjectionStep {
                y_seed: y_at_stop,
                n_x,
                t_eval: t_stop,
                tol: atol.max(1.0e-8),
                timeout: budget,
            },
            None,
            &mut scratch,
        )?;
    if converged {
        if sim_trace_enabled() {
            let changed = projected
                .iter()
                .zip(y_at_stop.iter())
                .any(|(lhs, rhs)| (lhs - rhs).abs() > 1.0e-12);
            eprintln!(
                "[sim-trace] runtime projection from settled event env at t={} changed={}",
                t_stop, changed
            );
        }
        Ok(projected)
    } else {
        if sim_trace_enabled() {
            eprintln!(
                "[sim-trace] runtime projection from settled event env at t={} failed; continuing without projection",
                t_stop
            );
        }
        Ok(y_at_stop.to_vec())
    }
}

pub(crate) fn apply_initial_sections_and_sync_startup_state<'a, Eqn, S>(
    solver: &mut S,
    input: StartupSyncInput<'_>,
) -> Result<(), SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    eval::clear_pre_values();
    let mut startup_y = solver.state().y.as_slice().to_vec();
    let startup_updates = problem::apply_initial_section_assignments_strict(
        input.dae,
        startup_y.as_mut_slice(),
        input.param_values,
        input.opts.t_start,
    )?;
    let projected = maybe_project_scheduled_event_state(
        input.dae,
        startup_y.as_slice(),
        input.n_x,
        input.opts.t_start,
        input.opts.atol,
        input.budget,
    )?;
    let projection_changed = projected
        .iter()
        .zip(startup_y.iter())
        .any(|(lhs, rhs)| (lhs - rhs).abs() > 1.0e-12);
    if projection_changed {
        startup_y = projected;
    }
    if startup_updates > 0 || projection_changed {
        overwrite_solver_state::<Eqn, S>(
            solver,
            SolverStateOverwriteInput {
                dae: input.dae,
                opts: input.opts,
                startup_profile: input.startup_profile,
                compiled_runtime: input.compiled_runtime,
                param_values: input.param_values,
                n_x: input.n_x,
                t: input.opts.t_start,
                y: startup_y.as_slice(),
            },
        )?;
    }
    crate::runtime::startup::refresh_pre_values_from_state_with_initial_assignments_strict(
        input.dae,
        solver.state().y.as_slice(),
        input.param_values,
        input.opts.t_start,
    )
    .map_err(SimError::CompiledEval)?;
    Ok(())
}

pub(crate) fn build_compiled_discrete_event_context(
    dae: &Dae,
    solver_len: usize,
) -> Result<Option<CompiledDiscreteEventContext>, SimError> {
    crate::build_compiled_discrete_event_context(dae, solver_len).map_err(SimError::CompiledEval)
}

pub(crate) fn settle_runtime_event_updates(
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    n_x: usize,
    t_eval: f64,
    compiled_discrete: Option<&CompiledDiscreteEventContext>,
) -> eval::VarEnv<f64> {
    let env = crate::settle_runtime_event_updates_with_compiled_discrete(
        dae,
        y,
        p,
        n_x,
        t_eval,
        compiled_discrete,
    );
    log_event_settle_targets_if_requested(&env, t_eval);
    env
}

pub(crate) fn settle_runtime_event_updates_frozen_pre(
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    n_x: usize,
    t_eval: f64,
    compiled_discrete: Option<&CompiledDiscreteEventContext>,
) -> eval::VarEnv<f64> {
    let env = crate::runtime::compiled_discrete::settle_runtime_event_updates_frozen_pre_with_compiled_discrete(
        dae,
        y,
        p,
        n_x,
        t_eval,
        compiled_discrete,
    );
    log_event_settle_targets_if_requested(&env, t_eval);
    env
}

fn log_event_settle_targets_if_requested(env: &eval::VarEnv<f64>, t_eval: f64) {
    if std::env::var("RUMOCA_SIM_INTROSPECT").is_err() {
        return;
    }
    let Ok(raw) = std::env::var("RUMOCA_SIM_INTROSPECT_TARGET_MATCH") else {
        return;
    };
    let needles: Vec<&str> = raw
        .split(',')
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .collect();
    if needles.is_empty() {
        return;
    }
    let mut matches: Vec<_> = env
        .vars
        .iter()
        .filter(|(name, _)| needles.iter().any(|needle| name.contains(needle)))
        .collect();
    if matches.is_empty() {
        return;
    }
    matches.sort_by_key(|(lhs, _)| *lhs);
    eprintln!(
        "[sim-introspect] event-settle snapshot t={} implicit_clock_active={}",
        t_eval,
        env.get(rumoca_eval_dae::runtime::IMPLICIT_CLOCK_ACTIVE_ENV_KEY)
    );
    for (name, value) in env
        .clock_intervals
        .iter()
        .filter(|(name, _)| needles.iter().any(|needle| name.contains(needle)))
    {
        eprintln!(
            "[sim-introspect] event-settle clock-interval t={} {}={}",
            t_eval, name, value
        );
    }
    for (name, value) in matches {
        eprintln!(
            "[sim-introspect] event-settle value t={} {}={}",
            t_eval, name, value
        );
    }
}
