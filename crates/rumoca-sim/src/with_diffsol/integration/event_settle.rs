use diffsol::{OdeEquations, OdeSolverMethod, VectorHost};

use super::{SolverStateOverwriteInput, overwrite_solver_state};
use crate::TimeoutBudget;
use crate::with_diffsol::{
    Dae, SimError, SimOptions, SolverStartupProfile, eval, problem, sim_trace_enabled,
};

pub(crate) use crate::CompiledDiscreteEventContext;

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

pub(super) fn apply_initial_sections_and_sync_startup_state<'a, Eqn, S>(
    solver: &mut S,
    dae: &Dae,
    opts: &SimOptions,
    startup_profile: SolverStartupProfile,
    param_values: &[f64],
    n_x: usize,
    budget: &TimeoutBudget,
) -> Result<(), SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    eval::clear_pre_values();
    let mut startup_y = solver.state().y.as_slice().to_vec();
    let startup_updates = problem::apply_initial_section_assignments(
        dae,
        startup_y.as_mut_slice(),
        param_values,
        opts.t_start,
    );
    let projected = maybe_project_scheduled_event_state(
        dae,
        startup_y.as_slice(),
        n_x,
        opts.t_start,
        opts.atol,
        budget,
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
                opts,
                startup_profile,
                dae,
                param_values,
                n_x,
                t: opts.t_start,
                y: startup_y.as_slice(),
            },
        )?;
    }
    crate::refresh_pre_values_from_state_with_initial_assignments(
        dae,
        solver.state().y.as_slice(),
        param_values,
        opts.t_start,
    );
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
    crate::settle_runtime_event_updates_with_compiled_discrete(
        dae,
        y,
        p,
        n_x,
        t_eval,
        compiled_discrete,
    )
}
