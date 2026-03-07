use diffsol::{OdeEquations, OdeSolverMethod, VectorHost};

use super::{
    event_restart_step_hint, integration_direction, is_interpolation_outside_step_sim_error,
    time_match_with_tol,
};
use crate::TimeoutBudget;
use crate::with_diffsol::{
    Dae, SimError, SimOptions, SolverStartupProfile, interp_err, map_solver_panic, problem,
    sim_trace_enabled,
};
pub(crate) fn solver_state_to_vec<'a, Eqn, S>(solver: &S) -> Vec<f64>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    solver.state().y.as_slice().to_vec()
}

pub(crate) fn solver_interpolate_to_vec<'a, Eqn, S>(
    solver: &S,
    t_sample: f64,
    budget: &TimeoutBudget,
    context: &str,
) -> Result<Vec<f64>, SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        solver.interpolate(t_sample)
    })) {
        Ok(Ok(state)) => Ok(state.as_slice().to_vec()),
        Ok(Err(interp)) => Err(interp_err(t_sample, interp)),
        Err(panic_info) => Err(map_solver_panic(budget, context, panic_info)),
    }
}

pub(crate) fn interpolate_output_state<'a, Eqn, S>(
    solver: &S,
    t_interp: f64,
    budget: &TimeoutBudget,
) -> Result<Vec<f64>, SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    match solver_interpolate_to_vec::<Eqn, S>(solver, t_interp, budget, "interpolate") {
        Ok(y) => Ok(y),
        Err(err) => {
            let current_t = solver.state().t;
            if is_interpolation_outside_step_sim_error(&err)
                && time_match_with_tol(t_interp, current_t)
            {
                if sim_trace_enabled() {
                    eprintln!(
                        "[sim-trace] output interpolation clamp: t_interp={} current_t={}",
                        t_interp, current_t
                    );
                }
                Ok(solver_state_to_vec::<Eqn, S>(solver))
            } else {
                Err(err)
            }
        }
    }
}

pub(crate) struct SolverStateOverwriteInput<'a> {
    pub(crate) opts: &'a SimOptions,
    pub(crate) startup_profile: SolverStartupProfile,
    pub(crate) dae: &'a Dae,
    pub(crate) param_values: &'a [f64],
    pub(crate) n_x: usize,
    pub(crate) t: f64,
    pub(crate) y: &'a [f64],
}

pub(crate) fn overwrite_solver_state<'a, Eqn, S>(
    solver: &mut S,
    input: SolverStateOverwriteInput<'_>,
) -> Result<(), SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    let state_len = solver.state().y.as_slice().len();
    if input.y.len() != state_len {
        return Err(SimError::SolverError(format!(
            "state overwrite size mismatch: expected {state_len}, got {}",
            input.y.len()
        )));
    }

    let mut rhs = vec![0.0; state_len];
    problem::eval_rhs_equations(
        input.dae,
        input.y,
        input.param_values,
        input.t,
        &mut rhs,
        input.n_x,
    );

    let restart_h = event_restart_step_hint(input.opts, input.t, input.startup_profile);
    let state = solver.state_mut();
    *state.t = input.t;
    state.y.as_mut_slice().copy_from_slice(input.y);
    let dy = state.dy.as_mut_slice();
    if dy.len() == rhs.len() {
        dy.copy_from_slice(&rhs);
    } else {
        dy.fill(0.0);
        let copy_len = dy.len().min(rhs.len());
        dy[..copy_len].copy_from_slice(&rhs[..copy_len]);
    }
    state.dg.as_mut_slice().fill(0.0);
    for ds in state.ds.iter_mut() {
        ds.as_mut_slice().fill(0.0);
    }
    for dsg in state.dsg.iter_mut() {
        dsg.as_mut_slice().fill(0.0);
    }
    if let Some(target_h_abs) = restart_h {
        let direction = integration_direction(input.opts);
        let new_h = direction * target_h_abs;
        if sim_trace_enabled() {
            eprintln!(
                "[sim-trace] event restart step-size reset: t={} h_old={} h_new={}",
                input.t, *state.h, new_h
            );
        }
        *state.h = new_h;
    }
    Ok(())
}
