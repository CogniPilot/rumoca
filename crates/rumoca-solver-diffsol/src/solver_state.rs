use crate::{RuntimeParameters, SimError};
use diffsol::{OdeEquations, OdeSolverMethod, OdeSolverState, VectorHost};

/// Write `(t, y, params)` from the Rust-side event state into the persistent BDF solver.
/// Calling this marks the solver state as mutated so that diffsol reinitialises BDF on
/// the next `step()` call. For param-only events where `y` did not change diffsol's
/// internal detection avoids unnecessary BDF order resets when possible.
///
/// When `dy` is provided it is written to `state.dy` so that `initialise_diff_to_first_order`
/// uses the post-event derivative rather than the stale pre-event one left over from
/// `state_mut_back`.
pub(crate) fn write_state_to_solver<'a, Eqn, S>(
    solver: &mut S,
    runtime_params: &RuntimeParameters,
    y: &[f64],
    dy: Option<&[f64]>,
    params: &[f64],
    t: f64,
) where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    runtime_params.borrow_mut().copy_from_slice(params);
    let state = solver.state_mut();
    state.y.as_mut_slice().copy_from_slice(y);
    if let Some(dy) = dy {
        state.dy.as_mut_slice().copy_from_slice(dy);
    }
    *state.t = t;
}

pub(crate) fn reset_solver_state<'a, Eqn, S>(
    solver: &mut S,
    runtime_params: &RuntimeParameters,
    y: &[f64],
    dy: &[f64],
    params: &[f64],
    t: f64,
    h_cap: f64,
) -> Result<(), SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    runtime_params.borrow_mut().copy_from_slice(params);
    let problem = solver.problem();
    let mut fresh_state = S::State::new_without_initialise(problem)
        .map_err(|err| SimError::SolverError(format!("solver state reset: {err}")))?;
    {
        let state = fresh_state.as_mut();
        state.y.as_mut_slice().copy_from_slice(y);
        state.dy.as_mut_slice().copy_from_slice(dy);
        *state.t = t;
    }
    fresh_state.set_step_size(problem.h0, &problem.atol, problem.rtol, &problem.eqn, 1);
    solver.set_state(fresh_state);
    cap_solver_step_size(solver, h_cap);
    Ok(())
}

fn cap_solver_step_size<'a, Eqn, S>(solver: &mut S, h_cap: f64)
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    let state = solver.state_mut();
    if *state.h > h_cap {
        *state.h = h_cap;
    }
}
