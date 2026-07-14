use std::{any::Any, collections::BTreeSet, fmt::Display, sync::Mutex};

use diffsol::{
    BdfState, MatrixCommon, OdeEquations, OdeEquationsImplicit, OdeSolverMethod, OdeSolverProblem,
    OdeSolverState, RkState, VectorHost,
};
use rumoca_eval_solve as solve_eval;
use rumoca_ir_solve as solve;
use rumoca_solver::PreparedMassMatrix;

use crate::{LinearSolver, Matrix, OdeModel, RuntimeParameters, Scalar, SimError, Vector};

static SOLVER_PANIC_HOOK_LOCK: Mutex<()> = Mutex::new(());

pub(crate) fn solver_call<T, E, F>(context: &str, f: F) -> Result<T, SimError>
where
    E: Display,
    F: FnOnce() -> Result<T, E>,
{
    match catch_solver_panic(f) {
        Ok(Ok(value)) => Ok(value),
        Ok(Err(error)) => Err(SimError::SolverError(format!("{context}: {error}"))),
        Err(message) => Err(SimError::SolverError(format!(
            "{context} panicked: {message}"
        ))),
    }
}

pub(crate) fn catch_solver_panic<T, F>(f: F) -> Result<T, String>
where
    F: FnOnce() -> T,
{
    let _guard = SOLVER_PANIC_HOOK_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let previous_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(|info| {
        if let Some(location) = info.location() {
            PANIC_LOCATION.with(|slot| {
                *slot.borrow_mut() = Some(format!(
                    "{}:{}:{}",
                    location.file(),
                    location.line(),
                    location.column()
                ));
            });
        }
    }));
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(f));
    std::panic::set_hook(previous_hook);
    result.map_err(panic_message)
}

thread_local! {
    static PANIC_LOCATION: std::cell::RefCell<Option<String>> =
        const { std::cell::RefCell::new(None) };
}

fn panic_message(panic_info: Box<dyn Any + Send>) -> String {
    let location = PANIC_LOCATION.with(|slot| slot.borrow_mut().take());
    let base = if let Some(message) = panic_info.downcast_ref::<&str>() {
        (*message).to_string()
    } else if let Some(message) = panic_info.downcast_ref::<String>() {
        message.clone()
    } else {
        "unknown panic".to_string()
    };
    match location {
        Some(loc) => format!("{base} (at {loc})"),
        None => base,
    }
}

pub(crate) fn initial_bdf_state<Eqn>(
    model: &solve::SolveModel,
    ode_model: &OdeModel,
    problem: &OdeSolverProblem<Eqn>,
    y: &[f64],
    p: &[f64],
) -> Result<BdfState<Vector>, SimError>
where
    Eqn: OdeEquationsImplicit<M = Matrix, V = Vector, T = Scalar, C = <Matrix as MatrixCommon>::C>,
{
    if initial_algebraic_residual_is_consistent(model, ode_model, y, p, problem.t0)? {
        return projected_initial_bdf_state(model, ode_model, problem, y, p);
    }
    let bdf_state = catch_solver_panic(|| problem.bdf_state::<LinearSolver>());
    match bdf_state {
        Ok(Ok(state)) => Ok(state),
        Ok(Err(_)) | Err(_)
            if initial_algebraic_residual_is_consistent(model, ode_model, y, p, problem.t0)? =>
        {
            projected_initial_bdf_state(model, ode_model, problem, y, p)
        }
        Err(_) => Err(SimError::SolverError(format!(
            "BDF init panicked; {}",
            initial_residual_summary(model, ode_model, y, p, problem.t0).unwrap_or_else(
                |summary_err| format!("initial residual summary unavailable: {summary_err}")
            )
        ))),
        Ok(Err(err)) => Err(SimError::SolverError(format!(
            "BDF init: {err}; {}",
            initial_residual_summary(model, ode_model, y, p, problem.t0).unwrap_or_else(
                |summary_err| format!("initial residual summary unavailable: {summary_err}")
            )
        ))),
    }
}

fn initial_residual_summary(
    model: &solve::SolveModel,
    ode_model: &OdeModel,
    y: &[f64],
    p: &[f64],
    t: f64,
) -> Result<String, SimError> {
    let mut rhs = vec![0.0; y.len()];
    ode_model.eval_residual(y, p, t, &mut rhs)?;
    let state_count = model.state_scalar_count();
    let all = residual_summary_entry(ode_model, &rhs, 0);
    let algebraic =
        residual_summary_entry(ode_model, &rhs[state_count.min(rhs.len())..], state_count);
    Ok(format!(
        "initial residuals: all={}, algebraic={}, nonfinite={}",
        all,
        algebraic,
        rhs.iter().filter(|value| !value.is_finite()).count()
    ))
}

fn initial_algebraic_residual_is_consistent(
    model: &solve::SolveModel,
    ode_model: &OdeModel,
    y: &[f64],
    p: &[f64],
    t: f64,
) -> Result<bool, SimError> {
    let mut rhs = vec![0.0; y.len()];
    ode_model.eval_residual(y, p, t, &mut rhs)?;
    if rhs.iter().any(|value| !value.is_finite()) {
        return Ok(false);
    }
    let state_count = model.state_scalar_count().min(rhs.len());
    Ok(max_abs(&rhs[state_count..]) <= 1.0e-8)
}

fn max_abs(values: &[f64]) -> f64 {
    values.iter().copied().map(f64::abs).fold(0.0, f64::max)
}

fn projected_initial_bdf_state<Eqn>(
    model: &solve::SolveModel,
    ode_model: &OdeModel,
    problem: &OdeSolverProblem<Eqn>,
    y: &[f64],
    p: &[f64],
) -> Result<BdfState<Vector>, SimError>
where
    Eqn: OdeEquationsImplicit<M = Matrix, V = Vector, T = Scalar, C = <Matrix as MatrixCommon>::C>,
{
    let dy = bdf_derivative_guess(model, ode_model, y, p, problem.t0)?;
    let mut state = BdfState::<Vector>::new_without_initialise(problem)
        .map_err(|err| SimError::SolverError(format!("BDF projected init: {err}")))?;
    {
        let state_ref = state.as_mut();
        state_ref.y.as_mut_slice().copy_from_slice(y);
        state_ref.dy.as_mut_slice().copy_from_slice(&dy);
    }
    state.set_step_size(problem.h0, &problem.atol, problem.rtol, &problem.eqn, 1);
    Ok(state)
}

/// Build the initial `RkState` for the SDIRK (ESDIRK34 / TR-BDF2) path.
///
/// Mirrors [`projected_initial_bdf_state`]: it seeds the solver with the
/// already-settled, algebraically-consistent `y` and the mass-matrix
/// derivative guess `dy` (the same `bdf_derivative_guess` used by BDF, which
/// is solver-agnostic). This preserves the exact-AD projection-aware
/// consistent initial condition for the implicit RK tableaus too — without it
/// SDIRK would lose the very startup robustness the BDF path gained.
///
/// Callers pass a `y` that has been settled by the simulate path's
/// equilibrium/initialisation pass, so it is already algebraically consistent
/// at `problem.t0`.
pub(crate) fn initial_rk_state<Eqn>(
    model: &solve::SolveModel,
    ode_model: &OdeModel,
    problem: &OdeSolverProblem<Eqn>,
    y: &[f64],
    p: &[f64],
) -> Result<RkState<Vector>, SimError>
where
    Eqn: OdeEquationsImplicit<M = Matrix, V = Vector, T = Scalar, C = <Matrix as MatrixCommon>::C>,
{
    let dy = bdf_derivative_guess(model, ode_model, y, p, problem.t0)?;
    let mut state = RkState::<Vector>::new_without_initialise(problem)
        .map_err(|err| SimError::SolverError(format!("SDIRK projected init: {err}")))?;
    {
        let state_ref = state.as_mut();
        state_ref.y.as_mut_slice().copy_from_slice(y);
        state_ref.dy.as_mut_slice().copy_from_slice(&dy);
    }
    state.set_step_size(problem.h0, &problem.atol, problem.rtol, &problem.eqn, 1);
    Ok(state)
}

pub(crate) fn bdf_derivative_guess(
    model: &solve::SolveModel,
    ode_model: &OdeModel,
    y: &[f64],
    p: &[f64],
    t: f64,
) -> Result<Vec<f64>, SimError> {
    let mut rhs = vec![0.0; y.len()];
    ode_model.eval_residual(y, p, t, &mut rhs)?;
    let state_count = model.state_scalar_count().min(rhs.len());
    let mut dy = vec![0.0; y.len()];
    if state_count > 0 {
        let mass_matrix =
            PreparedMassMatrix::new(&model.artifacts.continuous.mass_matrix, state_count)?;
        let state_dy = mass_matrix.solve(&rhs[..state_count])?;
        dy[..state_count].copy_from_slice(&state_dy);
    }
    Ok(dy)
}

fn residual_summary_entry(ode_model: &OdeModel, values: &[f64], row_offset: usize) -> String {
    let Some((row, value)) = values
        .iter()
        .copied()
        .enumerate()
        .max_by(|(_, lhs), (_, rhs)| residual_sort_key(*lhs).total_cmp(&residual_sort_key(*rhs)))
    else {
        return "none".to_string();
    };
    let row_idx = row_offset + row;
    let target = ode_model
        .target_name_for_row(row_idx)
        .map_or(String::new(), |name| format!(" target={name}"));
    format!("row={row_idx}{target} value={value:.6e}")
}

fn residual_sort_key(value: f64) -> f64 {
    if value.is_finite() {
        value.abs()
    } else {
        f64::INFINITY
    }
}

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
    let previous_h = solver.state().h;
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
    apply_event_restart_step_size::<Eqn, S>(&mut fresh_state, previous_h, h_cap);
    solver.set_state(fresh_state);
    mark_solver_state_modified_for_reinit(solver);
    Ok(())
}

fn apply_event_restart_step_size<'a, Eqn, S>(state: &mut S::State, previous_h: f64, h_cap: f64)
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    if let Some(restart_h) = event_restart_step_size(previous_h, h_cap) {
        *state.as_mut().h = restart_h;
    }
}

fn event_restart_step_size(previous_h: f64, h_cap: f64) -> Option<f64> {
    if !previous_h.is_finite() || previous_h == 0.0 || !h_cap.is_finite() || h_cap <= 0.0 {
        return None;
    }
    let magnitude = previous_h.abs().min(h_cap);
    (magnitude > 0.0).then(|| magnitude.copysign(previous_h))
}

fn mark_solver_state_modified_for_reinit<'a, Eqn, S>(solver: &mut S)
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    let _state = solver.state_mut();
}

pub(crate) fn can_use_state_only_bdf(model: &solve::SolveModel) -> Result<bool, SimError> {
    match state_only_bdf_eligibility(model)? {
        StateOnlyBdfEligibility::Eligible => Ok(true),
        StateOnlyBdfEligibility::Ineligible(reason) => {
            tracing::debug!(
                target: "rumoca_solver_diffsol::bdf_path",
                reason = reason.as_str(),
                "state-only BDF ineligible"
            );
            Ok(false)
        }
    }
}

enum StateOnlyBdfEligibility {
    Eligible,
    Ineligible(String),
}

fn state_only_bdf_eligibility(
    model: &solve::SolveModel,
) -> Result<StateOnlyBdfEligibility, SimError> {
    let state_count = model.state_scalar_count();
    let derivative_rhs_len = model
        .problem
        .continuous
        .derivative_rhs
        .len()
        .map_err(|err| SimError::SolveIr(err.to_string()))?;
    if derivative_rhs_len != state_count {
        return Ok(StateOnlyBdfEligibility::Ineligible(format!(
            "derivative_rhs_len={derivative_rhs_len} does not match state_count={state_count}"
        )));
    }
    let derivative_rows =
        solve_eval::to_scalar_program_block(&model.problem.continuous.derivative_rhs)?;
    let direct_deps = derivative_non_state_loads(model, &derivative_rows);
    if direct_deps.is_empty() {
        return Ok(StateOnlyBdfEligibility::Eligible);
    }
    if let Some(reason) = projection_plan_missing_non_state_loads(model, direct_deps)? {
        Ok(StateOnlyBdfEligibility::Ineligible(reason))
    } else {
        Ok(StateOnlyBdfEligibility::Eligible)
    }
}

fn derivative_non_state_loads(
    model: &solve::SolveModel,
    derivative_rows: &solve::ScalarProgramBlock,
) -> BTreeSet<usize> {
    let state_count = model.state_scalar_count();
    let solver_count = model.solver_scalar_count();
    derivative_rows
        .programs
        .iter()
        .take(state_count)
        .flat_map(|row| non_state_y_loads(row, state_count, solver_count))
        .collect()
}

fn projection_plan_missing_non_state_loads(
    model: &solve::SolveModel,
    direct_deps: BTreeSet<usize>,
) -> Result<Option<String>, SimError> {
    let state_count = model.state_scalar_count();
    let solver_count = model.solver_scalar_count();
    let implicit_rows =
        solve_eval::to_scalar_program_block(&model.problem.continuous.implicit_rhs)?;
    let producer_programs = match solve_eval::algebraic_projection_producer_programs(model) {
        Ok(producer_rows) => producer_rows,
        Err(error) => return Ok(Some(error.to_string())),
    };
    let mut needed = BTreeSet::new();
    let mut stack = direct_deps.into_iter().collect::<Vec<_>>();
    while let Some(index) = stack.pop() {
        if index < state_count || !needed.insert(index) {
            continue;
        }
        let Some(program_idx) = producer_programs.get(&index).copied() else {
            return Ok(Some(format!(
                "missing projection producer for solver_y[{index}] ({})",
                solver_name(model, index)
            )));
        };
        let Some(row) = implicit_rows.programs.get(program_idx) else {
            return Ok(Some(format!(
                "missing implicit producer program {program_idx} for solver_y[{index}] ({})",
                solver_name(model, index)
            )));
        };
        stack.extend(non_state_y_loads(row, state_count, solver_count));
    }
    Ok(None)
}

fn solver_name(model: &solve::SolveModel, index: usize) -> &str {
    model
        .problem
        .solve_layout
        .solver_maps
        .names
        .get(index)
        .map(String::as_str)
        .unwrap_or("<unnamed>")
}

fn non_state_y_loads(
    row: &[solve::LinearOp],
    state_count: usize,
    solver_count: usize,
) -> Vec<usize> {
    let mut loads = row
        .iter()
        .filter_map(|op| match *op {
            solve::LinearOp::LoadY { index, .. }
                if index >= state_count && index < solver_count =>
            {
                Some(index)
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    loads.sort_unstable();
    loads.dedup();
    loads
}

#[cfg(test)]
mod tests {
    use super::event_restart_step_size;

    #[test]
    fn event_restart_preserves_accepted_scale_direction_and_cap() {
        assert_eq!(event_restart_step_size(1.0e-4, 1.0e-3), Some(1.0e-4));
        assert_eq!(event_restart_step_size(2.0e-3, 1.0e-3), Some(1.0e-3));
        assert_eq!(event_restart_step_size(-2.0e-3, 1.0e-3), Some(-1.0e-3));
    }

    #[test]
    fn event_restart_rejects_invalid_scale_without_overwriting_fresh_state() {
        assert_eq!(event_restart_step_size(0.0, 1.0e-3), None);
        assert_eq!(event_restart_step_size(f64::NAN, 1.0e-3), None);
        assert_eq!(event_restart_step_size(1.0e-4, f64::INFINITY), None);
        assert_eq!(event_restart_step_size(1.0e-4, 0.0), None);
    }
}
