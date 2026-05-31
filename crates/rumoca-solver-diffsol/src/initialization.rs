use diffsol::{
    BdfState, MatrixCommon, OdeEquationsImplicit, OdeSolverProblem, OdeSolverState, VectorHost,
};
use rumoca_ir_solve as solve;
use rumoca_solver::PreparedMassMatrix;

use crate::{LinearSolver, Matrix, OdeModel, Scalar, SimError, Vector, catch_solver_panic};

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
