//! One-shot inspection probes backing `rumoca sim --inspect eval` and
//! `--inspect jacobian`: lower the DAE, evaluate it at a named `(state, t)`, and
//! report named solver values / a named dense state Jacobian.

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_solver::SimOptions;

use super::diagnostics::SimulationDiagnosticError;
use super::entry::lower_dae_for_simulation;

/// Tolerance / iteration budget for the `--inspect eval` algebraic refresh, matching
/// the rk45 backend's runtime settings so the probe sees the same algebraic
/// solution the solver would at that point.
const EVAL_AT_REFRESH_TOL: f64 = 1.0e-10;
const EVAL_AT_REFRESH_MAX_ITERS: usize = 32;

/// Result of a [`eval_dae_at`] probe: the named per-variable report plus the
/// state vector and state names actually evaluated.
#[derive(Debug, Clone)]
pub struct EvalAtProbe {
    /// Named solver values and state derivatives, with non-finite entries flagged.
    pub report: rumoca_eval_solve::EvalAtReport,
    /// State vector used for the evaluation, in model state order.
    pub state_used: Vec<f64>,
    /// State names in model order, aligned with `state_used` — the authoritative
    /// answer to "what states does this model have?".
    pub state_names: Vec<String>,
}

/// Lower `dae_model` to the solver runtime and evaluate it at `(state, t)`,
/// naming every solver value and state derivative and flagging non-finite ones.
///
/// States are addressed by name (`state_overrides`), never by position, so the
/// probe is robust to state reordering during lowering: any state not listed
/// keeps its model initial value, and an unknown name is an error that lists the
/// valid state names. This is a one-shot, scriptable NaN trace — it attributes a
/// `NaN`/`inf` to a specific model variable in one command instead of repeated
/// instrumented rebuilds.
pub fn eval_dae_at(
    dae_model: &dae::Dae,
    opts: &SimOptions,
    state_overrides: &[(String, f64)],
    t: f64,
) -> Result<EvalAtProbe, SimulationDiagnosticError> {
    let solve_model = lower_dae_for_simulation(dae_model, opts)
        .map_err(SimulationDiagnosticError::SolveLowering)?;
    let (state_used, state_names) =
        resolve_probe_state(&solve_model, state_overrides, "--inspect eval --at")?;

    let runtime = rumoca_eval_solve::SolveRuntime::new(&solve_model)
        .map_err(SimulationDiagnosticError::from)?;
    let report = runtime.eval_at(
        t,
        &state_used,
        &solve_model.parameters,
        EVAL_AT_REFRESH_TOL,
        EVAL_AT_REFRESH_MAX_ITERS,
    );
    Ok(EvalAtProbe {
        report,
        state_used,
        state_names,
    })
}

/// Resolve a probe's state vector from named overrides: seed the model's initial
/// state, then apply each `name=value` override by name. Returns
/// `(state_used, state_names)`. An unknown name is an error (tagged with `label`,
/// e.g. `--inspect eval --at` / `--inspect jacobian --at`) that lists the valid
/// state names.
fn resolve_probe_state(
    solve_model: &solve::SolveModel,
    state_overrides: &[(String, f64)],
    label: &str,
) -> Result<(Vec<f64>, Vec<String>), SimulationDiagnosticError> {
    let state_count = solve_model.state_scalar_count();
    let state_names = solve_model.problem.solve_layout.solver_maps.names[..state_count].to_vec();

    let mut state_used = vec![0.0; state_count];
    for (dst, src) in state_used
        .iter_mut()
        .zip(solve_model.initial_y.iter().copied())
    {
        *dst = src;
    }
    for (name, value) in state_overrides {
        let index = state_names
            .iter()
            .position(|state| state == name)
            .ok_or_else(|| {
                SimulationDiagnosticError::Solver(format!(
                    "{label}: `{name}` is not a state of this model; valid states: [{}]",
                    state_names.join(", ")
                ))
            })?;
        state_used[index] = *value;
    }
    Ok((state_used, state_names))
}

/// Result of a [`jacobian_for_dae`] probe: the named dense state Jacobian plus
/// the state vector it was evaluated at.
#[derive(Debug, Clone)]
pub struct JacobianProbe {
    /// Named dense state Jacobian with singular-column / zero-pivot flags.
    pub report: rumoca_eval_solve::JacobianReport,
    /// State vector used, in model state order.
    pub state_used: Vec<f64>,
    /// State names in model order, aligned with `state_used`.
    pub state_names: Vec<String>,
}

/// Lower `dae_model` and assemble the dense state Jacobian
/// `∂(der(state))/∂(state)` at `(state, t)` by finite difference, naming every
/// row/column (by qualified name) and flagging structurally-singular columns and zero
/// pivots. States are addressed by name (`state_overrides`); unset states keep
/// their model initial value. Backs the `rumoca sim --inspect jacobian` debug dump.
pub fn jacobian_for_dae(
    dae_model: &dae::Dae,
    opts: &SimOptions,
    state_overrides: &[(String, f64)],
    t: f64,
) -> Result<JacobianProbe, SimulationDiagnosticError> {
    let solve_model = lower_dae_for_simulation(dae_model, opts)
        .map_err(SimulationDiagnosticError::SolveLowering)?;
    let (state_used, state_names) =
        resolve_probe_state(&solve_model, state_overrides, "--inspect jacobian --at")?;

    let runtime = rumoca_eval_solve::SolveRuntime::new(&solve_model)
        .map_err(SimulationDiagnosticError::from)?;
    let report = runtime.eval_state_jacobian(
        t,
        &state_used,
        &solve_model.parameters,
        rumoca_eval_solve::AlgebraicSettle {
            tol: EVAL_AT_REFRESH_TOL,
            max_iters: EVAL_AT_REFRESH_MAX_ITERS,
        },
    );
    Ok(JacobianProbe {
        report,
        state_used,
        state_names,
    })
}
