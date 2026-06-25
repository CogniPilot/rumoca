//! One-shot inspection probes backing `rumoca sim --inspect eval` and
//! `--inspect jacobian`: lower the DAE, evaluate it at a named `(state, t)`, and
//! report named solver values / a named dense state Jacobian.

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_solver::SimOptions;

use super::diagnostics::SimulationDiagnosticError;
use super::entry::lower_dae_for_simulation;
use super::overrides::lower_for_simulation_with_overrides;

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

/// Result of a [`parameter_jacobian_for_dae`] probe: the named dense parameter
/// sensitivity plus the state vector it was evaluated at.
#[derive(Debug, Clone)]
pub struct ParameterJacobianProbe {
    /// Named dense `∂(der(state))/∂p` (rows = `der(state)`, cols = parameters).
    pub report: rumoca_eval_solve::ParameterJacobianReport,
    /// State vector used, in model state order.
    pub state_used: Vec<f64>,
    /// State names in model order, aligned with `state_used`.
    pub state_names: Vec<String>,
}

/// Lower `dae_model` and assemble the dense parameter sensitivity
/// `∂(der(state))/∂p` at `(state, t)` by the exact forward-mode AD JVP (one
/// parameter unit seed at a time), naming every row (`der(name)`) and column
/// (parameter name). States are addressed by name (`state_overrides`); unset
/// states keep their model initial value. Backs roadmap Track 0.3
/// (`rumoca sim --inspect jacobian` parameter block) and the forward-gradient
/// validation.
pub fn parameter_jacobian_for_dae(
    dae_model: &dae::Dae,
    opts: &SimOptions,
    state_overrides: &[(String, f64)],
    t: f64,
) -> Result<ParameterJacobianProbe, SimulationDiagnosticError> {
    let solve_model = lower_dae_for_simulation(dae_model, opts)
        .map_err(SimulationDiagnosticError::SolveLowering)?;
    let (state_used, state_names) =
        resolve_probe_state(&solve_model, state_overrides, "--inspect jacobian --at")?;

    let runtime = rumoca_eval_solve::SolveRuntime::new(&solve_model)
        .map_err(SimulationDiagnosticError::from)?;
    let report = runtime.eval_parameter_jacobian(
        t,
        &state_used,
        &solve_model.parameters,
        rumoca_eval_solve::AlgebraicSettle {
            tol: EVAL_AT_REFRESH_TOL,
            max_iters: EVAL_AT_REFRESH_MAX_ITERS,
        },
    );
    Ok(ParameterJacobianProbe {
        report,
        state_used,
        state_names,
    })
}

/// Result of a [`steady_state_objective_gradient_for_dae`] probe.
#[derive(Debug, Clone)]
pub struct ObjectiveGradientProbe {
    /// Parameter-named steady-state gradient `d(objective)/dp`.
    pub report: rumoca_eval_solve::ObjectiveGradientReport,
    /// State vector used (should be at/near steady state), in model state order.
    pub state_used: Vec<f64>,
    /// State names in model order, aligned with `state_used`.
    pub state_names: Vec<String>,
}

/// Lower `dae_model` and compute the steady-state gradient `d(objective)/dp` of
/// a designated model variable (state or output/algebraic) w.r.t. the model
/// parameters, via the implicit-function sensitivity (roadmap Track 0.2). The
/// caller supplies a settled state; the result is only meaningful at/near
/// `f(y, p) = 0`.
pub fn steady_state_objective_gradient_for_dae(
    dae_model: &dae::Dae,
    opts: &SimOptions,
    state_overrides: &[(String, f64)],
    objective: &str,
    t: f64,
) -> Result<ObjectiveGradientProbe, SimulationDiagnosticError> {
    // Overrides-aware lowering so a tunable-parameter override (CLI `--param`,
    // Python `with_params`) linearizes the gradient at the overridden value — and
    // un-applicable overrides (structural/folded/depended-upon) are rejected
    // loudly rather than silently ignored. Empty overrides are a no-op, so the CLI
    // path is unchanged.
    let solve_model = lower_for_simulation_with_overrides(dae_model, opts)?;
    let (state_used, state_names) =
        resolve_probe_state(&solve_model, state_overrides, "objective gradient --at")?;

    let runtime = rumoca_eval_solve::SolveRuntime::new(&solve_model)
        .map_err(SimulationDiagnosticError::from)?;
    let report = runtime.steady_state_objective_gradient(
        t,
        &state_used,
        &solve_model.parameters,
        objective,
        rumoca_eval_solve::AlgebraicSettle {
            tol: EVAL_AT_REFRESH_TOL,
            max_iters: EVAL_AT_REFRESH_MAX_ITERS,
        },
    );
    Ok(ObjectiveGradientProbe {
        report,
        state_used,
        state_names,
    })
}

/// Lower `dae_model` and compute the steady-state objective gradient via the
/// reverse-mode adjoint (`SolveRuntime::steady_state_adjoint_objective_gradient`):
/// the full DAE residual `[der; g]` solved transposed (matrix-free GMRES), so it
/// handles solver algebraics and algebraic/output objectives. Same result as the
/// forward [`steady_state_objective_gradient_for_dae`] (roadmap Track B).
pub fn steady_state_adjoint_objective_gradient_for_dae(
    dae_model: &dae::Dae,
    opts: &SimOptions,
    state_overrides: &[(String, f64)],
    objective: &str,
    t: f64,
) -> Result<ObjectiveGradientProbe, SimulationDiagnosticError> {
    // Overrides-aware lowering: see `steady_state_objective_gradient_for_dae`.
    let solve_model = lower_for_simulation_with_overrides(dae_model, opts)?;
    let (state_used, state_names) =
        resolve_probe_state(&solve_model, state_overrides, "adjoint gradient --at")?;

    let runtime = rumoca_eval_solve::SolveRuntime::new(&solve_model)
        .map_err(SimulationDiagnosticError::from)?;
    let report = runtime.steady_state_adjoint_objective_gradient(
        t,
        &state_used,
        &solve_model.parameters,
        objective,
        rumoca_eval_solve::AlgebraicSettle {
            tol: EVAL_AT_REFRESH_TOL,
            max_iters: EVAL_AT_REFRESH_MAX_ITERS,
        },
    );
    Ok(ObjectiveGradientProbe {
        report,
        state_used,
        state_names,
    })
}

/// Result of a [`steady_state_parameter_sensitivity_for_dae`] probe.
#[derive(Debug, Clone)]
pub struct SteadyStateSensitivityProbe {
    /// Named dense steady-state sensitivity `∂y/∂p` (rows = states, cols = params).
    pub report: rumoca_eval_solve::SteadyStateSensitivityReport,
    /// State vector used (should be at/near steady state), in model state order.
    pub state_used: Vec<f64>,
    /// State names in model order, aligned with `state_used`.
    pub state_names: Vec<String>,
}

/// Lower `dae_model` and compute the steady-state forward parameter sensitivity
/// `∂y/∂p = -(∂f/∂y)⁻¹·∂f/∂p` at `(state, t)` via the implicit-function theorem
/// (roadmap Track 0.2). The caller is responsible for supplying a settled state
/// (`state_overrides`, or simulate to steady state first); the result is only
/// meaningful at/near `f(y, p) = 0`.
pub fn steady_state_parameter_sensitivity_for_dae(
    dae_model: &dae::Dae,
    opts: &SimOptions,
    state_overrides: &[(String, f64)],
    t: f64,
) -> Result<SteadyStateSensitivityProbe, SimulationDiagnosticError> {
    let solve_model = lower_dae_for_simulation(dae_model, opts)
        .map_err(SimulationDiagnosticError::SolveLowering)?;
    let (state_used, state_names) = resolve_probe_state(
        &solve_model,
        state_overrides,
        "steady-state sensitivity --at",
    )?;

    let runtime = rumoca_eval_solve::SolveRuntime::new(&solve_model)
        .map_err(SimulationDiagnosticError::from)?;
    let report = runtime.steady_state_parameter_sensitivity(
        t,
        &state_used,
        &solve_model.parameters,
        rumoca_eval_solve::AlgebraicSettle {
            tol: EVAL_AT_REFRESH_TOL,
            max_iters: EVAL_AT_REFRESH_MAX_ITERS,
        },
    );
    Ok(SteadyStateSensitivityProbe {
        report,
        state_used,
        state_names,
    })
}

/// Result of a [`state_and_parameter_jacobian_for_dae`] probe: both the state
/// Jacobian and the parameter sensitivity, from a single lowering.
#[derive(Debug, Clone)]
pub struct StateAndParameterJacobianProbe {
    /// Named dense state Jacobian `∂(der(state))/∂(state)`.
    pub state: rumoca_eval_solve::JacobianReport,
    /// Named dense parameter sensitivity `∂(der(state))/∂p`.
    pub parameter: rumoca_eval_solve::ParameterJacobianReport,
    /// State vector used, in model state order.
    pub state_used: Vec<f64>,
    /// State names in model order, aligned with `state_used`.
    pub state_names: Vec<String>,
}

/// Assemble both the state Jacobian `∂(der)/∂(state)` and the parameter
/// sensitivity `∂(der)/∂p` at `(state, t)` from a *single* lowering and runtime
/// (the dense-inspector path needs both and lowering can be expensive on large
/// models). Equivalent to calling [`jacobian_for_dae`] and
/// [`parameter_jacobian_for_dae`] separately, without lowering twice.
pub fn state_and_parameter_jacobian_for_dae(
    dae_model: &dae::Dae,
    opts: &SimOptions,
    state_overrides: &[(String, f64)],
    t: f64,
) -> Result<StateAndParameterJacobianProbe, SimulationDiagnosticError> {
    let solve_model = lower_dae_for_simulation(dae_model, opts)
        .map_err(SimulationDiagnosticError::SolveLowering)?;
    let (state_used, state_names) =
        resolve_probe_state(&solve_model, state_overrides, "--inspect jacobian --at")?;

    let runtime = rumoca_eval_solve::SolveRuntime::new(&solve_model)
        .map_err(SimulationDiagnosticError::from)?;
    let settle = rumoca_eval_solve::AlgebraicSettle {
        tol: EVAL_AT_REFRESH_TOL,
        max_iters: EVAL_AT_REFRESH_MAX_ITERS,
    };
    let state = runtime.eval_state_jacobian(t, &state_used, &solve_model.parameters, settle);
    let parameter =
        runtime.eval_parameter_jacobian(t, &state_used, &solve_model.parameters, settle);
    Ok(StateAndParameterJacobianProbe {
        state,
        parameter,
        state_used,
        state_names,
    })
}
