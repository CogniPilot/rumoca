//! Solver-neutral application of a request's tunable-parameter and state-start
//! overrides onto a freshly lowered solve model. Every backend (diffsol, rk45)
//! funnels through here so overrides behave identically regardless of engine.

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_solver::SimOptions;

use super::diagnostics::SimulationDiagnosticError;

/// Lower a DAE to a simulation-ready solve model and apply the request's tunable
/// parameter / state-start overrides. Solver-neutral: every backend (diffsol,
/// rk45) funnels through here, so overrides are applied identically regardless of
/// which engine runs.
pub fn lower_for_simulation_with_overrides(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<solve::SolveModel, SimulationDiagnosticError> {
    // Tunable scalar-parameter overrides are applied *during* parameter-value
    // computation so their dependents — including parameter-derived array masks
    // (`sc/nc/sig`) that no scalar post-pass can re-derive — re-evaluate from the
    // override at parameter-set time. Non-tunable (structural) overrides are left
    // out of lowering so they cannot change sizing; `apply_simulation_overrides`
    // rejects them below with a clear "recompile" error.
    let param_overrides = tunable_param_overrides(dae_model, opts);
    let mut solve_model = super::entry::lower_dae_for_simulation_with_param_overrides(
        dae_model,
        opts,
        &param_overrides,
    )
    .map_err(SimulationDiagnosticError::SolveLowering)?;
    // Validate overrides and apply state-start overrides. Parameter dependents were
    // already re-derived during lowering, so do not re-derive them again here.
    apply_simulation_overrides(&mut solve_model, dae_model, opts)?;
    Ok(solve_model)
}

/// Lower a DAE to a differentiable runtime solve model and apply the same
/// tunable-parameter / state-start overrides as simulation lowering.
///
/// Unlike [`lower_for_simulation_with_overrides`], this entry point always
/// requests full sensitivity artifacts. `SimOptions::solver_mode` chooses an
/// integration backend for simulation, but optimization needs backend-neutral
/// derivative programs even when callers prefer the value-only RK path for
/// ordinary simulation.
pub fn lower_for_differentiation_with_overrides(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<solve::SolveModel, SimulationDiagnosticError> {
    let param_overrides = tunable_param_overrides(dae_model, opts);
    let mut solve_model = super::entry::lower_dae_for_differentiation_with_param_overrides(
        dae_model,
        opts,
        &param_overrides,
    )
    .map_err(SimulationDiagnosticError::SolveLowering)?;
    apply_simulation_overrides(&mut solve_model, dae_model, opts)?;
    Ok(solve_model)
}

pub(crate) fn tunable_param_overrides(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> std::collections::HashMap<String, f64> {
    opts.param_overrides
        .iter()
        .filter(|(name, _)| {
            dae_model
                .variables
                .parameters
                .iter()
                .find(|(key, _)| key.as_str() == name)
                .is_some_and(|(_, var)| var.is_tunable)
        })
        .map(|(name, value)| (name.clone(), *value))
        .collect()
}

/// Validate tunable parameter overrides and apply explicit parameter / state
/// start pins to a freshly lowered solve model in place. Dependent parameter
/// values are re-derived before this point by override-aware lowering; this
/// pass rejects structural parameters and folded parameters so an override is
/// never silently dropped or applied to the wrong runtime slot.
///
/// Exposed `pub(crate)` so the prepared-simulation and interactive-session entry
/// points apply overrides identically to the one-shot `simulate` path — an
/// override set in `SimOptions` is honored on every entry, never silently
/// ignored.
pub(crate) fn apply_simulation_overrides(
    solve_model: &mut solve::SolveModel,
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<(), SimulationDiagnosticError> {
    let reject = |message: String| SimulationDiagnosticError::InvalidOverride { message };

    if !opts.param_overrides.is_empty() {
        for (name, value) in &opts.param_overrides {
            match dae_model
                .variables
                .parameters
                .iter()
                .find(|(key, _)| key.as_str() == name)
                .map(|(_, var)| var.is_tunable)
            {
                None => {
                    return Err(reject(format!("`{name}` is not a parameter of this model")));
                }
                Some(false) => {
                    return Err(reject(format!(
                        "`{name}` is a structural parameter (it affects sizing/instantiation); \
                         change it by recompiling, not by a simulation override"
                    )));
                }
                Some(true) => {}
            }
            let Some(solve::ScalarSlot::P { index, .. }) = solve_model.problem.layout.binding(name)
            else {
                return Err(reject(format!(
                    "`{name}` has no runtime parameter slot (it was folded during lowering) and \
                     cannot be overridden at simulation time"
                )));
            };
            if solve_model
                .problem
                .solve_layout
                .discrete_valued_parameter_index(name)
                .is_some()
                && value.fract() != 0.0
            {
                return Err(reject(format!(
                    "`{name}` is a discrete-valued (Boolean/Integer/enum) parameter; override \
                     value {value} must be a whole number"
                )));
            }
            solve_model.parameters[index] = *value;
        }
    }

    if !opts.start_overrides.is_empty() {
        let state_count = solve_model.state_scalar_count();
        let names = &solve_model.problem.solve_layout.solver_maps.names;
        let state_names = &names[..state_count.min(names.len())];
        for (name, value) in &opts.start_overrides {
            let Some(index) = state_names.iter().position(|candidate| candidate == name) else {
                return Err(reject(format!(
                    "`{name}` is not a state of this model; start overrides apply to states"
                )));
            };
            solve_model.initial_y[index] = *value;
        }
    }

    Ok(())
}
