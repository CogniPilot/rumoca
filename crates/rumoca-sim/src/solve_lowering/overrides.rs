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
    let param_overrides: std::collections::HashMap<String, f64> = opts
        .param_overrides
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
        .collect();
    let mut solve_model = super::entry::lower_dae_for_simulation_with_param_overrides(
        dae_model,
        opts,
        &param_overrides,
    )
    .map_err(SimulationDiagnosticError::SolveLowering)?;
    // Validate overrides and apply state-start overrides. Parameter dependents were
    // already re-derived during lowering, so do not re-derive them again here.
    apply_simulation_overrides(&mut solve_model, dae_model, opts, false)?;
    Ok(solve_model)
}

/// Apply tunable parameter and state-start overrides to a freshly lowered solve
/// model in place. Rejects any override that cannot be applied *correctly* —
/// structural parameters (baked at lowering), folded parameters (no runtime
/// slot), and depended-upon parameters (overriding the leaf would leave the
/// dependent stale) — so an override is never silently dropped or silently
/// wrong. Solver-independent: operates only on the neutral [`solve::SolveModel`].
///
/// Exposed `pub(crate)` so the prepared-simulation and interactive-session entry
/// points apply overrides identically to the one-shot `simulate` path — an
/// override set in `SimOptions` is honored on every entry, never silently
/// ignored.
pub(crate) fn apply_simulation_overrides(
    solve_model: &mut solve::SolveModel,
    dae_model: &dae::Dae,
    opts: &SimOptions,
    rederive_dependents: bool,
) -> Result<(), SimulationDiagnosticError> {
    let reject = |message: String| SimulationDiagnosticError::InvalidOverride { message };

    if !opts.param_overrides.is_empty() {
        let mut pinned: std::collections::HashSet<String> = std::collections::HashSet::new();
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
            pinned.insert(name.clone());
        }
        // Propagate the overrides to any parameters whose bindings depend on
        // them (e.g. `parameter b = 2*a`), so a dependent is never left stale.
        // A dependent whose binding can't be re-evaluated is rejected, not
        // silently run with the folded value. Skipped when the caller already
        // re-derived dependents during lowering (override-aware lowering handles
        // array-valued dependents that this scalar post-pass cannot).
        if rederive_dependents {
            rumoca_phase_solve::propagate_parameter_overrides(dae_model, solve_model, &pinned)
                .map_err(|error| reject(error.to_string()))?;
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
