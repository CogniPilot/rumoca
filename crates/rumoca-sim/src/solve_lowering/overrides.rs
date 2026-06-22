//! Solver-neutral application of a request's tunable-parameter and state-start
//! overrides onto a freshly lowered solve model. Every backend (diffsol, rk45)
//! funnels through here so overrides behave identically regardless of engine.

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_solver::SimOptions;

use super::diagnostics::SimulationDiagnosticError;
use super::entry::lower_dae_for_simulation;

/// Lower a DAE to a simulation-ready solve model and apply the request's tunable
/// parameter / state-start overrides. Solver-neutral: every backend (diffsol,
/// rk45) funnels through here, so overrides are applied identically regardless of
/// which engine runs.
pub(crate) fn lower_for_simulation_with_overrides(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<solve::SolveModel, SimulationDiagnosticError> {
    let mut solve_model = lower_dae_for_simulation(dae_model, opts)
        .map_err(SimulationDiagnosticError::SolveLowering)?;
    apply_simulation_overrides(&mut solve_model, dae_model, opts)?;
    Ok(solve_model)
}

/// Apply tunable parameter and state-start overrides to a freshly lowered solve
/// model in place. Rejects any override that cannot be applied *correctly* —
/// structural parameters (baked at lowering), folded parameters (no runtime
/// slot), and depended-upon parameters (overriding the leaf would leave the
/// dependent stale) — so an override is never silently dropped or silently
/// wrong. Solver-independent: operates only on the neutral [`solve::SolveModel`].
///
/// Exposed `pub(crate)` so the prepared-simulation and interactive-stepper entry
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
        // silently run with the folded value.
        rumoca_phase_solve::propagate_parameter_overrides(dae_model, solve_model, &pinned)
            .map_err(|error| reject(error.to_string()))?;
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
