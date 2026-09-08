//! Solver-neutral simulation overrides over checked declarations.

use std::collections::{HashMap, HashSet};

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_solver::SimOptions;

use super::diagnostics::SimulationDiagnosticError;

pub fn lower_for_simulation_with_overrides(
    model: &dae::Dae,
    opts: &SimOptions,
) -> Result<solve::SolveModel, SimulationDiagnosticError> {
    let overrides = construction_overrides(model, opts)?;
    super::entry::lower_dae_for_simulation_with_stage_timing_and_runtime_overrides(
        model,
        opts,
        &overrides,
        |_| {},
    )
    .map(|(model, _)| model)
}

/// Lower one simulation model while retaining the phase-owned DAE/Solve
/// correlation needed for FMI construction or canonical component wire.
pub fn lower_correlated_for_simulation_with_overrides<'source>(
    model: &'source dae::Dae,
    opts: &SimOptions,
) -> Result<rumoca_phase_solve::LoweredSolveModel<'source>, SimulationDiagnosticError> {
    let overrides = construction_overrides(model, opts)?;
    super::entry::lower_correlated_for_simulation_with_stage_timing_and_runtime_overrides(
        model,
        opts,
        &overrides,
        |_| {},
    )
    .map(|(lowered, _)| lowered)
}

pub fn lower_for_differentiation_with_overrides(
    model: &dae::Dae,
    opts: &SimOptions,
) -> Result<solve::SolveModel, SimulationDiagnosticError> {
    lower_for_simulation_with_overrides(model, opts)
}

pub(crate) fn tunable_param_overrides(
    model: &dae::Dae,
    opts: &SimOptions,
) -> Result<HashMap<String, f64>, SimulationDiagnosticError> {
    model.inspect(|view| {
        let mut tunable_names = HashSet::new();
        let mut structural_names = HashSet::new();
        for (_, variable) in view.variables().filter(|(_, variable)| {
            matches!(
                variable.role(),
                dae::VariableRole::Parameter | dae::VariableRole::Constant
            )
        }) {
            record_parameter_names(variable, &mut tunable_names, &mut structural_names)?;
        }

        let mut overrides = HashMap::with_capacity(opts.param_overrides.len());
        for (name, value) in &opts.param_overrides {
            if !value.is_finite() {
                return Err(invalid(format!("override for `{name}` must be finite")));
            }
            if tunable_names.contains(name) {
                overrides.insert(name.clone(), *value);
            } else if structural_names.contains(name) {
                return Err(invalid(format!(
                    "`{name}` is structural or constant; change it by recompiling"
                )));
            } else {
                return Err(invalid(format!(
                    "`{name}` is not a tunable parameter of this model"
                )));
            }
        }
        Ok(overrides)
    })
}

fn record_parameter_names(
    variable: dae::VariableView<'_>,
    tunable_names: &mut HashSet<String>,
    structural_names: &mut HashSet<String>,
) -> Result<(), SimulationDiagnosticError> {
    for scalar in 0..variable.scalar_count() {
        let name = variable.scalar_name(scalar).ok_or_else(|| {
            invalid(format!(
                "checked parameter `{}` has no scalar identity at ordinal {scalar}",
                variable.name()
            ))
        })?;
        if variable.role() == dae::VariableRole::Parameter && variable.is_tunable() {
            tunable_names.insert(name);
        } else {
            structural_names.insert(name);
        }
    }
    Ok(())
}

fn record_state_names(
    variable: dae::VariableView<'_>,
    state_names: &mut HashSet<String>,
) -> Result<(), SimulationDiagnosticError> {
    for scalar in 0..variable.scalar_count() {
        let name = variable.scalar_name(scalar).ok_or_else(|| {
            invalid(format!(
                "checked state `{}` has no scalar identity at ordinal {scalar}",
                variable.name()
            ))
        })?;
        state_names.insert(name);
    }
    Ok(())
}

pub(crate) fn construction_overrides(
    model: &dae::Dae,
    opts: &SimOptions,
) -> Result<HashMap<String, f64>, SimulationDiagnosticError> {
    let mut overrides = tunable_param_overrides(model, opts)?;
    let state_names = model.inspect(
        |view| -> Result<HashSet<String>, SimulationDiagnosticError> {
            let mut names = HashSet::new();
            for (_, variable) in view
                .variables()
                .filter(|(_, variable)| variable.role() == dae::VariableRole::State)
            {
                record_state_names(variable, &mut names)?;
            }
            Ok(names)
        },
    )?;
    for (name, value) in &opts.start_overrides {
        if !value.is_finite() {
            return Err(invalid(format!(
                "start override for `{name}` must be finite"
            )));
        }
        if !state_names.contains(name) {
            return Err(invalid(format!("`{name}` is not a state of this model")));
        }
        if overrides.insert(name.clone(), *value).is_some() {
            return Err(invalid(format!(
                "`{name}` is supplied as both a parameter and state override"
            )));
        }
    }
    Ok(overrides)
}

fn invalid(message: impl Into<String>) -> SimulationDiagnosticError {
    SimulationDiagnosticError::InvalidOverride {
        message: message.into(),
    }
}
