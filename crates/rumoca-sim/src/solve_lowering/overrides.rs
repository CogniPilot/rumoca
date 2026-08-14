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
    let overrides = tunable_param_overrides(model, opts)?;
    let mut solve_model =
        super::entry::lower_dae_for_simulation_with_stage_timing_and_param_overrides(
            model,
            opts,
            &overrides,
            |_| {},
        )?
        .0;
    apply_state_overrides(&mut solve_model, opts)?;
    Ok(solve_model)
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
            record_parameter_names(variable, &mut tunable_names, &mut structural_names);
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
) {
    let names = (0..variable.scalar_count()).map(|scalar| {
        variable
            .scalar_name(scalar)
            .expect("checked scalar variable has a name")
    });
    if variable.role() == dae::VariableRole::Parameter && variable.is_tunable() {
        tunable_names.extend(names);
    } else {
        structural_names.extend(names);
    }
}

#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
pub(crate) fn apply_simulation_overrides(
    solve_model: &mut solve::SolveModel,
    model: &dae::Dae,
    opts: &SimOptions,
) -> Result<(), SimulationDiagnosticError> {
    // Parameter overrides must be incorporated while evaluating dependent
    // bindings, so rebuilding in place here would be incorrect. Verify that
    // callers supplied a model assembled through the override-aware entry.
    let expected = tunable_param_overrides(model, opts)?;
    for (name, value) in expected {
        let Some(solve::ScalarSlot::P { index, .. }) = solve_model.problem.layout.binding(&name)
        else {
            return Err(invalid(format!(
                "`{name}` has no runtime parameter slot and cannot be overridden"
            )));
        };
        let Some(actual) = solve_model.parameters.get(index) else {
            return Err(invalid(format!(
                "runtime parameter slot for `{name}` is outside the parameter vector"
            )));
        };
        if *actual != value {
            return Err(invalid(format!(
                "override for `{name}` was not applied while evaluating dependent bindings"
            )));
        }
    }
    apply_state_overrides(solve_model, opts)
}

fn apply_state_overrides(
    solve_model: &mut solve::SolveModel,
    opts: &SimOptions,
) -> Result<(), SimulationDiagnosticError> {
    let state_count = solve_model.state_scalar_count();
    let state_names = solve_model
        .problem
        .solve_layout
        .solver_maps
        .names
        .get(..state_count)
        .ok_or_else(|| invalid("state count exceeds the checked Solve name layout"))?;
    for (name, value) in &opts.start_overrides {
        if !value.is_finite() {
            return Err(invalid(format!(
                "start override for `{name}` must be finite"
            )));
        }
        let index = state_names
            .iter()
            .position(|candidate| candidate == name)
            .ok_or_else(|| invalid(format!("`{name}` is not a state of this model")))?;
        let target = solve_model.initial_y.get_mut(index).ok_or_else(|| {
            invalid(format!(
                "state `{name}` has no initial-value slot in the checked Solve model"
            ))
        })?;
        *target = *value;
    }
    Ok(())
}

fn invalid(message: impl Into<String>) -> SimulationDiagnosticError {
    SimulationDiagnosticError::InvalidOverride {
        message: message.into(),
    }
}
