//! The one construction of a scalar FMU's component from its options
//! document, shared by the FMU packager and the linked export, so the
//! component the guest constructs and the one whose programs were captured
//! come from the same lowering, options and step plan.
use crate::interactive_options::InteractiveOptions;
use crate::{SimOptions, SimSolverMode, SimulationDiagnosticError};
use rumoca_ir_solve::fmi::FmiComponent;

fn refusal(message: impl ToString) -> SimulationDiagnosticError {
    SimulationDiagnosticError::RuntimePreparation {
        message: message.to_string(),
        span: None,
    }
}

/// The options a scalar package lowers and plans with: the canonical solver
/// name, the document's step and the library defaults, then the document's
/// own settings. The model's experiment annotation is not read.
pub fn scalar_package_sim_options(
    options: &InteractiveOptions,
) -> Result<SimOptions, SimulationDiagnosticError> {
    let canonical = rumoca_core::canonical_solver_name(&options.solver).map_err(refusal)?;
    let base = SimOptions {
        solver_mode: SimSolverMode::from_external_name(canonical),
        dt: (options.dt > 0.0).then_some(options.dt),
        ..SimOptions::default()
    };
    Ok(options.configure(base, options.initial_inputs.clone()))
}

/// The scalar FMU's component: the FMI scalar export lowering of `dae` with
/// `sim`, carrying the step plan `sim` selects.
pub fn scalar_package_component(
    dae: &rumoca_ir_dae::Dae,
    sim: &SimOptions,
) -> Result<FmiComponent, SimulationDiagnosticError> {
    let component = crate::lower_fmi_scalar_export_component(dae, sim)?;
    let plan = crate::co_simulation_plan(sim).map_err(refusal)?;
    component.with_co_simulation(plan).map_err(refusal)
}
