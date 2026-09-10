//! Checked DAE → checked FMI component, as one simulation-facade call.
//!
//! The component owns the same executable kernel simulation runs, so the
//! emitted `modelDescription.xml`, the generated C, and an in-process host all
//! address one storage inventory. Assembling that kernel is simulation
//! preparation, which is why the CLI reaches this through the `rumoca-sim`
//! facade rather than depending on a phase crate.

use rumoca_ir_dae as dae;
#[cfg(feature = "fmi")]
use rumoca_ir_solve::fmi::FmiComponent;

use super::diagnostics::SimulationDiagnosticError;
#[cfg(feature = "fmi")]
use super::entry::host_driven_input_seeds;

/// Consume a still-correlated phase lowering into the sole runtime artifact,
/// constructing any optional execution backend while the checked Solve view is
/// still borrowed from that aggregate.
#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
pub(crate) fn finish_runtime_fmi_artifact(
    lowered: rumoca_phase_solve::LoweredSolveModel<'_>,
    opts: &rumoca_solver::SimOptions,
) -> Result<
    (
        rumoca_solver::fmi_me::MeModelArtifact,
        Option<rumoca_solver::fmi_me::MeExecutionBackend>,
    ),
    SimulationDiagnosticError,
> {
    let execution_backend =
        crate::native_execution::admitted_native_execution_backend(opts, lowered.model());
    let component = rumoca_phase_solve::fmi::finish_fmi_component(lowered).map_err(|error| {
        let span = error.span();
        SimulationDiagnosticError::RuntimePreparation {
            message: error.to_string(),
            span,
        }
    })?;
    Ok((
        rumoca_solver::fmi_me::MeModelArtifact::new(component),
        execution_backend,
    ))
}

/// Canonical DAE-to-runtime-artifact path for callers that do not need stage
/// timing hooks. All solver dispatchers share it.
#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
pub(crate) fn lower_runtime_fmi_artifact(
    model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
) -> Result<
    (
        rumoca_solver::fmi_me::MeModelArtifact,
        Option<rumoca_solver::fmi_me::MeExecutionBackend>,
    ),
    SimulationDiagnosticError,
> {
    let lowered = super::lower_correlated_for_simulation_with_overrides(model, opts)?;
    finish_runtime_fmi_artifact(lowered, opts)
}

/// Lower one checked DAE into the checked FMI component an export target
/// renders.
///
/// Inputs are seeded from their checked `start` attributes, exactly as GPU
/// preparation seeds them: an FMU importer writes every input before each
/// evaluation, so an input with no other driver is not the failure it is for
/// headless simulation. An input the checked declaration left without any
/// `start` still fails.
#[cfg(feature = "fmi")]
pub fn lower_fmi_component(model: &dae::Dae) -> Result<FmiComponent, SimulationDiagnosticError> {
    let host_driven_seeds = host_driven_input_seeds(model)?;
    rumoca_phase_solve::fmi::lower_to_fmi_component(model, &host_driven_seeds).map_err(|error| {
        let span = error.span();
        SimulationDiagnosticError::RuntimePreparation {
            message: error.to_string(),
            span,
        }
    })
}
