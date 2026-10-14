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
use super::entry::{lower_correlated_for_fmi_export, lower_correlated_for_host_driven_inputs};

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
        crate::native_execution::admitted_native_execution_backend(opts, lowered.model())?;
    let receipt = crate::native_execution::execution_receipt(opts, lowered.model());
    let component = rumoca_phase_solve::fmi::finish_fmi_component(lowered).map_err(|error| {
        let span = error.span();
        SimulationDiagnosticError::RuntimePreparation {
            message: error.to_string(),
            span,
        }
    })?;
    Ok((
        rumoca_solver::fmi_me::MeModelArtifact::new(component).with_execution_receipt(receipt),
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

/// Prepare a generic host component, preserving absent external inputs.
#[cfg(feature = "fmi")]
pub fn lower_fmi_component(model: &dae::Dae) -> Result<FmiComponent, SimulationDiagnosticError> {
    finish_component(lower_correlated_for_host_driven_inputs(
        model,
        &rumoca_solver::SimOptions::default(),
        rumoca_ir_solve::RefreshProfile::default(),
    )?)
}

/// Package an FMI component with explicit checked instantiation defaults.
/// Source absence is preserved; inputs initialize before importer Set.
#[cfg(feature = "fmi")]
pub fn lower_fmi_export_component(
    model: &dae::Dae,
) -> Result<FmiComponent, SimulationDiagnosticError> {
    lower_fmi_export_component_for(model, rumoca_ir_solve::RefreshProfile::NativeCompact)
}

/// Package an FMI component for the generated scalar C kernel.
///
/// The C kernel runs the event iteration of the scalar runtime, so the model is
/// lowered for the scalar consumer: call assertions own event roots (the
/// stateless native product has no event iteration and records them in the
/// status channel instead).
#[cfg(feature = "fmi")]
pub fn lower_fmi_c_export_component(
    model: &dae::Dae,
) -> Result<FmiComponent, SimulationDiagnosticError> {
    lower_fmi_export_component_for(model, rumoca_ir_solve::RefreshProfile::Scalar)
}

#[cfg(feature = "fmi")]
fn lower_fmi_export_component_for(
    model: &dae::Dae,
    profile: rumoca_ir_solve::RefreshProfile,
) -> Result<FmiComponent, SimulationDiagnosticError> {
    let lowered =
        lower_correlated_for_fmi_export(model, &rumoca_solver::SimOptions::default(), profile)?;
    finish_component(lowered)
}

#[cfg(feature = "fmi")]
fn finish_component(
    lowered: rumoca_phase_solve::LoweredSolveModel<'_>,
) -> Result<FmiComponent, SimulationDiagnosticError> {
    let component = rumoca_phase_solve::fmi::finish_fmi_component(lowered).map_err(|error| {
        let span = error.span();
        SimulationDiagnosticError::RuntimePreparation {
            message: error.to_string(),
            span,
        }
    })?;
    Ok(component)
}
