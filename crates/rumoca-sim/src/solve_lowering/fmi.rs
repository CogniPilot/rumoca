//! Checked DAE → checked FMI component, as one simulation-facade call.
//!
//! The component owns the same executable kernel simulation runs, so the
//! emitted `modelDescription.xml`, the generated C, and an in-process host all
//! address one storage inventory. Assembling that kernel is simulation
//! preparation, which is why the CLI reaches this through the `rumoca-sim`
//! facade rather than depending on a phase crate.

#[cfg(any(feature = "fmi", feature = "solver-diffsol", feature = "solver-rk45"))]
use rumoca_ir_dae as dae;
#[cfg(feature = "fmi")]
use rumoca_ir_solve::fmi::FmiComponent;

#[cfg(any(feature = "fmi", feature = "solver-diffsol", feature = "solver-rk45"))]
use super::diagnostics::SimulationDiagnosticError;

/// Pre-write value of every host-driven input scalar, by scalar name.
///
/// Inputs that carry a binding are left out: the binding is already a checked
/// default the runtime vectors evaluate. Inputs the checked DAE gave no `start`
/// are left out too, so they still fail in `runtime_vectors` naming the input,
/// rather than being seeded with a value this function would have to invent.
#[cfg(feature = "fmi")]
fn host_driven_input_seeds(
    model: &dae::Dae,
) -> Result<std::collections::HashMap<String, f64>, SimulationDiagnosticError> {
    model.inspect(|view| {
        let mut seeds = std::collections::HashMap::new();
        for (_, variable) in view
            .variables()
            .filter(|(_, variable)| variable.role() == dae::VariableRole::Input)
        {
            seed_host_driven_input(view, variable, &mut seeds)?;
        }
        Ok(seeds)
    })
}

#[cfg(feature = "fmi")]
fn seed_host_driven_input<'dae>(
    view: dae::DaeView<'dae>,
    variable: dae::VariableView<'dae>,
    seeds: &mut std::collections::HashMap<String, f64>,
) -> Result<(), SimulationDiagnosticError> {
    if variable.binding().is_some() || variable.start().is_none() {
        return Ok(());
    }
    let values = rumoca_phase_solve::host_driven_input_start_values(view, variable)
        .map_err(super::entry::model_lowering_error)?
        .expect("a supplied numeric input start evaluates to values");
    for scalar in 0..variable.scalar_count() {
        let name = variable.scalar_name(scalar).ok_or_else(|| {
            preparation_error(
                format!(
                    "checked input `{}` has no scalar name at ordinal {scalar}",
                    variable.name()
                ),
                variable,
            )
        })?;
        // One start expression may cover every scalar of an array input
        // (`input Real u[3](start = 0)`), exactly as the runtime vectors
        // broadcast a scalar value across the declared shape.
        let value = match values.as_slice() {
            [single] => *single,
            many => *many.get(scalar).ok_or_else(|| {
                preparation_error(
                    format!(
                        "start value for input `{}` contains {} scalars; expected {}",
                        variable.name(),
                        many.len(),
                        variable.scalar_count()
                    ),
                    variable,
                )
            })?,
        };
        seeds.insert(name, value);
    }
    Ok(())
}

#[cfg(feature = "fmi")]
fn preparation_error(
    message: String,
    variable: dae::VariableView<'_>,
) -> SimulationDiagnosticError {
    SimulationDiagnosticError::RuntimePreparation {
        message,
        span: Some(variable.declaration().span()),
    }
}

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
        crate::native_execution::admitted_native_execution_backend(opts, lowered.model())
            .map_err(SimulationDiagnosticError::from)?;
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
    let lowered = rumoca_phase_solve::lower_solve_model(model, &host_driven_seeds, |_| {})
        .map_err(super::entry::model_lowering_error)?;
    rumoca_phase_solve::fmi::finish_fmi_component(lowered).map_err(|error| {
        let span = error.span();
        SimulationDiagnosticError::RuntimePreparation {
            message: error.to_string(),
            span,
        }
    })
}
