use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_solver::SimOptions;

use super::diagnostics::SimulationDiagnosticError;

pub fn lower_dae_for_simulation(
    model: &dae::Dae,
    opts: &SimOptions,
) -> Result<solve::SolveModel, SimulationDiagnosticError> {
    lower_dae_for_simulation_with_stage_timing_and_runtime_overrides(
        model,
        opts,
        &std::collections::HashMap::new(),
        |_| {},
    )
    .map(|(model, _)| model)
}

pub(crate) fn lower_dae_for_simulation_with_stage_timing_and_runtime_overrides(
    model: &dae::Dae,
    opts: &SimOptions,
    runtime_overrides: &std::collections::HashMap<String, f64>,
    begin_stage: impl FnMut(&'static str),
) -> Result<(solve::SolveModel, crate::BuildSimulationTimings), SimulationDiagnosticError> {
    let (lowered, timings) =
        lower_correlated_for_simulation_with_stage_timing_and_runtime_overrides(
            model,
            opts,
            runtime_overrides,
            begin_stage,
        )?;
    Ok((lowered.into_model(), timings))
}

/// Lower while retaining the phase-owned DAE/Solve correlation until the
/// caller consumes it into the runtime FMI component.
pub(crate) fn lower_correlated_for_simulation_with_stage_timing_and_runtime_overrides<'source>(
    model: &'source dae::Dae,
    _opts: &SimOptions,
    runtime_overrides: &std::collections::HashMap<String, f64>,
    mut begin_stage: impl FnMut(&'static str),
) -> Result<
    (
        rumoca_phase_solve::LoweredSolveModel<'source>,
        crate::BuildSimulationTimings,
    ),
    SimulationDiagnosticError,
> {
    let lowered =
        rumoca_phase_solve::lower_solve_model(model, runtime_overrides, |stage| match stage {
            rumoca_phase_solve::SolveModelLoweringStage::Programs => begin_stage("ir_solve"),
            rumoca_phase_solve::SolveModelLoweringStage::RuntimeValues => {
                begin_stage("runtime_vectors");
            }
        })
        .map_err(model_lowering_error)?;
    let timings = crate::BuildSimulationTimings {
        ir_solve_seconds: lowered.program_seconds() + lowered.runtime_value_seconds(),
        ir_solve_structural_dae_seconds: lowered.runtime_value_seconds(),
        ir_solve_lower_seconds: lowered.program_seconds(),
        ..crate::BuildSimulationTimings::default()
    };
    Ok((lowered, timings))
}

pub(super) fn model_lowering_error(
    error: rumoca_phase_solve::SolveModelLoweringError,
) -> SimulationDiagnosticError {
    match error {
        rumoca_phase_solve::SolveModelLoweringError::Lower(error) => {
            SimulationDiagnosticError::SolveLowering(error)
        }
        rumoca_phase_solve::SolveModelLoweringError::RuntimeValues { message, span } => {
            SimulationDiagnosticError::RuntimePreparation { message, span }
        }
        rumoca_phase_solve::SolveModelLoweringError::InvalidOverride { message } => {
            SimulationDiagnosticError::InvalidOverride { message }
        }
        rumoca_phase_solve::SolveModelLoweringError::VariableCatalogRefinement { error, span } => {
            SimulationDiagnosticError::VariableCatalogRefinement { error, span }
        }
        rumoca_phase_solve::SolveModelLoweringError::ScalarConstantDerivativeRefinement {
            error,
            span,
        } => SimulationDiagnosticError::ScalarConstantDerivativeRefinement { error, span },
    }
}
