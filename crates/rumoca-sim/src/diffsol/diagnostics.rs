//! Lowering and preparation diagnostics as the backend's simulation error.

use super::*;

/// Preserve the originating diagnostic code when adapting to the backend's
/// string-carrying error.
/// diagnostic, which also carries the runtime `EX0xx` codes (notably `EX003`
/// for a rejected parameter/start override — otherwise unreachable downstream,
/// because a re-derivation from the stringified `SimError` can only ever
/// produce the `EX001`/`EX002` fallbacks).
pub(super) fn diagnostic_sim_error(err: SimulationDiagnosticError) -> SimError {
    // Carry the stage as typed data too. The `[CODE] ` tag is what the CLI
    // renders and what code-recovery reads back; the stage is what failure
    // classification consumes, so it must not have to re-parse that tag.
    match err {
        SimulationDiagnosticError::Simulation(error) => error,
        SimulationDiagnosticError::BackendOptions(error) => error.into(),
        error => {
            let stage = diagnostic_failure_stage(&error);
            SimError::SolveIr(format!("[{}] {error}", error.diagnostic_code())).at_stage(stage)
        }
    }
}

/// The simulation stage a lowering/preparation diagnostic belongs to, read off
/// the error variant rather than its rendered text.
fn diagnostic_failure_stage(err: &SimulationDiagnosticError) -> SimFailureStage {
    match err {
        SimulationDiagnosticError::SolveLowering(_) if err.is_structural() => {
            SimFailureStage::StructuralAnalysis
        }
        SimulationDiagnosticError::SolveLowering(_)
        | SimulationDiagnosticError::InvalidOverride { .. } => SimFailureStage::SolveLowering,
        SimulationDiagnosticError::RuntimePreparation { .. } => SimFailureStage::BackendBuild,
        SimulationDiagnosticError::Solver(_) => SimFailureStage::Integration,
        SimulationDiagnosticError::Simulation(error) => {
            error.stage().unwrap_or(SimFailureStage::Integration)
        }
        SimulationDiagnosticError::BackendOptions(_) => SimFailureStage::BackendBuild,
    }
}
