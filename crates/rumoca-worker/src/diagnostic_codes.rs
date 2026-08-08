//! Recovery of stable SPEC_0008 diagnostic codes from worker stage errors.
//!
//! The Solve and Sim stages report free-form strings (`ir_solve_error`,
//! `sim_error`) because `SimError` is a plain string carrier. `rumoca-sim`
//! therefore tags solve-lowering and runtime-diagnostic failures with a
//! `[CODE] ` prefix, mirroring the CLI's `[{code}] {error}` rendering, and this
//! module lifts that code back into a structured field so it reaches the MSL
//! result schema, `error_code_counts`, and `rumoca-msl-tools triage`.
//!
//! No code table is re-declared here. The `EX0xx` values come from
//! [`SimulationDiagnosticError::diagnostic_code`] — the canonical mapping that
//! also feeds the CLI and the LSP — and the `ES0xx` value from
//! [`rumoca_phase_structural::diagnostic_codes`], the grep-discoverable
//! structural registry. Re-declaring them as private constants would let a
//! renumbering in either registry diverge silently from what the MSL results
//! report.

use rumoca_phase_structural::diagnostic_codes::ES011_EMPTY_SYSTEM;
use rumoca_sim::{SimError, SimulationDiagnosticError};

/// Canonical code for "the numeric solver reported a failure while
/// integrating" (`EX001`), read from the mapping that mints it.
fn solver_failure_code() -> &'static str {
    SimulationDiagnosticError::Solver(String::new()).diagnostic_code()
}

/// Canonical code for "preparing the lowered model for execution failed"
/// (`EX002`), read from the mapping that mints it.
fn runtime_preparation_code() -> &'static str {
    SimulationDiagnosticError::RuntimePreparation {
        message: String::new(),
        span: None,
    }
    .diagnostic_code()
}

/// Recover the SPEC_0008 code a stage error carries as a `[CODE] ` prefix.
///
/// Returns `None` for messages with no bracketed prefix, or whose prefix is not
/// a `<two uppercase letters><digits>` code — an artifact-write IO error must
/// not be mistaken for a diagnostic.
#[must_use]
pub fn embedded_diagnostic_code(message: &str) -> Option<String> {
    let rest = message.strip_prefix('[')?;
    let (code, _) = rest.split_once(']')?;
    let code = rumoca_core::short_phase_error_code(code);
    let prefix = code.get(..2)?;
    let digits = code.get(2..)?;
    let is_code = prefix.chars().all(|ch| ch.is_ascii_uppercase())
        && !digits.is_empty()
        && digits.chars().all(|ch| ch.is_ascii_digit());
    is_code.then(|| code.to_string())
}

/// Stable SPEC_0008 code for a simulation-stage failure.
///
/// Only failures that are actually compiler/runtime *defects* get a code: a
/// timeout, a Modelica `assert`, and a `terminate` are model behaviour and stay
/// deliberately uncoded so they cannot pollute `error_code_counts`.
///
/// A delegated code carried in the message always wins over the `EX0xx`
/// fallback: a code identifies the defect, not the surface that reported it.
/// That delegation is how `EX003` (rejected override) and every `EL0xx`/`ES0xx`
/// lowering code reach this function — re-deriving from the `SimError` variant
/// alone can only ever produce the two fallbacks below, which is exactly why
/// both backends tag their lowering failures.
///
/// The fallbacks are *synthesised*, not reported: they name the surface that
/// failed, not the defect. Consumers that classify a failure must not treat
/// them as producer knowledge — see `classify_sim_reason` in
/// `rumoca-msl-tools triage`.
#[must_use]
pub fn sim_error_diagnostic_code(err: &SimError) -> Option<String> {
    // `kind()` peels the `SimFailureStage` annotations the solver backend
    // attaches, so an annotated failure yields exactly the same code as before.
    match err.kind() {
        SimError::Timeout { .. }
        | SimError::AssertionFailed { .. }
        | SimError::Terminated { .. } => None,
        SimError::EmptySystem => Some(ES011_EMPTY_SYSTEM.to_string()),
        SimError::SolveIr(message) => Some(
            embedded_diagnostic_code(message)
                .unwrap_or_else(|| runtime_preparation_code().to_string()),
        ),
        SimError::RuntimeContract { .. } => Some(runtime_preparation_code().to_string()),
        // Preparing the lowered model for execution failed: the model does not
        // present a reduced state-only system, which is a lowering outcome
        // discovered at backend-build time, not a numeric solver failure.
        SimError::StateOnlyPathUnavailable(_)
        | SimError::DirectionalDerivativeUnavailable { .. } => {
            Some(runtime_preparation_code().to_string())
        }
        SimError::SolverError(message) => Some(
            embedded_diagnostic_code(message).unwrap_or_else(|| solver_failure_code().to_string()),
        ),
        // Unreachable: `kind()` returns an unannotated failure.
        SimError::Staged { .. } => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn runtime_codes_match_the_canonical_simulation_mapping() {
        assert_eq!(solver_failure_code(), "EX001");
        assert_eq!(runtime_preparation_code(), "EX002");
        assert_eq!(ES011_EMPTY_SYSTEM, "ES011");
    }

    #[test]
    fn embedded_diagnostic_code_recovers_bracketed_spec_0008_codes() {
        assert_eq!(
            embedded_diagnostic_code("[EL001] unsupported expression"),
            Some("EL001".to_string())
        );
        assert_eq!(
            embedded_diagnostic_code("[ES010] singular system"),
            Some("ES010".to_string())
        );
        assert_eq!(
            embedded_diagnostic_code("[rumoca::solve::EL020] evaluation failed"),
            Some("EL020".to_string())
        );
        assert_eq!(embedded_diagnostic_code("no code here"), None);
        assert_eq!(embedded_diagnostic_code("[not a code] message"), None);
        assert_eq!(embedded_diagnostic_code("[EL] message"), None);
        assert_eq!(
            embedded_diagnostic_code("failed to write ir-solve.json: disk full"),
            None
        );
    }

    #[test]
    fn sim_error_diagnostic_code_only_codes_actual_defects() {
        assert_eq!(
            sim_error_diagnostic_code(&SimError::SolveIr("[EL006] scalarization".to_string())),
            Some("EL006".to_string())
        );
        assert_eq!(
            sim_error_diagnostic_code(&SimError::SolveIr("runtime refresh failed".to_string())),
            Some("EX002".to_string())
        );
        assert_eq!(
            sim_error_diagnostic_code(&SimError::SolverError("step size too small".to_string())),
            Some("EX001".to_string())
        );
        assert_eq!(
            sim_error_diagnostic_code(&SimError::EmptySystem),
            Some("ES011".to_string())
        );
        assert_eq!(
            sim_error_diagnostic_code(&SimError::DirectionalDerivativeUnavailable {
                reason: "non-differentiable operation".to_string(),
            }),
            Some("EX002".to_string())
        );
        assert_eq!(
            sim_error_diagnostic_code(&SimError::Timeout { seconds: 10.0 }),
            None
        );
        assert_eq!(
            sim_error_diagnostic_code(&SimError::AssertionFailed {
                time: 1.0,
                message: "x > 0".to_string(),
            }),
            None
        );
        assert_eq!(
            sim_error_diagnostic_code(&SimError::Terminated {
                time: 2.0,
                message: "done".to_string(),
            }),
            None
        );
    }

    /// `EX003` exists only on the structured diagnostic, so it can reach the
    /// result schema *only* through the tagged message the backends emit.
    /// Annotating a failure with the stage that raised it must not perturb the
    /// code the result schema reports: the stage is additional data, not a
    /// replacement for the SPEC_0008 code.
    #[test]
    fn stage_annotations_do_not_change_the_reported_code() {
        use rumoca_sim::SimFailureStage;
        for error in [
            SimError::SolveIr("[EL006] scalarization".to_string()),
            SimError::SolverError("step size too small".to_string()),
            SimError::EmptySystem,
            SimError::Timeout { seconds: 10.0 },
        ] {
            let bare = sim_error_diagnostic_code(&error);
            let staged = error.at_stage(SimFailureStage::ManifoldProjection);
            assert_eq!(sim_error_diagnostic_code(&staged), bare);
        }
    }

    /// A re-derivation from the `SimError` variant would make it unreachable.
    #[test]
    fn rejected_override_code_survives_into_the_result_schema() {
        let tagged = SimulationDiagnosticError::InvalidOverride {
            message: "unknown parameter 'nope'".to_string(),
        };
        let code = tagged.diagnostic_code();
        assert_eq!(code, "EX003");
        assert_eq!(
            sim_error_diagnostic_code(&SimError::SolveIr(format!("[{code}] {tagged}"))),
            Some("EX003".to_string()),
            "the override code must not be flattened into the EX002 fallback"
        );
    }
}
