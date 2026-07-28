//! The error surfaced by every solve-lowering / simulation entry point, with
//! the diagnostic metadata (code, label, span) the CLI renders.
//!
//! Error codes: `EX0xx` for the simulation runtime (per SPEC_0008 "Error Code
//! Ranges"). Solve-lowering failures keep the code of the phase that produced
//! them (`EL0xx` from `rumoca-phase-solve`, `ES0xx` from
//! `rumoca-phase-structural`) rather than being relabeled here — a code
//! identifies the defect, not the reporting surface.

/// The numeric solver reported a failure while integrating.
pub(crate) const EX001_SOLVER_FAILURE: &str = "EX001";
/// Preparing the lowered model for execution failed (scalarization of runtime
/// vectors, prepared-value refresh, and similar pre-integration work).
pub(crate) const EX002_RUNTIME_PREPARATION: &str = "EX002";
/// A requested parameter/start override was rejected.
pub(crate) const EX003_INVALID_OVERRIDE: &str = "EX003";

#[derive(Debug)]
pub enum SimulationDiagnosticError {
    SolveLowering(rumoca_phase_solve::SolveModelLowerError),
    Solver(String),
    RuntimePreparation {
        message: String,
        span: Option<rumoca_core::Span>,
    },
    /// A requested parameter/start override could not be applied correctly
    /// (unknown name, structural/folded/depended-upon parameter, or a
    /// non-integral discrete value). Rejected rather than silently dropped.
    InvalidOverride {
        message: String,
    },
}

impl SimulationDiagnosticError {
    /// Stable diagnostic code (SPEC_0008).
    ///
    /// Solve-lowering failures delegate, so a lowering defect keeps one code
    /// from the phase that raised it all the way to the CLI and the LSP.
    #[must_use]
    pub fn diagnostic_code(&self) -> &'static str {
        match self {
            Self::SolveLowering(error) => error.code(),
            Self::Solver(_) => EX001_SOLVER_FAILURE,
            Self::RuntimePreparation { .. } => EX002_RUNTIME_PREPARATION,
            Self::InvalidOverride { .. } => EX003_INVALID_OVERRIDE,
        }
    }

    pub fn diagnostic_label(&self) -> String {
        match self {
            Self::SolveLowering(error) => error.diagnostic_label(),
            Self::Solver(_) | Self::RuntimePreparation { .. } => {
                "simulation failure originates here".to_string()
            }
            Self::InvalidOverride { .. } => "override originates here".to_string(),
        }
    }

    pub fn source_span(&self) -> Option<rumoca_core::Span> {
        match self {
            Self::SolveLowering(error) => error.source_span(),
            Self::Solver(_) | Self::InvalidOverride { .. } => None,
            Self::RuntimePreparation { span, .. } => *span,
        }
    }
}

impl std::fmt::Display for SimulationDiagnosticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SolveLowering(error) => write!(f, "{error}"),
            Self::Solver(error) => write!(f, "{error}"),
            Self::RuntimePreparation { message, .. } => write!(f, "{message}"),
            Self::InvalidOverride { message } => write!(f, "{message}"),
        }
    }
}

impl std::error::Error for SimulationDiagnosticError {}

impl From<rumoca_eval_solve::EvalSolveError> for SimulationDiagnosticError {
    fn from(value: rumoca_eval_solve::EvalSolveError) -> Self {
        Self::RuntimePreparation {
            message: value.to_string(),
            span: value.source_span(),
        }
    }
}

/// Every code this module mints, in numeric order — the registry the
/// `EX0xx`-range tests check reachability against.
///
/// Test-only because production code reaches each constant directly from
/// [`SimulationDiagnosticError::diagnostic_code`]; the shipped contract is the
/// `&'static str` that method returns, which consumers compare by mnemonic.
/// Delegated `EL0xx`/`ES0xx` codes are deliberately absent — they belong to the
/// registries of the phases that produce them.
#[cfg(test)]
pub(crate) const SIM_RUNTIME_DIAGNOSTIC_CODES: &[&str] = &[
    EX001_SOLVER_FAILURE,
    EX002_RUNTIME_PREPARATION,
    EX003_INVALID_OVERRIDE,
];
