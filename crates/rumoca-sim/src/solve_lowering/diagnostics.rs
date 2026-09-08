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
    SolveLowering(rumoca_phase_solve::LowerError),
    VariableCatalogRefinement {
        error: rumoca_phase_solve::VariableCatalogRefinementError,
        span: Option<rumoca_core::Span>,
    },
    /// An in-profile scalar constant-derivative root failed its C61 equation
    /// refinement; the lowering was refused, never repaired.
    ScalarConstantDerivativeRefinement {
        error: rumoca_phase_solve::ScalarConstantDerivativeMismatch,
        span: Option<rumoca_core::Span>,
    },
    Solver(String),
    RuntimePreparation {
        message: String,
        span: Option<rumoca_core::Span>,
    },
    NativeExecution {
        stage: rumoca_solver::NativeExecutionStage,
        owner: rumoca_solver::NativeExecutionOwner,
        reason: String,
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
            Self::VariableCatalogRefinement { .. }
            | Self::ScalarConstantDerivativeRefinement { .. } => {
                rumoca_phase_solve::diagnostic_codes::EL005_INVALID_SOLVE_CONTRACT
            }
            Self::Solver(_) => EX001_SOLVER_FAILURE,
            Self::RuntimePreparation { .. } => EX002_RUNTIME_PREPARATION,
            Self::NativeExecution { stage, .. } => match stage {
                rumoca_solver::NativeExecutionStage::Compile => EX002_RUNTIME_PREPARATION,
                rumoca_solver::NativeExecutionStage::Call => EX001_SOLVER_FAILURE,
            },
            Self::InvalidOverride { .. } => EX003_INVALID_OVERRIDE,
        }
    }

    pub fn diagnostic_label(&self) -> String {
        match self {
            Self::SolveLowering(_)
            | Self::VariableCatalogRefinement { .. }
            | Self::ScalarConstantDerivativeRefinement { .. } => {
                "Solve lowering failed here".to_string()
            }
            Self::Solver(_) | Self::RuntimePreparation { .. } | Self::NativeExecution { .. } => {
                "simulation failure originates here".to_string()
            }
            Self::InvalidOverride { .. } => "override originates here".to_string(),
        }
    }

    /// True when the diagnostic came from structural analysis of the lowered
    /// system (index reduction, matching, tearing) rather than from expression
    /// lowering.
    ///
    /// Read off the `LowerError` *variant*, so a consumer never has to
    /// recognise a structural rejection by the wording of its message.
    #[must_use]
    pub fn is_structural(&self) -> bool {
        matches!(
            self,
            Self::SolveLowering(rumoca_phase_solve::LowerError::Structural { .. })
        )
    }

    pub fn source_span(&self) -> Option<rumoca_core::Span> {
        match self {
            Self::SolveLowering(error) => error.source_span(),
            Self::VariableCatalogRefinement { span, .. }
            | Self::ScalarConstantDerivativeRefinement { span, .. } => *span,
            Self::Solver(_) | Self::InvalidOverride { .. } | Self::NativeExecution { .. } => None,
            Self::RuntimePreparation { span, .. } => *span,
        }
    }
}

impl std::fmt::Display for SimulationDiagnosticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SolveLowering(error) => write!(f, "{error}"),
            Self::VariableCatalogRefinement { error, .. } => write!(f, "{error}"),
            Self::ScalarConstantDerivativeRefinement { error, .. } => write!(f, "{error}"),
            Self::Solver(error) => write!(f, "{error}"),
            Self::RuntimePreparation { message, .. } => write!(f, "{message}"),
            Self::NativeExecution {
                stage,
                owner,
                reason,
            } => write!(f, "native {stage} failed for {owner}: {reason}"),
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

impl From<rumoca_solver::RuntimeSolveError> for SimulationDiagnosticError {
    fn from(value: rumoca_solver::RuntimeSolveError) -> Self {
        match value {
            rumoca_solver::RuntimeSolveError::NativeExecution {
                stage,
                owner,
                reason,
            } => Self::NativeExecution {
                stage,
                owner,
                reason,
            },
            other => Self::RuntimePreparation {
                message: other.to_string(),
                span: other.source_span(),
            },
        }
    }
}

#[cfg(any(feature = "fmi", feature = "solver-diffsol", feature = "solver-rk45"))]
impl From<crate::SimError> for SimulationDiagnosticError {
    fn from(value: crate::SimError) -> Self {
        match value.into_kind() {
            crate::SimError::NativeExecution {
                execution_stage: stage,
                owner,
                reason,
            } => Self::NativeExecution {
                stage,
                owner,
                reason,
            },
            other => Self::Solver(other.to_string()),
        }
    }
}
