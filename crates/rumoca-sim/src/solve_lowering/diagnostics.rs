//! The error surfaced by every solve-lowering / simulation entry point, with
//! the diagnostic metadata (code, label, span) the CLI renders.

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
    pub fn diagnostic_code(&self) -> &'static str {
        match self {
            Self::SolveLowering(_) => "lowering",
            Self::Solver(_) | Self::RuntimePreparation { .. } => "simulation",
            Self::InvalidOverride { .. } => "override",
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
