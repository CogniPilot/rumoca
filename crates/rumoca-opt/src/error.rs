use thiserror::Error;

/// Error returned by optimization and backpropagation APIs.
#[derive(Debug, Error)]
pub enum OptError {
    #[error("failed to lower model for optimization: {0}")]
    Lowering(String),
    #[error("failed to build differentiable runtime: {0}")]
    Runtime(String),
    #[error("unknown trainable parameter `{name}`; available trainables: {available}")]
    UnknownTrainable { name: String, available: String },
    #[error("no trainable parameters selected")]
    EmptyTrainableSet,
    #[error("{what} length mismatch: got {got}, expected {expected}")]
    LengthMismatch {
        what: &'static str,
        got: usize,
        expected: usize,
    },
    #[error("gradient mode `{mode}` is unsupported for this model: {reason}")]
    UnsupportedGradientMode { mode: &'static str, reason: String },
    #[error("non-finite optimization value in `{what}`: {value}")]
    NonFinite { what: &'static str, value: f64 },
}

impl From<rumoca_sim::SimulationDiagnosticError> for OptError {
    fn from(value: rumoca_sim::SimulationDiagnosticError) -> Self {
        Self::Lowering(value.to_string())
    }
}

impl From<rumoca_eval_solve::EvalSolveError> for OptError {
    fn from(value: rumoca_eval_solve::EvalSolveError) -> Self {
        Self::Runtime(value.to_string())
    }
}

impl From<rumoca_solver::RuntimeSolveError> for OptError {
    fn from(value: rumoca_solver::RuntimeSolveError) -> Self {
        Self::Runtime(value.to_string())
    }
}
