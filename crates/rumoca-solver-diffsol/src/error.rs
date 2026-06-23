use rumoca_solver::RuntimeSolveError;

#[derive(Debug, thiserror::Error)]
pub enum SimError {
    #[error("empty system: no equations to simulate")]
    EmptySystem,

    #[error("solver error: {0}")]
    SolverError(String),

    #[error("solve-IR evaluation failed: {0}")]
    SolveIr(String),

    #[error("diffsol runtime contract violation: {reason}")]
    RuntimeContract { reason: String },

    #[error("Modelica assert failed at t={time:.9}: {message}")]
    AssertionFailed { time: f64, message: String },

    #[error("Modelica terminate requested at t={time:.9}: {message}")]
    Terminated { time: f64, message: String },

    #[error("timeout after {seconds:.3}s")]
    Timeout { seconds: f64 },
}

impl SimError {
    pub fn source_span(&self) -> Option<()> {
        None
    }
}

impl From<RuntimeSolveError> for SimError {
    fn from(value: RuntimeSolveError) -> Self {
        match value {
            RuntimeSolveError::SolveIr { message, span } => {
                let message = match span {
                    Some(span) => format!("{message} @ {span:?}"),
                    None => message,
                };
                Self::SolveIr(message)
            }
            RuntimeSolveError::UnsupportedModel { reason } => Self::SolveIr(reason),
            RuntimeSolveError::NonFiniteDerivative { state_name } => Self::SolveIr(format!(
                "non-finite derivative evaluation for state '{state_name}'"
            )),
            non_finite @ RuntimeSolveError::NonFiniteValue { .. } => {
                Self::SolveIr(non_finite.to_string())
            }
        }
    }
}

impl From<rumoca_eval_solve::EvalSolveError> for SimError {
    fn from(value: rumoca_eval_solve::EvalSolveError) -> Self {
        Self::SolveIr(value.to_string())
    }
}

impl From<rumoca_eval_solve::ScalarizeError> for SimError {
    fn from(value: rumoca_eval_solve::ScalarizeError) -> Self {
        Self::SolveIr(value.to_string())
    }
}
