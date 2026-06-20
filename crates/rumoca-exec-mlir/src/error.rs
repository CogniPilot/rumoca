use thiserror::Error;

#[derive(Debug, Error)]
pub enum MlirError {
    #[error("MLIR tool not found ({tool}): {source}")]
    ToolNotFound {
        tool: &'static str,
        source: std::io::Error,
    },
    #[error("{tool} failed (exit {code}):\n{stderr}")]
    ToolFailed {
        tool: &'static str,
        code: i32,
        stderr: String,
    },
    #[error("template rendering failed: {0}")]
    Template(String),
    #[error("built-in target {target} is missing required template {template}")]
    MissingBuiltinTemplate {
        target: &'static str,
        template: &'static str,
    },
    #[error("Solve-IR scalarization failed: {message}")]
    Scalarization {
        message: String,
        span: Option<rumoca_core::Span>,
    },
    #[error("Solve-IR shape contract failed: {message}")]
    ShapeContract {
        message: String,
        span: Option<rumoca_core::Span>,
    },
    #[error("{function} output length mismatch: expected {expected}, got {actual}")]
    OutputLength {
        function: &'static str,
        expected: usize,
        actual: usize,
    },
    #[error("{operation} invalid input: {message}")]
    InvalidInput {
        operation: &'static str,
        message: String,
    },
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("dlopen failed: {0}")]
    Loading(#[from] libloading::Error),
    #[error("symbol not found in compiled library: {0}")]
    MissingSymbol(String),
    /// NVPTX f64 trig/transcendental ops require CUDA libdevice but it was not found.
    #[error(
        "CUDA libdevice not found (needed for GPU sin/cos/exp/log on f64).\n\
         Install with: sudo apt-get install -y nvidia-cuda-toolkit\n\
         Or set MlirBackendOptions::libdevice_path explicitly.\n\
         Searched: {searched}"
    )]
    LibdeviceNotFound { searched: String },
}

impl MlirError {
    pub fn source_span(&self) -> Option<rumoca_core::Span> {
        match self {
            Self::Scalarization { span, .. } => *span,
            Self::ShapeContract { span, .. } => *span,
            _ => None,
        }
    }
}

impl From<rumoca_eval_solve::ScalarizeError> for MlirError {
    fn from(value: rumoca_eval_solve::ScalarizeError) -> Self {
        Self::Scalarization {
            message: value.to_string(),
            span: value.source_span(),
        }
    }
}

impl From<rumoca_ir_solve::SolveProblemShapeContractError> for MlirError {
    fn from(value: rumoca_ir_solve::SolveProblemShapeContractError) -> Self {
        Self::ShapeContract {
            message: value.to_string(),
            span: value.source_span(),
        }
    }
}
