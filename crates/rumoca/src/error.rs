//! Error types for the rumoca compiler.

use miette::Diagnostic;
use rumoca_session::compile::{ModelFailureDiagnostic, core::SourceMap};
use thiserror::Error;

/// Errors that can occur during compilation.
#[derive(Debug, Error, Diagnostic)]
pub enum CompilerError {
    /// No model name was specified.
    #[error("no model specified")]
    #[diagnostic(
        code(rumoca::compiler::E001),
        help("use --model <NAME> to specify the model to compile")
    )]
    NoModelSpecified,

    /// The specified model was not found.
    #[error("model `{0}` not found")]
    #[diagnostic(code(rumoca::compiler::E002))]
    ModelNotFound(String),

    /// IO error reading a file.
    #[error("failed to read file `{path}`: {message}")]
    #[diagnostic(code(rumoca::compiler::E003))]
    IoError { path: String, message: String },

    /// Parse error.
    #[error("parse error: {0}")]
    #[diagnostic(code(rumoca::compiler::E004))]
    ParseError(String),

    /// Resolution error.
    #[error("resolution error: {0}")]
    #[diagnostic(code(rumoca::compiler::E005))]
    ResolveError(String),

    /// Type checking error.
    #[error("type error: {0}")]
    #[diagnostic(code(rumoca::compiler::E006))]
    TypeCheckError(String),

    /// Instantiation error.
    #[error("instantiation error: {0}")]
    #[diagnostic(code(rumoca::compiler::E007))]
    InstantiateError(String),

    /// Flattening error.
    #[error("flatten error: {0}")]
    #[diagnostic(code(rumoca::compiler::E008))]
    FlattenError(String),

    /// DAE conversion error.
    #[error("DAE conversion error: {0}")]
    #[diagnostic(code(rumoca::compiler::E009))]
    ToDaeError(String),

    /// Template rendering error.
    #[error("template error: {0}")]
    #[diagnostic(code(rumoca::compiler::E010))]
    TemplateError(String),

    /// JSON serialization error.
    #[error("JSON error: {0}")]
    #[diagnostic(code(rumoca::compiler::E011))]
    JsonError(String),

    /// Best-effort compilation failure with aggregated diagnostics.
    #[error("best-effort compilation failed: {summary}")]
    #[diagnostic(code(rumoca::compiler::E012))]
    BestEffortError {
        summary: String,
        failures: Vec<ModelFailureDiagnostic>,
        source_map: Option<SourceMap>,
    },
}

impl CompilerError {
    /// Create an IO error.
    pub fn io_error(path: impl Into<String>, message: impl Into<String>) -> Self {
        Self::IoError {
            path: path.into(),
            message: message.into(),
        }
    }
}
