//! Error types for the rumoca compiler.

use miette::Diagnostic;
use rumoca_compile::compile::{
    ModelFailureDiagnostic,
    core::{Diagnostic as CommonDiagnostic, SourceMap},
};
use thiserror::Error;

/// Errors that can occur during compilation.
#[derive(Debug, Clone, Error, Diagnostic)]
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

    /// Strict compile failure with aggregated diagnostics.
    #[error("compilation failed: {summary}")]
    #[diagnostic(code(rumoca::compiler::E012))]
    CompileDiagnosticsError {
        summary: String,
        failures: Vec<ModelFailureDiagnostic>,
        source_map: Option<SourceMap>,
    },

    /// Structured source-backed diagnostics that should render directly in the CLI.
    #[error("{summary}")]
    #[diagnostic(code(rumoca::compiler::E013))]
    SourceDiagnosticsError {
        summary: String,
        diagnostics: Vec<CommonDiagnostic>,
        source_map: SourceMap,
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
