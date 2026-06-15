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

    // Note: E006 (TypeCheckError), E007 (InstantiateError), and E009
    // (ToDaeError) string wrappers were removed; phase failures now flow
    // through `CompileDiagnosticsError` so spans survive to the CLI.
    /// Flattening error (summary-only session path).
    #[error("flatten error: {0}")]
    #[diagnostic(code(rumoca::compiler::E008))]
    FlattenError(String),

    /// Template rendering error.
    #[error(transparent)]
    #[diagnostic(transparent)]
    TemplateError(#[from] rumoca_compile::codegen::CodegenError),

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
        source_map: Option<Box<SourceMap>>,
    },

    /// Structured source-backed diagnostics that should render directly in the CLI.
    #[error("{summary}")]
    #[diagnostic(code(rumoca::compiler::E013))]
    SourceDiagnosticsError {
        summary: String,
        diagnostics: Vec<CommonDiagnostic>,
        source_map: Box<SourceMap>,
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
