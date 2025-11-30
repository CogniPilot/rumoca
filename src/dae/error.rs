use thiserror::Error;

/// Errors that can occur during DAE (Differential-Algebraic Equation) operations
#[derive(Error, Debug)]
pub enum DaeError {
    #[error("Template rendering failed: {0}")]
    TemplateRenderError(String),

    #[error("Template file not found: {0}")]
    TemplateFileNotFound(String),
}
