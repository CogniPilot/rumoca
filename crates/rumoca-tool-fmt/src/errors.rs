//! Formatter error types.

use std::fmt;

/// Error that can occur during formatting.
#[derive(Debug, Clone)]
pub enum FormatError {
    /// The source code has syntax errors.
    SyntaxError(String),
}

impl fmt::Display for FormatError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FormatError::SyntaxError(msg) => write!(f, "syntax error: {}", msg),
        }
    }
}

impl std::error::Error for FormatError {}
