//! Evaluation errors for constant expression evaluation.

use rumoca_core::Span;
use thiserror::Error;

/// Errors that can occur during constant expression evaluation.
#[derive(Debug, Error)]
pub enum EvalError {
    /// Type mismatch during evaluation
    #[error("type mismatch: expected {expected}, got {actual}")]
    TypeMismatch {
        expected: String,
        actual: String,
        span: Span,
    },

    /// Division by zero
    #[error("division by zero")]
    DivisionByZero { span: Span },

    /// Unknown function
    #[error("unknown function: {name}")]
    UnknownFunction { name: String, span: Span },

    /// Unknown variable or constant
    #[error("unknown variable: {name}")]
    UnknownVariable { name: String, span: Span },

    /// Circular dependency detected during evaluation
    #[error("circular dependency detected: {path}")]
    CircularDependency { path: String, span: Span },

    /// Unsupported expression kind for constant evaluation
    #[error("unsupported expression for constant evaluation: {kind}")]
    UnsupportedExpression { kind: String, span: Span },

    /// Array index out of bounds
    #[error("array index out of bounds: index {index}, size {size}")]
    IndexOutOfBounds { index: i64, size: usize, span: Span },

    /// Wrong number of arguments to function
    #[error("wrong number of arguments: expected {expected}, got {actual}")]
    WrongArgCount {
        expected: usize,
        actual: usize,
        span: Span,
    },

    /// Function evaluation failed
    #[error("function evaluation failed: {message}")]
    FunctionError { message: String, span: Span },

    /// Record field not found
    #[error("record field not found: {field}")]
    FieldNotFound { field: String, span: Span },

    /// Not a constant expression
    #[error("expression is not constant: {reason}")]
    NotConstant { reason: String, span: Span },

    /// Range evaluation error
    #[error("range evaluation error: {message}")]
    RangeError { message: String, span: Span },

    /// Internal evaluation error
    #[error("internal evaluation error: {message}")]
    Internal { message: String },
}

impl EvalError {
    /// Get the span associated with this error, if any.
    pub fn span(&self) -> Option<Span> {
        match self {
            Self::TypeMismatch { span, .. }
            | Self::DivisionByZero { span }
            | Self::UnknownFunction { span, .. }
            | Self::UnknownVariable { span, .. }
            | Self::CircularDependency { span, .. }
            | Self::UnsupportedExpression { span, .. }
            | Self::IndexOutOfBounds { span, .. }
            | Self::WrongArgCount { span, .. }
            | Self::FunctionError { span, .. }
            | Self::FieldNotFound { span, .. }
            | Self::NotConstant { span, .. }
            | Self::RangeError { span, .. } => Some(*span),
            Self::Internal { .. } => None,
        }
    }

    /// Create a type mismatch error.
    pub fn type_mismatch(
        expected: impl Into<String>,
        actual: impl Into<String>,
        span: Span,
    ) -> Self {
        Self::TypeMismatch {
            expected: expected.into(),
            actual: actual.into(),
            span,
        }
    }

    /// Create an unknown variable error.
    pub fn unknown_variable(name: impl Into<String>, span: Span) -> Self {
        Self::UnknownVariable {
            name: name.into(),
            span,
        }
    }

    /// Create an unknown function error.
    pub fn unknown_function(name: impl Into<String>, span: Span) -> Self {
        Self::UnknownFunction {
            name: name.into(),
            span,
        }
    }

    /// Create a function error.
    pub fn function_error(message: impl Into<String>, span: Span) -> Self {
        Self::FunctionError {
            message: message.into(),
            span,
        }
    }

    /// Create a not constant error.
    pub fn not_constant(reason: impl Into<String>, span: Span) -> Self {
        Self::NotConstant {
            reason: reason.into(),
            span,
        }
    }

    /// Create a range error.
    pub fn range_error(message: impl Into<String>, span: Span) -> Self {
        Self::RangeError {
            message: message.into(),
            span,
        }
    }
}
