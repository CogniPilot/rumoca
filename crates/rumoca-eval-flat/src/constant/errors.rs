//! Evaluation errors for constant expression evaluation.

use rumoca_core::Span;
use thiserror::Error;

/// Why a constant evaluation left a value undetermined instead of proving the
/// expression wrong.
///
/// MLS §4.4 does not require every `parameter`/`constant` binding to fold at
/// translation time — a binding may only acquire its value during
/// initialization. A caller that folds bindings opportunistically therefore has
/// to distinguish "no value yet" from "this model is wrong", and that
/// distinction has to be a typed property of the failure rather than a decision
/// to ignore every failure.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeDependentReason {
    /// A value the expression reads is absent from the constant inventory.
    UnknownValue,
    /// The expression is proven to depend on something that is not constant.
    NotConstant,
    /// The constant evaluator implements no folding rule for this form, so the
    /// value stays for the runtime to establish.
    UnimplementedForm,
}

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

    /// Required source provenance was missing for an evaluation diagnostic.
    #[error("missing source context: {reason}")]
    MissingSourceContext { reason: String },

    /// Internal evaluation error
    #[error("internal evaluation error: {message}")]
    Internal { message: String },
}

impl EvalError {
    /// The typed reason this failure leaves the value for the runtime, or
    /// `None` when the failure proves the expression itself wrong.
    ///
    /// Callers that fold bindings opportunistically must surface every `None`:
    /// a type mismatch, a division by zero, an out-of-range index, a cyclic
    /// binding or a missing span is a defect in the model or in the compiler,
    /// not a value that initialization will supply.
    pub fn runtime_dependent_reason(&self) -> Option<RuntimeDependentReason> {
        match self {
            Self::UnknownVariable { .. } | Self::UnknownFunction { .. } => {
                Some(RuntimeDependentReason::UnknownValue)
            }
            Self::NotConstant { .. } => Some(RuntimeDependentReason::NotConstant),
            // `FunctionError` is the single channel the user-function
            // interpreter reports both unimplemented body forms and rejected
            // argument bindings through, so on its own it cannot prove a model
            // wrong. It is read as "this evaluator has no rule for that call"
            // until the two are separate variants.
            Self::UnsupportedExpression { .. } | Self::FunctionError { .. } => {
                Some(RuntimeDependentReason::UnimplementedForm)
            }
            Self::TypeMismatch { .. }
            | Self::DivisionByZero { .. }
            | Self::CircularDependency { .. }
            | Self::IndexOutOfBounds { .. }
            | Self::WrongArgCount { .. }
            | Self::FieldNotFound { .. }
            | Self::RangeError { .. }
            | Self::MissingSourceContext { .. }
            | Self::Internal { .. } => None,
        }
    }

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
            Self::MissingSourceContext { .. } | Self::Internal { .. } => None,
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

    /// Create a missing source context error.
    pub fn missing_source_context(reason: impl Into<String>) -> Self {
        Self::MissingSourceContext {
            reason: reason.into(),
        }
    }
}
