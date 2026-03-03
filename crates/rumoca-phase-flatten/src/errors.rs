//! Phase-local error types for flattening.
//!
//! Error codes: EF0xx for flatten phase (per SPEC_0008).
//!
//! Uses miette for rich diagnostic output with error codes and help text.

use miette::Diagnostic;
use rumoca_core::{BoxedResult, SourceSpan, error_constructor};
use thiserror::Error;

/// Type alias for flatten results with boxed errors.
pub type FlattenResult<T> = BoxedResult<T, FlattenError>;

/// Errors that can occur during flattening.
#[derive(Debug, Clone, Error, Diagnostic)]
pub enum FlattenError {
    /// A variable was referenced but not found.
    #[error("undefined variable: {name}")]
    #[diagnostic(
        code(rumoca::flatten::EF001),
        help("check that the variable is declared in scope")
    )]
    UndefinedVariable {
        name: String,
        #[label("referenced here")]
        span: SourceSpan,
    },

    /// A connection involves incompatible connector types.
    #[error("incompatible connector types in connection: {a} and {b}")]
    #[diagnostic(
        code(rumoca::flatten::EF002),
        help("MLS §9.1: connected components must have compatible connector types")
    )]
    IncompatibleConnectors {
        a: String,
        b: String,
        #[label("connection here")]
        span: SourceSpan,
    },

    /// A flow variable is missing in a connector.
    #[error("flow variable not found in connector: {connector}.{flow_var}")]
    #[diagnostic(
        code(rumoca::flatten::EF003),
        help("MLS §9.2: flow variables must be declared in connectors")
    )]
    MissingFlowVariable { connector: String, flow_var: String },

    /// Unsupported equation form.
    #[error("unsupported equation form: {description}")]
    #[diagnostic(code(rumoca::flatten::EF004))]
    UnsupportedEquation {
        description: String,
        #[label("unsupported equation")]
        span: SourceSpan,
    },

    /// Internal error during flattening.
    #[error("internal flatten error: {0}")]
    #[diagnostic(code(rumoca::flatten::EF005))]
    Internal(String),
    // Note: EF006 was EventTriggerOutsideWhen, removed per MLS Appendix B which
    // allows edge()/change() in discrete equations. Code reserved for future use.
    // Note: EF007 (UnevaluableDimensions) removed - typecheck phase (ET004) now handles this
    // per SPEC_0027 which moved dimension evaluation to typecheck phase.
}

impl FlattenError {
    // Constructor methods using the error_constructor! macro
    error_constructor!(undefined_variable, UndefinedVariable { name: String });
    error_constructor!(
        incompatible_connectors,
        IncompatibleConnectors {
            a: String,
            b: String
        }
    );
    error_constructor!(
        unsupported_equation,
        UnsupportedEquation {
            description: String
        }
    );

    /// Create a MissingFlowVariable error (no span).
    pub fn missing_flow_variable(
        connector: impl Into<String>,
        flow_var: impl Into<String>,
    ) -> Self {
        Self::MissingFlowVariable {
            connector: connector.into(),
            flow_var: flow_var.into(),
        }
    }

    /// Create an Internal error (no span).
    pub fn internal(message: impl Into<String>) -> Self {
        Self::Internal(message.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{SourceId, Span};

    #[test]
    fn test_undefined_variable_error() {
        let span = Span::from_offsets(SourceId(0), 10, 20);
        let err = FlattenError::undefined_variable("x", span);
        assert_eq!(format!("{err}"), "undefined variable: x");

        // Check that miette code is present
        use miette::Diagnostic;
        let code = err.code().map(|c| c.to_string());
        assert_eq!(code, Some("rumoca::flatten::EF001".to_string()));
    }

    #[test]
    fn test_incompatible_connectors_with_help() {
        let span = Span::from_offsets(SourceId(0), 0, 10);
        let err = FlattenError::incompatible_connectors("A", "B", span);

        // Check that help text is present
        use miette::Diagnostic;
        let help = err.help().map(|h| h.to_string());
        assert!(help.is_some());
        assert!(help.unwrap().contains("MLS §9.1"));
    }
}
