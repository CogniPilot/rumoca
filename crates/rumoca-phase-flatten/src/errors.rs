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

    /// A function call binds its argument slots incorrectly (MLS §12.4.1).
    #[error("invalid call of function `{function}`: {reason}")]
    #[diagnostic(
        code(rumoca::flatten::EF016),
        help(
            "MLS §12.4.1: each input slot is filled by exactly one positional or named argument; slots without defaults must be filled"
        )
    )]
    InvalidFunctionCallArgs {
        function: String,
        reason: String,
        #[label("function call here")]
        span: SourceSpan,
    },

    /// A record constructor was requested for a record with conditional
    /// components (MLS §12.6.1 / FUNC-029).
    #[error(
        "record `{record}` has conditional component `{component}` and cannot have a constructor"
    )]
    #[diagnostic(
        code(rumoca::flatten::EF018),
        help("MLS §12.6.1: records with conditional components have no record constructor")
    )]
    ConditionalComponentConstructor {
        record: String,
        component: String,
        #[label("record constructed here")]
        span: SourceSpan,
    },
    // Note: EF006 was EventTriggerOutsideWhen, removed per MLS Appendix B which
    // allows edge()/change() in discrete equations. Code reserved for future use.
    // Note: EF007 (UnevaluableDimensions) removed - typecheck phase (ET004) now handles this
    // per SPEC_0027 which moved dimension evaluation to typecheck phase.
    /// Source-scope metadata required for Modelica name lookup was missing.
    #[error("missing source scope for {context}: {name}")]
    #[diagnostic(
        code(rumoca::flatten::EF008),
        help(
            "instantiate must preserve the lexical source scope used for flatten-time name lookup"
        )
    )]
    MissingSourceScope {
        name: String,
        context: String,
        #[label("instance created here")]
        span: SourceSpan,
    },

    /// Flat IR contains a callable definition that is not executable.
    #[error("invalid flat IR function binding: {name}")]
    #[diagnostic(
        code(rumoca::flatten::EF009),
        help(
            "flatten must resolve replaceable package/function bindings to executable concrete functions before producing Flat IR"
        )
    )]
    FunctionWithoutBody {
        name: String,
        #[label("non-executable function reached the flat IR boundary")]
        span: SourceSpan,
    },

    /// A primitive component reached Flat IR with a symbolic dimension that could not be resolved.
    #[error("unresolved component dimension for {name}: {expression}")]
    #[diagnostic(
        code(rumoca::flatten::EF010),
        help("flatten must resolve primitive component array dimensions before emitting Flat IR")
    )]
    UnresolvedComponentDimension {
        name: String,
        expression: String,
        #[label("dimension declared here")]
        span: SourceSpan,
    },

    /// A numeric token accepted by the parser could not be converted to a number.
    #[error("malformed numeric literal: {text}")]
    #[diagnostic(
        code(rumoca::flatten::EF011),
        help("the lexer produced a numeric token that flatten could not parse")
    )]
    MalformedNumericLiteral {
        text: String,
        #[label("malformed numeric literal")]
        span: SourceSpan,
    },

    /// Function-override rewriting did not converge within the configured fixed-point cap.
    #[error(
        "function override rewriting did not converge after {iterations} iterations ({function_count} functions collected)"
    )]
    #[diagnostic(
        code(rumoca::flatten::EF012),
        help("flatten must reach a stable rewritten function table before emitting Flat IR")
    )]
    FunctionRewriteNoConverge {
        iterations: usize,
        function_count: usize,
    },

    /// A function output can be returned without an assignment.
    #[error("function output '{output}' is not definitely assigned in function '{function}'")]
    #[diagnostic(
        code(rumoca::flatten::EF013),
        help("MLS §12.4.4 requires every function output variable to be assigned before return")
    )]
    FunctionOutputUnassigned {
        function: String,
        output: String,
        #[label("function output may be unassigned here")]
        span: SourceSpan,
    },

    /// A variable reached flattening without a resolvable type name.
    #[error("unresolved variable type for `{name}`")]
    #[diagnostic(
        code(rumoca::flatten::EF014),
        help(
            "instantiate/typecheck must preserve a structured type id or type name before flattening variables"
        )
    )]
    UnresolvedVariableType {
        name: String,
        #[label("variable declared here")]
        span: SourceSpan,
    },

    /// A resolved class reached flattening without the DefId metadata required
    /// for scope-based lookup.
    #[error("missing resolved class metadata for `{name}` ({context})")]
    #[diagnostic(
        code(rumoca::flatten::EF015),
        help("name resolution must assign and preserve DefId metadata before flattening")
    )]
    MissingResolvedClassMetadata {
        name: String,
        context: String,
        #[label("class used here")]
        span: SourceSpan,
    },

    /// Source location metadata could not be mapped to the source text needed
    /// for a diagnostic span.
    #[error("missing source context: {reason}")]
    #[diagnostic(
        code(rumoca::flatten::EF017),
        help("earlier phases must preserve source-map entries and non-empty source locations")
    )]
    MissingSourceContext { reason: String },

    /// A function reference's display name disagreed with its structured path.
    #[error(
        "inconsistent resolved function reference: rendered `{rendered}`, structured `{structured}`"
    )]
    #[diagnostic(
        code(rumoca::flatten::EF019),
        help("name resolution must preserve one authoritative function exposure identity")
    )]
    InconsistentFunctionReference {
        rendered: String,
        structured: String,
        #[label("conflicting function reference")]
        span: SourceSpan,
    },
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

    /// Create a MissingSourceScope error.
    pub fn missing_source_scope(
        name: impl Into<String>,
        context: impl Into<String>,
        span: rumoca_core::Span,
    ) -> Self {
        Self::MissingSourceScope {
            name: name.into(),
            context: context.into(),
            span: rumoca_core::span_to_source_span(span),
        }
    }

    pub fn inconsistent_function_reference(
        rendered: impl Into<String>,
        structured: impl Into<String>,
        span: rumoca_core::Span,
    ) -> Self {
        Self::InconsistentFunctionReference {
            rendered: rendered.into(),
            structured: structured.into(),
            span: rumoca_core::span_to_source_span(span),
        }
    }

    /// Create an InvalidFunctionCallArgs error.
    pub fn invalid_function_call_args(
        function: impl Into<String>,
        reason: impl Into<String>,
        span: rumoca_core::Span,
    ) -> Self {
        Self::InvalidFunctionCallArgs {
            function: function.into(),
            reason: reason.into(),
            span: rumoca_core::span_to_source_span(span),
        }
    }

    /// Create a FunctionWithoutBody error.
    pub fn function_without_body(name: impl Into<String>, span: rumoca_core::Span) -> Self {
        Self::FunctionWithoutBody {
            name: name.into(),
            span: rumoca_core::span_to_source_span(span),
        }
    }

    /// Create an UnresolvedComponentDimension error.
    pub fn unresolved_component_dimension(
        name: impl Into<String>,
        expression: impl Into<String>,
        span: rumoca_core::Span,
    ) -> Self {
        Self::UnresolvedComponentDimension {
            name: name.into(),
            expression: expression.into(),
            span: rumoca_core::span_to_source_span(span),
        }
    }

    /// Create a MalformedNumericLiteral error.
    pub fn malformed_numeric_literal(text: impl Into<String>, span: rumoca_core::Span) -> Self {
        Self::MalformedNumericLiteral {
            text: text.into(),
            span: rumoca_core::span_to_source_span(span),
        }
    }

    /// Create a FunctionRewriteNoConverge error.
    pub fn function_rewrite_no_converge(iterations: usize, function_count: usize) -> Self {
        Self::FunctionRewriteNoConverge {
            iterations,
            function_count,
        }
    }

    /// Create a FunctionOutputUnassigned error.
    pub fn function_output_unassigned(
        function: impl Into<String>,
        output: impl Into<String>,
        span: rumoca_core::Span,
    ) -> Self {
        Self::FunctionOutputUnassigned {
            function: function.into(),
            output: output.into(),
            span: rumoca_core::span_to_source_span(span),
        }
    }

    /// Create an UnresolvedVariableType error.
    pub fn unresolved_variable_type(name: impl Into<String>, span: rumoca_core::Span) -> Self {
        Self::UnresolvedVariableType {
            name: name.into(),
            span: rumoca_core::span_to_source_span(span),
        }
    }

    /// Create a MissingResolvedClassMetadata error.
    pub fn missing_resolved_class_metadata(
        name: impl Into<String>,
        context: impl Into<String>,
        span: rumoca_core::Span,
    ) -> Self {
        Self::MissingResolvedClassMetadata {
            name: name.into(),
            context: context.into(),
            span: rumoca_core::span_to_source_span(span),
        }
    }

    /// Create a MissingSourceContext error.
    pub fn missing_source_context(reason: impl Into<String>) -> Self {
        Self::MissingSourceContext {
            reason: reason.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{SourceId, Span};

    #[test]
    fn test_undefined_variable_error() {
        let span = Span::from_offsets(
            SourceId::from_source_name("phase_flatten_errors_source_0.mo"),
            10,
            20,
        );
        let err = FlattenError::undefined_variable("x", span);
        assert_eq!(format!("{err}"), "undefined variable: x");

        // Check that miette code is present
        use miette::Diagnostic;
        let code = err.code().map(|c| c.to_string());
        assert_eq!(code, Some("rumoca::flatten::EF001".to_string()));
    }

    #[test]
    fn test_incompatible_connectors_with_help() {
        let span = Span::from_offsets(
            SourceId::from_source_name("phase_flatten_errors_source_0.mo"),
            0,
            10,
        );
        let err = FlattenError::incompatible_connectors("A", "B", span);

        // Check that help text is present
        use miette::Diagnostic;
        let help = err.help().map(|h| h.to_string());
        assert!(help.is_some());
        assert!(help.unwrap().contains("MLS §9.1"));
    }
}
