//! Error types for code generation.
//!
//! Error codes: EC0xx for codegen phase (per SPEC_0008).

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

/// Errors that can occur during code generation.
#[derive(Debug, Clone, Error, Diagnostic)]
pub enum CodegenError {
    /// Template rendering failed.
    #[error("template error: {message}")]
    #[diagnostic(
        code(rumoca::codegen::EC001),
        help("check template syntax and variable names")
    )]
    TemplateError { message: String },

    /// Template rendering failed with source context.
    #[error("template render error: {message}")]
    #[diagnostic(
        code(rumoca::codegen::EC002),
        help("check the expression or equation being rendered")
    )]
    TemplateRenderError {
        message: String,
        #[source_code]
        src: NamedSource<String>,
        #[label("error here")]
        span: SourceSpan,
    },

    /// IR serialization to JSON failed.
    #[error("IR serialization failed: {message}")]
    #[diagnostic(
        code(rumoca::codegen::EC003),
        help("the IR contains a type that cannot be serialized to JSON")
    )]
    SerializationFailed { message: String },

    // `EC004` was `ExternalFunctionNotCallable`. External functions are
    // refused by target capability gating before any rendering starts, so this
    // phase never reached a call it could not emit and the variant was
    // constructed only by its own test. The code is left unreused: a retired
    // diagnostic code that comes back meaning something else is worse than a
    // gap.
    /// Solve-IR scalar fallback generation failed.
    #[error("Solve-IR scalarization failed: {message}")]
    #[diagnostic(
        code(rumoca::codegen::EC005),
        help("check tensor native-family metadata emitted by solve lowering")
    )]
    SolveScalarizationFailed {
        message: String,
        span: Option<rumoca_core::Span>,
    },

    /// DAE preparation for a template-specific projection failed.
    #[error("DAE preparation failed: {message}")]
    #[diagnostic(
        code(rumoca::codegen::EC006),
        help("check source metadata used by the selected template target")
    )]
    DaePreparationFailed {
        message: String,
        span: Option<rumoca_core::Span>,
    },

    /// A template requested scalar equation rows whose authoritative body
    /// exists only in a compact structured family.
    #[error("scalar equation view is unavailable for structured family `{origin}` in {partition}")]
    #[diagnostic(
        code(rumoca::codegen::EC007),
        help(
            "select a target that consumes structured equation families, or lower through DAE/Solve IR"
        )
    )]
    NonMaterializedStructuredFamily {
        partition: &'static str,
        origin: String,
        span: Option<rumoca_core::Span>,
    },

    /// A target declared structured-family support, but the canonical family
    /// metadata is incomplete or inconsistent with its DAE partition.
    #[error("invalid structured-family ownership for `{origin}` in {partition}: {reason}")]
    #[diagnostic(
        code(rumoca::codegen::EC008),
        help("fix the DAE producer; code generation cannot infer missing family semantics")
    )]
    InvalidStructuredFamilyOwnership {
        partition: &'static str,
        origin: String,
        reason: String,
        span: Option<rumoca_core::Span>,
    },

    /// The selected target explicitly excludes a source-model capability.
    #[error(
        "unsupported-feature:{feature}: Target '{target}' does not support feature '{feature}': {detail}"
    )]
    #[diagnostic(
        code(rumoca::codegen::EC009),
        help("select a target whose checked capability profile admits this source feature")
    )]
    UnsupportedTargetFeature {
        target: String,
        feature: &'static str,
        detail: String,
        span: Option<rumoca_core::Span>,
    },
}

impl CodegenError {
    /// Create a template error.
    pub fn template(message: impl Into<String>) -> Self {
        Self::TemplateError {
            message: message.into(),
        }
    }

    /// Create a template diagnostic anchored to model source.
    pub(crate) fn template_render_at(
        message: impl Into<String>,
        source_name: impl Into<String>,
        source: impl Into<String>,
        span: rumoca_core::Span,
    ) -> Self {
        let source_name = source_name.into();
        Self::TemplateRenderError {
            message: message.into(),
            src: NamedSource::new(source_name, source.into()),
            span: SourceSpan::new(span.start.0.into(), span.end.0.saturating_sub(span.start.0)),
        }
    }

    pub fn dae_preparation_failed(
        message: impl Into<String>,
        span: Option<rumoca_core::Span>,
    ) -> Self {
        Self::DaePreparationFailed {
            message: message.into(),
            span,
        }
    }

    /// Construct a typed target-capability refusal before any artifact exists.
    pub fn unsupported_target_feature(
        target: impl Into<String>,
        feature: &'static str,
        detail: impl Into<String>,
        span: Option<rumoca_core::Span>,
    ) -> Self {
        Self::UnsupportedTargetFeature {
            target: target.into(),
            feature,
            detail: detail.into(),
            span,
        }
    }
}

/// Create a `minijinja::Error` from a message string.
///
/// Use this inside render functions that return `Result<String, minijinja::Error>`.
pub(crate) fn render_err(msg: impl std::fmt::Display) -> minijinja::Error {
    minijinja::Error::new(minijinja::ErrorKind::InvalidOperation, format!("{msg}"))
}

/// Compute the byte offset span for a 1-based line number in source text.
fn compute_line_span(source: &str, line: usize) -> SourceSpan {
    let mut offset = 0;
    for (i, text_line) in source.lines().enumerate() {
        if i + 1 == line {
            return SourceSpan::new(offset.into(), text_line.len());
        }
        offset += text_line.len() + 1; // +1 for newline
    }
    // Fallback: highlight the start
    SourceSpan::new(0.into(), 0)
}

impl From<minijinja::Error> for CodegenError {
    fn from(err: minijinja::Error) -> Self {
        // Try to extract template source context for rich diagnostics
        if let Some(line) = err.line() {
            let tmpl_name = err.name().unwrap_or("<inline>");
            if let Some(source) = err.template_source() {
                let span = compute_line_span(source, line);
                return CodegenError::TemplateRenderError {
                    // The alternate MiniJinja formatter appends the complete
                    // serialized render context. For compiler IR this can be
                    // tens of megabytes and belongs neither in diagnostics nor
                    // worker protocol rows; source and span are retained
                    // separately below.
                    message: err.to_string(),
                    src: NamedSource::new(tmpl_name, source.to_string()),
                    span,
                };
            }
        }
        CodegenError::template(err.to_string())
    }
}

impl From<rumoca_eval_solve::ScalarizeError> for CodegenError {
    fn from(err: rumoca_eval_solve::ScalarizeError) -> Self {
        Self::SolveScalarizationFailed {
            message: err.to_string(),
            span: err.source_span(),
        }
    }
}

impl From<rumoca_ir_solve::SolveProblemShapeContractError> for CodegenError {
    fn from(err: rumoca_ir_solve::SolveProblemShapeContractError) -> Self {
        Self::SolveScalarizationFailed {
            message: err.to_string(),
            span: err.source_span(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_template_error() {
        let err = CodegenError::template("missing variable");
        assert!(format!("{err}").contains("template error"));

        use miette::Diagnostic;
        let code = err.code().map(|c| c.to_string());
        assert_eq!(code, Some("rumoca::codegen::EC001".to_string()));
    }

    #[test]
    fn test_render_err_helper() {
        let e = render_err("something went wrong");
        assert!(e.to_string().contains("something went wrong"));
    }

    #[test]
    fn test_compute_line_span() {
        let src = "line1\nline2\nline3";
        let span = compute_line_span(src, 2);
        assert_eq!(span.offset(), 6); // "line1\n" = 6 bytes
        assert_eq!(span.len(), 5); // "line2" = 5 bytes
    }

    #[test]
    fn test_template_render_error_from_minijinja() {
        let mut env = minijinja::Environment::new();
        env.add_template("test.jinja", "{{ undefined_var.foo }}")
            .unwrap();
        let tmpl = env.get_template("test.jinja").unwrap();
        let err = tmpl.render(minijinja::context! {}).unwrap_err();
        let codegen_err: CodegenError = err.into();

        // Should produce a TemplateRenderError with source context
        match &codegen_err {
            CodegenError::TemplateRenderError { message, .. } => {
                assert!(!message.is_empty());
                assert!(message.len() < 1024);
                assert!(!message.contains("Referenced variables:"));
            }
            CodegenError::TemplateError { .. } => {
                // Also acceptable if debug feature doesn't expose source
            }
            CodegenError::SerializationFailed { .. } => {
                unreachable!(
                    "From<minijinja::Error> only constructs template errors, never serialization errors"
                );
            }
            CodegenError::SolveScalarizationFailed { .. } => {
                unreachable!(
                    "From<minijinja::Error> only constructs template errors, never scalarization errors"
                );
            }
            CodegenError::DaePreparationFailed { .. } => {
                unreachable!(
                    "From<minijinja::Error> only constructs template errors, never DAE preparation errors"
                );
            }
            CodegenError::NonMaterializedStructuredFamily { .. } => {
                unreachable!(
                    "From<minijinja::Error> only constructs template errors, never structured-family errors"
                );
            }
            CodegenError::InvalidStructuredFamilyOwnership { .. } => {
                unreachable!(
                    "From<minijinja::Error> only constructs template errors, never structured-family ownership errors"
                );
            }
            CodegenError::UnsupportedTargetFeature { .. } => {
                unreachable!(
                    "From<minijinja::Error> only constructs template errors, never target-capability errors"
                );
            }
        }

        use miette::Diagnostic;
        let code = codegen_err.code().map(|c| c.to_string());
        assert!(
            code == Some("rumoca::codegen::EC001".to_string())
                || code == Some("rumoca::codegen::EC002".to_string())
        );
    }
}
