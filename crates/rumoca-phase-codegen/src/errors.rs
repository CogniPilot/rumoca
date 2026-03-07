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
}

impl CodegenError {
    /// Create a template error.
    pub fn template(message: impl Into<String>) -> Self {
        Self::TemplateError {
            message: message.into(),
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
                    message: format!("{err:#}"),
                    src: NamedSource::new(tmpl_name, source.to_string()),
                    span,
                };
            }
        }
        CodegenError::template(format!("{err:#}"))
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
        // Create a minijinja error with template context
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
            }
            CodegenError::TemplateError { .. } => {
                // Also acceptable if debug feature doesn't expose source
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
