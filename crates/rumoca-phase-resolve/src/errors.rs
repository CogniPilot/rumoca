//! Error types for name resolution phase.

use miette::{Diagnostic as MietteDiagnostic, SourceSpan};
use rumoca_core::Span;
use thiserror::Error;

/// Type alias for resolve results with boxed errors.
///
/// Boxing the error type avoids clippy::result_large_err warnings while
/// preserving rich diagnostic information. The error path is cold (errors
/// are exceptional), so the allocation overhead is negligible.
pub type ResolveResult<T> = Result<T, Box<ResolveError>>;

/// Errors that can occur during name resolution.
#[derive(Debug, Clone, Error, MietteDiagnostic)]
pub enum ResolveError {
    /// A name was defined multiple times in the same scope.
    #[error("duplicate definition: `{name}` is already defined in this scope")]
    #[diagnostic(
        code(rumoca::resolve::ER001),
        help("MLS §5.3: Each name must be unique within its scope")
    )]
    DuplicateDefinition {
        name: String,
        #[label("duplicate definition here")]
        span: SourceSpan,
    },

    /// A name was referenced but not found.
    #[error("undefined reference: `{name}` not found")]
    #[diagnostic(
        code(rumoca::resolve::ER002),
        help("check that the name is declared before use")
    )]
    UndefinedReference {
        name: String,
        #[label("not found in scope")]
        span: SourceSpan,
    },

    /// Base class in extends clause not found.
    #[error("base class not found: `{name}` does not exist")]
    #[diagnostic(
        code(rumoca::resolve::ER003),
        help("MLS §7.1: The base class must be defined before it can be extended")
    )]
    BaseClassNotFound {
        name: String,
        #[label("base class not found")]
        span: SourceSpan,
    },

    /// Circular inheritance detected.
    #[error("circular inheritance: `{name}` extends itself (directly or indirectly)")]
    #[diagnostic(
        code(rumoca::resolve::ER004),
        help("MLS §7.1: A class cannot extend itself or create circular inheritance chains")
    )]
    CircularInheritance {
        name: String,
        #[label("circular extends chain")]
        span: SourceSpan,
    },
}

impl ResolveError {
    /// Create a DuplicateDefinition error.
    pub fn duplicate_definition(name: impl Into<String>, span: Span) -> Self {
        Self::DuplicateDefinition {
            name: name.into(),
            span: span.to_source_span(),
        }
    }

    /// Create an UndefinedReference error.
    pub fn undefined_reference(name: impl Into<String>, span: Span) -> Self {
        Self::UndefinedReference {
            name: name.into(),
            span: span.to_source_span(),
        }
    }

    /// Create a BaseClassNotFound error.
    pub fn base_class_not_found(name: impl Into<String>, span: Span) -> Self {
        Self::BaseClassNotFound {
            name: name.into(),
            span: span.to_source_span(),
        }
    }

    /// Create a CircularInheritance error.
    pub fn circular_inheritance(name: impl Into<String>, span: Span) -> Self {
        Self::CircularInheritance {
            name: name.into(),
            span: span.to_source_span(),
        }
    }
}
