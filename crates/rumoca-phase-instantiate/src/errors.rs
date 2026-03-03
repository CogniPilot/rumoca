//! Phase-local error types for instantiation.
//!
//! Error codes: EI0xx for instantiate phase (per SPEC_0008).
//!
//! ## Error Code Allocation
//!
//! | Code | Error | MLS Reference |
//! |------|-------|---------------|
//! | EI001 | ModelNotFound | - |
//! | EI002 | InvalidModPath | §7.2 |
//! | EI003 | ModTypeMismatch | §7.2 |
//! | EI004 | StructuralParamError | §4.4.3 |
//! | EI005 | ArrayDimMismatch | §10 |
//! | EI006 | ConditionalError | §4.8 |
//! | EI007 | RedeclareError | §7.3 |
//! | EI008 | MissingInner | §5.4 |
//! | EI009 | InnerOuterTypeMismatch | §5.4 |
//! | EI010 | ConflictingInheritance | §5.6 |
//! | EI011 | ConflictingModifications | §5.6/§7.2 |
//! | EI012 | PartialClassInstantiation | §4.7 |
//! | EI014 | RedeclareNonReplaceable | §7.3 |
//! | EI027 | RedeclareConstraintViolation | §7.3.2 |
//! | EI028 | RedeclareFinal | §7.2.6 |
//! | EI029 | InvalidBreakName | §7.4 |
//!
//! Uses miette for rich diagnostic output with error codes and help text.

use miette::Diagnostic;
use rumoca_core::{BoxedResult, SourceSpan, Span, error_constructor};
use thiserror::Error;

/// Type alias for instantiation results with boxed errors.
pub type InstantiateResult<T> = BoxedResult<T, InstantiateError>;

/// Errors that can occur during instantiation.
#[derive(Debug, Clone, Error, Diagnostic)]
pub enum InstantiateError {
    /// Model not found.
    #[error("model `{0}` not found")]
    #[diagnostic(code(rumoca::instantiate::EI001))]
    ModelNotFound(String),

    /// Model not found with span.
    #[error("model `{name}` not found")]
    #[diagnostic(code(rumoca::instantiate::EI001))]
    ModelNotFoundWithSpan {
        name: String,
        #[label("referenced here")]
        span: SourceSpan,
    },

    /// Invalid modification path.
    #[error("invalid modification path `{path}`")]
    #[diagnostic(code(rumoca::instantiate::EI002))]
    InvalidModPath {
        path: String,
        #[label("invalid path")]
        span: SourceSpan,
    },

    /// Type mismatch in modification.
    #[error("modification type mismatch for `{path}`: expected `{expected}`, found `{found}`")]
    #[diagnostic(code(rumoca::instantiate::EI003))]
    ModTypeMismatch {
        path: String,
        expected: String,
        found: String,
        #[label("type mismatch here")]
        span: SourceSpan,
    },

    /// Cannot evaluate structural parameter.
    #[error("cannot evaluate structural parameter `{name}`: {msg}")]
    #[diagnostic(code(rumoca::instantiate::EI004))]
    StructuralParamError {
        name: String,
        msg: String,
        #[label("structural parameter")]
        span: SourceSpan,
    },

    /// Array dimension mismatch.
    #[error("array dimension mismatch for `{name}`: expected {expected}, found {found}")]
    #[diagnostic(code(rumoca::instantiate::EI005))]
    ArrayDimMismatch {
        name: String,
        expected: String,
        found: String,
        #[label("dimension mismatch")]
        span: SourceSpan,
    },

    /// Conditional component with non-parameter condition.
    #[error("conditional component `{name}` requires parameter expression")]
    #[diagnostic(code(rumoca::instantiate::EI006))]
    ConditionalError {
        name: String,
        #[label("conditional component")]
        span: SourceSpan,
    },

    /// Redeclaration error.
    #[error("redeclaration error for `{name}`: {msg}")]
    #[diagnostic(code(rumoca::instantiate::EI007))]
    RedeclareError {
        name: String,
        msg: String,
        #[label("redeclaration")]
        span: SourceSpan,
    },

    /// Redeclare non-replaceable element (MLS §7.3).
    #[error("cannot redeclare `{name}`: element is not replaceable")]
    #[diagnostic(
        code(rumoca::instantiate::EI014),
        help("MLS §7.3: only elements declared with 'replaceable' can be redeclared")
    )]
    RedeclareNonReplaceable {
        name: String,
        #[label("redeclare of non-replaceable element")]
        span: SourceSpan,
    },

    /// Redeclare violates constrainedby (MLS §7.3.2).
    #[error(
        "redeclaration of `{name}` violates constrainedby: `{new_type}` is not a subtype of `{constraint}`"
    )]
    #[diagnostic(
        code(rumoca::instantiate::EI027),
        help("MLS §7.3.2: the redeclared element must be a subtype of the constraining type")
    )]
    RedeclareConstraintViolation {
        name: String,
        new_type: String,
        constraint: String,
        #[label("constraint violation")]
        span: SourceSpan,
    },

    /// Redeclare final element (MLS §7.2.6).
    #[error("cannot redeclare `{name}`: element is declared final")]
    #[diagnostic(
        code(rumoca::instantiate::EI028),
        help("MLS §7.2.6: final elements cannot be modified or redeclared")
    )]
    RedeclareFinal {
        name: String,
        #[label("redeclare of final element")]
        span: SourceSpan,
    },

    /// Invalid break name in selective extension (MLS §7.4).
    #[error("break name `{name}` does not exist in base class `{base_class}`")]
    #[diagnostic(
        code(rumoca::instantiate::EI029),
        help("MLS §7.4: break can only reference elements that exist in the base class")
    )]
    InvalidBreakName {
        name: String,
        base_class: String,
        #[label("invalid break name")]
        span: SourceSpan,
    },

    /// Missing inner declaration for outer component (MLS §5.4).
    #[error("outer component `{name}` has no matching inner declaration")]
    #[diagnostic(
        code(rumoca::instantiate::EI008),
        help(
            "MLS §5.4: an outer element shall have a matching inner element in the enclosing scope"
        )
    )]
    MissingInner {
        name: String,
        #[label("outer component")]
        span: SourceSpan,
    },

    /// Inner/outer type mismatch (MLS §5.4).
    #[error(
        "inner/outer type mismatch for `{name}`: outer expects `{outer_type}`, inner provides `{inner_type}`"
    )]
    #[diagnostic(
        code(rumoca::instantiate::EI009),
        help("MLS §5.4: inner and outer elements must have compatible types")
    )]
    InnerOuterTypeMismatch {
        name: String,
        outer_type: String,
        inner_type: String,
        #[label("type mismatch")]
        span: SourceSpan,
    },

    /// Conflicting component definitions from multiple inheritance (MLS §5.6).
    #[error("conflicting definitions of `{name}` from `{base1}` and `{base2}`")]
    #[diagnostic(
        code(rumoca::instantiate::EI010),
        help("MLS §5.6: multiple inheritance cannot have conflicting component definitions")
    )]
    ConflictingInheritance {
        name: String,
        base1: String,
        base2: String,
        #[label("conflict here")]
        span: SourceSpan,
    },

    /// Conflicting modifications in diamond inheritance (MLS §5.6/§7.2).
    #[error(
        "conflicting modifications of `{name}` in diamond inheritance: `{base1}` sets `{value1}`, `{base2}` sets `{value2}`"
    )]
    #[diagnostic(
        code(rumoca::instantiate::EI011),
        help("MLS §5.6/§7.2: diamond inheritance with different modifications is ambiguous")
    )]
    ConflictingModifications {
        name: String,
        base1: String,
        base2: String,
        value1: String,
        value2: String,
        #[label("conflicting modification")]
        span: SourceSpan,
    },

    /// Illegal instantiation of a partial class in a non-partial model (MLS §4.7).
    #[error("cannot instantiate partial class `{class_name}` for component `{component_path}`")]
    #[diagnostic(
        code(rumoca::instantiate::EI012),
        help("MLS §4.7: partial classes cannot be instantiated in non-partial simulation models")
    )]
    PartialClassInstantiation {
        component_path: String,
        class_name: String,
        #[label("partial class instantiation")]
        span: SourceSpan,
    },
}

impl InstantiateError {
    // Constructor methods using the error_constructor! macro
    error_constructor!(
        model_not_found_with_span,
        ModelNotFoundWithSpan { name: String }
    );
    error_constructor!(invalid_mod_path, InvalidModPath { path: String });
    error_constructor!(
        mod_type_mismatch,
        ModTypeMismatch {
            path: String,
            expected: String,
            found: String
        }
    );
    error_constructor!(
        structural_param_error,
        StructuralParamError {
            name: String,
            msg: String
        }
    );
    error_constructor!(
        array_dim_mismatch,
        ArrayDimMismatch {
            name: String,
            expected: String,
            found: String
        }
    );
    error_constructor!(conditional_error, ConditionalError { name: String });
    error_constructor!(
        redeclare_error,
        RedeclareError {
            name: String,
            msg: String
        }
    );
    error_constructor!(
        redeclare_non_replaceable,
        RedeclareNonReplaceable { name: String }
    );
    error_constructor!(
        redeclare_constraint_violation,
        RedeclareConstraintViolation {
            name: String,
            new_type: String,
            constraint: String
        }
    );
    error_constructor!(redeclare_final, RedeclareFinal { name: String });
    error_constructor!(
        invalid_break_name,
        InvalidBreakName {
            name: String,
            base_class: String
        }
    );
    error_constructor!(missing_inner, MissingInner { name: String });
    error_constructor!(
        inner_outer_type_mismatch,
        InnerOuterTypeMismatch {
            name: String,
            outer_type: String,
            inner_type: String
        }
    );
    error_constructor!(
        conflicting_inheritance,
        ConflictingInheritance {
            name: String,
            base1: String,
            base2: String
        }
    );
    error_constructor!(
        conflicting_modifications,
        ConflictingModifications {
            name: String,
            base1: String,
            base2: String,
            value1: String,
            value2: String
        }
    );
    error_constructor!(
        partial_class_instantiation,
        PartialClassInstantiation {
            component_path: String,
            class_name: String
        }
    );
}

/// Outcome of model instantiation.
///
/// Separates "needs inner declarations" from "actual compilation error"
/// to correctly handle partial models per MLS §5.4.
///
/// MLS §5.4: Models with `outer` components need matching `inner` declarations
/// from an enclosing scope. These are not failures - they're models designed
/// to be used within systems that provide the inner declarations.
#[derive(Debug)]
pub enum InstantiationOutcome {
    /// Model instantiated successfully.
    Success(rumoca_ir_ast::InstanceOverlay),

    /// Model has `outer` components without matching `inner` declarations.
    /// These models need to be used within an enclosing system that provides
    /// the inner declarations. Not a failure - just needs inner declarations.
    NeedsInner {
        /// Names of outer components that need inner declarations.
        missing_inners: Vec<String>,
        /// Source spans for each missing outer component declaration.
        missing_spans: Vec<Span>,
        /// Partial instantiation result (may still be useful for analysis).
        partial_overlay: rumoca_ir_ast::InstanceOverlay,
    },

    /// Actual instantiation error (not context-dependent).
    Error(Box<InstantiateError>),
}

impl InstantiationOutcome {
    /// Returns true if this is a successful instantiation.
    pub fn is_success(&self) -> bool {
        matches!(self, Self::Success(_))
    }

    /// Returns true if this model needs inner declarations (has outer without inner).
    pub fn needs_inner(&self) -> bool {
        matches!(self, Self::NeedsInner { .. })
    }

    /// Returns true if this is an actual error (not context-dependent).
    pub fn is_error(&self) -> bool {
        matches!(self, Self::Error(_))
    }

    /// Get the overlay if successful or partially instantiated.
    pub fn overlay(&self) -> Option<&rumoca_ir_ast::InstanceOverlay> {
        match self {
            Self::Success(o) => Some(o),
            Self::NeedsInner {
                partial_overlay, ..
            } => Some(partial_overlay),
            Self::Error(_) => None,
        }
    }

    /// Convert to Result, treating NeedsInner as an error for compatibility.
    /// Use this when you need the old behavior of treating missing inners as errors.
    pub fn into_result(self) -> InstantiateResult<rumoca_ir_ast::InstanceOverlay> {
        match self {
            Self::Success(overlay) => Ok(overlay),
            Self::NeedsInner { missing_inners, .. } => {
                let names = missing_inners.join(", ");
                Err(Box::new(InstantiateError::MissingInner {
                    name: names,
                    span: (0_usize, 0_usize).into(),
                }))
            }
            Self::Error(e) => Err(e),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{SourceId, Span};

    #[test]
    fn test_model_not_found_error() {
        let err = InstantiateError::ModelNotFound("TestModel".to_string());
        assert_eq!(format!("{err}"), "model `TestModel` not found");
    }

    #[test]
    fn test_model_not_found_with_span() {
        let span = Span::from_offsets(SourceId(0), 10, 20);
        let err = InstantiateError::model_not_found_with_span("TestModel", span);
        assert_eq!(format!("{err}"), "model `TestModel` not found");

        // Check that miette code is present
        use miette::Diagnostic;
        let code = err.code().map(|c| c.to_string());
        assert_eq!(code, Some("rumoca::instantiate::EI001".to_string()));
    }

    #[test]
    fn test_conflicting_inheritance_with_help() {
        let span = Span::from_offsets(SourceId(0), 0, 10);
        let err = InstantiateError::conflicting_inheritance("x", "Base1", "Base2", span);

        // Check that help text is present
        use miette::Diagnostic;
        let help = err.help().map(|h| h.to_string());
        assert!(help.is_some());
        assert!(help.unwrap().contains("MLS §5.6"));
    }
}
