//! GALEC language module: array-native AST, conformant printer, builtin
//! catalog, and diagnostics for the eFMI Algorithm Code language
//! (eFMI Standard 1.0.0 Beta 1, §3.2). See `spec/SPEC_0034_GALEC_EFMI_EXPORT.md`.
//!
//! This crate is a pure language module (SPEC_0034 GAL-010): it has no
//! Rumoca IR, eFMI packaging, phase, or CLI dependencies. The projection
//! from canonical DAE lives in `rumoca-galec-codegen`; container packaging
//! lives in `rumoca-galec-codegen`'s `manifest_context`.
//!
//! Module map:
//!
//! - [`ast`] — array-native GALEC AST (GAL-026) including the error-signal
//!   machinery (GAL-018); illegal shapes such as parameterized block methods
//!   (trap T1) or unary minus over non-references (trap T4) are
//!   unrepresentable;
//! - [`mod@print`] — `.alg` text printer implementing GAL-019 (traps T4–T7,
//!   T12);
//! - [`builtins`] — the §3.2.6 builtin catalog as data plus Appendix C
//!   reserved names and keyword lists (parity source per GAL-005, collision
//!   surface per GAL-015);
//! - [`mod@validate`] — the six-analysis validator (name / type /
//!   dimensionality / termination / side-effect / signals, SPEC_0034
//!   Validator Scope + GAL-018 escape-set dataflow), collect-all;
//! - [`diagnostic`] — SPEC_0008-shaped errors with stable `EG0xx` codes and
//!   structural AST-path locations (GALEC ASTs are generated, not parsed).

pub mod ast;
pub mod builtins;
pub mod diagnostic;
#[cfg(feature = "parse")]
pub mod parse;
pub mod print;
pub mod validate;

// Re-exports the parol-generated parser expects at crate root (mirrors
// `rumoca-phase-parse`: the generated parser imports `crate::grammar::<T>` and
// `crate::grammar_trait::<T>Auto`, see `user_trait_module_name("grammar")`).
#[cfg(feature = "parse")]
mod grammar {
    pub(crate) use crate::parse::GalecGrammar;
}
#[cfg(feature = "parse")]
pub use parse::generated::galec_grammar_trait as grammar_trait;

pub use ast::{Block, BlockMethod, BlockMethodKind, Expression, PredefinedSignal, Statement};
pub use builtins::{BUILTINS, Builtin, is_reserved_name};
pub use diagnostic::{GalecError, Location, PathSegment};
pub use print::{format_real_literal, is_conformant_real_literal, print_block, print_expression};
pub use validate::{span_of, validate};
