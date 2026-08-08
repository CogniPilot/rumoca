//! Checked GALEC Algorithm Code and semantic eFMI package data.
//! (eFMI Standard 1.0.0 Beta 1, §3.2). See `spec/SPEC_0034_GALEC_EFMI_EXPORT.md`.
//!
//! This crate is pure checked export data (SPEC_0034 GAL-010): it has no
//! canonical IR, phase, template, or CLI dependencies.
//!
//! Module map:
//!
//! - [`ast`] — array-native GALEC AST (GAL-026) including the error-signal
//!   machinery (GAL-018); illegal shapes such as parameterized block methods
//!   (trap T1) or unary minus over non-references (trap T4) are
//!   unrepresentable;
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
pub mod lexical;
pub mod package;
pub mod validate;

pub use ast::{Block, BlockMethod, BlockMethodKind, Expression, PredefinedSignal, Statement};
pub use builtins::{BUILTINS, Builtin, is_reserved_name};
pub use diagnostic::{GalecError, Location, PathSegment};
pub use lexical::{is_legal_plain_identifier, plain_identifier_shape_error};
pub use validate::{SymbolInfo, span_of, symbol_at, validate};
