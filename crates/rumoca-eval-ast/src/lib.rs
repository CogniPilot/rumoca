//! AST-level evaluation interfaces for Rumoca.
//!
//! This crate defines shared AST-facing evaluation contracts.
//! It is intentionally small and stable so phases can depend on a
//! single AST evaluation surface without pulling in Flat/DAE runtimes.

pub use rumoca_core::EvalLookup;
pub use rumoca_ir_ast as ast;
