// Allow clippy lints that suggest unstable features or are too strict for generated code
#![allow(clippy::collapsible_if)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::field_reassign_with_default)]
#![allow(clippy::writeln_empty_string)]

pub mod compiler;
pub mod dae;
pub mod errors;
pub mod ir;
pub mod modelica_grammar;
pub mod modelica_grammar_trait;
pub mod modelica_parser;

// Re-export the main API types for convenience
pub use compiler::{CompilationResult, Compiler};
