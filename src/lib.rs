// Allow clippy lints that suggest unstable features or are too strict for generated code
#![allow(clippy::collapsible_if)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::field_reassign_with_default)]
#![allow(clippy::writeln_empty_string)]
// Allow mutable_key_type for LSP - Uri has interior mutability but we use it correctly as a key
#![allow(clippy::mutable_key_type)]

pub mod compiler;
pub mod dae;
pub mod fmt;
pub mod ir;
pub mod lint;
#[cfg(feature = "lsp")]
pub mod lsp;
pub mod modelica_grammar;

// Re-export generated modules from modelica_grammar::generated for backward compatibility
pub use modelica_grammar::generated::modelica_grammar_trait;
pub use modelica_grammar::generated::modelica_parser;

// Re-export the main API types for convenience
pub use compiler::{CompilationResult, Compiler};
pub use fmt::{format_modelica, FormatOptions, CONFIG_FILE_NAMES};
pub use lint::{
    LINT_CONFIG_FILE_NAMES, LintConfig, LintLevel, LintMessage, LintResult, lint_file, lint_str,
};
