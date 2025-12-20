pub mod compiler;
pub mod dae;
pub mod fmt;
pub mod ir;
pub mod lint;

// LSP module is available with lsp-core feature (used by both native LSP and WASM)
#[cfg(feature = "lsp-core")]
pub mod lsp;
pub mod modelica_grammar;
#[cfg(feature = "python")]
mod python;
#[cfg(feature = "wasm")]
pub mod wasm;

// Re-export generated parser modules for convenience
pub use modelica_grammar::generated::modelica_grammar_trait;
pub use modelica_grammar::generated::modelica_parser;

// Re-export the main API types for convenience
pub use compiler::{
    CompilationResult, Compiler, extract_parse_error, parse_file_cached, parse_file_cached_result,
    parse_source, parse_source_simple,
};
pub use fmt::{CONFIG_FILE_NAMES, FormatOptions, format_modelica};
pub use lint::{
    LINT_CONFIG_FILE_NAMES, LintConfig, LintLevel, LintMessage, LintResult, lint_file, lint_str,
};
