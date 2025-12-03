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
pub use fmt::{CONFIG_FILE_NAMES, FormatOptions, format_modelica};
pub use lint::{
    LINT_CONFIG_FILE_NAMES, LintConfig, LintLevel, LintMessage, LintResult, lint_file, lint_str,
};
