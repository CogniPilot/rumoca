//! Error types with beautiful diagnostic reporting using miette

use miette::{Diagnostic, SourceSpan};
use owo_colors::OwoColorize;
use thiserror::Error;

/// Error for undefined variable references in Modelica code
#[derive(Error, Debug, Diagnostic)]
#[error("Undefined variable {var_name}", var_name = self.var_name.cyan().bold())]
#[diagnostic(
    code(rumoca::undefined_variable),
    help("Check for {typos} or make sure the variable is {declared} before use",
         typos = "typos".yellow(),
         declared = "declared".green())
)]
pub struct UndefinedVariableError {
    /// The source code being compiled
    #[source_code]
    pub src: String,

    /// Name of the undefined variable
    pub var_name: String,

    /// Location in source where the variable appears
    #[label("undefined variable here")]
    pub span: SourceSpan,
}

/// Error for parse/syntax errors in Modelica code
#[derive(Error, Debug, Diagnostic)]
#[error("Syntax error")]
#[diagnostic(
    code(rumoca::syntax_error),
    help("Check the {syntax} near the highlighted location", syntax = "Modelica syntax".cyan())
)]
pub struct SyntaxError {
    /// The source code being compiled
    #[source_code]
    pub src: String,

    /// Location of the syntax error
    #[label("{message}")]
    pub span: SourceSpan,

    /// Error message from the parser
    pub message: String,
}
