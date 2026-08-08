//! Input handed to every lint rule.

use rumoca_compile::parsing::ast;

/// Everything a lint rule is allowed to look at for one file.
///
/// The AST is parsed once by [`crate::lint`] and shared with every rule, so
/// rules reason about declarations and expressions instead of raw lines. That
/// is what keeps rules from firing on text inside comments and string literals.
///
/// The raw source text is deliberately *not* exposed here: a rule that can read
/// it can re-introduce the line-scanning false positives the AST rewrite
/// removed. Inline `// rumoca-lint:` directives are the one place that needs the
/// raw text, and [`crate::lint`] parses those itself before rules ever run.
pub struct LintContext<'a> {
    /// Display name reported in [`crate::LintMessage::file`].
    pub file_name: &'a str,
    /// The parsed abstract syntax tree for the linted file.
    pub ast: &'a ast::StoredDefinition,
}
