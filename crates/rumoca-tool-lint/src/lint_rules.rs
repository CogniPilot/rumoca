//! Lint rules for Modelica code.
//!
//! Rules are AST-driven ([`crate::LintContext`]): they inspect declarations and
//! expressions rather than raw source lines, so keywords or numbers that appear
//! inside comments and string literals never produce findings.

use serde::{Deserialize, Serialize};

use crate::lint_context::LintContext;

mod ast_rules;

pub(crate) use ast_rules::{
    ExternalPurityRule, MagicNumberRule, MissingDocumentationRule, NamingConventionRule,
};

/// Severity level for lint messages.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum LintLevel {
    /// Suggestions for improvement.
    Help,
    /// Style or convention issues.
    Note,
    /// Potential problems.
    #[default]
    Warning,
    /// Definite errors.
    Error,
}

impl std::fmt::Display for LintLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LintLevel::Help => write!(f, "help"),
            LintLevel::Note => write!(f, "note"),
            LintLevel::Warning => write!(f, "warning"),
            LintLevel::Error => write!(f, "error"),
        }
    }
}

/// A lint message.
#[derive(Debug, Clone)]
pub struct LintMessage {
    /// The lint rule that generated this message.
    pub rule: &'static str,
    /// Severity level.
    pub level: LintLevel,
    /// Human-readable message.
    pub message: String,
    /// File path.
    pub file: String,
    /// Line number (1-based).
    pub line: u32,
    /// Column number (1-based).
    pub column: u32,
    /// Optional suggestion for fixing the issue.
    pub suggestion: Option<String>,
}

impl LintMessage {
    /// Create a new lint message.
    pub fn new(
        rule: &'static str,
        level: LintLevel,
        message: impl Into<String>,
        file: impl Into<String>,
        line: u32,
        column: u32,
    ) -> Self {
        Self {
            rule,
            level,
            message: message.into(),
            file: file.into(),
            line,
            column,
            suggestion: None,
        }
    }

    /// Add a suggestion to the message.
    pub fn with_suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestion = Some(suggestion.into());
        self
    }
}

/// Check if a name starts with a lowercase letter.
fn starts_with_lowercase(name: &str) -> bool {
    name.chars().next().is_some_and(char::is_lowercase)
}

/// Suggested PascalCase rename for `name`.
fn pascal_case_suggestion(name: &str) -> String {
    let mut chars = name.chars();
    match chars.next() {
        Some(first) => format!("Rename to '{}{}'", first.to_uppercase(), chars.as_str()),
        None => "Provide a name".to_string(),
    }
}

/// Create a naming convention lint message for a lowercase name.
///
/// The message and suggestion wording is asserted by the wasm binding tests
/// (`crates/rumoca-bind-wasm/src/tests.rs`); keep it stable.
fn create_naming_message(
    keyword: &str,
    name: &str,
    file_name: &str,
    line: u32,
    column: u32,
) -> LintMessage {
    LintMessage::new(
        "naming-convention",
        LintLevel::Warning,
        format!("{keyword} name '{name}' should start with uppercase (PascalCase)"),
        file_name,
        line,
        column,
    )
    .with_suggestion(pascal_case_suggestion(name))
}

/// Check if a number should be flagged as a magic number.
///
/// `0`, `1`, `-1`, `2`, `10` and `100` are conventional and never reported.
fn is_magic_number(num: f64) -> bool {
    num.abs() > 1.0 && num != 2.0 && num != 10.0 && num != 100.0
}

/// Trait for lint rules.
pub trait LintRule {
    /// Check the file described by `ctx` and return lint messages.
    fn check(&self, ctx: &LintContext<'_>) -> Vec<LintMessage>;

    /// Get the rule name.
    fn name(&self) -> &'static str;

    /// Get a description of the rule.
    fn description(&self) -> &'static str;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lint_level_ordering() {
        assert!(LintLevel::Help < LintLevel::Note);
        assert!(LintLevel::Note < LintLevel::Warning);
        assert!(LintLevel::Warning < LintLevel::Error);
    }

    #[test]
    fn starts_with_lowercase_only_matches_lowercase_initials() {
        assert!(starts_with_lowercase("myModel"));
        assert!(!starts_with_lowercase("MyModel"));
        assert!(!starts_with_lowercase("'quoted'"));
        assert!(!starts_with_lowercase(""));
    }

    #[test]
    fn pascal_case_suggestion_uppercases_the_first_character_only() {
        assert_eq!(pascal_case_suggestion("foo"), "Rename to 'Foo'");
        assert_eq!(pascal_case_suggestion("fooBar"), "Rename to 'FooBar'");
        assert_eq!(pascal_case_suggestion(""), "Provide a name");
    }

    #[test]
    fn is_magic_number_exempts_conventional_constants() {
        for exempt in [0.0, 1.0, -1.0, 2.0, 10.0, 100.0] {
            assert!(!is_magic_number(exempt), "{exempt} should be exempt");
        }
        for magic in [42.0, 7.0, 350.0, -273.15] {
            assert!(is_magic_number(magic), "{magic} should be magic");
        }
    }
}
