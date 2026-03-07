//! Modelica linter for the Rumoca compiler.
//!
//! This crate provides lint rules for Modelica code, similar to Clippy for Rust.
//! It checks for style issues, potential bugs, and best practices.
//!
//! # Overview
//!
//! The linter provides:
//! - Style checks (naming conventions, documentation)
//! - Code quality checks (magic numbers)
//! - Best practices enforcement
//!
//! # Configuration
//!
//! The linter can be configured via:
//! - A `.rumoca_lint.toml` or `rumoca_lint.toml` file in the project root
//! - Programmatic options via `LintOptions`
//!
//! # Example
//!
//! ```ignore
//! use rumoca_tool_lint::{lint, LintOptions, LintLevel};
//!
//! let source = "model M Real x; end M;";
//! let messages = lint(source, "model.mo", &LintOptions::default());
//! for msg in messages {
//!     println!("{}: {}", msg.level, msg.message);
//! }
//! ```

use crate::lint_options::LintOptions;
use crate::lint_rules::{
    LintLevel, LintMessage, LintRule, MagicNumberRule, MissingDocumentationRule,
    NamingConventionRule,
};

use rumoca_session::parsing::validate_source_syntax;

/// Lint Modelica source code.
///
/// Returns a list of lint messages (warnings, errors, suggestions).
pub fn lint(source: &str, file_name: &str, options: &LintOptions) -> Vec<LintMessage> {
    let mut messages = Vec::new();

    // Check syntax first
    if let Err(e) = validate_source_syntax(source, file_name) {
        messages.push(LintMessage {
            rule: "syntax-error",
            level: LintLevel::Error,
            message: format!("Syntax error: {}", e),
            file: file_name.to_string(),
            line: 1,
            column: 1,
            suggestion: None,
        });
        return messages;
    }

    // Apply lint rules
    for rule in get_enabled_rules(options) {
        let rule_messages = rule.check(source, file_name);
        messages.extend(rule_messages);
    }

    // Filter by minimum level
    messages.retain(|m| m.level >= options.min_level);

    // Filter disabled rules
    messages.retain(|m| !options.disabled_rules.contains(&m.rule.to_string()));

    messages
}

/// Get the list of enabled lint rules.
fn get_enabled_rules(options: &LintOptions) -> Vec<Box<dyn LintRule>> {
    let mut rules: Vec<Box<dyn LintRule>> = Vec::new();

    // Add built-in rules
    if !options
        .disabled_rules
        .contains(&"naming-convention".to_string())
    {
        rules.push(Box::new(NamingConventionRule));
    }

    if !options
        .disabled_rules
        .contains(&"missing-documentation".to_string())
    {
        rules.push(Box::new(MissingDocumentationRule));
    }

    if !options.disabled_rules.contains(&"magic-number".to_string()) {
        rules.push(Box::new(MagicNumberRule));
    }

    rules
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lint_valid_model() {
        let source = "model M Real x; end M;";
        let messages = lint(source, "test.mo", &LintOptions::default());
        // May have style warnings but no errors
        assert!(messages.iter().all(|m| m.level != LintLevel::Error));
    }

    #[test]
    fn test_lint_syntax_error() {
        let source = "model M Real x end M;"; // Missing semicolon
        let messages = lint(source, "test.mo", &LintOptions::default());
        assert!(messages.iter().any(|m| m.level == LintLevel::Error));
    }

    #[test]
    fn test_lint_syntax_error_keeps_filename() {
        let source = "model M Real x end M;"; // Missing semicolon
        let messages = lint(source, "named_input.mo", &LintOptions::default());
        assert_eq!(messages.len(), 1);
        assert_eq!(messages[0].rule, "syntax-error");
        assert_eq!(messages[0].file, "named_input.mo");
    }

    #[test]
    fn test_lint_disabled_rule() {
        let source = "model m Real x; end m;"; // Lowercase model name
        let mut options = LintOptions::default();
        options.disabled_rules.push("naming-convention".to_string());
        let messages = lint(source, "test.mo", &options);
        assert!(messages.iter().all(|m| m.rule != "naming-convention"));
    }

    #[test]
    fn test_lint_min_level_filters_warnings() {
        let source = "model m Real x; end m;"; // Lowercase model name -> warning
        let options = LintOptions::errors_only();
        let messages = lint(source, "test.mo", &options);
        assert!(messages.is_empty());
    }
}
