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

use std::collections::{HashMap, HashSet};

use crate::lint_options::LintOptions;
use crate::lint_rules::{
    LintLevel, LintMessage, LintRule, MagicNumberRule, MissingDocumentationRule,
    NamingConventionRule,
};

use rumoca_compile::parsing::validate_source_syntax;

/// Lint Modelica source code.
///
/// Returns a list of lint messages (warnings, errors, suggestions).
///
/// Inline `//` comment directives suppress findings:
/// - `// rumoca-lint: ignore-file` anywhere → the whole file is skipped (even
///   syntax errors), e.g. for intentionally-broken parser test fixtures.
/// - `// rumoca-lint: allow` on a line → suppress every finding on that line.
/// - `// rumoca-lint: allow <rule>[, <rule>...]` → suppress only those rules on
///   that line (e.g. `x = 42; // rumoca-lint: allow magic-number`).
pub fn lint(source: &str, file_name: &str, options: &LintOptions) -> Vec<LintMessage> {
    let directives = LintDirectives::parse(source);
    if directives.ignore_file {
        return Vec::new();
    }

    let mut messages = Vec::new();

    // Check syntax first
    if let Err(e) = validate_source_syntax(source, file_name) {
        if !directives.suppresses(1, "syntax-error") {
            messages.push(LintMessage {
                rule: "syntax-error",
                level: LintLevel::Error,
                message: format!("Syntax error: {}", e),
                file: file_name.to_string(),
                line: 1,
                column: 1,
                suggestion: None,
            });
        }
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

    // Filter inline `// rumoca-lint: allow` directives
    messages.retain(|m| !directives.suppresses(m.line, m.rule));

    messages
}

const LINT_DIRECTIVE: &str = "rumoca-lint:";

/// Inline lint-suppression directives parsed from `//` comments.
#[derive(Default)]
struct LintDirectives {
    ignore_file: bool,
    /// 1-based line → rules allowed on that line (`None` = all rules allowed).
    line_allows: HashMap<u32, Option<HashSet<String>>>,
}

/// One parsed `// rumoca-lint:` directive.
enum Directive {
    IgnoreFile,
    /// Allow rules on the directive's line (`None` = all rules).
    Allow(Option<HashSet<String>>),
}

impl LintDirectives {
    fn parse(source: &str) -> Self {
        let mut directives = Self::default();
        for (index, line) in source.lines().enumerate() {
            match parse_directive_line(line) {
                Some(Directive::IgnoreFile) => directives.ignore_file = true,
                Some(Directive::Allow(allow)) => {
                    directives.line_allows.insert((index + 1) as u32, allow);
                }
                None => {}
            }
        }
        directives
    }

    fn suppresses(&self, line: u32, rule: &str) -> bool {
        match self.line_allows.get(&line) {
            Some(None) => true,
            Some(Some(rules)) => rules.contains(rule),
            None => false,
        }
    }
}

/// Parse a `// rumoca-lint: <directive>` from one source line, if present (only
/// honored inside a `//` line comment).
fn parse_directive_line(line: &str) -> Option<Directive> {
    let marker = line.find(LINT_DIRECTIVE)?;
    if !matches!(line.find("//"), Some(comment) if comment <= marker) {
        return None;
    }
    let body = line[marker + LINT_DIRECTIVE.len()..].trim();
    // First token is the directive; trailing text (e.g. an explanatory note in
    // the same comment) is ignored.
    match body.split_whitespace().next().unwrap_or_default() {
        "ignore-file" | "disable-file" => Some(Directive::IgnoreFile),
        "allow" => {
            let rules = body["allow".len()..].trim();
            let allow = (!rules.is_empty()).then(|| {
                rules
                    .split(',')
                    .map(|rule| rule.trim().to_string())
                    .filter(|rule| !rule.is_empty())
                    .collect()
            });
            Some(Directive::Allow(allow))
        }
        _ => None,
    }
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
    fn ignore_file_directive_skips_whole_file_including_syntax_errors() {
        // Intentionally broken source, but the file opts out of linting.
        let source = "// rumoca-lint: ignore-file (broken on purpose)\nmodel M Real x end M;";
        assert!(lint(source, "test.mo", &LintOptions::default()).is_empty());
    }

    #[test]
    fn allow_directive_suppresses_findings_on_its_line() {
        // Lowercase model name normally warns (naming-convention) on line 1.
        let warned = lint("model m Real x; end m;", "t.mo", &LintOptions::default());
        assert!(warned.iter().any(|m| m.rule == "naming-convention"));

        // Same line with a bare `allow` → nothing on that line.
        let suppressed = lint(
            "model m Real x; end m; // rumoca-lint: allow",
            "t.mo",
            &LintOptions::default(),
        );
        assert!(suppressed.iter().all(|m| m.line != 1));

        // Targeted `allow <rule>` only suppresses the named rule.
        let targeted = lint(
            "model m Real x; end m; // rumoca-lint: allow naming-convention",
            "t.mo",
            &LintOptions::default(),
        );
        assert!(targeted.iter().all(|m| m.rule != "naming-convention"));
    }

    #[test]
    fn directive_only_honored_inside_a_line_comment() {
        // The marker in a string literal must NOT trigger suppression.
        let source = "model M Real x = \"rumoca-lint: ignore-file\"; end M;";
        // (well-formed; just ensure it isn't treated as ignore-file by parsing it)
        let directives = LintDirectives::parse(source);
        assert!(!directives.ignore_file);
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
