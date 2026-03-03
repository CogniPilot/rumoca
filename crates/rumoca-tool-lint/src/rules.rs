//! Lint rules for Modelica code.

use serde::{Deserialize, Serialize};

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

/// Class keywords to check for naming conventions.
const CLASS_KEYWORDS: &[&str] = &[
    "model ",
    "class ",
    "block ",
    "connector ",
    "record ",
    "package ",
];

/// Keywords that should have documentation.
const DOC_KEYWORDS: &[&str] = &["model ", "function "];

/// Check if a name starts with a lowercase letter.
fn starts_with_lowercase(name: &str) -> bool {
    name.chars()
        .next()
        .map(|c| c.is_lowercase())
        .unwrap_or(false)
}

/// Extract the class/function name after a keyword.
fn extract_name_after_keyword<'a>(line: &'a str, keyword: &str) -> Option<&'a str> {
    line.strip_prefix(keyword)?.split_whitespace().next()
}

/// Check if the previous line has a documentation string.
fn has_doc_on_prev_line(lines: &[&str], line_num: usize) -> bool {
    line_num > 0 && lines[line_num - 1].trim().starts_with('"')
}

/// Create a naming convention lint message for a lowercase name.
fn create_naming_message(
    keyword: &str,
    name: &str,
    file_name: &str,
    line_num: usize,
) -> LintMessage {
    LintMessage::new(
        "naming-convention",
        LintLevel::Warning,
        format!(
            "{} name '{}' should start with uppercase (PascalCase)",
            keyword.trim(),
            name
        ),
        file_name,
        (line_num + 1) as u32,
        1,
    )
    .with_suggestion(if name.is_empty() {
        "Provide a name".to_string()
    } else {
        format!("Rename to '{}{}'", name[..1].to_uppercase(), &name[1..])
    })
}

/// Check if a number should be flagged as a magic number.
fn is_magic_number(num: f64) -> bool {
    num.abs() > 1.0 && num != 2.0 && num != 10.0 && num != 100.0
}

/// Check if a line should be skipped for magic number detection.
fn should_skip_magic_number_check(trimmed: &str) -> bool {
    trimmed.starts_with("parameter")
        || trimmed.starts_with("constant")
        || trimmed.contains("start")
        || trimmed.contains("= 0")
        || trimmed.contains("= 1")
        || trimmed.contains("= -1")
        || !trimmed.contains('=')
        || trimmed.starts_with("//")
}

/// Check if a character can be part of a number.
fn is_number_char(c: char, in_number: bool) -> bool {
    c.is_ascii_digit() || (c == '.' && in_number) || (c == '-' && !in_number)
}

/// Check if a number string represents a magic number and return it if so.
fn check_magic_number(num_str: &str, col: usize) -> Option<(usize, String)> {
    let num: f64 = num_str.parse().ok()?;
    if is_magic_number(num) {
        Some((col, num_str.to_string()))
    } else {
        None
    }
}

/// Find magic numbers in a line and return (column, number_string) pairs.
fn find_magic_numbers(trimmed: &str) -> Vec<(usize, String)> {
    let mut results = Vec::new();
    let mut in_number = false;
    let mut number_start = 0;

    for (i, c) in trimmed.char_indices() {
        if is_number_char(c, in_number) && !in_number {
            number_start = i;
            in_number = true;
            continue;
        }
        if !is_number_char(c, in_number) && in_number {
            if let Some(result) = check_magic_number(&trimmed[number_start..i], number_start) {
                results.push(result);
            }
            in_number = false;
        }
    }
    results
}

/// Trait for lint rules.
pub trait LintRule {
    /// Check source code and return lint messages.
    fn check(&self, source: &str, file_name: &str) -> Vec<LintMessage>;

    /// Get the rule name.
    fn name(&self) -> &'static str;

    /// Get a description of the rule.
    fn description(&self) -> &'static str;
}

/// Check a single line for naming convention violations.
fn check_line_naming(trimmed: &str, file_name: &str, line_num: usize) -> Vec<LintMessage> {
    CLASS_KEYWORDS
        .iter()
        .filter_map(|keyword| extract_name_after_keyword(trimmed, keyword).map(|n| (keyword, n)))
        .filter(|(_, name)| !name.is_empty() && starts_with_lowercase(name))
        .map(|(keyword, name)| create_naming_message(keyword, name, file_name, line_num))
        .collect()
}

/// Check a single line for missing documentation.
fn check_line_doc(
    trimmed: &str,
    lines: &[&str],
    file_name: &str,
    line_num: usize,
) -> Vec<LintMessage> {
    DOC_KEYWORDS
        .iter()
        .filter_map(|keyword| extract_name_after_keyword(trimmed, keyword).map(|n| (keyword, n)))
        .filter(|_| !has_doc_on_prev_line(lines, line_num))
        .map(|(keyword, name)| {
            let name = if name.is_empty() { "unknown" } else { name };
            LintMessage::new(
                "missing-documentation",
                LintLevel::Note,
                format!("{}'{}' is missing documentation", keyword.trim(), name),
                file_name,
                (line_num + 1) as u32,
                1,
            )
        })
        .collect()
}

/// Rule: Check naming conventions.
pub struct NamingConventionRule;

impl LintRule for NamingConventionRule {
    fn name(&self) -> &'static str {
        "naming-convention"
    }

    fn description(&self) -> &'static str {
        "Check that model and class names follow conventions (PascalCase)"
    }

    fn check(&self, source: &str, file_name: &str) -> Vec<LintMessage> {
        source
            .lines()
            .enumerate()
            .flat_map(|(line_num, line)| check_line_naming(line.trim(), file_name, line_num))
            .collect()
    }
}

/// Rule: Check for missing documentation.
pub struct MissingDocumentationRule;

impl LintRule for MissingDocumentationRule {
    fn name(&self) -> &'static str {
        "missing-documentation"
    }

    fn description(&self) -> &'static str {
        "Check that models and functions have documentation strings"
    }

    fn check(&self, source: &str, file_name: &str) -> Vec<LintMessage> {
        let lines: Vec<&str> = source.lines().collect();
        lines
            .iter()
            .enumerate()
            .flat_map(|(line_num, line)| check_line_doc(line.trim(), &lines, file_name, line_num))
            .collect()
    }
}

/// Rule: Check for magic numbers.
pub struct MagicNumberRule;

impl LintRule for MagicNumberRule {
    fn name(&self) -> &'static str {
        "magic-number"
    }

    fn description(&self) -> &'static str {
        "Check for literal numbers that should be named constants"
    }

    fn check(&self, source: &str, file_name: &str) -> Vec<LintMessage> {
        let mut messages = Vec::new();
        for (line_num, line) in source.lines().enumerate() {
            let trimmed = line.trim();
            if should_skip_magic_number_check(trimmed) {
                continue;
            }
            for (col, num_str) in find_magic_numbers(trimmed) {
                messages.push(LintMessage::new(
                    "magic-number",
                    LintLevel::Help,
                    format!("Consider extracting '{}' as a named constant", num_str),
                    file_name,
                    (line_num + 1) as u32,
                    (col + 1) as u32,
                ));
            }
        }
        messages
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_naming_convention_rule() {
        let rule = NamingConventionRule;
        let source = "model myModel Real x; end myModel;";
        let messages = rule.check(source, "test.mo");
        assert!(!messages.is_empty());
        assert_eq!(messages[0].rule, "naming-convention");
    }

    #[test]
    fn test_naming_convention_valid() {
        let rule = NamingConventionRule;
        let source = "model MyModel Real x; end MyModel;";
        let messages = rule.check(source, "test.mo");
        assert!(messages.is_empty());
    }

    #[test]
    fn test_lint_level_ordering() {
        assert!(LintLevel::Help < LintLevel::Note);
        assert!(LintLevel::Note < LintLevel::Warning);
        assert!(LintLevel::Warning < LintLevel::Error);
    }
}
