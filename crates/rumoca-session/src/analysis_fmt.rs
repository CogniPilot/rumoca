//! Modelica code formatter for the Rumoca compiler.
//!
//! This crate provides code formatting for Modelica source files.
//! It parses the source, validates syntax, and applies formatting rules.
//!
//! # Overview
//!
//! The formatter is responsible for:
//! - Consistent indentation (configurable spaces or tabs)
//! - Proper spacing around operators
//! - Multi-line array formatting with proper indentation
//! - Normalized line endings
//! - Preserving comments (when CST is available)
//!
//! # Configuration
//!
//! The formatter can be configured via:
//! - A `.rumoca_fmt.toml` or `rumoca_fmt.toml` file in the project root
//! - Programmatic options via `FormatOptions`
//!
//! # Example
//!
//! ```ignore
//! use rumoca_tool_fmt::{format, FormatOptions};
//!
//! let source = "model M Real x;equation der(x)=1;end M;";
//! let formatted = format(source, &FormatOptions::default())?;
//! ```
//!
//! # Specification Deviation
//!
//! SPEC_0015 specifies a CST-based formatter with trivia (comment/whitespace)
//! preservation. The current implementation is a **simplified placeholder** that:
//! - Does NOT preserve comments (they are lost)
//! - Uses basic line-by-line formatting instead of token stream
//! - Validates syntax but cannot reconstruct original structure
//!
//! Full SPEC_0015 compliance requires:
//! 1. CST lexer with trivia capture (SPEC_0012)
//! 2. Token-stream based formatting
//! 3. AST-equivalence validation
//!
//! See: spec/archive/deferred/SPEC_0012_CST_AST.md,
//!      spec/archive/deferred/SPEC_0015_FORMATTER.md

use crate::analysis_fmt_errors::FormatError;
use crate::analysis_fmt_options::FormatOptions;

use rumoca_phase_parse::parse_string;

/// Format Modelica source code.
///
/// Returns the formatted source code, or an error if the source has syntax errors.
pub(crate) fn format(source: &str, options: &FormatOptions) -> Result<String, FormatError> {
    // Validate syntax first
    if let Err(e) = parse_string(source, "<format>") {
        return Err(FormatError::SyntaxError(e.to_string()));
    }

    // Apply basic formatting
    Ok(format_source(source, options))
}

/// Format Modelica source code, returning original on error.
///
/// This is a convenience function that returns the original source code
/// if formatting fails due to syntax errors. Useful when you want to
/// attempt formatting but not fail on invalid input.
pub(crate) fn format_or_original(source: &str, options: &FormatOptions) -> String {
    format(source, options).unwrap_or_else(|_| source.to_string())
}

/// Apply formatting rules to source code.
fn format_source(source: &str, options: &FormatOptions) -> String {
    let mut result = String::with_capacity(source.len());
    let indent_str = if options.use_tabs {
        "\t".to_string()
    } else {
        " ".repeat(options.indent_size)
    };

    let mut indent_level: usize = 0;
    let mut in_string = false;
    let mut prev_char = '\0';

    for line in source.lines() {
        let trimmed = line.trim();

        // Skip empty lines
        if trimmed.is_empty() {
            result.push('\n');
            continue;
        }

        // Adjust indent level for closing keywords
        if !in_string {
            let lower = trimmed.to_lowercase();
            if lower.starts_with("end ")
                || lower == "end;"
                || lower.starts_with("else")
                || lower.starts_with("elseif")
            {
                indent_level = indent_level.saturating_sub(1);
            }
        }

        // Write indentation
        for _ in 0..indent_level {
            result.push_str(&indent_str);
        }

        // Write line content with basic spacing fixes
        let formatted_line = format_line(trimmed, options);
        result.push_str(&formatted_line);
        result.push('\n');

        // Track string state for multi-line strings
        for c in trimmed.chars() {
            if c == '"' && prev_char != '\\' {
                in_string = !in_string;
            }
            prev_char = c;
        }

        // Adjust indent level for opening keywords
        if !in_string {
            let lower = trimmed.to_lowercase();
            if lower.starts_with("model ")
                || lower.starts_with("class ")
                || lower.starts_with("block ")
                || lower.starts_with("connector ")
                || lower.starts_with("record ")
                || lower.starts_with("type ")
                || lower.starts_with("package ")
                || lower.starts_with("function ")
                || lower.starts_with("equation")
                || lower.starts_with("algorithm")
                || lower.starts_with("initial equation")
                || lower.starts_with("initial algorithm")
                || lower.starts_with("public")
                || lower.starts_with("protected")
                || lower.starts_with("if ")
                || lower.starts_with("for ")
                || lower.starts_with("while ")
                || lower.starts_with("when ")
            {
                indent_level += 1;
            }
        }
    }

    // Ensure file ends with newline
    if !result.ends_with('\n') {
        result.push('\n');
    }

    result
}

/// Check if we need a space before '=' based on previous character.
fn needs_space_before_eq(prev_char: char, result: &str) -> bool {
    !matches!(prev_char, ':' | '=' | '<' | '>') && !result.ends_with(' ') && !result.is_empty()
}

/// Check if we need a space after '=' based on next character.
fn needs_space_after_eq(next: Option<&char>) -> bool {
    matches!(next, Some(&c) if c != '=' && c != ' ')
}

/// Check if we need a space before an arithmetic operator.
fn needs_space_before_arith(prev_char: char, result: &str) -> bool {
    !matches!(prev_char, 'e' | 'E' | '(') && !result.ends_with(' ') && !result.is_empty()
}

/// Check if we need a space after an arithmetic operator.
fn needs_space_after_arith(next: Option<&char>) -> bool {
    matches!(next, Some(&c) if !matches!(c, ' ' | ')' | ';' | ','))
}

/// Check if we need a space after ','.
fn needs_space_after_comma(next: Option<&char>) -> bool {
    matches!(next, Some(&c) if c != ' ')
}

/// Format a single line with spacing fixes.
fn format_line(line: &str, _options: &FormatOptions) -> String {
    let mut result = String::with_capacity(line.len());
    let mut chars = line.chars().peekable();
    let mut in_string = false;
    let mut prev_char = '\0';

    while let Some(c) = chars.next() {
        // Track string state
        if c == '"' && prev_char != '\\' {
            in_string = !in_string;
        }

        if in_string {
            result.push(c);
            prev_char = c;
            continue;
        }

        // Add space around operators
        match c {
            '=' => {
                if needs_space_before_eq(prev_char, &result) {
                    result.push(' ');
                }
                result.push(c);
                if needs_space_after_eq(chars.peek()) {
                    result.push(' ');
                }
            }
            '+' | '-' | '*' | '/' => {
                if needs_space_before_arith(prev_char, &result) {
                    result.push(' ');
                }
                result.push(c);
                if needs_space_after_arith(chars.peek()) {
                    result.push(' ');
                }
            }
            ',' => {
                result.push(c);
                if needs_space_after_comma(chars.peek()) {
                    result.push(' ');
                }
            }
            _ => result.push(c),
        }

        prev_char = c;
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check_syntax(source: &str) -> Result<(), String> {
        parse_string(source, "<check>").map_err(|e| e.to_string())
    }

    #[test]
    fn test_format_simple_model() {
        let source = "model M Real x;equation der(x)=1;end M;";
        let formatted = format(source, &FormatOptions::default()).unwrap();
        assert!(formatted.contains("model M"));
        assert!(formatted.contains("end M;"));
    }

    #[test]
    fn test_format_syntax_error() {
        let source = "model M Real x end M;"; // Missing semicolon
        let result = format(source, &FormatOptions::default());
        assert!(result.is_err());
    }

    #[test]
    fn test_format_or_original() {
        let invalid_source = "model M Real x end M;";
        let result = format_or_original(invalid_source, &FormatOptions::default());
        assert_eq!(result, invalid_source);
    }

    #[test]
    fn test_check_syntax_valid() {
        let source = "model M Real x; end M;";
        assert!(check_syntax(source).is_ok());
    }

    #[test]
    fn test_check_syntax_invalid() {
        let source = "model M Real x end M;"; // Missing semicolon
        assert!(check_syntax(source).is_err());
    }

    #[test]
    fn test_format_options_default() {
        let opts = FormatOptions::default();
        assert_eq!(opts.indent_size, 2);
        assert!(!opts.use_tabs);
    }
}
