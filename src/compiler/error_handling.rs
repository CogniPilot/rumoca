//! Syntax error handling and diagnostic creation.
//!
//! This module provides utilities for converting parser errors into
//! user-friendly diagnostics with accurate source locations.

// False positive from miette derive macro on some nightly versions
#![allow(unused_assignments)]

use miette::{Diagnostic, SourceSpan};
use owo_colors::OwoColorize;
use parol_runtime::{ParolError, ParserError};
use thiserror::Error;

// =============================================================================
// Error Types
// =============================================================================

/// Error for parse/syntax errors in Modelica code
#[derive(Error, Debug, Diagnostic)]
#[error("Syntax error")]
#[diagnostic(
    code(rumoca::syntax_error),
    help("Check the {syntax} near the highlighted location", syntax = "Modelica syntax".cyan())
)]
#[allow(unused_assignments)] // False positive from miette derive macro
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

// =============================================================================
// Error Creation Helpers
// =============================================================================

/// Create a syntax error diagnostic from a parse error using structured error data
pub fn create_syntax_error(error: &ParolError, source: &str) -> SyntaxError {
    // Check for user errors (from anyhow::bail! in grammar actions)
    if let ParolError::UserError(user_error) = error {
        let message = user_error.to_string();
        // Try to extract line/column from the error message
        let (line, col) = extract_line_col_from_error(&message).unwrap_or((1, 1));
        let byte_offset = line_col_to_byte_offset(source, line, col);
        let remaining = source.len().saturating_sub(byte_offset);
        let span_len = remaining.min(10);

        return SyntaxError {
            src: source.to_string(),
            span: SourceSpan::new(byte_offset.into(), span_len),
            message,
        };
    }

    // Try to extract structured error information from ParolError
    if let ParolError::ParserError(parser_error) = error
        && let Some((line, col, message)) = extract_from_parser_error(parser_error, source)
    {
        let byte_offset = line_col_to_byte_offset(source, line, col);
        let remaining = source.len().saturating_sub(byte_offset);
        let span_len = remaining.min(10);

        return SyntaxError {
            src: source.to_string(),
            span: SourceSpan::new(byte_offset.into(), span_len),
            message,
        };
    }

    // Fallback for other error types
    SyntaxError {
        src: source.to_string(),
        span: SourceSpan::new(0.into(), 1_usize),
        message: "Syntax error".to_string(),
    }
}

/// Extract location and message from a ParserError
fn extract_from_parser_error(error: &ParserError, source: &str) -> Option<(usize, usize, String)> {
    match error {
        ParserError::SyntaxErrors { entries } => {
            if let Some(first) = entries.first() {
                let line = first.error_location.start_line as usize;
                let col = first.error_location.start_column as usize;

                // Build a clean message from expected/unexpected tokens
                let message = build_clean_message(first, source);
                return Some((line, col, message));
            }
        }
        ParserError::UnprocessedInput { last_token, .. } => {
            let line = last_token.start_line as usize;
            let col = last_token.start_column as usize;
            return Some((line, col, "Unexpected input after valid syntax".to_string()));
        }
        ParserError::PredictionError { cause } => {
            // The cause contains the ugly message, but we can try to extract location
            if let Some((line, col)) = extract_location_from_cause(cause) {
                return Some((line, col, "Unexpected token".to_string()));
            }
        }
        _ => {}
    }
    None
}

/// Build a clean error message from a SyntaxError, using source to extract actual token text
fn build_clean_message(err: &parol_runtime::SyntaxError, source: &str) -> String {
    // If there are unexpected tokens, describe what was found
    if !err.unexpected_tokens.is_empty() {
        let unexpected = &err.unexpected_tokens[0];
        // Extract actual token text from source using byte offsets
        let start = unexpected.token.start as usize;
        let end = unexpected.token.end as usize;
        let token_text = if start < source.len() && end <= source.len() && start < end {
            &source[start..end]
        } else {
            // Fallback to cleaned token_type if extraction fails
            return build_fallback_message(err);
        };

        // Build expected tokens list (limit to first few), cleaning up internal names
        let expected: Vec<String> = err
            .expected_tokens
            .iter()
            .take(5)
            .map(|s| clean_token_name(s))
            .collect();

        if expected.is_empty() {
            return format!("Unexpected '{}'", token_text);
        } else if expected.len() == 1 {
            return format!("Unexpected '{}', expected {}", token_text, expected[0]);
        } else {
            return format!(
                "Unexpected '{}', expected one of: {}",
                token_text,
                expected.join(", ")
            );
        }
    }

    // Fall back to a generic message
    "Syntax error".to_string()
}

/// Fallback message builder when source extraction fails
fn build_fallback_message(err: &parol_runtime::SyntaxError) -> String {
    if !err.unexpected_tokens.is_empty() {
        let token_name = clean_token_name(&err.unexpected_tokens[0].token_type);
        let expected: Vec<String> = err
            .expected_tokens
            .iter()
            .take(5)
            .map(|s| clean_token_name(s))
            .collect();

        if expected.is_empty() {
            return format!("Unexpected {}", token_name);
        } else {
            return format!(
                "Unexpected {}, expected one of: {}",
                token_name,
                expected.join(", ")
            );
        }
    }
    "Syntax error".to_string()
}

/// Clean up internal token names to be more user-friendly
fn clean_token_name(name: &str) -> String {
    // Map common punctuation and keywords
    match name {
        // Punctuation
        "Semicolon" => "';'".to_string(),
        "Comma" => "','".to_string(),
        "LParen" => "'('".to_string(),
        "RParen" => "')'".to_string(),
        "LBrace" => "'{'".to_string(),
        "RBrace" => "'}'".to_string(),
        "LBracket" => "'['".to_string(),
        "RBracket" => "']'".to_string(),
        "Assign" => "':='".to_string(),
        "Equals" => "'='".to_string(),
        "Colon" => "':'".to_string(),
        "Dot" => "'.'".to_string(),
        "Plus" => "'+'".to_string(),
        "Minus" => "'-'".to_string(),
        "Star" => "'*'".to_string(),
        "Slash" => "'/'".to_string(),
        "Caret" => "'^'".to_string(),
        "Less" => "'<'".to_string(),
        "Greater" => "'>'".to_string(),
        "LessEqual" => "'<='".to_string(),
        "GreaterEqual" => "'>='".to_string(),
        "NotEqual" => "'<>'".to_string(),

        // Regex-based identifier patterns (parol internal names)
        s if s.starts_with("LBracketUnderscore") && s.contains("AMinusZ") => {
            "identifier".to_string()
        }
        s if s.contains("AMinusZ") && s.contains("0Minus9") => "identifier".to_string(),

        // Number patterns
        s if s.contains("0Minus9") => "number".to_string(),

        // String patterns
        s if s.contains("QuotationMark") || s.contains("DoubleQuote") => "string".to_string(),

        // Clean up CamelCase keywords to lowercase
        s => s.to_lowercase(),
    }
}

/// Extract location from a prediction error cause string
fn extract_location_from_cause(cause: &str) -> Option<(usize, usize)> {
    // Look for pattern like "filename.mo:4:1"
    if let Some(mo_pos) = cause.find(".mo:") {
        let after = &cause[mo_pos + 4..];
        let parts: Vec<&str> = after.split(':').take(2).collect();
        if parts.len() >= 2
            && let (Ok(line), Ok(col)) = (parts[0].parse(), parts[1].split('-').next()?.parse())
        {
            return Some((line, col));
        }
    }
    None
}

/// Extract structured error information from a ParolError.
///
/// Returns (line, column, message) tuple for use in diagnostics.
/// Line and column are 1-indexed.
///
/// This is useful for LSP diagnostics and other error reporting that needs
/// line/column information without the full miette diagnostic.
pub fn extract_parse_error(error: &ParolError, source: &str) -> (u32, u32, String) {
    // Check for user errors (from anyhow::bail! in grammar actions)
    if let ParolError::UserError(user_error) = error {
        let message = user_error.to_string();
        if let Some((line, col)) = extract_line_col_from_error(&message) {
            return (line as u32, col as u32, message);
        }
        return (1, 1, message);
    }

    if let ParolError::ParserError(parser_error) = error
        && let Some((line, col, message)) = extract_from_parser_error(parser_error, source)
    {
        return (line as u32, col as u32, message);
    }

    // Fallback
    (1, 1, "Syntax error".to_string())
}

/// Convert line/column (1-indexed) to byte offset
pub fn line_col_to_byte_offset(source: &str, line: usize, col: usize) -> usize {
    let mut byte_offset = 0;
    for (i, line_content) in source.lines().enumerate() {
        if i + 1 == line {
            byte_offset += col.saturating_sub(1);
            break;
        }
        byte_offset += line_content.len() + 1; // +1 for newline
    }
    byte_offset
}

/// Extract line and column numbers from error messages like "at line X, column Y"
pub fn extract_line_col_from_error(error_msg: &str) -> Option<(usize, usize)> {
    // Look for pattern "at line X, column Y" or "line X, column Y"
    let patterns = ["at line ", "line "];

    for pattern in patterns {
        if let Some(pos) = error_msg.find(pattern) {
            let after_pattern = &error_msg[pos + pattern.len()..];

            // Parse line number
            let line_end = after_pattern
                .find(|c: char| !c.is_ascii_digit())
                .unwrap_or(after_pattern.len());
            let line: usize = after_pattern[..line_end].parse().ok()?;

            // Look for column
            let col_pattern = ", column ";
            if let Some(col_pos) = after_pattern.find(col_pattern) {
                let after_col = &after_pattern[col_pos + col_pattern.len()..];
                let col_end = after_col
                    .find(|c: char| !c.is_ascii_digit())
                    .unwrap_or(after_col.len());
                let col: usize = after_col[..col_end].parse().ok()?;
                return Some((line, col));
            }
        }
    }
    None
}
