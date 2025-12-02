//! Error extraction and formatting for diagnostics.
//!
//! Provides functions to extract structured error information from parser errors.

use parol_runtime::{ParolError, ParserError};

/// Extract structured error information from ParolError
pub fn extract_structured_error(error: &ParolError, source: &str) -> (u32, u32, String) {
    if let ParolError::ParserError(parser_error) = error {
        if let Some((line, col, message)) = extract_from_parser_error(parser_error, source) {
            return (line, col, message);
        }
    }
    // Fallback
    (1, 1, "Syntax error".to_string())
}

/// Extract location and message from a ParserError
fn extract_from_parser_error(error: &ParserError, source: &str) -> Option<(u32, u32, String)> {
    match error {
        ParserError::SyntaxErrors { entries } => {
            if let Some(first) = entries.first() {
                let line = first.error_location.start_line;
                let col = first.error_location.start_column;
                let message = build_clean_message(first, source);
                return Some((line, col, message));
            }
        }
        ParserError::UnprocessedInput { last_token, .. } => {
            let line = last_token.start_line;
            let col = last_token.start_column;
            return Some((line, col, "Unexpected input after valid syntax".to_string()));
        }
        ParserError::PredictionError { .. } => {
            return Some((1, 1, "Unexpected token".to_string()));
        }
        _ => {}
    }
    None
}

/// Build a clean error message from a SyntaxError, using source to extract actual token text
fn build_clean_message(err: &parol_runtime::SyntaxError, source: &str) -> String {
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
