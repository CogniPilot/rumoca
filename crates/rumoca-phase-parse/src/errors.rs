//! Phase-local error types for parsing.
//!
//! Error codes: EP0xx for parse phase (per SPEC_0008).

use parol_runtime::errors::{ParolError, ParserError, SyntaxError};
use rumoca_core::{BytePos, Diagnostic, Label, PhaseError, SourceId, Span};
use rumoca_ir_ast as ast;

/// Parse-phase semantic error carrying a concrete source span.
#[derive(Debug, Clone)]
pub struct ParseSemanticError {
    pub message: String,
    pub span: Span,
}

impl ParseSemanticError {
    pub fn from_token(message: impl Into<String>, token: &ast::Token) -> Self {
        Self::from_location(message, &token.location)
    }

    pub fn from_location(message: impl Into<String>, location: &ast::Location) -> Self {
        Self {
            message: message.into(),
            span: ast_location_to_span(location),
        }
    }
}

impl std::fmt::Display for ParseSemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for ParseSemanticError {}

/// Build an anyhow error that retains parse semantic span information.
pub fn semantic_error_from_token(message: impl Into<String>, token: &ast::Token) -> anyhow::Error {
    anyhow::Error::new(ParseSemanticError::from_token(message, token))
}

/// Build an anyhow error that retains parse semantic span information.
pub fn semantic_error_from_location(
    message: impl Into<String>,
    location: &ast::Location,
) -> anyhow::Error {
    anyhow::Error::new(ParseSemanticError::from_location(message, location))
}

/// Build a parse semantic error when a source location may be unavailable.
pub fn semantic_error_from_optional_location(
    message: impl Into<String>,
    location: Option<&ast::Location>,
) -> anyhow::Error {
    let message = message.into();
    match location {
        Some(location) => semantic_error_from_location(message, location),
        None => anyhow::anyhow!(message),
    }
}

/// Build a parse semantic error from a component reference when possible.
pub fn semantic_error_from_component_reference(
    message: impl Into<String>,
    comp: &ast::ComponentReference,
) -> anyhow::Error {
    semantic_error_from_optional_location(message, comp.get_location())
}

/// Build a parse semantic error from an expression when possible.
pub fn semantic_error_from_expression(
    message: impl Into<String>,
    expr: &ast::Expression,
) -> anyhow::Error {
    semantic_error_from_optional_location(message, expr.get_location())
}

/// Format expected tokens as a human-readable string.
fn format_expected(expected: &[String]) -> Option<String> {
    match expected.len() {
        0 => None,
        1 => Some(format!("expected {}", expected[0])),
        _ => Some(format!("expected one of: {}", expected.join(", "))),
    }
}

/// Extract substring between delimiters (exclusive).
/// Returns empty string if delimiters not found or malformed.
fn extract_between(s: &str, start: &str, end: char) -> String {
    let Some(start_pos) = s.find(start) else {
        return String::new();
    };
    let after_start = &s[start_pos + start.len()..];
    let Some(end_pos) = after_start.find(end) else {
        return String::new();
    };
    after_start[..end_pos].trim().to_string()
}

/// Extract token type from a string, trying " (Type)" first then "(Type)".
fn extract_token_type(s: &str) -> String {
    let t = extract_between(s, " (", ')');
    if t.is_empty() {
        extract_between(s, "(", ')')
    } else {
        t
    }
}

/// Extract a single token (name, type) from a cause fragment after "LA(n): ".
/// Returns None if the fragment doesn't contain a valid token.
fn extract_single_token(after_colon: &str) -> Option<(String, String)> {
    // Find token name - before first delimiter
    let name_end = [
        after_colon.find('\n'),
        after_colon.find(" ("),
        after_colon.find(" at "),
    ]
    .into_iter()
    .flatten()
    .min()
    .unwrap_or(after_colon.len());

    let token_name = after_colon[..name_end].trim();
    if token_name.is_empty() || token_name == "$" {
        return None;
    }

    Some((token_name.to_string(), extract_token_type(after_colon)))
}

/// Errors that can occur during parsing.
#[derive(Debug, Clone)]
pub enum ParseError {
    /// Syntax error with location information.
    SyntaxError {
        message: String,
        expected: Vec<String>,
        unexpected: Option<String>,
        span: Span,
    },
    /// No AST was produced despite successful parse.
    NoAstProduced,
    /// IO error while reading file.
    IoError { path: String, message: String },
}

impl PhaseError for ParseError {
    fn to_diagnostic(&self) -> Diagnostic {
        match self {
            Self::SyntaxError {
                message,
                expected,
                unexpected,
                span,
            } => {
                let mut diag = Diagnostic::error(message).with_code("EP001");

                // Add primary label at error location
                let label_msg = unexpected
                    .as_ref()
                    .map(|u| format!("unexpected `{}`", u))
                    .unwrap_or_else(|| "error here".to_string());
                diag = diag.with_label(Label::primary(*span).with_message(label_msg));

                // Add expected tokens as a note
                if let Some(expected_str) = format_expected(expected) {
                    diag = diag.with_note(expected_str);
                }

                diag
            }
            Self::NoAstProduced => {
                Diagnostic::error("parsing succeeded but no AST was produced").with_code("EP002")
            }
            Self::IoError { path, message } => {
                Diagnostic::error(format!("failed to read `{}`: {}", path, message))
                    .with_code("EP003")
            }
        }
    }
}

/// Convert a parol error to our parse error(s).
pub fn convert_parol_error(err: ParolError, source: &str) -> Vec<ParseError> {
    match err {
        ParolError::ParserError(parser_err) => convert_parser_error(parser_err, source),
        ParolError::LexerError(lexer_err) => {
            vec![ParseError::SyntaxError {
                message: format!("lexer error: {}", lexer_err),
                expected: vec![],
                unexpected: None,
                span: Span::DUMMY,
            }]
        }
        ParolError::UserError(user_err) => {
            if let Some(semantic_error) = user_err.downcast_ref::<ParseSemanticError>() {
                return vec![ParseError::SyntaxError {
                    message: format!("parse error: {}", semantic_error.message),
                    expected: vec![],
                    unexpected: None,
                    span: semantic_error.span,
                }];
            }
            vec![ParseError::SyntaxError {
                message: format!("parse error: {}", user_err),
                expected: vec![],
                unexpected: None,
                span: Span::DUMMY,
            }]
        }
    }
}

fn convert_parser_error(err: ParserError, source: &str) -> Vec<ParseError> {
    match err {
        ParserError::SyntaxErrors { entries } => entries
            .into_iter()
            .map(|e| convert_syntax_error(e, source))
            .collect(),
        ParserError::UnprocessedInput { last_token, .. } => {
            let span = location_to_span(&last_token);
            vec![ParseError::SyntaxError {
                message: "unexpected input after end of file".to_string(),
                expected: vec!["end of input".to_string()],
                unexpected: None,
                span,
            }]
        }
        ParserError::PredictionError { cause } => {
            vec![ParseError::SyntaxError {
                message: format!("syntax error: {}", cause),
                expected: vec![],
                unexpected: None,
                span: Span::DUMMY,
            }]
        }
        other => {
            vec![ParseError::SyntaxError {
                message: format!("parse error: {}", other),
                expected: vec![],
                unexpected: None,
                span: Span::DUMMY,
            }]
        }
    }
}

/// Reserved keywords that cannot be used as identifiers (MLS §2.3.3).
const RESERVED_KEYWORDS: &[&str] = &[
    "inner",
    "outer",
    "algorithm",
    "and",
    "annotation",
    "block",
    "break",
    "class",
    "connect",
    "connector",
    "constant",
    "constrainedby",
    "der",
    "discrete",
    "each",
    "else",
    "elseif",
    "elsewhen",
    "encapsulated",
    "end",
    "enumeration",
    "equation",
    "expandable",
    "extends",
    "external",
    "false",
    "final",
    "flow",
    "for",
    "function",
    "if",
    "import",
    "impure",
    "in",
    "initial",
    "input",
    "loop",
    "model",
    "not",
    "operator",
    "or",
    "output",
    "package",
    "parameter",
    "partial",
    "protected",
    "public",
    "pure",
    "record",
    "redeclare",
    "replaceable",
    "return",
    "stream",
    "then",
    "true",
    "type",
    "when",
    "while",
    "within",
];

fn convert_syntax_error(err: SyntaxError, source: &str) -> ParseError {
    let mut span = location_to_span(&err.error_location);
    if let Some(recovered) = span_from_cause_location(&err.cause, source)
        && should_replace_with_recovered_span(span, recovered)
    {
        span = recovered;
    }
    let expects_semicolon = err
        .expected_tokens
        .iter()
        .map(|token| clean_token_name(token))
        .any(|token| token == ";");

    // Extract first token for the primary error
    let first_token = extract_token_from_cause(&err.cause).unwrap_or_default();
    let first_token_type = extract_first_token_type_from_cause(&err.cause).unwrap_or_default();
    let token_lower = first_token.to_lowercase();
    let keyword_in_context = !first_token.is_empty()
        && RESERVED_KEYWORDS.contains(&token_lower.as_str())
        && first_token_type.to_lowercase() == token_lower;

    let unexpected = if first_token.is_empty() || first_token == "$" {
        None
    } else {
        Some(first_token.clone())
    };

    // Build a helpful message
    let message = if keyword_in_context && !expects_semicolon {
        format!(
            "`{}` is a reserved keyword in Modelica (MLS §2.3.3) and cannot be used as an identifier",
            token_lower
        )
    } else if first_token == "$" || first_token.is_empty() {
        "unexpected end of input".to_string()
    } else {
        format!("unexpected `{}`", clean_token_name(&first_token))
    };

    // Clean up expected token names
    let expected: Vec<String> = err
        .expected_tokens
        .iter()
        .filter_map(|t| {
            let clean = clean_token_name(t);
            // Filter out cryptic regex patterns
            if clean.len() > 30 || clean.contains("LBracket") || clean.contains("RBrace") {
                None
            } else {
                Some(clean)
            }
        })
        .collect();

    ParseError::SyntaxError {
        message,
        expected,
        unexpected,
        span,
    }
}

fn is_placeholder_span(span: Span) -> bool {
    span.start.0 <= 1 && span.end.0 <= 1
}

fn should_replace_with_recovered_span(original: Span, recovered: Span) -> bool {
    if is_placeholder_span(original) {
        return true;
    }
    let original_near_start = original.start.0 <= 1 && original.end.0 <= 2;
    original_near_start && recovered.start.0 > original.end.0.saturating_add(8)
}

fn span_from_cause_location(cause: &str, source: &str) -> Option<Span> {
    let location = parse_line_col_from_text(cause)?;
    let start = line_col_to_byte_offset_clamped(source, location.line, location.column)?;
    let end_column = location
        .end_column
        .unwrap_or(location.column.saturating_add(1));
    let mut end = line_col_to_byte_offset_clamped(source, location.line, end_column)?;
    if end <= start {
        end = start.saturating_add(1).min(source.len());
    }
    Some(Span::from_offsets(SourceId(0), start, end))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct LineCol {
    line: u32,
    column: u32,
    end_column: Option<u32>,
}

fn parse_line_col_from_text(text: &str) -> Option<LineCol> {
    let bytes = text.as_bytes();
    let mut i = 0usize;
    let mut best = None;

    while i < bytes.len() {
        if bytes[i] != b':' {
            i += 1;
            continue;
        }
        let Some((line, after_line)) = parse_u32_from(bytes, i + 1) else {
            i += 1;
            continue;
        };
        if after_line >= bytes.len() || bytes[after_line] != b':' {
            i += 1;
            continue;
        }
        let Some((column, after_column)) = parse_u32_from(bytes, after_line + 1) else {
            i += 1;
            continue;
        };
        if line == 0 || column == 0 {
            i += 1;
            continue;
        }

        let end_column = if after_column < bytes.len() && bytes[after_column] == b'-' {
            parse_u32_from(bytes, after_column + 1)
                .map(|(value, _)| value)
                .filter(|value| *value > 0)
        } else {
            None
        };

        best = Some(LineCol {
            line,
            column,
            end_column,
        });
        i += 1;
    }

    best
}

fn parse_u32_from(bytes: &[u8], mut index: usize) -> Option<(u32, usize)> {
    if index >= bytes.len() || !bytes[index].is_ascii_digit() {
        return None;
    }
    let mut value = 0u32;
    while index < bytes.len() && bytes[index].is_ascii_digit() {
        value = value
            .saturating_mul(10)
            .saturating_add((bytes[index] - b'0') as u32);
        index += 1;
    }
    Some((value, index))
}

fn line_col_to_byte_offset_clamped(source: &str, line: u32, column: u32) -> Option<usize> {
    let (line_start, line_end) = line_bounds(source, line)?;
    if column <= 1 {
        return Some(line_start);
    }

    let mut current_col = 1u32;
    for (rel, _) in source[line_start..line_end].char_indices() {
        if current_col == column {
            return Some(line_start + rel);
        }
        current_col = current_col.saturating_add(1);
    }

    Some(line_end)
}

fn line_bounds(source: &str, target_line: u32) -> Option<(usize, usize)> {
    if target_line == 0 {
        return None;
    }

    let mut line = 1u32;
    let mut start = 0usize;
    for (idx, ch) in source.char_indices() {
        if line == target_line && ch == '\n' {
            return Some((start, idx));
        }
        if ch == '\n' {
            line = line.saturating_add(1);
            start = idx + 1;
        }
    }
    if line == target_line {
        return Some((start, source.len()));
    }
    None
}

/// Extract the first lookahead token type from a parol cause string.
fn extract_first_token_type_from_cause(cause: &str) -> Option<String> {
    if !cause.starts_with("LA(") {
        return None;
    }
    let la_colon = cause.find("): ")?;
    let after_colon = &cause[la_colon + 3..];
    let (_, token_type) = extract_single_token(after_colon)?;
    Some(token_type)
}

/// Extract the token name from a parol cause string.
/// Cause format examples:
/// - "LA(1): inner (Inner) at /path:1:10-15[1] LA(2): ..."
/// - "LA(1): Test\n(LBracket...) at path:line..."
/// - "LA(1): ; (Semicolon) at path:line..."
fn extract_token_from_cause(cause: &str) -> Option<String> {
    if !cause.starts_with("LA(") {
        return None;
    }

    // Find "LA(n): " pattern and extract what follows
    // Format is "LA(1): token (Type)" or "LA(1): token\n(Type)"
    if let Some(la_colon) = cause.find("): ") {
        let after_colon = &cause[la_colon + 3..];

        // Find the minimum position of any delimiter
        let delimiters = [
            after_colon.find('\n'),
            after_colon.find(" ("),
            after_colon.find(" at "),
        ];

        let end_pos = delimiters
            .into_iter()
            .flatten() // Filter out None values
            .min()
            .unwrap_or(after_colon.len());

        let token = after_colon[..end_pos].trim();
        if !token.is_empty() && token != "$" {
            return Some(token.to_string());
        }
    }

    None
}

/// Clean up cryptic token names to be more readable.
fn clean_token_name(name: &str) -> String {
    // Map internal token names to readable names
    match name.to_lowercase().as_str() {
        "semicolon" => ";".to_string(),
        "comma" => ",".to_string(),
        "dot" => ".".to_string(),
        "colon" => ":".to_string(),
        "lparen" => "(".to_string(),
        "rparen" => ")".to_string(),
        "lbracket" => "[".to_string(),
        "rbracket" => "]".to_string(),
        "lbrace" => "{".to_string(),
        "rbrace" => "}".to_string(),
        "equ" => "=".to_string(),
        "plus" => "+".to_string(),
        "minus" => "-".to_string(),
        "star" => "*".to_string(),
        "slash" => "/".to_string(),
        "endofinput" => "end of input".to_string(),
        "inner" => "inner (keyword)".to_string(),
        "outer" => "outer (keyword)".to_string(),
        _ => {
            // If it looks like an identifier or keyword, return as-is (lowercase)
            if name.chars().all(|c| c.is_alphanumeric() || c == '_') {
                name.to_lowercase()
            } else {
                name.to_string()
            }
        }
    }
}

fn location_to_span(loc: &parol_runtime::lexer::Location) -> Span {
    // Use byte offsets from parol's Location
    let start = BytePos(loc.start as usize);
    let end = BytePos(loc.end as usize);
    // Use SourceId(0) as a placeholder - the actual source is tracked separately
    Span::new(SourceId(0), start, end)
}

fn ast_location_to_span(location: &ast::Location) -> Span {
    let start = BytePos(location.start as usize);
    let mut end = BytePos(location.end as usize);
    if end.0 <= start.0 {
        end = BytePos(start.0.saturating_add(1));
    }
    Span::new(SourceId(0), start, end)
}

/// Format a parse error with source context using miette.
pub fn format_parse_error(err: &ParseError, source_name: &str, source: &str) -> String {
    let diag = err.to_diagnostic();
    let report = diag.to_miette(source_name, source);
    format!("{:?}", miette::Report::new(report))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_syntax_error_diagnostic() {
        let span = Span::from_offsets(SourceId(0), 10, 15);
        let err = ParseError::SyntaxError {
            message: "unexpected token".to_string(),
            expected: vec!["SEMICOLON".to_string(), "END".to_string()],
            unexpected: Some("COMMA".to_string()),
            span,
        };
        let diag = err.to_diagnostic();
        assert!(diag.is_error());
        assert_eq!(diag.code, Some("EP001".to_string()));
        assert!(diag.message.contains("unexpected"));
        assert!(!diag.notes.is_empty());
    }

    #[test]
    fn test_extract_token_from_cause() {
        // Test format with newline
        let cause1 = "LA(1): Test\n(LBracket...) at path:1:7-11";
        assert_eq!(extract_token_from_cause(cause1), Some("Test".to_string()));

        // Test format with space and parens
        let cause2 = "LA(1): inner (Inner) at path:2:10-15";
        assert_eq!(extract_token_from_cause(cause2), Some("inner".to_string()));

        // Test format with semicolon token
        let cause3 = "LA(1): ; (Semicolon) at path:2:15-16";
        assert_eq!(extract_token_from_cause(cause3), Some(";".to_string()));

        // Test end of input
        let cause4 = "LA(1): $ (EndOfInput) at path:0:0-0";
        assert_eq!(extract_token_from_cause(cause4), None); // $ is filtered out
    }

    #[test]
    fn test_clean_token_name() {
        assert_eq!(clean_token_name("Semicolon"), ";");
        assert_eq!(clean_token_name("Inner"), "inner (keyword)");
        assert_eq!(clean_token_name("LParen"), "(");
        assert_eq!(clean_token_name("someIdentifier"), "someidentifier");
    }

    #[test]
    fn test_parse_line_col_from_text_prefers_last_location() {
        let text = "LA(1): end (End) at __input.mo:7:1-4[1]";
        assert_eq!(
            parse_line_col_from_text(text),
            Some(LineCol {
                line: 7,
                column: 1,
                end_column: Some(4)
            })
        );
    }

    #[test]
    fn test_span_from_cause_location_recovers_source_offsets() {
        let source = "model Ball\n  Real x;\nend Ball;\n";
        let span = span_from_cause_location("LA(1): end (End) at __input.mo:3:1-4[1]", source)
            .expect("expected span recovery");
        assert!(span.start.0 > 0, "expected non-dummy start offset");
        assert!(span.end.0 > span.start.0, "expected non-empty span");
    }

    #[test]
    fn test_should_replace_with_recovered_span_for_near_origin_placeholder() {
        let original = Span::from_offsets(SourceId(0), 1, 1);
        let recovered = Span::from_offsets(SourceId(0), 40, 43);
        assert!(should_replace_with_recovered_span(original, recovered));
    }
}
