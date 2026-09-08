//! Phase-local error types for parsing.
//!
//! Error codes: EP0xx for parse phase (per SPEC_0008).

use parol_runtime::errors::{ParolError, ParserError, SyntaxError};
use rumoca_core::{BytePos, SourceId, Span};
use rumoca_core::{Diagnostic, PhaseError, PrimaryLabel};
use rumoca_ir_ast as ast;

/// Parse-phase semantic error carrying a concrete source span.
#[derive(Debug, Clone)]
pub struct ParseSemanticError {
    pub message: String,
    pub span: Span,
}

impl ParseSemanticError {
    pub fn from_token(message: impl Into<String>, token: &rumoca_core::Token) -> Self {
        Self::from_location(message, &token.location)
    }

    pub fn from_location(message: impl Into<String>, location: &rumoca_core::Location) -> Self {
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

/// A source iterator that omitted its range (`for i` rather than `for i in r`).
///
/// MLS 3.7 §11.2.2.1 deduces an omitted range from the dimensions of the
/// expressions the iterator subscripts. Rumoca has no such inference, so the
/// omission is refused at the source boundary. This is a distinct error type,
/// not a [`ParseSemanticError`], so [`convert_parol_error`] can issue the
/// dedicated `EP004` instead of collapsing the refusal into a generic `EP001`
/// syntax error that any unrelated typo would also satisfy.
#[derive(Debug, Clone)]
pub struct OmittedIterationRange {
    /// The iterator whose range the source omitted.
    pub iterator: String,
    /// The iterator token's own span.
    pub span: Span,
}

impl std::fmt::Display for OmittedIterationRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", implicit_iteration_range_message(&self.iterator))
    }
}

impl std::error::Error for OmittedIterationRange {}

fn implicit_iteration_range_message(iterator: &str) -> String {
    format!(
        "iterator `{iterator}` omits its range; implicit iteration ranges \
         (MLS 3.7 §11.2.2.1) are not yet supported, so write `{iterator} in <range>`"
    )
}

/// Sole issuer of the omitted-iterator-range refusal.
///
/// The parser's single `ForIndex` converter calls this and nothing else builds
/// an [`OmittedIterationRange`], so for-equations, for-statements, array
/// comprehensions, and reduction arguments all refuse the omission identically.
pub(crate) fn omitted_iteration_range_error(iterator: &rumoca_core::Token) -> anyhow::Error {
    anyhow::Error::new(OmittedIterationRange {
        iterator: iterator.text.to_string(),
        span: ast_location_to_span(&iterator.location),
    })
}

/// Build an anyhow error that retains parse semantic span information.
pub fn semantic_error_from_token(
    message: impl Into<String>,
    token: &rumoca_core::Token,
) -> anyhow::Error {
    anyhow::Error::new(ParseSemanticError::from_token(message, token))
}

/// Build an anyhow error that retains parse semantic span information.
pub fn semantic_error_from_location(
    message: impl Into<String>,
    location: &rumoca_core::Location,
) -> anyhow::Error {
    anyhow::Error::new(ParseSemanticError::from_location(message, location))
}

/// Build a parse semantic error when a source location may be unavailable.
pub fn semantic_error_from_optional_location(
    message: impl Into<String>,
    location: Option<&rumoca_core::Location>,
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
    /// An iterator declared without the range Rumoca still requires.
    ///
    /// Distinct from [`Self::SyntaxError`] on purpose: the source is
    /// grammatically well formed, and a witness for this refusal must not be
    /// satisfiable by unrelated syntax corruption.
    UnsupportedImplicitIterationRange { iterator: String, span: Span },
    /// No AST was produced despite successful parse.
    NoAstProduced { span: Span },
    /// IO error while reading file.
    IoError {
        path: String,
        message: String,
        span: Span,
    },
}

impl ParseError {
    /// Parser-owned provenance for this failure.
    pub fn span(&self) -> Span {
        match self {
            Self::SyntaxError { span, .. }
            | Self::UnsupportedImplicitIterationRange { span, .. }
            | Self::NoAstProduced { span }
            | Self::IoError { span, .. } => *span,
        }
    }
}

const EP001_SYNTAX_ERROR: &str = "EP001";
const EP002_NO_AST_PRODUCED: &str = "EP002";
const EP003_IO_ERROR: &str = "EP003";
const EP004_UNSUPPORTED_IMPLICIT_ITERATION_RANGE: &str = "EP004";

pub(crate) fn default_parse_span(source: SourceId) -> Span {
    Span::from_offsets(source, 0, 1)
}

fn normalize_span(span: Span) -> Span {
    if span.end.0 <= span.start.0 {
        Span::from_offsets(span.source, span.start.0, span.start.0.saturating_add(1))
    } else {
        span
    }
}

fn fallback_span_from_source(source: &str, source_id: SourceId) -> Span {
    if source.is_empty() {
        return default_parse_span(source_id);
    }

    if let Some((start, ch)) = source.char_indices().find(|(_, ch)| !ch.is_whitespace()) {
        return Span::from_offsets(source_id, start, start + ch.len_utf8());
    }

    let end = source.chars().next().map_or(1, |ch| ch.len_utf8());
    Span::from_offsets(source_id, 0, end)
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
                let label_msg = unexpected
                    .as_ref()
                    .map(|u| format!("unexpected `{}`", u))
                    .unwrap_or_else(|| "error here".to_string());

                let mut diag = Diagnostic::error(
                    EP001_SYNTAX_ERROR,
                    message,
                    PrimaryLabel::new(normalize_span(*span)).with_message(label_msg),
                );

                // Add expected tokens as a note
                if let Some(expected_str) = format_expected(expected) {
                    diag = diag.with_note(expected_str);
                }

                diag
            }
            Self::UnsupportedImplicitIterationRange { iterator, span } => Diagnostic::error(
                EP004_UNSUPPORTED_IMPLICIT_ITERATION_RANGE,
                implicit_iteration_range_message(iterator),
                PrimaryLabel::new(normalize_span(*span)).with_message("iterator has no range"),
            ),
            Self::NoAstProduced { span } => Diagnostic::error(
                EP002_NO_AST_PRODUCED,
                "parsing succeeded but no AST was produced",
                PrimaryLabel::new(*span).with_message("at start of input"),
            ),
            Self::IoError {
                path,
                message,
                span,
            } => Diagnostic::error(
                EP003_IO_ERROR,
                format!("failed to read `{}`: {}", path, message),
                PrimaryLabel::new(*span).with_message("while reading source input"),
            ),
        }
    }
}

/// Convert a parol error to our parse error(s).
pub(crate) fn convert_parol_error(
    err: ParolError,
    source: &str,
    source_id: SourceId,
) -> Vec<ParseError> {
    match err {
        ParolError::ParserError(parser_err) => convert_parser_error(parser_err, source, source_id),
        ParolError::LexerError(lexer_err) => {
            vec![ParseError::SyntaxError {
                message: format!("lexer error: {}", lexer_err),
                expected: vec![],
                unexpected: None,
                span: fallback_span_from_source(source, source_id),
            }]
        }
        ParolError::UserError(user_err) => {
            // Dispatch on the concrete conversion-failure type. The omitted
            // iterator range is its own type precisely so it does not land in
            // the `ParseSemanticError` bucket below, which renders as `EP001`.
            if let Some(omitted) = user_err.downcast_ref::<OmittedIterationRange>() {
                return vec![ParseError::UnsupportedImplicitIterationRange {
                    iterator: omitted.iterator.clone(),
                    span: omitted.span,
                }];
            }
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
                span: fallback_span_from_source(source, source_id),
            }]
        }
    }
}

fn convert_parser_error(err: ParserError, source: &str, source_id: SourceId) -> Vec<ParseError> {
    match err {
        ParserError::SyntaxErrors { entries } => entries
            .into_iter()
            .map(|error| convert_syntax_error(error, source))
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
                span: fallback_span_from_source(source, source_id),
            }]
        }
        other => {
            vec![ParseError::SyntaxError {
                message: format!("parse error: {}", other),
                expected: vec![],
                unexpected: None,
                span: fallback_span_from_source(source, source_id),
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
    let unexpected_token = err.unexpected_tokens.first();
    let span = unexpected_token
        .map(|token| location_to_span(&token.token))
        .unwrap_or_else(|| location_to_span(&err.error_location));
    let expects_semicolon = err
        .expected_tokens
        .iter()
        .map(|token| clean_token_name(token))
        .any(|token| token == ";");

    // Parol already provides the unexpected token as structured parser data.
    // Consuming it directly keeps diagnostics independent of Parol's Display
    // formatting and makes the parser the sole owner of source interpretation.
    let first_token = unexpected_token
        .and_then(|token| source.get(token.token.start as usize..token.token.end as usize))
        .filter(|text| !text.is_empty() && *text != "$")
        .unwrap_or_default();
    let first_token_type = unexpected_token
        .map(|token| token.token_type.as_str())
        .unwrap_or_default();
    let token_lower = first_token.to_lowercase();
    let keyword_in_context = !first_token.is_empty()
        && RESERVED_KEYWORDS.contains(&token_lower.as_str())
        && first_token_type.to_lowercase() == token_lower;

    let unexpected = if first_token.is_empty() || first_token == "$" {
        None
    } else {
        Some(first_token.to_string())
    };

    let message = if keyword_in_context && !expects_semicolon {
        format!(
            "`{}` is a reserved keyword in Modelica (MLS §2.3.3) and cannot be used as an identifier",
            token_lower
        )
    } else if first_token == "$" || first_token.is_empty() {
        "unexpected end of input".to_string()
    } else {
        // Use the literal token text (not `clean_token_name`, which lowercases
        // identifiers and would render the user's `Ball2` as `ball2`). This
        // keeps the message consistent with the primary label built from
        // `unexpected`.
        format!("unexpected `{}`", first_token)
    };

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
    let source_name = loc.file_name.to_string_lossy();
    normalize_span(Span::new(
        rumoca_core::source_id_for_name(source_name.as_ref()),
        start,
        end,
    ))
}

fn ast_location_to_span(location: &rumoca_core::Location) -> Span {
    let start = BytePos(location.start as usize);
    let mut end = BytePos(location.end as usize);
    if end.0 <= start.0 {
        end = BytePos(start.0.saturating_add(1));
    }
    Span::new(location.source, start, end)
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
        let span = Span::from_offsets(
            SourceId::from_source_name("phase_parse_errors_source_0.mo"),
            10,
            15,
        );
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
    fn omitted_iteration_range_is_not_a_generic_syntax_error() {
        // Mutation this fails against: routing `OmittedIterationRange` through
        // the `ParseSemanticError`/`SyntaxError` bucket, or reusing `EP001` for
        // the new variant. Both would make the code read `EP001` here.
        let span = Span::from_offsets(
            SourceId::from_source_name("phase_parse_errors_source_1.mo"),
            21,
            22,
        );
        let err = ParseError::UnsupportedImplicitIterationRange {
            iterator: "i".to_string(),
            span,
        };
        let diag = err.to_diagnostic();
        assert!(diag.is_error());
        assert_eq!(diag.code, Some("EP004".to_string()));
        assert!(
            diag.message.contains("`i`") && diag.message.contains("§11.2.2.1"),
            "EP004 must name the iterator and its MLS rule: {}",
            diag.message
        );
        assert_eq!(
            err.span(),
            span,
            "EP004 must retain the iterator token's own provenance"
        );
    }

    #[test]
    fn parol_user_errors_keep_their_two_dispositions_apart() {
        // Mutation this fails against: deleting the `OmittedIterationRange`
        // arm from `convert_parol_error`. The omission then matches neither
        // remaining downcast and falls through to the generic `user_err`
        // bucket, which renders as `EP001`, so both inputs collapse onto one
        // code. Reordering the two downcast blocks is *not* the mutation: they
        // test disjoint concrete types, so their order is unobservable.
        let source_id = SourceId::from_source_name("phase_parse_errors_source_2.mo");
        let token = rumoca_core::Token {
            text: std::sync::Arc::from("j"),
            location: rumoca_core::Location {
                start_line: 4,
                start_column: 7,
                end_line: 4,
                end_column: 8,
                start: 30,
                end: 31,
                source: source_id,
            },
            token_number: 0,
            token_type: 0,
        };

        let omitted = convert_parol_error(
            ParolError::UserError(omitted_iteration_range_error(&token)),
            "model M end M;",
            source_id,
        );
        let semantic = convert_parol_error(
            ParolError::UserError(semantic_error_from_token("some other refusal", &token)),
            "model M end M;",
            source_id,
        );

        let codes = |errors: &[ParseError]| -> Vec<Option<String>> {
            errors.iter().map(|e| e.to_diagnostic().code).collect()
        };
        assert_eq!(codes(&omitted), vec![Some("EP004".to_string())]);
        assert_eq!(codes(&semantic), vec![Some("EP001".to_string())]);
    }

    #[test]
    fn test_clean_token_name() {
        assert_eq!(clean_token_name("Semicolon"), ";");
        assert_eq!(clean_token_name("Inner"), "inner (keyword)");
        assert_eq!(clean_token_name("LParen"), "(");
        assert_eq!(clean_token_name("someIdentifier"), "someidentifier");
    }
}
