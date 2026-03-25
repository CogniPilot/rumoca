//! Diagnostics handler for Modelica files.

use lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range};
use rumoca_session::Session;
use rumoca_session::compile::SemanticDiagnosticsMode;
#[cfg(test)]
use rumoca_session::compile::SourceRootKind;
use rumoca_session::compile::core as rumoca_core;
use rumoca_session::compile::core::{
    Diagnostic as CommonDiagnostic, DiagnosticSeverity as CommonSeverity, SourceMap,
};
use rumoca_session::parsing::ast;
use rumoca_session::parsing::{ParseError, parse_source_to_ast_with_errors};
use rumoca_tool_lint::{LintLevel, LintMessage, LintOptions, lint};
use serde_json::json;
use std::collections::HashSet;

const MAX_PARSE_DIAGNOSTICS: usize = 8;

/// Compute diagnostics for Modelica source code.
///
/// Returns a list of LSP diagnostics including syntax errors and lint warnings.
/// When a `session` is provided, also tries compiling each model in the source
/// and reports compilation errors (resolve, typecheck, flatten failures).
pub fn compute_diagnostics(
    source: &str,
    file_name: &str,
    session: Option<&mut Session>,
) -> Vec<Diagnostic> {
    compute_diagnostics_with_mode(
        source,
        file_name,
        session,
        SemanticDiagnosticsMode::Standard,
    )
}

pub(crate) fn compute_diagnostics_with_mode(
    source: &str,
    file_name: &str,
    session: Option<&mut Session>,
    mode: SemanticDiagnosticsMode,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // Parse check with structured errors (for precise ranges in editor markers).
    let ast = match parse_source_to_ast_with_errors(source, file_name) {
        Ok(ast) => ast,
        Err(parse_errors) => {
            for error in parse_errors.iter().take(MAX_PARSE_DIAGNOSTICS) {
                diagnostics.push(parse_error_to_diagnostic(error, source));
            }
            add_parse_overflow_diagnostic(&mut diagnostics, parse_errors.len());
            return diagnostics;
        }
    };

    // Run linter on successfully parsed source
    let lint_options = LintOptions::default();
    let lint_messages = lint(source, file_name, &lint_options);
    for msg in lint_messages {
        diagnostics.push(lint_to_diagnostic(&msg));
    }

    let Some(session) = session else {
        return diagnostics;
    };

    // Compilation diagnostics (session available).
    session.update_document(file_name, source);
    let mut seen_keys = HashSet::new();
    for model_name in collect_diagnostic_target_names(&ast) {
        let model_diags = session.semantic_diagnostics_query(&model_name, mode);
        let source_map = model_diags.source_map.as_ref();
        let is_global_resolution_failure = model_diags.global_resolution_failure;
        for diag in model_diags.diagnostics {
            let Some(lsp_diag) = common_diagnostic_to_lsp(&diag, source, file_name, source_map)
            else {
                continue;
            };
            let key = diagnostic_key(&lsp_diag);
            if !seen_keys.insert(key) {
                continue;
            }
            diagnostics.push(lsp_diag);
        }
        if is_global_resolution_failure {
            // Resolve/merge level failures are global to the current session.
            // Emitting them once avoids N-way duplication across every class target.
            break;
        }
    }

    diagnostics
}

#[cfg(feature = "server")]
pub(crate) fn common_diagnostics_for_file(
    diagnostics: &[CommonDiagnostic],
    file_name: &str,
    source_map: &SourceMap,
) -> Vec<Diagnostic> {
    let Some(source_id) = source_map.get_id(file_name) else {
        return Vec::new();
    };
    let Some((_, source)) = source_map.get_source(source_id) else {
        return Vec::new();
    };

    let mut out = Vec::new();
    let mut seen_keys = HashSet::new();
    for diag in diagnostics {
        if diag.labels.is_empty()
            || !diag
                .labels
                .iter()
                .any(|label| label_in_file(label, file_name, Some(source_map)))
        {
            continue;
        }
        let Some(lsp_diag) = common_diagnostic_to_lsp(diag, source, file_name, Some(source_map))
        else {
            continue;
        };
        let key = diagnostic_key(&lsp_diag);
        if !seen_keys.insert(key) {
            continue;
        }
        out.push(lsp_diag);
    }
    out
}

fn diagnostic_key(diag: &Diagnostic) -> String {
    let code = diag
        .code
        .as_ref()
        .map(|c| match c {
            NumberOrString::String(s) => s.as_str(),
            NumberOrString::Number(_) => "",
        })
        .unwrap_or("");
    format!(
        "{}:{}:{}:{}:{}:{}",
        diag.range.start.line,
        diag.range.start.character,
        diag.range.end.line,
        diag.range.end.character,
        code,
        diag.message
    )
}

/// Convert a parse error to LSP diagnostic with a precise source range.
fn parse_error_to_diagnostic(error: &ParseError, source: &str) -> Diagnostic {
    let (range, code, message, precise_range) = match error {
        ParseError::SyntaxError {
            message,
            expected,
            span,
            ..
        } => {
            let normalized = normalize_parse_message(message);
            let expected_msg = format_expected_tokens(expected);
            let msg = if expected_msg.is_empty() {
                normalized
            } else {
                format!("{normalized} ({expected_msg})")
            };
            let span_range = span_to_range(source, span.start.0, span.end.0);
            let message_range = range_from_message_location(message, source)
                .or_else(|| range_from_textual_line_hint(message, source));
            let (range, precise_range) = if is_placeholder_parse_span(span.start.0, span.end.0) {
                match message_range {
                    Some(range) => (range, true),
                    None => (
                        Range {
                            start: Position::new(0, 0),
                            end: Position::new(0, 1),
                        },
                        false,
                    ),
                }
            } else if range_is_top_left(&span_range) {
                match message_range {
                    Some(range) => (range, true),
                    None => (span_range, true),
                }
            } else {
                (span_range, true)
            };
            (range, "EP001".to_string(), msg, precise_range)
        }
        ParseError::NoAstProduced => (
            Range {
                start: Position::new(0, 0),
                end: Position::new(0, 1),
            },
            "EP002".to_string(),
            "parsing succeeded but no AST was produced".to_string(),
            false,
        ),
        ParseError::IoError { path, message } => (
            Range {
                start: Position::new(0, 0),
                end: Position::new(0, 1),
            },
            "EP003".to_string(),
            format!("failed to read `{path}`: {message}"),
            false,
        ),
    };

    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        code: Some(NumberOrString::String(code)),
        source: Some("rumoca".to_string()),
        message: collapse_whitespace(&message),
        related_information: None,
        tags: None,
        code_description: None,
        data: Some(json!({ "precise_range": precise_range })),
    }
}

fn add_parse_overflow_diagnostic(diagnostics: &mut Vec<Diagnostic>, total_parse_errors: usize) {
    if total_parse_errors <= MAX_PARSE_DIAGNOSTICS {
        return;
    }
    let suppressed = total_parse_errors - MAX_PARSE_DIAGNOSTICS;
    diagnostics.push(Diagnostic {
        range: Range {
            start: Position::new(0, 0),
            end: Position::new(0, 1),
        },
        severity: Some(DiagnosticSeverity::INFORMATION),
        code: Some(NumberOrString::String("EP000".to_string())),
        source: Some("rumoca".to_string()),
        message: format!("{suppressed} additional parse errors omitted"),
        related_information: None,
        tags: None,
        code_description: None,
        data: Some(json!({ "precise_range": false })),
    });
}

fn format_expected_tokens(expected: &[String]) -> String {
    if expected.is_empty() {
        return String::new();
    }
    const MAX_EXPECTED: usize = 6;
    let mut items: Vec<&str> = expected.iter().map(|s| s.as_str()).collect();
    items.sort_unstable();
    items.dedup();
    if items.len() > MAX_EXPECTED {
        let omitted = items.len() - MAX_EXPECTED;
        let head = items[..MAX_EXPECTED].join(", ");
        format!("expected one of: {head}, ... (+{omitted} more)")
    } else if items.len() == 1 {
        format!("expected {}", items[0])
    } else {
        format!("expected one of: {}", items.join(", "))
    }
}

fn collapse_whitespace(message: &str) -> String {
    message.split_whitespace().collect::<Vec<_>>().join(" ")
}

fn collect_diagnostic_target_names(ast: &ast::StoredDefinition) -> Vec<String> {
    let mut names = Vec::new();
    let root_prefix = ast
        .within
        .as_ref()
        .map(ToString::to_string)
        .unwrap_or_default();
    for (name, class) in &ast.classes {
        collect_diagnostic_targets_from_class(&root_prefix, name, class, &mut names);
    }
    names
}

fn collect_diagnostic_targets_from_class(
    prefix: &str,
    name: &str,
    class: &ast::ClassDef,
    names: &mut Vec<String>,
) {
    let qualified = if prefix.is_empty() {
        name.to_string()
    } else {
        format!("{prefix}.{name}")
    };
    if should_compile_for_diagnostics(class, &qualified) {
        names.push(qualified.clone());
    }
    for (nested_name, nested_class) in &class.classes {
        collect_diagnostic_targets_from_class(&qualified, nested_name, nested_class, names);
    }
}

fn should_compile_for_diagnostics(_class: &ast::ClassDef, qualified_name: &str) -> bool {
    if is_builtin_operator(qualified_name) {
        return false;
    }
    true
}

fn is_builtin_operator(name: &str) -> bool {
    name.contains("Connections.branch")
        || name.contains("Connections.root")
        || name.contains("Connections.potentialRoot")
        || name.contains("Connections.isRoot")
        || name.contains("Connections.rooted")
}

fn common_diagnostic_to_lsp(
    diag: &CommonDiagnostic,
    source: &str,
    file_name: &str,
    source_map: Option<&SourceMap>,
) -> Option<Diagnostic> {
    let range = preferred_range(diag, source, file_name, source_map)?;
    let precise_range = true;
    let severity = match diag.severity {
        CommonSeverity::Error => DiagnosticSeverity::ERROR,
        CommonSeverity::Warning => DiagnosticSeverity::WARNING,
        CommonSeverity::Note => DiagnosticSeverity::INFORMATION,
    };

    Some(Diagnostic {
        range,
        severity: Some(severity),
        code: diag.code.clone().map(NumberOrString::String),
        source: Some("rumoca".to_string()),
        message: summarize_message(&diag.message),
        related_information: None,
        tags: None,
        code_description: None,
        data: Some(json!({ "precise_range": precise_range })),
    })
}

fn preferred_range(
    diag: &CommonDiagnostic,
    source: &str,
    file_name: &str,
    source_map: Option<&SourceMap>,
) -> Option<Range> {
    if diag.labels.is_empty() {
        eprintln!(
            "[rumoca-lsp] unlabeled diagnostic, using top-left range: code={:?} message={}",
            diag.code, diag.message
        );
        return Some(top_left_range(source));
    }

    // Prefer labels that belong to the current file.
    let mut labels_in_file = diag
        .labels
        .iter()
        .filter(|label| label_in_file(label, file_name, source_map));
    let label = labels_in_file.find(|label| label.primary).or_else(|| {
        diag.labels
            .iter()
            .find(|label| label_in_file(label, file_name, source_map))
    })?;
    let label_source = label_source_text(source, file_name, source_map, label);
    strict_span_to_range(diag, label_source, label)
        .or_else(|| strict_span_to_range(diag, source, label))
}

fn label_in_file(
    label: &rumoca_core::Label,
    file_name: &str,
    source_map: Option<&SourceMap>,
) -> bool {
    let Some(source_map) = source_map else {
        return true;
    };
    source_map
        .get_source(label.span.source)
        .map(|(name, _)| name == file_name)
        .unwrap_or(false)
}

fn span_to_range(source: &str, start_byte: usize, end_byte: usize) -> Range {
    let start = byte_offset_to_position(source, start_byte);
    let mut end = byte_offset_to_position(source, end_byte);
    if (end.line < start.line) || (end.line == start.line && end.character <= start.character) {
        end = Position::new(start.line, start.character.saturating_add(1));
    }
    Range { start, end }
}

fn label_source_text<'a>(
    source: &'a str,
    file_name: &str,
    source_map: Option<&'a SourceMap>,
    label: &rumoca_core::Label,
) -> &'a str {
    let Some(source_map) = source_map else {
        return source;
    };
    let Some((name, content)) = source_map.get_source(label.span.source) else {
        return source;
    };
    if name == file_name { content } else { source }
}

fn strict_span_to_range(
    diag: &CommonDiagnostic,
    source: &str,
    label: &rumoca_core::Label,
) -> Option<Range> {
    label_range(source, label).or_else(|| {
        let start = label.span.start.0;
        let end = label.span.end.0;
        eprintln!(
            "[rumoca-lsp] dropping invalid label span: code={:?} message={} start={} end={} source_len={}",
            diag.code,
            diag.message,
            start,
            end,
            source.len()
        );
        None
    })
}

fn label_range(source: &str, label: &rumoca_core::Label) -> Option<Range> {
    let start = label.span.start.0;
    let end = label.span.end.0;
    if end <= start || end > source.len() {
        return None;
    }
    Some(span_to_range(source, start, end))
}

fn top_left_range(source: &str) -> Range {
    let end_char = source.chars().next().map_or(0, |ch| ch.len_utf16() as u32);
    Range {
        start: Position::new(0, 0),
        end: Position::new(0, end_char),
    }
}

fn is_placeholder_parse_span(start_byte: usize, end_byte: usize) -> bool {
    start_byte == 0 && end_byte <= 1
}

fn range_is_top_left(range: &Range) -> bool {
    range.start.line == 0 && range.start.character == 0
}

fn summarize_message(message: &str) -> String {
    const MAX_MESSAGE_LEN: usize = 180;
    let compact = collapse_whitespace(message);
    if compact.is_empty() {
        return compact;
    }

    // Keep related-failure summaries concise.
    if let Some((head, _)) = compact.split_once(" Related failures") {
        return truncate_message(head, MAX_MESSAGE_LEN);
    }

    // Collapse semicolon-delimited aggregates to first two items.
    let parts: Vec<&str> = compact
        .split(';')
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .collect();
    if parts.len() > 2 {
        let head = format!(
            "{}; {}; ... (+{} more)",
            parts[0],
            parts[1],
            parts.len() - 2
        );
        return truncate_message(&head, MAX_MESSAGE_LEN);
    }

    truncate_message(&compact, MAX_MESSAGE_LEN)
}

fn truncate_message(message: &str, max_len: usize) -> String {
    if message.chars().count() <= max_len {
        return message.to_string();
    }
    let mut out = String::with_capacity(max_len);
    for (idx, ch) in message.chars().enumerate() {
        if idx + 3 >= max_len {
            break;
        }
        out.push(ch);
    }
    out.push_str("...");
    out
}

fn normalize_parse_message(message: &str) -> String {
    let compact = collapse_whitespace(message);
    let lowered = compact.to_ascii_lowercase();
    if compact.contains("is a reserved keyword in Modelica")
        && (contains_quoted_keyword(&lowered, "equation")
            || contains_quoted_keyword(&lowered, "algorithm")
            || contains_quoted_keyword(&lowered, "initial")
            || contains_quoted_keyword(&lowered, "public")
            || contains_quoted_keyword(&lowered, "protected")
            || contains_quoted_keyword(&lowered, "annotation")
            || contains_quoted_keyword(&lowered, "external")
            || contains_quoted_keyword(&lowered, "end")
            || contains_quoted_keyword(&lowered, "der"))
    {
        if contains_quoted_keyword(&lowered, "equation") {
            return "unexpected `equation` (possible missing `;` before equation section)"
                .to_string();
        }
        if contains_quoted_keyword(&lowered, "algorithm") {
            return "unexpected `algorithm` (possible missing `;` before algorithm section)"
                .to_string();
        }
        if contains_quoted_keyword(&lowered, "end") {
            return "unexpected `end` (possible missing `;` before end statement)".to_string();
        }
        if contains_quoted_keyword(&lowered, "der") {
            return "unexpected `der` (possible missing `;` between equations)".to_string();
        }
        return "unexpected section keyword (possible missing `;` before this section)".to_string();
    }

    if compact.starts_with("syntax error: LA(") {
        if let Some(token) = extract_la1_token(&compact) {
            if token.eq_ignore_ascii_case("equation") {
                return "unexpected `equation` (possible missing `;` before equation section)"
                    .to_string();
            }
            return format!("unexpected `{}`", token);
        }
        return "syntax error".to_string();
    }

    compact
}

fn contains_quoted_keyword(message: &str, keyword: &str) -> bool {
    let backtick = format!("`{keyword}`");
    let apostrophe = format!("'{keyword}'");
    message.contains(&backtick) || message.contains(&apostrophe)
}

fn extract_la1_token(message: &str) -> Option<String> {
    let marker = "LA(1):";
    let start = message.find(marker)?;
    let rest = message[start + marker.len()..].trim_start();
    let end = rest
        .find(" (")
        .or_else(|| rest.find(" at "))
        .unwrap_or(rest.len());
    let token = rest[..end].trim();
    if token.is_empty() || token == "$" {
        None
    } else {
        Some(token.to_string())
    }
}

fn range_from_message_location(message: &str, _source: &str) -> Option<Range> {
    let bytes = message.as_bytes();
    let mut i = 0usize;
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
        let Some((column, _after_col)) = parse_u32_from(bytes, after_line + 1) else {
            i += 1;
            continue;
        };
        if line == 0 || column == 0 {
            i += 1;
            continue;
        }
        let start = Position::new(line.saturating_sub(1), column.saturating_sub(1));
        let end = Position::new(start.line, start.character.saturating_add(1));
        return Some(Range { start, end });
    }
    None
}

fn range_from_textual_line_hint(message: &str, source: &str) -> Option<Range> {
    let line_number = extract_first_line_number(message)?;
    if let Some(identifier) = extract_quoted_identifier(message)
        && let Some((start, end)) = find_identifier_on_line(source, line_number, &identifier)
    {
        return Some(span_to_range(source, start, end));
    }
    range_for_line_start(source, line_number)
}

fn extract_quoted_identifier(message: &str) -> Option<String> {
    extract_between_delimiter(message, '\'').or_else(|| extract_between_delimiter(message, '`'))
}

fn extract_between_delimiter(text: &str, delimiter: char) -> Option<String> {
    let start = text.find(delimiter)?;
    let rest = &text[start + delimiter.len_utf8()..];
    let end_rel = rest.find(delimiter)?;
    let candidate = rest[..end_rel].trim();
    if candidate.is_empty() {
        None
    } else {
        Some(candidate.to_string())
    }
}

fn extract_first_line_number(message: &str) -> Option<u32> {
    let lowered = message.to_ascii_lowercase();
    let mut words = lowered.split_whitespace();
    while let Some(word) = words.next() {
        let normalized = word.trim_matches(|c: char| !c.is_ascii_alphabetic());
        if normalized != "line" {
            continue;
        }
        let next = words.next()?;
        let digits: String = next
            .chars()
            .skip_while(|ch| !ch.is_ascii_digit())
            .take_while(|ch| ch.is_ascii_digit())
            .collect();
        if let Ok(line) = digits.parse::<u32>()
            && line > 0
        {
            return Some(line);
        }
    }
    None
}

fn find_identifier_on_line(source: &str, line_number: u32, ident: &str) -> Option<(usize, usize)> {
    if ident.is_empty() {
        return None;
    }
    let (line_start, line_end) = line_bounds(source, line_number)?;
    let line_text = &source[line_start..line_end];
    let mut found = None;
    for (rel_start, _) in line_text.match_indices(ident) {
        let start = line_start + rel_start;
        let end = start + ident.len();
        if is_identifier_boundary(source, start, end) {
            found = Some((start, end));
        }
    }
    found
}

fn is_identifier_boundary(source: &str, start: usize, end: usize) -> bool {
    let left_ok = source[..start]
        .chars()
        .next_back()
        .is_none_or(|c| !is_identifier_char(c));
    let right_ok = source[end..]
        .chars()
        .next()
        .is_none_or(|c| !is_identifier_char(c));
    left_ok && right_ok
}

fn is_identifier_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn range_for_line_start(source: &str, line_number: u32) -> Option<Range> {
    let (line_start, line_end) = line_bounds(source, line_number)?;
    let mut start = line_start;
    for (rel, ch) in source[line_start..line_end].char_indices() {
        if !ch.is_whitespace() {
            start = line_start + rel;
            break;
        }
    }
    let end = source[start..]
        .chars()
        .next()
        .map(|ch| start + ch.len_utf8())
        .unwrap_or(start);
    Some(span_to_range(source, start, end))
}

fn line_bounds(source: &str, line_number: u32) -> Option<(usize, usize)> {
    if line_number == 0 {
        return None;
    }
    let mut current_line = 1u32;
    let mut line_start = 0usize;

    for (idx, ch) in source.char_indices() {
        if current_line == line_number {
            break;
        }
        if ch == '\n' {
            current_line = current_line.saturating_add(1);
            line_start = idx + ch.len_utf8();
        }
    }

    if current_line != line_number {
        return None;
    }

    let line_end = source[line_start..]
        .find('\n')
        .map(|rel| line_start + rel)
        .unwrap_or(source.len());
    Some((line_start, line_end))
}

fn parse_u32_from(bytes: &[u8], mut index: usize) -> Option<(u32, usize)> {
    if index >= bytes.len() || !bytes[index].is_ascii_digit() {
        return None;
    }
    let mut value: u32 = 0;
    while index < bytes.len() && bytes[index].is_ascii_digit() {
        value = value
            .saturating_mul(10)
            .saturating_add((bytes[index] - b'0') as u32);
        index += 1;
    }
    Some((value, index))
}

fn byte_offset_to_position(source: &str, byte_offset: usize) -> Position {
    let clamped = byte_offset.min(source.len());
    let mut line = 0u32;
    let mut col_utf16 = 0u32;

    for (idx, ch) in source.char_indices() {
        if idx >= clamped {
            break;
        }
        if ch == '\n' {
            line = line.saturating_add(1);
            col_utf16 = 0;
        } else {
            col_utf16 = col_utf16.saturating_add(ch.len_utf16() as u32);
        }
    }

    Position::new(line, col_utf16)
}

/// Convert a lint message to LSP diagnostic.
fn lint_to_diagnostic(msg: &LintMessage) -> Diagnostic {
    let severity = match msg.level {
        LintLevel::Error => DiagnosticSeverity::ERROR,
        LintLevel::Warning => DiagnosticSeverity::WARNING,
        LintLevel::Note => DiagnosticSeverity::INFORMATION,
        LintLevel::Help => DiagnosticSeverity::HINT,
    };

    let line = msg.line.saturating_sub(1);
    let column = msg.column.saturating_sub(1);

    Diagnostic {
        range: Range {
            start: Position::new(line, column),
            end: Position::new(line, column + 1),
        },
        severity: Some(severity),
        code: Some(NumberOrString::String(msg.rule.to_string())),
        source: Some("rumoca".to_string()),
        message: msg.message.clone(),
        related_information: None,
        tags: None,
        code_description: None,
        data: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_session::compile::core::{PrimaryLabel, Span};

    #[test]
    fn parse_diagnostics_include_precise_range_and_compact_message() {
        let source = "model M\n  Real x\nequation\n  x = 1;\nend M;\n";
        let diagnostics = compute_diagnostics(source, "input.mo", None);
        assert!(
            !diagnostics.is_empty(),
            "expected parse diagnostics for missing semicolon"
        );
        let first = &diagnostics[0];
        assert_eq!(first.severity, Some(DiagnosticSeverity::ERROR));
        assert_ne!(first.range.start.line, 0, "expected non-zero parse range");
        assert!(!first.message.contains('\n'), "message should be compact");
    }

    #[test]
    fn parse_diagnostics_missing_semicolon_before_equation_has_nonzero_range() {
        let source = "model Ball\n  Real x(start=0);\n  Real v(start=1)\nequation\n  der(x) = v;\nend Ball;\n";
        let diagnostics = compute_diagnostics(source, "input.mo", None);
        assert!(!diagnostics.is_empty(), "expected parse diagnostics");
        let first = &diagnostics[0];
        assert!(
            first.range.start.line > 0 || first.range.start.character > 0,
            "expected range recovered away from line 1 when possible"
        );
        assert!(
            !first.message.contains("`equation` is a reserved keyword"),
            "should avoid reserved-keyword mislabel for section transition"
        );
    }

    #[test]
    fn parse_diagnostics_missing_semicolon_before_end_points_near_end() {
        let source = "model Ball\n  Real x(start=0);\n  Real v(start=1);\nequation\n  der(x) = v;\n  der(v) = -9.81\nend Ball;\n";
        let diagnostics = compute_diagnostics(source, "input.mo", None);
        let first = diagnostics
            .iter()
            .find(|d| d.code == Some(NumberOrString::String("EP001".to_string())))
            .expect("expected parse EP001 diagnostic");
        assert!(
            first.range.start.line >= 5,
            "expected parse error near missing semicolon before `end`, got line {} diag: {:?}",
            first.range.start.line + 1,
            first
        );
        assert!(
            first.message.contains("unexpected `end`")
                || first.message.contains("possible missing `;` before end"),
            "expected compact end-keyword parse message, got: {}",
            first.message
        );
    }

    #[test]
    fn parse_diagnostics_missing_semicolon_before_der_points_near_der_line() {
        let source = "model Ball\n  Real x(start=0);\n  Real v(start=1);\nequation\n  der(x) = v\n  der(v) = -9.81;\nend Ball;\n";
        let diagnostics = compute_diagnostics(source, "input.mo", None);
        let first = diagnostics
            .iter()
            .find(|d| d.code == Some(NumberOrString::String("EP001".to_string())))
            .expect("expected parse EP001 diagnostic");
        assert!(
            first.range.start.line >= 4,
            "expected parse error near second equation, got line {} diag: {:?}",
            first.range.start.line + 1,
            first
        );
        assert!(
            first.message.contains("unexpected `der`")
                || first
                    .message
                    .contains("possible missing `;` between equations"),
            "expected semicolon guidance for der-transition, got: {}",
            first.message
        );
        assert!(
            !first.message.contains("reserved keyword"),
            "expected normalized message instead of reserved keyword wording: {}",
            first.message
        );
    }

    #[test]
    fn parse_diagnostics_section_keyword_typo_points_to_header_line() {
        let source = "model Ball\n  Real x(start=0);\n  Real v(start=1);\nequations\n  der(x) = v;\n  der(v) = -9.81;\nend Ball;\n";
        let diagnostics = compute_diagnostics(source, "input.mo", None);
        let typo = diagnostics
            .iter()
            .find(|d| d.code == Some(NumberOrString::String("EP001".to_string())))
            .unwrap_or_else(|| panic!("expected EP001 diagnostic: {diagnostics:?}"));
        assert!(
            typo.range.start.line >= 3,
            "expected section parse error near header/equation lines, got {:?}",
            typo.range
        );
        assert!(
            typo.message.contains("unknown section keyword `equations`")
                || typo.message.contains("unexpected `der`")
                || typo.message.contains("unexpected `equation`"),
            "expected section keyword or follow-on parse diagnostic, got: {}",
            typo.message
        );
    }

    #[test]
    fn parse_diagnostics_duplicate_declaration_points_to_duplicate_identifier() {
        let source = "model Ball\n  Real x(start=0);\n  Real x;\n  Real v(start=1);\nequation\n  der(x) = v;\n  der(v) = -9.81;\nend Ball;\n";
        let diagnostics = compute_diagnostics(source, "input.mo", None);
        let duplicate = diagnostics
            .iter()
            .find(|d| {
                d.code == Some(NumberOrString::String("EP001".to_string()))
                    && d.message.contains("Duplicate declaration")
            })
            .unwrap_or_else(|| {
                panic!("expected duplicate-declaration diagnostic: {diagnostics:?}")
            });
        assert_eq!(duplicate.range.start.line, 2);
        assert_eq!(duplicate.range.start.character, 7);
        assert_eq!(duplicate.range.end.line, 2);
        assert_eq!(duplicate.range.end.character, 8);
        assert_eq!(duplicate.data, Some(json!({ "precise_range": true })));
    }

    #[test]
    fn parse_diagnostics_conflicting_component_name_uses_general_line_hint() {
        let source = "model M\n  model A\n  end A;\n  Real A;\nend M;\n";
        let diagnostics = compute_diagnostics(source, "input.mo", None);
        let conflict = diagnostics
            .iter()
            .find(|d| d.message.contains("conflicts with class"))
            .unwrap_or_else(|| panic!("expected name-conflict diagnostic: {diagnostics:?}"));
        assert_eq!(conflict.range.start.line, 3);
        assert_eq!(conflict.range.start.character, 7);
        assert_eq!(conflict.range.end.line, 3);
        assert_eq!(conflict.range.end.character, 8);
        assert_eq!(conflict.data, Some(json!({ "precise_range": true })));
    }

    #[test]
    fn diagnostic_target_names_include_within_prefix() {
        let source = "within Modelica.Blocks;\npackage Continuous\n  block PID\n  end PID;\nend Continuous;\n";
        let ast = parse_source_to_ast_with_errors(source, "input.mo")
            .expect("expected valid AST for within-qualified package");
        let targets = collect_diagnostic_target_names(&ast);
        assert!(
            targets.contains(&"Modelica.Blocks.Continuous".to_string()),
            "expected top-level target with within prefix, got: {targets:?}"
        );
        assert!(
            targets.contains(&"Modelica.Blocks.Continuous.PID".to_string()),
            "expected nested target with within prefix, got: {targets:?}"
        );
    }

    #[test]
    fn diagnostic_target_names_include_nested_functions() {
        let source = r#"
class EOTest
  extends ExternalObject;
  function constructor
    input Boolean verbose = true;
    output EOTest env;
    external "C" env = init(verbose);
  end constructor;
  function destructor
    input EOTest env;
    external "C" destroy(env);
  end destructor;
end EOTest;
"#;
        let ast = parse_source_to_ast_with_errors(source, "input.mo")
            .expect("expected valid AST for external object with nested functions");
        let targets = collect_diagnostic_target_names(&ast);
        assert!(
            targets.contains(&"EOTest".to_string()),
            "expected parent class target, got: {targets:?}"
        );
        assert!(
            targets.contains(&"EOTest.constructor".to_string()),
            "expected nested constructor target, got: {targets:?}"
        );
        assert!(
            targets.contains(&"EOTest.destructor".to_string()),
            "expected nested destructor target, got: {targets:?}"
        );
    }

    #[test]
    fn parse_diagnostics_include_syntax_errors_in_nested_functions() {
        let source = r#"
class EOTest
  extends ExternalObject;
  function constructor
    input Boolean verbose = true
    output EOTest env;
    external "C" env = init(verbose);
  end constructor;
end EOTest;
"#;
        let diagnostics = compute_diagnostics(source, "input.mo", None);
        let parse_diag = diagnostics
            .iter()
            .find(|d| d.code == Some(NumberOrString::String("EP001".to_string())))
            .unwrap_or_else(|| {
                panic!("expected parse diagnostic in nested function: {diagnostics:?}")
            });
        assert!(
            parse_diag.range.start.line >= 4,
            "expected nested-function parse range, got: {:?}",
            parse_diag.range
        );
    }

    #[test]
    fn unresolved_component_in_nested_external_function_call_is_reported() {
        let source = r#"
class EOTest
  extends ExternalObject;
  function constructor
    input Boolean verbdose = true;
    output EOTest env;
    external "C" env = init(verbose);
  end constructor;
  function destructor
    input EOTest env;
    external "C" destroy(env);
  end destructor;
end EOTest;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        assert!(
            diagnostics.iter().any(|d| {
                d.code == Some(NumberOrString::String("ER002".to_string()))
                    && d.message.contains("unresolved component reference")
                    && d.message.contains("verbose")
            }),
            "expected unresolved component reference in nested function external call, got: {diagnostics:?}"
        );
    }

    #[test]
    fn unresolved_component_in_function_output_binding_is_reported() {
        let source = r#"
function F
  input Real x;
  output Real y = missing;
algorithm
  y := x;
end F;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        assert!(
            diagnostics.iter().any(|d| {
                d.code == Some(NumberOrString::String("ER002".to_string()))
                    && d.message.contains("unresolved component reference")
                    && d.message.contains("missing")
            }),
            "expected unresolved component reference in function binding, got: {diagnostics:?}"
        );
    }

    #[test]
    fn unresolved_component_in_extends_modification_is_reported() {
        let source = r#"
model Base
  parameter Real k = 1;
end Base;

model Child
  extends Base(k = missing);
end Child;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        assert!(
            diagnostics.iter().any(|d| {
                d.code == Some(NumberOrString::String("ER002".to_string()))
                    && d.message.contains("unresolved component reference")
                    && d.message.contains("missing")
            }),
            "expected unresolved component reference in extends modification, got: {diagnostics:?}"
        );
    }

    #[test]
    fn unresolved_component_in_assert_level_is_reported() {
        let source = r#"
model M
equation
  assert(true, "ok", lvl);
end M;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        assert!(
            diagnostics.iter().any(|d| {
                d.code == Some(NumberOrString::String("ER002".to_string()))
                    && d.message.contains("unresolved component reference")
                    && d.message.contains("lvl")
            }),
            "expected unresolved component reference in assert-level argument, got: {diagnostics:?}"
        );
    }

    #[test]
    fn unresolved_component_in_statement_function_outputs_is_reported() {
        let source = r#"
model M
  Real x;
algorithm
  (x, y) := sin(1.0);
end M;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        assert!(
            diagnostics.iter().any(|d| {
                d.code == Some(NumberOrString::String("ER002".to_string()))
                    && d.message.contains("unresolved component reference")
                    && d.message.contains("y")
            }),
            "expected unresolved component reference in statement function outputs, got: {diagnostics:?}"
        );
    }

    #[test]
    fn unresolved_class_constrainedby_type_reference_is_reported() {
        let source = r#"
package RealMedium
end RealMedium;

model UsesMedium
  replaceable package Medium = RealMedium constrainedby MissingMedium;
end UsesMedium;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        assert!(
            diagnostics.iter().any(|d| {
                d.code == Some(NumberOrString::String("ER002".to_string()))
                    && d.message.contains("unresolved type reference")
                    && d.message.contains("MissingMedium")
            }),
            "expected unresolved type reference for class constrainedby, got: {diagnostics:?}"
        );
    }

    #[test]
    fn unresolved_component_constrainedby_type_reference_is_reported() {
        let source = r#"
model M
  replaceable Real x constrainedby Missing;
equation
  x = 0;
end M;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        assert!(
            diagnostics.iter().any(|d| {
                d.code == Some(NumberOrString::String("ER002".to_string()))
                    && d.message.contains("unresolved type reference")
                    && d.message.contains("Missing")
            }),
            "expected unresolved type reference for component constrainedby, got: {diagnostics:?}"
        );
    }

    #[test]
    fn unresolved_subscript_index_in_assignment_target_is_reported() {
        let source = r#"
model M
  Real a[2];
algorithm
  a[i] := 1;
end M;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        assert!(
            diagnostics.iter().any(|d| {
                d.code == Some(NumberOrString::String("ER002".to_string()))
                    && d.message.contains("unresolved component reference")
                    && d.message.contains("i")
            }),
            "expected unresolved subscript index diagnostic, got: {diagnostics:?}"
        );
    }

    #[test]
    fn unresolved_selective_import_member_is_reported() {
        let source = r#"
package P
  model A
  end A;
end P;

model M
  import P.{A, B};
end M;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        let unresolved = diagnostics
            .iter()
            .find(|d| {
                d.code == Some(NumberOrString::String("ER002".to_string()))
                    && d.message.contains("unresolved import member")
                    && d.message.contains("B")
            })
            .unwrap_or_else(|| {
                panic!("expected unresolved selective import member, got: {diagnostics:?}")
            });
        assert!(
            unresolved.range.start.line >= 6,
            "expected unresolved selective import diagnostic near import line, got: {:?}",
            unresolved.range
        );
    }

    #[test]
    fn unresolved_equation_function_call_is_reported_as_function_call() {
        let source = r#"
model M
equation
  unknown(1.0);
end M;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        assert!(
            diagnostics.iter().any(|d| {
                d.code == Some(NumberOrString::String("ER002".to_string()))
                    && d.message.contains("unresolved function call")
                    && d.message.contains("unknown")
            }),
            "expected unresolved function-call diagnostic for equation call, got: {diagnostics:?}"
        );
    }

    #[test]
    fn unresolved_statement_function_call_is_reported_as_function_call() {
        let source = r#"
model M
algorithm
  unknown(1.0);
end M;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        assert!(
            diagnostics.iter().any(|d| {
                d.code == Some(NumberOrString::String("ER002".to_string()))
                    && d.message.contains("unresolved function call")
                    && d.message.contains("unknown")
            }),
            "expected unresolved function-call diagnostic for statement call, got: {diagnostics:?}"
        );
    }

    #[test]
    fn unresolved_non_replaceable_partial_type_path_is_reported() {
        let source = r#"
model M
  package P
  end P;
  P.Missing x;
equation
  x = 0;
end M;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        assert!(
            diagnostics.iter().any(|d| {
                d.code == Some(NumberOrString::String("ER002".to_string()))
                    && d.message.contains("unresolved type reference")
                    && d.message.contains("P.Missing")
            }),
            "expected unresolved type reference for non-replaceable partial type path, got: {diagnostics:?}"
        );
    }

    #[test]
    fn nested_external_function_args_are_present_in_ast() {
        let source = r#"
class EOTest
  extends ExternalObject;
  function constructor
    input Boolean verbdose = true;
    output EOTest env;
    external "C" env = init(verbose);
  end constructor;
end EOTest;
"#;
        let ast = parse_source_to_ast_with_errors(source, "input.mo")
            .expect("source should parse for nested external function AST test");
        let eo = ast
            .classes
            .get("EOTest")
            .expect("EOTest class should exist");
        let ctor = eo
            .classes
            .get("constructor")
            .expect("constructor should be a nested class");
        let external = ctor
            .external
            .as_ref()
            .expect("constructor external declaration should be present");
        assert_eq!(
            external.args.len(),
            1,
            "expected exactly one external call argument in AST"
        );
        match &external.args[0] {
            ast::Expression::ComponentReference(comp) => {
                assert_eq!(comp.to_string(), "verbose");
            }
            other => {
                panic!("expected external arg to parse as component reference, got: {other:?}")
            }
        }
    }

    #[test]
    fn common_diagnostics_map_spans_to_editor_ranges() {
        let source = "model M\n  Real x;\n  Real y;\nequation\n  x = y;\nend M;\n";
        let mut source_map = SourceMap::new();
        let source_id = source_map.add("input.mo", source);

        // Span points to the `y` token in `x = y;`.
        let span = Span::from_offsets(source_id, 45, 46);
        let diag =
            CommonDiagnostic::error("ET001", "undefined variable `y`", PrimaryLabel::new(span));

        let converted = common_diagnostic_to_lsp(&diag, source, "input.mo", Some(&source_map))
            .expect("expected span-based diagnostic conversion");
        assert_eq!(
            converted.code,
            Some(NumberOrString::String("ET001".to_string()))
        );
        assert!(
            converted.range.start.line > 0 || converted.range.start.character > 0,
            "expected non-zero range from source span"
        );
        assert!(converted.range.end.line >= converted.range.start.line);
        assert_eq!(converted.data, Some(json!({ "precise_range": true })));
    }

    #[test]
    fn diagnostics_without_labels_use_top_left_range() {
        let source = "model M\n  Real x;\nend M;\n";
        let diag =
            CommonDiagnostic::global_error("EI000", "model-level failure without source labels");
        let converted = common_diagnostic_to_lsp(&diag, source, "input.mo", None);
        assert!(
            converted.is_some(),
            "expected unlabeled diagnostic to use top-left fallback range"
        );
    }

    #[test]
    fn unresolved_component_diagnostic_points_to_equation_site() {
        let source = r#"
model Ball
  Real x(start=0);
  Real v(start=1);
equation
  der(xdf) = v;
  der(v) = -9.81;
end Ball;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        let dump = diagnostics
            .iter()
            .map(|d| {
                let code = d
                    .code
                    .as_ref()
                    .map(|c| match c {
                        NumberOrString::String(s) => s.clone(),
                        NumberOrString::Number(n) => n.to_string(),
                    })
                    .unwrap_or_else(|| "-".to_string());
                format!(
                    "[{}] {}:{} {}",
                    code,
                    d.range.start.line + 1,
                    d.range.start.character + 1,
                    d.message
                )
            })
            .collect::<Vec<_>>()
            .join(" | ");
        let unresolved = diagnostics
            .iter()
            .find(|d| d.message.contains("unresolved component reference"))
            .or_else(|| {
                diagnostics
                    .iter()
                    .find(|d| d.message.contains("undefined variable"))
            })
            .or_else(|| diagnostics.iter().find(|d| d.message.contains("xdf")))
            .or_else(|| {
                diagnostics.iter().find(|d| {
                    d.code.as_ref().and_then(|c| match c {
                        NumberOrString::String(s) => Some(s.as_str()),
                        NumberOrString::Number(_) => None,
                    }) == Some("ER002")
                })
            })
            .unwrap_or_else(|| panic!("expected unresolved-component diagnostic, got: {dump}"));
        assert!(
            unresolved.range.start.line >= 4,
            "expected diagnostic near equation site, got line {} (diag: {:?}, all: {})",
            unresolved.range.start.line + 1,
            unresolved,
            dump
        );
        assert_eq!(unresolved.data, Some(json!({ "precise_range": true })));
    }

    #[test]
    fn unresolved_import_and_type_reference_map_to_distinct_sites() {
        let source = r#"
model Ball
  import Modelica.Blocks.Continuous.PID;
  PID pid();
end Ball;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));

        let unresolved_import = diagnostics
            .iter()
            .find(|d| d.message.contains("unresolved import"))
            .unwrap_or_else(|| {
                panic!("expected unresolved import diagnostic, got: {diagnostics:?}")
            });
        let unresolved_type = diagnostics
            .iter()
            .find(|d| d.message.contains("unresolved type reference"))
            .unwrap_or_else(|| panic!("expected unresolved type diagnostic, got: {diagnostics:?}"));

        assert!(
            unresolved_import.range.start.line >= 2,
            "expected unresolved import near import line, got {:?}",
            unresolved_import.range
        );
        assert!(
            unresolved_type.range.start.line > unresolved_import.range.start.line,
            "expected unresolved type reference to map after import line, got import={:?}, type={:?}",
            unresolved_import.range,
            unresolved_type.range
        );
    }

    #[test]
    fn loaded_source_root_clears_import_and_type_reference_diagnostics() {
        let lib = r#"
package Modelica
  package Blocks
    package Continuous
      model PID
        parameter Real k = 1;
      end PID;
    end Continuous;
  end Blocks;
end Modelica;
"#;
        let source = r#"
model Ball
  import Modelica.Blocks.Continuous.PID;
  PID pid();
end Ball;
"#;
        let mut session = Session::default();
        let parsed =
            parse_source_to_ast_with_errors(lib, "Modelica/package.mo").expect("parse source root");
        let inserted = session.replace_parsed_source_set(
            "external::Modelica",
            SourceRootKind::DurableExternal,
            vec![("Modelica/package.mo".to_string(), parsed)],
            None,
        );
        assert_eq!(inserted, 1, "source root should be inserted");

        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        assert!(
            !diagnostics.iter().any(|d| {
                d.code == Some(NumberOrString::String("ER002".to_string()))
                    && (d.message.contains("unresolved import")
                        || d.message.contains("unresolved type reference"))
            }),
            "loaded source root should clear import/type resolution diagnostics, got: {diagnostics:?}"
        );
    }

    #[test]
    fn unresolved_type_typo_reports_did_you_mean_and_points_to_type_name() {
        let source = r#"
model Ball
  Real x(start=0);
  Readl v(start=1);
equation
  der(x) = v;
  der(v) = -9.81;
end Ball;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        let unresolved = diagnostics
            .iter()
            .find(|d| {
                d.code == Some(NumberOrString::String("ER002".to_string()))
                    && d.message.contains("unresolved type reference")
            })
            .unwrap_or_else(|| panic!("expected unresolved-type diagnostic, got: {diagnostics:?}"));
        assert!(
            unresolved.message.contains("did you mean `Real`")
                || unresolved.message.contains("'Readl'")
                || unresolved.message.contains("`Readl`"),
            "expected unresolved-type message to include unresolved symbol and/or suggestion, got: {}",
            unresolved.message
        );
        assert!(
            unresolved.range.start.line >= 2,
            "expected unresolved type to map near declaration, got {:?}",
            unresolved.range
        );
        assert_eq!(unresolved.data, Some(json!({ "precise_range": true })));
    }

    #[test]
    fn needs_inner_diagnostic_maps_to_outer_declaration_range() {
        let source = r#"
model M
  outer Real shared;
equation
  shared = 1.0;
end M;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        let needs_inner = diagnostics
            .iter()
            .find(|d| d.code == Some(NumberOrString::String("EI008".to_string())))
            .unwrap_or_else(|| panic!("expected EI008 diagnostic, got: {diagnostics:?}"));
        assert!(
            needs_inner.range.start.line > 0 || needs_inner.range.start.character > 0,
            "expected non-origin range for missing-inner diagnostic, got {:?}",
            needs_inner.range
        );
        assert_eq!(needs_inner.data, Some(json!({ "precise_range": true })));
    }

    #[test]
    fn unknown_builtin_modifier_is_reported_via_lsp_compile_diagnostics() {
        let source = r#"
model M
  Real x(startd = 1.0);
equation
  der(x) = -x;
end M;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        let unknown_modifier = diagnostics
            .iter()
            .find(|d| d.message.contains("unknown modifier `startd`"))
            .unwrap_or_else(|| {
                panic!(
                    "expected unknown-modifier diagnostic, got: {:?}",
                    diagnostics
                )
            });
        assert_eq!(
            unknown_modifier.code,
            Some(NumberOrString::String("ET001".to_string()))
        );
        assert_eq!(unknown_modifier.severity, Some(DiagnosticSeverity::ERROR));
    }

    #[test]
    fn unknown_builtin_modifier_with_multiple_classes_is_still_reported() {
        let source = r#"
package Lib
  model Helper
    Real y;
  equation
    y = 1.0;
  end Helper;
end Lib;

model M
  Real x(startd = 1.0);
equation
  der(x) = -x;
end M;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        assert!(
            diagnostics.iter().any(|d| {
                d.code == Some(NumberOrString::String("ET001".to_string()))
                    && d.message.contains("unknown modifier `startd`")
            }),
            "expected unknown-modifier diagnostic, got: {:?}",
            diagnostics
        );
    }

    #[test]
    fn unknown_builtin_modifier_startdt_is_reported_via_lsp_compile_diagnostics() {
        let source = r#"
model M
  Real x(startdt = 1.0);
equation
  der(x) = -x;
end M;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        assert!(
            diagnostics.iter().any(|d| {
                d.code == Some(NumberOrString::String("ET001".to_string()))
                    && d.message.contains("unknown modifier `startdt`")
            }),
            "expected unknown-modifier diagnostic, got: {:?}",
            diagnostics
        );
    }

    #[test]
    fn unknown_builtin_modifier_is_reported_for_function_class() {
        let source = r#"
function F
  input Real x(startd = 1.0);
  output Real y;
algorithm
  y := x;
end F;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        assert!(
            diagnostics.iter().any(|d| {
                d.code == Some(NumberOrString::String("ET001".to_string()))
                    && d.message.contains("unknown modifier `startd`")
            }),
            "expected function-class unknown-modifier diagnostic, got: {:?}",
            diagnostics
        );
    }

    #[test]
    fn duplicate_import_diagnostic_points_to_import_alias() {
        let source = r#"
model M
  import A.B.Interfaces;
  import C.D.Interfaces;
end M;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        let duplicate = diagnostics
            .iter()
            .find(|d| d.code == Some(NumberOrString::String("ER012".to_string())))
            .unwrap_or_else(|| panic!("expected ER012 diagnostic, got: {:?}", diagnostics));
        assert!(
            duplicate.range.start.line >= 2,
            "expected duplicate import highlight near import line, got: {:?}",
            duplicate.range
        );
        assert_eq!(duplicate.data, Some(json!({ "precise_range": true })));
    }

    #[test]
    fn unknown_class_component_modifier_is_reported_via_lsp_compile_diagnostics() {
        let source = r#"
model PID
  parameter Real kp = 1.0;
end PID;

model Ball
  PID pid(kps = 10.0);
end Ball;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        assert!(
            diagnostics.iter().any(|d| {
                d.code == Some(NumberOrString::String("ET001".to_string()))
                    && d.message.contains("unknown modifier `kps`")
            }),
            "expected unknown class modifier diagnostic, got: {:?}",
            diagnostics
        );
    }

    #[test]
    fn unknown_class_component_modifier_is_reported_with_imported_type_alias() {
        let mut session = Session::default();
        session
            .add_document(
                "Lib.mo",
                r#"
package Modelica
  package Blocks
    package Continuous
      model PID
        parameter Real kp = 1.0;
      end PID;
    end Continuous;
  end Blocks;
end Modelica;
"#,
            )
            .expect("source-root preload should parse");

        let source = r#"
model Ball
  import Modelica.Blocks.Continuous.PID;
  PID pid(kps = 10.0);
end Ball;
"#;
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        assert!(
            diagnostics.iter().any(|d| {
                d.code == Some(NumberOrString::String("ET001".to_string()))
                    && d.message.contains("unknown modifier `kps`")
            }),
            "expected unknown class modifier diagnostic with import alias, got: {:?}",
            diagnostics
        );
    }

    #[test]
    fn unknown_nested_builtin_modifier_is_reported_via_lsp_compile_diagnostics() {
        let source = r#"
model Plane
  Real x, y, theta;
equation
  der(x) = cos(theta);
  der(y) = sin(theta);
  der(theta) = 1;
end Plane;

model Sim
  Plane p1(x.star88t = 1.0), p2(y.start = 10.0);
end Sim;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        assert!(
            diagnostics.iter().any(|d| {
                d.code == Some(NumberOrString::String("ET001".to_string()))
                    && d.message.contains("unknown modifier `x.star88t`")
            }),
            "expected unknown nested builtin modifier diagnostic, got: {:?}",
            diagnostics
        );
    }

    #[test]
    fn global_resolution_failures_are_not_duplicated_per_class_target() {
        let source = r#"
model A
  Real x;
equation
  x = 1;
end A;

model B
  Real y;
equation
  y = 2;
end B;
"#;
        let mut session = Session::default();
        session
            .add_document(
                "other.mo",
                r#"
model A
  Real z;
equation
  z = 3;
end A;
"#,
            )
            .expect("preload parses");
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        let dup_class_diags = diagnostics
            .iter()
            .filter(|d| d.message.contains("Duplicate class 'A'"))
            .count();
        assert_eq!(
            dup_class_diags, 1,
            "expected one global merge diagnostic, got: {:?}",
            diagnostics
        );
    }

    #[test]
    fn unresolved_source_root_diagnostics_do_not_panic_or_leak_into_active_file() {
        let source = "model M\n  Real x;\nequation\n  der(x) = -x;\nend M;\n";
        let mut session = Session::default();

        let mut broken_source_root = String::from("model BrokenLib\n  Real x;\nequation\n");
        for _ in 0..256 {
            broken_source_root.push_str("  // filler\n");
        }
        broken_source_root.push_str("  x = unknownLibFn(1.0);\nend BrokenLib;\n");

        session
            .add_document("lib_with_error.mo", &broken_source_root)
            .expect("source-root preload should parse");

        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        assert!(
            diagnostics
                .iter()
                .all(|diag| !diag.message.contains("unknownLibFn")),
            "source-root-only unresolved diagnostics should be filtered out for active file: {:?}",
            diagnostics
        );
    }

    #[test]
    fn unknown_builtin_modifier_is_reported_with_preloaded_source_root_session() {
        let source = r#"
model M
  Real x(startd = 1.0);
equation
  der(x) = -x;
end M;
"#;
        let mut session = Session::default();
        session
            .add_document(
                "Lib.mo",
                r#"
package Lib
  model Helper
    Real y;
  equation
    y = 1.0;
  end Helper;
end Lib;
"#,
            )
            .expect("source-root preload should parse");

        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        assert!(
            diagnostics.iter().any(|d| {
                d.code == Some(NumberOrString::String("ET001".to_string()))
                    && d.message.contains("unknown modifier `startd`")
            }),
            "expected unknown-modifier diagnostic with preloaded source roots, got: {:?}",
            diagnostics
        );
    }

    #[test]
    fn unknown_builtin_modifier_startdt_is_reported_with_preloaded_source_root_session() {
        let source = r#"
model M
  Real x(startdt=1.0);
equation
  der(x) = -x;
end M;
"#;
        let mut session = Session::default();
        session
            .add_document(
                "Lib.mo",
                r#"
package Lib
  model Helper
    Real y;
  equation
    y = 1.0;
  end Helper;
end Lib;
"#,
            )
            .expect("source-root preload should parse");

        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        assert!(
            diagnostics.iter().any(|d| {
                d.code == Some(NumberOrString::String("ET001".to_string()))
                    && d.message.contains("unknown modifier `startdt`")
            }),
            "expected unknown-modifier diagnostic with preloaded source roots, got: {:?}",
            diagnostics
        );
    }

    #[test]
    fn unknown_builtin_modifier_highlights_modifier_identifier() {
        let source = "model M\n  Real x(sltart=0);\nequation\n  der(x) = -x;\nend M;\n";
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        let unknown_modifier = diagnostics
            .iter()
            .find(|d| d.message.contains("unknown modifier `sltart`"))
            .unwrap_or_else(|| {
                panic!(
                    "expected unknown-modifier diagnostic, got: {:?}",
                    diagnostics
                )
            });
        assert_eq!(unknown_modifier.range.start.line, 1);
        assert_eq!(unknown_modifier.range.start.character, 9);
        assert_eq!(unknown_modifier.range.end.line, 1);
        assert_eq!(unknown_modifier.range.end.character, 15);
        assert_eq!(
            unknown_modifier.data,
            Some(json!({ "precise_range": true }))
        );
    }

    #[test]
    fn builtin_modifier_type_mismatch_is_reported_via_lsp_compile_diagnostics() {
        let source = r#"
model M
  Boolean df = true;
  Real v(start = df);
equation
  der(v) = -v;
end M;
"#;
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        assert!(
            diagnostics.iter().any(|d| {
                d.code == Some(NumberOrString::String("ET002".to_string()))
                    && d.message.contains("modifier `start`")
                    && d.message.contains("expects `Real`, found `Boolean`")
            }),
            "expected modifier type mismatch diagnostic, got: {:?}",
            diagnostics
        );
    }

    #[test]
    fn unknown_operator_record_member_is_reported_via_lsp_compile_diagnostics() {
        let source = "operator record SE2\n  Real x;\n  Real y;\n  Real theta;\nend SE2;\n\nmodel Test2\n  SE2 pose;\nequation\n  der(pose.x) = 1;\n  der(pose.y) = 0;\n  der(pose.z) = 2;\nend Test2;\n";
        let mut session = Session::default();
        let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
        let unknown_member = diagnostics
            .iter()
            .find(|diag| {
                diag.code == Some(NumberOrString::String("ET001".to_string()))
                    && diag.message.contains("unknown member `z`")
            })
            .unwrap_or_else(|| {
                panic!(
                    "expected ET001 unknown-member diagnostic, got: {:?}",
                    diagnostics
                )
            });
        assert_eq!(unknown_member.range.start.line, 11);
        assert_eq!(unknown_member.range.start.character, 11);
        assert_eq!(unknown_member.range.end.line, 11);
        assert_eq!(unknown_member.range.end.character, 12);
        assert_eq!(unknown_member.data, Some(json!({ "precise_range": true })));
    }
}
