//! Code actions handler for Modelica files.

use std::collections::HashMap;

use lsp_types::{
    CodeAction, CodeActionKind, Diagnostic, NumberOrString, Position, Range, TextEdit, Url,
    WorkspaceEdit,
};

const BUILTIN_MODIFIER_CANDIDATES: &[&str] = &[
    "start",
    "fixed",
    "nominal",
    "min",
    "max",
    "unit",
    "quantity",
    "displayUnit",
    "stateSelect",
    "uncertain",
    "distribution",
];
const BUILTIN_TYPE_CANDIDATES: &[&str] = &["Real", "Integer", "Boolean", "String"];

fn diagnostic_code(diag: &Diagnostic) -> Option<String> {
    match diag.code.as_ref()? {
        NumberOrString::String(s) => Some(s.clone()),
        NumberOrString::Number(n) => Some(n.to_string()),
    }
}

fn extract_unknown_modifier_name(message: &str) -> Option<&str> {
    let marker = "unknown modifier `";
    let start = message.find(marker)? + marker.len();
    let remainder = &message[start..];
    let end = remainder.find('`')?;
    Some(&remainder[..end])
}

fn extract_section_keyword_typo(message: &str) -> Option<(&str, &str)> {
    let wrong_marker = "unknown section keyword `";
    let wrong_start = message.find(wrong_marker)? + wrong_marker.len();
    let wrong_rest = &message[wrong_start..];
    let wrong_end = wrong_rest.find('`')?;
    let wrong = &wrong_rest[..wrong_end];

    let suggestion_marker = "did you mean `";
    let suggestion_start = message.find(suggestion_marker)? + suggestion_marker.len();
    let suggestion_rest = &message[suggestion_start..];
    let suggestion_end = suggestion_rest.find('`')?;
    let suggestion = &suggestion_rest[..suggestion_end];
    Some((wrong, suggestion))
}

fn extract_did_you_mean_suggestion(message: &str) -> Option<&str> {
    let marker = "did you mean `";
    let start = message.find(marker)? + marker.len();
    let rest = &message[start..];
    let end = rest.find('`')?;
    let suggestion = rest[..end].trim();
    if suggestion.is_empty() {
        None
    } else {
        Some(suggestion)
    }
}

fn indicates_missing_semicolon(message: &str) -> bool {
    if message.contains("possible missing `;`") {
        return true;
    }
    let lower = message.to_ascii_lowercase();
    lower.starts_with("unexpected `equation` (expected one of:")
        || lower.starts_with("unexpected `algorithm` (expected one of:")
        || lower.starts_with("unexpected `end` (expected one of:")
        || lower.starts_with("unexpected `der` (expected one of:")
}

fn levenshtein_distance(a: &str, b: &str) -> usize {
    if a.is_empty() {
        return b.chars().count();
    }
    if b.is_empty() {
        return a.chars().count();
    }

    let b_len = b.chars().count();
    let mut prev: Vec<usize> = (0..=b_len).collect();
    let mut curr = vec![0usize; b_len + 1];

    for (i, ca) in a.chars().enumerate() {
        curr[0] = i + 1;
        for (j, cb) in b.chars().enumerate() {
            let deletion = prev[j + 1] + 1;
            let insertion = curr[j] + 1;
            let substitution = prev[j] + usize::from(ca != cb);
            curr[j + 1] = deletion.min(insertion).min(substitution);
        }
        std::mem::swap(&mut prev, &mut curr);
    }

    prev[b_len]
}

fn suggest_builtin_modifier_name(unknown: &str) -> Option<&'static str> {
    let needle = unknown.trim();
    if needle.is_empty() {
        return None;
    }
    let needle_lower = needle.to_ascii_lowercase();

    let mut best: Option<(&'static str, usize)> = None;
    for candidate in BUILTIN_MODIFIER_CANDIDATES {
        let distance = levenshtein_distance(&needle_lower, &candidate.to_ascii_lowercase());
        match best {
            Some((_, best_distance)) if distance >= best_distance => {}
            _ => best = Some((candidate, distance)),
        }
    }

    let (candidate, distance) = best?;
    if distance <= 3 { Some(candidate) } else { None }
}

fn extract_unresolved_type_name(message: &str) -> Option<&str> {
    let marker = "unresolved type reference:";
    let start = message.find(marker)? + marker.len();
    let rest = message[start..].trim_start();
    let quote = rest.chars().next()?;
    if quote != '\'' && quote != '`' {
        return None;
    }
    let inner = &rest[quote.len_utf8()..];
    let end = inner.find(quote)?;
    let unknown = inner[..end].trim();
    if unknown.is_empty() {
        None
    } else {
        Some(unknown)
    }
}

fn suggest_builtin_type_name(unknown: &str) -> Option<&'static str> {
    let needle = unknown.trim();
    if needle.is_empty() {
        return None;
    }
    let needle_lower = needle.to_ascii_lowercase();
    let mut best: Option<(&'static str, usize)> = None;

    for candidate in BUILTIN_TYPE_CANDIDATES {
        let distance = levenshtein_distance(&needle_lower, &candidate.to_ascii_lowercase());
        match best {
            Some((_, best_distance)) if distance >= best_distance => {}
            _ => best = Some((candidate, distance)),
        }
    }

    let (candidate, distance) = best?;
    if distance <= 3 { Some(candidate) } else { None }
}

fn quick_fix_unknown_modifier(diag: &Diagnostic, uri: Option<&Url>) -> Option<CodeAction> {
    let uri = uri?;
    let code = diagnostic_code(diag)?;
    if code != "ET001" {
        return None;
    }
    let unknown = extract_unknown_modifier_name(&diag.message)?;
    let replacement = suggest_builtin_modifier_name(unknown)?;
    if replacement.eq_ignore_ascii_case(unknown) {
        return None;
    }

    let text_edit = TextEdit {
        range: diag.range,
        new_text: replacement.to_string(),
    };
    let workspace_edit = WorkspaceEdit {
        changes: Some(HashMap::from([(uri.clone(), vec![text_edit])])),
        document_changes: None,
        change_annotations: None,
    };

    Some(CodeAction {
        title: format!("Replace `{}` with `{}`", unknown, replacement),
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: Some(vec![diag.clone()]),
        is_preferred: Some(true),
        disabled: None,
        edit: Some(workspace_edit),
        command: None,
        data: None,
    })
}

fn quick_fix_unresolved_builtin_type(diag: &Diagnostic, uri: Option<&Url>) -> Option<CodeAction> {
    let uri = uri?;
    let code = diagnostic_code(diag)?;
    if code != "ER002" {
        return None;
    }
    if extract_did_you_mean_suggestion(&diag.message).is_some() {
        return None;
    }

    let unknown = extract_unresolved_type_name(&diag.message)?;
    let replacement = suggest_builtin_type_name(unknown)?;
    if replacement.eq_ignore_ascii_case(unknown) {
        return None;
    }

    let text_edit = TextEdit {
        range: diag.range,
        new_text: replacement.to_string(),
    };
    let workspace_edit = WorkspaceEdit {
        changes: Some(HashMap::from([(uri.clone(), vec![text_edit])])),
        document_changes: None,
        change_annotations: None,
    };

    Some(CodeAction {
        title: format!("Replace with `{}`", replacement),
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: Some(vec![diag.clone()]),
        is_preferred: Some(true),
        disabled: None,
        edit: Some(workspace_edit),
        command: None,
        data: None,
    })
}

fn is_skippable_statement_line(line: &str) -> bool {
    let trimmed = line.trim();
    trimmed.is_empty()
        || trimmed.starts_with("//")
        || trimmed.starts_with("/*")
        || trimmed.starts_with('*')
        || trimmed.starts_with("*/")
}

fn is_section_header_line(line: &str) -> bool {
    let normalized = line.trim().to_ascii_lowercase();
    matches!(
        normalized.as_str(),
        "equation"
            | "algorithm"
            | "initial equation"
            | "initial algorithm"
            | "public"
            | "protected"
    )
}

fn insertion_column_for_missing_semicolon(line: &str) -> Option<u32> {
    let comment_start = line.find("//").unwrap_or(line.len());
    let code_segment = &line[..comment_start];
    let trimmed = code_segment.trim_end();
    if trimmed.is_empty() || trimmed.ends_with(';') {
        return None;
    }
    Some(trimmed.chars().count() as u32)
}

fn quick_fix_missing_semicolon(
    diag: &Diagnostic,
    source: &str,
    uri: Option<&Url>,
) -> Option<CodeAction> {
    let uri = uri?;
    if !indicates_missing_semicolon(&diag.message) {
        return None;
    }

    let lines: Vec<&str> = source.lines().collect();
    if lines.is_empty() {
        return None;
    }

    let mut probe_line = diag.range.start.line as i64 - 1;
    while probe_line >= 0 {
        let line = lines.get(probe_line as usize)?;
        if is_skippable_statement_line(line) || is_section_header_line(line) {
            probe_line -= 1;
            continue;
        }
        let Some(insert_col) = insertion_column_for_missing_semicolon(line) else {
            probe_line -= 1;
            continue;
        };
        let insert_pos = Position {
            line: probe_line as u32,
            character: insert_col,
        };
        let text_edit = TextEdit {
            range: Range {
                start: insert_pos,
                end: insert_pos,
            },
            new_text: ";".to_string(),
        };
        let workspace_edit = WorkspaceEdit {
            changes: Some(HashMap::from([(uri.clone(), vec![text_edit])])),
            document_changes: None,
            change_annotations: None,
        };
        return Some(CodeAction {
            title: "Insert missing `;` at end of previous statement".to_string(),
            kind: Some(CodeActionKind::QUICKFIX),
            diagnostics: Some(vec![diag.clone()]),
            is_preferred: Some(true),
            disabled: None,
            edit: Some(workspace_edit),
            command: None,
            data: None,
        });
    }

    None
}

fn quick_fix_section_keyword_typo(diag: &Diagnostic, uri: Option<&Url>) -> Option<CodeAction> {
    let uri = uri?;
    let code = diagnostic_code(diag)?;
    if code != "EP001" {
        return None;
    }
    let (wrong, replacement) = extract_section_keyword_typo(&diag.message)?;
    if wrong == replacement {
        return None;
    }

    let text_edit = TextEdit {
        range: diag.range,
        new_text: replacement.to_string(),
    };
    let workspace_edit = WorkspaceEdit {
        changes: Some(HashMap::from([(uri.clone(), vec![text_edit])])),
        document_changes: None,
        change_annotations: None,
    };

    Some(CodeAction {
        title: format!("Replace section keyword `{}` with `{}`", wrong, replacement),
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: Some(vec![diag.clone()]),
        is_preferred: Some(true),
        disabled: None,
        edit: Some(workspace_edit),
        command: None,
        data: None,
    })
}

fn quick_fix_from_did_you_mean(diag: &Diagnostic, uri: Option<&Url>) -> Option<CodeAction> {
    let uri = uri?;
    let message_lower = diag.message.to_ascii_lowercase();
    if message_lower.contains("unknown section keyword") {
        // Dedicated action provides a better title for this case.
        return None;
    }
    let replacement = extract_did_you_mean_suggestion(&diag.message)?;
    let text_edit = TextEdit {
        range: diag.range,
        new_text: replacement.to_string(),
    };
    let workspace_edit = WorkspaceEdit {
        changes: Some(HashMap::from([(uri.clone(), vec![text_edit])])),
        document_changes: None,
        change_annotations: None,
    };
    Some(CodeAction {
        title: format!("Replace with `{}`", replacement),
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: Some(vec![diag.clone()]),
        is_preferred: Some(false),
        disabled: None,
        edit: Some(workspace_edit),
        command: None,
        data: None,
    })
}

/// Handle code actions request - quick fixes from diagnostics.
pub fn handle_code_actions(
    diagnostics: &[Diagnostic],
    source: &str,
    _range: &Range,
    uri: Option<&Url>,
) -> Vec<CodeAction> {
    let mut actions = Vec::new();

    for diag in diagnostics {
        if let Some(fix) = quick_fix_missing_semicolon(diag, source, uri) {
            actions.push(fix);
        }
        if let Some(fix) = quick_fix_section_keyword_typo(diag, uri) {
            actions.push(fix);
        }
        if let Some(fix) = quick_fix_from_did_you_mean(diag, uri) {
            actions.push(fix);
        }
        if let Some(fix) = quick_fix_unknown_modifier(diag, uri) {
            actions.push(fix);
        }
        if let Some(fix) = quick_fix_unresolved_builtin_type(diag, uri) {
            actions.push(fix);
        }

        // Provide informational actions for each diagnostic
        if let Some(code_str) = diagnostic_code(diag) {
            actions.push(CodeAction {
                title: format!("Learn about: {}", code_str),
                kind: Some(CodeActionKind::QUICKFIX),
                diagnostics: Some(vec![diag.clone()]),
                is_preferred: Some(false),
                disabled: None,
                edit: None,
                command: None,
                data: None,
            });
        }
    }

    actions
}

#[cfg(test)]
mod tests {
    use lsp_types::{DiagnosticSeverity, Position};

    use super::*;

    fn unknown_modifier_diagnostic(name: &str) -> Diagnostic {
        Diagnostic {
            range: Range {
                start: Position::new(1, 9),
                end: Position::new(1, 9 + name.len() as u32),
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String("ET001".to_string())),
            code_description: None,
            source: Some("rumoca".to_string()),
            message: format!(
                "unknown modifier `{}` for builtin component `x` of type `Real`",
                name
            ),
            related_information: None,
            tags: None,
            data: None,
        }
    }

    #[test]
    fn unknown_modifier_quick_fix_uses_workspace_edit() {
        let uri = Url::parse("file:///input.mo").expect("valid URI");
        let diagnostics = vec![unknown_modifier_diagnostic("startdt")];
        let actions = handle_code_actions(
            &diagnostics,
            "model M\n  Real x(startdt=1.0);\nend M;",
            &diagnostics[0].range,
            Some(&uri),
        );
        let fix = actions
            .iter()
            .find(|action| action.title.contains("Replace `startdt` with `start`"))
            .expect("missing quick-fix action");
        let changes = fix
            .edit
            .as_ref()
            .and_then(|edit| edit.changes.as_ref())
            .expect("quick-fix should include workspace edit");
        let edits = changes.get(&uri).expect("missing uri edits");
        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, "start");
        assert_eq!(edits[0].range, diagnostics[0].range);
    }

    #[test]
    fn section_keyword_typo_quick_fix_uses_workspace_edit() {
        let uri = Url::parse("file:///input.mo").expect("valid URI");
        let diagnostics = vec![Diagnostic {
            range: Range {
                start: Position::new(3, 0),
                end: Position::new(3, 9),
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String("EP001".to_string())),
            code_description: None,
            source: Some("rumoca".to_string()),
            message: "unknown section keyword `equations`; did you mean `equation`?".to_string(),
            related_information: None,
            tags: None,
            data: None,
        }];

        let actions = handle_code_actions(
            &diagnostics,
            "model M\n  Real x;\nequations\n  der(x)=1;\nend M;\n",
            &diagnostics[0].range,
            Some(&uri),
        );
        let fix = actions
            .iter()
            .find(|action| action.title.contains("`equations` with `equation`"))
            .expect("missing section-keyword quick-fix action");
        let changes = fix
            .edit
            .as_ref()
            .and_then(|edit| edit.changes.as_ref())
            .expect("quick-fix should include workspace edit");
        let edits = changes.get(&uri).expect("missing uri edits");
        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, "equation");
        assert_eq!(edits[0].range, diagnostics[0].range);
    }

    #[test]
    fn missing_semicolon_quick_fix_inserts_on_previous_statement_line() {
        let uri = Url::parse("file:///input.mo").expect("valid URI");
        let diagnostics = vec![Diagnostic {
            range: Range {
                start: Position::new(2, 0),
                end: Position::new(2, 8),
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String("EP001".to_string())),
            code_description: None,
            source: Some("rumoca".to_string()),
            message: "unexpected `equation` (possible missing `;` before equation section)"
                .to_string(),
            related_information: None,
            tags: None,
            data: None,
        }];
        let source = "model M\n  Real x(start=0)\nequation\n  der(x) = -x;\nend M;\n";

        let actions = handle_code_actions(&diagnostics, source, &diagnostics[0].range, Some(&uri));
        let fix = actions
            .iter()
            .find(|action| action.title.contains("Insert missing `;`"))
            .expect("missing semicolon quick-fix action");
        let changes = fix
            .edit
            .as_ref()
            .and_then(|edit| edit.changes.as_ref())
            .expect("quick-fix should include workspace edit");
        let edits = changes.get(&uri).expect("missing uri edits");
        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, ";");
        assert_eq!(edits[0].range.start.line, 1);
        assert_eq!(edits[0].range.start.character, 17);
        assert_eq!(edits[0].range.end, edits[0].range.start);
    }

    #[test]
    fn missing_semicolon_quick_fix_accepts_unexpected_equation_message_variant() {
        let uri = Url::parse("file:///input.mo").expect("valid URI");
        let diagnostics = vec![Diagnostic {
            range: Range {
                start: Position::new(2, 0),
                end: Position::new(2, 8),
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String("EP001".to_string())),
            code_description: None,
            source: Some("rumoca".to_string()),
            message: "unexpected `equation` (expected one of: ), ,, ;, =, annotation)".to_string(),
            related_information: None,
            tags: None,
            data: None,
        }];
        let source = "model M\n  Real x(start=0)\nequation\n  der(x) = -x;\nend M;\n";
        let actions = handle_code_actions(&diagnostics, source, &diagnostics[0].range, Some(&uri));
        assert!(
            actions
                .iter()
                .any(|action| action.title.contains("Insert missing `;`")),
            "expected missing-semicolon quick-fix for unexpected-equation variant, got: {actions:?}"
        );
    }

    #[test]
    fn did_you_mean_quick_fix_replaces_span_with_suggestion() {
        let uri = Url::parse("file:///input.mo").expect("valid URI");
        let diagnostics = vec![Diagnostic {
            range: Range {
                start: Position::new(2, 2),
                end: Position::new(2, 7),
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String("ER002".to_string())),
            code_description: None,
            source: Some("rumoca".to_string()),
            message: "unresolved type reference: `Readl` at Ball.v; did you mean `Real`?"
                .to_string(),
            related_information: None,
            tags: None,
            data: None,
        }];

        let actions = handle_code_actions(
            &diagnostics,
            "model Ball\n  Real x;\n  Readl v;\nend Ball;\n",
            &diagnostics[0].range,
            Some(&uri),
        );
        let fix = actions
            .iter()
            .find(|action| action.title.contains("Replace with `Real`"))
            .expect("missing did-you-mean quick-fix action");
        let changes = fix
            .edit
            .as_ref()
            .and_then(|edit| edit.changes.as_ref())
            .expect("quick-fix should include workspace edit");
        let edits = changes.get(&uri).expect("missing uri edits");
        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, "Real");
        assert_eq!(edits[0].range, diagnostics[0].range);
    }

    #[test]
    fn unresolved_builtin_type_quick_fix_without_did_you_mean() {
        let uri = Url::parse("file:///input.mo").expect("valid URI");
        let diagnostics = vec![Diagnostic {
            range: Range {
                start: Position::new(2, 2),
                end: Position::new(2, 7),
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String("ER002".to_string())),
            code_description: None,
            source: Some("rumoca".to_string()),
            message: "unresolved type reference: 'Readl' at Ball.v".to_string(),
            related_information: None,
            tags: None,
            data: None,
        }];

        let actions = handle_code_actions(
            &diagnostics,
            "model Ball\n  Real x;\n  Readl v;\nend Ball;\n",
            &diagnostics[0].range,
            Some(&uri),
        );
        let fix = actions
            .iter()
            .find(|action| action.title.contains("Replace with `Real`"))
            .expect("missing unresolved-type quick-fix action");
        let changes = fix
            .edit
            .as_ref()
            .and_then(|edit| edit.changes.as_ref())
            .expect("quick-fix should include workspace edit");
        let edits = changes.get(&uri).expect("missing uri edits");
        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, "Real");
        assert_eq!(edits[0].range, diagnostics[0].range);
    }
}
