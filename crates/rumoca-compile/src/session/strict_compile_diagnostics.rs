use super::{DaePhaseResult, Document, FailedPhase, ModelFailureDiagnostic, PhaseResult};
use indexmap::{IndexMap, IndexSet};
use rumoca_core::{
    Diagnostic as CommonDiagnostic, Diagnostics as CommonDiagnostics, Label, PrimaryLabel,
    SourceId, SourceMap, Span,
};
use rumoca_ir_ast as ast;
use std::sync::Arc;

pub(super) fn phase_result_to_failure(
    tree: &ast::ClassTree,
    model_name: &str,
    result: &PhaseResult,
) -> Option<ModelFailureDiagnostic> {
    match result {
        PhaseResult::Success(_) => None,
        PhaseResult::NeedsInner { missing_inners } => Some(ModelFailureDiagnostic {
            model_name: model_name.to_string(),
            phase: Some(FailedPhase::Instantiate),
            error_code: None,
            error: format!(
                "model needs inner declarations: {}",
                missing_inners.join(", ")
            ),
            primary_label: class_primary_label(tree, model_name, "model needs inner declarations"),
        }),
        PhaseResult::Failed {
            phase,
            error,
            error_code,
        } => Some(ModelFailureDiagnostic {
            model_name: model_name.to_string(),
            phase: Some(*phase),
            error_code: error_code.clone(),
            error: error.clone(),
            primary_label: class_primary_label(tree, model_name, "phase failed"),
        }),
    }
}

pub(super) fn dae_phase_result_to_failure(
    tree: &ast::ClassTree,
    model_name: &str,
    result: &DaePhaseResult,
) -> Option<ModelFailureDiagnostic> {
    match result {
        DaePhaseResult::Success(_) => None,
        DaePhaseResult::NeedsInner { missing_inners } => Some(ModelFailureDiagnostic {
            model_name: model_name.to_string(),
            phase: Some(FailedPhase::Instantiate),
            error_code: None,
            error: format!(
                "model needs inner declarations: {}",
                missing_inners.join(", ")
            ),
            primary_label: class_primary_label(tree, model_name, "model needs inner declarations"),
        }),
        DaePhaseResult::Failed {
            phase,
            error,
            error_code,
        } => Some(ModelFailureDiagnostic {
            model_name: model_name.to_string(),
            phase: Some(*phase),
            error_code: error_code.clone(),
            error: error.clone(),
            primary_label: class_primary_label(tree, model_name, "phase failed"),
        }),
    }
}

pub(super) fn class_primary_span(tree: &ast::ClassTree, model_name: &str) -> Option<Span> {
    let class = tree.get_class_by_qualified_name(model_name)?;
    let name_location = &class.name.location;
    let start = name_location.start as usize;
    let end = (name_location.end as usize).max(start.saturating_add(1));
    let span = if let Some(source_id) = tree.source_map.get_id(&name_location.file_name) {
        Span::from_offsets(source_id, start, end)
    } else {
        default_tree_span(&tree.source_map)
    };
    Some(span)
}

pub(super) fn collect_parse_failures_for_files(
    documents: &IndexMap<String, Arc<Document>>,
    source_map: &SourceMap,
    files: &IndexSet<String>,
) -> Vec<ModelFailureDiagnostic> {
    if files.is_empty() {
        return Vec::new();
    }
    documents
        .values()
        .flat_map(|doc| {
            let is_target_file = files.iter().any(|file| same_path(file, &doc.uri));
            if !is_target_file {
                return Vec::new();
            }
            collect_document_parse_failures(doc, source_map)
        })
        .collect()
}

pub(super) fn collect_resolve_failures_for_files(
    diagnostics: &CommonDiagnostics,
    source_map: &SourceMap,
    files: &IndexSet<String>,
) -> Vec<ModelFailureDiagnostic> {
    if files.is_empty() {
        return Vec::new();
    }
    diagnostics
        .iter()
        .filter(|diag| {
            diag.labels.iter().any(|label| {
                source_map
                    .get_source(label.span.source)
                    .is_some_and(|(file_name, _)| {
                        files.iter().any(|file| same_path(file, file_name))
                    })
            })
        })
        .map(|diag| ModelFailureDiagnostic {
            model_name: "<resolve>".to_string(),
            phase: None,
            error_code: diag.code.clone(),
            error: diag.message.clone(),
            primary_label: diag.labels.iter().find(|label| label.primary).cloned(),
        })
        .collect()
}

pub(super) fn collect_parse_error_diagnostics(
    documents: &IndexMap<String, Arc<Document>>,
    source_map: &SourceMap,
) -> Vec<CommonDiagnostic> {
    let mut out = Vec::new();
    for doc in documents.values() {
        out.extend(document_parse_diagnostics(doc, source_map));
    }
    out
}

pub(super) fn document_parse_diagnostics(
    doc: &Document,
    source_map: &SourceMap,
) -> Vec<CommonDiagnostic> {
    if !doc.parse_errors().is_empty() {
        return doc
            .parse_errors()
            .iter()
            .map(|error| parse_error_to_common_diagnostic(error, doc, source_map))
            .collect();
    }

    let Some(err) = doc.parse_error() else {
        return Vec::new();
    };
    if let Some(span) = doc_default_parse_span(doc, source_map) {
        return vec![
            CommonDiagnostic::error(
                "syntax-error",
                err.to_string(),
                PrimaryLabel::new(span).with_message("parse error in this document"),
            )
            .with_note(format!("document: {}", doc.uri)),
        ];
    }
    vec![
        CommonDiagnostic::global_error(
            "EI000",
            format!(
                "internal error: missing source-map entry for parse diagnostics document '{}'",
                doc.uri
            ),
        )
        .with_note(format!("original parse error: {err}")),
    ]
}

pub(super) fn default_tree_span(source_map: &SourceMap) -> Span {
    let source_id = SourceId(0);
    if let Some((_, content)) = source_map.get_source(source_id) {
        return leading_non_whitespace_span(source_id, content);
    }
    Span::from_offsets(source_id, 0, 1)
}

pub(super) fn collect_target_source_files(
    tree: &ast::ClassTree,
    targets: &[String],
) -> IndexSet<String> {
    let mut files = IndexSet::new();
    for target in targets {
        let Some(class) = tree.get_class_by_qualified_name(target) else {
            continue;
        };
        files.insert(class.location.file_name.clone());
    }
    files
}

fn class_primary_label(tree: &ast::ClassTree, model_name: &str, message: &str) -> Option<Label> {
    let span = class_primary_span(tree, model_name)?;
    Some(Label::primary(span).with_message(message))
}

fn collect_document_parse_failures(
    doc: &Document,
    source_map: &SourceMap,
) -> Vec<ModelFailureDiagnostic> {
    if !doc.parse_errors().is_empty() {
        return doc
            .parse_errors()
            .iter()
            .map(|error| {
                let diagnostic = parse_error_to_common_diagnostic(error, doc, source_map);
                ModelFailureDiagnostic {
                    model_name: doc.uri.clone(),
                    phase: None,
                    error_code: diagnostic.code.clone(),
                    error: diagnostic.message,
                    primary_label: diagnostic.labels.into_iter().find(|label| label.primary),
                }
            })
            .collect();
    }

    let Some(err) = doc.parse_error() else {
        return Vec::new();
    };
    vec![ModelFailureDiagnostic {
        model_name: doc.uri.clone(),
        phase: None,
        error_code: Some("syntax-error".to_string()),
        error: err.to_string(),
        primary_label: doc_default_parse_span(doc, source_map)
            .map(|span| Label::primary(span).with_message("parse error in this document")),
    }]
}

fn parse_error_to_common_diagnostic(
    error: &crate::parse::ParseError,
    doc: &Document,
    source_map: &SourceMap,
) -> CommonDiagnostic {
    let missing_source_error = || {
        CommonDiagnostic::global_error(
            "EI000",
            format!(
                "internal error: missing source-map entry for parse diagnostics document '{}'",
                doc.uri
            ),
        )
        .with_note(format!("document: {}", doc.uri))
    };
    match error {
        crate::parse::ParseError::SyntaxError {
            message,
            unexpected,
            span,
            ..
        } => {
            let Some(remapped_span) = remap_parse_span(doc, source_map, *span) else {
                return missing_source_error();
            };
            let label_message = unexpected
                .as_ref()
                .map(|unexpected| format!("unexpected `{unexpected}`"))
                .unwrap_or_else(|| "error here".to_string());
            CommonDiagnostic::error(
                "EP001",
                message.clone(),
                PrimaryLabel::new(remapped_span).with_message(label_message),
            )
        }
        crate::parse::ParseError::NoAstProduced => {
            let Some(span) = doc_default_parse_span(doc, source_map) else {
                return missing_source_error();
            };
            CommonDiagnostic::error(
                "EP002",
                "parsing succeeded but no AST was produced",
                PrimaryLabel::new(span).with_message("at start of input"),
            )
        }
        crate::parse::ParseError::IoError { path, message } => {
            let Some(span) = doc_default_parse_span(doc, source_map) else {
                return missing_source_error();
            };
            CommonDiagnostic::error(
                "EP003",
                format!("failed to read `{path}`: {message}"),
                PrimaryLabel::new(span).with_message("while reading source input"),
            )
        }
    }
    .with_note(format!("document: {}", doc.uri))
}

fn remap_parse_span(doc: &Document, source_map: &SourceMap, span: Span) -> Option<Span> {
    let source_id = document_source_id(doc, source_map)?;
    let start = span.start.0;
    let end = span.end.0.max(start.saturating_add(1));
    Some(Span::from_offsets(source_id, start, end))
}

fn doc_default_parse_span(doc: &Document, source_map: &SourceMap) -> Option<Span> {
    Some(leading_non_whitespace_span(
        document_source_id(doc, source_map)?,
        &doc.content,
    ))
}

fn document_source_id(doc: &Document, source_map: &SourceMap) -> Option<SourceId> {
    source_map.get_id(&doc.uri)
}

fn leading_non_whitespace_span(source_id: SourceId, content: &str) -> Span {
    if content.is_empty() {
        return Span::from_offsets(source_id, 0, 1);
    }
    if let Some((start, ch)) = content.char_indices().find(|(_, ch)| !ch.is_whitespace()) {
        return Span::from_offsets(source_id, start, start + ch.len_utf8());
    }
    let end = content.chars().next().map_or(1, |ch| ch.len_utf8());
    Span::from_offsets(source_id, 0, end)
}

pub(super) fn same_path(left: &str, right: &str) -> bool {
    let left_key = std::fs::canonicalize(left).unwrap_or_else(|_| std::path::PathBuf::from(left));
    let right_key =
        std::fs::canonicalize(right).unwrap_or_else(|_| std::path::PathBuf::from(right));
    left_key == right_key
}
