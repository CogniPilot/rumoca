use rumoca_core::{Diagnostic as CommonDiagnostic, Label, PrimaryLabel, SourceId, SourceMap, Span};

use crate::merge::{MergeSemanticError, MergeSemanticLabel};

use super::strict_compile_diagnostics::default_tree_span;

pub(super) fn miette_error_to_common(
    err: &dyn miette::Diagnostic,
    fallback_span: Span,
    source_map: &SourceMap,
) -> CommonDiagnostic {
    let code = err
        .code()
        .map(|c| c.to_string())
        .unwrap_or_else(|| "EI000".to_string());

    let mut converted_labels = Vec::new();
    if let Some(labels) = err.labels() {
        for labeled in labels {
            let start = labeled.offset();
            let len = labeled.len().max(1);
            let end = start.saturating_add(len);
            let source_id =
                miette_label_source_id(err, source_map, start, len, fallback_span.source);
            let mut label = Label::secondary(Span::from_offsets(source_id, start, end));
            if let Some(message) = labeled.label() {
                label = label.with_message(message.to_string());
            }
            converted_labels.push(label);
        }
    }

    let (primary_label, secondary_labels) = if let Some(first) = converted_labels.first() {
        let mut primary = PrimaryLabel::new(first.span);
        if let Some(message) = first.message.clone() {
            primary = primary.with_message(message);
        }
        (
            primary,
            converted_labels.into_iter().skip(1).collect::<Vec<_>>(),
        )
    } else {
        (
            PrimaryLabel::new(fallback_span).with_message("model compilation failed here"),
            Vec::new(),
        )
    };

    let mut diag = CommonDiagnostic::error(code, err.to_string(), primary_label);
    for label in secondary_labels {
        diag = diag.with_label(label);
    }

    diag
}

pub(super) fn merge_error_to_common(
    err: &anyhow::Error,
    source_map: &SourceMap,
) -> CommonDiagnostic {
    let fallback_span = default_tree_span(source_map);
    let Some(merge_error) = err.downcast_ref::<MergeSemanticError>() else {
        return CommonDiagnostic::error(
            "EM001",
            err.to_string(),
            PrimaryLabel::new(fallback_span).with_message("merge failed"),
        );
    };
    let primary_label = merge_error
        .labels
        .first()
        .and_then(|label| merge_label_to_span(source_map, label).map(|span| (span, label)))
        .map(|(span, label)| PrimaryLabel::new(span).with_message(label.message))
        .unwrap_or_else(|| PrimaryLabel::new(fallback_span).with_message("merge failed"));

    let mut diag = CommonDiagnostic::error("EM001", err.to_string(), primary_label);

    for merge_label in merge_error.labels.iter().skip(1) {
        let Some(span) = merge_label_to_span(source_map, merge_label) else {
            continue;
        };
        let label = if merge_label.primary {
            Label::primary(span)
        } else {
            Label::secondary(span)
        }
        .with_message(merge_label.message);
        diag = diag.with_label(label);
    }

    diag
}

fn miette_label_source_id(
    err: &dyn miette::Diagnostic,
    source_map: &SourceMap,
    offset: usize,
    len: usize,
    fallback_source_id: SourceId,
) -> SourceId {
    let Some(source_code) = err.source_code() else {
        return fallback_source_id;
    };
    let span = miette::SourceSpan::from((offset, len));
    let Ok(contents) = source_code.read_span(&span, 0, 0) else {
        return fallback_source_id;
    };
    let Some(name) = contents.name() else {
        return fallback_source_id;
    };
    source_map.get_id(name).unwrap_or(fallback_source_id)
}

fn merge_label_to_span(source_map: &SourceMap, merge_label: &MergeSemanticLabel) -> Option<Span> {
    let source_id = source_map.get_id(&merge_label.file_name)?;
    let end = merge_label.end.max(merge_label.start.saturating_add(1));
    Some(Span::from_offsets(source_id, merge_label.start, end))
}
