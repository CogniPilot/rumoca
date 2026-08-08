use rumoca_core::Span;
use rumoca_core::{Diagnostic as CommonDiagnostic, Label, PrimaryLabel, SourceMap};

use crate::merge::{MergeSemanticError, MergeSemanticLabel};

use super::strict_compile_diagnostics::default_tree_span;

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

pub(super) fn diagnostic_message_with_primary_location(
    diagnostic: &CommonDiagnostic,
    source_map: &SourceMap,
) -> String {
    let Some(label) = diagnostic.labels.iter().find(|label| label.primary) else {
        return diagnostic.message.clone();
    };
    let Some((name, source)) = source_map.get_source(label.span.source) else {
        return diagnostic.message.clone();
    };
    let offset = label.span.start.0.min(source.len());
    let prefix = &source.as_bytes()[..offset];
    let line = prefix.iter().filter(|byte| **byte == b'\n').count() + 1;
    let column = prefix
        .iter()
        .rposition(|byte| *byte == b'\n')
        .map_or(prefix.len() + 1, |newline| prefix.len() - newline);
    format!("{} [{name}:{line}:{column}]", diagnostic.message)
}

fn merge_label_to_span(source_map: &SourceMap, merge_label: &MergeSemanticLabel) -> Option<Span> {
    source_map.get_source(merge_label.source)?;
    let end = merge_label.end.max(merge_label.start.saturating_add(1));
    Some(Span::from_offsets(
        merge_label.source,
        merge_label.start,
        end,
    ))
}
