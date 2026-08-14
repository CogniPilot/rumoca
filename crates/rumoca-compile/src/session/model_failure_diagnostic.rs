//! The per-model failure record a strict compile hands to its callers.

use super::FailedPhase;
use rumoca_core::{Diagnostic as CommonDiagnostic, Label};
use serde::{Deserialize, Serialize};

/// Failure diagnostic for a single model in a strict-reachable-with-recovery pass.
///
/// This record is the whole of what a CLI consumer gets to render, so it carries
/// the originating diagnostic's *entire* label set and note list, not just the
/// span the report anchors on. A phase error such as `EF026` states two facts —
/// the offending `connect` endpoint and the declaration that has no such
/// dimension — and a phase error's `help(...)` text arrives in `notes`
/// (`rumoca_core::miette_phase_error_to_diagnostic` puts it there). Keeping only
/// the anchor here would leave both visible to the LSP and the API, which read
/// the diagnostic directly, and invisible to everyone who runs the compiler.
///
/// The label and note lists are `serde(default)` so failure records serialized
/// by an older build still deserialize.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModelFailureDiagnostic {
    pub model_name: String,
    pub phase: Option<FailedPhase>,
    pub error_code: Option<String>,
    pub error: String,
    /// The label the rendered report anchors on.
    pub primary_label: Option<Label>,
    /// Every other label of the originating diagnostic, in diagnostic order.
    #[serde(default)]
    pub secondary_labels: Vec<Label>,
    /// The originating diagnostic's notes, including its help text.
    #[serde(default)]
    pub notes: Vec<String>,
}

impl ModelFailureDiagnostic {
    /// Split `diagnostic`'s labels into the anchor label and everything else.
    ///
    /// The anchor is the first primary label, falling back to the first label of
    /// any kind. `None` means the diagnostic named no source location at all,
    /// which callers treat as "not renderable against a source".
    pub(crate) fn split_labels(diagnostic: &CommonDiagnostic) -> Option<(Label, Vec<Label>)> {
        let anchor = diagnostic
            .labels
            .iter()
            .position(|label| label.primary)
            .or_else(|| (!diagnostic.labels.is_empty()).then_some(0))?;
        let secondary = diagnostic
            .labels
            .iter()
            .enumerate()
            .filter(|(index, _)| *index != anchor)
            .map(|(_, label)| label.clone())
            .collect();
        Some((diagnostic.labels[anchor].clone(), secondary))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{PrimaryLabel, SourceId, Span};

    fn span(start: usize, end: usize) -> Span {
        Span::from_offsets(
            SourceId::from_source_name("model_failure_diagnostic_tests.mo"),
            start,
            end,
        )
    }

    /// The anchor is the primary label wherever it sits in the list, and every
    /// other label survives in diagnostic order.
    #[test]
    fn split_labels_anchors_on_the_primary_and_keeps_the_rest() {
        let diagnostic = CommonDiagnostic::error(
            "EF026",
            "two locations",
            PrimaryLabel::new(span(10, 20)).with_message("endpoint"),
        )
        .with_label(Label::secondary(span(0, 5)).with_message("declaration"));

        let (primary, secondary) =
            ModelFailureDiagnostic::split_labels(&diagnostic).expect("labeled diagnostic");
        assert_eq!(primary.span, span(10, 20));
        assert_eq!(secondary.len(), 1);
        assert_eq!(secondary[0].message.as_deref(), Some("declaration"));
    }

    /// A diagnostic that names no source cannot be rendered against one.
    #[test]
    fn split_labels_reports_a_label_less_diagnostic() {
        let diagnostic = CommonDiagnostic::global_error("EI000", "no source");
        assert!(ModelFailureDiagnostic::split_labels(&diagnostic).is_none());
    }
}
