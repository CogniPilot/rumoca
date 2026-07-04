//! Shared span helpers for the position-aware validator passes (`locate`,
//! `navigate`), so the `DUMMY`-vs-real and containment rules are defined once
//! (SPEC_0034 D11: spans are provenance; `Span::DUMMY` means "no origin").

use rumoca_core::Span;

/// `Some(span)` iff `span` is a real (non-dummy) span.
pub(super) fn non_dummy(span: Span) -> Option<Span> {
    (!span.is_dummy()).then_some(span)
}

/// Whether a real `span` covers the byte `offset` (`[start, end)`); a `DUMMY`
/// span covers nothing.
pub(super) fn span_contains(span: Span, offset: usize) -> bool {
    !span.is_dummy() && offset >= span.start.0 && offset < span.end.0
}
