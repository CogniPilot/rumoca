//! Span composition for source-backed GALEC parser nodes.

use rumoca_core::Span;

/// The smallest span covering two nodes from the same parsed source.
pub(crate) fn union(a: Span, b: Span) -> Span {
    Span::new(a.source, a.start.min(b.start), a.end.max(b.end))
}
