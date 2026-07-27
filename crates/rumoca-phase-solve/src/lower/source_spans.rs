use super::*;

pub(super) fn subscript_span(subscripts: &[rumoca_core::Subscript]) -> Option<rumoca_core::Span> {
    subscripts
        .iter()
        .map(rumoca_core::Subscript::span)
        .find(|span| !span.is_dummy())
}

pub(super) fn subscript_span_with_owner(
    subscripts: &[rumoca_core::Subscript],
    owner_span: rumoca_core::Span,
) -> rumoca_core::Span {
    subscript_span(subscripts).unwrap_or(owner_span)
}

pub(super) fn index_owner_span(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    owner_span: Option<rumoca_core::Span>,
) -> Option<rumoca_core::Span> {
    subscripts
        .iter()
        .find_map(subscript_source_provenance)
        .or_else(|| base.span().filter(|span| !span.is_dummy()))
        .or_else(|| owner_span.filter(|span| !span.is_dummy()))
}

pub(super) fn subscript_source_provenance(
    subscript: &rumoca_core::Subscript,
) -> Option<rumoca_core::Span> {
    let span = subscript.span();
    if !span.is_dummy() {
        return Some(span);
    }
    let rumoca_core::Subscript::Expr { expr, .. } = subscript else {
        return None;
    };
    expr.span()
}

pub(super) fn required_expression_span(
    expr: &rumoca_core::Expression,
    context: &'static str,
) -> Result<rumoca_core::Span, LowerError> {
    Ok(expr.require_span(context)?.span())
}
