use super::*;

pub(crate) fn checked_literal_positive_indices(
    subscripts: &[rumoca_core::Subscript],
    context_span: Option<rumoca_core::Span>,
) -> Result<Option<Vec<usize>>, LowerError> {
    if subscripts.is_empty() {
        return Ok(Some(Vec::new()));
    }
    let span = subscript_source_span(subscripts, context_span, "literal positive subscript")?;
    let mut indices = lower_vec_with_capacity(
        subscripts.len(),
        "literal positive subscript index count",
        span,
    )?;
    for subscript in subscripts {
        let rumoca_core::Subscript::Index { value, span } = subscript else {
            return Ok(None);
        };
        if *value <= 0 {
            return Ok(None);
        }
        let index = usize::try_from(*value).map_err(|_| {
            lower_contract_violation(
                format!("literal subscript index {value} exceeds host index range"),
                *span,
            )
        })?;
        indices.push(index);
    }
    Ok(Some(indices))
}

pub(crate) fn subscript_source_span(
    subscripts: &[rumoca_core::Subscript],
    context_span: Option<rumoca_core::Span>,
    context: &'static str,
) -> Result<rumoca_core::Span, LowerError> {
    subscripts
        .iter()
        .find_map(subscript_span_or_expr_span)
        .or_else(|| context_span.filter(|span| !span.is_dummy()))
        .ok_or_else(|| LowerError::UnspannedContractViolation {
            reason: format!("{context} is missing source provenance"),
        })
}

fn subscript_span_or_expr_span(subscript: &rumoca_core::Subscript) -> Option<rumoca_core::Span> {
    let span = subscript.span();
    if !span.is_dummy() {
        return Some(span);
    }
    let rumoca_core::Subscript::Expr { expr, .. } = subscript else {
        return None;
    };
    expr.span()
}
