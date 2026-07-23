use super::*;

pub(super) fn record_field_projection(
    base: rumoca_core::Expression,
    field: &str,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::FieldAccess {
        base: Box::new(base),
        field: field.to_string(),
        span,
    }
}

pub(super) fn merge_while_iteration_scope(
    builder: &mut LowerBuilder<'_>,
    cond: Reg,
    span: rumoca_core::Span,
    entry_scope: &Scope,
    body_scope: &Scope,
) -> Result<Scope, LowerError> {
    let mut merged = entry_scope.clone();
    let names = collect_scope_names(&merged, std::slice::from_ref(body_scope), entry_scope, span)?;
    for name in names {
        let old = if let Some(old) = entry_scope.get(&name).copied() {
            Some(old)
        } else if is_control_flag(&name) || body_scope.contains_key(&name) {
            Some(builder.emit_const_at(0.0, span)?)
        } else {
            None
        };
        let new = body_scope
            .get(&name)
            .copied()
            .or(old)
            .map(Ok)
            .unwrap_or_else(|| builder.emit_const_at(0.0, span))?;
        if let Some(old) = old {
            merged.insert(name, builder.emit_select_at(cond, new, old, span)?);
        }
    }
    Ok(merged)
}

pub(super) fn old_assignment_value(
    scope: &Scope,
    target: &str,
    idx: usize,
    span: rumoca_core::Span,
) -> Result<Reg, LowerError> {
    let indexed_key = format_subscript_binding_key(target, &[idx + 1]);
    let indexed_key = generated_scope_key(&indexed_key);
    let target_key = generated_scope_key(target);
    scope
        .get(&indexed_key)
        .or_else(|| (idx == 0).then(|| scope.get(&target_key)).flatten())
        .copied()
        .ok_or_else(|| missing_guarded_assignment_binding(target, span))
}

pub(super) fn old_indexed_assignment_value(
    scope: &Scope,
    target: &str,
    indices: &[usize],
    span: rumoca_core::Span,
) -> Result<Reg, LowerError> {
    let indexed_key = format_subscript_binding_key(target, indices);
    let indexed_scope_key = generated_scope_key(&indexed_key);
    let target_key = generated_scope_key(target);
    scope
        .get(&indexed_scope_key)
        .or_else(|| {
            indices
                .iter()
                .all(|index| *index == 1)
                .then(|| scope.get(&target_key))
                .flatten()
        })
        .copied()
        .ok_or_else(|| missing_guarded_assignment_binding(indexed_key.as_str(), span))
}

pub(super) fn missing_guarded_assignment_binding(
    target: &str,
    span: rumoca_core::Span,
) -> LowerError {
    LowerError::contract_violation(
        format!(
            "guarded assignment to `{target}` requires an existing binding to preserve on return/break"
        ),
        span,
    )
}

pub(super) fn positive_size_dimension(
    value: i64,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    if value <= 0 {
        return Err(unsupported_at("size dimension must be positive", span));
    }
    usize::try_from(value).map_err(|_| {
        LowerError::contract_violation(
            format!("size dimension {value} exceeds host index range"),
            span,
        )
    })
}

pub(super) fn checked_real_fft_frequency_count_arg(
    builder: &LowerBuilder<'_>,
    arg: Option<&rumoca_core::Expression>,
    const_scope: &IndexMap<String, f64>,
    call_span: rumoca_core::Span,
) -> Result<Option<usize>, LowerError> {
    let Some(arg) = arg else {
        return Ok(None);
    };
    let span = builder.statement_expr_or_context_span(arg, call_span)?;
    let value = builder
        .eval_compile_time_expr(arg, const_scope)
        .map_err(|err| err.with_fallback_span(span))?;
    checked_fft_frequency_count(value, span).map(Some)
}

impl<'a> LowerBuilder<'a> {
    pub(super) fn statement_blocks_span(
        &self,
        cond_blocks: &[rumoca_core::StatementBlock],
    ) -> Result<rumoca_core::Span, LowerError> {
        cond_blocks
            .iter()
            .find_map(|block| block.cond.span())
            .or_else(|| self.active_source_context_span())
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: "missing source provenance for statement condition blocks".to_string(),
            })
    }

    pub(super) fn statement_expr_span(
        &self,
        expr: &rumoca_core::Expression,
    ) -> Result<rumoca_core::Span, LowerError> {
        expr.span()
            .or_else(|| self.active_source_context_span())
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: "missing source provenance for statement expression".to_string(),
            })
    }

    pub(super) fn statement_expr_or_context_span(
        &self,
        expr: &rumoca_core::Expression,
        context_span: rumoca_core::Span,
    ) -> Result<rumoca_core::Span, LowerError> {
        expr.span()
            .or_else(|| (!context_span.is_dummy()).then_some(context_span))
            .or_else(|| self.active_source_context_span())
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: "missing source provenance for statement expression".to_string(),
            })
    }

    pub(super) fn statement_source_span(
        &self,
        statement: &rumoca_core::Statement,
    ) -> Result<rumoca_core::Span, LowerError> {
        statement
            .source_span()
            .or_else(|| self.active_source_context_span())
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: "missing source provenance for statement".to_string(),
            })
    }
}

pub(super) fn checked_usize_dims_to_i64(
    dims: &[usize],
    context: &str,
    span: rumoca_core::Span,
) -> Result<Vec<i64>, LowerError> {
    let mut converted = crate::lower_vec_with_capacity(
        dims.len(),
        "checked usize dimension conversion count",
        span,
    )?;
    for dim in dims {
        converted.push(i64::try_from(*dim).map_err(|_| {
            LowerError::contract_violation(
                format!("{context} dimension {dim} exceeds i64 range"),
                span,
            )
        })?);
    }
    Ok(converted)
}

pub(super) fn unsupported_with_optional_span(
    reason: impl Into<String>,
    span: Option<rumoca_core::Span>,
) -> LowerError {
    let reason = reason.into();
    match span {
        Some(span) => unsupported_at(reason, span),
        None => LowerError::Unsupported { reason },
    }
}

pub(super) fn concrete_dims_width(dims: &[i64]) -> Option<usize> {
    if dims.is_empty() {
        return None;
    }
    dims.iter().try_fold(1usize, |acc, dim| {
        let dim = usize::try_from(*dim).ok()?;
        if dim == 0 {
            return None;
        }
        acc.checked_mul(dim)
    })
}

pub(super) fn concrete_usize_dims_width(dims: &[usize]) -> Option<usize> {
    if dims.is_empty() {
        return None;
    }
    dims.iter().try_fold(1usize, |acc, dim| {
        if *dim == 0 {
            return None;
        }
        acc.checked_mul(*dim)
    })
}
