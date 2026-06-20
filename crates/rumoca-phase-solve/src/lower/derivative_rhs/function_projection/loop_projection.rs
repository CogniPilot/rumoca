use super::*;

#[derive(Debug, Clone, Copy)]
pub(super) struct ForProjectionCtx {
    pub(super) span: rumoca_core::Span,
    pub(super) depth: usize,
    pub(super) index_depth: usize,
}

impl FunctionProjectionAnalysis<'_> {
    pub(super) fn apply_for_statement(
        &self,
        function: &rumoca_core::Function,
        indices: &[rumoca_core::ForIndex],
        equations: &[rumoca_core::Statement],
        scope: &mut FunctionProjectionScope,
        projected: &mut Vec<ProjectedFunctionOutput>,
        ctx: ForProjectionCtx,
    ) -> Result<(), LowerError> {
        if ctx.index_depth >= indices.len() {
            for statement in equations {
                self.apply_statement(
                    function,
                    statement,
                    scope,
                    projected,
                    ctx.depth + 1,
                    ctx.span,
                )?;
            }
            return Ok(());
        }
        let index = &indices[ctx.index_depth];
        let values = self.for_index_values(&index.range, scope, ctx.span)?;
        let saved = scope.full.get(index.ident.as_str()).cloned();
        for value in values {
            scope.full.insert(
                index.ident.clone(),
                rumoca_core::Expression::Literal {
                    value: Literal::Integer(value),
                    span: ctx.span,
                },
            );
            self.apply_for_statement(
                function,
                indices,
                equations,
                scope,
                projected,
                ForProjectionCtx {
                    index_depth: ctx.index_depth + 1,
                    ..ctx
                },
            )?;
        }
        if let Some(saved) = saved {
            scope.full.insert(index.ident.clone(), saved);
        } else {
            scope.full.shift_remove(index.ident.as_str());
        }
        Ok(())
    }

    pub(super) fn substitute_component_reference(
        &self,
        component_ref: &rumoca_core::ComponentReference,
        scope: &FunctionProjectionScope,
    ) -> Result<rumoca_core::ComponentReference, LowerError> {
        let mut component_ref = component_ref.clone();
        for part in &mut component_ref.parts {
            let mut subscripts = loop_projection_vec_with_capacity(
                part.subs.len(),
                "function projection component-reference subscript count",
                part.span,
            )?;
            for subscript in &part.subs {
                subscripts.push(self.substitute_static_subscript(subscript, scope)?);
            }
            part.subs = subscripts;
        }
        Ok(component_ref)
    }

    fn substitute_static_subscript(
        &self,
        subscript: &rumoca_core::Subscript,
        scope: &FunctionProjectionScope,
    ) -> Result<rumoca_core::Subscript, LowerError> {
        match subscript {
            rumoca_core::Subscript::Expr { expr, span } => {
                let value = self.compile_time_int(&self.substitute(expr, scope)?, *span)?;
                Ok(rumoca_core::Subscript::Index { value, span: *span })
            }
            rumoca_core::Subscript::Index { .. } | rumoca_core::Subscript::Colon { .. } => {
                Ok(subscript.clone())
            }
        }
    }

    fn for_index_values(
        &self,
        range: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
        span: rumoca_core::Span,
    ) -> Result<Vec<i64>, LowerError> {
        let range = self.substitute(range, scope)?;
        match range {
            rumoca_core::Expression::Range {
                start, step, end, ..
            } => {
                let start = self.compile_time_int(&start, span)?;
                let end = self.compile_time_int(&end, span)?;
                let step = match step {
                    Some(step) => self.compile_time_int(&step, span)?,
                    None => 1,
                };
                if step == 0 {
                    return Err(unsupported_at(
                        "function projection for-loop step cannot be zero",
                        span,
                    ));
                }
                build_i64_range_values(start, end, step, span)
            }
            rumoca_core::Expression::Array { elements, .. } => elements.iter().try_fold(
                loop_projection_vec_with_capacity(
                    elements.len(),
                    "function projection range element count",
                    span,
                )?,
                |mut values, element| {
                    values.push(self.compile_time_int(element, span)?);
                    Ok(values)
                },
            ),
            _ => {
                let mut values = loop_projection_vec_with_capacity(
                    1,
                    "function projection scalar range value count",
                    span,
                )?;
                values.push(self.compile_time_int(&range, span)?);
                Ok(values)
            }
        }
    }

    fn compile_time_int(
        &self,
        expr: &rumoca_core::Expression,
        span: rumoca_core::Span,
    ) -> Result<i64, LowerError> {
        let value = self.compile_time_scalar(expr).ok_or_else(|| {
            unsupported_at("function projection requires a compile-time integer", span)
        })?;
        checked_compile_time_i64(value, span)
    }
}

fn checked_compile_time_i64(value: f64, span: rumoca_core::Span) -> Result<i64, LowerError> {
    let rounded = value.round();
    if !value.is_finite() || (rounded - value).abs() > 1e-9 {
        return Err(unsupported_at(
            format!("function projection requires an integer, got `{value}`"),
            span,
        ));
    }
    if rounded < i64::MIN as f64 || rounded >= i64::MAX as f64 {
        return Err(unsupported_at(
            format!("function projection integer `{value}` overflows i64"),
            span,
        ));
    }
    // Bounds and integrality are checked above; Rust has no TryFrom<f64>.
    Ok(rounded as i64)
}

fn build_i64_range_values(
    start: i64,
    end: i64,
    step: i64,
    span: rumoca_core::Span,
) -> Result<Vec<i64>, LowerError> {
    let mut values =
        loop_projection_vec_with_capacity(0, "function projection range value count", span)?;
    let mut current = start;
    while range_step_keeps_going(current, end, step) {
        reserve_loop_projection_capacity(&mut values, 1, "function projection range value", span)?;
        values.push(current);
        if values.len() > 100_000 {
            return Err(unsupported_at(
                "function projection for-loop range is too large",
                span,
            ));
        }
        if range_would_overshoot_i64(current, end, step) {
            break;
        }
        current = current.checked_add(step).ok_or_else(|| {
            LowerError::contract_violation(
                format!(
                    "function projection for-loop range step overflows after index {current} with step {step}"
                ),
                span,
            )
        })?;
    }
    Ok(values)
}

fn loop_projection_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<T>, LowerError> {
    let mut values = Vec::new();
    reserve_loop_projection_capacity(&mut values, capacity, context, span)?;
    Ok(values)
}

fn reserve_loop_projection_capacity<T>(
    values: &mut Vec<T>,
    additional: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    values.try_reserve_exact(additional).map_err(|_| {
        LowerError::contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })
}

fn range_step_keeps_going(current: i64, end: i64, step: i64) -> bool {
    if step > 0 {
        current <= end
    } else {
        current >= end
    }
}

fn range_would_overshoot_i64(current: i64, end: i64, step: i64) -> bool {
    if step > 0 && current <= end {
        return end
            .checked_sub(current)
            .is_some_and(|remaining| remaining < step);
    }
    if step < 0 && current >= end {
        let Some(step_abs) = step.checked_neg() else {
            return false;
        };
        return current
            .checked_sub(end)
            .is_some_and(|remaining| remaining < step_abs);
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_i64_range_values_stops_before_step_overflow() -> Result<(), String> {
        let Ok(values) =
            build_i64_range_values(i64::MAX - 1, i64::MAX, 2, rumoca_core::Span::DUMMY)
        else {
            return Err("overshooting final step failed instead of terminating".to_string());
        };

        assert_eq!(values, vec![i64::MAX - 1]);
        Ok(())
    }

    #[test]
    fn build_i64_range_values_rejects_large_ranges_with_span() -> Result<(), String> {
        let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(23), 1, 12);

        let Err(err) = build_i64_range_values(1, 100_002, 1, span) else {
            return Err("oversized function projection range succeeded".to_string());
        };

        assert_eq!(err.source_span(), Some(span));
        assert!(
            err.reason()
                .contains("function projection for-loop range is too large"),
            "{err:?}"
        );
        Ok(())
    }

    #[test]
    fn reserve_loop_projection_capacity_dummy_span_stays_unspanned() {
        let mut values = Vec::<i64>::new();
        let err = reserve_loop_projection_capacity(
            &mut values,
            usize::MAX,
            "function projection range value",
            rumoca_core::Span::DUMMY,
        )
        .expect_err("impossible loop projection capacity must be rejected");

        assert!(
            matches!(err, LowerError::UnspannedContractViolation { .. }),
            "dummy loop projection capacity span should stay unspanned: {err:?}"
        );
        assert!(err.reason().contains("capacity exceeds host memory limits"));
    }

    #[test]
    fn checked_compile_time_i64_rejects_upper_bound_with_span() -> Result<(), String> {
        let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(22), 4, 12);

        let Err(err) = checked_compile_time_i64(i64::MAX as f64, span) else {
            return Err("rounded f64 upper bound saturated to i64::MAX".to_string());
        };

        assert_eq!(err.source_span(), Some(span));
        assert!(err.reason().contains("overflows i64"));
        Ok(())
    }
}
