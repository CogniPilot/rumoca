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
                self.apply_statement(function, statement, scope, projected, ctx.depth + 1)?;
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
            part.subs = part
                .subs
                .iter()
                .map(|subscript| self.substitute_static_subscript(subscript, scope))
                .collect::<Result<Vec<_>, _>>()?;
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
                let value = self.compile_time_int(&self.substitute(expr, scope), *span)?;
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
        let range = self.substitute(range, scope);
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
                    return Err(LowerError::Unsupported {
                        reason: "function projection for-loop step cannot be zero".to_string(),
                    }
                    .with_fallback_span(span));
                }
                Ok(build_i64_range_values(start, end, step))
            }
            rumoca_core::Expression::Array { elements, .. } => elements
                .iter()
                .map(|element| self.compile_time_int(element, span))
                .collect(),
            _ => Ok(vec![self.compile_time_int(&range, span)?]),
        }
    }

    fn compile_time_int(
        &self,
        expr: &rumoca_core::Expression,
        span: rumoca_core::Span,
    ) -> Result<i64, LowerError> {
        let value = self.compile_time_scalar(expr).ok_or_else(|| {
            LowerError::Unsupported {
                reason: "function projection requires a compile-time integer".to_string(),
            }
            .with_fallback_span(span)
        })?;
        let rounded = value.round();
        if !value.is_finite() || (rounded - value).abs() > 1e-9 {
            return Err(LowerError::Unsupported {
                reason: format!("function projection requires an integer, got `{value}`"),
            }
            .with_fallback_span(span));
        }
        if rounded < i64::MIN as f64 || rounded > i64::MAX as f64 {
            return Err(LowerError::Unsupported {
                reason: format!("function projection integer `{value}` overflows i64"),
            }
            .with_fallback_span(span));
        }
        Ok(rounded as i64)
    }
}

fn build_i64_range_values(start: i64, end: i64, step: i64) -> Vec<i64> {
    let mut values = Vec::new();
    let mut current = start;
    if step > 0 {
        while current <= end {
            values.push(current);
            current += step;
        }
    } else {
        while current >= end {
            values.push(current);
            current += step;
        }
    }
    values
}
