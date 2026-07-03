use super::*;

mod builtins;
mod dynamic_selection;
mod helpers;
mod inference;
mod references;
mod selection_helpers;
mod structural_standard;
#[cfg(test)]
mod tests;
use helpers::*;
pub(super) use selection_helpers::*;

const MAX_STATIC_RANGE_VALUES: usize = 100_000;

pub(in crate::lower) struct ArrayComprehensionLowerCtx<'a> {
    indices: &'a [rumoca_core::ComprehensionIndex],
    filter: Option<&'a rumoca_core::Expression>,
    scope: &'a mut Scope,
    const_scope: &'a mut IndexMap<String, f64>,
    call_depth: usize,
}

#[derive(Clone)]
pub(super) struct ArrayOperand {
    pub(super) values: Vec<Reg>,
    pub(super) dims: Vec<usize>,
    pub(super) shape_span: rumoca_core::Span,
}

pub(in crate::lower) struct ArraySelectionPart {
    selector: Option<Reg>,
    slice_indices: Option<Vec<usize>>,
}

struct StructuralIndexLowerCtx<'a> {
    subscripts: &'a [rumoca_core::Subscript],
    scope: &'a Scope,
    call_depth: usize,
    projected_field: Option<&'a str>,
}

#[derive(Clone, Copy)]
pub(super) struct MatMulShape {
    pub m: usize,
    pub k: usize,
    pub n: usize,
}

enum LocalSubscriptResolution {
    NotLocal,
    Values(Vec<Reg>),
}

fn is_modelica_array_constructor_function(name: &rumoca_core::Reference) -> bool {
    name.as_str() == "array"
}

/// Row-major cartesian product over per-dimension sorted selections,
/// collecting the entries present in the indexed metadata. Enumerating the
/// product directly replaces the previous whole-group filter walk, which
/// was quadratic in array size.
fn collect_selected_indexed_entries(
    normalized: &[Vec<usize>],
    meta: &crate::lower::IndexedMeta,
    entries: &[IndexedBinding],
    span: rumoca_core::Span,
) -> Result<Vec<IndexedBinding>, LowerError> {
    let mut selection_capacity = 1usize;
    for selection in normalized {
        selection_capacity = selection_capacity
            .checked_mul(selection.len())
            .ok_or_else(|| {
                LowerError::contract_violation(
                    "indexed selection result count overflows host index range",
                    span,
                )
            })?;
    }
    selection_capacity = selection_capacity.min(entries.len());
    let mut selected =
        array_vec_with_capacity(selection_capacity, "indexed selection result count", span)?;
    if normalized.iter().any(|selection| selection.is_empty()) {
        return Ok(selected);
    }
    let mut cursor =
        array_vec_with_capacity(normalized.len(), "indexed selection cursor count", span)?;
    cursor.resize(normalized.len(), 0usize);
    // Reused across cells: dense arrays resolve `position` arithmetically, so
    // this buffer is the only per-selection allocation (not one per element).
    let mut indices =
        array_vec_with_capacity(normalized.len(), "indexed selection tuple count", span)?;
    loop {
        indices.clear();
        for (at, selection) in cursor.iter().zip(normalized) {
            indices.push(selection[*at]);
        }
        if let Some(position) = meta.position(&indices) {
            selected.push(entries[position].clone());
        }
        if !advance_row_major(&mut cursor, normalized) {
            break;
        }
    }
    Ok(selected)
}

/// Advance a row-major odometer (last dimension fastest); false on wrap.
fn advance_row_major(cursor: &mut [usize], dims: &[Vec<usize>]) -> bool {
    let mut dim = dims.len();
    while dim > 0 {
        dim -= 1;
        cursor[dim] += 1;
        if cursor[dim] < dims[dim].len() {
            return true;
        }
        cursor[dim] = 0;
    }
    false
}

fn expr_span_from_subscripts(subscripts: &[rumoca_core::Subscript]) -> Option<rumoca_core::Span> {
    subscripts.iter().find_map(subscript_source_provenance)
}

fn expr_span_from_subscripts_or(
    subscripts: &[rumoca_core::Subscript],
    fallback: rumoca_core::Span,
) -> rumoca_core::Span {
    match expr_span_from_subscripts(subscripts) {
        Some(span) => span,
        None => fallback,
    }
}

fn required_expr_span_from_subscripts_or_base(
    subscripts: &[rumoca_core::Subscript],
    base: &rumoca_core::Expression,
    owner_span: Option<rumoca_core::Span>,
    context: &'static str,
) -> Result<rumoca_core::Span, LowerError> {
    expr_span_from_subscripts(subscripts)
        .or_else(|| base.span().filter(|span| !span.is_dummy()))
        .or_else(|| owner_span.filter(|span| !span.is_dummy()))
        .ok_or_else(|| LowerError::UnspannedContractViolation {
            reason: format!("missing source provenance for {context}"),
        })
}

fn structural_index_span(
    subscripts: &[rumoca_core::Subscript],
    context_span: rumoca_core::Span,
) -> rumoca_core::Span {
    expr_span_from_subscripts_or(subscripts, context_span)
}

fn subscript_source_span(
    subscript: &rumoca_core::Subscript,
    context_span: rumoca_core::Span,
) -> rumoca_core::Span {
    let span = subscript.span();
    if !span.is_dummy() {
        return span;
    }
    if let rumoca_core::Subscript::Expr { expr, .. } = subscript
        && let Some(expr_span) = expr.span()
    {
        return expr_span;
    }
    context_span
}

fn required_span_from_expression_or_reference(
    expr: &rumoca_core::Expression,
    reference: &rumoca_core::Reference,
    context_span: Option<rumoca_core::Span>,
    context: &'static str,
) -> Result<rumoca_core::Span, LowerError> {
    expr.span()
        .or_else(|| reference.span())
        .or_else(|| context_span.filter(|span| !span.is_dummy()))
        .ok_or_else(|| LowerError::UnspannedContractViolation {
            reason: format!("missing source provenance for {context}"),
        })
}

fn reserve_array_capacity<T>(
    values: &mut Vec<T>,
    capacity: usize,
    context: &str,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    values.try_reserve_exact(capacity).map_err(|_| {
        LowerError::contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })
}

fn array_vec_with_capacity<T>(
    capacity: usize,
    context: &str,
    span: rumoca_core::Span,
) -> Result<Vec<T>, LowerError> {
    let mut values = Vec::new();
    reserve_array_capacity(&mut values, capacity, context, span)?;
    Ok(values)
}

fn single_reg_vec(
    value: Reg,
    context: &str,
    span: rumoca_core::Span,
) -> Result<Vec<Reg>, LowerError> {
    let mut values = array_vec_with_capacity(1, context, span)?;
    values.push(value);
    Ok(values)
}

fn append_reg_values(
    values: &mut Vec<Reg>,
    additional: Vec<Reg>,
    context: &str,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    reserve_array_capacity(values, additional.len(), context, span)?;
    for value in additional {
        values.push(value);
    }
    Ok(())
}

fn one_based_indices(
    dim: usize,
    context: &str,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let mut indices = array_vec_with_capacity(dim, context, span)?;
    for index in 1..=dim {
        indices.push(index);
    }
    Ok(indices)
}

fn single_usize_vec(
    value: usize,
    context: &str,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let mut values = array_vec_with_capacity(1, context, span)?;
    values.push(value);
    Ok(values)
}

/// True when every subscript selects a single element (no `:` and no range),
/// i.e. the access reduces to one scalar array element. Such selections — even
/// with a runtime (non-constant) index like `wp[current_wp, 1]` — must use the
/// scalar VarRef path (which lowers runtime indices to indexed loads), not the
/// compile-time slice enumeration which would try to fold the index.
fn subscripts_select_single_element(subscripts: &[rumoca_core::Subscript]) -> bool {
    !subscripts.is_empty()
        && subscripts.iter().all(|subscript| match subscript {
            rumoca_core::Subscript::Colon { .. } => false,
            rumoca_core::Subscript::Expr { expr, .. } => {
                !matches!(&**expr, rumoca_core::Expression::Range { .. })
            }
            _ => true,
        })
}

fn collect_indexed_record_field_keys(
    base_key: &str,
    field: &str,
    selections: &[Vec<usize>],
    depth: usize,
    current: &mut Vec<usize>,
    keys: &mut Vec<String>,
) {
    if depth >= selections.len() {
        let indexed_key = format_subscript_binding_key(base_key, current);
        keys.push(format!("{indexed_key}.{field}"));
        return;
    }

    for &index in &selections[depth] {
        current.push(index);
        collect_indexed_record_field_keys(base_key, field, selections, depth + 1, current, keys);
        current.pop();
    }
}

fn record_field_array_key(key: &str) -> Option<(&str, &str)> {
    let (base_key, field) = crate::path_utils::scope_split(key)?;
    (!base_key.is_empty() && !field.is_empty()).then_some((base_key, field))
}

fn lower_matrix_column_constructor_values(
    operands: &[ArrayOperand],
) -> Result<Vec<Reg>, LowerError> {
    let Some(first) = operands.first() else {
        return Ok(Vec::new());
    };
    if operands.iter().all(|operand| operand.is_scalar()) {
        return Ok(operands
            .iter()
            .map(|operand| operand.values[0])
            .collect::<Vec<_>>());
    }

    match first.dims.as_slice() {
        [rows]
            if operands
                .iter()
                .all(|operand| matches!(operand.dims.as_slice(), [candidate] if *candidate == *rows)) =>
        {
            let capacity = checked_dim_product(
                *rows,
                operands.len(),
                "matrix column constructor value count",
                first.shape_span,
            )?;
            let mut values = fallible_vec_with_capacity(
                capacity,
                "matrix column constructor value count",
                first.shape_span,
            )?;
            for row in 0..*rows {
                for operand in operands {
                    values.push(operand.values[row]);
                }
            }
            Ok(values)
        }
        [rows, _]
            if operands
                .iter()
                .all(|operand| matches!(operand.dims.as_slice(), [candidate_rows, _] if *candidate_rows == *rows)) =>
        {
            cat_matrix_columns(operands, first.shape_span)
        }
        _ => Err(unsupported_at(
            "matrix constructor operands have incompatible dimensions",
            first.shape_span,
        )),
    }
}

impl<'a> LowerBuilder<'a> {
    pub(super) fn lower_structural_index_expr(
        &mut self,
        base: &rumoca_core::Expression,
        subscripts: &[rumoca_core::Subscript],
        scope: &Scope,
        call_depth: usize,
        projected_field: Option<&str>,
    ) -> Result<Option<Reg>, LowerError> {
        if subscripts.is_empty() {
            return Ok(None);
        }
        match base {
            rumoca_core::Expression::Index {
                base: nested_base,
                subscripts: nested_subscripts,
                span: index_span,
            } => {
                let span = if let Some(span) = expr_span_from_subscripts(nested_subscripts)
                    .or_else(|| expr_span_from_subscripts(subscripts))
                {
                    span
                } else if !index_span.is_dummy() {
                    *index_span
                } else {
                    return Err(LowerError::UnspannedContractViolation {
                        reason: "nested structural index lowering requires a source span"
                            .to_string(),
                    });
                };
                let capacity = nested_subscripts
                    .len()
                    .checked_add(subscripts.len())
                    .ok_or_else(|| {
                        LowerError::contract_violation(
                            "nested subscript count overflows host index range",
                            span,
                        )
                    })?;
                let mut combined =
                    crate::lower_vec_with_capacity(capacity, "nested subscript count", span)?;
                combined.extend(nested_subscripts.iter().cloned());
                combined.extend(subscripts.iter().cloned());
                self.lower_structural_index_expr(
                    nested_base,
                    &combined,
                    scope,
                    call_depth,
                    projected_field,
                )
            }
            rumoca_core::Expression::Array {
                elements,
                is_matrix,
                ..
            } => self.lower_structural_index_array_elements(
                base,
                elements,
                *is_matrix,
                StructuralIndexLowerCtx {
                    subscripts,
                    scope,
                    call_depth,
                    projected_field,
                },
            ),
            rumoca_core::Expression::Tuple { elements, .. } => self
                .lower_structural_index_elements(
                    elements,
                    subscripts,
                    self.required_structural_index_context_span(
                        base,
                        "tuple structural index lowering",
                    )?,
                    scope,
                    call_depth,
                    projected_field,
                ),
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Fill,
                args,
                ..
            } if !args.is_empty() => self.lower_fill_structural_index_leaf(
                base,
                args,
                subscripts,
                projected_field,
                scope,
                call_depth,
            ),
            rumoca_core::Expression::FieldAccess { .. }
            | rumoca_core::Expression::FunctionCall { .. } => self
                .lower_array_like_structural_index(
                    base,
                    subscripts,
                    projected_field,
                    scope,
                    call_depth,
                ),
            _ => Ok(None),
        }
    }

    fn lower_fill_structural_index_leaf(
        &mut self,
        base: &rumoca_core::Expression,
        args: &[rumoca_core::Expression],
        subscripts: &[rumoca_core::Subscript],
        projected_field: Option<&str>,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        // MLS §10.6.2: fill(s, ...) constructs an array whose every selected
        // element is the scalar expression `s`.
        let owner_span = expr_span_from_subscripts(subscripts).or_else(|| base.span());
        let value = self.lower_structural_index_leaf(
            &args[0],
            projected_field,
            owner_span,
            scope,
            call_depth,
        )?;
        Ok(Some(value))
    }

    fn lower_structural_index_array_elements(
        &mut self,
        base: &rumoca_core::Expression,
        elements: &[rumoca_core::Expression],
        is_matrix: bool,
        ctx: StructuralIndexLowerCtx<'_>,
    ) -> Result<Option<Reg>, LowerError> {
        if let Some(element) = single_high_rank_matrix_concat_element(elements, is_matrix) {
            return self.lower_structural_index_expr(
                element,
                ctx.subscripts,
                ctx.scope,
                ctx.call_depth,
                ctx.projected_field,
            );
        }
        self.lower_structural_index_elements(
            elements,
            ctx.subscripts,
            self.required_structural_index_context_span(base, "array structural index lowering")?,
            ctx.scope,
            ctx.call_depth,
            ctx.projected_field,
        )
    }

    // SPEC_0021: Exception - structural index lowering keeps scalar fallback
    // and slice path validation together for traceable subscript errors.
    #[allow(clippy::excessive_nesting)]
    fn lower_array_like_structural_index(
        &mut self,
        base: &rumoca_core::Expression,
        subscripts: &[rumoca_core::Subscript],
        projected_field: Option<&str>,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let dims = self.infer_expr_dims(base, scope)?;
        let context_span = expr_span_from_subscripts(subscripts)
            .or_else(|| base.span().filter(|span| !span.is_dummy()));
        if dims.is_empty() {
            if subscripts.is_empty() {
                return Ok(None);
            }
            let Some(context_span) = context_span else {
                return Err(LowerError::UnspannedContractViolation {
                    reason: "structural array scalar index lowering requires a source span"
                        .to_string(),
                });
            };
            if let Some([index]) =
                static_subscript_indices_with_owner(subscripts, context_span)?.as_deref()
            {
                let values = self.lower_array_like_values(base, scope, call_depth)?;
                let Some(zero_based) = index.checked_sub(1) else {
                    let reason = "structural array subscript index must be one-based";
                    return Err(LowerError::contract_violation(reason, context_span));
                };
                return Ok(values.get(zero_based).copied());
            }
            return Ok(None);
        }
        let Some(span) = context_span else {
            return Err(LowerError::UnspannedContractViolation {
                reason: "structural array index lowering requires a source span".to_string(),
            });
        };
        let selections = self.slice_selections(subscripts, &dims, span, scope)?;
        if selections.iter().any(|selection| selection.len() != 1) {
            return Ok(None);
        }
        let indices = selections
            .iter()
            .map(|selection| selection[0])
            .collect::<Vec<_>>();
        let Some(flat_index) = flat_index_for_subscripts(&dims, &indices) else {
            return Ok(None);
        };
        let values = self.lower_array_like_values(base, scope, call_depth)?;
        let Some(value) = values.get(flat_index).copied() else {
            return Ok(None);
        };
        if let Some(field) = projected_field {
            return Err(unsupported_at(
                format!(
                    "field `{field}` projection from indexed function-call result is unsupported"
                ),
                span,
            ));
        }
        Ok(Some(value))
    }

    fn required_structural_index_context_span(
        &self,
        base: &rumoca_core::Expression,
        context: &'static str,
    ) -> Result<rumoca_core::Span, LowerError> {
        base.span()
            .or_else(|| self.active_source_context_span())
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: format!("missing source provenance for {context}"),
            })
    }

    fn required_array_value_context_span(
        &self,
        expr: &rumoca_core::Expression,
        context: &'static str,
    ) -> Result<rumoca_core::Span, LowerError> {
        expr.span()
            .or_else(|| self.active_source_context_span())
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: format!("missing source provenance for {context}"),
            })
    }

    fn required_reference_or_context_span(
        &self,
        reference: &rumoca_core::Reference,
        context: &'static str,
    ) -> Result<rumoca_core::Span, LowerError> {
        reference
            .span()
            .or_else(|| self.active_source_context_span())
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: format!(
                    "array reference `{}` requires source span metadata for {context}",
                    reference.as_str()
                ),
            })
    }

    fn lower_structural_index_elements(
        &mut self,
        elements: &[rumoca_core::Expression],
        subscripts: &[rumoca_core::Subscript],
        context_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
        projected_field: Option<&str>,
    ) -> Result<Option<Reg>, LowerError> {
        let span = structural_index_span(subscripts, context_span);
        if elements.is_empty() {
            return self.emit_const_at(0.0, span).map(Some);
        }

        let selector =
            self.lower_structural_index_selector(&subscripts[0], span, scope, call_depth)?;
        let fallback = self.emit_const_at(0.0, span)?;
        let mut merged = fallback;

        for (idx, element) in elements.iter().enumerate().rev() {
            let value = if subscripts.len() == 1 {
                self.lower_structural_index_leaf(
                    element,
                    projected_field,
                    (!span.is_dummy()).then_some(span),
                    scope,
                    call_depth,
                )?
            } else if let Some(value) = self.lower_structural_index_expr(
                element,
                &subscripts[1..],
                scope,
                call_depth,
                projected_field,
            )? {
                value
            } else {
                continue;
            };
            let index_reg = self.emit_const_at((idx + 1) as f64, span)?;
            let matches = self.emit_compare_at(CompareOp::Eq, selector, index_reg, span)?;
            merged = self.emit_select_at(matches, value, merged, span)?;
        }

        Ok(Some(merged))
    }

    pub(in crate::lower) fn lower_structural_index_selector(
        &mut self,
        subscript: &rumoca_core::Subscript,
        context_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let span = subscript_source_span(subscript, context_span);
        match subscript {
            rumoca_core::Subscript::Index { value: v, .. } if *v > 0 => {
                self.emit_const_at(*v as f64, span)
            }
            rumoca_core::Subscript::Expr { expr, .. } => {
                let raw = self.lower_expr(expr, scope, call_depth)?;
                self.emit_round_at(raw, span)
            }
            rumoca_core::Subscript::Colon { .. } => {
                Err(unsupported_at("slice subscript `:` is unsupported", span))
            }
            _ => Err(unsupported_at(
                "non-positive subscript is unsupported",
                span,
            )),
        }
    }

    fn lower_structural_index_leaf(
        &mut self,
        element: &rumoca_core::Expression,
        projected_field: Option<&str>,
        owner_span: Option<rumoca_core::Span>,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if let Some(field) = projected_field {
            let Some(owner_span) = owner_span else {
                return Err(LowerError::UnspannedContractViolation {
                    reason: "structural field projection requires a source span".to_string(),
                });
            };
            return self.lower_field_access(element, field, owner_span, scope, call_depth);
        }
        self.lower_expr(element, scope, call_depth)
    }

    pub(super) fn lower_sum_range(
        &mut self,
        expr: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let rumoca_core::Expression::Range {
            start, step, end, ..
        } = expr
        else {
            return Ok(None);
        };
        let span = self.required_array_value_context_span(expr, "range summation lowering")?;

        let start_reg = self.lower_expr(start, scope, call_depth)?;
        let end_reg = self.lower_expr(end, scope, call_depth)?;
        let step_reg = if let Some(step_expr) = step.as_ref() {
            self.lower_expr(step_expr, scope, call_depth)?
        } else {
            let cond = self.emit_compare_at(CompareOp::Ge, end_reg, start_reg, span)?;
            let pos = self.emit_const_at(1.0, span)?;
            let neg = self.emit_const_at(-1.0, span)?;
            self.emit_select_at(cond, pos, neg, span)?
        };

        let zero = self.emit_const_at(0.0, span)?;
        let step_gt_zero = self.emit_compare_at(CompareOp::Gt, step_reg, zero, span)?;
        let step_lt_zero = self.emit_compare_at(CompareOp::Lt, step_reg, zero, span)?;
        let start_le_end = self.emit_compare_at(CompareOp::Le, start_reg, end_reg, span)?;
        let start_ge_end = self.emit_compare_at(CompareOp::Ge, start_reg, end_reg, span)?;
        let forward_valid = self.emit_binary_at(BinaryOp::And, step_gt_zero, start_le_end, span)?;
        let backward_valid =
            self.emit_binary_at(BinaryOp::And, step_lt_zero, start_ge_end, span)?;
        let valid = self.emit_binary_at(BinaryOp::Or, forward_valid, backward_valid, span)?;

        let distance = self.emit_binary_at(BinaryOp::Sub, end_reg, start_reg, span)?;
        let ratio = self.emit_binary_at(BinaryOp::Div, distance, step_reg, span)?;
        let ratio_floor = self.emit_unary_at(UnaryOp::Floor, ratio, span)?;
        let one = self.emit_const_at(1.0, span)?;
        let n = self.emit_binary_at(BinaryOp::Add, ratio_floor, one, span)?;
        let two = self.emit_const_at(2.0, span)?;
        let two_start = self.emit_binary_at(BinaryOp::Mul, two, start_reg, span)?;
        let n_minus_one = self.emit_binary_at(BinaryOp::Sub, n, one, span)?;
        let stride = self.emit_binary_at(BinaryOp::Mul, n_minus_one, step_reg, span)?;
        let bracket = self.emit_binary_at(BinaryOp::Add, two_start, stride, span)?;
        let n_half = self.emit_binary_at(BinaryOp::Div, n, two, span)?;
        let sum = self.emit_binary_at(BinaryOp::Mul, n_half, bracket, span)?;

        let fallback = self.emit_const_at(0.0, span)?;
        self.emit_select_at(valid, sum, fallback, span).map(Some)
    }

    // SPEC_0021: Exception - array-like dispatch is the central expression
    // dispatcher for this module; split plans live in the module header.
    #[allow(clippy::excessive_nesting)]
    pub(super) fn lower_array_like_values(
        &mut self,
        expr: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let context_span = expr.span().filter(|span| !span.is_dummy());
        self.with_optional_source_context(context_span, |this| {
            this.lower_array_like_values_inner(expr, scope, call_depth)
        })
    }

    fn lower_array_like_values_inner(
        &mut self,
        expr: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let result = match expr {
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => {
                self.lower_var_ref_array_like_values(name, expr, scope, call_depth)
            }
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } => self
                .lower_subscripted_var_ref_array_like_values(name, subscripts, scope, call_depth),
            rumoca_core::Expression::FieldAccess { base, field, .. } => {
                self.lower_field_access_array_like_values(base, field, expr, scope, call_depth)
            }
            rumoca_core::Expression::BuiltinCall { function, args, .. } => {
                self.lower_builtin_array_like_values(*function, args, scope, call_depth, expr)
            }
            rumoca_core::Expression::FunctionCall {
                name,
                args,
                is_constructor,
                ..
            } => self.lower_function_call_array_like_values(
                name,
                args,
                *is_constructor,
                expr,
                scope,
                call_depth,
            ),
            rumoca_core::Expression::Array {
                elements,
                is_matrix,
                ..
            } => self.lower_array_constructor_values(elements, *is_matrix, scope, call_depth),
            rumoca_core::Expression::Tuple { elements, .. } => {
                self.lower_tuple_array_like_values(elements, expr, scope, call_depth)
            }
            rumoca_core::Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                ..
            } => self.lower_array_comprehension_values(
                expr,
                indices,
                filter.as_deref(),
                scope,
                call_depth,
            ),
            rumoca_core::Expression::Range {
                start,
                step,
                end,
                span,
            } => self.lower_range_array_like_values(start, step.as_deref(), end, *span),
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => self.lower_if_array_like_values(branches, else_branch, scope, call_depth),
            rumoca_core::Expression::Index {
                base,
                subscripts,
                span,
            } => self.lower_index_array_like_values(
                base,
                subscripts,
                self.span_or_source_context(*span),
                scope,
                call_depth,
            ),
            rumoca_core::Expression::Binary { op, lhs, rhs, span } => {
                self.lower_binary_array_like_values(op, lhs, rhs, *span, scope, call_depth)
            }
            rumoca_core::Expression::Unary { op, rhs, span } => self.lower_unary_array_like_values(
                op,
                rhs,
                self.span_or_source_context(*span),
                scope,
                call_depth,
            ),
            _ => {
                let span = expr
                    .span()
                    .or_else(|| self.active_source_context_span())
                    .ok_or_else(|| LowerError::UnspannedContractViolation {
                        reason: "scalar array-like value lowering requires a source span"
                            .to_string(),
                    })?;
                let mut values = array_vec_with_capacity(1, "scalar expression value count", span)?;
                values.push(self.lower_expr(expr, scope, call_depth)?);
                Ok(values)
            }
        };
        if let Some(span) = expr.span() {
            result.map_err(|err| err.with_fallback_span(span))
        } else {
            result
        }
    }

    fn lower_tuple_array_like_values(
        &mut self,
        elements: &[rumoca_core::Expression],
        expr: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let span = expr
            .span()
            .or_else(|| self.active_source_context_span())
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: "tuple array-like value lowering requires a source span".to_string(),
            })?;
        let mut values = array_vec_with_capacity(elements.len(), "tuple array value count", span)?;
        for element in elements {
            let element_values =
                self.lower_tuple_element_array_like_values(element, scope, call_depth)?;
            reserve_array_capacity(
                &mut values,
                element_values.len(),
                "tuple array value count",
                element.span().unwrap_or(span),
            )?;
            values.extend(element_values);
        }
        Ok(values)
    }

    fn lower_tuple_element_array_like_values(
        &mut self,
        expr: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
            span,
        } = expr
            && self
                .lookup_function(name)
                .is_some_and(|function| function.outputs.len() > 1)
            && let Some(values) = self.lower_user_function_call_output_values(
                name, args, *span, scope, call_depth, None,
            )?
        {
            return Ok(values);
        }
        self.lower_array_like_values(expr, scope, call_depth)
    }

    fn lower_function_call_array_like_values(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
        fallback: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let call_span = required_span_from_expression_or_reference(
            fallback,
            name,
            self.active_source_context_span(),
            "function-call array value lowering",
        )?;
        if is_synchronous_array_like_intrinsic(name.as_str()) {
            return self.lower_synchronous_array_like_intrinsic(
                name.as_str(),
                args,
                scope,
                call_depth,
                call_span,
            );
        }
        if is_stream_passthrough_intrinsic(name.as_str()) {
            return match args.first() {
                Some(arg) => self.lower_array_like_values(arg, scope, call_depth),
                None => Ok(Vec::new()),
            };
        }
        if is_modelica_array_constructor_function(name) {
            return self.lower_array_constructor_values(args, false, scope, call_depth);
        }
        if let Some(values) =
            self.lower_random_array_values(name, args, call_span, scope, call_depth)?
        {
            return Ok(values);
        }
        if let Some(values) =
            self.lower_structural_standard_array_values(name.as_str(), args, scope, call_depth)?
        {
            return Ok(values);
        }
        if self.is_record_constructor_call(name, is_constructor) {
            return self.lower_record_constructor_values(
                name,
                args,
                Some(call_span),
                scope,
                call_depth,
            );
        }
        if let Some(values) =
            self.lower_user_function_call_array_values(name, args, call_span, scope, call_depth)?
        {
            return Ok(values);
        }
        let mut values = array_vec_with_capacity(1, "function fallback value count", call_span)?;
        values.push(self.lower_expr(fallback, scope, call_depth)?);
        Ok(values)
    }

    pub(super) fn lower_multiplication_expr(
        &mut self,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let lhs = self
            .lower_array_operand(lhs, scope, call_depth)
            .map_err(|err| err.with_fallback_span(span))?;
        let rhs = self
            .lower_array_operand(rhs, scope, call_depth)
            .map_err(|err| err.with_fallback_span(span))?;
        let result = self
            .multiply_array_operands(&lhs, &rhs, span)
            .map_err(|err| err.with_fallback_span(span))?;
        match result.values.as_slice() {
            [value] => Ok(*value),
            values => Err(unsupported_at(
                format!(
                    "non-scalar multiplication result with width {} is unsupported in scalar context (lhs_shape={}, rhs_shape={}, result_shape={})",
                    values.len(),
                    format_usize_dims(&lhs.dims),
                    format_usize_dims(&rhs.dims),
                    format_usize_dims(&result.dims),
                ),
                span,
            )),
        }
    }

    fn lower_binary_array_like_values(
        &mut self,
        op: &rumoca_core::OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        use rumoca_core::OpBinary as Op;

        let result = match op {
            Op::Add | Op::AddElem => self.lower_elementwise_binary_values(
                BinaryOp::Add,
                lhs,
                rhs,
                span,
                scope,
                call_depth,
            ),
            Op::Sub | Op::SubElem => self.lower_elementwise_binary_values(
                BinaryOp::Sub,
                lhs,
                rhs,
                span,
                scope,
                call_depth,
            ),
            Op::MulElem => self.lower_elementwise_binary_values(
                BinaryOp::Mul,
                lhs,
                rhs,
                span,
                scope,
                call_depth,
            ),
            Op::DivElem | Op::ExpElem => {
                let bin = if matches!(op, Op::DivElem) {
                    BinaryOp::Div
                } else {
                    BinaryOp::Pow
                };
                self.lower_elementwise_binary_values(bin, lhs, rhs, span, scope, call_depth)
            }
            Op::Div => self.lower_division_array_values(lhs, rhs, span, scope, call_depth),
            Op::Mul => {
                let lhs = self.lower_array_operand(lhs, scope, call_depth)?;
                let rhs = self.lower_array_operand(rhs, scope, call_depth)?;
                Ok(self.multiply_array_operands(&lhs, &rhs, span)?.values)
            }
            Op::Exp | Op::And | Op::Or | Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq | Op::Neq => {
                let lhs = self.lower_array_operand(lhs, scope, call_depth)?;
                let rhs = self.lower_array_operand(rhs, scope, call_depth)?;
                if lhs.is_scalar() && rhs.is_scalar() {
                    let mut values =
                        array_vec_with_capacity(1, "scalar comparison value count", span)?;
                    let value =
                        self.lower_binary(op.clone(), lhs.values[0], rhs.values[0], span)?;
                    values.push(value);
                    return Ok(values);
                }
                Err(unsupported_at(
                    format!("operator `{op}` is unsupported for array-valued solve rows"),
                    span,
                ))
            }
            Op::Assign | Op::Empty => Err(unsupported_at(
                format!("binary operator `{op}` is unsupported"),
                span,
            )),
        };
        result.map_err(|err| err.with_fallback_span(span))
    }

    fn lower_elementwise_binary_values(
        &mut self,
        op: BinaryOp,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let lhs_operand = self.lower_elementwise_operand(lhs, rhs, scope, call_depth)?;
        let rhs_operand = self.lower_elementwise_operand(rhs, lhs, scope, call_depth)?;
        let pairs = broadcast_pairs(&lhs_operand, &rhs_operand)
            .map_err(|err| err.with_fallback_span(span))?;
        let mut values =
            array_vec_with_capacity(pairs.len(), "elementwise binary value count", span)?;
        for (lhs, rhs) in pairs {
            values.push(self.emit_binary_at(op, lhs, rhs, span)?);
        }
        Ok(values)
    }

    fn lower_elementwise_operand(
        &mut self,
        expr: &rumoca_core::Expression,
        other: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<ArrayOperand, LowerError> {
        if matches!(other, rumoca_core::Expression::Tuple { .. })
            && let rumoca_core::Expression::FunctionCall {
                name,
                args,
                is_constructor: false,
                span,
            } = expr
            && self
                .lookup_function(name)
                .is_some_and(|function| function.outputs.len() > 1)
            && let Some(values) = self.lower_user_function_call_output_values(
                name, args, *span, scope, call_depth, None,
            )?
        {
            return Ok(ArrayOperand {
                dims: vector_dims(values.len()),
                values,
                shape_span: *span,
            });
        }
        self.lower_array_operand(expr, scope, call_depth)
    }

    fn lower_division_array_values(
        &mut self,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let lhs = self.lower_array_operand(lhs, scope, call_depth)?;
        let rhs = self.lower_array_operand(rhs, scope, call_depth)?;
        if !rhs.is_scalar() {
            return Err(unsupported_at(
                // MLS §10.6.5: ordinary division is only array divided by
                // scalar. Element-wise division is represented by `./`.
                format!(
                    "array division requires a scalar denominator (lhs_shape={}, lhs_values={}, rhs_shape={}, rhs_values={})",
                    format_usize_dims(&lhs.dims),
                    lhs.values.len(),
                    format_usize_dims(&rhs.dims),
                    rhs.values.len()
                ),
                span,
            ));
        }
        let mut values =
            crate::lower_vec_with_capacity(lhs.values.len(), "array division value count", span)?;
        for value in lhs.values.iter().copied() {
            values.push(self.emit_binary_at(BinaryOp::Div, value, rhs.values[0], span)?);
        }
        Ok(values)
    }

    pub(super) fn lower_array_operand(
        &mut self,
        expr: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<ArrayOperand, LowerError> {
        let owner_span = expr.span().or_else(|| self.active_source_context_span());
        let values = self.lower_array_like_values_with_optional_source_context(
            expr, owner_span, scope, call_depth,
        )?;
        let dims = if self.expr_uses_flat_call_or_tuple_width(expr) {
            vector_dims(values.len())
        } else {
            self.infer_expr_dims(expr, scope)?
        };
        if !dims.is_empty() {
            let Some(span) = owner_span else {
                return Err(LowerError::UnspannedContractViolation {
                    reason: "array operand shape lowering requires a source span".to_string(),
                });
            };
            let expected = checked_shape_size(&dims, "array expression shape", span)?;
            if expected != values.len() {
                if self.expr_has_scalarized_complex_width(expr)
                    || (expected > 0 && values.len() > expected && values.len() % expected == 0)
                {
                    let width = values.len();
                    return ArrayOperand::with_shape_span(values, vector_dims(width), span);
                }
                let shape = format_usize_dims(&dims);
                return Err(unsupported_at(
                    format!(
                        "array expression shape {shape} requires {expected} scalar values, got {}",
                        values.len()
                    ),
                    span,
                ));
            }
            return ArrayOperand::with_shape_span(values, dims, span);
        }
        let Some(span) = owner_span else {
            return Err(LowerError::UnspannedContractViolation {
                reason: "array operand lowering requires a source span".to_string(),
            });
        };
        ArrayOperand::with_shape_span(values, dims, span)
    }

    fn expr_uses_flat_call_or_tuple_width(&self, expr: &rumoca_core::Expression) -> bool {
        match expr {
            rumoca_core::Expression::Tuple { .. } => true,
            rumoca_core::Expression::FunctionCall { name, .. } => self
                .lookup_function(name)
                .is_some_and(|function| function.outputs.len() > 1),
            _ => false,
        }
    }

    fn expr_has_scalarized_complex_width(&self, expr: &rumoca_core::Expression) -> bool {
        if self.scalarized_complex_fields_available(expr) {
            return true;
        }
        match expr {
            rumoca_core::Expression::Binary { lhs, rhs, .. } => {
                self.expr_has_scalarized_complex_width(lhs)
                    || self.expr_has_scalarized_complex_width(rhs)
            }
            rumoca_core::Expression::Unary { rhs, .. } => {
                self.expr_has_scalarized_complex_width(rhs)
            }
            rumoca_core::Expression::FieldAccess { base, .. }
            | rumoca_core::Expression::Index { base, .. } => {
                self.expr_has_scalarized_complex_width(base)
            }
            _ => false,
        }
    }

    // SPEC_0021: Exception - Modelica multiplication shape cases are kept in
    // one match so scalar/vector/matrix semantics remain auditable together.
    #[allow(clippy::excessive_nesting)]
    fn multiply_array_operands(
        &mut self,
        lhs: &ArrayOperand,
        rhs: &ArrayOperand,
        span: rumoca_core::Span,
    ) -> Result<ArrayOperand, LowerError> {
        match (lhs.dims.as_slice(), rhs.dims.as_slice()) {
            ([], []) => Ok(ArrayOperand::scalar_with_span(
                self.emit_binary_at(BinaryOp::Mul, lhs.values[0], rhs.values[0], span)?,
                span,
            )),
            ([], _) => self.scale_operand(lhs.values[0], rhs, span),
            (_, []) => self.scale_operand(rhs.values[0], lhs, span),
            ([lhs_len], [rhs_len]) if lhs_len == rhs_len => {
                // MLS §10.6.5: vector * vector is the scalar product.
                Ok(ArrayOperand::scalar_with_span(
                    self.emit_sum_of_products(&lhs.values, &rhs.values, span)?,
                    span,
                ))
            }
            ([rows, inner], [rhs_len]) if inner == rhs_len => {
                let mut values =
                    fallible_vec_with_capacity(*rows, "matrix-vector product value count", span)?;
                for row in 0..*rows {
                    let lhs_start = checked_dim_product(
                        row,
                        *inner,
                        "matrix-vector product lhs row offset",
                        span,
                    )?;
                    let lhs_end = lhs_start.checked_add(*inner).ok_or_else(|| {
                        LowerError::contract_violation(
                            "matrix-vector product lhs row slice end overflows host index range",
                            span,
                        )
                    })?;
                    let lhs_row = &lhs.values[lhs_start..lhs_end];
                    values.push(self.emit_sum_of_products(lhs_row, &rhs.values, span)?);
                }
                Ok(ArrayOperand {
                    values,
                    dims: vec![*rows],
                    shape_span: span,
                })
            }
            ([lhs_len], [inner, cols]) if lhs_len == inner => {
                let mut values =
                    fallible_vec_with_capacity(*cols, "vector-matrix product value count", span)?;
                for col in 0..*cols {
                    let mut col_values = fallible_vec_with_capacity(
                        *inner,
                        "vector-matrix product rhs column value count",
                        span,
                    )?;
                    for idx in 0..*inner {
                        let rhs_start = checked_dim_product(
                            idx,
                            *cols,
                            "vector-matrix product rhs row offset",
                            span,
                        )?;
                        let rhs_index = rhs_start.checked_add(col).ok_or_else(|| {
                            LowerError::contract_violation(
                                "vector-matrix product rhs column offset overflows host index range",
                                span,
                            )
                        })?;
                        col_values.push(rhs.values[rhs_index]);
                    }
                    values.push(self.emit_sum_of_products(&lhs.values, &col_values, span)?);
                }
                Ok(ArrayOperand {
                    values,
                    dims: vec![*cols],
                    shape_span: span,
                })
            }
            ([rows, inner], [rhs_inner, cols]) if inner == rhs_inner => {
                let values =
                    self.emit_matrix_product_values(lhs, rhs, *rows, *inner, *cols, span)?;
                Ok(ArrayOperand {
                    values,
                    dims: vec![*rows, *cols],
                    shape_span: span,
                })
            }
            _ => Err(unsupported_at(
                format!(
                    "array multiplication with shapes {} and {} is unsupported",
                    format_usize_dims(&lhs.dims),
                    format_usize_dims(&rhs.dims)
                ),
                span,
            )),
        }
    }

    /// Build a `ComputeNode::MatMul` for a matrix-matrix multiply whose shapes
    /// are statically known.  The two operands are evaluated in independent
    /// sub-builders (via `fork_with_next_reg`) so their register files do not
    /// overlap when concatenated by `scalarize_matmul`.
    pub(super) fn build_matmul_node(
        &self,
        lhs_expr: &rumoca_core::Expression,
        rhs_expr: &rumoca_core::Expression,
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
        shape: MatMulShape,
    ) -> Result<rumoca_ir_solve::ComputeNode, LowerError> {
        let MatMulShape { m, k, n } = shape;
        let mut lhs_builder = self.fork_with_next_reg(0);
        let lhs_values = lhs_builder.lower_array_like_values(lhs_expr, scope, call_depth)?;
        let lhs_expected = checked_dim_product(m, k, "MatMul lhs value count", span)?;
        if lhs_values.len() != lhs_expected {
            return Err(unsupported_at(
                format!(
                    "MatMul lhs: expected {} values for {}×{} matrix, got {}",
                    lhs_expected,
                    m,
                    k,
                    lhs_values.len()
                ),
                span,
            ));
        }
        let lhs_start = lhs_builder.try_pack_registers(&lhs_values, span)?;
        let lhs_next = lhs_builder.next_reg;

        let mut rhs_builder = self.fork_with_next_reg(lhs_next);
        let rhs_values = rhs_builder.lower_array_like_values(rhs_expr, scope, call_depth)?;
        let rhs_expected = checked_dim_product(k, n, "MatMul rhs value count", span)?;
        if rhs_values.len() != rhs_expected {
            return Err(unsupported_at(
                format!(
                    "MatMul rhs: expected {} values for {}×{} matrix, got {}",
                    rhs_expected,
                    k,
                    n,
                    rhs_values.len()
                ),
                span,
            ));
        }
        let rhs_start = rhs_builder.try_pack_registers(&rhs_values, span)?;

        let lhs_sparsity = detect_matrix_sparsity(&lhs_builder.ops, &lhs_values, m, k);
        let rhs_sparsity = detect_matrix_sparsity(&rhs_builder.ops, &rhs_values, k, n);

        Ok(rumoca_ir_solve::ComputeNode::MatMul {
            lhs_ops: lhs_builder.ops,
            lhs_start,
            rhs_ops: rhs_builder.ops,
            rhs_start,
            m,
            k,
            n,
            lhs_sparsity,
            rhs_sparsity,
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span,
        })
    }

    // SPEC_0021: Exception - matrix product emission keeps row/column offset
    // validation local so overflow errors carry the product span.
    #[allow(clippy::excessive_nesting)]
    fn emit_matrix_product_values(
        &mut self,
        lhs: &ArrayOperand,
        rhs: &ArrayOperand,
        rows: usize,
        inner: usize,
        cols: usize,
        span: rumoca_core::Span,
    ) -> Result<Vec<Reg>, LowerError> {
        let capacity = checked_dim_product(rows, cols, "matrix product value count", span)?;
        let mut values = fallible_vec_with_capacity(capacity, "matrix product value count", span)?;
        for row in 0..rows {
            let lhs_start = checked_dim_product(row, inner, "matrix product lhs row offset", span)?;
            let lhs_end = lhs_start.checked_add(inner).ok_or_else(|| {
                LowerError::contract_violation(
                    "matrix product lhs row slice end overflows host index range",
                    span,
                )
            })?;
            let lhs_row = &lhs.values[lhs_start..lhs_end];
            for col in 0..cols {
                let mut col_values = fallible_vec_with_capacity(
                    inner,
                    "matrix product rhs column value count",
                    span,
                )?;
                for idx in 0..inner {
                    let rhs_start =
                        checked_dim_product(idx, cols, "matrix product rhs row offset", span)?;
                    let rhs_index = rhs_start.checked_add(col).ok_or_else(|| {
                        LowerError::contract_violation(
                            "matrix product rhs column offset overflows host index range",
                            span,
                        )
                    })?;
                    col_values.push(rhs.values[rhs_index]);
                }
                values.push(self.emit_sum_of_products(lhs_row, &col_values, span)?);
            }
        }
        Ok(values)
    }

    fn scale_operand(
        &mut self,
        scalar: Reg,
        operand: &ArrayOperand,
        span: rumoca_core::Span,
    ) -> Result<ArrayOperand, LowerError> {
        let mut values = crate::lower_vec_with_capacity(
            operand.values.len(),
            "scalar-array product value count",
            span,
        )?;
        for value in operand.values.iter().copied() {
            values.push(self.emit_binary_at(BinaryOp::Mul, scalar, value, span)?);
        }
        Ok(ArrayOperand {
            values,
            dims: operand.dims.clone(),
            shape_span: operand.shape_span,
        })
    }

    fn emit_sum_of_products(
        &mut self,
        lhs_values: &[Reg],
        rhs_values: &[Reg],
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let Some((&lhs_first, &rhs_first)) = lhs_values.first().zip(rhs_values.first()) else {
            return self.emit_const_at(0.0, span);
        };
        let mut acc = self.emit_binary_at(BinaryOp::Mul, lhs_first, rhs_first, span)?;
        for (&lhs, &rhs) in lhs_values.iter().zip(rhs_values.iter()).skip(1) {
            let term = self.emit_binary_at(BinaryOp::Mul, lhs, rhs, span)?;
            acc = self.emit_binary_at(BinaryOp::Add, acc, term, span)?;
        }
        Ok(acc)
    }

    pub(super) fn lower_min_max_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
        call_span: rumoca_core::Span,
        op: BinaryOp,
        empty_identity: f64,
    ) -> Result<Reg, LowerError> {
        // MLS §10.3.4: min(A) and max(A) are reductions over every element of
        // an array expression. Keep this in solve-IR instead of scalarizing by
        // reading only the first array element.
        if args.len() == 1 {
            let span =
                required_min_max_arg_span(&args[0], Some(call_span), "array min/max reduction")?;
            let mut values = self.lower_array_like_values(&args[0], scope, call_depth)?;
            let Some(mut acc) = values.pop() else {
                return self.emit_const_at(empty_identity, span);
            };
            while let Some(value) = values.pop() {
                acc = self.emit_binary_at(op, value, acc, span)?;
            }
            return Ok(acc);
        }

        let Some(first) = args.first() else {
            let span = required_min_max_context_span(call_span, "array min/max reduction")?;
            return self.emit_const_at(empty_identity, span);
        };
        let span = required_min_max_arg_span(first, Some(call_span), "array min/max reduction")?;
        let mut acc = self.lower_expr(first, scope, call_depth)?;
        for expr in args.iter().skip(1) {
            let value = self.lower_expr(expr, scope, call_depth)?;
            acc = self.emit_binary_at(op, acc, value, expr.span().unwrap_or(span))?;
        }
        Ok(acc)
    }

    fn lower_subscripted_var_ref_array_like_values(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        // A function-scope (local) array binding shadows a global model
        // variable of the same name. The slice path below consults the global
        // layout shape, which would mis-resolve e.g. a `Real[4]` function input
        // `p` against a `Real[3]` model state `p` (yielding a phantom `p[4]`).
        // Resolve against the local binding first, mirroring the local-first
        // ordering of the unsubscripted `lower_var_ref_array_like_values` path.
        match self.local_shadowed_subscript_values(name, subscripts, scope, call_depth)? {
            LocalSubscriptResolution::Values(values) => return Ok(values),
            LocalSubscriptResolution::NotLocal => {}
        }
        let span = match expr_span_from_subscripts(subscripts) {
            Some(span) => span,
            None => self.required_reference_or_context_span(name, "array slice lowering")?,
        };
        // A single-element selection (no `:`/range) yields one scalar element.
        // Route it through the scalar VarRef path, which supports runtime
        // (non-constant) subscripts via indexed loads. The compile-time slice
        // enumeration below only handles constant indices/ranges, so sending a
        // runtime index like `wp[current_wp, 1]` (e.g. as a function argument)
        // through it would fail trying to fold the index.
        if subscripts_select_single_element(subscripts) {
            return Ok(vec![
                self.lower_var_ref(name, subscripts, span, scope, call_depth)?,
            ]);
        }
        if let Some(values) =
            self.lower_indexed_reference_slice_values(name, subscripts, span, scope)?
        {
            return Ok(values);
        }
        let Some(keys) = self.slice_binding_keys(name.as_str(), subscripts, span, scope)? else {
            // MLS §10.5: a scalar subscript selects one array element. The
            // array-like lowering path is used by element-wise intrinsics too,
            // so a non-slice selection must fall back to scalar VarRef lowering
            // instead of being rejected as a dynamic array slice.
            return Ok(vec![
                self.lower_var_ref(name, subscripts, span, scope, call_depth)?,
            ]);
        };
        self.load_binding_keys(&keys, span)
    }

    /// Cached per-key indexed metadata (dims + indices lookup), built once.
    fn indexed_meta_for_key(
        &self,
        key: &ComponentReferenceKey,
    ) -> Option<std::sync::Arc<crate::lower::IndexedMeta>> {
        if let Some(meta) = self.indexed_meta_cache.borrow().get(key) {
            return Some(meta.clone());
        }
        let entries = self.indexed_bindings.get(key)?;
        let meta = std::sync::Arc::new(crate::lower::IndexedMeta::build(entries));
        self.indexed_meta_cache
            .borrow_mut()
            .insert(key.clone(), meta.clone());
        Some(meta)
    }

    fn lower_indexed_reference_slice_values(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
        scope: &Scope,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let grouped = self.indexed_bindings.clone();
        let Some(key) = indexed_key_for_reference(&grouped, name, span)? else {
            return Ok(None);
        };
        let Some(meta) = self.indexed_meta_for_key(&key) else {
            return Ok(None);
        };
        if meta.dims.is_empty() {
            return Ok(None);
        }
        let selections = self.slice_selections(subscripts, &meta.dims, span, scope)?;
        // Enumerate the selected cells directly (row-major over per-dim
        // sorted selections) instead of filtering the whole group: the
        // filter walk was quadratic in array size. Sorting per-dim
        // reproduces the previous index-sorted output order.
        let entries = grouped.get(&key).ok_or_else(|| {
            LowerError::contract_violation(
                format!(
                    "indexed binding metadata for `{key:?}` has no corresponding binding group"
                ),
                span,
            )
        })?;
        let mut normalized =
            array_vec_with_capacity(selections.len(), "indexed selection dimension count", span)?;
        for selection in &selections {
            let mut indices =
                array_vec_with_capacity(selection.len(), "indexed selection index count", span)?;
            for index in selection {
                indices.push(*index);
            }
            indices.sort_unstable();
            indices.dedup();
            normalized.push(indices);
        }
        let selected_entries = collect_selected_indexed_entries(&normalized, &meta, entries, span)?;
        if selected_entries.is_empty() {
            return Err(unsupported_at(
                format!(
                    "array slice for `{}` selected no indexed solve-layout bindings",
                    name.as_str()
                ),
                span,
            ));
        }
        self.lower_indexed_entries_values(name.as_str(), &selected_entries, span)
    }

    /// Resolve `name[subscripts]` against a function-scope (local) array
    /// binding, ignoring any global model variable of the same name. Once the
    /// scoped local binding exists, resolution must either produce local values
    /// or report an error; falling through to global lookup would violate
    /// Modelica lexical shadowing.
    fn local_shadowed_subscript_values(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<LocalSubscriptResolution, LowerError> {
        let key = name.as_str();
        if subscripts.is_empty() {
            return Ok(LocalSubscriptResolution::NotLocal);
        }
        let span = match expr_span_from_subscripts(subscripts) {
            Some(span) => span,
            None => {
                self.required_reference_or_context_span(name, "local subscripted binding lowering")?
            }
        };
        let generated_key_path = generated_scope_key(key);
        let key_path = if scope.contains_key(&generated_key_path)
            || scope.indexed_entries(&generated_key_path).is_some()
        {
            generated_key_path
        } else {
            self.scope_key_from_reference(name, span)?
        };
        let Some(dims) = self.local_binding_dims.get(key).cloned() else {
            if scope.contains_key(&key_path) {
                return Err(unsupported_at(
                    format!("subscripted local binding `{key}` is not an array binding"),
                    span,
                ));
            }
            return Ok(LocalSubscriptResolution::NotLocal);
        };
        let Some(bindings) = scope.indexed_entries(&key_path) else {
            if let Some(indices) = self.compile_time_subscript_indices(subscripts, span)? {
                return Err(LowerError::MissingBinding {
                    name: format_subscript_binding_key(key, &indices),
                });
            }
            return Err(unsupported_at(
                format!("subscripted local array `{key}` has no assigned elements"),
                span,
            ));
        };
        if dims.iter().any(|dim| *dim < 0) {
            let shape = format_i64_dims(&dims);
            return Err(unsupported_at(
                format!("subscripted local array `{key}` has negative dimensions {shape}"),
                span,
            ));
        };
        let mut shape = crate::lower_vec_with_capacity(dims.len(), "local array shape rank", span)?;
        for dim in &dims {
            let Ok(dim) = usize::try_from(*dim) else {
                let shape = format_i64_dims(&dims);
                return Err(unsupported_at(
                    format!("subscripted local array `{key}` has unsupported dimensions {shape}"),
                    span,
                ));
            };
            shape.push(dim);
        }
        if subscripts.len() == shape.len() && subscripts.iter().all(is_scalar_selector_subscript) {
            let Some(indices) = self.compile_time_subscript_indices(subscripts, span)? else {
                let value = self.lower_dynamic_subscripted_binding(
                    DynamicBindingTarget::generated(key),
                    subscripts,
                    scope,
                    call_depth,
                    DynamicSubscriptSemantics::VarRef,
                )?;
                return Ok(LocalSubscriptResolution::Values(vec![value]));
            };
            if let Some(binding) = bindings.iter().find(|binding| binding.indices == indices) {
                return Ok(LocalSubscriptResolution::Values(vec![binding.reg]));
            }
            return Err(LowerError::MissingBinding {
                name: format_subscript_binding_key(key, &indices),
            });
        }
        let selections = self
            .slice_selections(subscripts, &shape, span, scope)
            .map_err(|err| {
                let shape = format_usize_dims(&shape);
                err.with_context(format!(
                    "resolving subscripted local array `{key}` with shape {shape}"
                ))
            })?;
        let mut combos = Vec::new();
        collect_slice_index_combos(&selections, 0, &mut Vec::new(), &mut combos);
        let mut regs =
            crate::lower_vec_with_capacity(combos.len(), "local array slice value count", span)?;
        for combo in &combos {
            let Some(binding) = bindings.iter().find(|binding| binding.indices == *combo) else {
                return Err(LowerError::MissingBinding {
                    name: format_subscript_binding_key(key, combo),
                });
            };
            regs.push(binding.reg);
        }
        Ok(LocalSubscriptResolution::Values(regs))
    }

    fn lower_index_array_like_values(
        &mut self,
        base: &rumoca_core::Expression,
        subscripts: &[rumoca_core::Subscript],
        owner_span: Option<rumoca_core::Span>,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        if matches!(
            base,
            rumoca_core::Expression::FieldAccess { .. }
                | rumoca_core::Expression::FunctionCall { .. }
        ) && expr_span_from_subscripts(subscripts)
            .or_else(|| base.span().filter(|span| !span.is_dummy()))
            .or_else(|| owner_span.filter(|span| !span.is_dummy()))
            .is_none()
        {
            return Err(LowerError::UnspannedContractViolation {
                reason: "structural array scalar index lowering requires a source span".to_string(),
            });
        }
        if let Some(values) =
            self.lower_function_output_projection_values(base, subscripts, scope, call_depth)?
        {
            return Ok(values);
        }
        if let Some(value) =
            self.lower_compile_time_indexed_local_value(base, subscripts, owner_span, scope)?
        {
            return Ok(vec![value]);
        }
        if let Some(values) = self.lower_array_like_dynamic_selection_values(
            base, subscripts, owner_span, scope, call_depth,
        )? {
            return Ok(values);
        }
        if is_static_singleton_scalar_projection(base, subscripts)? {
            return Ok(vec![self.lower_expr(base, scope, call_depth)?]);
        }
        if scalar_literal_projection(base, subscripts, owner_span)? {
            return Ok(vec![self.lower_expr(base, scope, call_depth)?]);
        }
        let base_key = dynamic_binding_base_key(base)?;
        let span = required_expr_span_from_subscripts_or_base(
            subscripts,
            base,
            owner_span,
            "array-like index binding slice",
        )?;
        if let Some(keys) = self.slice_binding_keys(base_key.as_str(), subscripts, span, scope)? {
            return self.load_binding_keys(&keys, span);
        }
        Ok(vec![self.lower_index(
            base, subscripts, owner_span, scope, call_depth,
        )?])
    }
}

fn required_min_max_arg_span(
    expr: &rumoca_core::Expression,
    owner_span: Option<rumoca_core::Span>,
    context: &'static str,
) -> Result<rumoca_core::Span, LowerError> {
    expr.span()
        .filter(|span| !span.is_dummy())
        .or_else(|| owner_span.filter(|span| !span.is_dummy()))
        .ok_or_else(|| LowerError::UnspannedContractViolation {
            reason: format!("missing source provenance for {context}"),
        })
}

fn required_min_max_context_span(
    span: rumoca_core::Span,
    context: &'static str,
) -> Result<rumoca_core::Span, LowerError> {
    (!span.is_dummy())
        .then_some(span)
        .ok_or_else(|| LowerError::UnspannedContractViolation {
            reason: format!("missing source provenance for {context}"),
        })
}
