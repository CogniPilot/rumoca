use super::*;

mod builtins;
mod dynamic_selection;
mod helpers;
mod inference;
mod structural_standard;
use helpers::*;

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
}

pub(in crate::lower) struct ArraySelectionPart {
    selector: Option<Reg>,
    slice_indices: Option<Vec<usize>>,
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

pub(super) fn matmul_shape_from_dims(
    lhs_dims: &[usize],
    rhs_dims: &[usize],
    scalar_count: usize,
) -> Option<MatMulShape> {
    match (lhs_dims, rhs_dims) {
        ([m, k], [rhs_inner, n]) if k == rhs_inner && m * n == scalar_count => Some(MatMulShape {
            m: *m,
            k: *k,
            n: *n,
        }),
        ([m, k], [rhs_inner]) if k == rhs_inner && *m == scalar_count => {
            Some(MatMulShape { m: *m, k: *k, n: 1 })
        }
        ([lhs_inner], [rhs_inner, n]) if lhs_inner == rhs_inner && *n == scalar_count => {
            Some(MatMulShape {
                m: 1,
                k: *lhs_inner,
                n: *n,
            })
        }
        ([lhs_inner], [rhs_inner]) if lhs_inner == rhs_inner && scalar_count == 1 => {
            Some(MatMulShape {
                m: 1,
                k: *lhs_inner,
                n: 1,
            })
        }
        _ => None,
    }
}

fn projected_record_field_expression(
    value: &rumoca_core::Expression,
    field: &str,
) -> rumoca_core::Expression {
    match value {
        rumoca_core::Expression::If {
            branches,
            else_branch,
            span,
        } => rumoca_core::Expression::If {
            branches: branches
                .iter()
                .map(|(cond, branch)| {
                    (
                        cond.clone(),
                        projected_record_field_expression(branch, field),
                    )
                })
                .collect(),
            else_branch: Box::new(projected_record_field_expression(else_branch, field)),
            span: *span,
        },
        _ => rumoca_core::Expression::FieldAccess {
            base: Box::new(value.clone()),
            field: field.to_string(),
            span: value.span().unwrap_or(rumoca_core::Span::DUMMY),
        },
    }
}

fn slice_indices_match(
    parts: &[ArraySelectionPart],
    indices: &[usize],
    output_tuple: &[usize],
) -> bool {
    let mut output_pos = 0usize;
    for (part, index) in parts.iter().zip(indices.iter().copied()) {
        if part.slice_indices.is_some() {
            let Some(expected) = output_tuple.get(output_pos).copied() else {
                return false;
            };
            if index != expected {
                return false;
            }
            output_pos += 1;
        }
    }
    output_pos == output_tuple.len()
}

fn indexed_record_field_key_indices(key: &str, base_key: &str, field: &str) -> Option<Vec<usize>> {
    let suffix = format!(".{field}");
    let indexed_base_key = key.strip_suffix(suffix.as_str())?;
    let (candidate_base, indices) = parse_indexed_binding_key(indexed_base_key)?;
    (candidate_base == base_key).then_some(indices)
}

/// Cartesian product of per-dimension index selections into one-based
/// subscript tuples (the same ordering `collect_slice_binding_keys` uses).
fn collect_slice_index_combos(
    selections: &[Vec<usize>],
    depth: usize,
    current: &mut Vec<usize>,
    combos: &mut Vec<Vec<usize>>,
) {
    if depth >= selections.len() {
        combos.push(current.clone());
        return;
    }
    for &index in &selections[depth] {
        current.push(index);
        collect_slice_index_combos(selections, depth + 1, current, combos);
        current.pop();
    }
}

fn infer_dims_from_index_sets(index_sets: impl IntoIterator<Item = Vec<usize>>) -> Vec<usize> {
    let mut dims = Vec::new();
    for indices in index_sets {
        if indices.len() > dims.len() {
            dims.resize(indices.len(), 0);
        }
        for (idx, value) in indices.into_iter().enumerate() {
            dims[idx] = dims[idx].max(value);
        }
    }
    dims
}

fn expr_span_from_subscripts(subscripts: &[rumoca_core::Subscript]) -> rumoca_core::Span {
    subscripts
        .iter()
        .map(rumoca_core::Subscript::span)
        .find(|span| !span.is_dummy())
        .unwrap_or(rumoca_core::Span::DUMMY)
}

fn indexed_entry_matches_selections(entry: &IndexedBinding, selections: &[Vec<usize>]) -> bool {
    entry.indices.len() == selections.len()
        && entry
            .indices
            .iter()
            .zip(selections)
            .all(|(index, selection)| selection.contains(index))
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
    let (base_key, field) = rumoca_core::split_last_top_level(key)?;
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
            let mut values = Vec::with_capacity(rows * operands.len());
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
            cat_matrix_columns(operands)
        }
        _ => Err(LowerError::Unsupported {
            reason: "matrix constructor operands have incompatible dimensions".to_string(),
        }),
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
                ..
            } => {
                let mut combined = nested_subscripts.to_vec();
                combined.extend_from_slice(subscripts);
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
            } => {
                if let Some(element) = single_high_rank_matrix_concat_element(elements, *is_matrix)
                {
                    return self.lower_structural_index_expr(
                        element,
                        subscripts,
                        scope,
                        call_depth,
                        projected_field,
                    );
                }
                self.lower_structural_index_elements(
                    elements,
                    subscripts,
                    scope,
                    call_depth,
                    projected_field,
                )
            }
            rumoca_core::Expression::Tuple { elements, .. } => self
                .lower_structural_index_elements(
                    elements,
                    subscripts,
                    scope,
                    call_depth,
                    projected_field,
                ),
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Fill,
                args,
                ..
            } if !args.is_empty() => {
                // MLS §10.6.2: fill(s, ...) constructs an array whose every
                // selected element is the scalar expression `s`.
                let value =
                    self.lower_structural_index_leaf(&args[0], projected_field, scope, call_depth)?;
                Ok(Some(value))
            }
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

    fn lower_array_like_structural_index(
        &mut self,
        base: &rumoca_core::Expression,
        subscripts: &[rumoca_core::Subscript],
        projected_field: Option<&str>,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let dims = self.infer_expr_dims(base, scope)?;
        if dims.is_empty() {
            if let Some([index]) = static_subscript_indices(subscripts)?.as_deref() {
                let values = self.lower_array_like_values(base, scope, call_depth)?;
                return Ok(values.get(index.saturating_sub(1)).copied());
            }
            return Ok(None);
        }
        let selections = self.slice_selections(subscripts, &dims, scope)?;
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
            return Err(LowerError::Unsupported {
                reason: format!(
                    "field `{field}` projection from indexed function-call result is unsupported"
                ),
            });
        }
        Ok(Some(value))
    }

    fn lower_structural_index_elements(
        &mut self,
        elements: &[rumoca_core::Expression],
        subscripts: &[rumoca_core::Subscript],
        scope: &Scope,
        call_depth: usize,
        projected_field: Option<&str>,
    ) -> Result<Option<Reg>, LowerError> {
        if elements.is_empty() {
            return Ok(Some(self.emit_const(0.0)));
        }

        let selector = self.lower_structural_index_selector(&subscripts[0], scope, call_depth)?;
        let fallback = self.emit_const(0.0);
        let mut merged = fallback;

        for (idx, element) in elements.iter().enumerate().rev() {
            let value = if subscripts.len() == 1 {
                self.lower_structural_index_leaf(element, projected_field, scope, call_depth)?
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
            let index_reg = self.emit_const((idx + 1) as f64);
            let matches = self.emit_compare(CompareOp::Eq, selector, index_reg);
            merged = self.emit_select(matches, value, merged);
        }

        Ok(Some(merged))
    }

    pub(in crate::lower) fn lower_structural_index_selector(
        &mut self,
        subscript: &rumoca_core::Subscript,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        match subscript {
            rumoca_core::Subscript::Index { value: v, .. } if *v > 0 => {
                Ok(self.emit_const(*v as f64))
            }
            rumoca_core::Subscript::Expr { expr, .. } => {
                let raw = self.lower_expr(expr, scope, call_depth)?;
                Ok(self.emit_round(raw))
            }
            rumoca_core::Subscript::Colon { .. } => Err(LowerError::Unsupported {
                reason: "slice subscript `:` is unsupported".to_string(),
            }),
            _ => Err(LowerError::Unsupported {
                reason: "non-positive subscript is unsupported".to_string(),
            }),
        }
    }

    fn lower_structural_index_leaf(
        &mut self,
        element: &rumoca_core::Expression,
        projected_field: Option<&str>,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if let Some(field) = projected_field {
            return self.lower_field_access(element, field, scope, call_depth);
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

        let start_reg = self.lower_expr(start, scope, call_depth)?;
        let end_reg = self.lower_expr(end, scope, call_depth)?;
        let step_reg = if let Some(step_expr) = step.as_ref() {
            self.lower_expr(step_expr, scope, call_depth)?
        } else {
            let cond = self.emit_compare(CompareOp::Ge, end_reg, start_reg);
            let pos = self.emit_const(1.0);
            let neg = self.emit_const(-1.0);
            self.emit_select(cond, pos, neg)
        };

        let zero = self.emit_const(0.0);
        let step_gt_zero = self.emit_compare(CompareOp::Gt, step_reg, zero);
        let step_lt_zero = self.emit_compare(CompareOp::Lt, step_reg, zero);
        let start_le_end = self.emit_compare(CompareOp::Le, start_reg, end_reg);
        let start_ge_end = self.emit_compare(CompareOp::Ge, start_reg, end_reg);
        let forward_valid = self.emit_binary(BinaryOp::And, step_gt_zero, start_le_end);
        let backward_valid = self.emit_binary(BinaryOp::And, step_lt_zero, start_ge_end);
        let valid = self.emit_binary(BinaryOp::Or, forward_valid, backward_valid);

        let distance = self.emit_binary(BinaryOp::Sub, end_reg, start_reg);
        let ratio = self.emit_binary(BinaryOp::Div, distance, step_reg);
        let ratio_floor = self.emit_unary(UnaryOp::Floor, ratio);
        let one = self.emit_const(1.0);
        let n = self.emit_binary(BinaryOp::Add, ratio_floor, one);
        let two = self.emit_const(2.0);
        let two_start = self.emit_binary(BinaryOp::Mul, two, start_reg);
        let n_minus_one = self.emit_binary(BinaryOp::Sub, n, one);
        let stride = self.emit_binary(BinaryOp::Mul, n_minus_one, step_reg);
        let bracket = self.emit_binary(BinaryOp::Add, two_start, stride);
        let n_half = self.emit_binary(BinaryOp::Div, n, two);
        let sum = self.emit_binary(BinaryOp::Mul, n_half, bracket);

        let fallback = self.emit_const(0.0);
        Ok(Some(self.emit_select(valid, sum, fallback)))
    }

    pub(super) fn lower_array_like_values(
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
                let mut values = Vec::new();
                for element in elements {
                    values.extend(
                        self.lower_tuple_element_array_like_values(element, scope, call_depth)?,
                    );
                }
                Ok(values)
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
                start, step, end, ..
            } => self.lower_range_array_like_values(start, step.as_deref(), end),
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => self.lower_if_array_like_values(branches, else_branch, scope, call_depth),
            rumoca_core::Expression::Index {
                base, subscripts, ..
            } => self.lower_index_array_like_values(base, subscripts, scope, call_depth),
            rumoca_core::Expression::Binary { op, lhs, rhs, span } => {
                self.lower_binary_array_like_values(op, lhs, rhs, *span, scope, call_depth)
            }
            rumoca_core::Expression::Unary { op, rhs, .. } => {
                self.lower_unary_array_like_values(op, rhs, scope, call_depth)
            }
            _ => Ok(vec![self.lower_expr(expr, scope, call_depth)?]),
        };
        if let Some(span) = expr.span() {
            result.map_err(|err| err.with_fallback_span(span))
        } else {
            result
        }
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
        if is_synchronous_array_like_intrinsic(name.as_str()) {
            return self.lower_synchronous_array_like_intrinsic(
                name.as_str(),
                args,
                scope,
                call_depth,
            );
        }
        if is_stream_passthrough_intrinsic(name.as_str()) {
            return match args.first() {
                Some(arg) => self.lower_array_like_values(arg, scope, call_depth),
                None => Ok(Vec::new()),
            };
        }
        if let Some(values) = self.lower_random_array_values(name, args, scope, call_depth)? {
            return Ok(values);
        }
        if let Some(values) =
            self.lower_structural_standard_array_values(name.as_str(), args, scope, call_depth)?
        {
            return Ok(values);
        }
        if self.is_record_constructor_call(name, is_constructor) {
            return self.lower_record_constructor_values(name, args, scope, call_depth);
        }
        if let Some(values) = self.lower_user_function_call_array_values(
            name,
            args,
            fallback.span().unwrap_or(rumoca_core::Span::DUMMY),
            scope,
            call_depth,
        )? {
            return Ok(values);
        }
        Ok(vec![self.lower_expr(fallback, scope, call_depth)?])
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
            .multiply_array_operands(&lhs, &rhs)
            .map_err(|err| err.with_fallback_span(span))?;
        match result.values.as_slice() {
            [value] => Ok(*value),
            values => Err(unsupported_at(
                format!(
                    "non-scalar multiplication result with width {} is unsupported in scalar context (lhs_shape={:?}, rhs_shape={:?}, result_shape={:?})",
                    values.len(),
                    lhs.dims,
                    rhs.dims,
                    result.dims,
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
            Op::Div => self.lower_division_array_values(lhs, rhs, scope, call_depth),
            Op::Mul => {
                let lhs = self.lower_array_operand(lhs, scope, call_depth)?;
                let rhs = self.lower_array_operand(rhs, scope, call_depth)?;
                Ok(self.multiply_array_operands(&lhs, &rhs)?.values)
            }
            Op::Exp | Op::And | Op::Or | Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq | Op::Neq => {
                let lhs = self.lower_array_operand(lhs, scope, call_depth)?;
                let rhs = self.lower_array_operand(rhs, scope, call_depth)?;
                if lhs.is_scalar() && rhs.is_scalar() {
                    let value = self.lower_binary(op.clone(), lhs.values[0], rhs.values[0])?;
                    return Ok(vec![value]);
                }
                Err(LowerError::Unsupported {
                    reason: format!("operator `{op}` is unsupported for array-valued solve rows"),
                })
            }
            Op::Assign | Op::Empty => Err(LowerError::Unsupported {
                reason: format!("binary operator `{op}` is unsupported"),
            }),
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
        Ok(broadcast_pairs(&lhs_operand, &rhs_operand)
            .map_err(|err| err.with_fallback_span(span))?
            .into_iter()
            .map(|(lhs, rhs)| self.emit_binary(op, lhs, rhs))
            .collect())
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
            });
        }
        self.lower_array_operand(expr, scope, call_depth)
    }

    fn lower_division_array_values(
        &mut self,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let lhs = self.lower_array_operand(lhs, scope, call_depth)?;
        let rhs_span = rhs.span();
        let rhs = self.lower_array_operand(rhs, scope, call_depth)?;
        if !rhs.is_scalar() {
            return Err(LowerError::Unsupported {
                // MLS §10.6.5: ordinary division is only array divided by
                // scalar. Element-wise division is represented by `./`.
                reason: format!(
                    "array division requires a scalar denominator (lhs_shape={:?}, lhs_values={}, rhs_shape={:?}, rhs_values={}, rhs_span={:?})",
                    lhs.dims,
                    lhs.values.len(),
                    rhs.dims,
                    rhs.values.len(),
                    rhs_span
                ),
            });
        }
        Ok(lhs
            .values
            .iter()
            .copied()
            .map(|value| self.emit_binary(BinaryOp::Div, value, rhs.values[0]))
            .collect())
    }

    pub(super) fn lower_array_operand(
        &mut self,
        expr: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<ArrayOperand, LowerError> {
        let values = self.lower_array_like_values(expr, scope, call_depth)?;
        let dims = if self.expr_uses_flat_call_or_tuple_width(expr) {
            vector_dims(values.len())
        } else {
            self.infer_expr_dims(expr, scope)?
        };
        if !dims.is_empty() {
            let expected = shape_size(&dims);
            if expected != values.len() {
                return Err(unsupported_at(
                    format!(
                        "array expression shape {:?} requires {expected} scalar values, got {}",
                        dims,
                        values.len()
                    ),
                    expr.span().unwrap_or(rumoca_core::Span::DUMMY),
                ));
            }
            return Ok(ArrayOperand { values, dims });
        }
        Ok(ArrayOperand::new(values, dims))
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

    fn multiply_array_operands(
        &mut self,
        lhs: &ArrayOperand,
        rhs: &ArrayOperand,
    ) -> Result<ArrayOperand, LowerError> {
        match (lhs.dims.as_slice(), rhs.dims.as_slice()) {
            ([], []) => Ok(ArrayOperand::scalar(self.emit_binary(
                BinaryOp::Mul,
                lhs.values[0],
                rhs.values[0],
            ))),
            ([], _) => Ok(self.scale_operand(lhs.values[0], rhs)),
            (_, []) => Ok(self.scale_operand(rhs.values[0], lhs)),
            ([lhs_len], [rhs_len]) if lhs_len == rhs_len => {
                // MLS §10.6.5: vector * vector is the scalar product.
                Ok(ArrayOperand::scalar(
                    self.emit_sum_of_products(&lhs.values, &rhs.values),
                ))
            }
            ([rows, inner], [rhs_len]) if inner == rhs_len => {
                let mut values = Vec::with_capacity(*rows);
                for row in 0..*rows {
                    let lhs_row = &lhs.values[row * inner..(row + 1) * inner];
                    values.push(self.emit_sum_of_products(lhs_row, &rhs.values));
                }
                Ok(ArrayOperand {
                    values,
                    dims: vec![*rows],
                })
            }
            ([lhs_len], [inner, cols]) if lhs_len == inner => {
                let mut values = Vec::with_capacity(*cols);
                for col in 0..*cols {
                    let col_values = (0..*inner)
                        .map(|idx| rhs.values[idx * cols + col])
                        .collect::<Vec<_>>();
                    values.push(self.emit_sum_of_products(&lhs.values, &col_values));
                }
                Ok(ArrayOperand {
                    values,
                    dims: vec![*cols],
                })
            }
            ([rows, inner], [rhs_inner, cols]) if inner == rhs_inner => {
                let values = self.emit_matrix_product_values(lhs, rhs, *rows, *inner, *cols);
                Ok(ArrayOperand {
                    values,
                    dims: vec![*rows, *cols],
                })
            }
            _ => Err(LowerError::Unsupported {
                reason: format!(
                    "array multiplication with shapes {:?} and {:?} is unsupported",
                    lhs.dims, rhs.dims
                ),
            }),
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
        if lhs_values.len() != m * k {
            return Err(LowerError::Unsupported {
                reason: format!(
                    "MatMul lhs: expected {} values for {}×{} matrix, got {}",
                    m * k,
                    m,
                    k,
                    lhs_values.len()
                ),
            });
        }
        let lhs_start = lhs_builder.pack_registers(&lhs_values);
        let lhs_next = lhs_builder.next_reg;

        let mut rhs_builder = self.fork_with_next_reg(lhs_next);
        let rhs_values = rhs_builder.lower_array_like_values(rhs_expr, scope, call_depth)?;
        if rhs_values.len() != k * n {
            return Err(LowerError::Unsupported {
                reason: format!(
                    "MatMul rhs: expected {} values for {}×{} matrix, got {}",
                    k * n,
                    k,
                    n,
                    rhs_values.len()
                ),
            });
        }
        let rhs_start = rhs_builder.pack_registers(&rhs_values);

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

    fn emit_matrix_product_values(
        &mut self,
        lhs: &ArrayOperand,
        rhs: &ArrayOperand,
        rows: usize,
        inner: usize,
        cols: usize,
    ) -> Vec<Reg> {
        let mut values = Vec::with_capacity(rows * cols);
        for row in 0..rows {
            let lhs_row = &lhs.values[row * inner..(row + 1) * inner];
            values.extend((0..cols).map(|col| {
                let col_values = (0..inner)
                    .map(|idx| rhs.values[idx * cols + col])
                    .collect::<Vec<_>>();
                self.emit_sum_of_products(lhs_row, &col_values)
            }));
        }
        values
    }

    fn scale_operand(&mut self, scalar: Reg, operand: &ArrayOperand) -> ArrayOperand {
        let values = operand
            .values
            .iter()
            .copied()
            .map(|value| self.emit_binary(BinaryOp::Mul, scalar, value))
            .collect();
        ArrayOperand {
            values,
            dims: operand.dims.clone(),
        }
    }

    fn emit_sum_of_products(&mut self, lhs_values: &[Reg], rhs_values: &[Reg]) -> Reg {
        let Some((&lhs_first, &rhs_first)) = lhs_values.first().zip(rhs_values.first()) else {
            return self.emit_const(0.0);
        };
        let mut acc = self.emit_binary(BinaryOp::Mul, lhs_first, rhs_first);
        for (&lhs, &rhs) in lhs_values.iter().zip(rhs_values.iter()).skip(1) {
            let term = self.emit_binary(BinaryOp::Mul, lhs, rhs);
            acc = self.emit_binary(BinaryOp::Add, acc, term);
        }
        acc
    }

    pub(super) fn lower_min_max_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
        op: BinaryOp,
        empty_identity: f64,
    ) -> Result<Reg, LowerError> {
        // MLS §10.3.4: min(A) and max(A) are reductions over every element of
        // an array expression. Keep this in solve-IR instead of scalarizing by
        // reading only the first array element.
        if args.len() == 1 {
            let mut values = self.lower_array_like_values(&args[0], scope, call_depth)?;
            let Some(mut acc) = values.pop() else {
                return Ok(self.emit_const(empty_identity));
            };
            while let Some(value) = values.pop() {
                acc = self.emit_binary(op, value, acc);
            }
            return Ok(acc);
        }

        let Some(first) = args.first() else {
            return Ok(self.emit_const(empty_identity));
        };
        let mut acc = self.lower_expr(first, scope, call_depth)?;
        for expr in args.iter().skip(1) {
            let value = self.lower_expr(expr, scope, call_depth)?;
            acc = self.emit_binary(op, acc, value);
        }
        Ok(acc)
    }

    fn lower_var_ref_array_like_values(
        &mut self,
        name: &rumoca_core::Reference,
        expr: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let generated_key = generated_scope_key(name.as_str());
        if let Some(values) = scoped_indexed_binding_values(scope, &generated_key) {
            return Ok(values);
        }
        if let Some(values) = self.local_indexed_binding_values(name.as_str()) {
            return Ok(values);
        }
        if let Some(reg) = scope.get(&generated_key).copied() {
            return Ok(vec![reg]);
        }

        let key_path =
            self.scope_key_from_reference(name, expr.span().unwrap_or(rumoca_core::Span::DUMMY))?;
        let key = name.as_str();
        if let Some(values) = scoped_indexed_binding_values(scope, &key_path) {
            return Ok(values);
        }
        if let Some(values) = self.local_indexed_binding_values(key) {
            return Ok(values);
        }
        if self.known_empty_local_arrays.contains(key) {
            return Ok(Vec::new());
        }
        if self
            .structural_bindings
            .get(super::size_binding_key(key, 1).as_str())
            .is_some_and(|dim| *dim == 0.0)
        {
            return Ok(Vec::new());
        }
        if let Some(reg) = scope.get(&key_path).copied() {
            return Ok(vec![reg]);
        }
        if let Some(pre_key) = self.pre_mode_base_key(key)
            && let Some(values) = self.lower_indexed_binding_values(pre_key.as_str())?
        {
            return Ok(values);
        }
        if let Some(values) = self.lower_direct_assignment_values_for_key(key, scope, call_depth)? {
            return Ok(values);
        }
        if let Some(values) = self.lower_indexed_binding_values_for_reference(
            name,
            expr.span().unwrap_or(rumoca_core::Span::DUMMY),
        )? {
            return Ok(values);
        }
        if let Some(values) = self.lower_record_field_array_values(key)? {
            return Ok(values);
        }
        if let Some(values) = self.lower_scalarized_component_values(&key_path, scope)? {
            return Ok(values);
        }
        Ok(vec![self.lower_expr(expr, scope, call_depth)?])
    }

    fn lower_record_field_array_values(
        &mut self,
        key: &str,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let Some((base_key, field)) = record_field_array_key(key) else {
            return Ok(None);
        };
        let field_keys = self.indexed_record_field_keys(base_key, field);
        if field_keys.is_empty() {
            return Ok(None);
        }
        let keys = field_keys.values().cloned().collect::<Vec<_>>();
        self.load_binding_keys(&keys).map(Some)
    }

    fn indexed_record_field_keys(
        &self,
        base_key: &str,
        field: &str,
    ) -> IndexMap<Vec<usize>, String> {
        let mut keys = IndexMap::new();
        for (binding_key, _slot) in self.layout.bindings() {
            if let Some(indices) = indexed_record_field_key_indices(binding_key, base_key, field) {
                keys.entry(indices).or_insert_with(|| binding_key.clone());
            }
        }
        for assignment_key in self.direct_assignments.keys() {
            if let Some(indices) = indexed_record_field_key_indices(assignment_key, base_key, field)
            {
                keys.entry(indices)
                    .or_insert_with(|| assignment_key.clone());
            }
        }
        keys.sort_keys();
        keys
    }

    fn lower_scalarized_component_values(
        &mut self,
        base_key: &ComponentReferenceKey,
        scope: &Scope,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let mut values = scope
            .iter()
            .into_iter()
            .filter_map(|(key, reg)| {
                scope_key_direct_child_suffix(&key, base_key).map(|_| (key, reg))
            })
            .collect::<IndexMap<_, _>>();

        let child_paths = self
            .layout
            .bindings()
            .keys()
            .filter_map(|name| {
                let key = generated_scope_key(name);
                scope_key_direct_child_suffix(&key, base_key)?;
                (!values.contains_key(&key)).then_some((key, name.clone()))
            })
            .collect::<Vec<_>>();

        for (key, name) in child_paths {
            let slot = self
                .layout
                .binding(&name)
                .ok_or(LowerError::MissingBinding { name })?;
            let reg = self.emit_slot_load(slot)?;
            values.insert(key, reg);
        }

        if values.is_empty() {
            return Ok(None);
        }

        Ok(Some(values.into_values().collect()))
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
        if let Some(values) = self.lower_indexed_reference_slice_values(
            name,
            subscripts,
            expr_span_from_subscripts(subscripts),
            scope,
        )? {
            return Ok(values);
        }
        let Some(keys) = self.slice_binding_keys(name.as_str(), subscripts, scope)? else {
            // MLS §10.5: a scalar subscript selects one array element. The
            // array-like lowering path is used by element-wise intrinsics too,
            // so a non-slice selection must fall back to scalar VarRef lowering
            // instead of being rejected as a dynamic array slice.
            return Ok(vec![self.lower_var_ref(
                name,
                subscripts,
                expr_span_from_subscripts(subscripts),
                scope,
                call_depth,
            )?]);
        };
        self.load_binding_keys(&keys)
    }

    fn lower_indexed_reference_slice_values(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
        scope: &Scope,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let entries = indexed_entries_for_reference(&self.indexed_bindings, name, span)?;
        if entries.is_empty() {
            return Ok(None);
        }
        let dims = infer_indexed_dims(&entries);
        if dims.is_empty() {
            return Ok(None);
        }
        let selections = self.slice_selections(subscripts, &dims, scope)?;
        let selected_entries = sorted_flat_entries(&entries)
            .into_iter()
            .filter(|entry| indexed_entry_matches_selections(entry, &selections))
            .cloned()
            .collect::<Vec<_>>();
        if selected_entries.is_empty() {
            return Err(LowerError::Unsupported {
                reason: format!(
                    "array slice for `{}` selected no indexed solve-layout bindings",
                    name.as_str()
                ),
            });
        }
        self.lower_indexed_entries_values(name.as_str(), &selected_entries)
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
        let generated_key_path = generated_scope_key(key);
        let key_path = if scope.contains_key(&generated_key_path)
            || scope.indexed_entries(&generated_key_path).is_some()
        {
            generated_key_path
        } else {
            self.scope_key_from_reference(name, expr_span_from_subscripts(subscripts))?
        };
        let Some(dims) = self.local_binding_dims.get(key).cloned() else {
            if scope.contains_key(&key_path) {
                return Err(LowerError::Unsupported {
                    reason: format!("subscripted local binding `{key}` is not an array binding"),
                });
            }
            return Ok(LocalSubscriptResolution::NotLocal);
        };
        let Some(bindings) = scope.indexed_entries(&key_path) else {
            if let Some(indices) = self.compile_time_subscript_indices(subscripts)? {
                return Err(LowerError::MissingBinding {
                    name: format_subscript_binding_key(key, &indices),
                });
            }
            return Err(LowerError::Unsupported {
                reason: format!("subscripted local array `{key}` has no assigned elements"),
            });
        };
        if dims.iter().any(|dim| *dim < 0) {
            return Err(LowerError::Unsupported {
                reason: format!("subscripted local array `{key}` has negative dimensions {dims:?}"),
            });
        };
        let shape = dims.iter().map(|dim| *dim as usize).collect::<Vec<_>>();
        if subscripts.len() == shape.len() && subscripts.iter().all(is_scalar_selector_subscript) {
            let Some(indices) = self.compile_time_subscript_indices(subscripts)? else {
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
            .slice_selections(subscripts, &shape, scope)
            .map_err(|err| {
                err.with_context(format!(
                    "resolving subscripted local array `{key}` with shape {shape:?}"
                ))
            })?;
        let mut combos = Vec::new();
        collect_slice_index_combos(&selections, 0, &mut Vec::new(), &mut combos);
        let mut regs = Vec::with_capacity(combos.len());
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
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        if let Some(values) =
            self.lower_function_output_projection_values(base, subscripts, scope, call_depth)?
        {
            return Ok(values);
        }
        if let Some(value) = self.lower_compile_time_indexed_local_value(base, subscripts, scope)? {
            return Ok(vec![value]);
        }
        if let Some(values) =
            self.lower_array_like_dynamic_selection_values(base, subscripts, scope, call_depth)?
        {
            return Ok(values);
        }
        if is_static_singleton_scalar_projection(base, subscripts)? {
            return Ok(vec![self.lower_expr(base, scope, call_depth)?]);
        }
        let base_key = dynamic_binding_base_key(base)?;
        if let Some(keys) = self.slice_binding_keys(base_key.as_str(), subscripts, scope)? {
            return self.load_binding_keys(&keys);
        }
        Ok(vec![self.lower_index(base, subscripts, scope, call_depth)?])
    }
}
