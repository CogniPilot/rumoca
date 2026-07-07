// SPEC_0021 file-size exception: dynamic array selection still combines record
// slice projection, compile-time indexing, and runtime selection. split plan:
// move record slice paths and runtime index lowering into separate modules.
use super::inference::concrete_i64_dims;
use super::*;

fn projected_field_expr_may_be_array_like(expr: &rumoca_core::Expression) -> bool {
    match expr {
        rumoca_core::Expression::Array { .. } => true,
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches
                .iter()
                .any(|(_, value)| projected_field_expr_may_be_array_like(value))
                || projected_field_expr_may_be_array_like(else_branch)
        }
        _ => false,
    }
}

fn resolve_zero_dims_from_value_count(
    dims: &[usize],
    value_count: usize,
    span: rumoca_core::Span,
) -> Result<Option<Vec<usize>>, LowerError> {
    let zero_count = dims.iter().filter(|dim| **dim == 0).count();
    if zero_count != 1 || value_count == 0 {
        return Ok(None);
    }
    let known_product = dims
        .iter()
        .filter(|dim| **dim != 0)
        .try_fold(1usize, |acc, dim| acc.checked_mul(*dim))
        .ok_or_else(|| {
            LowerError::contract_violation("array-like dynamic selection shape overflows", span)
        })?;
    if known_product == 0 || !value_count.is_multiple_of(known_product) {
        return Ok(None);
    }
    let mut resolved = dims.to_vec();
    let Some(zero_index) = resolved.iter().position(|dim| *dim == 0) else {
        return Ok(None);
    };
    resolved[zero_index] = value_count / known_product;
    Ok(Some(resolved))
}

fn collect_function_output_slice_indices(
    per_dim: &[Vec<usize>],
    dim_index: usize,
    current: &mut Vec<usize>,
    out: &mut Vec<Vec<usize>>,
) -> Result<(), LowerError> {
    if dim_index == per_dim.len() {
        out.try_reserve_exact(1)
            .map_err(|_| LowerError::UnspannedContractViolation {
                reason: "function output slice index count exceeds host memory limits".to_string(),
            })?;
        out.push(current.clone());
        return Ok(());
    }
    for index in &per_dim[dim_index] {
        current.push(*index);
        collect_function_output_slice_indices(per_dim, dim_index + 1, current, out)?;
        current.pop();
    }
    Ok(())
}

fn collect_full_shape_binding_keys(
    base_name: &str,
    shape: &[usize],
    depth: usize,
    current: &mut Vec<usize>,
    keys: &mut Vec<String>,
) {
    if depth == shape.len() {
        keys.push(format_subscript_binding_key(base_name, current));
        return;
    }
    for index in 1..=shape[depth] {
        current.push(index);
        collect_full_shape_binding_keys(base_name, shape, depth + 1, current, keys);
        current.pop();
    }
}

struct RecordArraySliceFieldPath<'a> {
    name: &'a rumoca_core::Reference,
    subscripts: &'a [rumoca_core::Subscript],
    span: rumoca_core::Span,
    fields: Vec<String>,
}

fn record_array_slice_field_path<'a>(
    base: &'a rumoca_core::Expression,
    leaf_field: &str,
) -> Option<RecordArraySliceFieldPath<'a>> {
    let mut fields = vec![leaf_field.to_string()];
    let mut cursor = base;
    let mut span = base.span()?;
    while let rumoca_core::Expression::FieldAccess {
        base: nested,
        field,
        span: field_span,
    } = cursor
    {
        fields.push(field.clone());
        span = *field_span;
        cursor = nested;
    }
    fields.reverse();
    let rumoca_core::Expression::Index {
        base: inner,
        subscripts,
        span: index_span,
    } = cursor
    else {
        return None;
    };
    let rumoca_core::Expression::VarRef {
        name,
        subscripts: ref_subscripts,
        ..
    } = inner.as_ref()
    else {
        return None;
    };
    if !ref_subscripts.is_empty() {
        return None;
    }
    Some(RecordArraySliceFieldPath {
        name,
        subscripts,
        span: if index_span.is_dummy() {
            span
        } else {
            *index_span
        },
        fields,
    })
}

impl<'a> LowerBuilder<'a> {
    pub(in crate::lower) fn lower_compile_time_indexed_local_value(
        &mut self,
        base: &rumoca_core::Expression,
        subscripts: &[rumoca_core::Subscript],
        owner_span: Option<rumoca_core::Span>,
        scope: &Scope,
    ) -> Result<Option<Reg>, LowerError> {
        let Ok(base_key) = dynamic_binding_base_key(base) else {
            return Ok(None);
        };
        let Some(owner_span) = owner_span.or_else(|| base.span().filter(|span| !span.is_dummy()))
        else {
            return Ok(None);
        };
        let Some(indices) = self.compile_time_subscript_indices(subscripts, owner_span)? else {
            return Ok(None);
        };
        if indices.is_empty() {
            return Ok(None);
        }
        let indexed_key = format_subscript_binding_key(base_key.as_str(), &indices);
        let indexed_path = generated_scope_key(&indexed_key);
        if let Some(value) = scope.get(&indexed_path).copied() {
            return Ok(Some(value));
        }
        if let Some(value) =
            self.local_indexed_bindings
                .get(base_key.as_str())
                .and_then(|bindings| {
                    bindings
                        .iter()
                        .find(|binding| binding.indices == indices)
                        .map(|binding| binding.reg)
                })
        {
            return Ok(Some(value));
        }
        if let Some(mut values) =
            self.lower_direct_assignment_values_for_key(indexed_key.as_str(), scope, 0)?
            && let Some(value) = values.pop()
        {
            return Ok(Some(value));
        }
        if let Some(slot) = self.pre_mode_slot_for_key(indexed_key.as_str()) {
            let span = required_expr_span_from_subscripts_or_base(
                subscripts,
                base,
                Some(owner_span),
                "compile-time indexed pre-mode slot load",
            )?;
            return Ok(Some(self.emit_slot_load(slot, span)?));
        }
        if let Some(slot) = self.layout.binding(indexed_key.as_str()) {
            let span = required_expr_span_from_subscripts_or_base(
                subscripts,
                base,
                Some(owner_span),
                "compile-time indexed slot load",
            )?;
            return Ok(Some(self.emit_slot_load(slot, span)?));
        }
        Ok(None)
    }

    pub(in crate::lower) fn lower_array_like_dynamic_index(
        &mut self,
        base: &rumoca_core::Expression,
        subscripts: &[rumoca_core::Subscript],
        owner_span: Option<rumoca_core::Span>,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let Some(mut values) = self.lower_array_like_dynamic_selection_values(
            base, subscripts, owner_span, scope, call_depth,
        )?
        else {
            return Ok(None);
        };
        if values.len() != 1 {
            return Err(unsupported_at(
                format!(
                    "array-like dynamic index selected {} values where a scalar was required",
                    values.len()
                ),
                required_expr_span_from_subscripts_or_base(
                    subscripts,
                    base,
                    owner_span,
                    "array-like dynamic index scalar selection",
                )?,
            ));
        }
        Ok(values.pop())
    }

    // SPEC_0021: Exception - dynamic selector assembly keeps selection proof,
    // tuple construction, and value lookup together to preserve span context.
    #[allow(clippy::excessive_nesting)]
    pub(in crate::lower) fn lower_array_like_dynamic_selection_values(
        &mut self,
        base: &rumoca_core::Expression,
        subscripts: &[rumoca_core::Subscript],
        owner_span: Option<rumoca_core::Span>,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        if !matches!(
            base,
            rumoca_core::Expression::VarRef { .. }
                | rumoca_core::Expression::Array { .. }
                | rumoca_core::Expression::Tuple { .. }
                | rumoca_core::Expression::If { .. }
                | rumoca_core::Expression::Index { .. }
                | rumoca_core::Expression::FieldAccess { .. }
                | rumoca_core::Expression::Binary { .. }
                | rumoca_core::Expression::BuiltinCall { .. }
        ) {
            return Ok(None);
        }
        let mut dims = self.infer_expr_dims(base, scope)?;
        if dims.is_empty() || subscripts.len() > dims.len() {
            return Ok(None);
        }

        let span = required_expr_span_from_subscripts_or_base(
            subscripts,
            base,
            owner_span,
            "array-like dynamic selection",
        )?;
        let values = self.lower_array_like_values_with_optional_source_context(
            base, owner_span, scope, call_depth,
        )?;
        if dims.contains(&0)
            && !values.is_empty()
            && let Some(resolved) = resolve_zero_dims_from_value_count(&dims, values.len(), span)?
        {
            dims = resolved;
        }
        let index_tuples = one_based_index_tuples(&dims, span)?;
        if index_tuples.is_empty() {
            return Ok(Some(Vec::new()));
        }
        if values.len() != index_tuples.len() {
            return Err(unsupported_at(
                format!(
                    "array-like dynamic index shape mismatch: {} values for shape {}",
                    values.len(),
                    format_usize_dims(&dims)
                ),
                span,
            ));
        }

        let selection_parts =
            self.lower_array_like_selection_parts(subscripts, &dims, span, scope, call_depth)?;
        let mut slice_choices = array_vec_with_capacity(
            selection_parts.len(),
            "array dynamic slice choice count",
            span,
        )?;
        for part in &selection_parts {
            if let Some(indices) = &part.slice_indices {
                let mut choice = array_vec_with_capacity(
                    indices.len(),
                    "array dynamic slice index count",
                    span,
                )?;
                for index in indices {
                    choice.push(*index);
                }
                slice_choices.push(choice);
            }
        }
        let output_tuples = if slice_choices.is_empty() {
            let mut tuples = array_vec_with_capacity(1, "array dynamic output tuple count", span)?;
            tuples.push(Vec::new());
            tuples
        } else {
            index_choice_tuples(&slice_choices, span)?
        };
        let mut selected_values = array_vec_with_capacity(
            output_tuples.len(),
            "array dynamic selected value count",
            span,
        )?;
        for output_tuple in output_tuples {
            selected_values.push(self.select_array_like_output_tuple(
                &selection_parts,
                &index_tuples,
                &values,
                &output_tuple,
                span,
            )?);
        }
        Ok(Some(selected_values))
    }

    pub(in crate::lower) fn select_array_like_output_tuple(
        &mut self,
        selection_parts: &[ArraySelectionPart],
        index_tuples: &[Vec<usize>],
        values: &[Reg],
        output_tuple: &[usize],
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let mut merged = self.emit_const_at(0.0, span)?;
        for (indices, value) in index_tuples.iter().zip(values.iter().copied()) {
            if slice_indices_match(selection_parts, indices, output_tuple) {
                let cond =
                    self.emit_non_slice_subscript_match_at(selection_parts, indices, span)?;
                merged = self.emit_select_at(cond, value, merged, span)?;
            }
        }
        Ok(merged)
    }

    pub(in crate::lower) fn lower_array_like_selection_parts(
        &mut self,
        subscripts: &[rumoca_core::Subscript],
        dims: &[usize],
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<ArraySelectionPart>, LowerError> {
        let mut parts =
            array_vec_with_capacity(dims.len(), "array dynamic selection part count", span)?;
        for (dim_index, dim) in dims.iter().copied().enumerate() {
            let part = match subscripts.get(dim_index) {
                Some(rumoca_core::Subscript::Colon { span }) => ArraySelectionPart {
                    selector: None,
                    slice_indices: Some(one_based_indices(
                        dim,
                        "array dynamic colon slice count",
                        *span,
                    )?),
                },
                Some(rumoca_core::Subscript::Expr { expr, .. })
                    if matches!(expr.as_ref(), rumoca_core::Expression::Range { .. }) =>
                {
                    let range_subscript = rumoca_core::Subscript::try_generated_expr(
                        expr.clone(),
                        span,
                        "array dynamic range selection subscript",
                    )?;
                    ArraySelectionPart {
                        selector: None,
                        slice_indices: Some(self.slice_subscript_indices(
                            &range_subscript,
                            dim,
                            scope,
                        )?),
                    }
                }
                Some(subscript) => ArraySelectionPart {
                    selector: Some(
                        self.lower_structural_index_selector(subscript, span, scope, call_depth)?,
                    ),
                    slice_indices: None,
                },
                None => ArraySelectionPart {
                    selector: None,
                    slice_indices: Some(one_based_indices(
                        dim,
                        "array dynamic implicit slice count",
                        span,
                    )?),
                },
            };
            parts.push(part);
        }
        Ok(parts)
    }

    pub(in crate::lower) fn emit_non_slice_subscript_match_at(
        &mut self,
        parts: &[ArraySelectionPart],
        indices: &[usize],
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let mut regs = Vec::new();
        let mut expected = Vec::new();
        for (part, index) in parts.iter().zip(indices.iter().copied()) {
            if let Some(selector) = part.selector {
                regs.push(selector);
                expected.push(index);
            }
        }
        if regs.is_empty() {
            return self.emit_const_at(1.0, span);
        }
        self.emit_subscript_match_at(&regs, &expected, span)
    }

    pub(in crate::lower) fn load_binding_keys(
        &mut self,
        keys: &[String],
        span: rumoca_core::Span,
    ) -> Result<Vec<Reg>, LowerError> {
        let mut values =
            array_vec_with_capacity(keys.len(), "array binding load value count", span)?;
        for key in keys {
            if let Some(mut direct_values) =
                self.lower_direct_assignment_values_for_key(key, &Scope::new(), 0)?
                && let Some(value) = direct_values.pop()
            {
                values.push(value);
                continue;
            }
            let Some(slot) = self.layout.binding(key.as_str()) else {
                return Err(LowerError::MissingBinding { name: key.clone() });
            };
            values.push(self.emit_slot_load(slot, span)?);
        }
        Ok(values)
    }

    pub(in crate::lower) fn local_indexed_binding_values(&self, key: &str) -> Option<Vec<Reg>> {
        let bindings = self.local_indexed_bindings.get(key)?;
        if bindings.is_empty() {
            return None;
        }
        let mut values = bindings
            .iter()
            .map(|binding| (binding.indices.clone(), binding.reg))
            .collect::<Vec<_>>();
        values.sort_by(|(lhs, _), (rhs, _)| lhs.cmp(rhs));
        Some(values.into_iter().map(|(_, reg)| reg).collect())
    }

    pub(in crate::lower) fn slice_binding_keys(
        &self,
        base_name: &str,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
        scope: &Scope,
    ) -> Result<Option<Vec<String>>, LowerError> {
        let shape: Option<Vec<usize>> = if let Some(shape) = self.layout.shape(base_name) {
            Some(shape.to_vec())
        } else if let Some(variable) = self
            .dae_variables
            .and_then(|variables| dae_variable(variables, &rumoca_core::VarName::new(base_name)))
            .filter(|variable| !variable.dims.is_empty())
        {
            Some(concrete_i64_dims(
                &variable.dims,
                base_name,
                "DAE variable dimensions",
                span,
            )?)
        } else {
            None
        };
        let Some(shape) = shape else {
            return Ok(None);
        };
        if subscripts.is_empty() {
            return Ok(None);
        }

        let selections = self.slice_selections(subscripts, &shape, span, scope)?;
        let mut keys = Vec::new();
        collect_slice_binding_keys(base_name, &selections, 0, &mut Vec::new(), &mut keys);
        Ok(Some(keys))
    }

    pub(in crate::lower) fn slice_selections(
        &self,
        subscripts: &[rumoca_core::Subscript],
        shape: &[usize],
        span: rumoca_core::Span,
        scope: &Scope,
    ) -> Result<Vec<Vec<usize>>, LowerError> {
        if subscripts.len() > shape.len() {
            return Err(unsupported_at(
                "array slice has more subscripts than dimensions",
                span,
            ));
        }

        let mut selections =
            array_vec_with_capacity(shape.len(), "array slice selection count", span)?;
        for (dim_index, subscript) in subscripts.iter().enumerate() {
            selections.push(self.slice_subscript_indices(subscript, shape[dim_index], scope)?);
        }
        for &dim in &shape[subscripts.len()..] {
            selections.push(one_based_indices(
                dim,
                "array implicit slice index count",
                span,
            )?);
        }
        Ok(selections)
    }

    pub(in crate::lower) fn slice_subscript_indices(
        &self,
        subscript: &rumoca_core::Subscript,
        dim: usize,
        scope: &Scope,
    ) -> Result<Vec<usize>, LowerError> {
        match subscript {
            rumoca_core::Subscript::Index { value, span } if *value > 0 => single_usize_vec(
                crate::lower::helpers::positive_i64_index(*value, *span)?,
                "array scalar slice index count",
                *span,
            ),
            rumoca_core::Subscript::Expr { expr, span } => {
                self.slice_expr_indices(expr, dim, scope, *span)
            }
            rumoca_core::Subscript::Colon { span } => {
                one_based_indices(dim, "array colon slice index count", *span)
            }
            _ => Err(unsupported_at(
                "non-positive subscript is unsupported".to_string(),
                subscript.span(),
            )),
        }
    }

    // SPEC_0021: Exception - range and scalar selector normalization stay
    // together so selector errors report the original subscript span.
    #[allow(clippy::excessive_nesting)]
    pub(in crate::lower) fn slice_expr_indices(
        &self,
        expr: &rumoca_core::Expression,
        dim: usize,
        scope: &Scope,
        context_span: rumoca_core::Span,
    ) -> Result<Vec<usize>, LowerError> {
        let const_scope = self.compile_time_slice_bindings(scope);
        let span = expr.span().unwrap_or(context_span);
        let indices = match expr {
            rumoca_core::Expression::Range {
                start,
                step,
                end,
                span: range_span,
            } => {
                let range_span = if range_span.is_dummy() {
                    span
                } else {
                    *range_span
                };
                let values = self.eval_compile_time_range_values(
                    start,
                    step.as_deref(),
                    end,
                    range_span,
                    &const_scope,
                    "array slice range",
                )?;
                let mut indices =
                    array_vec_with_capacity(values.len(), "array slice range index count", span)?;
                for value in values {
                    indices.push(usize::try_from(value).map_err(|_| {
                        unsupported_at("array slice range index must be positive", span)
                    })?);
                }
                indices
            }
            _ => single_usize_vec(
                self.eval_compile_time_positive_index_at(
                    expr,
                    &const_scope,
                    "array slice index",
                    span,
                )?,
                "array slice scalar index count",
                span,
            )?,
        };
        validate_slice_indices(&indices, dim, span)?;
        Ok(indices)
    }

    fn compile_time_slice_bindings(&self, _scope: &Scope) -> IndexMap<String, f64> {
        let mut bindings = (*self.structural_bindings).clone();
        bindings.extend(self.local_const_bindings.clone());
        bindings
    }

    fn required_dynamic_selection_span(
        &self,
        candidates: &[&rumoca_core::Expression],
        context: &'static str,
    ) -> Result<rumoca_core::Span, LowerError> {
        candidates
            .iter()
            .find_map(|expr| expr.span())
            .or_else(|| self.active_source_context_span())
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: format!("missing source provenance for {context}"),
            })
    }

    pub(in crate::lower) fn lower_field_access_array_like_values(
        &mut self,
        base: &rumoca_core::Expression,
        field: &str,
        expr: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let owner_span = expr.span().or_else(|| self.active_source_context_span());
        if let Some(values) =
            self.lower_indexed_record_field_values(base, field, owner_span, scope)?
        {
            return Ok(values);
        }
        if let rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Fill,
            args,
            span,
        } = base
            && let Some(values) =
                self.lower_fill_field_array_like_values(args, field, scope, call_depth, *span)?
        {
            return Ok(values);
        }
        if let Some(values) =
            self.lower_constructor_field_array_like_values(base, field, scope, call_depth)?
        {
            return Ok(values);
        }
        if let Some(values) = self.lower_projected_function_field_array_like_values(
            base, field, expr, scope, call_depth,
        )? {
            return Ok(values);
        }
        if let Some(values) =
            self.lower_record_array_slice_field_values(base, field, owner_span, scope, call_depth)?
        {
            return Ok(values);
        }
        if let rumoca_core::Expression::Binary { op, lhs, rhs, span } = base
            && matches!(op, rumoca_core::OpBinary::Add | rumoca_core::OpBinary::Sub)
        {
            let lhs_field = field_access_expr_with_owner(lhs, field, *span);
            let rhs_field = field_access_expr_with_owner(rhs, field, *span);
            let lhs_values = self.lower_array_like_values(&lhs_field, scope, call_depth)?;
            let rhs_values = self.lower_array_like_values(&rhs_field, scope, call_depth)?;
            if lhs_values.len() != rhs_values.len() {
                return Err(LowerError::contract_violation(
                    format!(
                        "binary field projection `{field}` has mismatched array widths {} and {}",
                        lhs_values.len(),
                        rhs_values.len()
                    ),
                    *span,
                ));
            }
            let mut values = array_vec_with_capacity(
                lhs_values.len(),
                "binary field projection value count",
                *span,
            )?;
            for (lhs, rhs) in lhs_values.into_iter().zip(rhs_values) {
                values.push(self.lower_binary(op.clone(), lhs, rhs, *span)?);
            }
            return Ok(values);
        }
        if let Ok(key) = field_access_binding_key(base, field) {
            let span = owner_span.ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: format!("field access `{key}` requires source span metadata"),
            })?;
            let generated_key = generated_scope_key(&key);
            if let Some(values) = scoped_indexed_binding_values(scope, &generated_key, span)? {
                return Ok(values);
            }
            if let Some(values) = self.local_indexed_binding_values(&key) {
                return Ok(values);
            }
            if let Some(values) = self.lower_indexed_binding_values_at(key.as_str(), span)? {
                return Ok(values);
            }
            if let Some(values) = self.lower_record_field_array_values(key.as_str(), span)? {
                return Ok(values);
            }
            if let Some(values) = self.lower_shaped_flattened_field_values(&key, span)? {
                return Ok(values);
            }
            if let Some(reg) = self.lower_var_ref_binding_key(&key, span, scope, call_depth)? {
                return single_reg_vec(reg, "scalarized record field access value count", span);
            }
        }
        if let Some(values) = self.lower_structural_field_values(base, field, scope, call_depth)? {
            return Ok(values);
        }
        let span = self.required_dynamic_selection_span(&[expr], "field access fallback value")?;
        let mut values = array_vec_with_capacity(1, "field access fallback value count", span)?;
        values.push(self.lower_expr(expr, scope, call_depth)?);
        Ok(values)
    }

    fn lower_shaped_flattened_field_values(
        &mut self,
        key: &str,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let shape = if let Some(shape) = self.layout.shape(key) {
            shape.to_vec()
        } else if let Some(variable) = self
            .dae_variables
            .and_then(|variables| dae_variable(variables, &rumoca_core::VarName::new(key)))
            .filter(|variable| !variable.dims.is_empty())
        {
            concrete_i64_dims(&variable.dims, key, "flattened field dimensions", span)?
        } else {
            return Ok(None);
        };
        if shape.is_empty() {
            return Ok(None);
        }
        let mut keys = Vec::new();
        collect_full_shape_binding_keys(key, &shape, 0, &mut Vec::new(), &mut keys);
        self.load_binding_keys(&keys, span).map(Some)
    }

    #[allow(clippy::excessive_nesting)]
    fn lower_projected_function_field_array_like_values(
        &mut self,
        base: &rumoca_core::Expression,
        field: &str,
        expr: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        if !matches!(base, rumoca_core::Expression::FunctionCall { .. }) {
            return Ok(None);
        }
        let span = expr
            .span()
            .or_else(|| self.active_source_context_span())
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: "projected function field array access requires source span".to_string(),
            })?;
        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
            ..
        } = base
            && let Some(materialized) = self.materialize_single_record_function_call_components(
                name, args, span, scope, call_depth,
            )?
        {
            for (suffix, bindings) in materialized.indexed_components {
                if suffix == field {
                    let mut values = bindings
                        .into_iter()
                        .map(|binding| (binding.indices, binding.reg))
                        .collect::<Vec<_>>();
                    values.sort_by(|(lhs, _), (rhs, _)| lhs.cmp(rhs));
                    return Ok(Some(values.into_iter().map(|(_, reg)| reg).collect()));
                }
            }
            for component in materialized.components {
                if component.suffix == field {
                    return Ok(Some(vec![component.reg]));
                }
            }
        }
        let mut dae_model = dae::Dae::default();
        dae_model.symbols.functions = self.functions.clone();
        if let Some(variables) = self.dae_variables {
            dae_model.variables = variables.clone();
        }
        let field_expr = rumoca_core::Expression::FieldAccess {
            base: Box::new(base.clone()),
            field: field.to_string(),
            span,
        };
        let Some(expressions) = (match derivative_rhs::function_call_projected_scalars_with_owner(
            &field_expr,
            &dae_model,
            self.structural_bindings.as_ref(),
            span,
        ) {
            Ok(values) => values,
            Err(_) => return Ok(None),
        }) else {
            return Ok(None);
        };
        if expressions.len() == 1 && projected_field_expr_may_be_array_like(&expressions[0]) {
            return self
                .lower_array_like_values(&expressions[0], scope, call_depth + 1)
                .map(Some);
        }
        let mut values = array_vec_with_capacity(
            expressions.len(),
            "projected function field array scalar count",
            span,
        )?;
        for expression in expressions {
            values.push(self.lower_expr(&expression, scope, call_depth + 1)?);
        }
        Ok(Some(values))
    }

    /// Lowers a record-array member slice such as `ac.pin[:].v` or
    /// `coil.ele[:].vol1.T` by loading each scalarized element variable.
    /// Declines when the selection is not a full one-dimensional colon slice
    /// over a structured base or no element variables exist in the layout.
    fn lower_record_array_slice_field_values(
        &mut self,
        base: &rumoca_core::Expression,
        field: &str,
        owner_span: Option<rumoca_core::Span>,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let Some(slice) = record_array_slice_field_path(base, field) else {
            return Ok(None);
        };
        if slice.subscripts.len() != 1
            || !matches!(slice.subscripts[0], rumoca_core::Subscript::Colon { .. })
        {
            return Ok(None);
        }
        let Some(component_ref) = slice.name.component_ref() else {
            return Ok(None);
        };
        let span = Self::non_dummy_span(slice.span)
            .or(owner_span)
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: "missing source provenance for record array member slice".to_string(),
            })?;
        let field_path = slice.fields.join(".");
        let mut values = Vec::new();
        for element in 1.. {
            let key = format!("{}[{element}].{field_path}", slice.name.as_str());
            if self.layout.binding(&key).is_none() {
                break;
            }
            let mut element_ref = component_ref.clone();
            let Some(last) = element_ref.parts.last_mut() else {
                return Ok(None);
            };
            let element_index = i64::from(element);
            last.subs = vec![rumoca_core::Subscript::try_generated_index(
                element_index,
                span,
                "record array member slice subscript",
            )?];
            for field in &slice.fields {
                element_ref.parts.push(rumoca_core::ComponentRefPart {
                    ident: field.clone(),
                    span,
                    subs: Vec::new(),
                });
            }
            let element_expr = rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::from_component_reference(element_ref),
                subscripts: vec![],
                span,
            };
            values.push(self.lower_expr(&element_expr, scope, call_depth)?);
        }
        if values.is_empty() {
            return Ok(None);
        }
        self.ensure_dense_record_array_slice(slice.name.as_str(), &field_path, values.len(), span)?;
        Ok(Some(values))
    }

    /// The slice probe above stops at the first missing `base[k].field`
    /// binding, so a gap in the scalarized element numbering would silently
    /// truncate the slice. Scalarization must emit dense 1..n elements;
    /// any element past the stopping point is a contract violation.
    pub(in crate::lower) fn ensure_dense_record_array_slice(
        &self,
        base: &str,
        field_path: &str,
        found: usize,
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        let prefix = format!("{base}[");
        let suffix = format!("].{field_path}");
        for key in self.layout.bindings().keys() {
            let Some(index) = key
                .strip_prefix(prefix.as_str())
                .and_then(|rest| rest.strip_suffix(suffix.as_str()))
                .and_then(|index| index.parse::<usize>().ok())
            else {
                continue;
            };
            if index > found {
                let missing = missing_record_array_member_index(found, span)?;
                return Err(LowerError::contract_violation(
                    format!(
                        "record-array member slice `{base}[:].{field_path}` is not densely \
                         scalarized: element {index} exists but element {missing} is missing",
                    ),
                    span,
                ));
            }
        }
        Ok(())
    }

    pub(in crate::lower) fn lower_constructor_field_array_like_values(
        &mut self,
        base: &rumoca_core::Expression,
        field: &str,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } = base
        else {
            return Ok(None);
        };
        if !self.is_record_constructor_call(name, *is_constructor) {
            return Ok(None);
        }

        let (named_args, positional_args) =
            function_calls::split_named_and_positional_call_args(name.as_str(), args)?;
        if let Some(expr) = named_args.get(field).copied() {
            return self
                .lower_array_like_values(expr, scope, call_depth)
                .map(Some);
        }

        let Some(constructor) = self.lookup_function(name).cloned() else {
            return Ok(None);
        };
        let Some(index) = constructor
            .inputs
            .iter()
            .position(|input| input.name == field)
        else {
            return Ok(None);
        };
        let Some(expr) = positional_args.get(index).copied().or_else(|| {
            constructor
                .inputs
                .get(index)
                .and_then(|input| input.default.as_ref())
        }) else {
            return Ok(None);
        };
        self.lower_array_like_values(expr, scope, call_depth)
            .map(Some)
    }

    // SPEC_0021: Exception - indexed record-field lowering handles scalar and
    // sliced selector forms in one pass to keep generated binding keys ordered.
    #[allow(clippy::excessive_nesting)]
    pub(in crate::lower) fn lower_indexed_record_field_values(
        &mut self,
        base: &rumoca_core::Expression,
        field: &str,
        owner_span: Option<rumoca_core::Span>,
        scope: &Scope,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let rumoca_core::Expression::Index {
            base: indexed_base,
            subscripts,
            ..
        } = base
        else {
            return Ok(None);
        };
        let Ok(base_key) = dynamic_binding_base_key(indexed_base) else {
            return Ok(None);
        };
        let field_keys = self.indexed_record_field_keys(base_key.as_str(), field);
        if field_keys.is_empty() {
            return Ok(None);
        }
        let dims = infer_dims_from_index_sets(field_keys.keys().cloned());
        if dims.is_empty() {
            return Ok(None);
        }
        if subscripts.len() == dims.len() && subscripts.iter().all(is_scalar_selector_subscript) {
            let span = required_expr_span_from_subscripts_or_base(
                subscripts,
                base,
                owner_span,
                "dynamic indexed record field scalar selection",
            )?;
            return self
                .lower_dynamic_indexed_record_field_value(subscripts, &field_keys, span, scope)
                .and_then(|value| {
                    value
                        .map(|reg| {
                            single_reg_vec(reg, "dynamic indexed record field value count", span)
                        })
                        .transpose()
                });
        }
        let span = required_expr_span_from_subscripts_or_base(
            subscripts,
            base,
            owner_span,
            "dynamic indexed record field slice selection",
        )?;
        let selections = self.slice_selections(subscripts, &dims, span, scope)?;
        let mut keys = Vec::new();
        collect_indexed_record_field_keys(
            base_key.as_str(),
            field,
            &selections,
            0,
            &mut Vec::new(),
            &mut keys,
        );
        self.load_binding_keys(&keys, span).map(Some)
    }

    pub(in crate::lower) fn lower_dynamic_indexed_record_field_value(
        &mut self,
        subscripts: &[rumoca_core::Subscript],
        field_keys: &IndexMap<Vec<usize>, String>,
        span: rumoca_core::Span,
        scope: &Scope,
    ) -> Result<Option<Reg>, LowerError> {
        let mut selectors =
            array_vec_with_capacity(subscripts.len(), "dynamic selector value count", span)?;
        for subscript in subscripts {
            selectors.push(self.lower_structural_index_selector(subscript, span, scope, 0)?);
        }
        let mut merged = self.emit_const_at(0.0, span)?;
        let mut matched = false;
        for (indices, key) in field_keys {
            if indices.len() != selectors.len() {
                continue;
            }
            let mut values = self.load_binding_keys(std::slice::from_ref(key), span)?;
            let value = values
                .pop()
                .ok_or_else(|| LowerError::MissingBinding { name: key.clone() })?;
            let cond = self.emit_subscript_match_at(&selectors, indices, span)?;
            merged = self.emit_select_at(cond, value, merged, span)?;
            matched = true;
        }
        Ok(matched.then_some(merged))
    }

    pub(in crate::lower) fn lower_range_array_like_values(
        &mut self,
        start: &rumoca_core::Expression,
        step: Option<&rumoca_core::Expression>,
        end: &rumoca_core::Expression,
        range_span: rumoca_core::Span,
    ) -> Result<Vec<Reg>, LowerError> {
        let span = if range_span.is_dummy() {
            start
                .span()
                .or_else(|| step.and_then(rumoca_core::Expression::span))
                .or_else(|| end.span())
                .ok_or_else(|| LowerError::UnspannedContractViolation {
                    reason: "missing source provenance for range array expansion".to_string(),
                })?
        } else {
            range_span
        };
        let Some(values) = lower_static_range_values(start, step, end, range_span)? else {
            return Err(unsupported_at(
                "dynamic range array expansion requires known structural bounds",
                span,
            ));
        };
        let mut regs = array_vec_with_capacity(values.len(), "range array value count", span)?;
        for value in values {
            regs.push(self.emit_const_at(value, span)?);
        }
        Ok(regs)
    }

    pub(in crate::lower) fn lower_unary_array_like_values(
        &mut self,
        op: &rumoca_core::OpUnary,
        rhs: &rumoca_core::Expression,
        owner_span: Option<rumoca_core::Span>,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let values = self.lower_array_like_values_with_optional_source_context(
            rhs, owner_span, scope, call_depth,
        )?;
        let span =
            rhs.span()
                .or(owner_span)
                .ok_or_else(|| LowerError::UnspannedContractViolation {
                    reason: "missing source provenance for unary array value lowering".to_string(),
                })?;
        let mut lowered = array_vec_with_capacity(values.len(), "unary array value count", span)?;
        for value in values {
            lowered.push(self.lower_unary(op.clone(), value, span)?);
        }
        Ok(lowered)
    }

    pub(in crate::lower) fn lower_user_function_call_array_values(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let output_projection = self.lookup_function_output_projection(name, span)?;
        if let Some(projection) = output_projection.as_ref()
            && let Some(values) =
                self.lower_fft_projection_values(projection, args, span, caller_scope, call_depth)?
        {
            return Ok(Some(values));
        }
        if output_projection.is_none()
            && self.lookup_function(name).is_some_and(|function| {
                function
                    .outputs
                    .first()
                    .is_some_and(|output| !output.dims.is_empty())
            })
            && let Some(values) = self.lower_user_function_call_output_values(
                name,
                args,
                span,
                caller_scope,
                call_depth,
                Some(0),
            )?
        {
            return Ok(Some(values));
        }
        if let Some(values) = self.lower_projected_scalar_function_call_values(
            name,
            args,
            span,
            caller_scope,
            call_depth,
        )? {
            return first_expression_output_values(self.lookup_function(name), values, span)
                .map(Some);
        }
        if let Some(projection) = output_projection.as_ref()
            && projection.indices.is_empty()
            && projection.output_field.is_none()
        {
            return self
                .lower_user_function_call_named_output_values(
                    &projection.base_function_name,
                    &projection.output_name,
                    args,
                    span,
                    caller_scope,
                    call_depth,
                )
                .map(Some);
        }
        self.lower_user_function_call_output_values(
            name,
            args,
            span,
            caller_scope,
            call_depth,
            Some(0),
        )
    }

    pub(in crate::lower) fn scoped_function_output_values(
        &self,
        output: &rumoca_core::FunctionParam,
        scope: &Scope,
    ) -> Result<Vec<Reg>, LowerError> {
        if output.type_class == Some(rumoca_core::ClassType::Record)
            && let Some(values) = record_output_component_values(&output.name, scope, output.span)?
        {
            return Ok(values);
        }
        if output.dims.is_empty() {
            return function_output_values(output, scope);
        }
        if let Some(values) = self.local_indexed_binding_values(&output.name) {
            return Ok(values);
        }
        let Some(dims) = self.local_binding_dims.get(&output.name) else {
            return function_output_values(output, scope);
        };
        if self.known_empty_local_arrays.contains(output.name.as_str()) {
            return Ok(Vec::new());
        }
        let mut resolved = output.clone();
        resolved.dims = dims.clone();
        function_output_values(&resolved, scope)
    }

    pub(in crate::lower) fn lower_function_output_projection_values(
        &mut self,
        base: &rumoca_core::Expression,
        subscripts: &[rumoca_core::Subscript],
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let rumoca_core::Expression::FunctionCall {
            name, args, span, ..
        } = base
        else {
            return Ok(None);
        };
        let Some(function) = self.lookup_function(name) else {
            return Ok(None);
        };
        if function.outputs.len() == 1 {
            return self.lower_single_output_function_index_values(
                name,
                args,
                *span,
                subscripts,
                caller_scope,
                call_depth,
            );
        }
        let [subscript] = subscripts else {
            return Ok(None);
        };
        let output_number = lower_subscript_index_with_owner(subscript, *span)?;
        if function.outputs.len() <= 1
            || output_number == 0
            || output_number > function.outputs.len()
        {
            return Ok(None);
        }
        self.lower_user_function_call_output_values(
            name,
            args,
            *span,
            caller_scope,
            call_depth,
            Some(output_number - 1),
        )
    }

    fn lower_single_output_function_index_values(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        subscripts: &[rumoca_core::Subscript],
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let Some(function) = self.lookup_function(name) else {
            return Ok(None);
        };
        let Some(output) = function.outputs.first() else {
            return Ok(None);
        };
        let dims = function_output_dims(name, output)?;
        let flat_indices =
            self.function_output_flat_indices(&dims, subscripts, caller_scope, span)?;
        if flat_indices.is_empty() {
            return Ok(None);
        }
        if let Some(values) = self.lower_projected_function_output_index_values(
            name,
            args,
            span,
            &flat_indices,
            caller_scope,
            call_depth,
        )? {
            return Ok(Some(values));
        }
        let Some(values) = self.lower_user_function_call_output_values(
            name,
            args,
            span,
            caller_scope,
            call_depth,
            Some(0),
        )?
        else {
            return Ok(None);
        };
        let mut selected = array_vec_with_capacity(
            flat_indices.len(),
            "function output slice projection value count",
            span,
        )?;
        for flat_index in flat_indices {
            let Some(value) = values.get(flat_index).copied() else {
                return Ok(None);
            };
            selected.push(value);
        }
        Ok(Some(selected))
    }

    fn function_output_flat_indices(
        &self,
        dims: &[usize],
        subscripts: &[rumoca_core::Subscript],
        scope: &Scope,
        span: rumoca_core::Span,
    ) -> Result<Vec<usize>, LowerError> {
        let selections = self.function_output_slice_indices(dims, subscripts, scope, span)?;
        if selections.is_empty() {
            return Ok(Vec::new());
        }
        let mut flat_indices = array_vec_with_capacity(
            selections.len(),
            "function output flat slice index count",
            span,
        )?;
        for indices in selections {
            let Some(flat_index) = flat_index_for_subscripts(dims, &indices) else {
                return Ok(Vec::new());
            };
            flat_indices.push(flat_index);
        }
        Ok(flat_indices)
    }

    fn lower_projected_function_output_index_values(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        flat_indices: &[usize],
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let mut dae_model = dae::Dae::default();
        dae_model.symbols.functions = self.functions.clone();
        if let Some(variables) = self.dae_variables {
            dae_model.variables = variables.clone();
        }
        let call = rumoca_core::Expression::FunctionCall {
            name: name.clone(),
            args: args.to_vec(),
            is_constructor: false,
            span,
        };
        let Some(expressions) = (match derivative_rhs::function_call_projected_scalars_with_owner(
            &call,
            &dae_model,
            self.structural_bindings.as_ref(),
            span,
        ) {
            Ok(values) => values,
            Err(_) => return Ok(None),
        }) else {
            return Ok(None);
        };
        let mut values = array_vec_with_capacity(
            flat_indices.len(),
            "projected function output slice value count",
            span,
        )?;
        for &flat_index in flat_indices {
            let Some(expression) = expressions.get(flat_index) else {
                return Ok(None);
            };
            values.push(self.lower_expr(expression, caller_scope, call_depth + 1)?);
        }
        Ok(Some(values))
    }

    fn function_output_slice_indices(
        &self,
        dims: &[usize],
        subscripts: &[rumoca_core::Subscript],
        scope: &Scope,
        span: rumoca_core::Span,
    ) -> Result<Vec<Vec<usize>>, LowerError> {
        if subscripts.len() > dims.len() {
            return Ok(Vec::new());
        }
        let mut per_dim = array_vec_with_capacity(dims.len(), "function output slice rank", span)?;
        for (dim_index, subscript) in subscripts.iter().enumerate() {
            per_dim.push(self.slice_subscript_indices(subscript, dims[dim_index], scope)?);
        }
        for &dim in &dims[subscripts.len()..] {
            per_dim.push(one_based_indices(
                dim,
                "function output implicit slice index count",
                span,
            )?);
        }
        let mut indices = array_vec_with_capacity(
            per_dim
                .iter()
                .try_fold(1usize, |count, dim_indices| {
                    count.checked_mul(dim_indices.len())
                })
                .ok_or_else(|| {
                    LowerError::contract_violation(
                        "function output slice selection count overflows host index range",
                        span,
                    )
                })?,
            "function output slice selection count",
            span,
        )?;
        collect_function_output_slice_indices(&per_dim, 0, &mut Vec::new(), &mut indices)?;
        Ok(indices)
    }

    pub(in crate::lower) fn lower_user_function_call_output_values(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        caller_scope: &Scope,
        call_depth: usize,
        output_index: Option<usize>,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        if call_depth >= MAX_FUNCTION_INLINE_DEPTH {
            return Ok(None);
        }
        let Some(function) = self.lookup_function(name).cloned() else {
            if let Some(closure) = self.lookup_function_closure(name, span)?.cloned() {
                let reg = self.lower_function_closure_call(
                    &closure,
                    args,
                    span,
                    caller_scope,
                    call_depth,
                )?;
                return single_reg_vec(reg, "function closure output value count", span).map(Some);
            }
            return Ok(None);
        };
        if function.external.is_some() || function.outputs.is_empty() {
            return Ok(None);
        }
        self.ensure_pure_inline_function(name.as_str(), &function, span)?;
        self.with_local_lower_frame(|this| {
            let bindings =
                this.bind_function_inputs(name, &function.inputs, args, caller_scope, call_depth)?;
            let mut scope = bindings.scope;
            this.local_const_bindings.extend(bindings.const_bindings);
            this.initialize_function_output_scope(&function, &mut scope, call_depth)?;
            let _returned = this.lower_statements(&function.body, &mut scope, call_depth + 1)?;
            let Some(output_index) = output_index else {
                return this
                    .scoped_all_function_output_values(&function, &scope)
                    .map(Some);
            };
            function
                .outputs
                .get(output_index)
                .map(|output| this.scoped_function_output_values(output, &scope))
                .transpose()
        })
    }

    fn scoped_all_function_output_values(
        &self,
        function: &rumoca_core::Function,
        scope: &Scope,
    ) -> Result<Vec<Reg>, LowerError> {
        let mut values = array_vec_with_capacity(
            function.outputs.len(),
            "function output value count",
            function.span,
        )?;
        for output in &function.outputs {
            append_reg_values(
                &mut values,
                self.scoped_function_output_values(output, scope)?,
                "function output value count",
                output.span,
            )?;
        }
        Ok(values)
    }

    pub(in crate::lower) fn lower_user_function_call_named_output_values(
        &mut self,
        function_name: &rumoca_core::VarName,
        output_name: &str,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        if call_depth >= MAX_FUNCTION_INLINE_DEPTH {
            return Err(LowerError::InvalidFunction {
                name: function_name.as_str().to_string(),
                reason: format!("recursion depth exceeded ({MAX_FUNCTION_INLINE_DEPTH})"),
            });
        }
        let Some(function) = self.lookup_function_key(function_name.as_str()).cloned() else {
            return Err(LowerError::MissingFunction {
                name: function_name.as_str().to_string(),
            });
        };
        if function.external.is_some() {
            return Err(unsupported_at(
                format!(
                    "external function call `{}` cannot be inlined",
                    function_name.as_str()
                ),
                span,
            ));
        }
        self.ensure_pure_inline_function(function_name.as_str(), &function, span)?;
        self.with_local_lower_frame(|this| {
            let bindings = this.bind_function_inputs_for_name(
                function_name.as_str(),
                &function.inputs,
                args,
                caller_scope,
                call_depth,
            )?;
            let mut scope = bindings.scope;
            this.local_const_bindings.extend(bindings.const_bindings);
            this.initialize_function_output_scope(&function, &mut scope, call_depth)?;
            let _returned = this.lower_statements(&function.body, &mut scope, call_depth + 1)?;
            let Some(output) = function
                .outputs
                .iter()
                .find(|output| output.name == output_name)
            else {
                return Err(LowerError::InvalidFunction {
                    name: function_name.as_str().to_string(),
                    reason: format!("missing output `{output_name}`"),
                });
            };
            this.scoped_function_output_values(output, &scope)
        })
    }

    pub(in crate::lower) fn lower_array_like_values_in_mode(
        &mut self,
        expr: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
        mode: ValueMode,
    ) -> Result<Vec<Reg>, LowerError> {
        let old_mode = self.value_mode;
        self.value_mode = mode;
        let result = self.lower_array_like_values(expr, scope, call_depth);
        self.value_mode = old_mode;
        result
    }

    pub(in crate::lower) fn lower_array_constructor_values(
        &mut self,
        elements: &[rumoca_core::Expression],
        is_matrix: bool,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        if !is_matrix {
            let mut values = Vec::new();
            for element in elements {
                values.extend(self.lower_array_like_values(element, scope, call_depth)?);
            }
            return Ok(values);
        }
        if let Some(element) = single_high_rank_matrix_concat_element(elements, is_matrix) {
            return self.lower_array_like_values(element, scope, call_depth);
        }
        if elements.iter().all(|element| {
            matches!(
                element,
                rumoca_core::Expression::Array { .. } | rumoca_core::Expression::Tuple { .. }
            )
        }) {
            let mut values = Vec::new();
            for element in elements {
                values.extend(self.lower_array_like_values(element, scope, call_depth)?);
            }
            return Ok(values);
        }

        let span = elements
            .first()
            .and_then(rumoca_core::Expression::span)
            .or_else(|| self.active_source_context_span())
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: "missing source provenance for matrix constructor operand count"
                    .to_string(),
            })?;
        let mut operands = crate::lower_vec_with_capacity(
            elements.len(),
            "matrix constructor operand count",
            span,
        )?;
        for element in elements {
            operands.push(self.lower_array_operand(element, scope, call_depth)?);
        }
        lower_matrix_column_constructor_values(&operands)
    }

    pub(in crate::lower) fn lower_structural_field_values(
        &mut self,
        base: &rumoca_core::Expression,
        field: &str,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        match base {
            rumoca_core::Expression::Array { elements, .. }
            | rumoca_core::Expression::Tuple { elements, .. } => {
                let mut values = Vec::new();
                for element in elements {
                    let projected = projected_record_field_expression(element, field)?;
                    values.extend(self.lower_array_like_values(&projected, scope, call_depth)?);
                }
                Ok(Some(values))
            }
            rumoca_core::Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                ..
            } => {
                let projected = projected_record_field_expression(expr, field)?;
                self.lower_array_comprehension_values(
                    &projected,
                    indices,
                    filter.as_deref(),
                    scope,
                    call_depth,
                )
                .map(Some)
            }
            rumoca_core::Expression::FunctionCall {
                name, args, span, ..
            } => self.lower_user_function_output_field_values(
                name, args, field, *span, scope, call_depth,
            ),
            _ => Ok(None),
        }
    }

    pub(in crate::lower) fn lower_user_function_output_field_values(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        field: &str,
        span: rumoca_core::Span,
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        if call_depth >= MAX_FUNCTION_INLINE_DEPTH {
            return Ok(None);
        }
        let Some(function) = self.lookup_function(name).cloned() else {
            return Ok(None);
        };
        if function.external.is_some() || function.outputs.len() != 1 {
            return Ok(None);
        }
        self.ensure_pure_inline_function(name.as_str(), &function, span)?;

        self.with_local_lower_frame(|this| {
            let bindings =
                this.bind_function_inputs(name, &function.inputs, args, caller_scope, call_depth)?;
            let mut scope = bindings.scope;
            this.local_const_bindings.extend(bindings.const_bindings);
            this.initialize_function_output_scope(&function, &mut scope, call_depth)?;
            let output = &function.outputs[0];

            if let Some(values) = this.lower_projected_record_output_assignments(
                &function,
                output,
                field,
                &mut scope,
                call_depth + 1,
            )? {
                return Ok(Some(values));
            }

            let _returned = this.lower_statements(&function.body, &mut scope, call_depth + 1)?;
            this.scoped_record_output_field_values(output, field, &scope)
        })
    }

    pub(in crate::lower) fn lower_projected_record_output_assignments(
        &mut self,
        function: &rumoca_core::Function,
        output: &rumoca_core::FunctionParam,
        field: &str,
        scope: &mut Scope,
        call_depth: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        if output.type_class != Some(rumoca_core::ClassType::Record) {
            return Ok(None);
        }

        let mut lowered_projected_assignment = false;
        for statement in &function.body {
            let rumoca_core::Statement::Assignment { comp, value, .. } = statement else {
                match self.lower_statement_or_stop(statement, scope, call_depth)? {
                    true => break,
                    false => continue,
                }
            };

            let target = assignment_target(comp, &self.local_const_bindings)?;
            if target.indices.is_some() || target.base != output.name {
                match self.lower_statement_or_stop(statement, scope, call_depth)? {
                    true => break,
                    false => continue,
                }
            }

            if !self.can_project_record_field_expression(value) {
                match self.lower_statement_or_stop(statement, scope, call_depth)? {
                    true => break,
                    false => continue,
                }
            }

            let projected = projected_record_field_expression(value, field)?;
            let field_key = format!("{}.{}", output.name, field);
            let values = self.lower_array_like_values(&projected, scope, call_depth)?;
            self.bind_assignment_values_at(scope, &field_key, &values, output.span)?;
            lowered_projected_assignment = true;
        }

        if lowered_projected_assignment {
            return self.scoped_record_output_field_values(output, field, scope);
        }
        Ok(None)
    }

    fn lower_statement_or_stop(
        &mut self,
        statement: &rumoca_core::Statement,
        scope: &mut Scope,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        self.lower_statement(statement, scope, call_depth)
    }

    fn can_project_record_field_expression(&self, expr: &rumoca_core::Expression) -> bool {
        match expr {
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => {
                branches
                    .iter()
                    .all(|(_, branch)| self.can_project_record_field_expression(branch))
                    && self.can_project_record_field_expression(else_branch)
            }
            rumoca_core::Expression::Array { elements, .. }
            | rumoca_core::Expression::Tuple { elements, .. } => elements
                .iter()
                .all(|element| self.can_project_record_field_expression(element)),
            rumoca_core::Expression::VarRef { .. }
            | rumoca_core::Expression::FieldAccess { .. } => true,
            rumoca_core::Expression::FunctionCall {
                name,
                is_constructor,
                ..
            } => {
                self.is_record_constructor_call(name, *is_constructor)
                    || self.lookup_function(name).is_some_and(|function| {
                        matches!(
                            function.outputs.as_slice(),
                            [output]
                                if output.type_class == Some(rumoca_core::ClassType::Record)
                        )
                    })
            }
            _ => false,
        }
    }

    pub(in crate::lower) fn scoped_record_output_field_values(
        &self,
        output: &rumoca_core::FunctionParam,
        field: &str,
        scope: &Scope,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let field_key = format!("{}.{}", output.name, field);
        if let Some(values) = self.local_indexed_binding_values(&field_key) {
            return Ok(Some(values));
        }
        let field_path = generated_scope_key(&field_key);
        if let Some(values) = scoped_indexed_binding_values(scope, &field_path, output.span)? {
            return Ok(Some(values));
        }
        if let Some(reg) = scope.get(&field_path).copied() {
            return single_reg_vec(reg, "record output field value count", output.span).map(Some);
        }
        Ok(None)
    }

    pub(in crate::lower) fn lower_array_comprehension_values(
        &mut self,
        expr: &rumoca_core::Expression,
        indices: &[rumoca_core::ComprehensionIndex],
        filter: Option<&rumoca_core::Expression>,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let mut scope = scope.clone();
        let mut const_scope = self.local_const_bindings.clone();
        let mut values = Vec::new();
        let mut ctx = ArrayComprehensionLowerCtx {
            indices,
            filter,
            scope: &mut scope,
            const_scope: &mut const_scope,
            call_depth,
        };
        self.collect_array_comprehension_values(expr, 0, &mut ctx, &mut values)?;
        Ok(values)
    }

    pub(in crate::lower) fn collect_array_comprehension_values(
        &mut self,
        expr: &rumoca_core::Expression,
        depth: usize,
        ctx: &mut ArrayComprehensionLowerCtx<'_>,
        out: &mut Vec<Reg>,
    ) -> Result<(), LowerError> {
        if depth >= ctx.indices.len() {
            if let Some(filter_expr) = ctx.filter
                && self.eval_compile_time_expr(filter_expr, ctx.const_scope)? == 0.0
            {
                return Ok(());
            }
            append_reg_values(
                out,
                self.with_local_const_bindings(ctx.const_scope, |this| {
                    this.lower_array_like_values(expr, ctx.scope, ctx.call_depth)
                })?,
                "array comprehension value count",
                self.required_dynamic_selection_span(&[expr], "array comprehension value count")?,
            )?;
            return Ok(());
        }

        let iter = &ctx.indices[depth];
        let iter_values = self.eval_for_index_values(&iter.range, ctx.const_scope)?;
        for value in iter_values {
            let iter_span = self.required_dynamic_selection_span(
                &[&iter.range],
                "array comprehension iterator value",
            )?;
            let iter_reg = self.emit_const_at(value, iter_span)?;
            ctx.scope.push_frame();
            let result = (|| {
                ctx.scope
                    .insert_scoped(generated_scope_key(&iter.name), iter_reg)?;
                ctx.const_scope.insert(iter.name.clone(), value);
                let result = self.collect_array_comprehension_values(expr, depth + 1, ctx, out);
                ctx.const_scope.shift_remove(&iter.name);
                result
            })();
            ctx.scope.pop_frame();
            result?;
        }
        Ok(())
    }

    pub(in crate::lower) fn lower_record_constructor_values(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        owner_span: Option<rumoca_core::Span>,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let owner_span = owner_span
            .filter(|span| !span.is_dummy())
            .or_else(|| name.span());
        let Some(function) = self.lookup_function(name).cloned() else {
            let Some(span) = owner_span else {
                return Err(LowerError::UnspannedContractViolation {
                    reason: format!(
                        "missing source provenance for record constructor `{}` arguments",
                        name.as_str()
                    ),
                });
            };
            let mut values = array_vec_with_capacity(
                args.len(),
                "record constructor argument value count",
                span,
            )?;
            for arg in args {
                let arg_span = self.required_array_value_context_span(
                    arg,
                    "record constructor argument value count",
                )?;
                append_reg_values(
                    &mut values,
                    self.lower_array_like_values(arg, scope, call_depth)?,
                    "record constructor argument value count",
                    arg_span,
                )?;
            }
            return Ok(values);
        };

        let (named_args, positional_args) =
            function_calls::split_named_and_positional_call_args(name.as_str(), args)?;
        let mut positional_idx = 0usize;
        let mut values = array_vec_with_capacity(
            function.inputs.len(),
            "record constructor value count",
            function.span,
        )?;

        for input in &function.inputs {
            let arg_expr = named_args.get(input.name.as_str()).copied().or_else(|| {
                let positional = positional_args.get(positional_idx).copied();
                positional_idx += usize::from(positional.is_some());
                positional
            });

            if let Some(expr) = arg_expr {
                let span =
                    self.required_array_value_context_span(expr, "record constructor value count")?;
                append_reg_values(
                    &mut values,
                    self.lower_array_like_values(expr, scope, call_depth)?,
                    "record constructor value count",
                    span,
                )?;
            } else if let Some(default) = input.default.as_ref() {
                append_reg_values(
                    &mut values,
                    self.lower_array_like_values(default, scope, call_depth + 1)?,
                    "record constructor default value count",
                    default.span().unwrap_or(input.span),
                )?;
            } else {
                return Err(LowerError::MissingActualArgument {
                    function: name.as_str().to_string(),
                    what: "record constructor field",
                    input: input.name.clone(),
                    span: input.span,
                });
            }
        }

        Ok(values)
    }

    pub(in crate::lower) fn lower_if_array_like_values(
        &mut self,
        branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
        else_branch: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        if let Some(selected) = self.compile_time_if_array_selection(branches, else_branch) {
            return self.lower_array_like_values(selected, scope, call_depth);
        }
        let mut result = self.lower_array_like_values(else_branch, scope, call_depth)?;
        for (cond, value) in branches.iter().rev() {
            let cond_reg = self.lower_expr(cond, scope, call_depth)?;
            let branch_values = self.lower_array_like_values(value, scope, call_depth)?;
            if branch_values.len() != result.len() {
                // MLS §3.6 / §3.8: if-expression branches must agree on
                // shape. Keep compiled lowering strict instead of silently
                // truncating structured results.
                let span = value
                    .span()
                    .or_else(|| cond.span())
                    .or_else(|| else_branch.span())
                    .or_else(|| self.active_source_context_span())
                    .ok_or_else(|| LowerError::UnspannedContractViolation {
                        reason: "missing source provenance for if-expression branch width"
                            .to_string(),
                    })?;
                return Err(unsupported_at(
                    format!(
                        "if-expression branches with mismatched array-like widths are unsupported: branch width {}, accumulated else width {}, branch={} {}, else={} {}",
                        branch_values.len(),
                        result.len(),
                        expr_tag(value),
                        short_expr(value, 240),
                        expr_tag(else_branch),
                        short_expr(else_branch, 240)
                    ),
                    span,
                ));
            }
            let span =
                self.required_dynamic_selection_span(&[cond], "if-expression select value")?;
            let mut selected =
                array_vec_with_capacity(result.len(), "if-expression value count", span)?;
            for (if_true, if_false) in branch_values.into_iter().zip(result) {
                selected.push(self.emit_select_at(cond_reg, if_true, if_false, span)?);
            }
            result = selected;
        }
        Ok(result)
    }

    pub(in crate::lower) fn compile_time_if_array_selection<'expr>(
        &self,
        branches: &'expr [(rumoca_core::Expression, rumoca_core::Expression)],
        else_branch: &'expr rumoca_core::Expression,
    ) -> Option<&'expr rumoca_core::Expression> {
        for (cond, value) in branches {
            // Only fold the branch away when the predicate is a genuine
            // translation-time constant. A condition that reads a run-time value
            // (state, input, `time`, continuous algebraic, tunable parameter, …)
            // must lower to a run-time `Select`, not be collapsed to one branch
            // using the variable's `start` value. Otherwise an inline relation
            // such as `if a >= 0 then 1 else -1` (e.g. the body of a
            // `noEvent(...)`, whose relation carries no event memory) silently
            // bakes in the start-value branch and yields wrong dynamics.
            if !self.condition_is_compile_time_constant(cond) {
                return None;
            }
            let Ok(condition) = self.eval_compile_time_expr(cond, &self.local_const_bindings)
            else {
                return None;
            };
            if condition != 0.0 {
                return Some(value);
            }
        }
        Some(else_branch)
    }

    /// True when `expr` can be evaluated at translation time without reading any
    /// run-time value, so it is sound to use it to select an if-branch during
    /// lowering. Mirrors the expression forms `eval_compile_time_expr` accepts;
    /// anything else is treated conservatively as non-constant so lowering falls
    /// through to a run-time `Select`.
    fn condition_is_compile_time_constant(&self, expr: &rumoca_core::Expression) -> bool {
        match expr {
            rumoca_core::Expression::Literal { .. } => true,
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } => self.var_ref_is_compile_time_constant(name, subscripts),
            rumoca_core::Expression::Unary { rhs, .. } => {
                self.condition_is_compile_time_constant(rhs)
            }
            rumoca_core::Expression::Binary { lhs, rhs, .. } => {
                self.condition_is_compile_time_constant(lhs)
                    && self.condition_is_compile_time_constant(rhs)
            }
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => {
                branches.iter().all(|(cond, value)| {
                    self.condition_is_compile_time_constant(cond)
                        && self.condition_is_compile_time_constant(value)
                }) && self.condition_is_compile_time_constant(else_branch)
            }
            rumoca_core::Expression::BuiltinCall { function, args, .. } => {
                // `size`/`ndims` only read an array's structural shape, never its
                // run-time contents, so they stay translation-time constant
                // regardless of the argument.
                matches!(
                    function,
                    rumoca_core::BuiltinFunction::Size | rumoca_core::BuiltinFunction::Ndims
                ) || args
                    .iter()
                    .all(|arg| self.condition_is_compile_time_constant(arg))
            }
            rumoca_core::Expression::FunctionCall { args, .. } => args
                .iter()
                .all(|arg| self.condition_is_compile_time_constant(arg)),
            _ => false,
        }
    }

    fn var_ref_is_compile_time_constant(
        &self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) -> bool {
        let key = name.as_str();
        // `__pre__.X` is the previous-step memory of a discrete state, not a
        // constant. The DAE registers it as a non-tunable parameter (and seeds
        // its `start` value into `structural_bindings`) purely as a
        // representational artifact, but `snapshot_pre` rewrites the slot every
        // step. Folding an if-branch on it would bake in the state's start value
        // (e.g. `if pre(current_wp) == 1` collapsing to the wp==1 branch),
        // silently producing the wrong dynamics — exactly what scalar `lower_if`
        // already refuses to do. Treat it as run-time state so lowering keeps a
        // `Select`.
        if key.starts_with("__pre__.") {
            return false;
        }
        // Loop indices and projected function parameters bound to constants.
        if subscripts.is_empty() && self.local_const_bindings.contains_key(key) {
            return true;
        }
        // Structural quantities: sizes, enum-literal ordinals, structural params.
        if self.structural_bindings.contains_key(key) {
            return true;
        }
        // `time` always varies at run time.
        if key == "time" {
            return false;
        }
        let Some(variables) = self.dae_variables else {
            return false;
        };
        let var_name = name.var_name();
        if variables.constants.contains_key(var_name) {
            return true;
        }
        // Non-tunable parameters are fixed for the whole simulation; tunable
        // parameters and every continuous/discrete variable may change.
        if let Some(parameter) = variables.parameters.get(var_name) {
            return !parameter.is_tunable;
        }
        false
    }

    pub(in crate::lower) fn lower_indexed_binding_values_at(
        &mut self,
        key: &str,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        if let Some(values) = self.lower_direct_assignment_values_for_key(key, &Scope::new(), 0)? {
            return Ok(Some(values));
        }
        let entries = indexed_entries_for_key(&self.indexed_bindings, key);
        if entries.is_empty() {
            return Ok(None);
        }
        self.lower_indexed_entries_values(key, &entries, span)
    }

    pub(in crate::lower) fn lower_indexed_binding_values_for_reference(
        &mut self,
        reference: &rumoca_core::Reference,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let key = reference.as_str();
        if let Some(values) = self.lower_direct_assignment_values_for_key(key, &Scope::new(), 0)? {
            return Ok(Some(values));
        }
        let entries = indexed_entries_for_reference(&self.indexed_bindings, reference, span)?;
        if entries.is_empty() {
            return Ok(None);
        }
        self.lower_indexed_entries_values(key, &entries, span)
    }

    pub(in crate::lower) fn lower_indexed_binding_values_for_resolved_key(
        &mut self,
        display_key: &str,
        key: &ComponentReferenceKey,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let Some(entries) = self.indexed_bindings.get(key).cloned() else {
            return Ok(None);
        };
        self.lower_indexed_entries_values(display_key, &entries, span)
    }

    pub(in crate::lower) fn lower_indexed_entries_values(
        &mut self,
        key: &str,
        entries: &[IndexedBinding],
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let flat = sorted_flat_entries(entries);
        if flat.is_empty() {
            return Ok(None);
        }
        let mut values = array_vec_with_capacity(flat.len(), "indexed binding value count", span)?;
        for entry in flat {
            if entry.indices.is_empty() {
                values.push(self.emit_slot_load(entry.slot, span)?);
                continue;
            }
            let scalar_key = format_subscript_binding_key(key, &entry.indices);
            if let Some(mut direct_values) =
                self.lower_direct_assignment_values_for_key(&scalar_key, &Scope::new(), 0)?
                && let Some(value) = direct_values.pop()
            {
                values.push(value);
                continue;
            }
            values.push(self.emit_slot_load(entry.slot, span)?);
        }
        Ok(Some(values))
    }

    #[allow(clippy::excessive_nesting)]
    pub(in crate::lower) fn lower_if(
        &mut self,
        branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
        else_branch: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let mut runtime_branches = Vec::new();
        let mut selected_static_branch = None;
        for (cond, value) in branches {
            if compile_time_string_condition_call(cond) {
                let condition = self.eval_compile_time_expr(cond, &self.local_const_bindings)?;
                if condition != 0.0 {
                    selected_static_branch = Some(value);
                    break;
                }
                continue;
            }
            match lower_static_condition_truth(cond)? {
                Some(false) => {}
                Some(true) => {
                    selected_static_branch = Some(value);
                    break;
                }
                None => runtime_branches.push((cond, value)),
            }
        }

        let mut result = if let Some(value) = selected_static_branch {
            self.lower_expr(value, scope, call_depth)?
        } else {
            self.lower_expr(else_branch, scope, call_depth)?
        };
        for (cond, value) in runtime_branches.into_iter().rev() {
            let cond_reg = self.lower_expr(cond, scope, call_depth)?;
            let value_reg = self.lower_expr(value, scope, call_depth)?;
            result = self.emit_select_at(
                cond_reg,
                value_reg,
                result,
                self.required_dynamic_selection_span(&[cond], "if-expression scalar select")?,
            )?;
        }
        Ok(result)
    }
}

fn compile_time_string_condition_call(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::FunctionCall { name, .. }
            if matches!(
                name.as_str(),
                "Modelica.Utilities.Strings.isEqual" | "Strings.isEqual" | "isEqual"
            )
    )
}

fn first_expression_output_values(
    function: Option<&rumoca_core::Function>,
    values: Vec<Reg>,
    span: rumoca_core::Span,
) -> Result<Vec<Reg>, LowerError> {
    let Some(output) = function.and_then(|function| function.outputs.first()) else {
        return Ok(values);
    };
    let width = output.dims.iter().try_fold(1usize, |acc, dim| {
        let dim = usize::try_from(*dim).map_err(|_| {
            LowerError::contract_violation(
                format!(
                    "function output `{}` has invalid dimension `{dim}`",
                    output.name
                ),
                span,
            )
        })?;
        acc.checked_mul(dim).ok_or_else(|| {
            LowerError::contract_violation(
                format!(
                    "function output `{}` scalar count exceeds host index range",
                    output.name
                ),
                span,
            )
        })
    })?;
    if values.len() <= width {
        return Ok(values);
    }
    Ok(values.into_iter().take(width).collect())
}

fn function_output_dims(
    function_name: &rumoca_core::Reference,
    output: &rumoca_core::FunctionParam,
) -> Result<Vec<usize>, LowerError> {
    output.dims.iter().try_fold(
        array_vec_with_capacity(
            output.dims.len(),
            "function output dimension count",
            output.span,
        )?,
        |mut dims, dim| {
            dims.push(usize::try_from(*dim).map_err(|_| {
                LowerError::contract_violation(
                    format!(
                        "function `{function_name}` output `{}` has invalid dimension `{dim}`",
                        output.name
                    ),
                    output.span,
                )
            })?);
            Ok(dims)
        },
    )
}

fn missing_record_array_member_index(
    found: usize,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    found.checked_add(1).ok_or_else(|| {
        LowerError::contract_violation(
            "record-array member slice gap index overflows host index range",
            span,
        )
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn output_param_with_dims(
        name: &str,
        dims: Vec<i64>,
        span: rumoca_core::Span,
    ) -> rumoca_core::FunctionParam {
        rumoca_core::FunctionParam {
            def_id: None,
            name: name.to_string(),
            span,
            type_name: "Real".to_string(),
            type_class: None,
            dims,
            shape_expr: Vec::new(),
            default: None,
            description: None,
        }
    }

    #[test]
    fn projected_record_field_expression_preserves_value_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("project_record_field.mo"),
            6,
            12,
        );
        let value = rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("pin"),
            subscripts: Vec::new(),
            span,
        };

        let projected = projected_record_field_expression(&value, "v")
            .expect("spanned field projection should be generated");

        assert_eq!(projected.span(), Some(span));
    }

    #[test]
    fn projected_record_field_expression_rejects_unspanned_value() {
        let value = rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("pin"),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        };

        let err = projected_record_field_expression(&value, "v")
            .expect_err("source-derived field projection requires an owner span");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(
            err.reason().contains("projected record field expression"),
            "error should name the missing provenance context: {err}"
        );
    }

    #[test]
    fn function_output_dims_invalid_dummy_span_stays_unspanned() {
        let output = output_param_with_dims("y", vec![-1], rumoca_core::Span::DUMMY);
        let err = function_output_dims(&rumoca_core::Reference::new("f"), &output)
            .expect_err("negative output dimension must be rejected");

        assert!(
            matches!(err, LowerError::UnspannedContractViolation { .. }),
            "dummy output span should not be fabricated into a source span: {err:?}"
        );
        assert!(
            err.to_string().contains("invalid dimension `-1`"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn function_output_dims_invalid_real_span_is_preserved() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("function_output_dims.mo"),
            14,
            18,
        );
        let output = output_param_with_dims("y", vec![-1], span);
        let err = function_output_dims(&rumoca_core::Reference::new("f"), &output)
            .expect_err("negative output dimension must be rejected");

        assert!(
            matches!(err, LowerError::ContractViolation { span: err_span, .. } if err_span == span),
            "real output span should be preserved: {err:?}"
        );
    }
}
