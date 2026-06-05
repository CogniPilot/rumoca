use super::*;

impl<'a> LowerBuilder<'a> {
    pub(in crate::lower) fn lower_compile_time_indexed_local_value(
        &mut self,
        base: &rumoca_core::Expression,
        subscripts: &[rumoca_core::Subscript],
        scope: &Scope,
    ) -> Result<Option<Reg>, LowerError> {
        let Ok(base_key) = dynamic_binding_base_key(base) else {
            return Ok(None);
        };
        let Some(indices) = self.compile_time_subscript_indices(subscripts)? else {
            return Ok(None);
        };
        if indices.is_empty() {
            return Ok(None);
        }
        let indexed_key = format_subscript_binding_key(base_key.as_str(), &indices);
        let indexed_path = ComponentPath::from_flat_path(&indexed_key);
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
            return Ok(Some(self.emit_slot_load(slot)?));
        }
        if let Some(slot) = self.layout.binding(indexed_key.as_str()) {
            return Ok(Some(self.emit_slot_load(slot)?));
        }
        Ok(None)
    }

    pub(in crate::lower) fn lower_array_like_dynamic_index(
        &mut self,
        base: &rumoca_core::Expression,
        subscripts: &[rumoca_core::Subscript],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let Some(mut values) =
            self.lower_array_like_dynamic_selection_values(base, subscripts, scope, call_depth)?
        else {
            return Ok(None);
        };
        if values.len() != 1 {
            return Err(LowerError::Unsupported {
                reason: format!(
                    "array-like dynamic index selected {} values where a scalar was required",
                    values.len()
                ),
            });
        }
        Ok(values.pop())
    }

    pub(in crate::lower) fn lower_array_like_dynamic_selection_values(
        &mut self,
        base: &rumoca_core::Expression,
        subscripts: &[rumoca_core::Subscript],
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
        let dims = self.infer_expr_dims(base, scope);
        if dims.is_empty() || subscripts.len() > dims.len() {
            return Ok(None);
        }

        let values = self.lower_array_like_values(base, scope, call_depth)?;
        let index_tuples = one_based_index_tuples(&dims);
        if values.len() != index_tuples.len() {
            return Err(LowerError::Unsupported {
                reason: format!(
                    "array-like dynamic index shape mismatch: {} values for shape {:?}",
                    values.len(),
                    dims
                ),
            });
        }

        let selection_parts =
            self.lower_array_like_selection_parts(subscripts, &dims, scope, call_depth)?;
        let slice_choices = selection_parts
            .iter()
            .filter_map(|part| part.slice_indices.clone())
            .collect::<Vec<_>>();
        let output_tuples = if slice_choices.is_empty() {
            vec![Vec::new()]
        } else {
            index_choice_tuples(&slice_choices)
        };
        let mut selected_values = Vec::with_capacity(output_tuples.len());
        for output_tuple in output_tuples {
            selected_values.push(self.select_array_like_output_tuple(
                &selection_parts,
                &index_tuples,
                &values,
                &output_tuple,
            ));
        }
        Ok(Some(selected_values))
    }

    pub(in crate::lower) fn select_array_like_output_tuple(
        &mut self,
        selection_parts: &[ArraySelectionPart],
        index_tuples: &[Vec<usize>],
        values: &[Reg],
        output_tuple: &[usize],
    ) -> Reg {
        let mut merged = self.emit_const(0.0);
        for (indices, value) in index_tuples.iter().zip(values.iter().copied()) {
            if slice_indices_match(selection_parts, indices, output_tuple) {
                let cond = self.emit_non_slice_subscript_match(selection_parts, indices);
                merged = self.emit_select(cond, value, merged);
            }
        }
        merged
    }

    pub(in crate::lower) fn lower_array_like_selection_parts(
        &mut self,
        subscripts: &[rumoca_core::Subscript],
        dims: &[usize],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<ArraySelectionPart>, LowerError> {
        dims.iter()
            .copied()
            .enumerate()
            .map(|(dim_index, dim)| match subscripts.get(dim_index) {
                Some(rumoca_core::Subscript::Colon { .. }) => Ok(ArraySelectionPart {
                    selector: None,
                    slice_indices: Some((1..=dim).collect()),
                }),
                Some(rumoca_core::Subscript::Expr { expr, .. })
                    if matches!(expr.as_ref(), rumoca_core::Expression::Range { .. }) =>
                {
                    Ok(ArraySelectionPart {
                        selector: None,
                        slice_indices: Some(self.slice_subscript_indices(
                            &rumoca_core::Subscript::generated_expr(expr.clone()),
                            dim,
                            scope,
                        )?),
                    })
                }
                Some(subscript) => Ok(ArraySelectionPart {
                    selector: Some(
                        self.lower_structural_index_selector(subscript, scope, call_depth)?,
                    ),
                    slice_indices: None,
                }),
                None => Ok(ArraySelectionPart {
                    selector: None,
                    slice_indices: Some((1..=dim).collect()),
                }),
            })
            .collect()
    }

    pub(in crate::lower) fn emit_non_slice_subscript_match(
        &mut self,
        parts: &[ArraySelectionPart],
        indices: &[usize],
    ) -> Reg {
        let mut regs = Vec::new();
        let mut expected = Vec::new();
        for (part, index) in parts.iter().zip(indices.iter().copied()) {
            if let Some(selector) = part.selector {
                regs.push(selector);
                expected.push(index);
            }
        }
        if regs.is_empty() {
            return self.emit_const(1.0);
        }
        self.emit_subscript_match(&regs, &expected)
    }

    pub(in crate::lower) fn load_binding_keys(
        &mut self,
        keys: &[String],
    ) -> Result<Vec<Reg>, LowerError> {
        let mut values = Vec::with_capacity(keys.len());
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
            values.push(self.emit_slot_load(slot)?);
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
        scope: &Scope,
    ) -> Result<Option<Vec<String>>, LowerError> {
        let Some(shape) = self.layout.shape(base_name) else {
            return Ok(None);
        };
        if subscripts.is_empty() {
            return Ok(None);
        }

        let selections = self.slice_selections(subscripts, shape, scope)?;
        let mut keys = Vec::new();
        collect_slice_binding_keys(base_name, &selections, 0, &mut Vec::new(), &mut keys);
        Ok(Some(keys))
    }

    pub(in crate::lower) fn slice_binding_keys_or_scalar_fallback(
        &self,
        base_name: &str,
        subscripts: &[rumoca_core::Subscript],
        scope: &Scope,
    ) -> Result<Option<Vec<String>>, LowerError> {
        match self.slice_binding_keys(base_name, subscripts, scope) {
            Ok(keys) => Ok(keys),
            Err(_) if self.subscripts_select_scalar(base_name, subscripts) => Ok(None),
            Err(err) => Err(err),
        }
    }

    pub(in crate::lower) fn subscripts_select_scalar(
        &self,
        base_name: &str,
        subscripts: &[rumoca_core::Subscript],
    ) -> bool {
        let Some(shape) = self.layout.shape(base_name) else {
            return false;
        };
        subscripts.len() == shape.len() && subscripts.iter().all(is_scalar_selector_subscript)
    }

    pub(in crate::lower) fn slice_selections(
        &self,
        subscripts: &[rumoca_core::Subscript],
        shape: &[usize],
        scope: &Scope,
    ) -> Result<Vec<Vec<usize>>, LowerError> {
        if subscripts.len() > shape.len() {
            return Err(LowerError::Unsupported {
                reason: "array slice has more subscripts than dimensions".to_string(),
            });
        }

        let mut selections = Vec::with_capacity(shape.len());
        for (dim_index, subscript) in subscripts.iter().enumerate() {
            selections.push(self.slice_subscript_indices(subscript, shape[dim_index], scope)?);
        }
        for &dim in &shape[subscripts.len()..] {
            selections.push((1..=dim).collect());
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
            rumoca_core::Subscript::Index { value, .. } if *value > 0 => Ok(vec![*value as usize]),
            rumoca_core::Subscript::Expr { expr, .. } => self.slice_expr_indices(expr, dim, scope),
            rumoca_core::Subscript::Colon { .. } => Ok((1..=dim).collect()),
            _ => Err(LowerError::Unsupported {
                reason: "non-positive subscript is unsupported".to_string(),
            }),
        }
    }

    pub(in crate::lower) fn slice_expr_indices(
        &self,
        expr: &rumoca_core::Expression,
        dim: usize,
        scope: &Scope,
    ) -> Result<Vec<usize>, LowerError> {
        let _ = scope;
        let const_scope = IndexMap::new();
        let indices = match expr {
            rumoca_core::Expression::Range {
                start, step, end, ..
            } => {
                let values = self.eval_compile_time_range_values(
                    start,
                    step.as_deref(),
                    end,
                    &const_scope,
                    "array slice range",
                )?;
                values
                    .into_iter()
                    .map(|value| {
                        usize::try_from(value).map_err(|_| LowerError::Unsupported {
                            reason: "array slice range index must be positive".to_string(),
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?
            }
            _ => vec![self.eval_compile_time_positive_index(
                expr,
                &const_scope,
                "array slice index",
            )?],
        };
        validate_slice_indices(&indices, dim)?;
        Ok(indices)
    }

    pub(in crate::lower) fn lower_field_access_array_like_values(
        &mut self,
        base: &rumoca_core::Expression,
        field: &str,
        expr: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        if let Some(values) = self.lower_indexed_record_field_values(base, field, scope)? {
            return Ok(values);
        }
        if let Some(values) =
            self.lower_constructor_field_array_like_values(base, field, scope, call_depth)?
        {
            return Ok(values);
        }
        if let Ok(key) = field_access_binding_key(base, field)
            && let Some(values) = self.lower_indexed_binding_values(key.as_str())?
        {
            return Ok(values);
        }
        if let Some(values) = self.lower_structural_field_values(base, field, scope, call_depth)? {
            return Ok(values);
        }
        Ok(vec![self.lower_expr(expr, scope, call_depth)?])
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

    pub(in crate::lower) fn lower_indexed_record_field_values(
        &mut self,
        base: &rumoca_core::Expression,
        field: &str,
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
            return self
                .lower_dynamic_indexed_record_field_value(subscripts, &field_keys, scope)
                .map(|value| value.map(|reg| vec![reg]));
        }
        let selections = self.slice_selections(subscripts, &dims, scope)?;
        let mut keys = Vec::new();
        collect_indexed_record_field_keys(
            base_key.as_str(),
            field,
            &selections,
            0,
            &mut Vec::new(),
            &mut keys,
        );
        self.load_binding_keys(&keys).map(Some)
    }

    pub(in crate::lower) fn lower_dynamic_indexed_record_field_value(
        &mut self,
        subscripts: &[rumoca_core::Subscript],
        field_keys: &IndexMap<Vec<usize>, String>,
        scope: &Scope,
    ) -> Result<Option<Reg>, LowerError> {
        let selectors = subscripts
            .iter()
            .map(|subscript| self.lower_structural_index_selector(subscript, scope, 0))
            .collect::<Result<Vec<_>, _>>()?;
        let mut merged = self.emit_const(0.0);
        let mut matched = false;
        for (indices, key) in field_keys {
            if indices.len() != selectors.len() {
                continue;
            }
            let mut values = self.load_binding_keys(std::slice::from_ref(key))?;
            let value = values
                .pop()
                .ok_or_else(|| LowerError::MissingBinding { name: key.clone() })?;
            let cond = self.emit_subscript_match(&selectors, indices);
            merged = self.emit_select(cond, value, merged);
            matched = true;
        }
        Ok(matched.then_some(merged))
    }

    pub(in crate::lower) fn lower_range_array_like_values(
        &mut self,
        start: &rumoca_core::Expression,
        step: Option<&rumoca_core::Expression>,
        end: &rumoca_core::Expression,
    ) -> Result<Vec<Reg>, LowerError> {
        let Some(values) = lower_static_range_values(start, step, end)? else {
            return Err(LowerError::Unsupported {
                reason: "dynamic range array expansion requires known structural bounds"
                    .to_string(),
            });
        };
        Ok(values
            .into_iter()
            .map(|value| self.emit_const(value))
            .collect())
    }

    pub(in crate::lower) fn lower_unary_array_like_values(
        &mut self,
        op: &rumoca_core::OpUnary,
        rhs: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let values = self.lower_array_like_values(rhs, scope, call_depth)?;
        values
            .into_iter()
            .map(|value| self.lower_unary(op.clone(), value))
            .collect()
    }

    pub(in crate::lower) fn lower_user_function_call_array_values(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        if let Some(projection) = self.lookup_function_output_projection(name)
            && let Some(values) =
                self.lower_fft_projection_values(&projection, args, caller_scope, call_depth)?
        {
            return Ok(Some(values));
        }
        if let Some(projection) = self.lookup_function_output_projection(name)
            && projection.indices.is_empty()
            && projection.output_field.is_none()
        {
            return self
                .lower_user_function_call_named_output_values(
                    &projection.base_function_name,
                    &projection.output_name,
                    args,
                    caller_scope,
                    call_depth,
                )
                .map(Some);
        }
        self.lower_user_function_call_output_values(name, args, caller_scope, call_depth, Some(0))
    }

    pub(in crate::lower) fn scoped_function_output_values(
        &self,
        output: &rumoca_core::FunctionParam,
        scope: &Scope,
    ) -> Result<Vec<Reg>, LowerError> {
        if output.type_class == Some(rumoca_core::ClassType::Record)
            && let Some(values) = record_output_component_values(&output.name, scope)
        {
            return Ok(values);
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
        let rumoca_core::Expression::FunctionCall { name, args, .. } = base else {
            return Ok(None);
        };
        let Some(function) = self.lookup_function(name) else {
            return Ok(None);
        };
        if function.outputs.len() == 1 {
            return self.lower_single_output_function_index_values(
                name,
                args,
                subscripts,
                caller_scope,
                call_depth,
            );
        }
        let [subscript] = subscripts else {
            return Ok(None);
        };
        let output_number = lower_subscript_index(subscript)?;
        if function.outputs.len() <= 1
            || output_number == 0
            || output_number > function.outputs.len()
        {
            return Ok(None);
        }
        self.lower_user_function_call_output_values(
            name,
            args,
            caller_scope,
            call_depth,
            Some(output_number - 1),
        )
    }

    fn lower_single_output_function_index_values(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        subscripts: &[rumoca_core::Subscript],
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let Some(indices) = static_subscript_indices(subscripts)? else {
            return Ok(None);
        };
        let Some(function) = self.lookup_function(name) else {
            return Ok(None);
        };
        let Some(output) = function.outputs.first() else {
            return Ok(None);
        };
        let dims = output
            .dims
            .iter()
            .map(|dim| usize::try_from(*dim).ok().filter(|value| *value > 0))
            .collect::<Option<Vec<_>>>()
            .unwrap_or_default();
        let Some(flat_index) = flat_index_for_subscripts(&dims, &indices) else {
            return Ok(None);
        };
        let Some(values) = self.lower_user_function_call_output_values(
            name,
            args,
            caller_scope,
            call_depth,
            Some(0),
        )?
        else {
            return Ok(None);
        };
        Ok(values.get(flat_index).copied().map(|value| vec![value]))
    }

    pub(in crate::lower) fn lower_user_function_call_output_values(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        caller_scope: &Scope,
        call_depth: usize,
        output_index: Option<usize>,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        if call_depth >= MAX_FUNCTION_INLINE_DEPTH {
            return Ok(None);
        }
        let Some(function) = self.lookup_function(name).cloned() else {
            if let Some(closure) = self.lookup_function_closure(name).cloned() {
                let reg = self.lower_function_closure_call(
                    &closure,
                    args,
                    rumoca_core::Span::DUMMY,
                    caller_scope,
                    call_depth,
                )?;
                return Ok(Some(vec![reg]));
            }
            return Ok(None);
        };
        if function.external.is_some() || function.outputs.is_empty() {
            return Ok(None);
        }
        self.ensure_pure_inline_function(name.as_str(), &function)?;
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
        let mut values = Vec::new();
        for output in &function.outputs {
            values.extend(self.scoped_function_output_values(output, scope)?);
        }
        Ok(values)
    }

    pub(in crate::lower) fn lower_user_function_call_named_output_values(
        &mut self,
        function_name: &rumoca_core::VarName,
        output_name: &str,
        args: &[rumoca_core::Expression],
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
            return Err(LowerError::Unsupported {
                reason: format!(
                    "external function call `{}` cannot be inlined",
                    function_name.as_str()
                ),
            });
        }
        self.ensure_pure_inline_function(function_name.as_str(), &function)?;
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

        let operands = elements
            .iter()
            .map(|element| self.lower_array_operand(element, scope, call_depth))
            .collect::<Result<Vec<_>, _>>()?;
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
                    let projected = rumoca_core::Expression::FieldAccess {
                        base: Box::new(element.clone()),
                        field: field.to_string(),
                        span: rumoca_core::Span::DUMMY,
                    };
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
                let projected = rumoca_core::Expression::FieldAccess {
                    base: Box::new((**expr).clone()),
                    field: field.to_string(),
                    span: rumoca_core::Span::DUMMY,
                };
                self.lower_array_comprehension_values(
                    &projected,
                    indices,
                    filter.as_deref(),
                    scope,
                    call_depth,
                )
                .map(Some)
            }
            rumoca_core::Expression::FunctionCall { name, args, .. } => {
                self.lower_user_function_output_field_values(name, args, field, scope, call_depth)
            }
            _ => Ok(None),
        }
    }

    pub(in crate::lower) fn lower_user_function_output_field_values(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        field: &str,
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
        self.ensure_pure_inline_function(name.as_str(), &function)?;

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
            Ok(this.scoped_record_output_field_values(output, field, &scope))
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

            let projected = projected_record_field_expression(value, field);
            let field_key = format!("{}.{}", output.name, field);
            let values = self.lower_array_like_values(&projected, scope, call_depth)?;
            self.bind_assignment_values(scope, &field_key, &values);
            lowered_projected_assignment = true;
        }

        if lowered_projected_assignment {
            return Ok(self.scoped_record_output_field_values(output, field, scope));
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
    ) -> Option<Vec<Reg>> {
        let field_key = format!("{}.{}", output.name, field);
        if let Some(values) = self.local_indexed_binding_values(&field_key) {
            return Some(values);
        }
        let field_path = ComponentPath::from_flat_path(&field_key);
        if let Some(values) = scoped_indexed_binding_values(scope, &field_path) {
            return Some(values);
        }
        if let Some(reg) = scope.get(&field_path).copied() {
            return Some(vec![reg]);
        }
        None
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
        let mut const_scope = IndexMap::<String, f64>::new();
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
            out.extend(self.lower_array_like_values(expr, ctx.scope, ctx.call_depth)?);
            return Ok(());
        }

        let iter = &ctx.indices[depth];
        let iter_values = self.eval_for_index_values(&iter.range, ctx.const_scope)?;
        for value in iter_values {
            let iter_reg = self.emit_const(value);
            ctx.scope.push_frame();
            ctx.scope
                .insert_scoped(ComponentPath::from_flat_path(&iter.name), iter_reg);
            ctx.const_scope.insert(iter.name.clone(), value);
            let result = self.collect_array_comprehension_values(expr, depth + 1, ctx, out);
            ctx.const_scope.shift_remove(&iter.name);
            ctx.scope.pop_frame();
            result?;
        }
        Ok(())
    }

    pub(in crate::lower) fn lower_record_constructor_values(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let Some(function) = self.lookup_function(name).cloned() else {
            let mut values = Vec::new();
            for arg in args {
                values.extend(self.lower_array_like_values(arg, scope, call_depth)?);
            }
            return Ok(values);
        };

        let (named_args, positional_args) =
            function_calls::split_named_and_positional_call_args(name.as_str(), args)?;
        let mut positional_idx = 0usize;
        let mut values = Vec::new();

        for input in &function.inputs {
            let arg_expr = named_args.get(input.name.as_str()).copied().or_else(|| {
                let positional = positional_args.get(positional_idx).copied();
                positional_idx += usize::from(positional.is_some());
                positional
            });

            if let Some(expr) = arg_expr {
                values.extend(self.lower_array_like_values(expr, scope, call_depth)?);
            } else if let Some(default) = input.default.as_ref() {
                values.extend(self.lower_array_like_values(default, scope, call_depth + 1)?);
            } else {
                values.push(self.emit_const(0.0));
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
                return Err(LowerError::Unsupported {
                    // MLS §3.6 / §3.8: if-expression branches must agree on
                    // shape. Keep compiled lowering strict instead of silently
                    // truncating structured results.
                    reason: format!(
                        "if-expression branches with mismatched array-like widths are unsupported: branch width {}, accumulated else width {}, branch={} {}, else={} {}",
                        branch_values.len(),
                        result.len(),
                        expr_tag(value),
                        short_expr(value, 240),
                        expr_tag(else_branch),
                        short_expr(else_branch, 240)
                    ),
                });
            }
            result = branch_values
                .into_iter()
                .zip(result)
                .map(|(if_true, if_false)| self.emit_select(cond_reg, if_true, if_false))
                .collect();
        }
        Ok(result)
    }

    pub(in crate::lower) fn compile_time_if_array_selection<'expr>(
        &self,
        branches: &'expr [(rumoca_core::Expression, rumoca_core::Expression)],
        else_branch: &'expr rumoca_core::Expression,
    ) -> Option<&'expr rumoca_core::Expression> {
        for (cond, value) in branches {
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

    pub(in crate::lower) fn lower_indexed_binding_values(
        &mut self,
        key: &str,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        if let Some(values) = self.lower_direct_assignment_values_for_key(key, &Scope::new(), 0)? {
            return Ok(Some(values));
        }
        let entries = indexed_entries_for_key(&self.indexed_bindings, key);
        if entries.is_empty() {
            return Ok(None);
        }
        let flat = sorted_flat_entries(&entries);
        if flat.is_empty() {
            return Ok(None);
        }
        let mut values = Vec::with_capacity(flat.len());
        for entry in flat {
            let scalar_key = format_subscript_binding_key(key, &entry.indices);
            if let Some(mut direct_values) =
                self.lower_direct_assignment_values_for_key(&scalar_key, &Scope::new(), 0)?
                && let Some(value) = direct_values.pop()
            {
                values.push(value);
                continue;
            }
            values.push(self.emit_slot_load(entry.slot)?);
        }
        Ok(Some(values))
    }

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
            result = self.emit_select(cond_reg, value_reg, result);
        }
        Ok(result)
    }
}
