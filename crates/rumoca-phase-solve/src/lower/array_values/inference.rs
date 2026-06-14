use super::*;

impl<'a> LowerBuilder<'a> {
    pub(in crate::lower) fn infer_expr_dims(
        &self,
        expr: &rumoca_core::Expression,
        scope: &Scope,
    ) -> Vec<usize> {
        match expr {
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => self.infer_unsubscripted_var_ref_dims(name, scope),
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } => self
                .infer_slice_dims(name.as_str(), subscripts, scope)
                .unwrap_or_default()
                .unwrap_or_default(),
            rumoca_core::Expression::FieldAccess { base, field, .. } => {
                field_access_binding_key(base, field)
                    .ok()
                    .and_then(|key| {
                        self.local_binding_dims
                            .get(&key)
                            .map(|dims| {
                                dims.iter()
                                    .filter_map(|dim| (*dim > 0).then_some(*dim as usize))
                                    .collect::<Vec<_>>()
                            })
                            .or_else(|| self.layout.shape(&key).map(<[usize]>::to_vec))
                            .or_else(|| {
                                let dims = infer_indexed_dims(&indexed_entries_for_key(
                                    &self.indexed_bindings,
                                    &key,
                                ));
                                (!dims.is_empty()).then_some(dims)
                            })
                    })
                    .or_else(|| self.infer_function_call_field_dims(base, field))
                    .unwrap_or_else(|| self.infer_expr_dims(base, scope))
            }
            rumoca_core::Expression::BuiltinCall { function, args, .. } => {
                self.infer_builtin_call_dims(function, args, scope)
            }
            rumoca_core::Expression::FunctionCall { name, args, .. }
                if is_synchronous_array_like_intrinsic(name.as_str()) =>
            {
                args.first()
                    .map(|arg| self.infer_expr_dims(arg, scope))
                    .unwrap_or_default()
            }
            rumoca_core::Expression::FunctionCall { name, args, .. } => {
                // Returning an empty Vec (scalar) when the function is unknown or has no
                // declared first output is intentional: inference is best-effort.
                // A `debug_assert!` gates the regression-prone case where the function IS
                // known but its first output has non-empty dims — those must not silently
                // collapse to scalar.
                let inferred = self.infer_function_call_output_dims(name, args, scope);
                debug_assert!(
                    inferred.is_some()
                        || self
                            .lookup_function(name)
                            .and_then(|function| function.outputs.first())
                            .is_none_or(|output| {
                                dims_i64_to_usize(&output.dims).is_empty() == output.dims.is_empty()
                            }),
                    "function `{}` has non-scalar declared output dims but inference \
                     would return scalar — check dims_i64_to_usize is not lossy",
                    name.as_str()
                );
                inferred.unwrap_or_default()
            }
            rumoca_core::Expression::Array {
                elements,
                is_matrix,
                ..
            } => infer_array_literal_dims_with(elements, *is_matrix, |element| {
                self.infer_expr_dims(element, scope)
            }),
            rumoca_core::Expression::Tuple { elements, .. } => {
                self.infer_tuple_flat_dims(elements, scope)
            }
            rumoca_core::Expression::Range {
                start, step, end, ..
            } => lower_static_range_values(start, step.as_deref(), end)
                .expect("static range dimension inference must only receive scalar bounds")
                .map_or_else(Vec::new, |values| vector_dims(values.len())),
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => {
                if let Some(selected) = self.compile_time_if_array_selection(branches, else_branch)
                {
                    return self.infer_expr_dims(selected, scope);
                }
                let else_dims = self.infer_expr_dims(else_branch, scope);
                if !else_dims.is_empty() {
                    return else_dims;
                }
                branches
                    .iter()
                    .map(|(_, value)| self.infer_expr_dims(value, scope))
                    .find(|dims| !dims.is_empty())
                    .unwrap_or_default()
            }
            rumoca_core::Expression::Unary { rhs, .. } => self.infer_expr_dims(rhs, scope),
            rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
                self.infer_binary_dims(op, lhs, rhs, scope)
            }
            rumoca_core::Expression::Index {
                base, subscripts, ..
            } => self.infer_index_expr_dims(base, subscripts, scope),
            rumoca_core::Expression::ArrayComprehension { .. }
            | rumoca_core::Expression::Literal { value: _, .. }
            | rumoca_core::Expression::Empty { .. } => Vec::new(),
        }
    }

    fn infer_function_call_output_dims(
        &self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        scope: &Scope,
    ) -> Option<Vec<usize>> {
        let function = self.lookup_function(name)?;
        let output = function.outputs.first()?;
        let declared = dims_i64_to_usize(&output.dims);
        if !declared.is_empty() {
            return Some(declared);
        }
        if output.dims.is_empty() {
            return Some(Vec::new());
        }
        if output.shape_expr.len() != output.dims.len() {
            return None;
        }
        output
            .shape_expr
            .iter()
            .map(|subscript| {
                self.infer_function_shape_dim(subscript, &function.inputs, args, scope)
            })
            .collect::<Option<Vec<_>>>()
            .filter(|dims| !dims.is_empty())
    }

    fn infer_function_shape_dim(
        &self,
        subscript: &rumoca_core::Subscript,
        inputs: &[rumoca_core::FunctionParam],
        args: &[rumoca_core::Expression],
        scope: &Scope,
    ) -> Option<usize> {
        match subscript {
            rumoca_core::Subscript::Index { value, .. } if *value > 0 => Some(*value as usize),
            rumoca_core::Subscript::Expr { expr, .. } => {
                self.infer_function_shape_expr_dim(expr, inputs, args, scope)
            }
            rumoca_core::Subscript::Colon { .. } | rumoca_core::Subscript::Index { .. } => None,
        }
    }

    fn infer_function_shape_expr_dim(
        &self,
        expr: &rumoca_core::Expression,
        inputs: &[rumoca_core::FunctionParam],
        args: &[rumoca_core::Expression],
        scope: &Scope,
    ) -> Option<usize> {
        match expr {
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(value),
                ..
            } if *value > 0 => Some(*value as usize),
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Size,
                args: size_args,
                ..
            } => self.infer_size_shape_expr_dim(size_args, inputs, args, scope),
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => {
                let input_idx = inputs
                    .iter()
                    .position(|input| input.name == name.as_str())?;
                let actual = function_actual_arg_for_input(name.as_str(), input_idx, args)?;
                self.infer_positive_integer_expr(actual)
            }
            _ => None,
        }
    }

    fn infer_positive_integer_expr(&self, expr: &rumoca_core::Expression) -> Option<usize> {
        if let Some(value) = integer_literal_usize(expr) {
            return Some(value);
        }
        if let Ok(value) = compile_time_index_expr(expr, &self.structural_bindings) {
            return Some(value);
        }
        let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = expr
        else {
            return None;
        };
        if !subscripts.is_empty() {
            return None;
        }
        let start = self.variable_starts?.get(name.as_str())?;
        integer_literal_usize(start)
            .or_else(|| compile_time_index_expr(start, &self.structural_bindings).ok())
    }

    fn infer_size_shape_expr_dim(
        &self,
        size_args: &[rumoca_core::Expression],
        inputs: &[rumoca_core::FunctionParam],
        args: &[rumoca_core::Expression],
        scope: &Scope,
    ) -> Option<usize> {
        let rumoca_core::Expression::VarRef {
            name: input_name,
            subscripts,
            ..
        } = size_args.first()?
        else {
            return None;
        };
        if !subscripts.is_empty() {
            return None;
        }
        let dim = size_args
            .get(1)
            .and_then(integer_literal_usize)
            .unwrap_or(1);
        let input_idx = inputs
            .iter()
            .position(|input| input.name == input_name.as_str())?;
        let actual = function_actual_arg_for_input(input_name.as_str(), input_idx, args)?;
        self.infer_expr_dims(actual, scope)
            .get(dim.checked_sub(1)?)
            .copied()
    }

    fn infer_tuple_flat_dims(
        &self,
        elements: &[rumoca_core::Expression],
        scope: &Scope,
    ) -> Vec<usize> {
        let scalar_count = elements
            .iter()
            .map(|element| {
                let dims = self.infer_expr_dims(element, scope);
                shape_size(&dims)
            })
            .sum();
        vector_dims(scalar_count)
    }

    fn infer_function_call_field_dims(
        &self,
        base: &rumoca_core::Expression,
        field: &str,
    ) -> Option<Vec<usize>> {
        let rumoca_core::Expression::FunctionCall { name, .. } = base else {
            return None;
        };
        let function = self.lookup_function(name)?;
        let [output] = function.outputs.as_slice() else {
            return None;
        };
        for statement in &function.body {
            if let Some(dims) = self.infer_assigned_record_field_dims(statement, output, field) {
                return Some(dims);
            }
        }
        None
    }

    fn infer_assigned_record_field_dims(
        &self,
        statement: &rumoca_core::Statement,
        output: &rumoca_core::FunctionParam,
        field: &str,
    ) -> Option<Vec<usize>> {
        let rumoca_core::Statement::Assignment { comp, value, .. } = statement else {
            return None;
        };
        match comp.parts.as_slice() {
            [target] if target.ident == output.name && target.subs.is_empty() => {
                self.infer_constructor_field_dims(value, field)
            }
            [target, selected]
                if target.ident == output.name
                    && selected.ident == field
                    && target.subs.is_empty()
                    && selected.subs.is_empty() =>
            {
                Some(self.infer_expr_dims(value, &Scope::new()))
            }
            _ => None,
        }
    }

    fn infer_constructor_field_dims(
        &self,
        expr: &rumoca_core::Expression,
        field: &str,
    ) -> Option<Vec<usize>> {
        match expr {
            rumoca_core::Expression::FunctionCall {
                name,
                args,
                is_constructor,
                ..
            } => {
                if !self.is_record_constructor_call(name, *is_constructor) {
                    return None;
                }
                self.lookup_function(name)
                    .and_then(|function| {
                        function
                            .inputs
                            .iter()
                            .find(|input| input.name == field)
                            .map(|input| dims_i64_to_usize(&input.dims))
                    })
                    .or_else(|| self.infer_named_constructor_field_dims(args, field))
            }
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => {
                let else_dims = self.infer_constructor_field_dims(else_branch, field);
                if else_dims.as_ref().is_some_and(|dims| !dims.is_empty()) {
                    return else_dims;
                }
                branches
                    .iter()
                    .filter_map(|(_, value)| self.infer_constructor_field_dims(value, field))
                    .find(|dims| !dims.is_empty())
                    .or(else_dims)
            }
            _ => None,
        }
    }

    fn infer_named_constructor_field_dims(
        &self,
        args: &[rumoca_core::Expression],
        field: &str,
    ) -> Option<Vec<usize>> {
        args.iter()
            .filter_map(super::function_calls::decode_named_function_arg)
            .find_map(|(name, value)| {
                (name == field).then(|| self.infer_expr_dims(value, &Scope::new()))
            })
    }

    fn infer_unsubscripted_var_ref_dims(
        &self,
        name: &rumoca_core::Reference,
        scope: &Scope,
    ) -> Vec<usize> {
        let name_path = ComponentPath::from_reference(name);
        let name = name.as_str();
        if let Some(dims) = self.local_binding_dims.get(name)
            && dims.iter().all(|dim| *dim > 0)
        {
            return dims.iter().map(|dim| *dim as usize).collect();
        }
        if scope.contains_key(&name_path) {
            return Vec::new();
        }
        if let Some(shape) = self.layout.shape(name) {
            return shape.to_vec();
        }
        if let Some(values) = scoped_indexed_binding_values(scope, &name_path) {
            return vector_dims(values.len());
        }
        if let Some(values) = self.local_indexed_binding_values(name) {
            return vector_dims(values.len());
        }
        let indexed_dims =
            infer_indexed_dims(&indexed_entries_for_key(&self.indexed_bindings, name));
        if !indexed_dims.is_empty() {
            return indexed_dims;
        }
        self.local_binding_dims
            .get(name)
            .map(|dims| {
                dims.iter()
                    .filter_map(|dim| (*dim >= 0).then_some(*dim as usize))
                    .collect()
            })
            .unwrap_or_default()
    }

    fn infer_builtin_call_dims(
        &self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
        scope: &Scope,
    ) -> Vec<usize> {
        if let Some(dims) = self.infer_linear_algebra_builtin_dims(function, args, scope) {
            return dims;
        }
        match function {
            rumoca_core::BuiltinFunction::Der
            | rumoca_core::BuiltinFunction::Pre
            | rumoca_core::BuiltinFunction::Sample
            | rumoca_core::BuiltinFunction::NoEvent => args
                .first()
                .map(|arg| self.infer_expr_dims(arg, scope))
                .unwrap_or_default(),
            rumoca_core::BuiltinFunction::Smooth => args
                .get(1)
                .map(|arg| self.infer_expr_dims(arg, scope))
                .unwrap_or_default(),
            function if unary_array_builtin_op(function).is_some() => args
                .first()
                .map(|arg| self.infer_expr_dims(arg, scope))
                .unwrap_or_default(),
            rumoca_core::BuiltinFunction::Fill => {
                self.infer_fill_dims(&args[1..]).unwrap_or_default()
            }
            rumoca_core::BuiltinFunction::Zeros | rumoca_core::BuiltinFunction::Ones => {
                self.infer_fill_dims(args).unwrap_or_default()
            }
            rumoca_core::BuiltinFunction::Linspace => args
                .get(2)
                .and_then(|dim| {
                    self.eval_compile_time_positive_index(
                        dim,
                        &self.local_const_bindings,
                        "linspace count",
                    )
                    .ok()
                })
                .map(vector_dims)
                .unwrap_or_default(),
            rumoca_core::BuiltinFunction::Cat => {
                self.infer_cat_dims(args, scope).unwrap_or_default()
            }
            _ => Vec::new(),
        }
    }

    fn infer_linear_algebra_builtin_dims(
        &self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
        scope: &Scope,
    ) -> Option<Vec<usize>> {
        match function {
            rumoca_core::BuiltinFunction::Identity => Some(self.infer_identity_dims(args)),
            rumoca_core::BuiltinFunction::Diagonal => Some(self.infer_diagonal_dims(args, scope)),
            rumoca_core::BuiltinFunction::Transpose => Some(self.infer_transpose_dims(args, scope)),
            rumoca_core::BuiltinFunction::Scalar => Some(Vec::new()),
            rumoca_core::BuiltinFunction::Vector => Some(self.infer_vector_dims(args, scope)),
            rumoca_core::BuiltinFunction::Matrix => Some(self.infer_matrix_dims(args, scope)),
            rumoca_core::BuiltinFunction::Cross => Some(vec![3]),
            rumoca_core::BuiltinFunction::Skew => Some(vec![3, 3]),
            rumoca_core::BuiltinFunction::OuterProduct => {
                Some(self.infer_outer_product_dims(args, scope))
            }
            rumoca_core::BuiltinFunction::Symmetric => Some(self.infer_symmetric_dims(args, scope)),
            _ => None,
        }
    }

    fn infer_identity_dims(&self, args: &[rumoca_core::Expression]) -> Vec<usize> {
        args.first()
            .and_then(|dim| {
                self.eval_compile_time_positive_index(
                    dim,
                    &self.local_const_bindings,
                    "identity dimension",
                )
                .ok()
            })
            .map(|dim| vec![dim, dim])
            .unwrap_or_default()
    }

    fn infer_diagonal_dims(&self, args: &[rumoca_core::Expression], scope: &Scope) -> Vec<usize> {
        args.first()
            .map(|arg| self.infer_expr_dims(arg, scope))
            .and_then(|dims| match dims.as_slice() {
                [dim] => Some(vec![*dim, *dim]),
                _ => None,
            })
            .unwrap_or_default()
    }

    fn infer_transpose_dims(&self, args: &[rumoca_core::Expression], scope: &Scope) -> Vec<usize> {
        args.first()
            .map(|arg| self.infer_expr_dims(arg, scope))
            .map(|dims| match dims.as_slice() {
                [rows, cols] => vec![*cols, *rows],
                _ => dims,
            })
            .unwrap_or_default()
    }

    fn infer_vector_dims(&self, args: &[rumoca_core::Expression], scope: &Scope) -> Vec<usize> {
        args.first()
            .map(|arg| {
                let dims = self.infer_expr_dims(arg, scope);
                vector_dims(shape_size(&dims))
            })
            .unwrap_or_default()
    }

    fn infer_matrix_dims(&self, args: &[rumoca_core::Expression], scope: &Scope) -> Vec<usize> {
        args.first()
            .map(|arg| match self.infer_expr_dims(arg, scope).as_slice() {
                [] => vec![1, 1],
                [dim] => vec![*dim, 1],
                [rows, cols] => vec![*rows, *cols],
                _ => Vec::new(),
            })
            .unwrap_or_default()
    }

    fn infer_outer_product_dims(
        &self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
    ) -> Vec<usize> {
        let lhs_dims = args
            .first()
            .map(|arg| self.infer_expr_dims(arg, scope))
            .unwrap_or_default();
        let rhs_dims = args
            .get(1)
            .map(|arg| self.infer_expr_dims(arg, scope))
            .unwrap_or_default();
        match (lhs_dims.as_slice(), rhs_dims.as_slice()) {
            ([lhs], [rhs]) => vec![*lhs, *rhs],
            _ => Vec::new(),
        }
    }

    fn infer_symmetric_dims(&self, args: &[rumoca_core::Expression], scope: &Scope) -> Vec<usize> {
        args.first()
            .map(|arg| self.infer_expr_dims(arg, scope))
            .filter(|dims| matches!(dims.as_slice(), [rows, cols] if rows == cols))
            .unwrap_or_default()
    }

    fn infer_slice_dims(
        &self,
        base_name: &str,
        subscripts: &[rumoca_core::Subscript],
        scope: &Scope,
    ) -> Result<Option<Vec<usize>>, LowerError> {
        let Some(shape) = self.layout.shape(base_name) else {
            return Ok(None);
        };
        let selections = self.slice_selections(subscripts, shape, scope)?;
        let dims = selections
            .into_iter()
            .filter_map(|indices| (indices.len() > 1).then_some(indices.len()))
            .collect::<Vec<_>>();
        Ok(Some(dims))
    }

    fn infer_index_expr_dims(
        &self,
        base: &rumoca_core::Expression,
        subscripts: &[rumoca_core::Subscript],
        scope: &Scope,
    ) -> Vec<usize> {
        if let Ok(base_name) = dynamic_binding_base_key(base)
            && let Ok(Some(dims)) = self.infer_slice_dims(base_name.as_str(), subscripts, scope)
        {
            return dims;
        }
        let base_dims = self.infer_expr_dims(base, scope);
        if subscripts.len() > base_dims.len() {
            return Vec::new();
        }
        inferred_subscripted_dims(&base_dims, subscripts, self, scope).unwrap_or_default()
    }

    pub(super) fn eval_compile_time_positive_index(
        &self,
        expr: &rumoca_core::Expression,
        const_scope: &IndexMap<String, f64>,
        context: &str,
    ) -> Result<usize, LowerError> {
        let value = self.eval_compile_time_int(expr, const_scope, context)?;
        usize::try_from(value)
            .ok()
            .filter(|value| *value > 0)
            .ok_or_else(|| LowerError::Unsupported {
                reason: format!("{context} must be positive"),
            })
    }

    pub(in crate::lower) fn compile_time_subscript_indices(
        &self,
        subscripts: &[rumoca_core::Subscript],
    ) -> Result<Option<Vec<usize>>, LowerError> {
        if subscripts.is_empty() {
            return Ok(Some(Vec::new()));
        }

        let const_scope = &self.local_const_bindings;
        let mut indices = Vec::with_capacity(subscripts.len());
        for subscript in subscripts {
            match subscript {
                rumoca_core::Subscript::Index { value, .. } if *value > 0 => {
                    indices.push(*value as usize)
                }
                rumoca_core::Subscript::Expr { expr, .. }
                    if !matches!(expr.as_ref(), rumoca_core::Expression::Range { .. })
                        && self.expr_can_eval_compile_time(expr, const_scope) =>
                {
                    indices.push(self.eval_compile_time_positive_index(
                        expr,
                        const_scope,
                        "array subscript",
                    )?);
                }
                rumoca_core::Subscript::Expr { .. } | rumoca_core::Subscript::Colon { .. } => {
                    return Ok(None);
                }
                _ => {
                    return Err(LowerError::Unsupported {
                        reason: "non-positive subscript is unsupported".to_string(),
                    });
                }
            }
        }
        Ok(Some(indices))
    }

    fn expr_can_eval_compile_time(
        &self,
        expr: &rumoca_core::Expression,
        const_scope: &IndexMap<String, f64>,
    ) -> bool {
        match expr {
            rumoca_core::Expression::Literal { value: _, .. } => true,
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } => {
                if subscripts.is_empty() && const_scope.contains_key(name.as_str()) {
                    return true;
                }
                compile_time_var_key(name, subscripts, const_scope)
                    .ok()
                    .is_some_and(|key| {
                        self.structural_bindings.contains_key(key.as_str())
                            || matches!(
                                self.layout.binding(key.as_str()),
                                Some(ScalarSlot::Constant(_))
                            )
                    })
            }
            rumoca_core::Expression::Unary { rhs, .. } => {
                self.expr_can_eval_compile_time(rhs, const_scope)
            }
            rumoca_core::Expression::Binary { lhs, rhs, .. } => {
                self.expr_can_eval_compile_time(lhs, const_scope)
                    && self.expr_can_eval_compile_time(rhs, const_scope)
            }
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => {
                branches.iter().all(|(cond, value)| {
                    self.expr_can_eval_compile_time(cond, const_scope)
                        && self.expr_can_eval_compile_time(value, const_scope)
                }) && self.expr_can_eval_compile_time(else_branch, const_scope)
            }
            rumoca_core::Expression::BuiltinCall { function, args, .. } => match function {
                rumoca_core::BuiltinFunction::Size => args
                    .get(1)
                    .is_none_or(|dim| self.expr_can_eval_compile_time(dim, const_scope)),
                rumoca_core::BuiltinFunction::Abs
                | rumoca_core::BuiltinFunction::Sign
                | rumoca_core::BuiltinFunction::Sqrt
                | rumoca_core::BuiltinFunction::Floor
                | rumoca_core::BuiltinFunction::Integer
                | rumoca_core::BuiltinFunction::Ceil
                | rumoca_core::BuiltinFunction::Min
                | rumoca_core::BuiltinFunction::Max => args
                    .iter()
                    .all(|arg| self.expr_can_eval_compile_time(arg, const_scope)),
                _ => false,
            },
            rumoca_core::Expression::FunctionCall { .. }
            | rumoca_core::Expression::ArrayComprehension { .. }
            | rumoca_core::Expression::Tuple { .. }
            | rumoca_core::Expression::FieldAccess { .. }
            | rumoca_core::Expression::Index { .. }
            | rumoca_core::Expression::Range { .. }
            | rumoca_core::Expression::Array { .. }
            | rumoca_core::Expression::Empty { .. } => false,
        }
    }

    pub(super) fn eval_compile_time_range_values(
        &self,
        start: &rumoca_core::Expression,
        step: Option<&rumoca_core::Expression>,
        end: &rumoca_core::Expression,
        const_scope: &IndexMap<String, f64>,
        context: &str,
    ) -> Result<Vec<i64>, LowerError> {
        let start = self.eval_compile_time_int(start, const_scope, context)?;
        let end = self.eval_compile_time_int(end, const_scope, context)?;
        let step = match step {
            Some(step) => self.eval_compile_time_int(step, const_scope, context)?,
            None => 1,
        };
        if step == 0 {
            return Err(LowerError::Unsupported {
                reason: format!("{context} step must be non-zero"),
            });
        }

        let mut values = Vec::new();
        let mut value = start;
        for _ in 0..MAX_STATIC_RANGE_VALUES {
            if (step > 0 && value > end) || (step < 0 && value < end) {
                break;
            }
            values.push(value);
            value = value
                .checked_add(step)
                .ok_or_else(|| LowerError::Unsupported {
                    reason: format!("{context} overflows i64"),
                })?;
        }
        if (step > 0 && value <= end) || (step < 0 && value >= end) {
            return Err(LowerError::Unsupported {
                reason: format!(
                    "{context} exceeds maximum lowered range length {MAX_STATIC_RANGE_VALUES}"
                ),
            });
        }
        Ok(values)
    }

    fn infer_cat_dims(
        &self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
    ) -> Result<Vec<usize>, LowerError> {
        if args.len() <= 1 {
            return Ok(Vec::new());
        }
        let dim = self.eval_compile_time_positive_index(
            &args[0],
            &self.local_const_bindings,
            "cat dimension",
        )?;
        let operands = args
            .iter()
            .skip(1)
            .map(|arg| ArrayOperand {
                values: Vec::new(),
                dims: self.infer_expr_dims(arg, scope),
            })
            .collect::<Vec<_>>();
        cat_array_dims(dim, &operands)
    }

    fn infer_fill_dims(&self, dims: &[rumoca_core::Expression]) -> Result<Vec<usize>, LowerError> {
        let const_scope = &self.local_const_bindings;
        let mut values = Vec::with_capacity(dims.len());
        for dim in dims {
            let dim_value = self.eval_compile_time_int(dim, const_scope, "fill dimension")?;
            if dim_value < 0 {
                return Err(LowerError::Unsupported {
                    reason: "fill dimension must be non-negative".to_string(),
                });
            }
            values.push(dim_value as usize);
        }
        Ok(values)
    }

    fn infer_binary_dims(
        &self,
        op: &rumoca_core::OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        scope: &Scope,
    ) -> Vec<usize> {
        use rumoca_core::OpBinary as Op;

        let lhs_dims = self.infer_expr_dims(lhs, scope);
        let rhs_dims = self.infer_expr_dims(rhs, scope);
        match op {
            Op::Mul => multiplication_dims(&lhs_dims, &rhs_dims).unwrap_or_default(),
            Op::Add
            | Op::AddElem
            | Op::Sub
            | Op::SubElem
            | Op::MulElem
            | Op::DivElem
            | Op::ExpElem => broadcast_shape(&lhs_dims, &rhs_dims).unwrap_or_default(),
            Op::Div if rhs_dims.is_empty() => lhs_dims,
            _ => Vec::new(),
        }
    }
}

fn integer_literal_usize(expr: &rumoca_core::Expression) -> Option<usize> {
    let rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        ..
    } = expr
    else {
        return None;
    };
    (*value > 0).then_some(*value as usize)
}

fn function_actual_arg_for_input<'a>(
    input_name: &str,
    input_idx: usize,
    args: &'a [rumoca_core::Expression],
) -> Option<&'a rumoca_core::Expression> {
    let named_prefix = format!("__rumoca_named_arg__.{input_name}");
    for arg in args {
        let rumoca_core::Expression::FunctionCall {
            name,
            args: named_args,
            ..
        } = arg
        else {
            continue;
        };
        if name.as_str() == named_prefix {
            return named_args.first();
        }
    }
    args.iter()
        .filter(|arg| !is_named_argument_expression(arg))
        .nth(input_idx)
}

fn is_named_argument_expression(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::FunctionCall {
            name,
            is_constructor: true,
            ..
        } if name.as_str().starts_with("__rumoca_named_arg__.")
    )
}
