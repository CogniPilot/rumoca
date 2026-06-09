use super::*;
use crate::lower::function_projection::FunctionOutputProjection;

impl<'a> LowerBuilder<'a> {
    pub(in crate::lower) fn infer_expr_dims(
        &self,
        expr: &rumoca_core::Expression,
        scope: &Scope,
    ) -> Result<Vec<usize>, LowerError> {
        Ok(match expr {
            rumoca_core::Expression::VarRef {
                name,
                subscripts,
                span,
                ..
            } if subscripts.is_empty() => {
                self.infer_unsubscripted_var_ref_dims(name, scope, *span)?
            }
            rumoca_core::Expression::VarRef {
                name,
                subscripts,
                span,
                ..
            } => self.infer_required_subscripted_var_ref_dims(name, subscripts, scope, *span)?,
            rumoca_core::Expression::FieldAccess { base, field, .. } => {
                field_access_binding_key(base, field)
                    .ok()
                    .and_then(|key| self.layout.shape(&key).map(<[usize]>::to_vec))
                    .map(Ok)
                    .or_else(|| self.infer_function_call_field_dims(base, field).transpose())
                    .transpose()?
                    .map(Ok)
                    .unwrap_or_else(|| self.infer_expr_dims(base, scope))?
            }
            rumoca_core::Expression::BuiltinCall { function, args, .. } => {
                self.infer_builtin_call_dims(function, args, scope)?
            }
            rumoca_core::Expression::FunctionCall { name, args, .. }
                if is_synchronous_array_like_intrinsic(name.as_str()) =>
            {
                self.infer_expr_dims(required_arg(args, name.as_str())?, scope)?
            }
            rumoca_core::Expression::FunctionCall { name, span, .. } => {
                self.infer_function_call_output_dims(name, *span)?
            }
            rumoca_core::Expression::Array {
                elements,
                is_matrix,
                ..
            } => infer_array_literal_dims_with_result(elements, *is_matrix, |element| {
                self.infer_expr_dims(element, scope)
            })?,
            rumoca_core::Expression::Tuple { elements, .. } => {
                self.infer_tuple_flat_dims(elements, scope)?
            }
            rumoca_core::Expression::Range {
                start, step, end, ..
            } => lower_static_range_values(start, step.as_deref(), end)
                .expect("static range dimension inference must only receive scalar bounds")
                .map_or_else(Vec::new, |values| vector_dims(values.len())),
            rumoca_core::Expression::If {
                branches,
                else_branch,
                span,
                ..
            } => self.infer_if_expr_dims(branches, else_branch, scope, *span)?,
            rumoca_core::Expression::Unary { rhs, .. } => self.infer_expr_dims(rhs, scope)?,
            rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
                self.infer_binary_dims(op, lhs, rhs, scope)?
            }
            rumoca_core::Expression::Index {
                base, subscripts, ..
            } => self.infer_index_expr_dims(base, subscripts, scope)?,
            rumoca_core::Expression::ArrayComprehension { .. }
            | rumoca_core::Expression::Literal { value: _, .. }
            | rumoca_core::Expression::Empty { .. } => Vec::new(),
        })
    }

    fn infer_tuple_flat_dims(
        &self,
        elements: &[rumoca_core::Expression],
        scope: &Scope,
    ) -> Result<Vec<usize>, LowerError> {
        let mut scalar_count = 0usize;
        for element in elements {
            let dims = self.infer_expr_dims(element, scope)?;
            scalar_count += shape_size(&dims);
        }
        Ok(vector_dims(scalar_count))
    }

    fn infer_function_call_field_dims(
        &self,
        base: &rumoca_core::Expression,
        field: &str,
    ) -> Result<Option<Vec<usize>>, LowerError> {
        let rumoca_core::Expression::FunctionCall { name, .. } = base else {
            return Ok(None);
        };
        let Some(function) = self.lookup_function(name) else {
            return Ok(None);
        };
        let [output] = function.outputs.as_slice() else {
            return Ok(None);
        };
        for statement in &function.body {
            if let Some(dims) = self.infer_assigned_record_field_dims(statement, output, field)? {
                return Ok(Some(dims));
            }
        }
        Ok(None)
    }

    fn infer_assigned_record_field_dims(
        &self,
        statement: &rumoca_core::Statement,
        output: &rumoca_core::FunctionParam,
        field: &str,
    ) -> Result<Option<Vec<usize>>, LowerError> {
        let rumoca_core::Statement::Assignment { comp, value, .. } = statement else {
            return Ok(None);
        };
        Ok(match comp.parts.as_slice() {
            [target] if target.ident == output.name && target.subs.is_empty() => {
                self.infer_constructor_field_dims(value, field)?
            }
            [target, selected]
                if target.ident == output.name
                    && selected.ident == field
                    && target.subs.is_empty()
                    && selected.subs.is_empty() =>
            {
                Some(self.infer_expr_dims(value, &Scope::new())?)
            }
            _ => None,
        })
    }

    fn infer_constructor_field_dims(
        &self,
        expr: &rumoca_core::Expression,
        field: &str,
    ) -> Result<Option<Vec<usize>>, LowerError> {
        match expr {
            rumoca_core::Expression::FunctionCall {
                name,
                args,
                is_constructor,
                ..
            } => {
                if !self.is_record_constructor_call(name, *is_constructor) {
                    return Ok(None);
                }
                if let Some(input) = self
                    .lookup_function(name)
                    .and_then(|function| function.inputs.iter().find(|input| input.name == field))
                {
                    return concrete_i64_dims(
                        &input.dims,
                        field,
                        "constructor field dimensions",
                        input.span,
                    )
                    .map(Some);
                }
                self.infer_named_constructor_field_dims(args, field)
            }
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => {
                let else_dims = self.infer_constructor_field_dims(else_branch, field)?;
                if else_dims.as_ref().is_some_and(|dims| !dims.is_empty()) {
                    return Ok(else_dims);
                }
                Ok(branches
                    .iter()
                    .map(|(_, value)| self.infer_constructor_field_dims(value, field))
                    .find_map(|dims| match dims {
                        Ok(Some(dims)) if !dims.is_empty() => Some(Ok(dims)),
                        Ok(_) => None,
                        Err(err) => Some(Err(err)),
                    })
                    .transpose()?
                    .or(else_dims))
            }
            _ => Ok(None),
        }
    }

    fn infer_named_constructor_field_dims(
        &self,
        args: &[rumoca_core::Expression],
        field: &str,
    ) -> Result<Option<Vec<usize>>, LowerError> {
        args.iter()
            .filter_map(super::function_calls::decode_named_function_arg)
            .find_map(|(name, value)| {
                (name == field).then(|| self.infer_expr_dims(value, &Scope::new()))
            })
            .transpose()
    }

    fn infer_unsubscripted_var_ref_dims(
        &self,
        name: &rumoca_core::Reference,
        scope: &Scope,
        span: rumoca_core::Span,
    ) -> Result<Vec<usize>, LowerError> {
        let name_path = self.scope_key_from_reference(name, span)?;
        let name_text = name.as_str();
        if let Some(dims) = self.local_binding_dims.get(name_text)
            && dims.iter().all(|dim| *dim >= 0)
        {
            return Ok(dims.iter().map(|dim| *dim as usize).collect());
        }
        if let Some(values) = scoped_indexed_binding_values(scope, &name_path) {
            return Ok(vector_dims(values.len()));
        }
        if let Some(values) = self.local_indexed_binding_values(name_text) {
            return Ok(vector_dims(values.len()));
        }
        if let Some(shape) = self.layout.shape(name_text) {
            return Ok(shape.to_vec());
        }
        let indexed_dims = infer_indexed_dims(&indexed_entries_for_reference(
            &self.indexed_bindings,
            name,
            span,
        )?);
        if !indexed_dims.is_empty() {
            return Ok(indexed_dims);
        }
        if let Some(dims) = self.local_binding_dims.get(name_text) {
            return concrete_i64_dims(dims, name_text, "local binding dimensions", span);
        }
        if self.local_const_bindings.contains_key(name_text)
            || self.structural_bindings.contains_key(name_text)
            || self.layout.binding(name_text).is_some()
        {
            return Ok(Vec::new());
        }
        Ok(Vec::new())
    }

    fn infer_required_subscripted_var_ref_dims(
        &self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        scope: &Scope,
        span: rumoca_core::Span,
    ) -> Result<Vec<usize>, LowerError> {
        if let Some(dims) = self.infer_slice_dims(name.as_str(), subscripts, scope)? {
            return Ok(dims);
        }
        let base_dims = self.infer_unsubscripted_var_ref_dims(name, scope, span)?;
        if base_dims.is_empty() {
            return Err(LowerError::ContractViolation {
                reason: format!(
                    "subscripted variable `{}` has no array shape metadata",
                    name.as_str()
                ),
                span: subscript_or_expr_span(subscripts, span),
            });
        }
        if subscripts.len() > base_dims.len() {
            return Err(LowerError::ContractViolation {
                reason: format!(
                    "subscripted variable `{}` has {} subscripts for {} dimensions",
                    name.as_str(),
                    subscripts.len(),
                    base_dims.len()
                ),
                span: subscript_or_expr_span(subscripts, span),
            });
        }
        inferred_subscripted_dims(&base_dims, subscripts, self, scope)
    }

    fn infer_function_call_output_dims(
        &self,
        name: &rumoca_core::Reference,
        span: rumoca_core::Span,
    ) -> Result<Vec<usize>, LowerError> {
        if resolve_intrinsic_builtin(name.as_str()).is_some() {
            return Ok(Vec::new());
        }
        if let Some(projection) = self.lookup_function_output_projection(name) {
            return self.infer_projected_function_call_output_dims(&projection, span);
        }
        let function = self
            .lookup_function(name)
            .ok_or_else(|| LowerError::MissingFunction {
                name: name.as_str().to_string(),
            })?;
        let output = function
            .outputs
            .first()
            .ok_or_else(|| LowerError::ContractViolation {
                reason: format!("function `{}` has no output value", name.as_str()),
                span,
            })?;
        concrete_i64_dims(
            &output.dims,
            name.as_str(),
            "function output dimensions",
            if output.span.is_dummy() {
                span
            } else {
                output.span
            },
        )
    }

    fn infer_projected_function_call_output_dims(
        &self,
        projection: &FunctionOutputProjection,
        span: rumoca_core::Span,
    ) -> Result<Vec<usize>, LowerError> {
        let function = self
            .lookup_function_key(projection.base_function_name.as_str())
            .ok_or_else(|| LowerError::MissingFunction {
                name: projection.base_function_name.as_str().to_string(),
            })?;
        let output = function
            .outputs
            .iter()
            .find(|output| output.name == projection.output_name)
            .ok_or_else(|| LowerError::ContractViolation {
                reason: format!(
                    "function `{}` has no output `{}`",
                    projection.base_function_name.as_str(),
                    projection.output_name
                ),
                span,
            })?;
        let dims = concrete_i64_dims(
            &output.dims,
            projection.output_name.as_str(),
            "projected function output dimensions",
            if output.span.is_dummy() {
                span
            } else {
                output.span
            },
        )?;
        if projection.indices.is_empty() {
            return Ok(dims);
        }
        if projection.indices.len() > dims.len() {
            return Err(LowerError::ContractViolation {
                reason: format!(
                    "projected function output `{}` has {} indices for {} dimensions",
                    projection.output_name,
                    projection.indices.len(),
                    dims.len()
                ),
                span,
            });
        }
        Ok(dims[projection.indices.len()..].to_vec())
    }

    fn infer_if_expr_dims(
        &self,
        branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
        else_branch: &rumoca_core::Expression,
        scope: &Scope,
        span: rumoca_core::Span,
    ) -> Result<Vec<usize>, LowerError> {
        let expected = self.infer_expr_dims(else_branch, scope)?;
        for (_, value) in branches {
            let dims = self.infer_expr_dims(value, scope)?;
            if dims != expected {
                return Err(LowerError::ContractViolation {
                    reason: "if-expression branches have mismatched array dimensions".to_string(),
                    span: value.span().unwrap_or(span),
                });
            }
        }
        Ok(expected)
    }

    fn infer_builtin_call_dims(
        &self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
        scope: &Scope,
    ) -> Result<Vec<usize>, LowerError> {
        if let Some(dims) = self.infer_linear_algebra_builtin_dims(function, args, scope)? {
            return Ok(dims);
        }
        Ok(match function {
            rumoca_core::BuiltinFunction::Der
            | rumoca_core::BuiltinFunction::Pre
            | rumoca_core::BuiltinFunction::Sample
            | rumoca_core::BuiltinFunction::NoEvent => {
                self.infer_expr_dims(required_builtin_arg(args, function, 0)?, scope)?
            }
            rumoca_core::BuiltinFunction::Smooth => {
                self.infer_expr_dims(required_builtin_arg(args, function, 1)?, scope)?
            }
            function if unary_array_builtin_op(function).is_some() => {
                self.infer_expr_dims(required_builtin_arg(args, function, 0)?, scope)?
            }
            rumoca_core::BuiltinFunction::Fill => self.infer_fill_dims(&args[1..])?,
            rumoca_core::BuiltinFunction::Zeros | rumoca_core::BuiltinFunction::Ones => {
                self.infer_fill_dims(args)?
            }
            rumoca_core::BuiltinFunction::Linspace => {
                vector_dims(self.eval_compile_time_positive_index(
                    required_builtin_arg(args, function, 2)?,
                    &self.local_const_bindings,
                    "linspace count",
                )?)
            }
            rumoca_core::BuiltinFunction::Cat => self.infer_cat_dims(args, scope)?,
            _ => Vec::new(),
        })
    }

    fn infer_linear_algebra_builtin_dims(
        &self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
        scope: &Scope,
    ) -> Result<Option<Vec<usize>>, LowerError> {
        Ok(match function {
            rumoca_core::BuiltinFunction::Identity => Some(self.infer_identity_dims(args)?),
            rumoca_core::BuiltinFunction::Diagonal => Some(self.infer_diagonal_dims(args, scope)?),
            rumoca_core::BuiltinFunction::Transpose => {
                Some(self.infer_transpose_dims(args, scope)?)
            }
            rumoca_core::BuiltinFunction::Scalar => Some(Vec::new()),
            rumoca_core::BuiltinFunction::Vector => Some(self.infer_vector_dims(args, scope)?),
            rumoca_core::BuiltinFunction::Matrix => Some(self.infer_matrix_dims(args, scope)?),
            rumoca_core::BuiltinFunction::Cross => Some(vec![3]),
            rumoca_core::BuiltinFunction::Skew => Some(vec![3, 3]),
            rumoca_core::BuiltinFunction::OuterProduct => {
                Some(self.infer_outer_product_dims(args, scope)?)
            }
            rumoca_core::BuiltinFunction::Symmetric => {
                Some(self.infer_symmetric_dims(args, scope)?)
            }
            _ => None,
        })
    }

    fn infer_identity_dims(
        &self,
        args: &[rumoca_core::Expression],
    ) -> Result<Vec<usize>, LowerError> {
        let dim = self.eval_compile_time_positive_index(
            required_builtin_arg(args, &rumoca_core::BuiltinFunction::Identity, 0)?,
            &self.local_const_bindings,
            "identity dimension",
        )?;
        Ok(vec![dim, dim])
    }

    fn infer_diagonal_dims(
        &self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
    ) -> Result<Vec<usize>, LowerError> {
        let dims = self.infer_expr_dims(
            required_builtin_arg(args, &rumoca_core::BuiltinFunction::Diagonal, 0)?,
            scope,
        )?;
        Ok(match dims.as_slice() {
            [dim] => vec![*dim, *dim],
            _ => Vec::new(),
        })
    }

    fn infer_transpose_dims(
        &self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
    ) -> Result<Vec<usize>, LowerError> {
        let dims = self.infer_expr_dims(
            required_builtin_arg(args, &rumoca_core::BuiltinFunction::Transpose, 0)?,
            scope,
        )?;
        Ok(match dims.as_slice() {
            [rows, cols] => vec![*cols, *rows],
            _ => dims,
        })
    }

    fn infer_vector_dims(
        &self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
    ) -> Result<Vec<usize>, LowerError> {
        let dims = self.infer_expr_dims(
            required_builtin_arg(args, &rumoca_core::BuiltinFunction::Vector, 0)?,
            scope,
        )?;
        Ok(vector_dims(shape_size(&dims)))
    }

    fn infer_matrix_dims(
        &self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
    ) -> Result<Vec<usize>, LowerError> {
        let dims = self.infer_expr_dims(
            required_builtin_arg(args, &rumoca_core::BuiltinFunction::Matrix, 0)?,
            scope,
        )?;
        Ok(match dims.as_slice() {
            [] => vec![1, 1],
            [dim] => vec![*dim, 1],
            [rows, cols] => vec![*rows, *cols],
            _ => Vec::new(),
        })
    }

    fn infer_outer_product_dims(
        &self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
    ) -> Result<Vec<usize>, LowerError> {
        let lhs_dims = self.infer_expr_dims(
            required_builtin_arg(args, &rumoca_core::BuiltinFunction::OuterProduct, 0)?,
            scope,
        )?;
        let rhs_dims = self.infer_expr_dims(
            required_builtin_arg(args, &rumoca_core::BuiltinFunction::OuterProduct, 1)?,
            scope,
        )?;
        Ok(match (lhs_dims.as_slice(), rhs_dims.as_slice()) {
            ([lhs], [rhs]) => vec![*lhs, *rhs],
            _ => Vec::new(),
        })
    }

    fn infer_symmetric_dims(
        &self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
    ) -> Result<Vec<usize>, LowerError> {
        let dims = self.infer_expr_dims(
            required_builtin_arg(args, &rumoca_core::BuiltinFunction::Symmetric, 0)?,
            scope,
        )?;
        Ok(if matches!(dims.as_slice(), [rows, cols] if rows == cols) {
            dims
        } else {
            Vec::new()
        })
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
    ) -> Result<Vec<usize>, LowerError> {
        if let Ok(base_name) = dynamic_binding_base_key(base)
            && let Some(dims) = self.infer_slice_dims(base_name.as_str(), subscripts, scope)?
        {
            return Ok(dims);
        }
        let base_dims = self.infer_expr_dims(base, scope)?;
        if base_dims.is_empty() && scalar_singleton_projection(subscripts) {
            return Ok(Vec::new());
        }
        if subscripts.len() > base_dims.len() {
            return Err(LowerError::ContractViolation {
                reason: format!(
                    "indexed expression has {} subscripts for {} inferred dimensions",
                    subscripts.len(),
                    base_dims.len()
                ),
                span: base.span().unwrap_or_else(|| {
                    subscript_or_expr_span(subscripts, rumoca_core::Span::DUMMY)
                }),
            });
        }
        inferred_subscripted_dims(&base_dims, subscripts, self, scope)
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
            None if end >= start => 1,
            None => -1,
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
            .map(|arg| {
                Ok(ArrayOperand {
                    values: Vec::new(),
                    dims: self.infer_expr_dims(arg, scope)?,
                })
            })
            .collect::<Result<Vec<_>, LowerError>>()?;
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
    ) -> Result<Vec<usize>, LowerError> {
        use rumoca_core::OpBinary as Op;

        let lhs_dims = self.infer_expr_dims(lhs, scope)?;
        let rhs_dims = self.infer_expr_dims(rhs, scope)?;
        Ok(match op {
            Op::Mul => multiplication_dims(&lhs_dims, &rhs_dims)?,
            Op::Add
            | Op::AddElem
            | Op::Sub
            | Op::SubElem
            | Op::MulElem
            | Op::DivElem
            | Op::ExpElem => broadcast_shape(&lhs_dims, &rhs_dims)?,
            Op::Div if rhs_dims.is_empty() => lhs_dims,
            _ => Vec::new(),
        })
    }
}

fn required_arg<'a>(
    args: &'a [rumoca_core::Expression],
    name: &str,
) -> Result<&'a rumoca_core::Expression, LowerError> {
    args.first().ok_or_else(|| LowerError::ContractViolation {
        reason: format!("{name} requires at least one argument"),
        span: rumoca_core::Span::DUMMY,
    })
}

fn required_builtin_arg<'a>(
    args: &'a [rumoca_core::Expression],
    function: &rumoca_core::BuiltinFunction,
    index: usize,
) -> Result<&'a rumoca_core::Expression, LowerError> {
    args.get(index)
        .ok_or_else(|| LowerError::ContractViolation {
            reason: format!("{function:?} requires argument {}", index + 1),
            span: rumoca_core::Span::DUMMY,
        })
}

fn scalar_singleton_projection(subscripts: &[rumoca_core::Subscript]) -> bool {
    !subscripts.is_empty()
        && subscripts.iter().all(|subscript| match subscript {
            rumoca_core::Subscript::Index { value, .. } => *value == 1,
            rumoca_core::Subscript::Colon { .. } => true,
            rumoca_core::Subscript::Expr { .. } => false,
        })
}

fn concrete_i64_dims(
    dims: &[i64],
    name: &str,
    context: &str,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    dims.iter()
        .map(|dim| {
            usize::try_from(*dim).map_err(|_| LowerError::ContractViolation {
                reason: format!(
                    "{context} for `{name}` contain unresolved negative dimension {dim}"
                ),
                span,
            })
        })
        .collect()
}

fn subscript_or_expr_span(
    subscripts: &[rumoca_core::Subscript],
    fallback: rumoca_core::Span,
) -> rumoca_core::Span {
    subscripts
        .iter()
        .map(rumoca_core::Subscript::span)
        .find(|span| !span.is_dummy())
        .unwrap_or(fallback)
}
