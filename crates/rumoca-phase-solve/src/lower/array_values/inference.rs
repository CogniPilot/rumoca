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
                    .and_then(|key| self.infer_field_access_binding_dims(&key, scope))
                    .map(Ok)
                    .or_else(|| self.infer_function_call_field_dims(base, field).transpose())
                    .transpose()?
                    .map(Ok)
                    .unwrap_or_else(|| self.infer_expr_dims(base, scope))?
            }
            rumoca_core::Expression::BuiltinCall {
                function,
                args,
                span,
            } => self.infer_builtin_call_dims(function, args, scope, *span)?,
            rumoca_core::Expression::FunctionCall {
                name, args, span, ..
            } if is_synchronous_array_like_intrinsic(name.as_str()) => {
                self.infer_expr_dims(required_arg(args, name.as_str(), *span)?, scope)?
            }
            rumoca_core::Expression::FunctionCall {
                name, args, span, ..
            } if is_stream_passthrough_intrinsic(name.as_str()) => {
                self.infer_expr_dims(required_arg(args, name.as_str(), *span)?, scope)?
            }
            rumoca_core::Expression::FunctionCall { name, args, .. }
                if is_modelica_array_constructor_function(name) =>
            {
                infer_array_literal_dims_with_result(args, false, |element| {
                    self.infer_expr_dims(element, scope)
                })?
            }
            rumoca_core::Expression::FunctionCall {
                is_constructor: true,
                ..
            } => Vec::new(),
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
                start,
                step,
                end,
                span,
            } => self.infer_range_dims(start, step.as_deref(), end, *span)?,
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
                base,
                subscripts,
                span,
            } => self.infer_index_expr_dims(base, subscripts, *span, scope)?,
            rumoca_core::Expression::ArrayComprehension { .. }
            | rumoca_core::Expression::Literal { value: _, .. }
            | rumoca_core::Expression::Empty { .. } => Vec::new(),
        })
    }

    fn infer_field_access_binding_dims(&self, key: &str, scope: &Scope) -> Option<Vec<usize>> {
        if let Some(dims) = self.local_binding_dims.get(key)
            && dims.iter().all(|dim| *dim >= 0)
        {
            return dims.iter().map(|dim| usize::try_from(*dim).ok()).collect();
        }
        if let Some(values) = self.local_indexed_binding_values(key) {
            return Some(vector_dims(values.len()));
        }
        if let Some(shape) = self.layout.shape(key) {
            return Some(shape.to_vec());
        }
        if scope.contains_key(&generated_scope_key(key))
            || self.local_const_bindings.contains_key(key)
            || self.structural_bindings.contains_key(key)
            || self.direct_assignments.contains_key(key)
            || self.layout.binding(key).is_some()
        {
            return Some(Vec::new());
        }
        None
    }

    fn infer_range_dims(
        &self,
        start: &rumoca_core::Expression,
        step: Option<&rumoca_core::Expression>,
        end: &rumoca_core::Expression,
        span: rumoca_core::Span,
    ) -> Result<Vec<usize>, LowerError> {
        if let Some(values) = lower_static_range_values(start, step, end, span)? {
            return Ok(vector_dims(values.len()));
        }
        match self.eval_compile_time_range_values(
            start,
            step,
            end,
            span,
            &self.local_const_bindings,
            "range dimension inference",
        ) {
            Ok(values) => Ok(vector_dims(values.len())),
            Err(_) => Ok(Vec::new()),
        }
    }

    fn infer_tuple_flat_dims(
        &self,
        elements: &[rumoca_core::Expression],
        scope: &Scope,
    ) -> Result<Vec<usize>, LowerError> {
        let mut scalar_count = 0usize;
        for element in elements {
            let dims = self.infer_expr_dims(element, scope)?;
            let span = self.required_inference_expr_span(element, "tuple element scalar count")?;
            let element_size = checked_shape_size(&dims, "tuple element scalar count", span)?;
            scalar_count = scalar_count.checked_add(element_size).ok_or_else(|| {
                LowerError::contract_violation(
                    "tuple scalar count overflows host index range",
                    span,
                )
            })?;
        }
        Ok(vector_dims(scalar_count))
    }

    fn required_inference_expr_span(
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

    fn required_inference_span_from_exprs(
        &self,
        exprs: &[&rumoca_core::Expression],
        context: &'static str,
    ) -> Result<rumoca_core::Span, LowerError> {
        exprs
            .iter()
            .find_map(|expr| expr.span())
            .or_else(|| self.active_source_context_span())
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: format!("missing source provenance for {context}"),
            })
    }

    fn required_subscript_or_context_span(
        &self,
        subscripts: &[rumoca_core::Subscript],
        owner_span: Option<rumoca_core::Span>,
        context: &'static str,
    ) -> Result<rumoca_core::Span, LowerError> {
        subscripts
            .iter()
            .map(rumoca_core::Subscript::span)
            .find(|span| !span.is_dummy())
            .or_else(|| owner_span.filter(|span| !span.is_dummy()))
            .or_else(|| self.active_source_context_span())
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: format!("missing source provenance for {context}"),
            })
    }

    fn infer_function_call_field_dims(
        &self,
        base: &rumoca_core::Expression,
        field: &str,
    ) -> Result<Option<Vec<usize>>, LowerError> {
        let rumoca_core::Expression::FunctionCall {
            name,
            is_constructor,
            ..
        } = base
        else {
            return Ok(None);
        };
        if self.is_record_constructor_call(name, *is_constructor) {
            return self.infer_constructor_field_dims(base, field);
        }
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
        let name_text = name.as_str();
        if let Some(dims) = self.local_binding_dims.get(name_text)
            && dims.iter().all(|dim| *dim >= 0)
        {
            return concrete_i64_dims(dims, name_text, "local binding dimensions", span);
        }
        if let Some(values) =
            scoped_indexed_binding_values(scope, &generated_scope_key(name_text), span)?
        {
            return Ok(vector_dims(values.len()));
        }
        if let Some(values) = self.local_indexed_binding_values(name_text) {
            return Ok(vector_dims(values.len()));
        }
        if let Some(shape) = self.layout.shape(name_text) {
            return copy_shape_dims(shape, "layout binding shape rank", span);
        }
        if let Some(key) = indexed_key_for_reference(&self.indexed_bindings, name, span)? {
            let Some(meta) = self.indexed_meta_for_key(&key) else {
                return Err(LowerError::contract_violation(
                    format!(
                        "indexed binding metadata for `{key:?}` has no corresponding binding group"
                    ),
                    span,
                ));
            };
            if !meta.dims.is_empty() {
                return Ok(meta.dims.clone());
            }
        }
        if let Some(dims) = self.infer_record_array_aggregate_dims_from_dae_variables(name)? {
            return Ok(dims);
        }
        if let Some(variable) = self
            .dae_variables
            .and_then(|variables| dae_variable(variables, name.var_name()))
            && !variable.dims.is_empty()
        {
            return concrete_i64_dims(&variable.dims, name_text, "DAE variable dimensions", span);
        }
        if let Some(dims) = self.local_binding_dims.get(name_text) {
            return concrete_i64_dims(dims, name_text, "local binding dimensions", span);
        }
        if self.local_const_bindings.contains_key(name_text)
            || self.structural_bindings.contains_key(name_text)
            || scope.contains_key(&generated_scope_key(name_text))
            || self.layout.binding(name_text).is_some()
        {
            return Ok(Vec::new());
        }
        if let Some(reference) = self.singleton_record_array_field_reference(name)
            && let Some(variable) = self
                .dae_variables
                .and_then(|variables| dae_variable(variables, reference.var_name()))
        {
            return concrete_i64_dims(
                &variable.dims,
                reference.as_str(),
                "singleton record-array field dimensions",
                span,
            );
        }
        let name_path = self.scope_key_from_reference(name, span)?;
        if let Some(values) = scoped_indexed_binding_values(scope, &name_path, span)? {
            return Ok(vector_dims(values.len()));
        }
        Ok(Vec::new())
    }

    fn infer_record_array_aggregate_dims_from_dae_variables(
        &self,
        name: &rumoca_core::Reference,
    ) -> Result<Option<Vec<usize>>, LowerError> {
        let Some(base_ref) = name.component_ref() else {
            return Ok(None);
        };
        if base_ref.parts.is_empty()
            || base_ref
                .parts
                .last()
                .is_some_and(|part| !part.subs.is_empty())
        {
            return Ok(None);
        }
        let Some(variables) = self.dae_variables else {
            return Ok(None);
        };
        let mut extent = 0usize;
        for variable in variables
            .states
            .values()
            .chain(variables.algebraics.values())
            .chain(variables.inputs.values())
            .chain(variables.outputs.values())
            .chain(variables.parameters.values())
            .chain(variables.constants.values())
            .chain(variables.discrete_reals.values())
            .chain(variables.discrete_valued.values())
        {
            let Some(candidate_ref) = variable.component_ref.as_ref() else {
                continue;
            };
            let Some(index) = record_array_prefix_index(base_ref, candidate_ref)? else {
                continue;
            };
            extent = extent.max(index);
        }
        Ok((extent > 0).then(|| vector_dims(extent)))
    }

    fn infer_required_subscripted_var_ref_dims(
        &self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        scope: &Scope,
        span: rumoca_core::Span,
    ) -> Result<Vec<usize>, LowerError> {
        if let Some(dims) =
            self.infer_slice_dims(name.as_str(), subscripts, scope, Self::non_dummy_span(span))?
        {
            return Ok(dims);
        }
        let base_dims = self.infer_unsubscripted_var_ref_dims(name, scope, span)?;
        if base_dims.is_empty() {
            return Err(LowerError::contract_violation(
                format!(
                    "subscripted variable `{}` has no array shape metadata",
                    name.as_str()
                ),
                subscript_or_expr_span(subscripts, span),
            ));
        }
        if subscripts.len() > base_dims.len() {
            return Err(LowerError::contract_violation(
                format!(
                    "subscripted variable `{}` has {} subscripts for {} dimensions",
                    name.as_str(),
                    subscripts.len(),
                    base_dims.len()
                ),
                subscript_or_expr_span(subscripts, span),
            ));
        }
        inferred_subscripted_dims(&base_dims, subscripts, Some(span), self, scope)
    }

    fn infer_function_call_output_dims(
        &self,
        name: &rumoca_core::Reference,
        span: rumoca_core::Span,
    ) -> Result<Vec<usize>, LowerError> {
        if resolve_intrinsic_builtin(name.as_str()).is_some() {
            return Ok(Vec::new());
        }
        if let Some(projection) = self.lookup_function_output_projection(name, span)? {
            return self.infer_projected_function_call_output_dims(&projection, span);
        }
        let function = self
            .lookup_function(name)
            .ok_or_else(|| LowerError::MissingFunction {
                name: name.as_str().to_string(),
            })?;
        let output = function.outputs.first().ok_or_else(|| {
            LowerError::contract_violation(
                format!("function `{}` has no output value", name.as_str()),
                span,
            )
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
            .ok_or_else(|| {
                LowerError::contract_violation(
                    format!(
                        "function `{}` has no output `{}`",
                        projection.base_function_name.as_str(),
                        projection.output_name
                    ),
                    span,
                )
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
        if projection.scope_indices.is_empty() {
            return Ok(dims);
        }
        if projection.scope_indices.len() > dims.len() {
            return Err(LowerError::contract_violation(
                format!(
                    "projected function output `{}` has {} indices for {} dimensions",
                    projection.output_name,
                    projection.scope_indices.len(),
                    dims.len()
                ),
                span,
            ));
        }
        copy_shape_dims(
            &dims[projection.scope_indices.len()..],
            "projected function output remaining shape rank",
            span,
        )
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
                return Err(LowerError::contract_violation(
                    "if-expression branches have mismatched array dimensions",
                    value.span().unwrap_or(span),
                ));
            }
        }
        Ok(expected)
    }

    fn infer_builtin_call_dims(
        &self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_span: rumoca_core::Span,
    ) -> Result<Vec<usize>, LowerError> {
        if let Some(dims) =
            self.infer_linear_algebra_builtin_dims(function, args, scope, call_span)?
        {
            return Ok(dims);
        }
        Ok(match function {
            rumoca_core::BuiltinFunction::Der
            | rumoca_core::BuiltinFunction::Pre
            | rumoca_core::BuiltinFunction::Sample
            | rumoca_core::BuiltinFunction::NoEvent => {
                self.infer_expr_dims(required_builtin_arg(args, function, 0, call_span)?, scope)?
            }
            rumoca_core::BuiltinFunction::Smooth => {
                self.infer_expr_dims(required_builtin_arg(args, function, 1, call_span)?, scope)?
            }
            function if unary_array_builtin_op(function).is_some() => {
                self.infer_expr_dims(required_builtin_arg(args, function, 0, call_span)?, scope)?
            }
            rumoca_core::BuiltinFunction::Fill => {
                let dims = fill_dimension_args(args, call_span)?;
                self.infer_fill_dims(dims, call_span)?
            }
            rumoca_core::BuiltinFunction::Zeros | rumoca_core::BuiltinFunction::Ones => {
                self.infer_fill_dims(args, call_span)?
            }
            rumoca_core::BuiltinFunction::Linspace => {
                vector_dims(self.eval_compile_time_positive_index(
                    required_builtin_arg(args, function, 2, call_span)?,
                    &self.local_const_bindings,
                    "linspace count",
                )?)
            }
            rumoca_core::BuiltinFunction::Cat => self.infer_cat_dims(args, scope, call_span)?,
            _ => Vec::new(),
        })
    }

    fn infer_linear_algebra_builtin_dims(
        &self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_span: rumoca_core::Span,
    ) -> Result<Option<Vec<usize>>, LowerError> {
        Ok(match function {
            rumoca_core::BuiltinFunction::Identity => {
                Some(self.infer_identity_dims(args, call_span)?)
            }
            rumoca_core::BuiltinFunction::Diagonal => {
                Some(self.infer_diagonal_dims(args, scope, call_span)?)
            }
            rumoca_core::BuiltinFunction::Transpose => {
                Some(self.infer_transpose_dims(args, scope, call_span)?)
            }
            rumoca_core::BuiltinFunction::Scalar => Some(Vec::new()),
            rumoca_core::BuiltinFunction::Vector => {
                Some(self.infer_vector_dims(args, scope, call_span)?)
            }
            rumoca_core::BuiltinFunction::Matrix => {
                Some(self.infer_matrix_dims(args, scope, call_span)?)
            }
            rumoca_core::BuiltinFunction::Cross => Some(vec![3]),
            rumoca_core::BuiltinFunction::Skew => Some(vec![3, 3]),
            rumoca_core::BuiltinFunction::OuterProduct => {
                Some(self.infer_outer_product_dims(args, scope, call_span)?)
            }
            rumoca_core::BuiltinFunction::Symmetric => {
                Some(self.infer_symmetric_dims(args, scope, call_span)?)
            }
            _ => None,
        })
    }

    fn infer_identity_dims(
        &self,
        args: &[rumoca_core::Expression],
        call_span: rumoca_core::Span,
    ) -> Result<Vec<usize>, LowerError> {
        let dim = self.eval_compile_time_positive_index(
            required_builtin_arg(args, &rumoca_core::BuiltinFunction::Identity, 0, call_span)?,
            &self.local_const_bindings,
            "identity dimension",
        )?;
        Ok(vec![dim, dim])
    }

    fn infer_diagonal_dims(
        &self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_span: rumoca_core::Span,
    ) -> Result<Vec<usize>, LowerError> {
        let dims = self.infer_expr_dims(
            required_builtin_arg(args, &rumoca_core::BuiltinFunction::Diagonal, 0, call_span)?,
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
        call_span: rumoca_core::Span,
    ) -> Result<Vec<usize>, LowerError> {
        let dims = self.infer_expr_dims(
            required_builtin_arg(args, &rumoca_core::BuiltinFunction::Transpose, 0, call_span)?,
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
        call_span: rumoca_core::Span,
    ) -> Result<Vec<usize>, LowerError> {
        let dims = self.infer_expr_dims(
            required_builtin_arg(args, &rumoca_core::BuiltinFunction::Vector, 0, call_span)?,
            scope,
        )?;
        Ok(vector_dims(checked_shape_size(
            &dims,
            "vector() value count",
            args.first()
                .and_then(rumoca_core::Expression::span)
                .unwrap_or(call_span),
        )?))
    }

    fn infer_matrix_dims(
        &self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_span: rumoca_core::Span,
    ) -> Result<Vec<usize>, LowerError> {
        let dims = self.infer_expr_dims(
            required_builtin_arg(args, &rumoca_core::BuiltinFunction::Matrix, 0, call_span)?,
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
        call_span: rumoca_core::Span,
    ) -> Result<Vec<usize>, LowerError> {
        let lhs_dims = self.infer_expr_dims(
            required_builtin_arg(
                args,
                &rumoca_core::BuiltinFunction::OuterProduct,
                0,
                call_span,
            )?,
            scope,
        )?;
        let rhs_dims = self.infer_expr_dims(
            required_builtin_arg(
                args,
                &rumoca_core::BuiltinFunction::OuterProduct,
                1,
                call_span,
            )?,
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
        call_span: rumoca_core::Span,
    ) -> Result<Vec<usize>, LowerError> {
        let dims = self.infer_expr_dims(
            required_builtin_arg(args, &rumoca_core::BuiltinFunction::Symmetric, 0, call_span)?,
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
        owner_span: Option<rumoca_core::Span>,
    ) -> Result<Option<Vec<usize>>, LowerError> {
        let Some(shape) = self.layout.shape(base_name) else {
            return Ok(None);
        };
        let span = self.required_subscript_or_context_span(
            subscripts,
            owner_span,
            "inferred slice dimension count",
        )?;
        let counts = self.slice_selection_counts(subscripts, shape, scope, Some(span))?;
        let mut dims =
            array_vec_with_capacity(counts.len(), "inferred slice dimension count", span)?;
        for (subscript, count) in subscripts.iter().zip(counts.iter().copied()) {
            if subscript_preserves_array_rank(subscript) {
                dims.push(count);
            }
        }
        dims.extend(counts.iter().skip(subscripts.len()).copied());
        Ok(Some(dims))
    }

    /// Per-dimension selection widths of a subscripted variable. Unlike
    /// `slice_selections` this never needs the runtime *value* of a scalar
    /// index subscript: a scalar subscript always selects one element, so
    /// only range subscripts require compile-time evaluation here.
    fn slice_selection_counts(
        &self,
        subscripts: &[rumoca_core::Subscript],
        shape: &[usize],
        scope: &Scope,
        owner_span: Option<rumoca_core::Span>,
    ) -> Result<Vec<usize>, LowerError> {
        let span = self.required_subscript_or_context_span(
            subscripts,
            owner_span,
            "slice selection count",
        )?;
        if subscripts.len() > shape.len() {
            return Err(unsupported_at(
                "array slice has more subscripts than dimensions",
                span,
            ));
        }
        let mut counts = array_vec_with_capacity(shape.len(), "slice selection count", span)?;
        for (dim_index, subscript) in subscripts.iter().enumerate() {
            counts.push(self.slice_subscript_count(subscript, shape[dim_index], scope)?);
        }
        reserve_array_capacity(
            &mut counts,
            shape.len() - subscripts.len(),
            "slice selection count",
            span,
        )?;
        for dim in &shape[subscripts.len()..] {
            counts.push(*dim);
        }
        Ok(counts)
    }

    fn slice_subscript_count(
        &self,
        subscript: &rumoca_core::Subscript,
        dim: usize,
        scope: &Scope,
    ) -> Result<usize, LowerError> {
        match subscript {
            rumoca_core::Subscript::Index { value, .. } if *value > 0 => Ok(1),
            rumoca_core::Subscript::Colon { .. } => Ok(dim),
            rumoca_core::Subscript::Expr { expr, span } => match expr.as_ref() {
                rumoca_core::Expression::Range { .. } => {
                    Ok(self.slice_expr_indices(expr, dim, scope, *span)?.len())
                }
                _ => {
                    let index_dims = self.infer_expr_dims(expr, scope)?;
                    if !index_dims.is_empty() {
                        return Err(unsupported_at(
                            "array-valued subscript is unsupported in slice shape inference",
                            expr.span().unwrap_or_else(|| subscript.span()),
                        ));
                    }
                    Ok(1)
                }
            },
            _ => Err(unsupported_at(
                "non-positive subscript is unsupported",
                subscript.span(),
            )),
        }
    }

    fn infer_index_expr_dims(
        &self,
        base: &rumoca_core::Expression,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
        scope: &Scope,
    ) -> Result<Vec<usize>, LowerError> {
        let owner_span = Self::non_dummy_span(span).or_else(|| base.span());
        if scalar_literal_projection(base, subscripts, owner_span)? {
            return Ok(Vec::new());
        }
        if let Ok(base_name) = dynamic_binding_base_key(base)
            && let Some(dims) =
                self.infer_slice_dims(base_name.as_str(), subscripts, scope, owner_span)?
        {
            return Ok(dims);
        }
        if dynamic_binding_base_key(base).is_err()
            && subscripts
                .iter()
                .all(super::helpers::is_scalar_selector_subscript)
        {
            return Ok(Vec::new());
        }
        let base_dims = self.infer_expr_dims(base, scope)?;
        if base_dims.is_empty() && scalar_singleton_projection(subscripts) {
            return Ok(Vec::new());
        }
        if subscripts.len() > base_dims.len() {
            let reason = format!(
                "indexed expression has {} subscripts for {} inferred dimensions",
                subscripts.len(),
                base_dims.len()
            );
            let span = expr_span_from_subscripts(subscripts).or_else(|| base.span());
            if let Some(span) = span {
                return Err(LowerError::contract_violation(reason, span));
            }
            return Err(LowerError::UnspannedContractViolation { reason });
        }
        inferred_subscripted_dims(&base_dims, subscripts, base.span(), self, scope)
    }

    pub(super) fn eval_compile_time_positive_index(
        &self,
        expr: &rumoca_core::Expression,
        const_scope: &IndexMap<String, f64>,
        context: &str,
    ) -> Result<usize, LowerError> {
        let span = expr
            .span()
            .or_else(|| self.active_source_context_span())
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: format!("{context} requires source span metadata"),
            })?;
        self.eval_compile_time_positive_index_at(expr, const_scope, context, span)
    }

    pub(super) fn eval_compile_time_positive_index_at(
        &self,
        expr: &rumoca_core::Expression,
        const_scope: &IndexMap<String, f64>,
        context: &str,
        context_span: rumoca_core::Span,
    ) -> Result<usize, LowerError> {
        let span = expr.span().unwrap_or(context_span);
        let value = self.eval_compile_time_int_at(expr, const_scope, context, span)?;
        usize::try_from(value)
            .ok()
            .filter(|value| *value > 0)
            .map_or_else(
                || Err(unsupported_at(format!("{context} must be positive"), span)),
                Ok,
            )
    }

    #[allow(clippy::excessive_nesting)]
    pub(in crate::lower) fn compile_time_subscript_indices(
        &self,
        subscripts: &[rumoca_core::Subscript],
        owner_span: rumoca_core::Span,
    ) -> Result<Option<Vec<usize>>, LowerError> {
        if subscripts.is_empty() {
            return Ok(Some(Vec::new()));
        }

        let const_scope = &self.local_const_bindings;
        let span = subscript_or_expr_span(subscripts, owner_span);
        let mut indices =
            array_vec_with_capacity(subscripts.len(), "compile-time subscript index count", span)?;
        for subscript in subscripts {
            match subscript {
                rumoca_core::Subscript::Index { value, span } if *value > 0 => {
                    indices.push(crate::lower::helpers::positive_i64_index(
                        *value,
                        crate::lower::helpers::span_or_owner(*span, owner_span),
                    )?)
                }
                rumoca_core::Subscript::Expr { expr, .. }
                    if !matches!(expr.as_ref(), rumoca_core::Expression::Range { .. })
                        && self.expr_can_eval_compile_time(expr, const_scope) =>
                {
                    match self.eval_compile_time_positive_index(
                        expr,
                        const_scope,
                        "array subscript",
                    ) {
                        Ok(index) => indices.push(index),
                        Err(_)
                            if matches!(expr.as_ref(), rumoca_core::Expression::VarRef { .. }) =>
                        {
                            return Ok(None);
                        }
                        Err(err) => return Err(err),
                    }
                }
                rumoca_core::Subscript::Expr { .. } | rumoca_core::Subscript::Colon { .. } => {
                    return Ok(None);
                }
                _ => {
                    return Err(unsupported_at(
                        "non-positive subscript is unsupported",
                        subscript.span(),
                    ));
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
                name,
                subscripts,
                span,
                ..
            } => {
                // MLS §3.7.5/§8.6: `pre(x)` lowers to the synthetic `__pre__.x`
                // parameter that *holds the previous-event value* of a discrete
                // (or state) variable. Folding it would pin a runtime array
                // subscript such as `wp[pre(k), 1]` to its start index, so a
                // `__pre__.` reference must take the runtime dynamic-selection
                // path instead of compile-time subscript folding.
                if name.as_str().starts_with("__pre__.") {
                    return false;
                }
                if subscripts.is_empty() && const_scope.contains_key(name.as_str()) {
                    return true;
                }
                if self
                    .dae_variables
                    .is_some_and(|variables| !var_ref_is_translation_constant(variables, name))
                {
                    return false;
                }
                compile_time_var_key(name, subscripts, const_scope, *span)
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

    pub(in crate::lower) fn eval_compile_time_range_values(
        &self,
        start: &rumoca_core::Expression,
        step: Option<&rumoca_core::Expression>,
        end: &rumoca_core::Expression,
        range_span: rumoca_core::Span,
        const_scope: &IndexMap<String, f64>,
        context: &str,
    ) -> Result<Vec<i64>, LowerError> {
        let step_span = step.and_then(rumoca_core::Expression::span);
        let start = self.eval_compile_time_int(start, const_scope, context)?;
        let end = self.eval_compile_time_int(end, const_scope, context)?;
        let step = match step {
            Some(step) => self.eval_compile_time_int(step, const_scope, context)?,
            None if end >= start => 1,
            None => -1,
        };
        if step == 0 {
            return Err(unsupported_at(
                format!("{context} step must be non-zero"),
                step_span.unwrap_or(range_span),
            ));
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
                .ok_or_else(|| unsupported_at(format!("{context} overflows i64"), range_span))?;
        }
        if (step > 0 && value <= end) || (step < 0 && value >= end) {
            return Err(unsupported_at(
                format!("{context} exceeds maximum lowered range length {MAX_STATIC_RANGE_VALUES}"),
                range_span,
            ));
        }
        Ok(values)
    }

    fn infer_cat_dims(
        &self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_span: rumoca_core::Span,
    ) -> Result<Vec<usize>, LowerError> {
        if args.len() <= 1 {
            return Ok(Vec::new());
        }
        let dim = self.eval_compile_time_positive_index(
            &args[0],
            &self.local_const_bindings,
            "cat dimension",
        )?;
        let span = args[0].span().unwrap_or(call_span);
        let mut operands = array_vec_with_capacity(args.len() - 1, "cat operand count", span)?;
        for arg in args.iter().skip(1) {
            operands.push(ArrayOperand {
                values: Vec::new(),
                dims: self.infer_expr_dims(arg, scope)?,
                shape_span: arg.span().unwrap_or(span),
            });
        }
        cat_array_dims(dim, &operands, span)
    }

    fn infer_fill_dims(
        &self,
        dims: &[rumoca_core::Expression],
        call_span: rumoca_core::Span,
    ) -> Result<Vec<usize>, LowerError> {
        let const_scope = &self.local_const_bindings;
        let span = dims
            .first()
            .and_then(rumoca_core::Expression::span)
            .unwrap_or(call_span);
        let mut values = array_vec_with_capacity(dims.len(), "fill dimension count", span)?;
        for dim in dims {
            let dim_value = self.eval_compile_time_int(dim, const_scope, "fill dimension")?;
            values.push(non_negative_fill_dimension(
                dim_value,
                dim.span().unwrap_or(call_span),
            )?);
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
        let span =
            self.required_inference_span_from_exprs(&[lhs, rhs], "binary dimension inference")?;
        Ok(match op {
            Op::Mul => multiplication_dims(&lhs_dims, &rhs_dims, span)?,
            Op::Add
            | Op::AddElem
            | Op::Sub
            | Op::SubElem
            | Op::MulElem
            | Op::DivElem
            | Op::ExpElem => broadcast_shape(&lhs_dims, &rhs_dims, span)?,
            Op::Div if rhs_dims.is_empty() => lhs_dims,
            Op::Div if lhs_dims.is_empty() => rhs_dims,
            _ => Vec::new(),
        })
    }
}

fn record_array_prefix_index(
    base_ref: &rumoca_core::ComponentReference,
    candidate_ref: &rumoca_core::ComponentReference,
) -> Result<Option<usize>, LowerError> {
    let prefix_len = base_ref.parts.len();
    if candidate_ref.parts.len() <= prefix_len || candidate_ref.local != base_ref.local {
        return Ok(None);
    }
    let mut index = None;
    for (base, candidate) in base_ref
        .parts
        .iter()
        .zip(candidate_ref.parts[..prefix_len].iter())
    {
        if base.ident != candidate.ident || !base.subs.is_empty() {
            return Ok(None);
        }
        match candidate.subs.as_slice() {
            [] => {}
            [subscript] if index.is_none() => {
                index = Some(static_positive_subscript_index(subscript)?);
            }
            _ => return Ok(None),
        }
    }
    Ok(index)
}

fn static_positive_subscript_index(
    subscript: &rumoca_core::Subscript,
) -> Result<usize, LowerError> {
    match subscript {
        rumoca_core::Subscript::Index { value, span } if *value > 0 => {
            crate::lower::helpers::positive_i64_index(*value, *span)
        }
        rumoca_core::Subscript::Index { span, .. } => Err(unsupported_at(
            "non-positive record-array aggregate index is unsupported",
            *span,
        )),
        rumoca_core::Subscript::Colon { span } | rumoca_core::Subscript::Expr { span, .. } => {
            Err(unsupported_at(
                "dynamic record-array aggregate index is unsupported in shape inference",
                *span,
            ))
        }
    }
}

fn var_ref_is_translation_constant(
    variables: &dae::DaeVariables,
    name: &rumoca_core::Reference,
) -> bool {
    let var_name = name.var_name();
    if variables.constants.contains_key(var_name) {
        return true;
    }
    if let Some(parameter) = variables.parameters.get(var_name) {
        return !parameter.is_tunable;
    }
    false
}

fn required_arg<'a>(
    args: &'a [rumoca_core::Expression],
    name: &str,
    call_span: rumoca_core::Span,
) -> Result<&'a rumoca_core::Expression, LowerError> {
    args.first().ok_or_else(|| {
        LowerError::contract_violation(format!("{name} requires at least one argument"), call_span)
    })
}

fn required_builtin_arg<'a>(
    args: &'a [rumoca_core::Expression],
    function: &rumoca_core::BuiltinFunction,
    index: usize,
    call_span: rumoca_core::Span,
) -> Result<&'a rumoca_core::Expression, LowerError> {
    args.get(index).ok_or_else(|| {
        LowerError::contract_violation(
            format!("{} requires argument {}", function.name(), index + 1),
            args.first()
                .and_then(rumoca_core::Expression::span)
                .unwrap_or(call_span),
        )
    })
}

fn fill_dimension_args(
    args: &[rumoca_core::Expression],
    call_span: rumoca_core::Span,
) -> Result<&[rumoca_core::Expression], LowerError> {
    if args.len() < 2 {
        return Err(LowerError::contract_violation(
            format!("fill requires at least 2 arguments, got {}", args.len()),
            args.first()
                .and_then(rumoca_core::Expression::span)
                .unwrap_or(call_span),
        ));
    }
    Ok(&args[1..])
}

fn scalar_singleton_projection(subscripts: &[rumoca_core::Subscript]) -> bool {
    !subscripts.is_empty()
        && subscripts.iter().all(|subscript| match subscript {
            rumoca_core::Subscript::Index { value, .. } => *value == 1,
            rumoca_core::Subscript::Colon { .. } => true,
            rumoca_core::Subscript::Expr { .. } => false,
        })
}

pub(super) fn concrete_i64_dims(
    dims: &[i64],
    name: &str,
    context: &str,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let mut concrete = array_vec_with_capacity(dims.len(), context, span)?;
    for dim in dims {
        concrete.push(usize::try_from(*dim).map_err(|_| {
            LowerError::contract_violation(
                format!("{context} for `{name}` contain unresolved negative dimension {dim}"),
                span,
            )
        })?);
    }
    Ok(concrete)
}

fn non_negative_fill_dimension(value: i64, span: rumoca_core::Span) -> Result<usize, LowerError> {
    if value < 0 {
        return Err(unsupported_at("fill dimension must be non-negative", span));
    }
    usize::try_from(value).map_err(|_| {
        LowerError::contract_violation(
            format!("fill dimension {value} exceeds host index range"),
            span,
        )
    })
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

#[cfg(test)]
mod tests {
    use super::*;

    fn literal_i64(value: i64, span: rumoca_core::Span) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            span,
        }
    }

    fn lower_builder<'a>(
        layout: &'a VarLayout,
        functions: &'a IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    ) -> LowerBuilder<'a> {
        LowerBuilder::new(layout, functions)
    }

    #[test]
    fn slice_selection_counts_reports_excess_subscripts_with_subscript_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("slice_excess.mo"),
            20,
            21,
        );
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let builder = lower_builder(&layout, &functions);
        let subscripts = [
            rumoca_core::Subscript::index(1, span),
            rumoca_core::Subscript::colon(span),
        ];

        let err = builder
            .slice_selection_counts(&subscripts, &[3], &Scope::new(), Some(span))
            .expect_err("excess subscripts must fail");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "array slice has more subscripts than dimensions"
        );
    }

    #[test]
    fn slice_subscript_count_reports_non_positive_subscript_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("slice_non_positive.mo"),
            8,
            9,
        );
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let builder = lower_builder(&layout, &functions);
        let subscript = rumoca_core::Subscript::index(0, span);

        let err = builder
            .slice_subscript_count(&subscript, 3, &Scope::new())
            .expect_err("zero subscript must fail");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(err.reason(), "non-positive subscript is unsupported");
    }

    #[test]
    fn range_slice_keeps_singleton_dimension_while_scalar_index_drops_it() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("singleton_slice.mo"),
            10,
            13,
        );
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let mut variables = dae::DaeVariables::default();
        variables.parameters.insert(
            rumoca_core::VarName::new("buf"),
            dae::Variable {
                dims: vec![2],
                ..dae::Variable::new(rumoca_core::VarName::new("buf"), span)
            },
        );
        let builder = LowerBuilder::new_with_metadata(
            &layout,
            &functions,
            LowerBuilderMetadata {
                dae_variables: Some(&variables),
                ..LowerBuilderMetadata::default()
            },
        );
        let range_slice = rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("buf"),
            subscripts: vec![
                rumoca_core::Subscript::try_generated_expr(
                    Box::new(rumoca_core::Expression::Range {
                        start: Box::new(literal_i64(1, span)),
                        step: None,
                        end: Box::new(literal_i64(1, span)),
                        span,
                    }),
                    span,
                    "singleton range test subscript",
                )
                .expect("range subscript should build"),
            ],
            span,
        };
        let scalar_index = rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("buf"),
            subscripts: vec![rumoca_core::Subscript::index(1, span)],
            span,
        };

        assert_eq!(
            builder
                .infer_expr_dims(&range_slice, &Scope::new())
                .expect("range slice dims should infer"),
            vec![1]
        );
        assert_eq!(
            builder
                .infer_expr_dims(&scalar_index, &Scope::new())
                .expect("scalar index dims should infer"),
            Vec::<usize>::new()
        );
    }

    #[test]
    fn range_dims_use_compile_time_size_expression_end() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("range_size_end.mo"),
            10,
            40,
        );
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let builder = lower_builder(&layout, &functions);
        let fill = rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Fill,
            args: vec![
                literal_i64(0, span),
                literal_i64(0, span),
                literal_i64(2, span),
            ],
            span,
        };
        let size = rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args: vec![fill, literal_i64(2, span)],
            span,
        };
        let range = rumoca_core::Expression::Range {
            start: Box::new(literal_i64(2, span)),
            step: None,
            end: Box::new(size.clone()),
            span,
        };

        assert_eq!(
            builder
                .eval_compile_time_range_values(
                    &literal_i64(2, span),
                    None,
                    &size,
                    span,
                    &IndexMap::new(),
                    "range dimension inference",
                )
                .expect("compile-time size range values should evaluate"),
            vec![2]
        );
        let range_size = rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args: vec![range, literal_i64(1, span)],
            span,
        };
        assert_eq!(
            builder
                .eval_compile_time_expr(&range_size, &IndexMap::new())
                .expect("size of compile-time range should evaluate"),
            1.0
        );
        let literal_size = rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args: vec![
                rumoca_core::Expression::Array {
                    elements: vec![literal_i64(0, span)],
                    is_matrix: false,
                    span,
                },
                literal_i64(1, span),
            ],
            span,
        };
        let max_size = rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Max,
            args: vec![rumoca_core::Expression::Array {
                elements: vec![
                    rumoca_core::Expression::Array {
                        elements: vec![range_size],
                        is_matrix: true,
                        span,
                    },
                    rumoca_core::Expression::Array {
                        elements: vec![literal_size],
                        is_matrix: true,
                        span,
                    },
                ],
                is_matrix: true,
                span,
            }],
            span,
        };
        let ones = rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Ones,
            args: vec![max_size],
            span,
        };
        assert_eq!(
            builder
                .infer_expr_dims(&ones, &Scope::new())
                .expect("ones(size(range, 1)) dims should infer"),
            vec![1]
        );
    }

    #[test]
    fn eval_compile_time_positive_index_reports_expr_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("index_positive.mo"),
            13,
            14,
        );
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let builder = lower_builder(&layout, &functions);
        let expr = literal_i64(0, span);

        let err = builder
            .eval_compile_time_positive_index(&expr, &IndexMap::new(), "array subscript")
            .expect_err("zero compile-time index must fail");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(err.reason(), "array subscript must be positive");
    }

    #[test]
    fn slice_expr_indices_reports_context_span_for_generated_expr() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("slice_generated_expr.mo"),
            5,
            8,
        );
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let builder = lower_builder(&layout, &functions);
        let expr = literal_i64(0, rumoca_core::Span::DUMMY);

        let err = builder
            .slice_expr_indices(&expr, 4, &Scope::new(), span)
            .expect_err("zero generated slice expression must fail");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(err.reason(), "array slice index must be positive");
    }

    #[test]
    fn builtin_dim_inference_missing_arg_uses_call_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("identity_empty_dims.mo"),
            4,
            14,
        );
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let builder = lower_builder(&layout, &functions);
        let expr = rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Identity,
            args: Vec::new(),
            span,
        };

        let err = builder
            .infer_expr_dims(&expr, &Scope::new())
            .expect_err("identity() dimension inference must require an argument");

        assert_eq!(err.source_span(), Some(span));
        assert!(
            err.reason().contains("identity requires argument 1"),
            "{err:?}"
        );
    }

    #[test]
    fn synchronous_dim_inference_missing_arg_uses_call_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("previous_empty_dims.mo"),
            2,
            12,
        );
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let builder = lower_builder(&layout, &functions);
        let expr = rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("previous"),
            args: Vec::new(),
            is_constructor: false,
            span,
        };

        let err = builder
            .infer_expr_dims(&expr, &Scope::new())
            .expect_err("previous() dimension inference must require an argument");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "invalid IR contract: previous requires at least one argument"
        );
    }

    #[test]
    fn synchronous_dim_inference_missing_arg_without_span_stays_unspanned() {
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let builder = lower_builder(&layout, &functions);
        let expr = rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("previous"),
            args: Vec::new(),
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        };

        let err = builder
            .infer_expr_dims(&expr, &Scope::new())
            .expect_err("previous() dimension inference must require an argument");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert_eq!(
            err.reason(),
            "invalid IR contract: previous requires at least one argument"
        );
    }

    #[test]
    fn fill_dim_inference_uses_call_span_when_dim_is_generated() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("fill_generated_dim.mo"),
            7,
            18,
        );
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let builder = lower_builder(&layout, &functions);
        let expr = rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Fill,
            args: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.0),
                    span: rumoca_core::Span::DUMMY,
                },
                literal_i64(-1, rumoca_core::Span::DUMMY),
            ],
            span,
        };

        let err = builder
            .infer_expr_dims(&expr, &Scope::new())
            .expect_err("negative generated fill dimension must fail");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(err.reason(), "fill dimension must be non-negative");
    }

    #[test]
    fn fill_dim_inference_missing_args_uses_call_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("fill_empty_dims.mo"),
            3,
            9,
        );
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let builder = lower_builder(&layout, &functions);
        let expr = rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Fill,
            args: Vec::new(),
            span,
        };

        let err = builder
            .infer_expr_dims(&expr, &Scope::new())
            .expect_err("fill() dimension inference must require value and dimension arguments");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "invalid IR contract: fill requires at least 2 arguments, got 0"
        );
    }

    #[test]
    fn fill_dim_inference_missing_args_without_span_stays_unspanned() {
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let builder = lower_builder(&layout, &functions);
        let expr = rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Fill,
            args: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        };

        let err = builder
            .infer_expr_dims(&expr, &Scope::new())
            .expect_err("fill() dimension inference must require value and dimension arguments");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert_eq!(
            err.reason(),
            "invalid IR contract: fill requires at least 2 arguments, got 0"
        );
    }

    #[test]
    fn concrete_i64_dims_rejects_negative_dim_without_fabricating_span() {
        let err = concrete_i64_dims(
            &[-1],
            "x",
            "array inference dimensions",
            rumoca_core::Span::DUMMY,
        )
        .expect_err("negative inferred dimensions must fail");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert_eq!(
            err.reason(),
            "invalid IR contract: array inference dimensions for `x` contain unresolved negative dimension -1"
        );
    }

    #[test]
    fn eval_compile_time_range_values_reports_zero_step_span() {
        let range_span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("range_zero_step.mo"),
            3,
            14,
        );
        let step_span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("range_zero_step.mo"),
            7,
            8,
        );
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let builder = lower_builder(&layout, &functions);
        let start = literal_i64(1, range_span);
        let step = literal_i64(0, step_span);
        let end = literal_i64(4, range_span);

        let err = builder
            .eval_compile_time_range_values(
                &start,
                Some(&step),
                &end,
                range_span,
                &IndexMap::new(),
                "array slice range",
            )
            .expect_err("zero range step must fail");

        assert_eq!(err.source_span(), Some(step_span));
        assert_eq!(err.reason(), "array slice range step must be non-zero");
    }

    #[test]
    fn non_negative_fill_dimension_rejects_negative_with_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_array_values_inference_source_41.mo",
            ),
            3,
            8,
        );

        let err =
            non_negative_fill_dimension(-1, span).expect_err("negative fill dimension must fail");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(err.reason(), "fill dimension must be non-negative");
    }

    #[test]
    fn non_negative_fill_dimension_rejects_host_overflow_with_span() {
        if usize::BITS >= i64::BITS {
            return;
        }
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_array_values_inference_source_42.mo",
            ),
            11,
            17,
        );

        let err = non_negative_fill_dimension(i64::MAX, span)
            .expect_err("oversized fill dimension must fail");

        assert_eq!(err.source_span(), Some(span));
        assert!(
            err.reason().contains("exceeds host index range"),
            "unexpected error: {err}"
        );
    }
}
