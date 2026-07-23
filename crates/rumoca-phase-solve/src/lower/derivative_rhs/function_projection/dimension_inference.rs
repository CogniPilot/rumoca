use super::compile_time::{compile_time_binary, compile_time_var_key, literal_to_f64};
use super::*;
use crate::projection_suffix::{
    output_projection_suffix, record_output_field_param, resolve_function_reference,
};

impl<'a> FunctionProjectionAnalysis<'a> {
    // SPEC_0021: Exception - function projection dimension inference keeps
    // call, field, array, and comprehension cases in one recursive walker.
    #[cfg(test)]
    pub(super) fn expr_dims(
        &self,
        expr: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
        depth: usize,
        owner_span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        let span = projection_arg_or_context_span(expr, owner_span)?;
        self.expr_dims_with_owner(expr, scope, depth, span)
    }

    // SPEC_0021: Exception - function projection dimension inference keeps
    // call, field, array, and comprehension cases in one recursive walker.
    #[allow(clippy::too_many_lines)]
    pub(super) fn expr_dims_with_owner(
        &self,
        expr: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
        depth: usize,
        owner_span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        let span = inherited_projection_source_span(expr.span(), owner_span);
        match expr {
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } => {
                let Some(base_dims) = self.variable_reference_dims(name, scope, depth + 1, span)?
                else {
                    return Ok(None);
                };
                self.subscripted_projection_dims(&base_dims, subscripts, scope, depth + 1, span)
                    .map(Some)
            }
            rumoca_core::Expression::Array {
                elements,
                is_matrix,
                ..
            } => self.array_expression_dims(elements, *is_matrix, scope, depth, span),
            rumoca_core::Expression::Range { .. } => {
                let count = self.for_index_values(expr, scope, span)?.len();
                copy_projection_dims(
                    &[checked_usize_to_i64(
                        count,
                        "function projection range length",
                        span,
                    )?],
                    "function projection range dimension count",
                    span,
                )
                .map(Some)
            }
            rumoca_core::Expression::Index {
                base, subscripts, ..
            } => {
                let Some(base_dims) = self.expr_dims_with_owner(base, scope, depth + 1, span)?
                else {
                    return Ok(None);
                };
                self.subscripted_projection_dims(&base_dims, subscripts, scope, depth + 1, span)
                    .map(Some)
            }
            rumoca_core::Expression::Literal { .. } => Ok(Some(Vec::new())),
            rumoca_core::Expression::Unary { rhs, .. } => {
                self.expr_dims_with_owner(rhs, scope, depth, span)
            }
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Der,
                args,
                ..
            } => match args.first() {
                Some(arg) => self.expr_dims_with_owner(arg, scope, depth, span),
                None => Ok(None),
            },
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Transpose,
                args,
                ..
            } => {
                let Some(arg) = args.first() else {
                    return Ok(None);
                };
                let Some(dims) = self.expr_dims_with_owner(arg, scope, depth, span)? else {
                    return Ok(None);
                };
                let [rows, cols] = dims.as_slice() else {
                    return Ok(None);
                };
                Ok(Some(copy_projection_dims(
                    &[*cols, *rows],
                    "transpose dimension count",
                    span,
                )?))
            }
            rumoca_core::Expression::BuiltinCall { function, args, .. } => {
                self.linear_algebra_builtin_dims(function, args, scope, depth, span)
            }
            rumoca_core::Expression::FunctionCall {
                name,
                is_constructor: false,
                ..
            } => self.function_call_expr_dims(name, expr, scope, span, depth),
            rumoca_core::Expression::FieldAccess { base, field, .. } => {
                if let Some(dims) = self.bound_field_access_dims(base, field, scope, span, depth)? {
                    return Ok(Some(dims));
                }
                self.function_field_access_dims(base, field, scope, span, depth)
            }
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => self.if_expression_dims(branches, else_branch, scope, depth, span),
            rumoca_core::Expression::Binary { op, lhs, rhs, .. } if is_mul(op) => {
                let Some(lhs_dims) = self.expr_dims_with_owner(lhs, scope, depth, span)? else {
                    return Ok(None);
                };
                let Some(rhs_dims) = self.expr_dims_with_owner(rhs, scope, depth, span)? else {
                    return Ok(None);
                };
                binary_mul_dims(&lhs_dims, &rhs_dims, span)
            }
            rumoca_core::Expression::Binary { op, lhs, rhs, .. } if is_add(op) || is_sub(op) => {
                let Some(lhs_dims) = self.expr_dims_with_owner(lhs, scope, depth, span)? else {
                    return Ok(None);
                };
                let Some(rhs_dims) = self.expr_dims_with_owner(rhs, scope, depth, span)? else {
                    return Ok(None);
                };
                elementwise_binary_dims(&lhs_dims, &rhs_dims, span)
            }
            rumoca_core::Expression::Binary { op, lhs, rhs, .. } if is_div(op) => {
                let Some(lhs_dims) = self.expr_dims_with_owner(lhs, scope, depth, span)? else {
                    return Ok(None);
                };
                let Some(rhs_dims) = self.expr_dims_with_owner(rhs, scope, depth, span)? else {
                    return Ok(None);
                };
                elementwise_binary_dims(&lhs_dims, &rhs_dims, span)
            }
            _ => Ok(None),
        }
    }

    fn variable_reference_dims(
        &self,
        name: &rumoca_core::Reference,
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        if name.as_str() == "time" {
            return Ok(Some(Vec::new()));
        }
        if let Some(dims) = scope.dims.get(name.as_str()) {
            return copy_projection_dims(dims, "projected scope dimension count", span).map(Some);
        }
        if let Some(values) = scope.scalars.get(name.as_str()) {
            return Self::projected_scalar_values_dims(values, span).map(Some);
        }
        if let Some(expr) = scope.full.get(name.as_str())
            && !is_same_plain_var_ref(expr, name.as_str())
        {
            return self.expr_dims_with_owner(expr, scope, depth + 1, span);
        }
        if let Some(variable) = variable_by_name(self.dae_model, name.as_str()) {
            return variable_dims_i64(variable, span).map(Some);
        }
        if scalarized_aggregate_binding(self.dae_model, name, span)?.is_some() {
            return Ok(Some(Vec::new()));
        }
        Ok(None)
    }

    pub(super) fn resolved_function_param_dims(
        &self,
        param: &rumoca_core::FunctionParam,
        scope: &FunctionProjectionScope,
        _depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        if param.dims.is_empty() || param.dims.iter().all(|dim| *dim > 0) {
            return copy_projection_dims(
                &param.dims,
                "resolved function parameter dimension count",
                span,
            )
            .map(Some);
        }
        if param.shape_expr.len() != param.dims.len() {
            return Ok(None);
        }
        let mut dims = projection_vec_with_capacity(
            param.shape_expr.len(),
            "resolved function parameter dimension count",
            span,
        )?;
        for subscript in &param.shape_expr {
            let dim = match subscript {
                rumoca_core::Subscript::Index { value, .. } if *value >= 0 => *value,
                rumoca_core::Subscript::Expr {
                    expr,
                    span: subscript_span,
                } => self.compile_time_int(
                    expr,
                    scope,
                    inherited_projection_span(*subscript_span, span),
                )?,
                _ => return Ok(None),
            };
            if dim < 0 {
                return Err(LowerError::contract_violation(
                    format!(
                        "function parameter `{}` has negative dimension {dim}",
                        param.name
                    ),
                    span,
                ));
            }
            dims.push(dim);
        }
        Ok(Some(dims))
    }

    pub(super) fn subscripted_projection_dims(
        &self,
        base_dims: &[i64],
        subscripts: &[rumoca_core::Subscript],
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Vec<i64>, LowerError> {
        if subscripts.len() > base_dims.len() {
            return Err(LowerError::contract_violation(
                format!(
                    "array selection has {} subscripts for dimensions {}",
                    subscripts.len(),
                    crate::lower::helpers::format_i64_dims(base_dims)
                ),
                span,
            ));
        }
        let mut selected = projection_vec_with_capacity(
            base_dims.len(),
            "selected expression dimension count",
            span,
        )?;
        for (axis, subscript) in subscripts.iter().enumerate() {
            if let Some(dim) = self.projected_subscript_dimension(
                base_dims[axis],
                subscript,
                scope,
                depth + 1,
                span,
            )? {
                selected.push(dim);
            }
        }
        selected.extend(base_dims.iter().skip(subscripts.len()).copied());
        Ok(selected)
    }

    fn projected_subscript_dimension(
        &self,
        base_dim: i64,
        subscript: &rumoca_core::Subscript,
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Option<i64>, LowerError> {
        match subscript {
            rumoca_core::Subscript::Colon { .. } => Ok(Some(base_dim)),
            rumoca_core::Subscript::Index {
                value,
                span: subscript_span,
            } => validate_scalar_projection_subscript(
                *value,
                base_dim,
                inherited_projection_span(*subscript_span, span),
            ),
            rumoca_core::Subscript::Expr {
                expr,
                span: subscript_span,
            } => {
                let span = inherited_projection_span(*subscript_span, span);
                let dims = self
                    .expr_dims_with_owner(expr, scope, depth + 1, span)?
                    .ok_or_else(|| {
                        unsupported_at("array subscript has unknown dimensions", span)
                    })?;
                projection_subscript_result_dimension(&dims, span)
            }
        }
    }

    fn if_expression_dims(
        &self,
        branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
        else_branch: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        let Some(dims) = self.expr_dims_with_owner(else_branch, scope, depth + 1, span)? else {
            return Ok(None);
        };
        for (_, value) in branches {
            let Some(branch_dims) = self.expr_dims_with_owner(value, scope, depth + 1, span)?
            else {
                return Ok(None);
            };
            if branch_dims != dims {
                return Ok(None);
            }
        }
        Ok(Some(dims))
    }

    fn linear_algebra_builtin_dims(
        &self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        match function {
            rumoca_core::BuiltinFunction::Zeros | rumoca_core::BuiltinFunction::Ones => {
                self.dimension_argument_builtin_dims(args, scope, span)
            }
            rumoca_core::BuiltinFunction::Fill => {
                self.dimension_argument_builtin_dims(args.get(1..).unwrap_or_default(), scope, span)
            }
            rumoca_core::BuiltinFunction::Identity => {
                let Some(size) = args.first() else {
                    return Ok(None);
                };
                let size = self.compile_time_int(size, scope, span)?;
                copy_projection_dims(&[size, size], "identity matrix dimension count", span)
                    .map(Some)
            }
            rumoca_core::BuiltinFunction::Cross => {
                copy_projection_dims(&[3], "cross product dimension count", span).map(Some)
            }
            rumoca_core::BuiltinFunction::Skew => {
                copy_projection_dims(&[3, 3], "skew matrix dimension count", span).map(Some)
            }
            rumoca_core::BuiltinFunction::Diagonal => {
                self.diagonal_builtin_dims(args, scope, depth, span)
            }
            rumoca_core::BuiltinFunction::OuterProduct => {
                self.outer_product_builtin_dims(args, scope, depth, span)
            }
            rumoca_core::BuiltinFunction::Symmetric => {
                self.symmetric_builtin_dims(args, scope, depth, span)
            }
            rumoca_core::BuiltinFunction::Size => self.size_builtin_dims(args, scope, depth, span),
            rumoca_core::BuiltinFunction::Ndims
            | rumoca_core::BuiltinFunction::Scalar
            | rumoca_core::BuiltinFunction::Sum
            | rumoca_core::BuiltinFunction::Product
            | rumoca_core::BuiltinFunction::Div
            | rumoca_core::BuiltinFunction::Mod
            | rumoca_core::BuiltinFunction::Rem
            | rumoca_core::BuiltinFunction::Min
            | rumoca_core::BuiltinFunction::Max
            | rumoca_core::BuiltinFunction::Atan2
            | rumoca_core::BuiltinFunction::Initial
            | rumoca_core::BuiltinFunction::Terminal
            | rumoca_core::BuiltinFunction::Edge
            | rumoca_core::BuiltinFunction::Change
            | rumoca_core::BuiltinFunction::Reinit => Ok(Some(Vec::new())),
            rumoca_core::BuiltinFunction::Smooth => self.argument_dims(args, 1, scope, depth, span),
            function if projection_builtin_preserves_first_arg(*function) => {
                self.argument_dims(args, 0, scope, depth, span)
            }
            _ => Ok(None),
        }
    }

    fn argument_dims(
        &self,
        args: &[rumoca_core::Expression],
        index: usize,
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        let Some(arg) = args.get(index) else {
            return Ok(None);
        };
        self.expr_dims_with_owner(arg, scope, depth + 1, span)
    }

    fn size_builtin_dims(
        &self,
        args: &[rumoca_core::Expression],
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        if args.len() > 1 {
            return Ok(Some(Vec::new()));
        }
        let Some(array) = args.first() else {
            return Ok(None);
        };
        let Some(array_dims) = self.expr_dims_with_owner(array, scope, depth + 1, span)? else {
            return Ok(None);
        };
        copy_projection_dims(
            &[checked_usize_to_i64(
                array_dims.len(),
                "size() result length",
                span,
            )?],
            "size() result dimension count",
            span,
        )
        .map(Some)
    }

    fn dimension_argument_builtin_dims(
        &self,
        args: &[rumoca_core::Expression],
        scope: &FunctionProjectionScope,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        if args.is_empty() {
            return Ok(None);
        }
        let mut dims =
            projection_vec_with_capacity(args.len(), "array constructor dimension count", span)?;
        for arg in args {
            let dim = self.compile_time_int(arg, scope, span)?;
            if dim < 0 {
                return Err(LowerError::contract_violation(
                    format!("array constructor dimension must be non-negative, got {dim}"),
                    span,
                ));
            }
            dims.push(dim);
        }
        Ok(Some(dims))
    }

    fn diagonal_builtin_dims(
        &self,
        args: &[rumoca_core::Expression],
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        let Some(arg) = args.first() else {
            return Ok(None);
        };
        let Some(dims) = self.expr_dims_with_owner(arg, scope, depth + 1, span)? else {
            return Ok(None);
        };
        let [len] = dims.as_slice() else {
            return Ok(None);
        };
        copy_projection_dims(&[*len, *len], "diagonal matrix dimension count", span).map(Some)
    }

    fn outer_product_builtin_dims(
        &self,
        args: &[rumoca_core::Expression],
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        let (Some(lhs), Some(rhs)) = (args.first(), args.get(1)) else {
            return Ok(None);
        };
        let Some(lhs_dims) = self.expr_dims_with_owner(lhs, scope, depth + 1, span)? else {
            return Ok(None);
        };
        let Some(rhs_dims) = self.expr_dims_with_owner(rhs, scope, depth + 1, span)? else {
            return Ok(None);
        };
        let ([rows], [cols]) = (lhs_dims.as_slice(), rhs_dims.as_slice()) else {
            return Ok(None);
        };
        copy_projection_dims(
            &[*rows, *cols],
            "outer product matrix dimension count",
            span,
        )
        .map(Some)
    }

    fn symmetric_builtin_dims(
        &self,
        args: &[rumoca_core::Expression],
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        let Some(arg) = args.first() else {
            return Ok(None);
        };
        let Some(dims) = self.expr_dims_with_owner(arg, scope, depth + 1, span)? else {
            return Ok(None);
        };
        match dims.as_slice() {
            [rows, cols] if rows == cols => {
                copy_projection_dims(&dims, "symmetric matrix dimension count", span).map(Some)
            }
            _ => Ok(None),
        }
    }

    fn array_expression_dims(
        &self,
        elements: &[rumoca_core::Expression],
        is_matrix: bool,
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        let child_dims = elements
            .iter()
            .map(|element| self.expr_dims_with_owner(element, scope, depth + 1, span))
            .collect::<Result<Vec<_>, _>>()?;
        array_expression_dims(elements, is_matrix, &child_dims, span)
    }

    pub(super) fn known_expr_dims(
        &self,
        expr: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
        depth: usize,
        context: &str,
        fallback_span: rumoca_core::Span,
    ) -> Result<Vec<i64>, LowerError> {
        let Some(dims) = self.expr_dims_with_owner(expr, scope, depth, fallback_span)? else {
            let span = if fallback_span.is_dummy() {
                projection_arg_or_context_span(expr, fallback_span)?
            } else {
                fallback_span
            };
            return Err(unsupported_at(
                format!("{context} has unknown dimensions"),
                span,
            ));
        };
        Ok(dims)
    }

    fn function_field_access_dims(
        &self,
        base: &rumoca_core::Expression,
        field: &str,
        scope: &FunctionProjectionScope,
        span: rumoca_core::Span,
        depth: usize,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        if !matches!(base, rumoca_core::Expression::FunctionCall { .. }) {
            return Ok(None);
        }
        let call = self.substitute_for_call(base, scope)?;
        let Some(outputs) = self.function_call_outputs_with_owner(&call, depth + 1, span)? else {
            return Ok(None);
        };
        projected_field_output_dims(&outputs, field, span)
    }

    fn bound_field_access_dims(
        &self,
        base: &rumoca_core::Expression,
        field: &str,
        scope: &FunctionProjectionScope,
        span: rumoca_core::Span,
        depth: usize,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        let Ok(key) = field_access_binding_key(base, field) else {
            return Ok(None);
        };
        if let Some(dims) = scope.dims.get(key.as_str()) {
            return copy_projection_dims(dims, "field access scope dimension count", span)
                .map(Some);
        }
        if let Some(values) = scope.scalars.get(key.as_str()) {
            return Self::projected_scalar_values_dims(values, span).map(Some);
        }
        if let Some(expr) = scope.full.get(key.as_str())
            && !is_same_plain_var_ref(expr, key.as_str())
        {
            return self.expr_dims_with_owner(expr, scope, depth + 1, span);
        }
        variable_by_name(self.dae_model, key.as_str())
            .map(|variable| variable_dims_i64(variable, span))
            .transpose()
    }

    fn projected_scalar_values_dims(
        values: &[rumoca_core::Expression],
        span: rumoca_core::Span,
    ) -> Result<Vec<i64>, LowerError> {
        match values.len() {
            0 | 1 => Ok(Vec::new()),
            len => copy_projection_dims(
                &[checked_usize_to_i64(
                    len,
                    "projected scalar value count",
                    span,
                )?],
                "projected scalar dimension count",
                span,
            ),
        }
    }

    fn function_call_expr_dims(
        &self,
        name: &rumoca_core::Reference,
        expr: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
        owner_span: rumoca_core::Span,
        depth: usize,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        let rumoca_core::Expression::FunctionCall { span, .. } = expr else {
            return Ok(None);
        };
        let call_span = inherited_projection_span(*span, owner_span);
        if let Some(dims) = self.declared_function_output_dims(name, call_span)? {
            return Ok(Some(dims));
        }
        let call = self.substitute_for_call(expr, scope)?;
        let Some(outputs) = self.function_call_outputs_with_owner(&call, depth + 1, call_span)?
        else {
            return Ok(None);
        };
        if let Some(dims) = self.projected_call_expr_dims(name, &outputs, call_span)? {
            return Ok(Some(dims));
        }
        function_outputs_dims(outputs.len(), call_span).map(Some)
    }

    fn projected_call_expr_dims(
        &self,
        requested: &rumoca_core::Reference,
        outputs: &[ProjectedFunctionOutput],
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        let Some((_, function)) =
            resolve_function_reference(&self.dae_model.symbols.functions, requested)
        else {
            return Ok(None);
        };
        let projection = output_projection_suffix(function, requested);
        let output_name = projection
            .as_ref()
            .map(|projection| projection.output_name.as_str())
            .or_else(|| function.outputs.first().map(|output| output.name.as_str()));
        let Some(output_name) = output_name else {
            return Ok(None);
        };
        let field_path = projection
            .as_ref()
            .map(|projection| projection.output_fields.as_slice())
            .unwrap_or_default();
        let selector_indices = outputs
            .iter()
            .filter(|output| {
                output.output_name.as_deref() == Some(output_name)
                    && output.field_path == field_path
            })
            .map(|output| output.selector_indices.as_slice())
            .collect::<Vec<_>>();
        selector_dims_from_indices(&selector_indices, span)
    }

    fn declared_function_output_dims(
        &self,
        name: &rumoca_core::Reference,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        if let Some((function_name, function)) =
            resolve_function_reference(&self.dae_model.symbols.functions, name)
            && name.var_name() == function_name
        {
            return exact_declared_function_output_dims(function, span);
        }
        self.projected_declared_function_output_dims(name, span)
    }

    fn projected_declared_function_output_dims(
        &self,
        requested: &rumoca_core::Reference,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        let Some((_, function)) =
            resolve_function_reference(&self.dae_model.symbols.functions, requested)
        else {
            return Ok(None);
        };
        let Some(projection_suffix) = output_projection_suffix(function, requested) else {
            return Ok(None);
        };
        let Some(output) = function
            .outputs
            .iter()
            .find(|output| output.name == projection_suffix.output_name)
        else {
            return Ok(None);
        };
        let projected_output = match projection_suffix.output_fields.as_slice() {
            [field] => {
                match record_output_field_param(
                    &self.dae_model.symbols.functions,
                    output,
                    &projection_suffix.output_fields,
                ) {
                    Some(field_output) => field_output,
                    None if rumoca_core::qualified_type_name_matches(
                        &output.type_name,
                        "Complex",
                    ) && matches!(field.as_str(), "re" | "im") =>
                    {
                        output
                    }
                    None => return Ok(None),
                }
            }
            [] => output,
            _ => match record_output_field_param(
                &self.dae_model.symbols.functions,
                output,
                &projection_suffix.output_fields,
            ) {
                Some(field_output) => field_output,
                None => return Ok(None),
            },
        };
        projected_declared_output_dims(projected_output, &projection_suffix.indices, span)
    }

    pub(super) fn reference_with_dae_component_ref(
        &self,
        name: &rumoca_core::Reference,
    ) -> rumoca_core::Reference {
        if name
            .component_ref()
            .is_some_and(|component_ref| component_ref.def_id.is_some())
        {
            return name.clone();
        }
        self.dae_variable_component_ref(name.var_name())
            .map(|component_ref| {
                rumoca_core::Reference::with_component_reference(name.as_str(), component_ref)
            })
            .unwrap_or_else(|| name.clone())
    }

    fn dae_variable_component_ref(
        &self,
        name: &rumoca_core::VarName,
    ) -> Option<rumoca_core::ComponentReference> {
        self.dae_model
            .variables
            .states
            .get(name)
            .or_else(|| self.dae_model.variables.algebraics.get(name))
            .or_else(|| self.dae_model.variables.outputs.get(name))
            .or_else(|| self.dae_model.variables.parameters.get(name))
            .or_else(|| self.dae_model.variables.inputs.get(name))
            .or_else(|| self.dae_model.variables.discrete_reals.get(name))
            .or_else(|| self.dae_model.variables.discrete_valued.get(name))
            .or_else(|| self.dae_model.variables.constants.get(name))
            .and_then(|var| var.component_ref.clone())
    }

    pub(super) fn compile_time_if_selection<'expr>(
        &self,
        branches: &'expr [(rumoca_core::Expression, rumoca_core::Expression)],
        else_branch: &'expr rumoca_core::Expression,
        scope: &FunctionProjectionScope,
    ) -> Result<Option<&'expr rumoca_core::Expression>, LowerError> {
        for (condition, branch) in branches {
            let condition = self.substitute(condition, scope)?;
            let Some(value) = self.compile_time_scalar(&condition) else {
                return Ok(None);
            };
            if value != 0.0 {
                return Ok(Some(branch));
            }
        }
        Ok(Some(else_branch))
    }

    pub(super) fn compile_time_scalar(&self, expr: &rumoca_core::Expression) -> Option<f64> {
        match expr {
            rumoca_core::Expression::Literal { value, .. } => literal_to_f64(value),
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } => {
                let key = compile_time_var_key(name, subscripts)?;
                self.structural_bindings.get(key.as_str()).copied()
            }
            rumoca_core::Expression::Unary { op, rhs, .. } => {
                let value = self.compile_time_scalar(rhs)?;
                match op {
                    rumoca_core::OpUnary::Plus
                    | rumoca_core::OpUnary::DotPlus
                    | rumoca_core::OpUnary::Empty => Some(value),
                    rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => Some(-value),
                    rumoca_core::OpUnary::Not => Some(f64::from(value == 0.0)),
                }
            }
            rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
                let lhs = self.compile_time_scalar(lhs)?;
                let rhs = self.compile_time_scalar(rhs)?;
                compile_time_binary(op, lhs, rhs)
            }
            _ => None,
        }
    }
}

fn projection_builtin_preserves_first_arg(function: rumoca_core::BuiltinFunction) -> bool {
    matches!(
        function,
        rumoca_core::BuiltinFunction::Der
            | rumoca_core::BuiltinFunction::Pre
            | rumoca_core::BuiltinFunction::Abs
            | rumoca_core::BuiltinFunction::Sign
            | rumoca_core::BuiltinFunction::Sqrt
            | rumoca_core::BuiltinFunction::Floor
            | rumoca_core::BuiltinFunction::Ceil
            | rumoca_core::BuiltinFunction::Integer
            | rumoca_core::BuiltinFunction::Sin
            | rumoca_core::BuiltinFunction::Cos
            | rumoca_core::BuiltinFunction::Tan
            | rumoca_core::BuiltinFunction::Asin
            | rumoca_core::BuiltinFunction::Acos
            | rumoca_core::BuiltinFunction::Atan
            | rumoca_core::BuiltinFunction::Sinh
            | rumoca_core::BuiltinFunction::Cosh
            | rumoca_core::BuiltinFunction::Tanh
            | rumoca_core::BuiltinFunction::Exp
            | rumoca_core::BuiltinFunction::Log
            | rumoca_core::BuiltinFunction::Log10
            | rumoca_core::BuiltinFunction::Sample
            | rumoca_core::BuiltinFunction::NoEvent
            | rumoca_core::BuiltinFunction::Homotopy
            | rumoca_core::BuiltinFunction::SemiLinear
            | rumoca_core::BuiltinFunction::Delay
    )
}

fn validate_scalar_projection_subscript(
    value: i64,
    dimension: i64,
    span: rumoca_core::Span,
) -> Result<Option<i64>, LowerError> {
    if value <= 0 || value > dimension {
        return Err(unsupported_at(
            format!("array subscript {value} is outside dimension {dimension}"),
            span,
        ));
    }
    Ok(None)
}

fn projection_subscript_result_dimension(
    dimensions: &[i64],
    span: rumoca_core::Span,
) -> Result<Option<i64>, LowerError> {
    match dimensions {
        [] => Ok(None),
        [dimension] => Ok(Some(*dimension)),
        _ => Err(unsupported_at(
            "array subscript expression must be scalar or one-dimensional",
            span,
        )),
    }
}
