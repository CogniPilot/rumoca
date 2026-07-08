use super::compile_time::{compile_time_binary, compile_time_var_key, literal_to_f64};
use super::*;
use crate::lower::is_stream_passthrough_intrinsic;
use crate::projection_suffix::parse_output_projection_suffix;

fn merge_vectorized_scalar_actual_dims(
    dims: &mut Option<Vec<i64>>,
    actual_dims: &[i64],
    span: rumoca_core::Span,
) -> Result<bool, LowerError> {
    if actual_dims == [1] && dims.as_ref().is_some_and(|dims| dims.as_slice() != [1]) {
        return Ok(true);
    }
    if dims
        .as_ref()
        .is_some_and(|dims| dims.as_slice() == [1] && actual_dims != [1])
    {
        *dims = Some(actual_dims.to_vec());
        return Ok(true);
    }
    let Some(merged) = elementwise_binary_dims(dims.as_deref().unwrap_or(&[]), actual_dims, span)?
    else {
        return Ok(false);
    };
    *dims = Some(merged);
    Ok(true)
}

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
        if depth > super::super::super::MAX_FUNCTION_INLINE_DEPTH {
            return Ok(None);
        }
        let span = inherited_projection_source_span(expr.span(), owner_span);
        match expr {
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => self.unsubscripted_var_ref_dims(name, scope, depth, span),
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } => self.subscripted_var_ref_dims(name, subscripts, scope, depth, span),
            rumoca_core::Expression::Array {
                elements,
                is_matrix,
                ..
            } => self.array_expression_dims(elements, *is_matrix, scope, depth, span),
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
                function: rumoca_core::BuiltinFunction::Sum,
                ..
            } => Ok(Some(Vec::new())),
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Cat,
                args,
                ..
            } => self.cat_expr_dims(args, scope, depth, span),
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => self.if_expr_dims(branches, else_branch, scope, depth, span),
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
                self.builtin_call_dims(function, args, scope, depth, span)
            }
            rumoca_core::Expression::FunctionCall {
                name,
                args,
                is_constructor: false,
                ..
            } if is_stream_passthrough_intrinsic(name.as_str()) => match args.first() {
                Some(arg) => self.expr_dims_with_owner(arg, scope, depth, span),
                None => Ok(None),
            },
            rumoca_core::Expression::FunctionCall {
                name,
                is_constructor: false,
                ..
            } => self.function_call_expr_dims(name, expr, scope, span, depth),
            rumoca_core::Expression::FieldAccess { base, field, .. } => {
                if let Some(dims) = self.bound_field_access_dims(base, field, scope, span, depth)? {
                    return Ok(Some(dims));
                }
                self.function_field_access_dims(base, field, span, depth)
            }
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
            rumoca_core::Expression::Binary { op, lhs, rhs, .. }
                if is_elementwise_binary_projection_op(op) =>
            {
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

    fn builtin_call_dims(
        &self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        if let Some(dims) = self.linear_algebra_builtin_dims(function, args, scope, depth, span)? {
            return Ok(Some(dims));
        }
        if matches!(
            function,
            rumoca_core::BuiltinFunction::Ones | rumoca_core::BuiltinFunction::Zeros
        ) {
            return self.array_constructor_dims(args, scope, span);
        }
        if matches!(function, rumoca_core::BuiltinFunction::Fill) {
            return self.array_constructor_dims(args.get(1..).unwrap_or(&[]), scope, span);
        }
        if matches!(function, rumoca_core::BuiltinFunction::Homotopy) {
            let Some(actual) = args.first() else {
                return Ok(None);
            };
            return self.expr_dims_with_owner(actual, scope, depth + 1, span);
        }
        if is_elementwise_builtin_projection(function) {
            return self.elementwise_builtin_dims(args, scope, depth, span);
        }
        Ok(None)
    }

    fn array_constructor_dims(
        &self,
        args: &[rumoca_core::Expression],
        scope: &FunctionProjectionScope,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        let mut dims =
            projection_vec_with_capacity(args.len(), "array constructor dimension count", span)?;
        for arg in args {
            let Some(value) = self.compile_time_scalar_in_scope(arg, scope)? else {
                return Ok(None);
            };
            if value < 0.0 || value.fract() != 0.0 || value > i64::MAX as f64 {
                return Err(LowerError::contract_violation(
                    format!("array constructor dimension `{value}` is not a valid integer"),
                    span,
                ));
            }
            dims.push(value as i64);
        }
        Ok(Some(dims))
    }

    fn elementwise_builtin_dims(
        &self,
        args: &[rumoca_core::Expression],
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        let mut dims = Vec::new();
        for arg in args {
            let Some(arg_dims) = self.expr_dims_with_owner(arg, scope, depth + 1, span)? else {
                return Ok(None);
            };
            let Some(merged) = elementwise_binary_dims(&dims, &arg_dims, span)? else {
                return Ok(None);
            };
            dims = merged;
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
            _ => Ok(None),
        }
    }

    fn unsubscripted_var_ref_dims(
        &self,
        name: &rumoca_core::Reference,
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        if let Some(dims) = scope.dims.get(name.as_str()) {
            return Ok(Some(copy_projection_dims(
                dims,
                "projected scope dimension count",
                span,
            )?));
        }
        if let Some(values) = scope.scalars.get(name.as_str()) {
            return Ok(Some(match values.len() {
                0 | 1 => Vec::new(),
                len => copy_projection_dims(
                    &[checked_usize_to_i64(
                        len,
                        "projected scalar value count",
                        span,
                    )?],
                    "projected scalar dimension count",
                    span,
                )?,
            }));
        }
        if let Some(expr) = scope.full.get(name.as_str())
            && !is_same_plain_var_ref(expr, name.as_str())
        {
            return self.expr_dims_with_owner(expr, scope, depth + 1, span);
        }
        variable_by_name(self.dae_model, name.as_str())
            .map(|variable| variable_dims_i64(variable, span))
            .transpose()
    }

    fn subscripted_var_ref_dims(
        &self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        let Some(base_dims) = self.unsubscripted_var_ref_dims(name, scope, depth + 1, span)? else {
            return Ok(None);
        };
        if base_dims.is_empty() {
            return Ok(None);
        }
        self.subscripted_dims(&base_dims, subscripts, scope, span)
    }

    fn subscripted_dims(
        &self,
        base_dims: &[i64],
        subscripts: &[rumoca_core::Subscript],
        scope: &FunctionProjectionScope,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        if subscripts.len() > base_dims.len() {
            return Err(LowerError::contract_violation(
                "subscripted expression has more subscripts than inferred dimensions",
                span,
            ));
        }
        let mut dims = projection_vec_with_capacity(
            base_dims.len(),
            "projected subscript dimension count",
            span,
        )?;
        for (dim, subscript) in base_dims.iter().copied().zip(subscripts) {
            if let Some(count) = self.subscript_preserved_dim_count(dim, subscript, scope, span)? {
                dims.push(count);
            }
        }
        dims.extend_from_slice(&base_dims[subscripts.len()..]);
        Ok(Some(dims))
    }

    fn subscript_preserved_dim_count(
        &self,
        dim: i64,
        subscript: &rumoca_core::Subscript,
        scope: &FunctionProjectionScope,
        span: rumoca_core::Span,
    ) -> Result<Option<i64>, LowerError> {
        match subscript {
            rumoca_core::Subscript::Colon { .. } => Ok(Some(dim)),
            rumoca_core::Subscript::Index { .. } => Ok(None),
            rumoca_core::Subscript::Expr { expr, .. } => {
                if let rumoca_core::Expression::Range {
                    start, step, end, ..
                } = expr.as_ref()
                {
                    return self.range_subscript_dim_count(
                        start,
                        step.as_deref(),
                        end,
                        scope,
                        span,
                    );
                }
                Ok(None)
            }
        }
    }

    fn range_subscript_dim_count(
        &self,
        start: &rumoca_core::Expression,
        step: Option<&rumoca_core::Expression>,
        end: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
        span: rumoca_core::Span,
    ) -> Result<Option<i64>, LowerError> {
        let Some(start) = self.compile_time_scalar_in_scope(start, scope)? else {
            return Ok(None);
        };
        let step = match step {
            Some(step) => {
                let Some(step) = self.compile_time_scalar_in_scope(step, scope)? else {
                    return Ok(None);
                };
                step
            }
            None => 1.0,
        };
        let Some(end) = self.compile_time_scalar_in_scope(end, scope)? else {
            return Ok(None);
        };
        if step == 0.0 {
            return Err(LowerError::contract_violation(
                "array slice range step must be non-zero",
                span,
            ));
        }
        let span_len = end - start;
        if span_len == 0.0 {
            return Ok(Some(1));
        }
        if span_len.signum() != step.signum() {
            return Ok(Some(0));
        }
        let count = (span_len / step).floor() + 1.0;
        if !count.is_finite() || count < 0.0 || count > i64::MAX as f64 {
            return Err(LowerError::contract_violation(
                "array slice range length exceeds i64",
                span,
            ));
        }
        Ok(Some(count as i64))
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
            if let Some(dims) = self.scope_projected_expr_dims(expr, scope, fallback_span)? {
                return Ok(dims);
            }
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

    pub(super) fn scope_projected_expr_dims(
        &self,
        expr: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        let mut dims = None;
        collect_scope_projected_expr_dims(expr, scope, &mut dims, span)?;
        Ok(dims)
    }

    fn function_field_access_dims(
        &self,
        base: &rumoca_core::Expression,
        field: &str,
        span: rumoca_core::Span,
        depth: usize,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        if !matches!(base, rumoca_core::Expression::FunctionCall { .. }) {
            return Ok(None);
        }
        let Some(outputs) = self.function_call_outputs_with_owner(base, depth + 1, span)? else {
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
        let rumoca_core::Expression::FunctionCall { args, span, .. } = expr else {
            return Ok(None);
        };
        let call_span = inherited_projection_span(*span, owner_span);
        if let Some(dims) =
            self.declared_function_output_dims(name, args, scope, call_span, depth)?
        {
            return Ok(Some(dims));
        }
        let Some(outputs) = self.function_call_outputs_with_owner(expr, depth + 1, call_span)?
        else {
            return Ok(Some(Vec::new()));
        };
        function_outputs_dims(outputs.len(), call_span).map(Some)
    }

    fn declared_function_output_dims(
        &self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        scope: &FunctionProjectionScope,
        span: rumoca_core::Span,
        depth: usize,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        if let Some(function) = self.dae_model.symbols.functions.get(name.var_name()) {
            let Some(dims) = exact_declared_function_output_dims(function, span)? else {
                return Ok(None);
            };
            if !dims.is_empty() {
                return Ok(Some(dims));
            }
            return self
                .vectorized_scalar_function_call_dims(function, args, scope, span, depth)?
                .map_or_else(|| Ok(Some(dims)), |dims| Ok(Some(dims)));
        }
        self.projected_declared_function_output_dims(name.as_str(), span)
    }

    fn vectorized_scalar_function_call_dims(
        &self,
        function: &rumoca_core::Function,
        args: &[rumoca_core::Expression],
        scope: &FunctionProjectionScope,
        span: rumoca_core::Span,
        depth: usize,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        let (named, positional) =
            super::super::super::function_calls::split_named_and_positional_call_args(
                function.name.as_str(),
                args,
            )?;
        let mut positional_idx = 0usize;
        let mut dims: Option<Vec<i64>> = None;
        for input in &function.inputs {
            let actual = named.get(input.name.as_str()).copied().or_else(|| {
                super::super::super::function_calls::next_positional_function_input_arg(
                    input,
                    &positional,
                    &mut positional_idx,
                )
            });
            let Some(actual) = actual else {
                continue;
            };
            if !input.dims.is_empty() {
                continue;
            }
            let Some(actual_dims) = self.expr_dims_with_owner(actual, scope, depth + 1, span)?
            else {
                continue;
            };
            if actual_dims.is_empty() {
                continue;
            }
            if !merge_vectorized_scalar_actual_dims(
                &mut dims,
                &actual_dims,
                actual.span().unwrap_or(span),
            )? {
                return Ok(None);
            }
        }
        Ok(dims)
    }

    #[allow(clippy::excessive_nesting)]
    fn cat_expr_dims(
        &self,
        args: &[rumoca_core::Expression],
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        let Some(dim_expr) = args.first() else {
            return Ok(None);
        };
        let Some(dim_value) = self.compile_time_scalar_in_scope(dim_expr, scope)? else {
            return Ok(None);
        };
        if (dim_value - 1.0).abs() > f64::EPSILON {
            return Ok(None);
        }
        let mut output_dims: Option<Vec<i64>> = None;
        for operand in &args[1..] {
            let Some(operand_dims) = self.expr_dims_with_owner(operand, scope, depth + 1, span)?
            else {
                return Ok(None);
            };
            if operand_dims.is_empty() {
                return Ok(None);
            }
            match &mut output_dims {
                None => output_dims = Some(operand_dims),
                Some(dims) => {
                    if dims.len() != operand_dims.len() || dims[1..] != operand_dims[1..] {
                        return Ok(None);
                    }
                    dims[0] = dims[0].checked_add(operand_dims[0]).ok_or_else(|| {
                        LowerError::contract_violation("cat(1, ...) dimension overflows i64", span)
                    })?;
                }
            }
        }
        Ok(output_dims)
    }

    fn projected_declared_function_output_dims(
        &self,
        requested: &str,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        rumoca_core::find_map_top_level_splits_rev(requested, |base_name, suffix| {
            match self.projected_declared_function_output_dims_split(base_name, suffix, span) {
                Ok(Some(dims)) => Some(Ok(dims)),
                Ok(None) => None,
                Err(err) => Some(Err(err)),
            }
        })
        .transpose()
    }

    fn projected_declared_function_output_dims_split(
        &self,
        base_name: &str,
        suffix: &str,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        let Some(function) = self
            .dae_model
            .symbols
            .functions
            .get(&rumoca_core::VarName::new(base_name))
        else {
            return Ok(None);
        };
        let Some(projection_suffix) = parse_output_projection_suffix(suffix) else {
            return Ok(None);
        };
        let Some(output) = function
            .outputs
            .iter()
            .find(|output| output.name == projection_suffix.output_name)
        else {
            return Ok(None);
        };
        if projection_suffix.output_field.is_some()
            && !rumoca_core::qualified_type_name_matches(&output.type_name, "Complex")
        {
            return Ok(None);
        }
        projected_declared_output_dims(output, &projection_suffix.indices, span)
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
            let Some(value) = self.compile_time_scalar_in_scope(&condition, scope)? else {
                return Ok(None);
            };
            if value != 0.0 {
                return Ok(Some(branch));
            }
        }
        Ok(Some(else_branch))
    }

    pub(super) fn compile_time_scalar(&self, expr: &rumoca_core::Expression) -> Option<f64> {
        self.compile_time_scalar_in_scope(expr, &FunctionProjectionScope::default())
            .ok()
            .flatten()
    }

    #[allow(clippy::too_many_lines, clippy::excessive_nesting)]
    pub(super) fn compile_time_scalar_in_scope(
        &self,
        expr: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
    ) -> Result<Option<f64>, LowerError> {
        match expr {
            rumoca_core::Expression::Literal { value, .. } => Ok(literal_to_f64(value)),
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } => {
                if subscripts.is_empty()
                    && let Some(value) = scope.full.get(name.as_str())
                    && !is_same_plain_var_ref(value, name.as_str())
                {
                    return self.compile_time_scalar_in_scope(value, scope);
                }
                let Some(key) = compile_time_var_key(name, subscripts) else {
                    return Ok(None);
                };
                Ok(self.structural_bindings.get(key.as_str()).copied())
            }
            rumoca_core::Expression::Unary { op, rhs, .. } => {
                let Some(value) = self.compile_time_scalar_in_scope(rhs, scope)? else {
                    return Ok(None);
                };
                Ok(match op {
                    rumoca_core::OpUnary::Plus
                    | rumoca_core::OpUnary::DotPlus
                    | rumoca_core::OpUnary::Empty => value,
                    rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => -value,
                    rumoca_core::OpUnary::Not => f64::from(value == 0.0),
                }
                .into())
            }
            rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
                let Some(lhs) = self.compile_time_scalar_in_scope(lhs, scope)? else {
                    return Ok(None);
                };
                let Some(rhs) = self.compile_time_scalar_in_scope(rhs, scope)? else {
                    return Ok(None);
                };
                Ok(compile_time_binary(op, lhs, rhs))
            }
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Size,
                args,
                span,
            } => self.compile_time_size(args, scope, *span),
            rumoca_core::Expression::BuiltinCall {
                function:
                    rumoca_core::BuiltinFunction::NoEvent | rumoca_core::BuiltinFunction::Homotopy,
                args,
                ..
            } => {
                let Some(arg) = args.first() else {
                    return Ok(None);
                };
                self.compile_time_scalar_in_scope(arg, scope)
            }
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Smooth,
                args,
                ..
            } => {
                let Some(arg) = args.get(1) else {
                    return Ok(None);
                };
                self.compile_time_scalar_in_scope(arg, scope)
            }
            rumoca_core::Expression::BuiltinCall {
                function,
                args,
                span: _,
            } if is_compile_time_unary_numeric_builtin(function) => {
                let [arg] = args.as_slice() else {
                    return Ok(None);
                };
                let Some(value) = self.compile_time_scalar_in_scope(arg, scope)? else {
                    return Ok(None);
                };
                if *function == rumoca_core::BuiltinFunction::Integer {
                    return Ok(Some(value.floor()));
                }
                Ok(rumoca_core::apply_scalar_unary_math(*function, value))
            }
            rumoca_core::Expression::BuiltinCall {
                function,
                args,
                span: _,
            } if is_compile_time_binary_numeric_builtin(function) => {
                let [lhs, rhs] = args.as_slice() else {
                    return Ok(None);
                };
                let Some(lhs) = self.compile_time_scalar_in_scope(lhs, scope)? else {
                    return Ok(None);
                };
                let Some(rhs) = self.compile_time_scalar_in_scope(rhs, scope)? else {
                    return Ok(None);
                };
                Ok(rumoca_core::apply_scalar_binary_math(*function, lhs, rhs))
            }
            rumoca_core::Expression::FunctionCall {
                is_constructor: false,
                name,
                args,
                span,
                ..
            } => {
                if let Some((call, selected_index)) =
                    selected_function_output_call(expr, self.dae_model)?
                {
                    if !self.function_call_args_are_compile_time_scalars(args, scope)? {
                        return Ok(None);
                    }
                    let Some(outputs) = self.function_call_outputs_with_projection_scope(
                        &call,
                        0,
                        inherited_projection_source_span(call.span(), *span),
                        Some(scope),
                    )?
                    else {
                        return Ok(None);
                    };
                    let Some(output) = outputs.get(selected_index) else {
                        return Ok(None);
                    };
                    return self.compile_time_scalar_in_scope(&output.expr, scope);
                }
                if let Some(value) = self.compile_time_modelica_math_call(name, args, scope)? {
                    return Ok(Some(value));
                }
                let Some(outputs) = self.function_call_outputs_with_projection_scope(
                    expr,
                    0,
                    inherited_projection_source_span(Some(*span), *span),
                    Some(scope),
                )?
                else {
                    return Ok(None);
                };
                let [output] = outputs.as_slice() else {
                    return Ok(None);
                };
                self.compile_time_scalar_in_scope(&output.expr, scope)
            }
            rumoca_core::Expression::Index {
                base,
                subscripts,
                span,
            } => {
                let Some(dims) = self.expr_dims_with_owner(base, scope, 0, *span)? else {
                    return Ok(None);
                };
                let Some(indices) = self.compile_time_subscript_indices(subscripts, scope)? else {
                    return Ok(None);
                };
                let Some(flat_index) =
                    flat_index_from_indices(&dims, &indices, *span, "compile-time indexed scalar")?
                else {
                    return Ok(None);
                };
                let Some(value) = self.project_value(base, &dims, flat_index, scope, 0, *span)?
                else {
                    return Ok(None);
                };
                self.compile_time_scalar_in_scope(&value, scope)
            }
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => {
                let Some(selected) =
                    self.compile_time_if_selection(branches, else_branch, scope)?
                else {
                    return Ok(None);
                };
                self.compile_time_scalar_in_scope(selected, scope)
            }
            _ => Ok(None),
        }
    }

    fn compile_time_modelica_math_call(
        &self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        scope: &FunctionProjectionScope,
    ) -> Result<Option<f64>, LowerError> {
        let [arg] = args else {
            return Ok(None);
        };
        let Some(value) = self.compile_time_scalar_in_scope(arg, scope)? else {
            return Ok(None);
        };
        let name = name.as_str();
        if name == "asinh" || name.ends_with(".asinh") {
            return Ok(Some(value.asinh()));
        }
        Ok(None)
    }

    fn function_call_args_are_compile_time_scalars(
        &self,
        args: &[rumoca_core::Expression],
        scope: &FunctionProjectionScope,
    ) -> Result<bool, LowerError> {
        for arg in args {
            if self.compile_time_scalar_in_scope(arg, scope)?.is_none() {
                return Ok(false);
            }
        }
        Ok(true)
    }

    #[allow(clippy::excessive_nesting)]
    fn compile_time_subscript_indices(
        &self,
        subscripts: &[rumoca_core::Subscript],
        scope: &FunctionProjectionScope,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        if subscripts.is_empty() {
            return Ok(Some(Vec::new()));
        }
        let mut indices = projection_vec_with_capacity(
            subscripts.len(),
            "compile-time subscript index count",
            subscripts[0].span(),
        )?;
        for subscript in subscripts {
            let index = match subscript {
                rumoca_core::Subscript::Index { value, .. } => *value,
                rumoca_core::Subscript::Expr { expr, span } => {
                    let Some(value) = self.compile_time_scalar_in_scope(expr, scope)? else {
                        return Ok(None);
                    };
                    checked_shape_dimension(value, *span)?
                }
                rumoca_core::Subscript::Colon { .. } => return Ok(None),
            };
            indices.push(index);
        }
        Ok(Some(indices))
    }

    fn compile_time_size(
        &self,
        args: &[rumoca_core::Expression],
        scope: &FunctionProjectionScope,
        span: rumoca_core::Span,
    ) -> Result<Option<f64>, LowerError> {
        let [array_expr, dim_expr] = args else {
            return Ok(None);
        };
        let Some(dim_value) = self.compile_time_scalar_in_scope(dim_expr, scope)? else {
            return Ok(None);
        };
        let dim_index = if dim_value.fract().abs() < f64::EPSILON && dim_value >= 1.0 {
            dim_value as usize
        } else {
            return Ok(None);
        };
        let Some(dims) = self.expr_dims_with_owner(array_expr, scope, 0, span)? else {
            return Ok(None);
        };
        Ok(dims.get(dim_index - 1).map(|dim| *dim as f64))
    }

    fn if_expr_dims(
        &self,
        branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
        else_branch: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        if let Some(selected) = self.compile_time_if_selection(branches, else_branch, scope)? {
            return self.expr_dims_with_owner(selected, scope, depth + 1, span);
        }
        let Some(else_dims) = self.expr_dims_with_owner(else_branch, scope, depth + 1, span)?
        else {
            return Ok(None);
        };
        for (_, branch) in branches {
            let Some(branch_dims) = self.expr_dims_with_owner(branch, scope, depth + 1, span)?
            else {
                return Ok(None);
            };
            if branch_dims != else_dims {
                return Ok(None);
            }
        }
        Ok(Some(else_dims))
    }
}

fn is_compile_time_unary_numeric_builtin(function: &rumoca_core::BuiltinFunction) -> bool {
    matches!(
        function,
        rumoca_core::BuiltinFunction::Abs
            | rumoca_core::BuiltinFunction::Sign
            | rumoca_core::BuiltinFunction::Sqrt
            | rumoca_core::BuiltinFunction::Floor
            | rumoca_core::BuiltinFunction::Ceil
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
            | rumoca_core::BuiltinFunction::Integer
    )
}

fn is_compile_time_binary_numeric_builtin(function: &rumoca_core::BuiltinFunction) -> bool {
    matches!(
        function,
        rumoca_core::BuiltinFunction::Atan2
            | rumoca_core::BuiltinFunction::Min
            | rumoca_core::BuiltinFunction::Max
            | rumoca_core::BuiltinFunction::Div
            | rumoca_core::BuiltinFunction::Mod
            | rumoca_core::BuiltinFunction::Rem
    )
}

fn collect_scope_projected_expr_dims(
    expr: &rumoca_core::Expression,
    scope: &FunctionProjectionScope,
    dims: &mut Option<Vec<i64>>,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            if let Some(candidate) = scope.dims.get(name.as_str())
                && !candidate.is_empty()
            {
                merge_vectorized_scalar_dims(dims, candidate, name.as_str(), span)?;
            } else if let Some(replacement) = scope.full.get(name.as_str())
                && !is_same_plain_var_ref(replacement, name.as_str())
            {
                collect_scope_projected_expr_dims(replacement, scope, dims, span)?;
            }
        }
        rumoca_core::Expression::Unary { rhs, .. } => {
            collect_scope_projected_expr_dims(rhs, scope, dims, span)?;
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            collect_scope_projected_expr_dims(lhs, scope, dims, span)?;
            collect_scope_projected_expr_dims(rhs, scope, dims, span)?;
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, branch) in branches {
                collect_scope_projected_expr_dims(condition, scope, dims, span)?;
                collect_scope_projected_expr_dims(branch, scope, dims, span)?;
            }
            collect_scope_projected_expr_dims(else_branch, scope, dims, span)?;
        }
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            for element in elements {
                collect_scope_projected_expr_dims(element, scope, dims, span)?;
            }
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            collect_scope_projected_expr_dims(start, scope, dims, span)?;
            if let Some(step) = step {
                collect_scope_projected_expr_dims(step, scope, dims, span)?;
            }
            collect_scope_projected_expr_dims(end, scope, dims, span)?;
        }
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_scope_projected_expr_dims(arg, scope, dims, span)?;
            }
        }
        rumoca_core::Expression::FieldAccess { base, .. }
        | rumoca_core::Expression::Index { base, .. } => {
            collect_scope_projected_expr_dims(base, scope, dims, span)?;
        }
        _ => {}
    }
    Ok(())
}
