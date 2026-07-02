use super::compile_time::{compile_time_binary, compile_time_var_key, literal_to_f64};
use super::*;
use crate::projection_suffix::parse_output_projection_suffix;

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
            } if subscripts.is_empty() => {
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
                Ok(variable_by_name(self.dae_model, name.as_str())
                    .map(|variable| variable_dims_i64(variable, span))
                    .transpose()?)
            }
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
            } => self.function_call_expr_dims(name, expr, span, depth),
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
            _ => Ok(None),
        }
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
        let Some(outputs) = self.function_call_outputs_with_owner(expr, depth + 1, call_span)?
        else {
            return Ok(None);
        };
        function_outputs_dims(outputs.len(), call_span).map(Some)
    }

    fn declared_function_output_dims(
        &self,
        name: &rumoca_core::Reference,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        if let Some(function) = self.dae_model.symbols.functions.get(name.var_name()) {
            return exact_declared_function_output_dims(function, span);
        }
        self.projected_declared_function_output_dims(name.as_str(), span)
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
