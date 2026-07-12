use super::*;

impl<'a> LowerBuilder<'a> {
    pub(in crate::lower) fn set_known_local_array_dims(
        &mut self,
        name: &str,
        dims: Vec<i64>,
        value_count: usize,
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        let is_empty = !dims.is_empty() && dims.iter().all(|dim| *dim >= 0) && dims.contains(&0);
        self.bind_local_array_shape(name, &dims, value_count, span)?;
        self.local_binding_dims
            .insert(name.to_string(), dims.clone());
        if is_empty {
            self.known_empty_local_arrays.insert(name.to_string());
        } else {
            self.known_empty_local_arrays.shift_remove(name);
        }
        Ok(())
    }

    pub(super) fn bind_declared_function_param_shape(
        &mut self,
        param: &rumoca_core::FunctionParam,
    ) -> Result<(), LowerError> {
        if param.dims.is_empty() {
            return Ok(());
        }
        if let Some(dims) = self.resolve_function_param_shape(param) {
            self.set_known_local_array_dims(
                &param.name,
                dims.clone(),
                exact_dim_value_count(
                    &dims,
                    format!("function parameter `{}`", param.name),
                    param.span,
                )?,
                param.span,
            )?;
            return Ok(());
        }
        if param.dims.contains(&0) && param.shape_expr.is_empty() {
            self.set_known_local_array_dims(&param.name, param.dims.clone(), 0, param.span)?;
            return Ok(());
        }
        if param.dims.iter().any(|dim| *dim < 0) {
            self.known_empty_local_arrays
                .shift_remove(param.name.as_str());
            return Ok(());
        }

        let value_count = dims_scalar_count(
            &param.dims,
            format!("function parameter `{}`", param.name),
            param.span,
        )?;
        self.set_known_local_array_dims(&param.name, param.dims.clone(), value_count, param.span)?;
        Ok(())
    }

    fn resolve_function_param_shape(&self, param: &rumoca_core::FunctionParam) -> Option<Vec<i64>> {
        if param.shape_expr.len() != param.dims.len() {
            return None;
        }
        param
            .shape_expr
            .iter()
            .map(|subscript| match subscript {
                rumoca_core::Subscript::Index { value, .. } if *value >= 0 => Some(*value),
                rumoca_core::Subscript::Expr { expr, .. } => self
                    .eval_compile_time_int(expr, &self.local_const_bindings, "function shape")
                    .ok()
                    .filter(|dim| *dim >= 0),
                rumoca_core::Subscript::Colon { .. } | rumoca_core::Subscript::Index { .. } => None,
            })
            .collect()
    }
}
