use super::*;

impl FunctionProjectionAnalysis<'_> {
    pub(super) fn initialize_projected_declared_arrays(
        &self,
        function: &rumoca_core::Function,
        scope: &mut FunctionProjectionScope,
        depth: usize,
        owner_span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        for param in function.outputs.iter().chain(function.locals.iter()) {
            self.initialize_projected_declared_param(param, scope, depth + 1, owner_span)?;
            self.initialize_projected_record_fields(param, scope, depth + 1, owner_span)?;
        }
        Ok(())
    }

    fn initialize_projected_declared_param(
        &self,
        param: &rumoca_core::FunctionParam,
        scope: &mut FunctionProjectionScope,
        depth: usize,
        owner_span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        if self.initialize_declared_default(param, scope, depth + 1, owner_span)? {
            return Ok(());
        }
        let param_span = inherited_projection_span(param.span, owner_span);
        let Some(dims) = self.resolved_function_param_dims(param, scope, depth + 1, param_span)?
        else {
            return Ok(());
        };
        if dims.is_empty() || scope.scalars.contains_key(param.name.as_str()) {
            return Ok(());
        }
        let count = scalar_count_for_dims(&dims, "function declared array dimensions", param_span)?;
        let dims = copy_projection_dims(
            &dims,
            "projected declared array dimension count",
            param_span,
        )?;
        scope.dims.insert(param.name.clone(), dims);
        let mut scalars = projection_vec_with_capacity(
            count,
            "projected declared array scalar count",
            param_span,
        )?;
        for _ in 0..count {
            scalars.push(rumoca_core::Expression::Empty { span: param_span });
        }
        scope.scalars.insert(param.name.clone(), scalars);
        Ok(())
    }

    fn initialize_projected_record_fields(
        &self,
        param: &rumoca_core::FunctionParam,
        scope: &mut FunctionProjectionScope,
        depth: usize,
        owner_span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        if param.type_class != Some(rumoca_core::ClassType::Record)
            || depth > crate::lower::MAX_FUNCTION_INLINE_DEPTH
        {
            return Ok(());
        }
        let Some(type_def_id) = param.type_def_id else {
            return Ok(());
        };
        let Ok(constructor) = rumoca_core::resolve_record_constructor(
            self.dae_model.symbols.functions.values(),
            &param.type_name,
            type_def_id,
        ) else {
            return Ok(());
        };
        for field in &constructor.inputs {
            let mut scoped_field = field.clone();
            scoped_field.name = format!("{}.{}", param.name, field.name);
            self.initialize_projected_declared_param(&scoped_field, scope, depth + 1, owner_span)?;
            self.initialize_projected_record_fields(&scoped_field, scope, depth + 1, owner_span)?;
        }
        Ok(())
    }
}
