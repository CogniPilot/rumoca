use super::*;

impl<'a> LowerBuilder<'a> {
    pub(super) fn bind_complex_input(
        &mut self,
        scope: &mut Scope,
        function_name: &str,
        input: &rumoca_core::FunctionParam,
        expr: &rumoca_core::Expression,
        expr_scope: &Scope,
        call_depth: usize,
    ) -> Result<(), LowerError> {
        if input.dims.is_empty() {
            let owner_span = expr.span().unwrap_or(input.span);
            let (re, im) =
                self.lower_complex_operand_parts(expr, owner_span, expr_scope, call_depth)?;
            self.bind_complex_scalar_components(scope, &input.name, re, im);
            return Ok(());
        }

        let binding_dims =
            self.resolve_function_input_binding_dims(function_name, input, expr, expr_scope)?;
        let span = expr.span().unwrap_or(input.span);
        let expected = dims_scalar_count(
            &binding_dims,
            format!(
                "function `{function_name}` input `{}` declared shape",
                input.name
            ),
            span,
        )?;
        if let rumoca_core::Expression::FieldAccess { field, .. } = expr
            && matches!(field.as_str(), "re" | "im")
        {
            let component_values = self.lower_array_like_values(expr, expr_scope, call_depth)?;
            validate_complex_component_width(
                function_name,
                input,
                &binding_dims,
                expected,
                component_values.len(),
                span,
            )?;
            let zeros = self.complex_zero_values(component_values.len(), span)?;
            if field == "re" {
                self.bind_complex_component_values(
                    scope,
                    &input.name,
                    &component_values,
                    &zeros,
                    expected,
                    span,
                )?;
            } else {
                self.bind_complex_component_values(
                    scope,
                    &input.name,
                    &zeros,
                    &component_values,
                    expected,
                    span,
                )?;
            }
            self.bind_local_array_shape(&input.name, &binding_dims, expected, span)?;
            return Ok(());
        }

        let re_expr = rumoca_core::Expression::FieldAccess {
            base: Box::new(expr.clone()),
            field: "re".to_string(),
            span,
        };
        let im_expr = rumoca_core::Expression::FieldAccess {
            base: Box::new(expr.clone()),
            field: "im".to_string(),
            span,
        };

        let re_values = self.lower_array_like_values(&re_expr, expr_scope, call_depth)?;
        let im_values = self.lower_array_like_values(&im_expr, expr_scope, call_depth)?;
        validate_complex_component_width(
            function_name,
            input,
            &binding_dims,
            expected,
            re_values.len(),
            span,
        )?;
        validate_complex_component_width(
            function_name,
            input,
            &binding_dims,
            expected,
            im_values.len(),
            span,
        )?;
        self.bind_complex_component_values(
            scope,
            &input.name,
            &re_values,
            &im_values,
            expected,
            span,
        )?;
        self.bind_local_array_shape(&input.name, &binding_dims, expected, span)?;
        Ok(())
    }

    fn bind_complex_scalar_components(
        &mut self,
        scope: &mut Scope,
        base_name: &str,
        re: Reg,
        im: Reg,
    ) {
        // MLS §3.7.2 and §12.6: Complex is a record with Real fields `re`
        // and `im`; scalar function inputs must bind those field components
        // whether the caller passed a record expression or an already-projected
        // component reference.
        scope.insert(generated_scope_key(base_name), re);
        scope.insert(generated_scope_key(format!("{base_name}.re")), re);
        scope.insert(generated_scope_key(format!("{base_name}.im")), im);
    }

    fn bind_complex_component_values(
        &mut self,
        scope: &mut Scope,
        base_name: &str,
        re_values: &[Reg],
        im_values: &[Reg],
        expected: usize,
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        let width = re_values.len().max(im_values.len());
        if width != expected {
            return Err(LowerError::contract_violation(
                format!(
                    "Complex input `{base_name}` expected {expected} scalar value(s), got {width}"
                ),
                span,
            ));
        }
        let zero_values = self.complex_zero_values(width, span)?;
        if let Some(first) = re_values.first().copied() {
            scope.insert(generated_scope_key(base_name), first);
        }
        if let Some(re) = re_values.first().copied() {
            scope.insert(generated_scope_key(format!("{base_name}.re")), re);
        }
        if let Some(im) = im_values.first().copied() {
            scope.insert(generated_scope_key(format!("{base_name}.im")), im);
        }
        self.bind_assignment_values_at(scope, &format!("{base_name}[:].re"), re_values, span)?;
        self.bind_assignment_values_at(scope, &format!("{base_name}[:].im"), im_values, span)?;
        self.bind_assignment_values_at(scope, &format!("{base_name}[:].re.re"), re_values, span)?;
        self.bind_assignment_values_at(
            scope,
            &format!("{base_name}[:].re.im"),
            &zero_values,
            span,
        )?;
        self.bind_assignment_values_at(
            scope,
            &format!("{base_name}[:].im.re"),
            &zero_values,
            span,
        )?;
        self.bind_assignment_values_at(scope, &format!("{base_name}[:].im.im"), im_values, span)?;
        for (idx, reg) in re_values.iter().copied().enumerate() {
            scope.insert(
                generated_scope_key(format!("{base_name}[{}].re", idx + 1)),
                reg,
            );
        }
        for (idx, reg) in im_values.iter().copied().enumerate() {
            scope.insert(
                generated_scope_key(format!("{base_name}[{}].im", idx + 1)),
                reg,
            );
        }
        Ok(())
    }

    fn complex_zero_values(
        &mut self,
        width: usize,
        span: rumoca_core::Span,
    ) -> Result<Vec<Reg>, LowerError> {
        let mut values =
            crate::lower_vec_with_capacity(width, "Complex zero component value count", span)?;
        for _ in 0..width {
            values.push(self.emit_const_at(0.0, span)?);
        }
        Ok(values)
    }
}
