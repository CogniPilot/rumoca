use super::*;

impl<'a> LowerBuilder<'a> {
    pub(super) fn lower_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
        span: rumoca_core::Span,
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if self.is_record_constructor_call(name, is_constructor) {
            let (named_args, positional_args) =
                function_calls::split_named_and_positional_call_args(name.as_str(), args)?;
            if let Some(expr) = named_args
                .get("re")
                .copied()
                .or_else(|| positional_args.first().copied())
            {
                // Modelica.Complex and other scalar record constructors use
                // declared field order; numeric scalar contexts read the first
                // field unless a projection selects another component.
                return self.lower_expr(expr, caller_scope, call_depth + 1);
            }
            if let Some(default_expr) = self
                .lookup_function(name)
                .and_then(|constructor| constructor.inputs.first())
                .and_then(|input| input.default.as_ref())
            {
                return self.lower_expr(default_expr, caller_scope, call_depth + 1);
            }
            return Err(LowerError::InvalidFunction {
                name: name.as_str().to_string(),
                reason: "record constructor scalar projection requires a first field argument or default binding"
                    .to_string(),
            });
        }

        if let Some(reg) = self.lower_runtime_string_special_intrinsic(name.as_str(), args, span)? {
            return Ok(reg);
        }

        if let Some(reg) = self.lower_qualified_standard_numeric_intrinsic(
            name,
            args,
            span,
            caller_scope,
            call_depth,
        )? {
            return Ok(reg);
        }

        if call_depth >= MAX_FUNCTION_INLINE_DEPTH {
            if let Some(reg) =
                self.lower_intrinsic_function_call(name, args, span, caller_scope, call_depth)?
            {
                return Ok(reg);
            }
            return Err(LowerError::InvalidFunction {
                name: name.as_str().to_string(),
                reason: format!("recursion depth exceeded ({MAX_FUNCTION_INLINE_DEPTH})"),
            });
        }

        let function = if let Some(function) = self.lookup_function(name).cloned() {
            function
        } else if let Some(projection) = self.lookup_function_output_projection(name, span)? {
            return self.lower_projected_function_call(
                &projection,
                args,
                span,
                caller_scope,
                call_depth,
            );
        } else if let Some(closure) = self.lookup_function_closure(name, span)?.cloned() {
            return self.lower_function_closure_call(
                &closure,
                args,
                span,
                caller_scope,
                call_depth,
            );
        } else if let Some(reg) =
            self.lower_intrinsic_function_call(name, args, span, caller_scope, call_depth)?
        {
            return Ok(reg);
        } else {
            return Err(LowerError::MissingFunction {
                name: name.as_str().to_string(),
            });
        };

        if function.external.is_some() {
            if let Some(reg) =
                self.lower_intrinsic_function_call(name, args, span, caller_scope, call_depth)?
            {
                return Ok(reg);
            }
            return Err(unsupported_at(
                format!(
                    "external function call `{}` cannot be inlined",
                    name.as_str()
                ),
                span,
            ));
        }

        if function.pure
            && let Some(reg) = self.lower_projected_scalar_function_call(
                name,
                args,
                span,
                caller_scope,
                call_depth,
            )?
        {
            return Ok(reg);
        }

        self.with_local_lower_frame(|this| {
            let bindings =
                this.bind_used_function_inputs(&function, args, caller_scope, call_depth)?;
            let mut scope = bindings.scope;
            this.local_const_bindings.extend(bindings.const_bindings);
            this.initialize_function_output_scope(&function, &mut scope, call_depth)?;

            let _returned = this.lower_statements(&function.body, &mut scope, call_depth + 1)?;

            this.lower_scalar_function_output_value(name.as_str(), &function, &scope, span)
        })
    }

    fn lower_projected_scalar_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        if !args.iter().any(contains_field_access_expr) {
            return Ok(None);
        }
        let mut dae_model = dae::Dae::default();
        dae_model.symbols.functions = self.functions.clone();
        if let Some(variables) = self.dae_variables {
            dae_model.variables = variables.clone();
        }
        let expr = rumoca_core::Expression::FunctionCall {
            name: name.clone(),
            args: args.to_vec(),
            is_constructor: false,
            span,
        };
        let Some(mut values) = (match derivative_rhs::function_call_projected_scalars_with_owner(
            &expr,
            &dae_model,
            self.structural_bindings.as_ref(),
            span,
        ) {
            Ok(values) => values,
            Err(_) => return Ok(None),
        }) else {
            return Ok(None);
        };
        if values.len() != 1 {
            return Ok(None);
        }
        let value = values.remove(0);
        if matches!(
            &value,
            rumoca_core::Expression::FunctionCall {
                name: projected_name,
                args: projected_args,
                is_constructor: false,
                ..
            } if projected_name == name && projected_args == args
        ) {
            return Ok(None);
        }
        match self.lower_expr(&value, caller_scope, call_depth + 1) {
            Ok(reg) => Ok(Some(reg)),
            Err(_) => Ok(None),
        }
    }

    pub(super) fn lookup_function_closure(
        &self,
        name: &rumoca_core::Reference,
        span: rumoca_core::Span,
    ) -> Result<Option<&FunctionClosure>, LowerError> {
        if !name.is_generated()
            && name.component_ref().is_none()
            && self
                .dae_variables
                .and_then(|variables| dae_variable(variables, name.var_name()))
                .is_none()
        {
            return Ok(None);
        }
        let key = self.scope_key_from_reference(name, span)?;
        Ok(self.function_closures.get(&key))
    }

    pub(super) fn lower_function_closure_call(
        &mut self,
        closure: &FunctionClosure,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if call_depth >= MAX_FUNCTION_INLINE_DEPTH {
            return Err(LowerError::InvalidFunction {
                name: closure.target_name.as_str().to_string(),
                reason: format!("recursion depth exceeded ({MAX_FUNCTION_INLINE_DEPTH})"),
            });
        }
        let Some(function) = self.lookup_function(&closure.target_name).cloned() else {
            return Err(LowerError::MissingFunction {
                name: closure.target_name.as_str().to_string(),
            });
        };
        self.ensure_pure_inline_function(closure.target_name.as_str(), &function, span)?;
        if function.external.is_some() {
            return Err(unsupported_at(
                format!(
                    "external function call `{}` cannot be inlined",
                    closure.target_name.as_str()
                ),
                span,
            ));
        }

        self.with_local_lower_frame(|this| {
            let bindings = this.bind_function_closure_inputs(
                &closure.target_name,
                &function.inputs,
                args,
                caller_scope,
                closure,
                call_depth,
            )?;
            let mut scope = bindings.scope;
            this.local_const_bindings.extend(bindings.const_bindings);
            this.initialize_function_output_scope(&function, &mut scope, call_depth)?;

            let _returned = this.lower_statements(&function.body, &mut scope, call_depth + 1)?;

            this.lower_scalar_function_output_value(
                closure.target_name.as_str(),
                &function,
                &scope,
                span,
            )
        })
    }

    fn lower_scalar_function_output_value(
        &self,
        function_name: &str,
        function: &rumoca_core::Function,
        scope: &Scope,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let Some(output) = function.outputs.first() else {
            return Err(LowerError::InvalidFunction {
                name: function_name.to_string(),
                reason: "function call used in scalar expression has no output".to_string(),
            });
        };
        let values = self.scoped_function_output_values(output, scope)?;
        match values.as_slice() {
            [value] => Ok(*value),
            [] => Err(LowerError::InvalidFunction {
                name: function_name.to_string(),
                reason: format!("output `{}` was not assigned", output.name),
            }),
            values => Err(unsupported_at(
                format!(
                    "array-valued output `{}` of function `{function_name}` has {} scalar values in scalar context",
                    output.name,
                    values.len()
                ),
                span,
            )),
        }
    }

    pub(super) fn lookup_function(
        &self,
        name: &rumoca_core::Reference,
    ) -> Option<&'a rumoca_core::Function> {
        self.lookup_function_key(name.as_str())
    }

    pub(super) fn lookup_function_key(&self, name: &str) -> Option<&'a rumoca_core::Function> {
        let lookup_name = VarName::new(name);
        if let Some(function) = self.functions.get(&lookup_name) {
            return Some(function);
        }
        self.functions
            .iter()
            .find(|(key, _)| key.as_str() == name)
            .map(|(_, value)| value)
    }

    pub(super) fn is_record_constructor_call(
        &self,
        name: &rumoca_core::Reference,
        is_constructor: bool,
    ) -> bool {
        self.is_record_constructor_call_key(name.as_str(), is_constructor)
    }

    pub(super) fn is_record_constructor_call_key(&self, name: &str, is_constructor: bool) -> bool {
        is_constructor
            || self
                .lookup_function_key(name)
                .is_some_and(|function| is_record_constructor_signature(name, function))
    }
}

fn contains_field_access_expr(expr: &rumoca_core::Expression) -> bool {
    match expr {
        rumoca_core::Expression::FieldAccess { .. } => true,
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            contains_field_access_expr(lhs) || contains_field_access_expr(rhs)
        }
        rumoca_core::Expression::Unary { rhs, .. } => contains_field_access_expr(rhs),
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. }
        | rumoca_core::Expression::Tuple { elements: args, .. }
        | rumoca_core::Expression::Array { elements: args, .. } => {
            args.iter().any(contains_field_access_expr)
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, value)| {
                contains_field_access_expr(condition) || contains_field_access_expr(value)
            }) || contains_field_access_expr(else_branch)
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            contains_field_access_expr(base)
                || subscripts.iter().any(|subscript| {
                    matches!(
                        subscript,
                        rumoca_core::Subscript::Expr { expr, .. }
                            if contains_field_access_expr(expr)
                    )
                })
        }
        rumoca_core::Expression::ArrayComprehension { expr, filter, .. } => {
            contains_field_access_expr(expr)
                || filter.as_deref().is_some_and(contains_field_access_expr)
        }
        rumoca_core::Expression::Literal { .. }
        | rumoca_core::Expression::VarRef { .. }
        | rumoca_core::Expression::Range { .. }
        | rumoca_core::Expression::Empty { .. } => false,
    }
}
