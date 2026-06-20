use super::*;

impl<'a> LowerBuilder<'a> {
    pub(super) fn lower_complex_operand_parts(
        &mut self,
        expr: &rumoca_core::Expression,
        owner_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<(Reg, Reg), LowerError> {
        if self.requires_complex_projection(expr, scope)? {
            let re = self.lower_field_access(expr, "re", owner_span, scope, call_depth)?;
            let im = self.lower_field_access(expr, "im", owner_span, scope, call_depth)?;
            return Ok((re, im));
        }

        let re = self.lower_expr(expr, scope, call_depth)?;
        let im = self.emit_const_at(
            0.0,
            expr.require_span("complex real operand imaginary component")?
                .span(),
        )?;
        Ok((re, im))
    }

    pub(super) fn requires_complex_projection(
        &self,
        expr: &rumoca_core::Expression,
        scope: &Scope,
    ) -> Result<bool, LowerError> {
        match expr {
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => {
                let span = complex_projection_reference_span(expr, name)?;
                let key = self.scope_key_from_reference(name, span)?;
                Ok(self.component_field_available(&key, name, "re")
                    || self.component_field_available(&key, name, "im")
                    || scope_field_available(scope, &key, "re")
                    || scope_field_available(scope, &key, "im"))
            }
            rumoca_core::Expression::FieldAccess { base, field, .. } => {
                Ok(field_access_binding_key(base, field)
                    .ok()
                    .map(|key| {
                        let source_ref = component_reference_for_field_base(base, field)
                            .ok()
                            .flatten();
                        let scope_key = source_ref
                            .as_ref()
                            .map(|component_ref| component_reference_key(component_ref.clone()))
                            .transpose()
                            .ok()
                            .flatten()
                            .unwrap_or_else(|| generated_scope_key(&key));
                        self.component_field_key_available(&scope_key, "re")
                            || self
                                .component_reference_field_available_opt(source_ref.as_ref(), "re")
                            || self.component_field_key_available(&scope_key, "im")
                            || self
                                .component_reference_field_available_opt(source_ref.as_ref(), "im")
                            || scope_field_available(scope, &scope_key, "re")
                            || scope_field_available(scope, &scope_key, "im")
                    })
                    .unwrap_or(false))
            }
            rumoca_core::Expression::Binary { lhs, rhs, .. } => Ok(self
                .requires_complex_projection(lhs, scope)?
                || self.requires_complex_projection(rhs, scope)?),
            rumoca_core::Expression::Unary {
                op: rumoca_core::OpUnary::Minus,
                rhs,
                ..
            } => self.requires_complex_projection(rhs, scope),
            rumoca_core::Expression::FunctionCall {
                name,
                is_constructor,
                ..
            } => Ok(complex_operator_call_op(name.as_str()).is_some()
                || self.function_call_returns_complex_parts(name, *is_constructor)),
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => Ok(self.branches_require_complex_projection(branches, scope)?
                || self.requires_complex_projection(else_branch, scope)?),
            _ => Ok(false),
        }
    }

    fn branches_require_complex_projection(
        &self,
        branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
        scope: &Scope,
    ) -> Result<bool, LowerError> {
        for (_, value) in branches {
            if self.requires_complex_projection(value, scope)? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn component_field_available(
        &self,
        base_key: &ComponentReferenceKey,
        reference: &rumoca_core::Reference,
        field: &str,
    ) -> bool {
        self.component_field_key_available(base_key, field)
            || self.component_reference_field_available(reference, field)
    }

    fn component_field_key_available(&self, base_key: &ComponentReferenceKey, field: &str) -> bool {
        let Some(field_key) = component_field_key(base_key, field) else {
            return false;
        };
        let generated_name = match &field_key {
            ComponentReferenceKey::Generated { name } => Some(name.as_str()),
            ComponentReferenceKey::Source { .. } => None,
        };
        generated_name.is_some_and(|name| self.layout.binding(name).is_some())
            || generated_name.is_some_and(|name| self.direct_assignments.contains_key(name))
            || self.indexed_bindings.contains_key(&field_key)
    }

    fn component_reference_field_available(
        &self,
        reference: &rumoca_core::Reference,
        field: &str,
    ) -> bool {
        let Some(source_ref) = component_reference_for_source_reference(reference) else {
            return false;
        };
        self.component_reference_field_available_opt(Some(&source_ref), field)
    }

    fn component_reference_field_available_opt(
        &self,
        reference: Option<&rumoca_core::ComponentReference>,
        field: &str,
    ) -> bool {
        let Some(mut component_ref) = reference.cloned() else {
            return false;
        };
        component_ref.parts.push(rumoca_core::ComponentRefPart {
            ident: field.to_string(),
            span: component_ref.span,
            subs: Vec::new(),
        });
        let display_name =
            rumoca_core::ComponentPath::from_component_reference(&component_ref).to_flat_string();
        if self.layout.binding(&display_name).is_some()
            || self.direct_assignments.contains_key(&display_name)
        {
            return true;
        }
        #[cfg(test)]
        {
            if let Some(key) =
                crate::test_support::fixture_key_for_component_ref(&component_ref, &display_name)
            {
                return self.indexed_bindings.contains_key(&key);
            }
        }
        ComponentReferenceKey::from_component_reference(&component_ref)
            .ok()
            .is_some_and(|key| self.indexed_bindings.contains_key(&key))
    }

    fn function_call_returns_complex_parts(
        &self,
        name: &rumoca_core::Reference,
        is_constructor: bool,
    ) -> bool {
        if is_constructor && name.last_segment() == "Complex" {
            return true;
        }
        let Some(function) = self.lookup_function(name) else {
            return false;
        };
        if self.is_record_constructor_call(name, is_constructor) {
            return function.inputs.iter().any(|input| input.name == "re")
                && function.inputs.iter().any(|input| input.name == "im");
        }
        function.outputs.first().is_some_and(|output| {
            output.type_class == Some(rumoca_core::ClassType::Record)
                && rumoca_core::qualified_type_name_matches(&output.type_name, "Complex")
        })
    }

    pub(super) fn lower_constructor_field_access(
        &mut self,
        base: &rumoca_core::Expression,
        field: &str,
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span: _,
        } = base
        else {
            return Ok(None);
        };

        if !self.is_record_constructor_call(name, *is_constructor) {
            return Ok(None);
        }

        if let Some(index) = constructor_positional_field_index(field)
            && let Some(expr) = args.get(index)
        {
            return self.lower_expr(expr, caller_scope, call_depth).map(Some);
        }

        let Some(constructor) = self.lookup_function(name).cloned() else {
            return Ok(None);
        };

        let mut local_scope = Scope::new();
        let mut input_regs = IndexMap::<String, Reg>::new();
        for (idx, input) in constructor.inputs.iter().enumerate() {
            let reg = if let Some(arg_expr) = args.get(idx) {
                self.lower_expr(arg_expr, caller_scope, call_depth + 1)?
            } else if let Some(default_expr) = input.default.as_ref() {
                self.lower_expr(default_expr, &local_scope, call_depth + 1)?
            } else {
                return Err(LowerError::MissingActualArgument {
                    function: name.as_str().to_string(),
                    what: "constructor input",
                    input: input.name.clone(),
                    span: input.span,
                });
            };
            local_scope.insert(generated_scope_key(&input.name), reg);
            input_regs.insert(input.name.clone(), reg);
        }

        if let Some(reg) = input_regs.get(field).copied() {
            return Ok(Some(reg));
        }

        if let Some(output) = constructor
            .outputs
            .iter()
            .find(|output| output.name == field)
        {
            if let Some(default_expr) = output.default.as_ref() {
                let reg = self.lower_expr(default_expr, &local_scope, call_depth + 1)?;
                return Ok(Some(reg));
            }
            if let Some(reg) = local_scope.get(&generated_scope_key(&output.name)).copied() {
                return Ok(Some(reg));
            }
        }

        Ok(None)
    }
}

fn complex_projection_reference_span(
    expr: &rumoca_core::Expression,
    name: &rumoca_core::Reference,
) -> Result<rumoca_core::Span, LowerError> {
    if let Some(span) = expr.span().or_else(|| name.span()) {
        return Ok(span);
    }
    Err(LowerError::UnspannedContractViolation {
        reason: format!(
            "complex projection for `{}` requires source span metadata",
            name.as_str()
        ),
    })
}
