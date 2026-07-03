use super::*;

impl<'a> LowerBuilder<'a> {
    pub(super) fn lower_var_ref_array_like_values(
        &mut self,
        name: &rumoca_core::Reference,
        expr: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let span = self.var_ref_array_lookup_span(name, expr)?;
        let generated_key = generated_scope_key(name.as_str());
        if let Some(reg) = scope.get(&generated_key).copied()
            && self.local_scalar_binding_precedes_indexed_values(name.as_str())
        {
            return Ok(vec![reg]);
        }
        if let Some(values) = scoped_indexed_binding_values(scope, &generated_key, span)? {
            return Ok(values);
        }
        if let Some(values) = self.local_indexed_binding_values(name.as_str()) {
            return Ok(values);
        }
        if let Some(reg) = scope.get(&generated_key).copied() {
            return Ok(vec![reg]);
        }

        let key = name.as_str();
        if self
            .dae_variables
            .and_then(|variables| dae_variable(variables, name.var_name()))
            .is_some_and(|variable| variable.dims.iter().any(|dim| *dim == 0))
        {
            return Ok(Vec::new());
        }
        if let Some(values) = self.lower_record_field_array_values(key, span)? {
            return Ok(values);
        }

        let key_path = self.scope_key_from_reference(name, span)?;
        if let Some(reg) = scope.get(&key_path).copied()
            && self.local_scalar_binding_precedes_indexed_values(key)
        {
            return Ok(vec![reg]);
        }
        if let Some(values) = scoped_indexed_binding_values(scope, &key_path, span)? {
            return Ok(values);
        }
        if let Some(values) = self.local_indexed_binding_values(key) {
            return Ok(values);
        }
        if self.known_empty_local_arrays.contains(key) {
            return Ok(Vec::new());
        }
        if self
            .structural_bindings
            .get(super::size_binding_key(key, 1).as_str())
            .is_some_and(|dim| *dim == 0.0)
        {
            return Ok(Vec::new());
        }
        if let Some(pre_key) = self.pre_mode_base_key(key)
            && let Some(values) = self.lower_indexed_binding_values_at(pre_key.as_str(), span)?
        {
            return Ok(values);
        }
        if let Some(values) = self.lower_direct_assignment_values_for_key(key, scope, call_depth)? {
            return Ok(values);
        }
        if let Some(values) =
            self.lower_indexed_binding_values_for_resolved_key(key, &key_path, span)?
        {
            return Ok(values);
        }
        if let Some(values) = self.lower_indexed_binding_values_for_reference(name, span)? {
            return Ok(values);
        }
        if let Some(values) = self.lower_scalarized_component_values(&key_path, scope, span)? {
            return Ok(values);
        }
        if let Some(reg) = scope.get(&key_path).copied() {
            return Ok(vec![reg]);
        }
        Ok(vec![self.lower_expr(expr, scope, call_depth)?])
    }

    fn local_scalar_binding_precedes_indexed_values(&self, key: &str) -> bool {
        if self
            .local_indexed_bindings
            .get(key)
            .is_some_and(|bindings| bindings.len() > 1)
        {
            return false;
        }
        !self.local_binding_dims.contains_key(key)
            && !self.known_empty_local_arrays.contains(key)
            && !self
                .structural_bindings
                .contains_key(super::size_binding_key(key, 1).as_str())
    }

    fn lower_record_field_array_values(
        &mut self,
        key: &str,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let Some((base_key, field)) = record_field_array_key(key) else {
            return Ok(None);
        };
        let field_keys = self.indexed_record_field_keys(base_key, field);
        if field_keys.is_empty() {
            return Ok(None);
        }
        let keys = field_keys.values().cloned().collect::<Vec<_>>();
        self.load_binding_keys(&keys, span).map(Some)
    }

    pub(in crate::lower) fn indexed_record_field_keys(
        &self,
        base_key: &str,
        field: &str,
    ) -> IndexMap<Vec<usize>, String> {
        let mut keys = IndexMap::new();
        for (binding_key, _slot) in self.layout.bindings() {
            if let Some(indices) = indexed_record_field_key_indices(binding_key, base_key, field) {
                keys.entry(indices).or_insert_with(|| binding_key.clone());
            }
        }
        for assignment_key in self.direct_assignments.keys() {
            if let Some(indices) = indexed_record_field_key_indices(assignment_key, base_key, field)
            {
                keys.entry(indices)
                    .or_insert_with(|| assignment_key.clone());
            }
        }
        keys.sort_keys();
        keys
    }

    /// Index from a parent binding path to its direct scalarized children.
    ///
    /// Mirrors `scope_key_direct_child_suffix` for generated keys: a binding
    /// `<parent>.<seg>` is a direct child of `<parent>` when `<seg>` contains
    /// neither `.` nor `[`. Built once per lowering; the previous full scan
    /// of `layout.bindings()` per reference was quadratic in model size.
    fn scalarized_children_index(
        &mut self,
    ) -> &IndexMap<String, Vec<(ComponentReferenceKey, String)>> {
        self.scalarized_children_index
            .get_or_insert_with(|| build_scalarized_children_index(self.layout))
    }

    // SPEC_0021: Exception - scalarized component fallback combines local
    // values and generated child bindings to preserve deterministic order.
    #[allow(clippy::excessive_nesting)]
    fn lower_scalarized_component_values(
        &mut self,
        base_key: &ComponentReferenceKey,
        scope: &Scope,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let scope_entries = scope.iter_checked("scalarized component scope source count", span)?;
        let mut values = IndexMap::new();
        values.try_reserve(scope_entries.len()).map_err(|_| {
            LowerError::contract_violation(
                "scalarized component value count capacity exceeds host memory limits",
                span,
            )
        })?;
        for (key, reg) in scope_entries {
            if scope_key_direct_child_suffix(&key, base_key).is_some() {
                values.insert(key, reg);
            }
        }

        // Bindings are keyed by generated names, so only a generated base
        // key can have binding children (matching the Generated/Generated
        // arm of `scope_key_direct_child_suffix`).
        let child_paths = match generated_scope_key_name(base_key) {
            Some(base_name) => {
                let base_name = base_name.to_string();
                if let Some(children) = self.scalarized_children_index().get(&base_name) {
                    let mut child_paths = crate::lower_vec_with_capacity(
                        children.len(),
                        "scalarized child path count",
                        span,
                    )?;
                    for (key, name) in children {
                        if !values.contains_key(key) {
                            child_paths.push((key.clone(), name.clone()));
                        }
                    }
                    child_paths
                } else {
                    Vec::new()
                }
            }
            None => Vec::new(),
        };

        for (key, name) in child_paths {
            let slot = self
                .layout
                .binding(&name)
                .ok_or(LowerError::MissingBinding { name })?;
            let reg = self.emit_slot_load(slot, span)?;
            values.insert(key, reg);
        }

        if values.is_empty() {
            return Ok(None);
        }

        Ok(Some(values.into_values().collect()))
    }
    fn var_ref_array_lookup_span(
        &self,
        name: &rumoca_core::Reference,
        expr: &rumoca_core::Expression,
    ) -> Result<rumoca_core::Span, LowerError> {
        if let Some(span) = expr
            .span()
            .filter(|span| !span.is_dummy())
            .or_else(|| name.span().filter(|span| !span.is_dummy()))
            .or_else(|| self.active_source_context_span())
        {
            return Ok(span);
        }
        Err(LowerError::UnspannedContractViolation {
            reason: format!(
                "array reference `{}` requires source span metadata",
                name.as_str()
            ),
        })
    }
}
