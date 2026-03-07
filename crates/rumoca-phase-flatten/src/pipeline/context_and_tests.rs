use super::*;

impl Context {
    fn top_level_suffix_candidates(reference: &str) -> Vec<String> {
        let parts = crate::path_utils::parse_path_with_indices(reference);
        (1..parts.len())
            .map(|start| parts[start..].join("."))
            .collect()
    }

    /// Create a new flatten context.
    pub(crate) fn new() -> Self {
        Self {
            parameter_values: rustc_hash::FxHashMap::default(),
            real_parameter_values: rustc_hash::FxHashMap::default(),
            boolean_parameter_values: rustc_hash::FxHashMap::default(),
            enum_parameter_values: rustc_hash::FxHashMap::default(),
            constant_values: rustc_hash::FxHashMap::default(),
            modified_constant_keys: rustc_hash::FxHashSet::default(),
            flat_parameter_constant_keys: rustc_hash::FxHashSet::default(),
            array_dimensions: rustc_hash::FxHashMap::default(),
            structural_params: std::collections::HashSet::new(),
            non_structural_params: std::collections::HashSet::new(),
            functions: rustc_hash::FxHashMap::default(),
            record_aliases: rustc_hash::FxHashMap::default(),
            vcg_is_root: rustc_hash::FxHashMap::default(),
            vcg_rooted: rustc_hash::FxHashMap::default(),
            cardinality_counts: rustc_hash::FxHashMap::default(),
            eval_fallback_context: std::cell::OnceCell::new(),
            current_imports: crate::qualify::ImportMap::default(),
        }
    }

    /// Build parameter lookup table from flat model variables.
    ///
    /// This extracts integer and boolean values from parameters that have literal bindings,
    /// and array dimensions for all variables. Used to evaluate for-equation ranges like
    /// `1:n`, if-equation conditions, and `size(array, dim)` calls.
    ///
    /// Uses multi-pass evaluation to handle parameters with conditional bindings:
    /// 1. First pass: extract literal values
    /// 2. Subsequent passes: evaluate expressions using already-known values
    /// 3. Repeat until no new values are found (fixpoint)
    ///
    /// Also tracks structural parameters (Evaluate=true or final) for safe branch selection.
    pub(crate) fn build_parameter_lookup(&mut self, flat: &Model, tree: &ClassTree) {
        let _ = tree; // Used for function evaluation context

        self.seed_flat_parameter_constant_keys(flat);
        let params = self.collect_parameters(flat);

        self.supplement_record_aliases(&params);
        self.init_array_dimensions(flat);

        let var_bindings = Self::collect_var_bindings(flat);
        self.infer_dims_from_literals(flat);

        // Multi-pass evaluation until fixpoint
        self.run_multipass_evaluation(&params, &var_bindings);
    }

    fn seed_flat_parameter_constant_keys(&mut self, flat: &Model) {
        self.flat_parameter_constant_keys.extend(
            flat.variables
                .iter()
                .filter(|(_, var)| {
                    matches!(
                        var.variability,
                        flat::Variability::Parameter(_) | flat::Variability::Constant(_)
                    )
                })
                .map(|(name, _)| name.to_string()),
        );
    }

    /// Refresh enum parameter values after additional constants/booleans are injected.
    ///
    /// This is intentionally narrower than `build_parameter_lookup`: it only updates
    /// enum parameter bindings, preserving previously inferred integer/array metadata.
    pub(crate) fn refresh_enum_parameter_lookup(&mut self, flat: &Model) {
        let params = self.collect_parameters(flat);
        let _ = self.eval_enum_params(&params);
    }

    /// Collect parameters with bindings or start values (MLS §8.6, §4.4.4).
    ///
    /// Also collects non-parameter Integer/Boolean variables with bindings
    /// (e.g., `Integer nX = size(X_boundary, 1)`) so their values are available
    /// for for-equation range evaluation (MLS §8.3.3).
    fn collect_parameters(&mut self, flat: &Model) -> Vec<(String, Expression)> {
        flat.variables
            .iter()
            .filter(|(_, var)| {
                // Include parameters and constants
                matches!(
                    var.variability,
                    flat::Variability::Parameter(_) | flat::Variability::Constant(_)
                )
                // Also include non-parameter Integer/Boolean variables with bindings.
                // These may define compile-time values like `Integer nX = size(arr, 1)`
                // needed for for-equation range evaluation.
                || var.is_discrete_type
            })
            .filter_map(|(name, var)| {
                if matches!(var.variability, flat::Variability::Parameter(_))
                    && var.fixed == Some(false)
                    && !var.evaluate
                {
                    self.non_structural_params.insert(name.to_string());
                }
                let is_fixed_parameter = matches!(var.variability, flat::Variability::Parameter(_))
                    && var.fixed != Some(false);
                if var.evaluate
                    || matches!(var.variability, flat::Variability::Constant(_))
                    || is_fixed_parameter
                {
                    self.structural_params.insert(name.to_string());
                }
                let is_param_or_const = matches!(
                    var.variability,
                    flat::Variability::Parameter(_) | flat::Variability::Constant(_)
                );
                // For parameters/constants: use binding or start value as default.
                // For non-parameter discrete types (Integer/Boolean variables):
                // only use actual bindings. Start values are initial conditions,
                // not compile-time constants (MLS §8.6). Using start values would
                // incorrectly resolve if-equations with dynamic Boolean conditions.
                let expr = if is_param_or_const {
                    var.binding.as_ref().or(var.start.as_ref())
                } else {
                    var.binding.as_ref()
                };
                expr.map(|b| (name.to_string(), b.clone()))
            })
            .collect()
    }

    /// Supplement record aliases from flat variable bindings (MLS §7.2.3).
    fn supplement_record_aliases(&mut self, params: &[(String, Expression)]) {
        for (name, binding) in params {
            if let Expression::VarRef {
                name: alias_target,
                subscripts,
            } = binding
                && subscripts.is_empty()
                && !self.record_aliases.contains_key(name)
            {
                self.record_aliases
                    .insert(name.clone(), alias_target.to_string());
            }
        }
    }

    /// Initialize array dimensions from declared dims (MLS §10.1).
    fn init_array_dimensions(&mut self, flat: &Model) {
        for (name, var) in &flat.variables {
            if var.dims.is_empty() {
                continue;
            }
            let dims_to_use = try_infer_better_dims(var);
            self.array_dimensions.insert(name.to_string(), dims_to_use);
        }
    }

    /// Collect variable bindings for dimension inference.
    fn collect_var_bindings(flat: &Model) -> Vec<(String, Expression)> {
        flat.variables
            .iter()
            .filter_map(|(name, var)| var.binding.as_ref().map(|b| (name.to_string(), b.clone())))
            .collect()
    }

    /// Infer dimensions from array literal bindings (MLS §10.1).
    fn infer_dims_from_literals(&mut self, flat: &Model) {
        for (name, var) in &flat.variables {
            if self
                .array_dimensions
                .contains_key(name.to_string().as_str())
            {
                continue;
            }
            if let Some(binding) = &var.binding
                && let Some(inferred_dims) = infer_array_dimensions(binding)
            {
                #[cfg(feature = "tracing")]
                tracing::debug!(var = %name, dims = ?inferred_dims, "inferred array dimensions from binding");
                self.array_dimensions
                    .insert(name.to_string(), inferred_dims);
            }
        }
    }

    /// Run multi-pass evaluation until fixpoint (MLS §10.4).
    fn run_multipass_evaluation(
        &mut self,
        params: &[(String, Expression)],
        var_bindings: &[(String, Expression)],
    ) {
        const MAX_PASSES: usize = 10;
        for _pass in 0..MAX_PASSES {
            let enum_progress = self.eval_enum_params(params);
            let real_progress = self.eval_real_params(params);
            let int_progress = self.eval_integer_params(params);
            let bool_progress = self.eval_boolean_params(params);
            let dim_progress = self.eval_array_dimensions(var_bindings);
            let varref_dim_progress = self.propagate_varref_dimensions(var_bindings);
            let alias_progress = self.propagate_through_aliases(params);
            if !enum_progress
                && !real_progress
                && !int_progress
                && !bool_progress
                && !dim_progress
                && !varref_dim_progress
                && !alias_progress
            {
                break;
            }
        }
    }

    /// Propagate parameter values through record aliases (MLS §7.2.3).
    ///
    /// For each record alias (e.g., "battery2.cellData" -> "cellData2"),
    /// propagate values from the alias target to the aliased prefix.
    /// This ensures that "battery2.cellData.nRC" has the same value as "cellData2.nRC".
    fn propagate_through_aliases(&mut self, params: &[(String, Expression)]) -> bool {
        let mut progress = false;

        // For each parameter, check if it can be resolved through an alias
        for (name, _) in params {
            let resolved = self.resolve_alias(name);
            if resolved == *name {
                continue; // No alias applies
            }

            // Propagate integer value if available
            if !self.parameter_values.contains_key(name)
                && let Some(val) = self.parameter_values.get(&resolved).copied()
            {
                self.parameter_values.insert(name.clone(), val);
                progress = true;
            }

            // Propagate boolean value if available
            if !self.boolean_parameter_values.contains_key(name)
                && let Some(val) = self.boolean_parameter_values.get(&resolved).copied()
            {
                self.boolean_parameter_values.insert(name.clone(), val);
                progress = true;
            }

            // Propagate array dimensions if available.
            // Skip when the name passes through an expanded array component element,
            // since alias resolution would point to the parent array's dims.
            if !has_embedded_array_subscript_in_parent(name)
                && !self.array_dimensions.contains_key(name)
                && let Some(dims) = self.array_dimensions.get(&resolved).cloned()
            {
                self.array_dimensions.insert(name.clone(), dims);
                progress = true;
            }

            // Propagate enum values if available
            if !self.enum_parameter_values.contains_key(name)
                && let Some(val) = self.enum_parameter_values.get(&resolved).cloned()
            {
                self.enum_parameter_values.insert(name.clone(), val);
                progress = true;
            }
        }

        progress
    }

    /// Try to infer array dimensions using known integer parameters (MLS §10.4).
    ///
    /// This allows evaluating `zeros(n)`, `ones(m)`, `fill(v, n1, n2)` when
    /// the dimension arguments are now-known parameter values.
    ///
    /// Also handles Range expressions like `2:size(table, 2)` when the array
    /// dimensions of the referenced arrays are known.
    ///
    /// Also handles conditional expressions like `table = if cond then A else B`
    /// by evaluating conditions using known boolean and enum parameters.
    fn eval_array_dimensions(&mut self, var_bindings: &[(String, Expression)]) -> bool {
        let mut new_dims = false;
        for (name, binding) in var_bindings {
            new_dims |= self.try_infer_array_dims(name, binding);
        }
        new_dims
    }

    /// Try to infer array dimensions for a single binding.
    fn try_infer_array_dims(&mut self, name: &str, binding: &Expression) -> bool {
        // Skip when the variable is inside an expanded array component element.
        // During array expansion, sub-component modifications (e.g., `L=fill(L1sigma,m)`)
        // are NOT indexed for each element. So `inductor[1].L` gets the same unindexed
        // binding as the parent `inductor.L`, which infers to the parent's array dims.
        // Detect this by checking if any path segment (not the last) has embedded subscripts.
        if has_embedded_array_subscript_in_parent(name) {
            return false;
        }

        let inferred = infer_array_dimensions_full_with_conds(
            binding,
            &self.parameter_values,
            &self.boolean_parameter_values,
            &self.enum_parameter_values,
            &self.array_dimensions,
        );
        let inferred_dims = match inferred {
            Some(dims) => dims,
            None => return false,
        };

        // Check if we should update (MLS §10.1)
        let should_update = self
            .array_dimensions
            .get(name)
            .is_none_or(|existing| inferred_dims.len() > existing.len());

        if should_update {
            #[cfg(feature = "tracing")]
            tracing::debug!(var = %name, dims = ?inferred_dims, "inferred array dimensions from builtin");
            self.array_dimensions
                .insert(name.to_string(), inferred_dims);
            true
        } else {
            false
        }
    }

    /// Propagate array dimensions for VarRef bindings (MLS §10.1, §7.2.3).
    ///
    /// When a parameter has a VarRef binding (e.g., `table = cellData.OCV_SOC_internal`),
    /// we need to propagate dimensions from the target variable. This handles:
    /// - Direct VarRef lookups
    /// - VarRef targets that need alias resolution
    /// - Better dimension propagation (more complete dims replace incomplete ones)
    fn propagate_varref_dimensions(&mut self, var_bindings: &[(String, Expression)]) -> bool {
        var_bindings
            .iter()
            .filter_map(|(name, binding)| self.try_propagate_varref_dims(name, binding))
            .count()
            > 0
    }

    /// Try to propagate dimensions from a VarRef binding.
    fn try_propagate_varref_dims(&mut self, name: &str, binding: &Expression) -> Option<()> {
        let target_name = match binding {
            Expression::VarRef {
                name: target,
                subscripts,
            } if subscripts.is_empty() => target.to_string(),
            _ => return None,
        };

        // Skip when the name passes through an expanded array component element.
        if has_embedded_array_subscript_in_parent(name) {
            return None;
        }

        // Get dimensions from direct lookup and alias resolution
        let direct_dims = self.array_dimensions.get(&target_name);
        let resolved_name = self.resolve_alias(&target_name);
        let alias_dims = (resolved_name != target_name)
            .then(|| self.array_dimensions.get(&resolved_name))
            .flatten();

        // Use better (more complete) dimensions
        let target_dims = best_dims(direct_dims, alias_dims)?;

        // Update if we don't have dims or new dims are better
        let should_update = self
            .array_dimensions
            .get(name)
            .is_none_or(|existing| target_dims.len() > existing.len());

        if should_update {
            self.array_dimensions.insert(name.to_string(), target_dims);
            Some(())
        } else {
            None
        }
    }

    /// Try to evaluate integer parameters in one pass.
    ///
    /// Uses full context including enums to handle conditional bindings like:
    /// `parameter Integer nr = if filterType == LowPass then order else 0`
    ///
    /// Also passes variable context for modification binding resolution (MLS §7.2):
    /// When a binding like `G1(n=n)` has unqualified refs, they're resolved
    /// relative to the parent scope.
    pub(crate) fn eval_integer_params(&mut self, params: &[(String, Expression)]) -> bool {
        // Keep seeded integer values consistent with already-evaluated reals.
        // This prevents stale defaults from shadowing the evaluated binding for
        // the same parameter name in structural integer contexts.
        let mut progress = false;
        for (name, real_val) in &self.real_parameter_values {
            if !real_val.is_finite() || real_val.fract() != 0.0 {
                continue;
            }
            let int_val = *real_val as i64;
            if let Some(existing) = self.parameter_values.get(name).copied()
                && existing != int_val
            {
                self.parameter_values.insert(name.clone(), int_val);
                progress = true;
            }
        }

        // Collect new values to avoid cloning HashMaps for borrow splitting.
        // Build eval context once per pass (not per parameter).
        let eval_ctx = build_eval_context(
            &self.parameter_values,
            &self.real_parameter_values,
            &self.boolean_parameter_values,
            &self.array_dimensions,
            &self.functions,
        );

        let new_vals: Vec<(String, i64)> = params
            .iter()
            .filter_map(|(name, binding)| {
                // Try evaluation with full context including functions
                let int_ctx = ParamEvalContext {
                    known_ints: &self.parameter_values,
                    known_reals: &self.real_parameter_values,
                    known_bools: &self.boolean_parameter_values,
                    known_enums: &self.enum_parameter_values,
                    array_dims: &self.array_dimensions,
                    functions: &self.functions,
                    var_context: Some(name.as_str()),
                };
                if let Some(val) = try_eval_integer_with_context(binding, &int_ctx) {
                    return Some((name.clone(), val));
                }

                // Fallback to rumoca_eval_const for complex expressions
                rumoca_eval_flat::constant::try_eval_integer(binding, &eval_ctx)
                    .map(|val| (name.clone(), val))
            })
            .collect();

        for (name, val) in new_vals {
            if self.parameter_values.get(&name).copied() != Some(val) {
                self.parameter_values.insert(name, val);
                progress = true;
            }
        }
        progress
    }

    /// Try to evaluate boolean parameters in one pass.
    fn eval_boolean_params(&mut self, params: &[(String, Expression)]) -> bool {
        let new_vals: Vec<(String, bool)> = params
            .iter()
            .filter(|(name, _)| !self.boolean_parameter_values.contains_key(name))
            .filter_map(|(name, binding)| {
                try_eval_flat_expr_boolean(
                    binding,
                    &self.parameter_values,
                    &self.boolean_parameter_values,
                    &self.enum_parameter_values,
                )
                .map(|v| (name.clone(), v))
            })
            .collect();

        let progress = !new_vals.is_empty();
        for (name, val) in new_vals {
            self.boolean_parameter_values.insert(name, val);
        }
        progress
    }

    /// Try to evaluate real parameters in one pass.
    fn eval_real_params(&mut self, params: &[(String, Expression)]) -> bool {
        let new_vals: Vec<(String, f64)> = params
            .iter()
            .filter(|(name, _)| !self.real_parameter_values.contains_key(name))
            .filter_map(|(name, binding)| {
                // Try simple real evaluation first
                if let Some(val) = try_eval_flat_expr_real(
                    binding,
                    &self.parameter_values,
                    &self.real_parameter_values,
                ) {
                    return Some((name.clone(), val));
                }
                // Try user-defined function evaluation for function call bindings
                self.try_eval_real_func_call(name, binding)
                    .map(|val| (name.clone(), val))
            })
            .collect();

        let progress = !new_vals.is_empty();
        for (name, val) in new_vals {
            self.real_parameter_values.insert(name, val);
        }
        progress
    }

    /// Try evaluating a function call binding as a real value.
    fn try_eval_real_func_call(&self, name: &str, binding: &Expression) -> Option<f64> {
        let Expression::FunctionCall {
            name: func_name,
            args,
            ..
        } = binding
        else {
            return None;
        };
        let int_ctx = ParamEvalContext {
            known_ints: &self.parameter_values,
            known_reals: &self.real_parameter_values,
            known_bools: &self.boolean_parameter_values,
            known_enums: &self.enum_parameter_values,
            array_dims: &self.array_dimensions,
            functions: &self.functions,
            var_context: Some(name),
        };
        eval_user_func_real(func_name, args, &int_ctx)
    }

    /// Extract enumeration parameter values (MLS §4.9.5).
    ///
    /// Enumeration values are stored as qualified name strings (e.g., "Types.FilterType.LowPass").
    /// This handles both direct enum literals and references to other enum parameters.
    /// MLS §4.9.5: Enumeration types have literals that are constant values.
    pub(crate) fn eval_enum_params(&mut self, params: &[(String, Expression)]) -> bool {
        let param_names: rustc_hash::FxHashSet<&str> =
            params.iter().map(|(name, _)| name.as_str()).collect();

        let mut progress = false;
        loop {
            let new_vals = self.collect_unresolved_enum_values(params, &param_names);
            if new_vals.is_empty() {
                break;
            }
            progress |= self.insert_enum_values(new_vals);
        }

        if progress {
            self.normalize_enum_parameter_values();
        }
        progress
    }

    fn collect_unresolved_enum_values(
        &self,
        params: &[(String, Expression)],
        param_names: &rustc_hash::FxHashSet<&str>,
    ) -> Vec<(String, String)> {
        params
            .iter()
            .filter(|(name, _)| !self.enum_parameter_values.contains_key(name))
            .filter_map(|(name, binding)| {
                self.resolve_enum_binding_value(binding, param_names)
                    .map(|enum_val| (name.clone(), enum_val))
            })
            .collect()
    }

    fn insert_enum_values(&mut self, new_vals: Vec<(String, String)>) -> bool {
        let mut progress = false;
        for (name, val) in new_vals {
            let should_insert = self
                .enum_parameter_values
                .get(&name)
                .is_none_or(|existing| existing != &val);
            if should_insert {
                self.enum_parameter_values.insert(name, val);
                progress = true;
            }
        }
        progress
    }

    fn resolve_enum_binding_value(
        &self,
        binding: &Expression,
        param_names: &rustc_hash::FxHashSet<&str>,
    ) -> Option<String> {
        let enum_val = self.try_eval_enum_binding(binding)?;
        if !self.enum_reference_matches_parameter(&enum_val, param_names) {
            return Some(enum_val);
        }
        self.resolve_non_parameter_enum_varref(binding, param_names)
    }

    fn try_eval_enum_binding(&self, binding: &Expression) -> Option<String> {
        try_eval_flat_expr_enum(
            binding,
            &self.parameter_values,
            &self.boolean_parameter_values,
            &self.enum_parameter_values,
        )
        .or_else(|| self.resolve_varref_enum_reference(binding))
    }

    fn resolve_non_parameter_enum_varref(
        &self,
        binding: &Expression,
        param_names: &rustc_hash::FxHashSet<&str>,
    ) -> Option<String> {
        let resolved = self.resolve_varref_enum_reference(binding)?;
        if self.enum_reference_matches_parameter(&resolved, param_names) {
            return None;
        }
        Some(resolved)
    }

    fn resolve_varref_enum_reference(&self, binding: &Expression) -> Option<String> {
        let Expression::VarRef { name, subscripts } = binding else {
            return None;
        };
        if !subscripts.is_empty() {
            return None;
        }
        self.resolve_enum_reference_value(&name.to_string())
    }

    /// Returns true when `reference` points to another enum parameter name.
    ///
    /// Uses direct lookup, alias lookup, and suffix lookup to cover outer-like
    /// references (e.g. `pipe.system.energyDynamics` -> `system.energyDynamics`).
    pub(crate) fn enum_reference_matches_parameter(
        &self,
        reference: &str,
        param_names: &rustc_hash::FxHashSet<&str>,
    ) -> bool {
        if param_names.contains(reference) {
            return true;
        }

        let alias_resolved = self.resolve_alias(reference);
        if alias_resolved != reference && param_names.contains(alias_resolved.as_str()) {
            return true;
        }

        for suffix in Self::top_level_suffix_candidates(reference) {
            if param_names.contains(suffix.as_str()) {
                return true;
            }
            let suffix_resolved = self.resolve_alias(&suffix);
            if suffix_resolved != suffix && param_names.contains(suffix_resolved.as_str()) {
                return true;
            }
        }

        false
    }

    pub(crate) fn lookup_enum_reference_candidate(&self, reference: &str) -> Option<String> {
        if let Some(enum_val) = self.enum_parameter_values.get(reference) {
            return Some(enum_val.clone());
        }

        let alias_resolved = self.resolve_alias(reference);
        if alias_resolved != reference
            && let Some(enum_val) = self.enum_parameter_values.get(&alias_resolved)
        {
            return Some(enum_val.clone());
        }

        // Fallback for unresolved outer-like paths where only a shorter scoped
        // key exists (e.g. `pipe1.system.energyDynamics` -> `system.energyDynamics`).
        for suffix in Self::top_level_suffix_candidates(reference) {
            if let Some(enum_val) = self.enum_parameter_values.get(&suffix) {
                return Some(enum_val.clone());
            }
            let suffix_resolved = self.resolve_alias(&suffix);
            if suffix_resolved != suffix
                && let Some(enum_val) = self.enum_parameter_values.get(&suffix_resolved)
            {
                return Some(enum_val.clone());
            }
        }

        None
    }

    fn resolve_enum_reference_value(&self, reference: &str) -> Option<String> {
        self.resolve_enum_reference_value_at_depth(reference, 0)
    }

    fn resolve_enum_reference_value_at_depth(
        &self,
        reference: &str,
        depth: usize,
    ) -> Option<String> {
        const MAX_ENUM_REF_DEPTH: usize = 16;
        if depth >= MAX_ENUM_REF_DEPTH {
            return None;
        }

        let candidate = self.lookup_enum_reference_candidate(reference)?;
        if candidate == reference {
            return Some(candidate);
        }

        self.resolve_enum_reference_value_at_depth(&candidate, depth + 1)
            .or(Some(candidate))
    }

    /// Collapse enum parameter values to their final literal values.
    ///
    /// This avoids preserving intermediate references like
    /// `HEX.system.energyDynamics` in the value map, which can suppress
    /// compile-time condition evaluation in initial equations.
    fn normalize_enum_parameter_values(&mut self) {
        let names: Vec<String> = self.enum_parameter_values.keys().cloned().collect();
        for name in names {
            let Some(current) = self.enum_parameter_values.get(&name).cloned() else {
                continue;
            };
            if let Some(resolved) = self.resolve_enum_reference_value(&current)
                && resolved != current
            {
                self.enum_parameter_values.insert(name, resolved);
            }
        }
    }

    /// Get array dimensions for a variable.
    ///
    /// Returns the dimensions vector if the variable has array dimensions,
    /// or None for scalar variables.
    pub(crate) fn get_array_dimensions(&self, name: &str) -> Option<&Vec<i64>> {
        self.array_dimensions.get(name)
    }

    /// Return the shared `rumoca_eval_const` context used by complex-expression fallback.
    pub(crate) fn eval_fallback_context(&self) -> &rumoca_eval_flat::constant::EvalContext {
        self.eval_fallback_context
            .get_or_init(|| equations::build_eval_context(self, None))
    }

    #[cfg(test)]
    pub(crate) fn has_cached_eval_fallback_context(&self) -> bool {
        self.eval_fallback_context.get().is_some()
    }

    /// Check if a boolean expression can be safely evaluated at compile time.
    ///
    /// Returns true if:
    /// Resolve a parameter name through record aliases (MLS §7.2.3).
    ///
    /// If `name` has a prefix that's a record alias, returns the resolved name.
    /// For example, if "battery2.cellData" aliases "cellData2", then
    /// "battery2.cellData.nRC" resolves to "cellData2.nRC".
    ///
    /// This function iteratively resolves aliases until no more can be applied,
    /// handling chains like:
    /// - `stack.cell.cell.cellData` -> `stack.cell.stackData.cellData`
    /// - `stack.cell.stackData.cellData` -> `stack.stackData.cellData`
    ///
    /// Returns the original name if no alias applies.
    fn resolve_alias(&self, name: &str) -> String {
        const MAX_DEPTH: usize = 10; // Prevent infinite loops
        let mut current = name.to_string();
        for _iteration in 0..MAX_DEPTH {
            let resolved = self.resolve_alias_once(&current);
            if resolved == current {
                // No alias applied, we're done
                break;
            }
            current = resolved;
        }
        current
    }

    /// Apply one level of alias resolution.
    pub(crate) fn resolve_alias_once(&self, name: &str) -> String {
        // Check all possible prefixes from longest to shortest
        // We prefer prefix-based resolution over exact matches because field-level
        // exact matches may have incorrect targets (qualified with wrong parent).
        let parts = crate::path_utils::parse_path_with_indices(name);
        for i in (1..parts.len()).rev() {
            let prefix = parts[..i].join(".");
            if let Some(alias_target) = self.record_aliases.get(&prefix) {
                // Replace prefix with alias target
                let suffix = parts[i..].join(".");
                return format!("{}.{}", alias_target, suffix);
            }
        }

        // Fall back to exact match (for top-level aliases without fields)
        if let Some(alias_target) = self.record_aliases.get(name) {
            return alias_target.clone();
        }

        name.to_string()
    }

    fn integral_real_param(&self, name: &str) -> Option<i64> {
        self.real_parameter_values.get(name).and_then(|val| {
            if val.is_finite() && val.fract() == 0.0 {
                Some(*val as i64)
            } else {
                None
            }
        })
    }

    /// Look up an integer parameter value, resolving through aliases if needed.
    pub(crate) fn get_integer_param(&self, name: &str) -> Option<i64> {
        // Try direct lookup in integer parameters first
        if let Some(val) = self.parameter_values.get(name).copied() {
            // Prefer the evaluated real value when both maps disagree.
            // Later constant/default injection can seed stale integer values.
            return Some(self.integral_real_param(name).unwrap_or(val));
        }
        // Try alias resolution for integers
        let resolved = self.resolve_alias(name);
        if resolved != name
            && let Some(val) = self.parameter_values.get(&resolved).copied()
        {
            return Some(self.integral_real_param(&resolved).unwrap_or(val));
        }
        // Fallback: try real parameters that are whole numbers (e.g., Real m = 3)
        let real_name = if resolved != name { &resolved } else { name };
        if let Some(val) = self
            .real_parameter_values
            .get(real_name)
            .or_else(|| self.real_parameter_values.get(name))
            .copied()
            && val.fract() == 0.0
            && val.is_finite()
        {
            return Some(val as i64);
        }
        None
    }

    /// Look up a boolean parameter value, resolving through aliases if needed.
    pub(crate) fn get_boolean_param(&self, name: &str) -> Option<bool> {
        // Try direct lookup first
        if let Some(val) = self.boolean_parameter_values.get(name) {
            return Some(*val);
        }
        // Try alias resolution
        let resolved = self.resolve_alias(name);
        if resolved != name {
            return self.boolean_parameter_values.get(&resolved).copied();
        }
        None
    }

    /// Look up an enum parameter value, resolving through aliases if needed.
    pub(crate) fn get_enum_param(&self, name: &str) -> Option<String> {
        // Try direct lookup first
        if let Some(val) = self.enum_parameter_values.get(name) {
            return Some(val.clone());
        }
        // Try alias resolution
        let resolved = self.resolve_alias(name);
        if resolved != name {
            return self.enum_parameter_values.get(&resolved).cloned();
        }
        None
    }

    /// Look up array dimensions, resolving through aliases if needed.
    pub(crate) fn get_array_dims(&self, name: &str) -> Option<Vec<i64>> {
        // Try direct lookup first
        if let Some(dims) = self.array_dimensions.get(name) {
            return Some(dims.clone());
        }
        // Try alias resolution
        let resolved = self.resolve_alias(name);
        if resolved != name {
            return self.array_dimensions.get(&resolved).cloned();
        }
        None
    }
}

pub(crate) fn scoped_lookup_candidates(name: &str, scope: &str) -> Vec<String> {
    let mut candidates = Vec::new();
    let mut current_scope = Some(scope);
    while let Some(scope_name) = current_scope {
        if scope_name.is_empty() {
            candidates.push(name.to_string());
            break;
        }
        candidates.push(format!("{scope_name}.{name}"));
        current_scope = crate::path_utils::parent_scope(scope_name);
        if current_scope.is_none() {
            current_scope = Some("");
        }
    }
    candidates
}

impl rumoca_core::EvalLookup for Context {
    fn lookup_integer(&self, name: &str, scope: &str) -> Option<i64> {
        for candidate in scoped_lookup_candidates(name, scope) {
            if let Some(value) = self.get_integer_param(&candidate) {
                return Some(value);
            }
        }

        if crate::path_utils::has_top_level_dot(name) {
            if let Some(value) = lookup_with_scope(name, scope, &self.parameter_values) {
                return Some(value);
            }
            if let Some(value) = lookup_with_scope(name, scope, &self.real_parameter_values)
                && value.is_finite()
                && value.fract() == 0.0
            {
                return Some(value as i64);
            }
        }
        None
    }

    fn lookup_real(&self, name: &str, scope: &str) -> Option<f64> {
        for candidate in scoped_lookup_candidates(name, scope) {
            if let Some(value) = self.real_parameter_values.get(&candidate).copied() {
                return Some(value);
            }

            let resolved = self.resolve_alias(&candidate);
            if resolved != candidate
                && let Some(value) = self.real_parameter_values.get(&resolved).copied()
            {
                return Some(value);
            }

            if let Some(value) = self.get_integer_param(&candidate) {
                return Some(value as f64);
            }
        }

        if crate::path_utils::has_top_level_dot(name) {
            if let Some(value) = lookup_with_scope(name, scope, &self.real_parameter_values) {
                return Some(value);
            }
            if let Some(value) = lookup_with_scope(name, scope, &self.parameter_values) {
                return Some(value as f64);
            }
        }
        None
    }

    fn lookup_boolean(&self, name: &str, scope: &str) -> Option<bool> {
        for candidate in scoped_lookup_candidates(name, scope) {
            if let Some(value) = self.get_boolean_param(&candidate) {
                return Some(value);
            }
        }

        if crate::path_utils::has_top_level_dot(name) {
            return lookup_with_scope(name, scope, &self.boolean_parameter_values);
        }
        None
    }

    fn lookup_enum<'a>(&'a self, name: &str, scope: &str) -> Option<std::borrow::Cow<'a, str>> {
        for candidate in scoped_lookup_candidates(name, scope) {
            if let Some(value) = self.enum_parameter_values.get(&candidate) {
                return Some(std::borrow::Cow::Borrowed(value.as_str()));
            }

            let resolved = self.resolve_alias(&candidate);
            if resolved != candidate
                && let Some(value) = self.enum_parameter_values.get(&resolved)
            {
                return Some(std::borrow::Cow::Borrowed(value.as_str()));
            }
        }

        if crate::path_utils::has_top_level_dot(name) {
            return lookup_with_scope(name, scope, &self.enum_parameter_values)
                .map(std::borrow::Cow::Owned);
        }
        None
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

/// Process a class instance to extract equations and algorithms.
pub(crate) fn process_class_instance(
    ctx: &mut Context,
    flat: &mut Model,
    class_data: &ClassInstanceData,
    component_override_map: &ComponentOverrideMap,
    tree: &ClassTree,
) -> Result<(), FlattenError> {
    let prefix = &class_data.qualified_name;
    let def_map = Some(&tree.def_map);
    let (override_packages, override_functions) = override_context_for_scope(
        &class_data.qualified_name.to_flat_string(),
        component_override_map,
        tree,
    );

    // MLS §13.2: Set the import map for this class instance so that
    // qualification resolves imported short names (e.g., `pi` → `Modelica.Constants.pi`)
    // instead of incorrectly prefixing them with the component path.
    let mut imports: crate::qualify::ImportMap =
        class_data.resolved_imports.iter().cloned().collect();
    if let Some(class_name) = class_scope_name_for_instance(class_data) {
        crate::qualify::collect_lexical_package_aliases(tree, &class_name, &mut imports);
    }
    ctx.current_imports = imports;

    // Convert regular equations.
    for inst_eq in &class_data.equations {
        // Handle when-equations separately (pass context for parameter evaluation).
        let mut clauses = when_equations::flatten_when_equation(ctx, inst_eq, prefix, def_map)?;
        for clause in &mut clauses {
            rewrite_function_overrides_in_when_clause(
                clause,
                tree,
                &override_packages,
                &override_functions,
            );
        }
        flat.when_clauses.extend(clauses);

        // Handle other equations (including for-loops that may contain when-equations).
        let mut flattened =
            equations::flatten_equation_with_def_map(ctx, inst_eq, prefix, def_map)?;
        rewrite_function_overrides_in_flattened(
            &mut flattened,
            tree,
            &override_packages,
            &override_functions,
        );
        let equation_base = flat.equations.len();
        for eq in flattened.equations {
            flat.add_equation(eq);
        }
        for mut for_eq in flattened.for_equations {
            for_eq.first_equation_index += equation_base;
            flat.add_for_equation(for_eq);
        }
        flat.assert_equations.extend(flattened.assert_equations);
        flat.when_clauses.extend(flattened.when_clauses);
        flat.definite_roots.extend(flattened.definite_roots);
        flat.branches.extend(flattened.branches);
        flat.potential_roots.extend(flattened.potential_roots);
    }

    // Convert initial equations (when-equations are rejected per EQN-006).
    for inst_eq in &class_data.initial_equations {
        if matches!(&inst_eq.equation, rumoca_ir_ast::Equation::When(_)) {
            return Err(FlattenError::unsupported_equation(
                "when-equations are not allowed in initial equations (MLS §8.6)",
                inst_eq.span,
            ));
        }

        let mut flattened =
            equations::flatten_equation_with_def_map(ctx, inst_eq, prefix, def_map)?;
        rewrite_function_overrides_in_flattened(
            &mut flattened,
            tree,
            &override_packages,
            &override_functions,
        );
        let equation_base = flat.initial_equations.len();
        for eq in flattened.equations {
            flat.add_initial_equation(eq);
        }
        for mut for_eq in flattened.for_equations {
            for_eq.first_equation_index += equation_base;
            flat.add_initial_for_equation(for_eq);
        }
        flat.initial_assert_equations
            .extend(flattened.assert_equations);
        if !flattened.when_clauses.is_empty() {
            return Err(FlattenError::unsupported_equation(
                "when-equations are not allowed in initial equations (MLS §8.6)",
                inst_eq.span,
            ));
        }
    }

    // Convert algorithms (preserve structure per SPEC_0020)
    let imports = &ctx.current_imports;
    for inst_algs in &class_data.algorithms {
        let mut flat_alg = flatten_algorithm_section(inst_algs, prefix, imports, def_map)?;
        rewrite_function_overrides_in_algorithm(
            &mut flat_alg,
            tree,
            &override_packages,
            &override_functions,
        );
        flat.algorithms.push(flat_alg);
    }

    // Convert initial algorithms
    for inst_algs in &class_data.initial_algorithms {
        let mut flat_alg = flatten_algorithm_section(inst_algs, prefix, imports, def_map)?;
        rewrite_function_overrides_in_algorithm(
            &mut flat_alg,
            tree,
            &override_packages,
            &override_functions,
        );
        flat.initial_algorithms.push(flat_alg);
    }

    Ok(())
}

fn class_scope_name_for_instance(class_data: &ClassInstanceData) -> Option<String> {
    class_data
        .equations
        .first()
        .map(|eq| eq.origin.to_flat_string())
        .or_else(|| {
            class_data
                .initial_equations
                .first()
                .map(|eq| eq.origin.to_flat_string())
        })
        .or_else(|| {
            class_data
                .algorithms
                .first()
                .and_then(|alg| alg.first().map(|stmt| stmt.origin.to_flat_string()))
        })
        .or_else(|| {
            class_data
                .initial_algorithms
                .first()
                .and_then(|alg| alg.first().map(|stmt| stmt.origin.to_flat_string()))
        })
}

/// Flatten an algorithm section.
///
/// Per SPEC_0020: Algorithms are preserved as structured statements,
/// with variable names qualified and outputs identified.
pub(crate) fn flatten_algorithm_section(
    statements: &[InstanceStatement],
    prefix: &QualifiedName,
    imports: &qualify::ImportMap,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<Algorithm, FlattenError> {
    // Get span from first statement if available
    let span = statements.first().map(|s| s.span).unwrap_or(Span::DUMMY);

    // Extract raw statements from InstanceStatements
    let raw_statements: Vec<_> = statements.iter().map(|s| s.statement.clone()).collect();

    let origin = format!("algorithm from {}", prefix.to_flat_string());
    let no_locals: std::collections::HashSet<String> = std::collections::HashSet::new();

    // Use the algorithms module for qualification and output extraction
    algorithms::flatten_algorithm_section(
        &raw_statements,
        prefix,
        span,
        origin,
        imports,
        def_map,
        &no_locals,
    )
}

use super::function_overrides_and_dims::*;

/// Process a component instance to create a flat variable.
///
/// Only primitive types (Real, Integer, Boolean, String) become flat variables.
/// Class types (connectors, models, records) are containers and are skipped.
pub(crate) fn process_component_instance(
    _ctx: &mut Context,
    flat: &mut Model,
    instance_data: &rumoca_ir_ast::InstanceData,
    component_override_map: &ComponentOverrideMap,
    tree: &rumoca_ir_ast::ClassTree,
    global_imports: &qualify::ImportMap,
) -> Result<(), FlattenError> {
    // Skip if this is an empty path (root)
    let var_name = qualified_to_var_name(&instance_data.qualified_name);
    if var_name.as_str().is_empty() {
        return Ok(());
    }

    // Skip non-primitive types (class types like connectors, models, records)
    // These are containers, not scalar variables
    if !instance_data.is_primitive {
        return Ok(());
    }

    let mut flat_var = variables::create_flat_variable(instance_data, tree, global_imports)?;
    let (override_packages, override_functions) =
        override_context_for_scope(var_name.as_str(), component_override_map, tree);
    rewrite_function_overrides_in_flat_variable(
        &mut flat_var,
        tree,
        &override_packages,
        &override_functions,
    );
    flat.variable_type_names.insert(
        var_name.clone(),
        variables::flat_output_type_name(instance_data, tree),
    );
    if instance_data.is_final {
        flat.variable_final_flags.insert(var_name.clone(), true);
    }
    flat.add_variable(var_name, flat_var);

    Ok(())
}

/// Convert a QualifiedName to a flat VarName string.
pub(crate) fn qualified_to_var_name(qn: &QualifiedName) -> VarName {
    VarName::new(qn.to_flat_string())
}

/// Qualify an expression with a prefix (convert local names to global names).
///
/// This walks the expression tree and prefixes all component references
/// with the given prefix. For example, if prefix is "sub" and the expression
/// contains "x", it becomes "sub.x".
///
/// Uses default options: does not skip local refs, resets def_id.
/// Does NOT resolve imports — use `qualify_expression_imports` for that.
pub(crate) fn qualify_expression(
    expr: &ast::Expression,
    prefix: &QualifiedName,
) -> rumoca_ir_flat::Expression {
    qualify_expression_imports(expr, prefix, &qualify::ImportMap::default())
}

/// Qualify an expression with import-aware resolution (MLS §13.2).
///
/// Like `qualify_expression`, but also resolves imported short names to their
/// fully-qualified forms using the provided import map. For example, if imports
/// contain `("pi", "Modelica.Constants.pi")`, then `pi` becomes
/// `Modelica.Constants.pi` instead of being prefixed with the component path.
pub(crate) fn qualify_expression_imports(
    expr: &ast::Expression,
    prefix: &QualifiedName,
    imports: &qualify::ImportMap,
) -> rumoca_ir_flat::Expression {
    qualify_expression_imports_with_def_map(expr, prefix, imports, None)
}

/// Qualify an expression with import-aware resolution and optional def-map canonicalization.
///
/// When a component reference carries a resolved `def_id` (notably function calls),
/// `def_map` canonicalizes it to the fully-qualified declaration name.
pub(crate) fn qualify_expression_imports_with_def_map(
    expr: &ast::Expression,
    prefix: &QualifiedName,
    imports: &qualify::ImportMap,
    def_map: Option<&crate::ResolveDefMap>,
) -> rumoca_ir_flat::Expression {
    // Use default options for equation qualification
    let opts = qualify::QualifyOptions {
        preserve_def_id: true,
        ..qualify::QualifyOptions::default()
    };
    let qualified = qualify::qualify_expression_with_imports(expr, prefix, opts, imports);
    crate::ast_lower::expression_from_ast_with_def_map(&qualified, def_map)
}
