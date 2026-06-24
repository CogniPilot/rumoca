use super::enum_dimensions::{enum_type_dimension, infer_enum_range_dimensions};
use super::*;
use rumoca_eval_flat::phase_constant::try_eval_flat_expr_enum;

mod import_shadow;

use import_shadow::imports_without_shadowed_aliases;

#[derive(Clone, Copy)]
struct ParamBinding<'a> {
    name: &'a str,
    binding: &'a Expression,
    may_be_record_alias: bool,
    binding_from_modification: bool,
}

fn insert_record_alias(
    aliases: &mut rustc_hash::FxHashMap<rumoca_core::ComponentPath, rumoca_core::ComponentPath>,
    source_path: rumoca_core::ComponentPath,
    alias_target: &rumoca_core::Reference,
) {
    aliases
        .entry(source_path)
        .or_insert_with(|| rumoca_core::ComponentPath::from_flat_path(alias_target.as_str()));
}

fn is_array_literal_binding(binding: &Expression) -> bool {
    matches!(binding, Expression::Array { .. })
}

impl Context {
    /// Create a new flatten context.
    pub(crate) fn new() -> Self {
        Self {
            parameter_values: rustc_hash::FxHashMap::default(),
            real_parameter_values: rustc_hash::FxHashMap::default(),
            boolean_parameter_values: rustc_hash::FxHashMap::default(),
            enum_parameter_values: rustc_hash::FxHashMap::default(),
            constant_values: rustc_hash::FxHashMap::default(),
            target_def_names: rustc_hash::FxHashMap::default(),
            modified_constant_keys: rustc_hash::FxHashSet::default(),
            flat_parameter_constant_keys: rustc_hash::FxHashSet::default(),
            array_dimensions: rustc_hash::FxHashMap::default(),
            structural_params: std::collections::HashSet::new(),
            non_structural_params: std::collections::HashSet::new(),
            functions: rustc_hash::FxHashMap::default(),
            record_aliases: rustc_hash::FxHashMap::default(),
            component_members: super::component_member_scope::ComponentMemberScopes::default(),
            vcg_is_root: rustc_hash::FxHashMap::default(),
            vcg_rooted: rustc_hash::FxHashMap::default(),
            cardinality_counts: rustc_hash::FxHashMap::default(),
            eval_fallback_context: std::cell::OnceCell::new(),
            current_imports: crate::qualify::ImportMap::default(),
            class_def_ids: std::sync::Arc::new(rustc_hash::FxHashSet::default()),
            current_class_scope_path: None,
            simulated_root_name: None,
        }
    }

    pub(crate) fn instance_name_for_prefix(&self, prefix: &QualifiedName) -> Option<String> {
        let root = self.simulated_root_name.as_ref()?;
        let suffix = prefix.to_flat_string();
        if suffix.is_empty() {
            Some(root.clone())
        } else {
            Some(format!("{root}.{suffix}"))
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

    pub(crate) fn recompute_symbolic_component_dimensions(
        &mut self,
        flat: &mut Model,
        overlay: &InstanceOverlay,
        tree: &ClassTree,
    ) -> Result<bool, FlattenError> {
        let mut changed = false;
        for instance_data in overlay.components.values() {
            if !instance_data.is_primitive || instance_data.dims_expr.is_empty() {
                continue;
            }
            let var_name = qualified_to_var_name(&instance_data.qualified_name);
            let Some(flat_var) = flat.variables.get(&var_name) else {
                continue;
            };
            let span = instance_source_span(instance_data, tree)?;
            let resolved_dims = self.resolve_component_dims_expr(
                var_name.as_str(),
                &instance_data.dims_expr,
                flat_var,
                tree,
                span,
            )?;
            let Some(flat_var) = flat.variables.get_mut(&var_name) else {
                continue;
            };
            if flat_var.dims != resolved_dims {
                flat_var.dims.clone_from(&resolved_dims);
                changed = true;
            }
            if self.array_dimensions.get(var_name.as_str()) != Some(&resolved_dims) {
                self.array_dimensions
                    .insert(var_name.to_string(), resolved_dims);
                changed = true;
            }
        }
        Ok(changed)
    }

    fn resolve_component_dims_expr(
        &self,
        var_name: &str,
        dims_expr: &[ast::Subscript],
        flat_var: &flat::Variable,
        tree: &ClassTree,
        span: rumoca_core::Span,
    ) -> Result<Vec<i64>, FlattenError> {
        let mut dims = Vec::with_capacity(dims_expr.len());
        for (index, subscript) in dims_expr.iter().enumerate() {
            let dim = match subscript {
                ast::Subscript::Expression(_) => {
                    self.eval_component_dim_subscript(var_name, subscript, tree, span)?
                }
                ast::Subscript::Range { .. } | ast::Subscript::Empty => {
                    self.resolve_colon_component_dimension(var_name, flat_var, index, tree, span)?
                }
            };
            dims.push(dim);
        }
        Ok(dims)
    }

    fn resolve_colon_component_dimension(
        &self,
        var_name: &str,
        flat_var: &flat::Variable,
        index: usize,
        tree: &ClassTree,
        span: rumoca_core::Span,
    ) -> Result<i64, FlattenError> {
        let inferred_dims = flat_var
            .binding
            .as_ref()
            .and_then(|binding| self.infer_binding_dimensions(var_name, binding, tree));
        let resolved_dims = best_dims(self.array_dimensions.get(var_name), inferred_dims.as_ref());

        if let Some(dim) = resolved_dims
            .as_ref()
            .and_then(|dims| dims.get(index).copied())
            .filter(|dim| *dim >= 0)
        {
            return Ok(dim);
        }

        if (flat_var.dims.len() > 1 || flat_var.dims.iter().any(|dim| *dim > 1))
            && let Some(dim) = flat_var.dims.get(index).copied().filter(|dim| *dim >= 0)
        {
            return Ok(dim);
        }

        let Some(dim) = resolved_dims.and_then(|dims| dims.get(index).copied()) else {
            return Err(FlattenError::unresolved_component_dimension(
                var_name,
                ":".to_string(),
                span,
            ));
        };
        if dim < 0 {
            return Err(FlattenError::unresolved_component_dimension(
                var_name,
                ":".to_string(),
                span,
            ));
        }
        Ok(dim)
    }

    fn infer_binding_dimensions(
        &self,
        var_name: &str,
        binding: &Expression,
        tree: &ClassTree,
    ) -> Option<Vec<i64>> {
        infer_enum_range_dimensions(binding, tree).or_else(|| {
            infer_array_dimensions_full_with_functions(
                binding,
                &ParamEvalContext::new(
                    &self.parameter_values,
                    &self.real_parameter_values,
                    &self.boolean_parameter_values,
                    &self.enum_parameter_values,
                    &self.array_dimensions,
                    &self.functions,
                    Some(var_name),
                ),
            )
        })
    }

    fn eval_component_dim_subscript(
        &self,
        var_name: &str,
        subscript: &ast::Subscript,
        tree: &ClassTree,
        span: rumoca_core::Span,
    ) -> Result<i64, FlattenError> {
        let ast::Subscript::Expression(expr) = subscript else {
            return Err(FlattenError::unresolved_component_dimension(
                var_name,
                subscript.to_string(),
                span,
            ));
        };
        if let Some(dim) = enum_type_dimension(expr, tree) {
            return Ok(dim);
        }
        let lowered =
            crate::ast_lower::expression_from_ast_with_def_map(expr, Some(&tree.def_map))?;
        let eval_ctx = ParamEvalContext {
            known_ints: &self.parameter_values,
            known_reals: &self.real_parameter_values,
            known_bools: &self.boolean_parameter_values,
            known_enums: &self.enum_parameter_values,
            array_dims: &self.array_dimensions,
            functions: &self.functions,
            user_func_eval_ctx: None,
            var_context: Some(var_name),
        };
        let Some(dim) = try_eval_integer_with_context(&lowered, &eval_ctx) else {
            return Err(FlattenError::unresolved_component_dimension(
                var_name,
                expr.to_string(),
                span,
            ));
        };
        if dim < 0 {
            return Err(FlattenError::unresolved_component_dimension(
                var_name,
                expr.to_string(),
                span,
            ));
        }
        Ok(dim)
    }

    pub(crate) fn seed_flat_parameter_constant_keys(&mut self, flat: &Model) {
        self.flat_parameter_constant_keys.extend(
            flat.variables
                .iter()
                .filter(|(_, var)| {
                    matches!(
                        var.variability,
                        rumoca_core::Variability::Parameter(_)
                            | rumoca_core::Variability::Constant(_)
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
        let _ = self.eval_enum_param_bindings(&params);
    }

    /// Collect parameters with bindings (MLS §4.5, §8.6).
    ///
    /// Also collects non-parameter Integer/Boolean variables with bindings
    /// (e.g., `Integer nX = size(X_boundary, 1)`) so their values are available
    /// for for-equation range evaluation (MLS §8.3.3).
    fn collect_parameters<'a>(&mut self, flat: &'a Model) -> Vec<ParamBinding<'a>> {
        flat.variables
            .iter()
            .filter(|(_, var)| {
                // Include parameters and constants
                matches!(
                    var.variability,
                    rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
                )
                // Also include non-parameter Integer/Boolean variables with bindings.
                // These may define compile-time values like `Integer nX = size(arr, 1)`
                // needed for for-equation range evaluation.
                || var.is_discrete_type
            })
            .filter_map(|(name, var)| {
                if matches!(var.variability, rumoca_core::Variability::Parameter(_))
                    && var.fixed == Some(false)
                    && !var.evaluate
                {
                    self.non_structural_params.insert(name.to_string());
                }
                let is_fixed_parameter =
                    matches!(var.variability, rumoca_core::Variability::Parameter(_))
                        && var.fixed != Some(false);
                let may_be_record_alias = !var.is_primitive;
                if var.evaluate
                    || matches!(var.variability, rumoca_core::Variability::Constant(_))
                    || is_fixed_parameter
                {
                    self.structural_params.insert(name.to_string());
                }
                // For parameters/constants: use declaration bindings only.
                // `start` is an initialization guess/default and must not drive
                // structural branch selection; otherwise `p(start=a)=b` can
                // flatten equations as if `p == a`.
                // For non-parameter discrete types (Integer/Boolean variables):
                // only use actual bindings. Start values are initial conditions,
                // not compile-time constants (MLS §8.6). Using start values would
                // incorrectly resolve if-equations with dynamic Boolean conditions.
                var.binding.as_ref().map(|binding| ParamBinding {
                    name: name.as_str(),
                    binding,
                    may_be_record_alias,
                    binding_from_modification: var.binding_from_modification,
                })
            })
            .collect()
    }

    /// Supplement record aliases from flat variable bindings (MLS §7.2.3).
    fn supplement_record_aliases(&mut self, params: &[ParamBinding<'_>]) {
        for ParamBinding {
            name,
            binding,
            may_be_record_alias,
            ..
        } in params
        {
            if !may_be_record_alias {
                continue;
            }
            if let Expression::VarRef {
                name: alias_target,
                subscripts,
                span: rumoca_core::Span::DUMMY,
            } = binding
                && subscripts.is_empty()
            {
                let source_path = rumoca_core::ComponentPath::from_flat_path(name);
                insert_record_alias(&mut self.record_aliases, source_path, alias_target);
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
    fn collect_var_bindings(flat: &Model) -> Vec<ParamBinding<'_>> {
        flat.variables
            .iter()
            .filter_map(|(name, var)| {
                var.binding.as_ref().map(|binding| ParamBinding {
                    name: name.as_str(),
                    binding,
                    may_be_record_alias: !var.is_primitive,
                    binding_from_modification: var.binding_from_modification,
                })
            })
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
        params: &[ParamBinding<'_>],
        var_bindings: &[ParamBinding<'_>],
    ) {
        const MAX_PASSES: usize = 10;
        for _pass in 0..MAX_PASSES {
            let enum_progress = self.eval_enum_param_bindings(params);
            let real_progress = self.eval_real_params(params);
            let int_progress = self.eval_integer_param_bindings(params);
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
    fn propagate_through_aliases(&mut self, params: &[ParamBinding<'_>]) -> bool {
        let mut progress = false;

        // For each parameter, check if it can be resolved through an alias
        for ParamBinding { name, .. } in params {
            let resolved = self.resolve_alias(name);
            if resolved == *name {
                continue; // No alias applies
            }

            // Propagate integer value if available
            if !self.parameter_values.contains_key(*name)
                && let Some(val) = self.parameter_values.get(&resolved).copied()
            {
                self.parameter_values.insert((*name).to_string(), val);
                progress = true;
            }

            // Propagate boolean value if available
            if !self.boolean_parameter_values.contains_key(*name)
                && let Some(val) = self.boolean_parameter_values.get(&resolved).copied()
            {
                self.boolean_parameter_values
                    .insert((*name).to_string(), val);
                progress = true;
            }

            // Propagate array dimensions if available.
            // Skip when the name passes through an expanded array component element,
            // since alias resolution would point to the parent array's dims.
            if !has_embedded_array_subscript_in_parent(name)
                && !self.array_dimensions.contains_key(*name)
                && let Some(dims) = self.array_dimensions.get(&resolved).cloned()
            {
                self.array_dimensions.insert((*name).to_string(), dims);
                progress = true;
            }

            // Propagate enum values if available
            if !self.enum_parameter_values.contains_key(*name)
                && let Some(val) = self.enum_parameter_values.get(&resolved).cloned()
            {
                self.enum_parameter_values.insert((*name).to_string(), val);
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
    fn eval_array_dimensions(&mut self, var_bindings: &[ParamBinding<'_>]) -> bool {
        let mut new_dims = false;
        for ParamBinding {
            name,
            binding,
            binding_from_modification,
            ..
        } in var_bindings
        {
            new_dims |= self.try_infer_array_dims(name, binding, *binding_from_modification);
        }
        new_dims
    }

    /// Try to infer array dimensions for a single binding.
    fn try_infer_array_dims(
        &mut self,
        name: &str,
        binding: &Expression,
        binding_from_modification: bool,
    ) -> bool {
        // Skip when the variable is inside an expanded array component element.
        // During array expansion, sub-component modifications (e.g., `L=fill(L1sigma,m)`)
        // are NOT indexed for each element. So `inductor[1].L` gets the same unindexed
        // binding as the parent `inductor.L`, which infers to the parent's array dims.
        // Detect this by checking if any path segment (not the last) has embedded subscripts.
        if has_embedded_array_subscript_in_parent(name)
            && !(binding_from_modification && is_array_literal_binding(binding))
        {
            return false;
        }

        let inferred = infer_array_dimensions_full_with_functions(
            binding,
            &ParamEvalContext::new(
                &self.parameter_values,
                &self.real_parameter_values,
                &self.boolean_parameter_values,
                &self.enum_parameter_values,
                &self.array_dimensions,
                &self.functions,
                Some(name),
            ),
        );
        let inferred_dims = match inferred {
            Some(dims) => dims,
            None => return false,
        };

        // Check if we should update (MLS §10.1)
        let should_update = self
            .array_dimensions
            .get(name)
            .is_none_or(|existing| dims_are_better(&inferred_dims, existing));

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
    fn propagate_varref_dimensions(&mut self, var_bindings: &[ParamBinding<'_>]) -> bool {
        var_bindings
            .iter()
            .filter_map(|ParamBinding { name, binding, .. }| {
                self.try_propagate_varref_dims(name, binding)
            })
            .count()
            > 0
    }

    /// Try to propagate dimensions from a VarRef binding.
    fn try_propagate_varref_dims(&mut self, name: &str, binding: &Expression) -> Option<()> {
        let target_name = match binding {
            Expression::VarRef {
                name: target,
                subscripts,
                ..
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
            .is_none_or(|existing| dims_are_better(&target_dims, existing));

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
    #[cfg(test)]
    pub(crate) fn eval_integer_params(&mut self, params: &[(String, Expression)]) -> bool {
        let params = params
            .iter()
            .map(|(name, binding)| ParamBinding {
                name: name.as_str(),
                binding,
                may_be_record_alias: false,
                binding_from_modification: false,
            })
            .collect::<Vec<_>>();
        self.eval_integer_param_bindings(&params)
    }

    #[cfg(test)]
    pub(crate) fn eval_modified_integer_params(&mut self, params: &[(String, Expression)]) -> bool {
        let params = params
            .iter()
            .map(|(name, binding)| ParamBinding {
                name: name.as_str(),
                binding,
                may_be_record_alias: false,
                binding_from_modification: true,
            })
            .collect::<Vec<_>>();
        self.eval_integer_param_bindings(&params)
    }

    fn eval_integer_param_bindings(&mut self, params: &[ParamBinding<'_>]) -> bool {
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
            .filter_map(
                |ParamBinding {
                     name,
                     binding,
                     binding_from_modification,
                     ..
                 }| {
                    if let Some(val) = self.try_eval_modifier_scoped_integer_alias(
                        name,
                        binding,
                        *binding_from_modification,
                    ) {
                        return Some(((*name).to_string(), val));
                    }

                    // Try evaluation with full context including functions
                    let int_ctx = ParamEvalContext {
                        known_ints: &self.parameter_values,
                        known_reals: &self.real_parameter_values,
                        known_bools: &self.boolean_parameter_values,
                        known_enums: &self.enum_parameter_values,
                        array_dims: &self.array_dimensions,
                        functions: &self.functions,
                        user_func_eval_ctx: None,
                        var_context: Some(name),
                    };
                    if let Some(val) = try_eval_integer_with_context(binding, &int_ctx) {
                        return Some(((*name).to_string(), val));
                    }

                    // Fallback to rumoca_eval_const for complex expressions
                    rumoca_eval_flat::constant::try_eval_integer(binding, &eval_ctx)
                        .map(|val| ((*name).to_string(), val))
                },
            )
            .collect();

        for (name, val) in new_vals {
            if self.parameter_values.get(&name).copied() != Some(val) {
                self.parameter_values.insert(name.clone(), val);
                progress = true;
            }
            if let Some(real_val) = self.real_parameter_values.get_mut(&name)
                && (real_val.fract() == 0.0)
                && (*real_val as i64 != val)
            {
                *real_val = val as f64;
                progress = true;
            }
        }
        progress
    }

    fn try_eval_modifier_scoped_integer_alias(
        &self,
        name: &str,
        binding: &Expression,
        binding_from_modification: bool,
    ) -> Option<i64> {
        if !binding_from_modification {
            return None;
        }
        let target = unqualified_varref_name(binding)?;
        let source_scope = modifier_source_scope(name)?;
        rumoca_core::EvalLookup::lookup_integer(self, target, source_scope.as_str())
    }

    /// Try to evaluate boolean parameters in one pass.
    fn eval_boolean_params(&mut self, params: &[ParamBinding<'_>]) -> bool {
        let new_vals: Vec<(String, bool)> = params
            .iter()
            .filter_map(|ParamBinding { name, binding, .. }| {
                let bool_ctx = ParamEvalContext {
                    known_ints: &self.parameter_values,
                    known_reals: &self.real_parameter_values,
                    known_bools: &self.boolean_parameter_values,
                    known_enums: &self.enum_parameter_values,
                    array_dims: &self.array_dimensions,
                    functions: &self.functions,
                    user_func_eval_ctx: None,
                    var_context: Some(name),
                };
                try_eval_flat_expr_boolean_with_context(binding, &bool_ctx)
                    .map(|v| ((*name).to_string(), v))
            })
            .collect();

        let mut progress = false;
        for (name, val) in new_vals {
            if self.boolean_parameter_values.get(&name).copied() != Some(val) {
                self.boolean_parameter_values.insert(name, val);
                progress = true;
            }
        }
        progress
    }

    /// Try to evaluate real parameters in one pass.
    fn eval_real_params(&mut self, params: &[ParamBinding<'_>]) -> bool {
        let new_vals: Vec<(String, f64)> = params
            .iter()
            .filter_map(|ParamBinding { name, binding, .. }| {
                // Try simple real evaluation first
                if let Some(val) = try_eval_flat_expr_real(
                    binding,
                    &self.parameter_values,
                    &self.real_parameter_values,
                ) {
                    return Some(((*name).to_string(), val));
                }
                // Try user-defined function evaluation for function call bindings
                self.try_eval_real_func_call(name, binding)
                    .map(|val| ((*name).to_string(), val))
            })
            .collect();

        let mut progress = false;
        for (name, val) in new_vals {
            if self
                .real_parameter_values
                .get(&name)
                .copied()
                .is_none_or(|existing| existing != val)
            {
                self.real_parameter_values.insert(name, val);
                progress = true;
            }
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
            user_func_eval_ctx: None,
            var_context: Some(name),
        };
        eval_user_func_real(func_name, args, &int_ctx)
    }

    /// Extract enumeration parameter values (MLS §4.9.5).
    ///
    /// Enumeration values are stored as qualified name strings (e.g., "Types.FilterType.LowPass").
    /// This handles both direct enum literals and references to other enum parameters.
    /// MLS §4.9.5: Enumeration types have literals that are constant values.
    #[cfg(test)]
    pub(crate) fn eval_enum_params(&mut self, params: &[(String, Expression)]) -> bool {
        let params = params
            .iter()
            .map(|(name, binding)| ParamBinding {
                name: name.as_str(),
                binding,
                may_be_record_alias: false,
                binding_from_modification: false,
            })
            .collect::<Vec<_>>();
        self.eval_enum_param_bindings(&params)
    }

    fn eval_enum_param_bindings(&mut self, params: &[ParamBinding<'_>]) -> bool {
        let param_names: rustc_hash::FxHashSet<&str> =
            params.iter().map(|binding| binding.name).collect();

        let mut progress = false;
        loop {
            let mut reference_cache = rustc_hash::FxHashMap::default();
            let new_vals = self.collect_enum_values(params, &param_names, &mut reference_cache);
            if new_vals.is_empty() {
                break;
            }
            let pass_progress = self.insert_enum_values(new_vals);
            progress |= pass_progress;
            if !pass_progress {
                break;
            }
        }

        if progress {
            self.normalize_enum_parameter_values();
        }
        progress
    }

    fn collect_enum_values(
        &self,
        params: &[ParamBinding<'_>],
        param_names: &rustc_hash::FxHashSet<&str>,
        reference_cache: &mut rustc_hash::FxHashMap<String, Option<String>>,
    ) -> Vec<(String, String)> {
        params
            .iter()
            .filter_map(|ParamBinding { name, binding, .. }| {
                if self.enum_parameter_values.contains_key(*name) {
                    return None;
                }
                self.resolve_enum_binding_value(binding, param_names, reference_cache)
                    .map(|enum_val| ((*name).to_string(), enum_val))
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
        reference_cache: &mut rustc_hash::FxHashMap<String, Option<String>>,
    ) -> Option<String> {
        let enum_val = self.try_eval_enum_binding(binding, reference_cache)?;
        if !self.enum_reference_matches_parameter(&enum_val, param_names) {
            return Some(enum_val);
        }
        self.resolve_non_parameter_enum_varref(binding, param_names, reference_cache)
    }

    fn try_eval_enum_binding(
        &self,
        binding: &Expression,
        reference_cache: &mut rustc_hash::FxHashMap<String, Option<String>>,
    ) -> Option<String> {
        try_eval_flat_expr_enum(
            binding,
            &self.parameter_values,
            &self.boolean_parameter_values,
            &self.enum_parameter_values,
        )
        .or_else(|| self.resolve_varref_enum_reference(binding, reference_cache))
    }

    fn resolve_non_parameter_enum_varref(
        &self,
        binding: &Expression,
        param_names: &rustc_hash::FxHashSet<&str>,
        reference_cache: &mut rustc_hash::FxHashMap<String, Option<String>>,
    ) -> Option<String> {
        let resolved = self.resolve_varref_enum_reference(binding, reference_cache)?;
        if self.enum_reference_matches_parameter(&resolved, param_names) {
            return None;
        }
        Some(resolved)
    }

    fn resolve_varref_enum_reference(
        &self,
        binding: &Expression,
        reference_cache: &mut rustc_hash::FxHashMap<String, Option<String>>,
    ) -> Option<String> {
        let Expression::VarRef {
            name, subscripts, ..
        } = binding
        else {
            return None;
        };
        if !subscripts.is_empty() {
            return None;
        }
        self.resolve_enum_reference_value(name.as_str(), reference_cache)
    }

    /// Returns true when `reference` points to another enum parameter name.
    ///
    /// Uses direct lookup and structural alias lookup. Outer-like references must
    /// be represented by aliases before this point; this lookup must not recover
    /// structure by dropping leading path segments.
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

        None
    }

    fn resolve_enum_reference_value(
        &self,
        reference: &str,
        reference_cache: &mut rustc_hash::FxHashMap<String, Option<String>>,
    ) -> Option<String> {
        const MAX_ENUM_REF_DEPTH: usize = 16;
        if let Some(cached) = reference_cache.get(reference) {
            return cached.clone();
        }

        let mut current = reference.to_string();
        let mut visited = Vec::new();
        let mut result = None;
        for _ in 0..MAX_ENUM_REF_DEPTH {
            if let Some(cached) = reference_cache.get(&current).cloned() {
                result = cached;
                break;
            }
            visited.push(current.clone());
            let Some(candidate) = self.lookup_enum_reference_candidate(&current) else {
                break;
            };
            if candidate == current || self.lookup_enum_reference_candidate(&candidate).is_none() {
                result = Some(candidate);
                break;
            }
            current = candidate;
        }

        for visited_reference in visited {
            reference_cache.insert(visited_reference, result.clone());
        }
        result
    }

    /// Collapse enum parameter values to their final literal values.
    ///
    /// This avoids preserving intermediate references like
    /// `HEX.system.energyDynamics` in the value map, which can suppress
    /// compile-time condition evaluation in initial equations.
    fn normalize_enum_parameter_values(&mut self) {
        let names: Vec<String> = self.enum_parameter_values.keys().cloned().collect();
        let mut reference_cache = rustc_hash::FxHashMap::default();
        for name in names {
            let Some(current) = self.enum_parameter_values.get(&name).cloned() else {
                continue;
            };
            if let Some(resolved) =
                self.resolve_enum_reference_value(&current, &mut reference_cache)
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
        let mut current = rumoca_core::ComponentPath::from_flat_path(name);
        for _iteration in 0..MAX_DEPTH {
            let resolved = self.resolve_alias_once_path(&current);
            if resolved == current {
                // No alias applied, we're done
                break;
            }
            current = resolved;
        }
        current.to_flat_string()
    }

    /// Apply one level of alias resolution.
    #[cfg(test)]
    pub(crate) fn resolve_alias_once(&self, name: &str) -> String {
        self.resolve_alias_once_path(&rumoca_core::ComponentPath::from_flat_path(name))
            .to_flat_string()
    }

    fn resolve_alias_once_path(
        &self,
        path: &rumoca_core::ComponentPath,
    ) -> rumoca_core::ComponentPath {
        crate::alias_paths::resolve_component_alias_once(path, None, &self.record_aliases)
            .unwrap_or_else(|| path.clone())
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
    scoped_lookup_candidates_with_scope(name, scope)
        .into_iter()
        .map(|(candidate, _candidate_scope)| candidate)
        .collect()
}

pub(crate) fn scoped_lookup_candidates_with_scope(
    name: &str,
    scope: &str,
) -> Vec<(String, String)> {
    let name_path = rumoca_core::ComponentPath::from_flat_path(name);
    let mut candidates = Vec::new();
    let mut current_scope = Some(rumoca_core::ComponentPath::from_flat_path(scope));
    while let Some(scope_path) = current_scope {
        candidates.push((
            scope_path.join(&name_path).to_flat_string(),
            scope_path.to_flat_string(),
        ));
        current_scope = scope_path.parent();
    }
    if !scope.is_empty() {
        candidates.push((name_path.to_flat_string(), String::new()));
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

        if crate::path_utils::is_nested_name(name) {
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

        if crate::path_utils::is_nested_name(name) {
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

        if crate::path_utils::is_nested_name(name) {
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

        if crate::path_utils::is_nested_name(name) {
            return lookup_with_scope(name, scope, &self.enum_parameter_values)
                .map(std::borrow::Cow::Owned);
        }
        None
    }
}

fn unqualified_varref_name(expr: &Expression) -> Option<&str> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let path = rumoca_core::ComponentPath::from_flat_path(name.as_str());
    (path.len() == 1).then_some(name.as_str())
}

fn modifier_source_scope(name: &str) -> Option<String> {
    let variable_path = rumoca_core::ComponentPath::from_flat_path(name);
    let component_scope = variable_path.parent()?;
    let source_scope = component_scope.parent()?;
    Some(source_scope.to_flat_string())
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
    class_def_id: Option<rumoca_core::DefId>,
    component_override_map: &ComponentOverrideMap,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> Result<(), FlattenError> {
    let previous_class_scope = ctx.current_class_scope_path.clone();
    ctx.current_class_scope_path = class_def_id.and_then(|id| tree.def_map.get(&id).cloned());
    let result = process_class_instance_body(
        ctx,
        flat,
        class_data,
        component_override_map,
        tree,
        class_index,
    );
    ctx.current_class_scope_path = previous_class_scope;
    result
}

// SPEC_0021: Exception - top-level flatten phase entry point for a class instance.
#[allow(clippy::too_many_lines)]
fn process_class_instance_body(
    ctx: &mut Context,
    flat: &mut Model,
    class_data: &ClassInstanceData,
    component_override_map: &ComponentOverrideMap,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> Result<(), FlattenError> {
    let prefix = &class_data.qualified_name;
    let def_map = Some(&tree.def_map);
    let class_scope = class_data.qualified_name.to_component_path();
    let (override_packages, override_functions) =
        override_context_for_component_path(&class_scope, component_override_map);
    let override_package_names = override_package_names(&override_packages);
    let override_aliases =
        override_aliases_for_component_path(&class_scope, component_override_map);

    // Convert regular equations.
    for inst_eq in &class_data.equations {
        set_class_instance_imports_for_scope(
            ctx,
            class_data,
            tree,
            class_index,
            ImportScope {
                source_scope: inst_eq.source_scope.as_ref(),
                source_scope_id: inst_eq.source_scope_id,
                span: inst_eq.span,
            },
            &override_package_names,
            &override_aliases,
        )?;
        let inst_eq = mark_member_function_calls_in_instance_equation(
            inst_eq,
            tree,
            class_index,
            &override_functions,
        );
        // Handle when-equations separately (pass context for parameter evaluation).
        let mut clauses = when_equations::flatten_when_equation(ctx, &inst_eq, prefix, def_map)?;
        for clause in &mut clauses {
            rewrite_function_overrides_in_when_clause(
                clause,
                tree,
                class_index,
                &override_packages,
                &override_functions,
            );
        }
        flat.when_clauses.extend(clauses);

        // Handle other equations (including for-loops that may contain when-equations).
        let mut flattened =
            equations::flatten_equation_with_def_map(ctx, &inst_eq, prefix, def_map)?;
        rewrite_function_overrides_in_flattened(
            &mut flattened,
            tree,
            class_index,
            &override_packages,
            &override_functions,
        );
        let equation_base = flat.equations.len();
        for eq in flattened.equations {
            flat.add_equation(eq);
        }
        for mut for_eq in flattened.structured_equations {
            for_eq.first_equation_index += equation_base;
            flat.add_structured_equation(for_eq);
        }
        flat.assert_equations.extend(flattened.assert_equations);
        flat.when_clauses.extend(flattened.when_clauses);
        flat.definite_roots.extend(flattened.definite_roots);
        flat.branches.extend(flattened.branches);
        flat.potential_roots.extend(flattened.potential_roots);
    }

    // Convert initial equations (when-equations are rejected per EQN-006).
    for inst_eq in &class_data.initial_equations {
        set_class_instance_imports_for_scope(
            ctx,
            class_data,
            tree,
            class_index,
            ImportScope {
                source_scope: inst_eq.source_scope.as_ref(),
                source_scope_id: inst_eq.source_scope_id,
                span: inst_eq.span,
            },
            &override_package_names,
            &override_aliases,
        )?;
        let inst_eq = mark_member_function_calls_in_instance_equation(
            inst_eq,
            tree,
            class_index,
            &override_functions,
        );
        if matches!(&inst_eq.equation, rumoca_ir_ast::Equation::When(_)) {
            return Err(FlattenError::unsupported_equation(
                "when-equations are not allowed in initial equations (MLS §8.6)",
                inst_eq.span,
            ));
        }

        let mut flattened =
            equations::flatten_equation_with_def_map(ctx, &inst_eq, prefix, def_map)?;
        rewrite_function_overrides_in_flattened(
            &mut flattened,
            tree,
            class_index,
            &override_packages,
            &override_functions,
        );
        let equation_base = flat.initial_equations.len();
        for eq in flattened.equations {
            flat.add_initial_equation(eq);
        }
        for mut for_eq in flattened.structured_equations {
            for_eq.first_equation_index += equation_base;
            flat.add_initial_structured_equation(for_eq);
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
    for inst_algs in &class_data.algorithms {
        set_class_instance_imports_for_statement_block(
            ctx,
            class_data,
            tree,
            class_index,
            inst_algs,
            &override_package_names,
            &override_aliases,
        )?;
        let imports = &ctx.current_imports;
        let inst_algs = mark_member_function_calls_in_instance_statements(
            inst_algs,
            tree,
            class_index,
            &override_functions,
        );
        let instance_name = ctx.instance_name_for_prefix(prefix);
        let mut flat_alg = flatten_algorithm_section(
            &inst_algs,
            prefix,
            imports,
            def_map,
            &tree.source_map,
            instance_name.as_deref(),
        )?;
        rewrite_function_overrides_in_algorithm(
            &mut flat_alg,
            tree,
            class_index,
            &override_packages,
            &override_functions,
        );
        flat.algorithms.push(flat_alg);
    }

    // Convert initial algorithms
    for inst_algs in &class_data.initial_algorithms {
        set_class_instance_imports_for_statement_block(
            ctx,
            class_data,
            tree,
            class_index,
            inst_algs,
            &override_package_names,
            &override_aliases,
        )?;
        let imports = &ctx.current_imports;
        let inst_algs = mark_member_function_calls_in_instance_statements(
            inst_algs,
            tree,
            class_index,
            &override_functions,
        );
        let instance_name = ctx.instance_name_for_prefix(prefix);
        let mut flat_alg = flatten_algorithm_section(
            &inst_algs,
            prefix,
            imports,
            def_map,
            &tree.source_map,
            instance_name.as_deref(),
        )?;
        rewrite_function_overrides_in_algorithm(
            &mut flat_alg,
            tree,
            class_index,
            &override_packages,
            &override_functions,
        );
        flat.initial_algorithms.push(flat_alg);
    }

    Ok(())
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
    source_map: &rumoca_core::SourceMap,
    instance_name: Option<&str>,
) -> Result<Algorithm, FlattenError> {
    let span = statements
        .iter()
        .map(|statement| statement.span)
        .find(|span| !span.is_dummy())
        .ok_or_else(|| {
            FlattenError::missing_source_context(format!(
                "algorithm section for `{}` has no statement source span",
                prefix.to_flat_string()
            ))
        })?;

    // Extract raw statements from InstanceStatements
    let raw_statements: Vec<_> = statements.iter().map(|s| s.statement.clone()).collect();

    let origin = format!("algorithm from {}", prefix.to_flat_string());
    let no_locals: std::collections::HashSet<String> = std::collections::HashSet::new();

    // Use the algorithms module for qualification and output extraction
    algorithms::flatten_algorithm_section(
        &raw_statements,
        algorithms::AlgorithmSectionContext {
            prefix,
            imports,
            def_map,
            initial_locals: &no_locals,
            source_map: Some(source_map),
            instance_name,
        },
        algorithms::AlgorithmSectionMetadata::new(span, origin),
    )
}

use super::function_overrides_and_dims::*;

/// Process a component instance to create a flat variable.
///
/// Only primitive types (Real, Integer, Boolean, String) become flat variables.
/// Class types (connectors, models, records) are containers and are skipped.
pub(crate) struct ComponentInstanceProcess<'a, 'tree> {
    pub(crate) flat: &'a mut Model,
    pub(crate) instance_data: &'a rumoca_ir_ast::InstanceData,
    pub(crate) simulated_root_name: Option<&'a str>,
    pub(crate) component_override_map: &'a ComponentOverrideMap,
    pub(crate) tree: &'a rumoca_ir_ast::ClassTree,
    pub(crate) class_index: &'a rumoca_ir_ast::ClassDefIndex<'tree>,
    pub(crate) import_cache: &'a mut ImportCaches<'tree>,
    pub(crate) scope_index: &'a OverlayScopeIndex<'a>,
    pub(crate) component_members: &'a component_member_scope::ComponentMemberScopes,
}

pub(crate) fn process_component_instance(
    request: ComponentInstanceProcess<'_, '_>,
) -> Result<(), FlattenError> {
    // Skip if this is an empty path (root)
    let var_name = qualified_to_var_name(&request.instance_data.qualified_name);
    if var_name.as_str().is_empty() {
        return Ok(());
    }

    // Skip non-primitive types (class types like connectors, models, records)
    // These are containers, not scalar variables
    if !request.instance_data.is_primitive {
        return Ok(());
    }

    let import_context = variable_import_context_for_instance(
        request.instance_data,
        request.tree,
        request.class_index,
        request.import_cache,
        request.scope_index,
        request.component_override_map,
    )?;
    let mut flat_var = variables::create_flat_variable(
        request.instance_data,
        request.tree,
        request.class_index,
        &import_context,
        request.simulated_root_name,
    )?;
    assign_instance_identity_to_flat_variable(
        request.flat,
        &mut flat_var,
        request.tree,
        request.instance_data,
    );
    let instance_scope = request.instance_data.qualified_name.to_component_path();
    let (override_packages, override_functions) =
        override_context_for_component_path(&instance_scope, request.component_override_map);
    let receiver_scope = instance_scope
        .parent()
        .unwrap_or_else(rumoca_core::ComponentPath::root);
    rewrite_function_overrides_in_flat_variable(
        &mut flat_var,
        request.tree,
        request.class_index,
        &override_packages,
        &override_functions,
        &receiver_scope,
        request.component_members,
    );
    request.flat.variable_type_names.insert(
        var_name.clone(),
        variables::flat_output_type_name(request.instance_data, request.tree)?,
    );
    if request.instance_data.is_final {
        request
            .flat
            .variable_final_flags
            .insert(var_name.clone(), true);
    }
    request.flat.add_variable(var_name, flat_var);

    Ok(())
}

fn instance_source_span(
    instance_data: &rumoca_ir_ast::InstanceData,
    tree: &rumoca_ir_ast::ClassTree,
) -> Result<rumoca_core::Span, FlattenError> {
    let location = &instance_data.source_location;
    if location.file_name.is_empty() || location.start >= location.end {
        return Err(FlattenError::missing_source_context(
            "symbolic component dimensions are missing a non-empty source location",
        ));
    }
    tree.source_map
        .try_location_to_span(
            &location.file_name,
            location.start as usize,
            location.end as usize,
        )
        .ok_or_else(|| {
            FlattenError::missing_source_context(format!(
                "source file `{}` for symbolic component dimensions was not found",
                location.file_name
            ))
        })
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
) -> Result<rumoca_core::Expression, FlattenError> {
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
) -> Result<rumoca_core::Expression, FlattenError> {
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
) -> Result<rumoca_core::Expression, FlattenError> {
    // Use default options for equation qualification
    let opts = qualify::QualifyOptions {
        preserve_def_id: true,
        ..qualify::QualifyOptions::default()
    };
    let filtered_imports;
    let imports = if let Some(def_map) = def_map {
        filtered_imports = imports_without_shadowed_aliases(expr, imports, def_map);
        &filtered_imports
    } else {
        imports
    };
    qualify_expression_with_effective_imports(expr, prefix, imports, def_map, opts, None)
}

/// Like `qualify_expression_imports_with_def_map`, but receives flatten context
/// semantic metadata. The context is currently used by class-reference
/// canonicalization in qualification call sites; keeping this entry point
/// prevents those call sites from falling back to context-free qualification.
pub(crate) fn qualify_expression_imports_with_def_map_ctx(
    expr: &ast::Expression,
    prefix: &QualifiedName,
    imports: &qualify::ImportMap,
    def_map: Option<&crate::ResolveDefMap>,
    ctx: &Context,
) -> Result<rumoca_core::Expression, FlattenError> {
    let opts = qualify::QualifyOptions {
        preserve_def_id: true,
        ..qualify::QualifyOptions::default()
    };
    let def_filtered_imports;
    let imports = if let Some(def_map) = def_map {
        def_filtered_imports = imports_without_shadowed_aliases(expr, imports, def_map);
        &def_filtered_imports
    } else {
        imports
    };
    let scoped_imports = super::component_member_scope::imports_without_instance_member_aliases(
        expr, prefix, imports, ctx,
    );
    let instance_name = ctx.instance_name_for_prefix(prefix);
    qualify_expression_with_effective_imports(
        expr,
        prefix,
        &scoped_imports,
        def_map,
        opts,
        instance_name.as_deref(),
    )
}

fn qualify_expression_with_effective_imports(
    expr: &ast::Expression,
    prefix: &QualifiedName,
    imports: &qualify::ImportMap,
    def_map: Option<&crate::ResolveDefMap>,
    opts: qualify::QualifyOptions,
    instance_name: Option<&str>,
) -> Result<rumoca_core::Expression, FlattenError> {
    let qualified = qualify::qualify_expression_with_imports(expr, prefix, opts, imports);
    crate::ast_lower::expression_from_ast_with_context(
        &qualified,
        crate::ast_lower::LoweringContext {
            def_map,
            instance_name,
        },
    )
}

#[cfg(test)]
mod import_shadow_tests;
