//! Flatten-context construction and the parameter lookup table it builds from
//! a flat model: binding collection, record-alias supplementation, and the
//! multi-pass fixpoint that drives the per-kind evaluation passes.

use super::param_binding::ParamBinding;
use super::*;

fn insert_record_alias(
    aliases: &mut rustc_hash::FxHashMap<rumoca_core::ComponentPath, rumoca_core::ComponentPath>,
    source_path: rumoca_core::ComponentPath,
    alias_target: &rumoca_core::Reference,
) {
    aliases
        .entry(source_path)
        .or_insert_with(|| rumoca_core::ComponentPath::from_flat_path(alias_target.as_str()));
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
            constant_values_by_def_id: rustc_hash::FxHashMap::default(),
            constant_values_by_occurrence: rustc_hash::FxHashMap::default(),
            class_owner_components: rustc_hash::FxHashMap::default(),
            component_instance_references: rustc_hash::FxHashMap::default(),
            root_class_instance: None,
            target_def_names: rustc_hash::FxHashMap::default(),
            predefined_string_declaration: None,
            predefined_intrinsics: ast_lower::PredefinedIntrinsicIds::default(),
            modified_constant_keys: rustc_hash::FxHashSet::default(),
            flat_parameter_constant_keys: rustc_hash::FxHashSet::default(),
            expanded_component_keys: rustc_hash::FxHashSet::default(),
            array_dimensions: rustc_hash::FxHashMap::default(),
            structural_params: std::collections::HashSet::new(),
            non_structural_params: std::collections::HashSet::new(),
            functions: rustc_hash::FxHashMap::default(),
            record_aliases: rustc_hash::FxHashMap::default(),
            component_members: component_member_scope::ComponentMemberScopes::default(),
            vcg_is_root: rustc_hash::FxHashMap::default(),
            vcg_rooted: rustc_hash::FxHashMap::default(),
            cardinality_counts: rustc_hash::FxHashMap::default(),
            eval_fallback_context: std::cell::OnceCell::new(),
            current_imports: crate::qualify::ImportMap::default(),
            class_def_ids: std::sync::Arc::new(rustc_hash::FxHashSet::default()),
            current_class_scope_path: None,
            simulated_root_name: None,
            materialize_structured_families: true,
            param_variability_family_bases: rustc_hash::FxHashSet::default(),
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
        self.seed_expanded_component_keys(flat);
    }

    /// Record the component paths the flat model materialized only through their
    /// members, so constant folding leaves references to them symbolic.
    ///
    /// A record-valued parameter such as `src.Phi` is instantiated as
    /// `src.Phi.re` / `src.Phi.im`; the whole-record path never becomes a flat
    /// variable, so `flat_parameter_constant_keys` does not cover it and folding
    /// would fall back to the declaration default recorded from the class body,
    /// discarding the component modification (MLS §7.2.4).
    pub(crate) fn seed_expanded_component_keys(&mut self, flat: &Model) {
        let names: rustc_hash::FxHashSet<String> =
            flat.variables.keys().map(|name| name.to_string()).collect();
        let prefixes = names
            .iter()
            .flat_map(|name| name.match_indices('.').map(|(offset, _)| &name[..offset]))
            .filter(|prefix| !names.contains(*prefix));
        for prefix in prefixes {
            if !self.expanded_component_keys.contains(prefix) {
                self.expanded_component_keys.insert(prefix.to_string());
            }
        }
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
                span,
            } = binding
                && span.is_dummy()
                && subscripts.is_empty()
            {
                let source_path = rumoca_core::ComponentPath::from_flat_path(name);
                insert_record_alias(&mut self.record_aliases, source_path, alias_target);
            }
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
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}
