//! Flatten-context construction and the parameter lookup table it builds from
//! a flat model: binding collection, record-alias supplementation, and the
//! multi-pass fixpoint that drives the per-kind evaluation passes.

use super::param_binding::{ParamBinding, ParamPrimitive};
use super::*;

struct ResolvedFlatVariable<'a> {
    name: &'a rumoca_core::VarName,
    variable: &'a rumoca_ir_flat::Variable,
    identity: rumoca_eval_flat::constant::ResolvedOccurrenceKey,
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

impl Context {
    /// Create a new flatten context.
    pub(crate) fn new() -> Self {
        Self {
            parameter_values: rustc_hash::FxHashMap::default(),
            real_parameter_values: rustc_hash::FxHashMap::default(),
            boolean_parameter_values: rustc_hash::FxHashMap::default(),
            enum_parameter_values: rustc_hash::FxHashMap::default(),
            resolved_enum_catalog: rumoca_eval_flat::constant::ResolvedEnumCatalog::empty(),
            parameter_values_by_identity: rustc_hash::FxHashMap::default(),
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
            array_dimensions_by_identity: rustc_hash::FxHashMap::default(),
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
            current_import_refusals: super::super::import_scopes::ImportRefusalMap::default(),
            class_def_ids: std::sync::Arc::new(rustc_hash::FxHashSet::default()),
            current_class_scope_path: None,
            current_class_instance_id: None,
            simulated_root_name: None,
            materialize_structured_families: true,
            param_variability_families:
                crate::param_variability::ParameterVariabilityFamilies::default(),
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
    pub(crate) fn build_parameter_lookup(
        &mut self,
        flat: &Model,
        tree: &ClassTree,
    ) -> Result<(), FlattenError> {
        let flat_variables = Self::collect_flat_variables(flat)?;
        self.resolved_enum_catalog =
            super::super::constant_injection::resolved_tree_enum_catalog(tree)?;

        self.seed_flat_parameter_constant_keys(flat);
        let params = self.collect_parameters(flat, &flat_variables);

        self.supplement_record_aliases(&params);
        crate::compute_transitive_alias_closure(&mut self.record_aliases)?;
        self.init_array_dimensions(flat)?;

        let var_bindings = Self::collect_var_bindings(flat, &flat_variables);
        self.infer_dims_from_literals(flat)?;
        self.sync_flat_identity_inventories(&flat_variables);

        // Multi-pass evaluation until fixpoint.
        self.run_multipass_evaluation(&params, &var_bindings)
    }

    fn collect_flat_variables(flat: &Model) -> Result<Vec<ResolvedFlatVariable<'_>>, FlattenError> {
        flat.variables
            .iter()
            .map(|(name, variable)| {
                let Some(reference) = variable.component_ref.as_ref() else {
                    return Err(FlattenError::missing_flat_variable_identity(
                        name.as_str(),
                        variable.source_span,
                    ));
                };
                Ok(ResolvedFlatVariable {
                    name,
                    variable,
                    identity: rumoca_eval_flat::constant::ResolvedOccurrenceKey {
                        instance_id: variable.instance_id,
                        root_def_id: reference.root_def_id(),
                    },
                })
            })
            .collect()
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
    pub(crate) fn refresh_enum_parameter_lookup(
        &mut self,
        flat: &Model,
    ) -> Result<(), FlattenError> {
        let flat_variables = Self::collect_flat_variables(flat)?;
        let params = self.collect_parameters(flat, &flat_variables);
        self.eval_enum_param_bindings(&params).map(|_| ())
    }

    /// Collect parameters with bindings (MLS §4.5, §8.6).
    ///
    /// Also collects non-parameter Integer/Boolean variables with bindings
    /// (e.g., `Integer nX = size(X_boundary, 1)`) so their values are available
    /// for for-equation range evaluation (MLS §8.3.3).
    fn collect_parameters<'a>(
        &mut self,
        flat: &'a Model,
        variables: &[ResolvedFlatVariable<'a>],
    ) -> Vec<ParamBinding<'a>> {
        variables
            .iter()
            .filter(|entry| {
                let var = entry.variable;
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
            .filter_map(|entry| {
                let name = entry.name;
                let var = entry.variable;
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
                    identity: entry.identity,
                    binding,
                    primitive: ParamPrimitive::from_variable(flat, var),
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
    fn collect_var_bindings<'a>(
        flat: &'a Model,
        variables: &[ResolvedFlatVariable<'a>],
    ) -> Vec<ParamBinding<'a>> {
        variables
            .iter()
            .filter_map(|entry| {
                let name = entry.name;
                let var = entry.variable;
                var.binding.as_ref().map(|binding| ParamBinding {
                    name: name.as_str(),
                    identity: entry.identity,
                    binding,
                    primitive: ParamPrimitive::from_variable(flat, var),
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
    ) -> Result<(), FlattenError> {
        let ordered_params = dependency_ordered_parameters(params, &self.resolved_enum_catalog)?;
        loop {
            let enum_progress = self.eval_enum_param_bindings(&ordered_params)?;
            let real_progress = self.eval_real_params(&ordered_params)?;
            let int_progress = self.eval_integer_param_bindings(&ordered_params)?;
            let bool_progress = self.eval_boolean_params(&ordered_params)?;
            let dim_progress = self.eval_array_dimensions(var_bindings)?;
            let varref_dim_progress = self.propagate_varref_dimensions(var_bindings);
            let alias_progress = self.propagate_through_aliases(params);
            self.sync_binding_identity_inventories(params, var_bindings);
            if !enum_progress
                && !real_progress
                && !int_progress
                && !bool_progress
                && !dim_progress
                && !varref_dim_progress
                && !alias_progress
            {
                return Ok(());
            }
        }
    }

    fn sync_flat_identity_inventories(&mut self, variables: &[ResolvedFlatVariable<'_>]) {
        for entry in variables {
            self.sync_identity_inventory_entry(entry.name.as_str(), entry.identity);
        }
    }

    fn sync_binding_identity_inventories(
        &mut self,
        params: &[ParamBinding<'_>],
        var_bindings: &[ParamBinding<'_>],
    ) {
        for binding in params.iter().chain(var_bindings) {
            self.sync_identity_inventory_entry(binding.name, binding.identity);
        }
    }

    fn sync_identity_inventory_entry(
        &mut self,
        name: &str,
        identity: rumoca_eval_flat::constant::ResolvedOccurrenceKey,
    ) {
        let value = if let Some(value) = self.parameter_values.get(name) {
            Some(rumoca_eval_flat::constant::Value::Integer(*value))
        } else if let Some(value) = self.real_parameter_values.get(name) {
            Some(rumoca_eval_flat::constant::Value::Real(*value))
        } else if let Some(value) = self.boolean_parameter_values.get(name) {
            Some(rumoca_eval_flat::constant::Value::Bool(*value))
        } else {
            self.enum_parameter_values
                .get(name)
                .map(|value| rumoca_eval_flat::constant::Value::ResolvedEnum(value.clone()))
        };
        if let Some(value) = value {
            self.parameter_values_by_identity.insert(identity, value);
        }
        if let Some(dimensions) = self.array_dimensions.get(name) {
            self.array_dimensions_by_identity
                .insert(identity, dimensions.clone());
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
        for ParamBinding {
            name, primitive, ..
        } in params
        {
            let resolved = self.resolve_alias(name);
            if resolved == *name {
                continue; // No alias applies
            }

            progress |= self.propagate_alias_value(name, &resolved, *primitive);

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
        }

        progress
    }

    fn propagate_alias_value(
        &mut self,
        name: &str,
        resolved: &str,
        primitive: ParamPrimitive,
    ) -> bool {
        match primitive {
            ParamPrimitive::Integer => {
                if self.parameter_values.contains_key(name) {
                    return false;
                }
                let Some(value) = self.parameter_values.get(resolved).copied() else {
                    return false;
                };
                self.parameter_values.insert(name.to_string(), value);
                true
            }
            ParamPrimitive::Real => {
                if self.real_parameter_values.contains_key(name) {
                    return false;
                }
                let Some(value) = self.real_parameter_values.get(resolved).copied() else {
                    return false;
                };
                self.real_parameter_values.insert(name.to_string(), value);
                true
            }
            ParamPrimitive::Boolean => {
                if self.boolean_parameter_values.contains_key(name) {
                    return false;
                }
                let Some(value) = self.boolean_parameter_values.get(resolved).copied() else {
                    return false;
                };
                self.boolean_parameter_values
                    .insert(name.to_string(), value);
                true
            }
            ParamPrimitive::Enumeration => {
                if self.enum_parameter_values.contains_key(name) {
                    return false;
                }
                let Some(value) = self.enum_parameter_values.get(resolved).cloned() else {
                    return false;
                };
                self.enum_parameter_values.insert(name.to_string(), value);
                true
            }
            ParamPrimitive::Other | ParamPrimitive::Unknown => false,
        }
    }
}

fn dependency_ordered_parameters<'a>(
    params: &[ParamBinding<'a>],
    enum_catalog: &rumoca_eval_flat::constant::ResolvedEnumCatalog,
) -> Result<Vec<ParamBinding<'a>>, FlattenError> {
    let indices = params
        .iter()
        .enumerate()
        .map(|(index, binding)| (binding.identity, index))
        .collect::<rustc_hash::FxHashMap<_, _>>();
    let mut dependencies = Vec::with_capacity(params.len());
    for binding in params {
        let mut collector = ExactDependencyCollector::new(enum_catalog);
        collector.visit_expression(binding.binding);
        if let Some(name) = collector.malformed_reference {
            return Err(FlattenError::internal(format!(
                "post-Resolve parameter binding `{}` contains identity-free reference `{name}`",
                binding.name
            )));
        }
        let mut edges = collector
            .dependencies
            .into_iter()
            .filter_map(|identity| indices.get(&identity).copied())
            .collect::<Vec<_>>();
        edges.sort_unstable();
        edges.dedup();
        dependencies.push(edges);
    }
    let components = rumoca_core::dependency_first_sccs(&dependencies)
        .map_err(|error| FlattenError::internal(error.to_string()))?;
    let mut ordered = Vec::with_capacity(params.len());
    for component in components {
        if component.recursive {
            let names = component
                .members
                .iter()
                .map(|index| params[*index].name)
                .collect::<Vec<_>>()
                .join(" -> ");
            return Err(FlattenError::internal(format!(
                "cyclic post-Resolve parameter dependency: {names}"
            )));
        }
        ordered.extend(component.members.iter().map(|index| params[*index]));
    }
    Ok(ordered)
}

struct ExactDependencyCollector<'a> {
    enum_catalog: &'a rumoca_eval_flat::constant::ResolvedEnumCatalog,
    dependencies: Vec<rumoca_eval_flat::constant::ResolvedOccurrenceKey>,
    malformed_reference: Option<String>,
}

impl<'a> ExactDependencyCollector<'a> {
    fn new(enum_catalog: &'a rumoca_eval_flat::constant::ResolvedEnumCatalog) -> Self {
        Self {
            enum_catalog,
            dependencies: Vec::new(),
            malformed_reference: None,
        }
    }
}

impl rumoca_core::ExpressionVisitor for ExactDependencyCollector<'_> {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        if name.structured_binder().is_some() {
            self.walk_var_ref(name, subscripts);
            return;
        }
        match (name.instance_id(), name.root_def_id()) {
            (Some(instance_id), Some(root_def_id)) => {
                self.dependencies
                    .push(rumoca_eval_flat::constant::ResolvedOccurrenceKey {
                        instance_id,
                        root_def_id,
                    });
            }
            _ if self.enum_catalog.contains_reference(name) => {}
            _ => {
                if self.malformed_reference.is_none() {
                    self.malformed_reference = Some(name.as_str().to_string());
                }
            }
        }
        self.walk_var_ref(name, subscripts);
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests;
