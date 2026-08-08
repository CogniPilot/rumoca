//! Enumeration parameter evaluation for the flatten context: literal
//! extraction, reference chasing through parameters and record aliases, and
//! normalization to final literal values (MLS §4.9.5).

use super::param_binding::{ParamBinding, is_plain_component_reference};
use super::*;

impl Context {
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

    pub(super) fn eval_enum_param_bindings(&mut self, params: &[ParamBinding<'_>]) -> bool {
        let param_names: rustc_hash::FxHashSet<&str> =
            params.iter().map(|binding| binding.name).collect();

        let mut progress = false;
        loop {
            let new_vals = self.collect_enum_values(params, &param_names);
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
    ) -> Vec<(String, String)> {
        params
            .iter()
            .filter_map(|ParamBinding { name, binding, .. }| {
                self.resolve_enum_binding_value(binding, param_names)
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
    ) -> Option<String> {
        let enum_val = self.try_eval_enum_binding(binding)?;
        if !self.enum_reference_matches_parameter(&enum_val, param_names) {
            return Some(enum_val);
        }
        self.resolve_non_parameter_enum_varref(binding, param_names)
    }

    fn try_eval_enum_binding(&self, binding: &Expression) -> Option<String> {
        // A bare component reference is resolved structurally: its own
        // composite name, then the record aliases that carry `outer`/inherited
        // bindings. The shared flat evaluator would additionally accept a
        // unique-suffix match found by dropping leading path segments, which
        // MLS 3.7 §5.3.2 does not permit: every identifier after the first
        // must name an element of the instance found so far, so `pipe1.system.x`
        // is an element of `pipe1` and never the top-level `system.x` unless an
        // alias states that `pipe1.system` *is* `system`.
        if is_plain_component_reference(binding) {
            return self.resolve_varref_enum_reference(binding);
        }
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
        let Expression::VarRef {
            name, subscripts, ..
        } = binding
        else {
            return None;
        };
        if !subscripts.is_empty() {
            return None;
        }
        self.resolve_enum_reference_value(name.as_str())
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
}
