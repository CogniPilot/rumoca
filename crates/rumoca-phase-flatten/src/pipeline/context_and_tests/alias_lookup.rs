//! Record-alias resolution and scoped name lookup: the alias chain walk
//! (MLS §7.2.3), the enclosing-scope candidate order (MLS §5.3), and the
//! `EvalLookup` implementation constant evaluation resolves names through.

use super::*;

impl Context {
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
    pub(super) fn resolve_alias(&self, name: &str) -> String {
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
