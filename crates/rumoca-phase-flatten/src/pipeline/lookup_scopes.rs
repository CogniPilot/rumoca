use super::*;

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
