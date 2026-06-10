use super::*;

impl<'a> LowerBuilder<'a> {
    /// A generated scope key for an unknown reference at a dummy span, when
    /// a matching fixture binding exists. Test fixtures construct layouts
    /// without DAE variable metadata, so this stands in for the production
    /// contract that every synthesized reference resolves to a DAE variable.
    pub(super) fn test_fixture_generated_scope_key(
        &self,
        name: &rumoca_core::Reference,
        span: rumoca_core::Span,
    ) -> Option<ComponentReferenceKey> {
        if !span.is_dummy() {
            return None;
        }
        let generated = ComponentReferenceKey::generated(name.as_str());
        self.test_fixture_generated_binding_available(name.as_str(), &generated)
            .then_some(generated)
    }

    pub(super) fn test_fixture_generated_binding_available(
        &self,
        display_key: &str,
        generated_key: &ComponentReferenceKey,
    ) -> bool {
        self.layout.binding(display_key).is_some()
            || self.indexed_bindings.contains_key(generated_key)
            || self.direct_assignments.contains_key(display_key)
            || self
                .direct_assignments
                .keys()
                .any(|candidate| test_fixture_scalarized_path_match(display_key, candidate))
            || self
                .layout
                .bindings()
                .keys()
                .any(|candidate| test_fixture_scalarized_path_match(display_key, candidate))
    }
}

fn test_fixture_scalarized_path_match(requested: &str, candidate: &str) -> bool {
    let requested_parts = rumoca_core::split_path_with_indices(requested);
    let candidate_parts = rumoca_core::split_path_with_indices(candidate);
    requested_parts.len() == candidate_parts.len()
        && requested_parts
            .iter()
            .zip(candidate_parts.iter())
            .all(|(requested, candidate)| {
                test_fixture_segment_base(requested) == test_fixture_segment_base(candidate)
            })
}

fn test_fixture_segment_base(segment: &str) -> &str {
    rumoca_core::parse_scalar_name(segment)
        .map(|scalar| scalar.base)
        .unwrap_or(segment)
}
