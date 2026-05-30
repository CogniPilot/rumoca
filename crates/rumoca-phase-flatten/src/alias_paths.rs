use rumoca_core::ComponentPath;
use rustc_hash::FxHashMap;

pub(crate) fn resolve_component_alias_once(
    current: &ComponentPath,
    source: Option<&ComponentPath>,
    aliases: &FxHashMap<ComponentPath, ComponentPath>,
) -> Option<ComponentPath> {
    for prefix_len in (1..=current.len()).rev() {
        let prefix_parts = &current.parts()[..prefix_len];
        let Some(alias_target) = aliases.get(prefix_parts) else {
            continue;
        };
        let resolved = alias_target.join_part_slice(&current.parts()[prefix_len..]);
        if &resolved == current || source.is_some_and(|source| &resolved == source) {
            continue;
        }
        return Some(resolved);
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolves_exact_alias_as_full_length_prefix() {
        let mut aliases = FxHashMap::default();
        aliases.insert(
            ComponentPath::from_flat_path("pipe.system"),
            ComponentPath::from_flat_path("system"),
        );

        let resolved = resolve_component_alias_once(
            &ComponentPath::from_flat_path("pipe.system"),
            None,
            &aliases,
        );

        assert_eq!(resolved, Some(ComponentPath::from_flat_path("system")));
    }

    #[test]
    fn resolves_field_alias_from_structured_prefix() {
        let mut aliases = FxHashMap::default();
        aliases.insert(
            ComponentPath::from_flat_path("pipe.system"),
            ComponentPath::from_flat_path("system"),
        );

        let resolved = resolve_component_alias_once(
            &ComponentPath::from_flat_path("pipe.system.energyDynamics"),
            None,
            &aliases,
        );

        assert_eq!(
            resolved,
            Some(ComponentPath::from_flat_path("system.energyDynamics"))
        );
    }

    #[test]
    fn ignores_dot_inside_subscript_expression() {
        let mut aliases = FxHashMap::default();
        aliases.insert(
            ComponentPath::from_flat_path("pipe.system[data"),
            ComponentPath::from_flat_path("bad"),
        );

        let resolved = resolve_component_alias_once(
            &ComponentPath::from_flat_path("pipe.system[data.medium].energyDynamics"),
            None,
            &aliases,
        );

        assert_eq!(resolved, None);
    }
}
