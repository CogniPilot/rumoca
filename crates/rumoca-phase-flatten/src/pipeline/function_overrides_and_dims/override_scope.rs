//! Scope lookup over the component override map: the package and
//! function aliases visible from a component path.

use super::*;

pub(crate) fn override_context_for_scope(
    scope: &str,
    component_override_map: &ComponentOverrideMap,
) -> (Vec<OverrideTarget>, OverrideFunctionMap) {
    let scope_path = ComponentPath::from_flat_path(scope);
    override_context_for_component_path(&scope_path, component_override_map)
}

pub(crate) fn override_context_for_component_path(
    scope_path: &ComponentPath,
    component_override_map: &ComponentOverrideMap,
) -> (Vec<OverrideTarget>, OverrideFunctionMap) {
    fn apply_scope_override(
        alias: &str,
        target: &OverrideTarget,
        packages: &mut Vec<OverrideTarget>,
        function_overrides: &mut OverrideFunctionMap,
    ) {
        if target.is_package() {
            let mut package_target = target.clone();
            package_target.alias = alias.to_string();
            update_package_override_slot(packages, &package_target);
        }
        update_function_override_entry(function_overrides, alias, target);
    }

    if component_override_map.is_empty() {
        return (Vec::new(), OverrideFunctionMap::default());
    }
    let estimated_overrides = override_scope_entry_count(scope_path, component_override_map);
    let mut packages = Vec::new();
    let mut function_overrides = OverrideFunctionMap::default();
    packages.reserve(estimated_overrides);
    function_overrides.reserve(estimated_overrides);
    for path in scope_chain_inner_to_outer(scope_path) {
        if let Some(path_overrides) = component_override_map.get(&path) {
            for (alias, target) in &path_overrides.by_alias {
                apply_scope_override(alias, target, &mut packages, &mut function_overrides);
            }
            for target in &path_overrides.exact_packages {
                apply_scope_override(
                    &target.alias,
                    target,
                    &mut packages,
                    &mut function_overrides,
                );
            }
        }
    }
    if let Some(path_overrides) = root_override_entries(component_override_map) {
        for (alias, target) in &path_overrides.by_alias {
            apply_scope_override(alias, target, &mut packages, &mut function_overrides);
        }
        for target in &path_overrides.exact_packages {
            apply_scope_override(
                &target.alias,
                target,
                &mut packages,
                &mut function_overrides,
            );
        }
    }
    (packages, function_overrides)
}

fn update_function_override_entry(
    function_overrides: &mut OverrideFunctionMap,
    alias: &str,
    target: &OverrideTarget,
) {
    match function_overrides.get(alias) {
        Some(existing) if target.active && !existing.active => {
            function_overrides.insert(alias.to_string(), target.clone());
        }
        Some(_) => {}
        None => {
            function_overrides.insert(alias.to_string(), target.clone());
        }
    }
}

fn update_package_override_slot(packages: &mut Vec<OverrideTarget>, target: &OverrideTarget) {
    if let Some(existing) = packages.iter_mut().find(|existing| {
        existing.alias_def_id == target.alias_def_id
            && (target.alias_def_id.is_some() || existing.def_id == target.def_id)
    }) {
        if target.active && !existing.active {
            *existing = target.clone();
        }
        return;
    }
    packages.push(target.clone());
}

pub(crate) fn override_aliases_for_component_path(
    scope_path: &ComponentPath,
    component_override_map: &ComponentOverrideMap,
) -> Vec<(String, String)> {
    let (packages, _) = override_context_for_component_path(scope_path, component_override_map);
    packages
        .into_iter()
        .map(|target| (target.alias, target.name))
        .collect()
}

pub(crate) fn override_package_names(override_packages: &[OverrideTarget]) -> Vec<String> {
    override_package_names_with_preferred_aliases(override_packages, &[])
}

pub(crate) fn override_package_names_with_preferred_aliases(
    override_packages: &[OverrideTarget],
    preferred_aliases: &[String],
) -> Vec<String> {
    let mut names = Vec::with_capacity(override_packages.len());
    for alias in preferred_aliases {
        names.extend(
            override_packages
                .iter()
                .filter(|target| &target.alias == alias)
                .map(|target| target.name.clone()),
        );
    }
    override_packages
        .iter()
        .filter(|target| !preferred_aliases.iter().any(|alias| alias == &target.alias))
        .map(|target| target.name.clone())
        .for_each(|name| names.push(name));
    names
}

/// Scope prefixes of `scope_path`, innermost first.
///
/// Yields owned paths rather than `&[String]` slices: a `ComponentPath` probes
/// the override map by its interned identity, while a slice probe hashed every
/// segment. Callers guard the walk with an emptiness check so the usual
/// override-free model does no prefix work at all.
fn scope_chain_inner_to_outer(
    scope_path: &ComponentPath,
) -> impl Iterator<Item = ComponentPath> + '_ {
    (1..=scope_path.len())
        .rev()
        .filter_map(|end| scope_path.prefix(end))
}

fn root_override_entries(
    component_override_map: &ComponentOverrideMap,
) -> Option<&OverrideEntries> {
    component_override_map.get(&ComponentPath::root())
}

fn override_scope_entry_count(
    scope_path: &ComponentPath,
    component_override_map: &ComponentOverrideMap,
) -> usize {
    if component_override_map.is_empty() {
        return 0;
    }
    let scoped_count = scope_chain_inner_to_outer(scope_path)
        .filter_map(|path| component_override_map.get(&path))
        .map(OverrideEntries::len)
        .sum::<usize>();
    scoped_count
        + root_override_entries(component_override_map)
            .map(OverrideEntries::len)
            .unwrap_or(0)
}

pub(super) fn override_context_cache_key(
    scope_path: &ComponentPath,
    component_override_map: &ComponentOverrideMap,
) -> ComponentPath {
    for end in (1..=scope_path.len()).rev() {
        let Some(prefix) = scope_path.prefix(end) else {
            continue;
        };
        if component_override_map.contains_key(&prefix) {
            return prefix;
        }
    }
    ComponentPath::root()
}
