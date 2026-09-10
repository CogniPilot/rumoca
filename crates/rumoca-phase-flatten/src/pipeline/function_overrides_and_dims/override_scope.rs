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
    fn apply_scope_override<'a>(
        alias: &'a str,
        target: &OverrideTarget,
        packages: &mut Vec<OverrideTarget>,
        package_aliases: &mut rustc_hash::FxHashMap<&'a str, usize>,
        function_overrides: &mut OverrideFunctionMap,
    ) {
        if target.is_package() {
            if let Some(index) = package_aliases.get(alias).copied() {
                update_package_override_slot(packages, index, target);
            } else {
                package_aliases.insert(alias, packages.len());
                packages.push(target.clone());
            }
        }
        update_function_override_entry(function_overrides, alias, target);
    }

    if component_override_map.is_empty() {
        return (Vec::new(), OverrideFunctionMap::default());
    }
    let estimated_overrides = override_scope_entry_count(scope_path, component_override_map);
    let mut packages = Vec::new();
    let mut package_aliases = rustc_hash::FxHashMap::default();
    let mut function_overrides = OverrideFunctionMap::default();
    packages.reserve(estimated_overrides);
    package_aliases.reserve(estimated_overrides);
    function_overrides.reserve(estimated_overrides);
    for path in scope_chain_inner_to_outer(scope_path) {
        if let Some(path_overrides) = component_override_map.get(&path) {
            for (alias, target) in path_overrides {
                apply_scope_override(
                    alias,
                    target,
                    &mut packages,
                    &mut package_aliases,
                    &mut function_overrides,
                );
            }
        }
    }
    if let Some(path_overrides) = root_override_entries(component_override_map) {
        for (alias, target) in path_overrides {
            apply_scope_override(
                alias,
                target,
                &mut packages,
                &mut package_aliases,
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

fn update_package_override_slot(
    packages: &mut [OverrideTarget],
    index: usize,
    target: &OverrideTarget,
) {
    if target.active && !packages[index].active {
        packages[index] = target.clone();
    }
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
) -> Option<&rustc_hash::FxHashMap<String, OverrideTarget>> {
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
        .map(rustc_hash::FxHashMap::len)
        .sum::<usize>();
    scoped_count
        + root_override_entries(component_override_map)
            .map(rustc_hash::FxHashMap::len)
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
