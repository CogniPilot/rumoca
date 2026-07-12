use super::*;

pub(super) fn active_constant_def_overrides(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    active_function: &ast::ClassDef,
    def_map: &crate::ResolveDefMap,
) -> crate::ResolveDefMap {
    let Some(active_function_def_id) = active_function.def_id else {
        return crate::ResolveDefMap::default();
    };
    let Some(active_package_def_id) = class_index.parent_def_id(active_function_def_id) else {
        return crate::ResolveDefMap::default();
    };
    let Some(active_package) = tree.def_map.get(&active_package_def_id) else {
        return crate::ResolveDefMap::default();
    };
    let mut package_chain = Vec::new();
    let mut visited = FxHashSet::default();
    collect_package_chain(
        tree,
        class_index,
        active_package,
        &mut package_chain,
        &mut visited,
    );
    if package_chain.is_empty() {
        package_chain.push(active_package_def_id);
    }
    let package_chain: FxHashSet<rumoca_core::DefId> = package_chain.into_iter().collect();
    def_map
        .iter()
        .filter_map(|(def_id, _resolved_path)| {
            let source_def_id = class_index.parent_def_id(*def_id)?;
            if source_def_id == active_package_def_id {
                return None;
            }
            if !package_chain.contains(&source_def_id) {
                return None;
            }
            let source_class = class_index.get(source_def_id)?;
            let leaf = class_index.local_name(*def_id)?;
            let component = source_class.components.get(leaf)?;
            if component.def_id != Some(*def_id)
                || !matches!(
                    component.variability,
                    rumoca_core::Variability::Constant(_) | rumoca_core::Variability::Parameter(_)
                )
            {
                return None;
            }
            Some((*def_id, format!("{active_package}.{leaf}")))
        })
        .collect()
}
