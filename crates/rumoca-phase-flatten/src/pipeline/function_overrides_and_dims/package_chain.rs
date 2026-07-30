//! Package inheritance chain walks used to resolve functions and
//! members through a selected package.

use super::*;

pub(crate) fn collect_package_chain(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    package_name: &str,
    chain: &mut Vec<rumoca_core::DefId>,
    visited: &mut FxHashSet<rumoca_core::DefId>,
) {
    let Some(class_def) = class_index.get_by_qualified_name(package_name) else {
        return;
    };
    collect_package_chain_from_class(tree, class_index, class_def, chain, visited);
}

fn collect_package_chain_from_class(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    class_def: &rumoca_ir_ast::ClassDef,
    chain: &mut Vec<rumoca_core::DefId>,
    visited: &mut FxHashSet<rumoca_core::DefId>,
) {
    let Some(def_id) = class_def.def_id else {
        return;
    };
    if !visited.insert(def_id) {
        return;
    }
    chain.push(def_id);
    let package_name = tree.def_map.get(&def_id).map(String::as_str);
    for ext in &class_def.extends {
        let Some(base_def_id) = ext.base_def_id.or(ext.base_name.def_id).or_else(|| {
            let package_name = package_name?;
            let base_name = ext.base_name.to_string();
            resolve_class_in_scope_indexed(class_index, &base_name, package_name)
                .0
                .and_then(|class_def| class_def.def_id)
        }) else {
            continue;
        };
        if let Some(base_class) = class_index.get(base_def_id) {
            collect_package_chain_from_class(tree, class_index, base_class, chain, visited);
        }
    }
}

pub(crate) fn package_chain_contains(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    package_name: &str,
    query_prefix: &str,
) -> bool {
    let mut chain = Vec::new();
    let mut visited = FxHashSet::default();
    collect_package_chain(tree, class_index, package_name, &mut chain, &mut visited);
    let Some(query_def_id) = class_index
        .get_by_qualified_name(query_prefix)
        .and_then(|class_def| class_def.def_id)
    else {
        return false;
    };
    chain.contains(&query_def_id)
}

pub(super) fn package_chain_contains_def_id(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    package: &OverrideTarget,
    query_def_id: rumoca_core::DefId,
) -> bool {
    let mut chain = Vec::new();
    let mut visited = FxHashSet::default();
    collect_package_chain(tree, class_index, &package.name, &mut chain, &mut visited);
    chain.contains(&query_def_id)
}

pub(crate) fn resolve_function_in_package_chain(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    package: &OverrideTarget,
    function_leaf: &str,
) -> Option<String> {
    fn resolve_inner(
        tree: &ClassTree,
        class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
        class_def: &rumoca_ir_ast::ClassDef,
        function_leaf: &str,
        visited: &mut FxHashSet<String>,
    ) -> Option<String> {
        let package_name = class_def
            .def_id
            .and_then(|def_id| tree.def_map.get(&def_id))
            .map(String::as_str)?;
        if !visited.insert(package_name.to_string()) {
            return None;
        }

        let direct = format!("{package_name}.{function_leaf}");
        if let Some(function_def) = class_index.get_by_qualified_name(&direct)
            && function_def.class_type == rumoca_core::ClassType::Function
        {
            return Some(direct);
        }

        for ext in &class_def.extends {
            let Some(base_def_id) = ext.base_def_id.or(ext.base_name.def_id).or_else(|| {
                let base_name = ext.base_name.to_string();
                resolve_class_in_scope_indexed(class_index, &base_name, package_name)
                    .0
                    .and_then(|class_def| class_def.def_id)
            }) else {
                continue;
            };
            if let Some(base_class) = class_index.get(base_def_id)
                && let Some(found) =
                    resolve_inner(tree, class_index, base_class, function_leaf, visited)
            {
                return Some(found);
            }
        }

        None
    }

    let mut visited = FxHashSet::default();
    let class_def = class_index.get(package.def_id)?;
    resolve_inner(tree, class_index, class_def, function_leaf, &mut visited)
}

pub(crate) fn resolve_function_in_package_chain_exposed(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    package: &OverrideTarget,
    function_leaf: &str,
) -> Option<String> {
    fn resolve_inner(
        tree: &ClassTree,
        class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
        class_def: &rumoca_ir_ast::ClassDef,
        exposed_package_name: &str,
        function_leaf: &str,
        visited: &mut FxHashSet<String>,
    ) -> Option<String> {
        let package_name = class_def
            .def_id
            .and_then(|def_id| tree.def_map.get(&def_id))
            .map(String::as_str)?;
        if !visited.insert(package_name.to_string()) {
            return None;
        }

        let direct = format!("{package_name}.{function_leaf}");
        let exposed = format!("{exposed_package_name}.{function_leaf}");
        if let Some(function_def) = class_index.get_by_qualified_name(&direct)
            && function_def.class_type == rumoca_core::ClassType::Function
        {
            return Some(exposed);
        }

        for ext in &class_def.extends {
            let Some(base_def_id) = ext.base_def_id.or(ext.base_name.def_id).or_else(|| {
                let base_name = ext.base_name.to_string();
                resolve_class_in_scope_indexed(class_index, &base_name, package_name)
                    .0
                    .and_then(|class_def| class_def.def_id)
            }) else {
                continue;
            };
            if let Some(base_class) = class_index.get(base_def_id)
                && resolve_inner(
                    tree,
                    class_index,
                    base_class,
                    exposed_package_name,
                    function_leaf,
                    visited,
                )
                .is_some()
            {
                return Some(exposed);
            }
        }

        None
    }

    let mut visited = FxHashSet::default();
    let class_def = class_index.get(package.def_id)?;
    resolve_inner(
        tree,
        class_index,
        class_def,
        &package.name,
        function_leaf,
        &mut visited,
    )
}

pub(super) fn resolve_member_in_package_chain_exposed(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    package: &OverrideTarget,
    member_leaf: &str,
) -> Option<String> {
    fn resolve_inner(
        tree: &ClassTree,
        class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
        class_def: &rumoca_ir_ast::ClassDef,
        exposed_package_name: &str,
        member_leaf: &str,
        visited: &mut FxHashSet<rumoca_core::DefId>,
    ) -> Option<String> {
        let package_def_id = class_def.def_id?;
        if !visited.insert(package_def_id) {
            return None;
        }
        let package_name = tree.def_map.get(&package_def_id)?;
        let direct = format!("{package_name}.{member_leaf}");
        if class_def.components.contains_key(member_leaf)
            || class_def.classes.contains_key(member_leaf)
            || tree.name_map.contains_key(&direct)
        {
            return Some(format!("{exposed_package_name}.{member_leaf}"));
        }

        for ext in &class_def.extends {
            let Some(base_def_id) = ext.base_def_id.or(ext.base_name.def_id).or_else(|| {
                let base_name = ext.base_name.to_string();
                resolve_class_in_scope_indexed(class_index, &base_name, package_name)
                    .0
                    .and_then(|class_def| class_def.def_id)
            }) else {
                continue;
            };
            if let Some(base_class) = class_index.get(base_def_id)
                && resolve_inner(
                    tree,
                    class_index,
                    base_class,
                    exposed_package_name,
                    member_leaf,
                    visited,
                )
                .is_some()
            {
                return Some(format!("{exposed_package_name}.{member_leaf}"));
            }
        }

        None
    }

    let mut visited = FxHashSet::default();
    let class_def = class_index.get(package.def_id)?;
    resolve_inner(
        tree,
        class_index,
        class_def,
        &package.name,
        member_leaf,
        &mut visited,
    )
}
