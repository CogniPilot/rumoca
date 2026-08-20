//! Lexical context assembly for a callable class: inherited members, algorithm
//! sections, and the import/alias map its body is lowered against.
//!
//! A function body is converted from the class tree rather than instantiated,
//! so the names it may see must be rebuilt here: `extends`-inherited components
//! and algorithms (MLS §7.1), import clauses in the class and every lexical
//! ancestor (MLS §13.2.1), and the enclosing-scope constants and package
//! parameters a body may reference unqualified (MLS §5.3.2).

use super::*;

#[derive(Default)]
pub(super) struct FunctionClassContext {
    pub(super) components: IndexMap<String, ast::Component>,
    pub(super) algorithms: Vec<Vec<ast::Statement>>,
    pub(super) imports: qualify::ImportMap,
}

pub(super) fn collect_function_context<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    class_def: &'tree ast::ClassDef,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
) -> FunctionClassContext {
    let mut visited = HashSet::new();
    let mut context = FunctionClassContext::default();
    collect_function_context_recursive(
        tree,
        class_index,
        class_def,
        &mut visited,
        &mut context,
        member_cache,
    );
    context
}

fn collect_function_context_recursive<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    class_def: &'tree ast::ClassDef,
    visited: &mut HashSet<usize>,
    context: &mut FunctionClassContext,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
) {
    let class_key = class_def as *const ast::ClassDef as usize;
    if !visited.insert(class_key) {
        return;
    }

    for extend in &class_def.extends {
        let base_class = extend
            .base_def_id
            .and_then(|def_id| class_index.get(def_id))
            .or_else(|| {
                let qualified = extend.base_name.to_string();
                class_index.get_by_qualified_name(&qualified)
            });
        if let Some(base_class) = base_class {
            collect_function_context_recursive(
                tree,
                class_index,
                base_class,
                visited,
                context,
                member_cache,
            );
        }
    }

    if let Some(class_def_id) = class_def.def_id {
        collect_lexical_ancestor_imports(class_index, class_def_id, &mut context.imports);
        qualify::collect_lexical_package_aliases_for_def_id_with_member_cache(
            tree,
            class_index,
            class_def_id,
            &mut context.imports,
            Some(member_cache),
        );
        qualify::collect_lexical_class_aliases_for_def_id_with_member_cache(
            tree,
            class_index,
            class_def_id,
            &mut context.imports,
            Some(member_cache),
        );
        qualify::collect_lexical_constant_aliases_for_def_id_with_packages_and_member_cache(
            tree,
            class_index,
            class_def_id,
            &[],
            &mut context.imports,
            Some(member_cache),
        );
    }
    resolve_import_pairs(&class_def.imports, class_index, &mut context.imports);
    context.algorithms.extend(class_def.algorithms.clone());
    context.components.extend(class_def.components.clone());
}

fn collect_lexical_ancestor_imports(
    class_index: &ast::ClassDefIndex<'_>,
    class_def_id: rumoca_core::DefId,
    map: &mut qualify::ImportMap,
) {
    let mut ancestor_def_ids = Vec::new();
    let mut current = class_index.parent_def_id(class_def_id);
    while let Some(def_id) = current {
        ancestor_def_ids.push(def_id);
        current = class_index.parent_def_id(def_id);
    }
    for ancestor_def_id in ancestor_def_ids.into_iter().rev() {
        let Some(ancestor_class) = class_index.get(ancestor_def_id) else {
            continue;
        };
        resolve_import_pairs(&ancestor_class.imports, class_index, map);
    }
}

pub(super) fn resolve_import_pairs(
    imports: &[ast::Import],
    class_index: &ast::ClassDefIndex<'_>,
    map: &mut qualify::ImportMap,
) {
    for import in imports {
        match import {
            ast::Import::Qualified { path, .. } => {
                let fqn = path.to_string();
                map.insert(path_utils::leaf_segment(&fqn).to_string(), fqn);
            }
            ast::Import::Renamed { alias, path, .. } => {
                map.insert(alias.text.to_string(), path.to_string());
            }
            ast::Import::Unqualified { path, .. } => {
                let pkg_name = path.to_string();
                let Some(class_def) = class_index.get_by_qualified_name(&pkg_name) else {
                    continue;
                };
                for name in class_def.components.keys() {
                    map.insert(name.clone(), format!("{pkg_name}.{name}"));
                }
                for name in class_def.classes.keys() {
                    map.insert(name.clone(), format!("{pkg_name}.{name}"));
                }
            }
            ast::Import::Selective { path, names, .. } => {
                let pkg_name = path.to_string();
                for name_tok in names {
                    let name = name_tok.text.to_string();
                    map.insert(name.clone(), format!("{pkg_name}.{name}"));
                }
            }
        }
    }
}

pub(super) fn function_initial_import_map<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    class_def: &ast::ClassDef,
    qualified_name: &str,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
) -> qualify::ImportMap {
    let mut import_map = qualify::ImportMap::default();
    if let Some(class_def_id) = class_def.def_id {
        qualify::collect_lexical_package_aliases_for_def_id_with_member_cache(
            tree,
            class_index,
            class_def_id,
            &mut import_map,
            Some(member_cache),
        );
        qualify::collect_lexical_class_aliases_for_def_id_with_member_cache(
            tree,
            class_index,
            class_def_id,
            &mut import_map,
            Some(member_cache),
        );
        collect_lexical_constant_aliases(tree, class_index, class_def_id, &mut import_map, false);
        collect_lexical_ancestor_imports(class_index, class_def_id, &mut import_map);
    } else {
        qualify::collect_lexical_package_aliases(
            tree,
            class_index,
            qualified_name,
            &mut import_map,
        );
        qualify::collect_lexical_class_aliases(tree, class_index, qualified_name, &mut import_map);
    }
    import_map
}

pub(super) fn extend_imports_if_absent(
    imports: &mut qualify::ImportMap,
    aliases: qualify::ImportMap,
) {
    for (name, target) in aliases {
        imports.entry(name).or_insert(target);
    }
}

pub(super) fn collect_lexical_constant_aliases<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    class_def_id: rumoca_core::DefId,
    imports: &mut qualify::ImportMap,
    overwrite: bool,
) {
    let mut ancestor_def_ids = Vec::new();
    let mut current = class_index.parent_def_id(class_def_id);
    while let Some(def_id) = current {
        ancestor_def_ids.push(def_id);
        current = class_index.parent_def_id(def_id);
    }
    for ancestor_def_id in ancestor_def_ids {
        let Some(scope) = class_index.qualified_name(ancestor_def_id) else {
            continue;
        };
        collect_effective_package_constant_aliases(tree, class_index, scope, imports, overwrite);
    }
}

fn collect_effective_package_constant_aliases(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    active_scope: &str,
    imports: &mut qualify::ImportMap,
    overwrite: bool,
) {
    let mut chain = Vec::new();
    let mut visited = FxHashSet::default();
    collect_package_chain(tree, class_index, active_scope, &mut chain, &mut visited);
    if chain.is_empty()
        && let Some(active_def_id) = class_index
            .get_by_qualified_name(active_scope)
            .and_then(|class_def| class_def.def_id)
    {
        chain.push(active_def_id);
    }
    for source_def_id in chain {
        let Some(class_def) = class_index.get(source_def_id) else {
            continue;
        };
        if !tree.def_map.contains_key(&source_def_id) {
            continue;
        }
        // MLS §5.3.2: enclosing-scope lookup reaches class constants (and,
        // for package enclosers, package members). A non-package class's
        // parameters are instance members and must never become
        // class-qualified alias targets.
        let is_package = matches!(class_def.class_type, rumoca_core::ClassType::Package);
        for (name, component) in &class_def.components {
            let alias_visible = match component.variability {
                rumoca_core::Variability::Constant(_) => true,
                rumoca_core::Variability::Parameter(_) => is_package,
                _ => false,
            };
            if !alias_visible {
                continue;
            }
            let target = format!("{active_scope}.{name}");
            if overwrite {
                imports.insert(name.clone(), target);
            } else {
                imports.entry(name.clone()).or_insert(target);
            }
        }
    }
}
