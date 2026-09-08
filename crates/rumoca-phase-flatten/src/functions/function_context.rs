//! Lexical context assembly for a callable class: inherited members, algorithm
//! sections, and the origin scope each of them resolves imports in.
//!
//! A function body is converted from the class tree rather than instantiated,
//! so the names it may see must be rebuilt here: `extends`-inherited components
//! and algorithms (MLS §7.1) keep the scope of the class that textually
//! declares them, import clauses are decided by the one lookup authority for
//! that scope alone (MLS §13.2: imports are never inherited), and the
//! enclosing-scope constants and package parameters a body may reference
//! unqualified (MLS §5.3.2) flow through the declaration-backed alias
//! channels.

use super::*;

/// The scope a collected member or algorithm section resolves imports in:
/// the scope of the class that textually declares it.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub(super) enum ImportOrigin {
    /// The declaring class's resolved scope.
    Scope(rumoca_core::ScopeId),
    /// The declaring class was synthesized after resolve and declares no
    /// import clauses, so there is no scope to seed imports from.
    Unscoped,
}

/// A component together with the scope of the class that declares it.
pub(super) struct OriginComponent {
    pub(super) origin: ImportOrigin,
    pub(super) component: ast::Component,
}

/// An algorithm section together with the scope of the class that declares it.
pub(super) struct OriginAlgorithmSection {
    pub(super) origin: ImportOrigin,
    pub(super) statements: Vec<ast::Statement>,
}

#[derive(Default)]
pub(super) struct FunctionClassContext {
    pub(super) components: IndexMap<String, OriginComponent>,
    pub(super) algorithms: Vec<OriginAlgorithmSection>,
    /// Declaration-backed alias channels (lexical package and class aliases,
    /// enclosing constants) of every class contributing members, merged.
    /// Import clauses never flow through here: the lookup authority decides
    /// them per origin scope.
    pub(super) aliases: qualify::ImportMap,
}

pub(super) fn collect_function_context<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    class_def: &'tree ast::ClassDef,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
) -> Result<FunctionClassContext, FlattenError> {
    let mut visited = HashSet::new();
    let mut context = FunctionClassContext::default();
    collect_function_context_recursive(
        tree,
        class_index,
        class_def,
        &mut visited,
        &mut context,
        member_cache,
    )?;
    Ok(context)
}

fn collect_function_context_recursive<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    class_def: &'tree ast::ClassDef,
    visited: &mut HashSet<usize>,
    context: &mut FunctionClassContext,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
) -> Result<(), FlattenError> {
    let class_key = class_def as *const ast::ClassDef as usize;
    if !visited.insert(class_key) {
        return Ok(());
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
            )?;
        }
    }

    if let Some(class_def_id) = class_def.def_id {
        qualify::collect_lexical_package_aliases_for_def_id_with_member_cache(
            tree,
            class_index,
            class_def_id,
            &mut context.aliases,
            Some(member_cache),
        );
        qualify::collect_lexical_class_aliases_for_def_id_with_member_cache(
            tree,
            class_index,
            class_def_id,
            &mut context.aliases,
            Some(member_cache),
        );
        qualify::collect_lexical_constant_aliases_for_def_id_with_packages_and_member_cache(
            tree,
            class_index,
            class_def_id,
            &[],
            &mut context.aliases,
            Some(member_cache),
        );
    }
    let origin = import_origin_for(class_def)?;
    context
        .algorithms
        .extend(
            class_def
                .algorithms
                .iter()
                .map(|statements| OriginAlgorithmSection {
                    origin,
                    statements: statements.clone(),
                }),
        );
    for (name, component) in &class_def.components {
        context.components.insert(
            name.clone(),
            OriginComponent {
                origin,
                component: component.clone(),
            },
        );
    }
    Ok(())
}

/// The import origin of a class definition: its resolved scope, or the
/// explicit absence of one for a synthesized class. A class that declares
/// import clauses without a resolved scope is an invalid state and is
/// refused rather than having its imports silently dropped.
pub(super) fn import_origin_for(class_def: &ast::ClassDef) -> Result<ImportOrigin, FlattenError> {
    match class_def.scope_id {
        Some(scope_id) => Ok(ImportOrigin::Scope(scope_id)),
        None if class_def.imports.is_empty() => Ok(ImportOrigin::Unscoped),
        None => Err(FlattenError::internal(format!(
            "class {} declares imports but carries no resolved scope id",
            class_def.name.text
        ))),
    }
}

/// Declaration-backed alias channels of the function's own lexical position:
/// enclosing package and class aliases plus enclosing-scope constants.
/// Import clauses are not collected here; the lookup authority decides them.
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
    aliases: &qualify::ImportMap,
) {
    for (name, target) in aliases {
        imports
            .entry(name.clone())
            .or_insert_with(|| target.clone());
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
