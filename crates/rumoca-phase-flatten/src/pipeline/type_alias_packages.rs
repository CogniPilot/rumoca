//! The package a component's type is spelled through, when Resolve resolved
//! the type outright (MLS §5.3, §7.3).
//!
//! `package Medium = Modelica.Media.Air.MoistAir` is not replaceable, so
//! `Medium.BaseProperties medium` resolves straight to the type class and
//! Instantiate records no type root: nothing says which package the
//! component's class reads its package constants from. That package is the
//! one the first segment of the type name denotes, through any short class
//! definitions (`package Medium = MoistAir` extends `MoistAir` and adds
//! nothing). It is accepted only when the component's type class is declared
//! in it or in a package it extends; otherwise there is no answer, and the
//! constant lookup falls back to what the declaration alone determines.

use rumoca_core::{DefId, InstanceId};
use rumoca_ir_ast as ast;
use rustc_hash::{FxHashMap, FxHashSet};

/// Short class definitions followed before giving up (a cycle or a chain this
/// long is not an alias the lookup relies on).
const MAX_ALIAS_DEPTH: usize = 16;

/// For each component whose qualified type has no recorded root, the package
/// its type name is spelled through.
pub(crate) fn component_type_alias_packages(
    overlay: &ast::InstanceOverlay,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    package_def_ids: &FxHashSet<DefId>,
) -> FxHashMap<InstanceId, DefId> {
    let class_scopes: FxHashMap<DefId, rumoca_core::ScopeId> = tree
        .scope_to_class
        .iter()
        .map(|(scope, class)| (*class, *scope))
        .collect();
    overlay
        .components
        .values()
        .filter(|component| component.type_reference_root_def_id.is_none())
        .filter_map(|component| {
            let type_class = component.type_def_id?;
            let (first, rest) = component.type_name.split_once('.')?;
            if rest.is_empty() {
                return None;
            }
            let scope = component
                .declaration_source_scope
                .as_ref()?
                .to_flat_string();
            let spelled = resolved_class(tree, &class_scopes, class_index, &scope, first)?;
            let package = alias_target(class_index, spelled)?;
            (package_def_ids.contains(&package)
                && declares_or_inherits(class_index, package, type_class))
            .then_some((component.instance_id, package))
        })
        .collect()
}

/// The class `name` denotes where the component was declared, by Resolve's own
/// lookup from that class's scope (MLS 3.7 section 5.3: inherited elements and
/// imports before enclosing scopes). A second, separate lookup could disagree
/// with what Resolve resolved the type through.
fn resolved_class(
    tree: &ast::ClassTree,
    class_scopes: &FxHashMap<DefId, rumoca_core::ScopeId>,
    class_index: &ast::ClassDefIndex<'_>,
    scope: &str,
    name: &str,
) -> Option<DefId> {
    let declaring = class_index.def_id_by_qualified_name(scope)?;
    let class_scope = *class_scopes.get(&declaring)?;
    tree.scope_tree.lookup(
        class_scope,
        &rumoca_core::ComponentPath::from_flat_path(name),
    )
}

/// The class a chain of short class definitions without modifications ends
/// at; any other class is its own target.
fn alias_target(class_index: &ast::ClassDefIndex<'_>, mut class: DefId) -> Option<DefId> {
    for _ in 0..MAX_ALIAS_DEPTH {
        let def = class_index.get(class)?;
        let [extend] = def.extends.as_slice() else {
            return Some(class);
        };
        let pure_alias = extend.modifications.is_empty()
            && def.components.is_empty()
            && def.classes.is_empty()
            && def.equations.is_empty()
            && def.algorithms.is_empty();
        if !pure_alias {
            return Some(class);
        }
        class = extend.base_def_id?;
    }
    None
}

/// Whether `type_class` is declared in `package` or in a package `package`
/// extends, directly or through its bases.
fn declares_or_inherits(
    class_index: &ast::ClassDefIndex<'_>,
    package: DefId,
    type_class: DefId,
) -> bool {
    let Some(declaring) = class_index.parent_def_id(type_class) else {
        return false;
    };
    let mut pending = vec![package];
    let mut seen = FxHashSet::default();
    while let Some(class) = pending.pop() {
        if class == declaring {
            return true;
        }
        if !seen.insert(class) {
            continue;
        }
        if let Some(def) = class_index.get(class) {
            pending.extend(def.extends.iter().filter_map(|extend| extend.base_def_id));
        }
    }
    false
}
