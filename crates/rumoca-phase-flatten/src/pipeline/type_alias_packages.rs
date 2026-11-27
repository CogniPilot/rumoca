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
    // A class instance no component owns (the model being flattened, and any
    // other such instance in the overlay) reads its constants through the
    // package that declares its class (`TwoPhaseWater.ExtendedProperties`
    // simulated on its own reads TwoPhaseWater's `nS`), when that package
    // declares or inherits the class.
    let root = overlay
        .classes
        .values()
        .filter(|class| class.owner_component_id.is_none())
        .filter_map(|class| {
            let model = class.class_def_id?;
            let package = class_index.parent_def_id(model)?;
            (package_def_ids.contains(&package)
                && declares_or_inherits(class_index, package, model))
            .then_some((class.instance_id, package))
        });
    overlay
        .components
        .values()
        .filter(|component| component.type_reference_root_def_id.is_none())
        .filter_map(|component| {
            let type_class = component.type_def_id?;
            let scope = component
                .declaration_source_scope
                .as_ref()?
                .to_flat_string();
            let type_path = rumoca_core::ComponentPath::from_flat_path(&component.type_name);
            let package = match type_path.len() {
                0 => return None,
                1 => member_package(tree, &class_scopes, class_index, &scope, &type_path)?,
                _ => {
                    let head = type_path.prefix(1)?;
                    let spelled = resolved_class(tree, &class_scopes, class_index, &scope, &head)?;
                    alias_target(class_index, spelled)?
                }
            };
            (package_def_ids.contains(&package)
                && declares_or_inherits(class_index, package, type_class))
            .then_some((component.instance_id, package))
        })
        .chain(root)
        .collect()
}

/// For a type named by one identifier (`ExtendedProperties medium` written
/// inside the package that declares it), the class whose member the name is:
/// the first scope, walking outward from where the component was declared,
/// that declares or inherits it (MLS 3.7 section 5.3, in Resolve's order). A
/// name an import supplies, or one found past an encapsulated boundary, has
/// no enclosing package to read constants through, so there is no answer.
fn member_package(
    tree: &ast::ClassTree,
    class_scopes: &FxHashMap<DefId, rumoca_core::ScopeId>,
    class_index: &ast::ClassDefIndex<'_>,
    scope: &str,
    name: &rumoca_core::ComponentPath,
) -> Option<DefId> {
    let declaring = class_index.def_id_by_qualified_name(scope)?;
    let mut current = Some(*class_scopes.get(&declaring)?);
    while let Some(scope_id) = current {
        let scope = tree.scope_tree.get(scope_id)?;
        if tree.scope_tree.lookup_member(scope_id, name).is_some() {
            return tree.scope_to_class.get(&scope_id).copied();
        }
        if scope
            .imports
            .iter()
            .any(|import| import.resolves(name).is_some())
            || scope.is_encapsulated()
        {
            return None;
        }
        current = scope.parent;
    }
    None
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
    name: &rumoca_core::ComponentPath,
) -> Option<DefId> {
    let declaring = class_index.def_id_by_qualified_name(scope)?;
    let class_scope = *class_scopes.get(&declaring)?;
    tree.scope_tree.lookup(class_scope, name)
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
