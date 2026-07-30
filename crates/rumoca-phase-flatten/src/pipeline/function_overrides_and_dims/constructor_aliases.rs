//! Constructor-alias collection: the receiver aliases a class or
//! component instance contributes (components, nested packages,
//! inherited declarations).

use super::*;

fn resolve_component_type_ref<'a>(
    component: &rumoca_ir_ast::Component,
    tree: &'a ClassTree,
    class_index: &'a rumoca_ir_ast::ClassDefIndex<'a>,
    class_scope: &str,
) -> Option<ResolvedClassRef<'a>> {
    if let Some(target_def_id) = component.type_def_id.or(component.type_name.def_id) {
        return Some(ResolvedClassRef {
            name: tree.def_map.get(&target_def_id)?.clone(),
            def_id: target_def_id,
            class_def: class_index.get(target_def_id)?,
        });
    }

    let raw_type_name = component.type_name.to_string();
    if raw_type_name.is_empty() {
        return None;
    }

    if let Some(class_def) = class_index.get_by_qualified_name(&raw_type_name) {
        return Some(ResolvedClassRef {
            name: raw_type_name,
            def_id: class_def.def_id?,
            class_def,
        });
    }

    let (class_def, resolved_name) =
        resolve_class_in_scope_indexed(class_index, &raw_type_name, class_scope);
    let class_def = class_def?;
    Some(ResolvedClassRef {
        name: resolved_name?,
        def_id: class_def.def_id?,
        class_def,
    })
}

pub(crate) fn collect_component_constructor_aliases_for_class(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    class_def: &rumoca_ir_ast::ClassDef,
    class_scope: &str,
    active_aliases: bool,
    visited_classes: &mut FxHashSet<usize>,
    overrides: &mut rustc_hash::FxHashMap<String, OverrideTarget>,
) {
    let class_ptr = class_def as *const rumoca_ir_ast::ClassDef as usize;
    if !visited_classes.insert(class_ptr) {
        return;
    }

    for ext in &class_def.extends {
        let base_name = ext.base_name.to_string();
        let (base_class, resolved_base_name) = if let Some(base_def_id) = ext.base_def_id {
            (
                class_index.get(base_def_id),
                tree.def_map.get(&base_def_id).cloned(),
            )
        } else {
            resolve_class_in_scope_indexed(class_index, &base_name, class_scope)
        };

        let Some(base_class) = base_class else {
            continue;
        };
        let base_scope = resolved_base_name.unwrap_or(base_name);
        collect_component_constructor_aliases_for_class(
            tree,
            class_index,
            base_class,
            &base_scope,
            false,
            visited_classes,
            overrides,
        );
    }

    collect_nested_package_aliases_for_class(
        tree,
        class_index,
        class_def,
        class_scope,
        active_aliases,
        overrides,
    );
    collect_extends_redeclare_aliases_for_class(
        tree,
        class_index,
        class_def,
        class_scope,
        overrides,
    );

    for (component_name, component) in &class_def.components {
        let Some(target_ref) =
            resolve_component_type_ref(component, tree, class_index, class_scope)
        else {
            continue;
        };
        if !is_receiver_alias_type(&target_ref.class_def.class_type) {
            continue;
        }
        // Derived classes should override inherited aliases with the same name.
        overrides.insert(
            component_name.clone(),
            OverrideTarget::from_resolved(component_name.clone(), target_ref, active_aliases),
        );
    }
}

fn collect_nested_package_aliases_for_class(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    class_def: &rumoca_ir_ast::ClassDef,
    class_scope: &str,
    active_aliases: bool,
    overrides: &mut rustc_hash::FxHashMap<String, OverrideTarget>,
) {
    for (alias, nested) in &class_def.classes {
        if nested.class_type != rumoca_core::ClassType::Package {
            continue;
        }
        let Some(target_ref) =
            nested_package_alias_target_ref(tree, class_index, nested, class_scope)
        else {
            continue;
        };
        if target_ref.class_def.class_type == rumoca_core::ClassType::Package {
            let active_alias = active_aliases && leaf_segment(&target_ref.name) != alias;
            overrides.insert(
                alias.clone(),
                OverrideTarget::from_resolved(alias.clone(), target_ref, active_alias),
            );
        }
    }
}

fn nested_package_alias_target_ref<'a>(
    tree: &'a ClassTree,
    class_index: &'a rumoca_ir_ast::ClassDefIndex<'a>,
    class_def: &rumoca_ir_ast::ClassDef,
    class_scope: &str,
) -> Option<ResolvedClassRef<'a>> {
    if !is_package_alias_definition(class_def) {
        return None;
    }
    let ext = class_def.extends.first()?;
    if let Some(def_id) = ext.base_def_id {
        return Some(ResolvedClassRef {
            name: tree.def_map.get(&def_id)?.clone(),
            def_id,
            class_def: class_index.get(def_id)?,
        });
    }
    let (class_def, name) =
        resolve_class_in_scope_indexed(class_index, &ext.base_name.to_string(), class_scope);
    let class_def = class_def?;
    Some(ResolvedClassRef {
        name: name?,
        def_id: class_def.def_id?,
        class_def,
    })
}

fn is_package_alias_definition(class_def: &rumoca_ir_ast::ClassDef) -> bool {
    class_def.extends.len() == 1
        && class_def.imports.is_empty()
        && class_def.classes.is_empty()
        && class_def.components.is_empty()
        && class_def.equations.is_empty()
        && class_def.initial_equations.is_empty()
        && class_def.algorithms.is_empty()
        && class_def.initial_algorithms.is_empty()
        && class_def.enum_literals.is_empty()
        && class_def.external.is_none()
}

pub(super) fn resolve_package_alias_chain<'a>(
    tree: &'a ClassTree,
    class_index: &'a rumoca_ir_ast::ClassDefIndex<'a>,
    def_id: rumoca_core::DefId,
) -> Option<ResolvedClassRef<'a>> {
    let mut current_def_id = def_id;
    let mut visited = FxHashSet::default();

    loop {
        if !visited.insert(current_def_id) {
            return None;
        }
        let current = resolved_class_ref_for_def_id(tree, class_index, current_def_id)?;
        if current.class_def.class_type != rumoca_core::ClassType::Package
            || !is_package_alias_definition(current.class_def)
        {
            return Some(current);
        }
        let ext = current.class_def.extends.first()?;
        let next_def_id = ext.base_def_id.or(ext.base_name.def_id)?;
        current_def_id = next_def_id;
    }
}

pub(super) fn resolve_class_ref_name(
    tree: &ClassTree,
    cref: &rumoca_ir_ast::ComponentReference,
) -> Option<String> {
    if let Some(name) = cref
        .root_def_id()
        .and_then(|def_id| tree.def_map.get(&def_id))
    {
        return Some(name.clone());
    }

    let first = cref.parts.first()?;
    let mut current = tree
        .definitions
        .classes
        .get(first.ident.text.as_ref())
        .or_else(|| {
            cref.root_def_id()
                .and_then(|def_id| tree.get_class_by_def_id(def_id))
                .filter(|class_def| class_def.name.text.as_ref() == first.ident.text.as_ref())
        });
    for part in cref.parts.iter().skip(1) {
        current = current.and_then(|class_def| class_def.classes.get(part.ident.text.as_ref()));
    }
    current
        .and_then(|class_def| class_def.def_id)
        .and_then(|def_id| tree.def_map.get(&def_id).cloned())
}

pub(crate) fn collect_component_constructor_aliases(
    instance: &rumoca_ir_ast::InstanceData,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    overrides: &mut rustc_hash::FxHashMap<String, OverrideTarget>,
) {
    let Some(type_def_id) = instance.type_def_id else {
        return;
    };
    let Some(class_def) = class_index.get(type_def_id) else {
        return;
    };
    let class_scope = tree
        .def_map
        .get(&type_def_id)
        .map(String::as_str)
        .unwrap_or(class_def.name.text.as_ref());
    let mut visited_classes = FxHashSet::default();
    collect_component_constructor_aliases_for_class(
        tree,
        class_index,
        class_def,
        class_scope,
        false,
        &mut visited_classes,
        overrides,
    );
}
