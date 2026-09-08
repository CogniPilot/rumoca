use rumoca_core::{ComponentPath, DefId, TypeId};
use rumoca_ir_ast::{ClassDef, ClassTree, Component, WildcardMember};
use std::collections::HashMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum ModifierMember {
    Typed(TypeId),
    Ambiguous,
    UnresolvedType,
}

pub(crate) type ModifierMemberCatalog = HashMap<DefId, HashMap<ComponentPath, ModifierMember>>;

/// Project Resolve's exact direct-and-inherited member view into the type
/// identities needed for modifier-path traversal.
///
/// The ScopeTree remains the sole inheritance/name authority: this projection
/// preserves its ambiguity verdicts and never walks `extends` or rebuilds
/// declaration identity from rendered names.
pub(crate) fn build_modifier_member_catalog(
    tree: &ClassTree,
    type_ids_by_def_id: &HashMap<DefId, TypeId>,
) -> ModifierMemberCatalog {
    let components = component_declarations_by_def_id(tree);
    tree.def_map
        .keys()
        .filter_map(|&class_def_id| {
            let class = tree.get_class_by_def_id(class_def_id)?;
            let scope = class.scope_id?;
            let members = tree
                .scope_tree
                .importable_members(scope)
                .into_iter()
                .map(|(name, member)| {
                    let member = match member {
                        WildcardMember::Unique(member_def_id) => {
                            modifier_member_type(member_def_id, &components, type_ids_by_def_id)
                        }
                        WildcardMember::AmbiguousInherited => ModifierMember::Ambiguous,
                    };
                    (name, member)
                })
                .collect();
            Some((class_def_id, members))
        })
        .collect()
}

fn modifier_member_type(
    member_def_id: DefId,
    components: &HashMap<DefId, &Component>,
    type_ids_by_def_id: &HashMap<DefId, TypeId>,
) -> ModifierMember {
    if let Some(type_id) = type_ids_by_def_id.get(&member_def_id) {
        return ModifierMember::Typed(*type_id);
    }
    let Some(component) = components.get(&member_def_id) else {
        return ModifierMember::UnresolvedType;
    };
    component
        .type_def_id
        .and_then(|type_def_id| type_ids_by_def_id.get(&type_def_id))
        .copied()
        .map_or(ModifierMember::UnresolvedType, ModifierMember::Typed)
}

fn component_declarations_by_def_id(tree: &ClassTree) -> HashMap<DefId, &Component> {
    let mut components = HashMap::new();
    for class in tree.definitions.classes.values() {
        collect_component_declarations(class, &mut components);
    }
    components
}

fn collect_component_declarations<'a>(
    class: &'a ClassDef,
    components: &mut HashMap<DefId, &'a Component>,
) {
    components.extend(
        class
            .components
            .values()
            .filter_map(|component| component.def_id.map(|def_id| (def_id, component))),
    );
    for nested in class.classes.values() {
        collect_component_declarations(nested, components);
    }
}
