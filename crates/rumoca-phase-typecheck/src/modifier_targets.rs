use rumoca_core::{DefId, TypeId};
use rumoca_ir_ast::{ClassTree, Component, TypeTable};
use std::collections::{HashMap, HashSet};

pub(crate) fn build_component_modifier_targets(
    tree: &ClassTree,
) -> HashMap<DefId, HashSet<String>> {
    let mut cache = HashMap::new();
    let mut visiting = HashSet::new();
    for def_id in tree.def_map.keys().copied() {
        let _ = collect_component_modifier_targets(tree, def_id, &mut cache, &mut visiting);
    }
    cache
}

pub(crate) fn build_component_modifier_member_types(
    tree: &ClassTree,
    type_table: &TypeTable,
    type_ids_by_def_id: &HashMap<DefId, TypeId>,
    type_suffix_index: &HashMap<String, Option<TypeId>>,
) -> HashMap<DefId, HashMap<String, TypeId>> {
    let mut cache = HashMap::new();
    let mut visiting = HashSet::new();
    for def_id in tree.def_map.keys().copied() {
        let _ = collect_component_modifier_member_types(
            tree,
            type_table,
            type_ids_by_def_id,
            type_suffix_index,
            def_id,
            &mut cache,
            &mut visiting,
        );
    }
    cache
}

fn collect_component_modifier_targets(
    tree: &ClassTree,
    def_id: DefId,
    cache: &mut HashMap<DefId, HashSet<String>>,
    visiting: &mut HashSet<DefId>,
) -> Option<HashSet<String>> {
    if let Some(existing) = cache.get(&def_id) {
        return Some(existing.clone());
    }
    if !visiting.insert(def_id) {
        return Some(HashSet::new());
    }

    let class = tree.get_class_by_def_id(def_id)?;
    let mut names: HashSet<String> = class
        .components
        .keys()
        .cloned()
        .chain(class.classes.keys().cloned())
        .collect();

    for ext in &class.extends {
        let Some(base_def_id) = ext.base_def_id else {
            continue;
        };
        if let Some(base_names) =
            collect_component_modifier_targets(tree, base_def_id, cache, visiting)
        {
            names.extend(base_names);
        }
        for break_name in &ext.break_names {
            names.remove(break_name);
        }
    }

    visiting.remove(&def_id);
    cache.insert(def_id, names.clone());
    Some(names)
}

fn collect_component_modifier_member_types(
    tree: &ClassTree,
    type_table: &TypeTable,
    type_ids_by_def_id: &HashMap<DefId, TypeId>,
    type_suffix_index: &HashMap<String, Option<TypeId>>,
    def_id: DefId,
    cache: &mut HashMap<DefId, HashMap<String, TypeId>>,
    visiting: &mut HashSet<DefId>,
) -> Option<HashMap<String, TypeId>> {
    if let Some(existing) = cache.get(&def_id) {
        return Some(existing.clone());
    }
    if !visiting.insert(def_id) {
        return Some(HashMap::new());
    }

    let class = tree.get_class_by_def_id(def_id)?;
    let mut member_types = HashMap::<String, TypeId>::new();

    for ext in &class.extends {
        let Some(base_def_id) = ext.base_def_id else {
            continue;
        };
        if let Some(base_member_types) = collect_component_modifier_member_types(
            tree,
            type_table,
            type_ids_by_def_id,
            type_suffix_index,
            base_def_id,
            cache,
            visiting,
        ) {
            member_types.extend(base_member_types);
        }
        for break_name in &ext.break_names {
            member_types.remove(break_name);
        }
    }

    for (member_name, member_comp) in &class.components {
        let member_type_id = resolve_component_type_for_modifier_members(
            member_comp,
            type_table,
            type_ids_by_def_id,
            type_suffix_index,
        );
        member_types.insert(member_name.clone(), member_type_id);
    }

    visiting.remove(&def_id);
    cache.insert(def_id, member_types.clone());
    Some(member_types)
}

fn resolve_component_type_for_modifier_members(
    component: &Component,
    type_table: &TypeTable,
    type_ids_by_def_id: &HashMap<DefId, TypeId>,
    type_suffix_index: &HashMap<String, Option<TypeId>>,
) -> TypeId {
    if let Some(type_def_id) = component.type_def_id
        && let Some(type_id) = type_ids_by_def_id.get(&type_def_id)
    {
        return *type_id;
    }

    let type_name = component.type_name.to_string();
    type_table
        .lookup(&type_name)
        .or_else(|| type_suffix_index.get(&type_name).copied().flatten())
        .unwrap_or(TypeId::UNKNOWN)
}
