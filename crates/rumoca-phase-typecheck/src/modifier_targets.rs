use crate::{TypeCheckError, TypeCheckResult};
use rumoca_core::{ComponentPath, DefId, SourceMap, TypeId};
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
    source_map: &SourceMap,
) -> TypeCheckResult<HashMap<DefId, HashMap<String, TypeId>>> {
    let mut cache = HashMap::new();
    let mut visiting = HashSet::new();
    let ctx = ComponentModifierMemberTypeContext {
        tree,
        type_table,
        type_ids_by_def_id,
        source_map,
    };
    for def_id in tree.def_map.keys().copied() {
        let _ = collect_component_modifier_member_types(&ctx, def_id, &mut cache, &mut visiting)?;
    }
    Ok(cache)
}

pub(crate) fn build_component_modifier_member_types_for_def_ids<I>(
    tree: &ClassTree,
    type_table: &TypeTable,
    type_ids_by_def_id: &HashMap<DefId, TypeId>,
    source_map: &SourceMap,
    root_def_ids: I,
) -> TypeCheckResult<HashMap<DefId, HashMap<String, TypeId>>>
where
    I: IntoIterator<Item = DefId>,
{
    let mut cache = HashMap::new();
    let mut visiting = HashSet::new();
    let ctx = ComponentModifierMemberTypeContext {
        tree,
        type_table,
        type_ids_by_def_id,
        source_map,
    };
    for def_id in root_def_ids {
        let _ = collect_component_modifier_member_types(&ctx, def_id, &mut cache, &mut visiting)?;
    }
    Ok(cache)
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

struct ComponentModifierMemberTypeContext<'a> {
    tree: &'a ClassTree,
    type_table: &'a TypeTable,
    type_ids_by_def_id: &'a HashMap<DefId, TypeId>,
    source_map: &'a SourceMap,
}

fn collect_component_modifier_member_types(
    ctx: &ComponentModifierMemberTypeContext<'_>,
    def_id: DefId,
    cache: &mut HashMap<DefId, HashMap<String, TypeId>>,
    visiting: &mut HashSet<DefId>,
) -> TypeCheckResult<Option<HashMap<String, TypeId>>> {
    if let Some(existing) = cache.get(&def_id) {
        return Ok(Some(existing.clone()));
    }
    if !visiting.insert(def_id) {
        return Ok(Some(HashMap::new()));
    }

    let Some(class) = ctx.tree.get_class_by_def_id(def_id) else {
        return Ok(None);
    };
    let mut member_types = HashMap::<String, TypeId>::new();

    for ext in &class.extends {
        let Some(base_def_id) = ext.base_def_id else {
            continue;
        };
        if let Some(base_member_types) =
            collect_component_modifier_member_types(ctx, base_def_id, cache, visiting)?
        {
            member_types.extend(base_member_types);
        }
        for break_name in &ext.break_names {
            member_types.remove(break_name);
        }
    }

    for (member_name, member_comp) in &class.components {
        let member_type_id = resolve_component_type_for_modifier_members(member_comp, ctx)?;
        if let Some(member_type_id) = member_type_id {
            member_types.insert(member_name.clone(), member_type_id);
        }
    }

    visiting.remove(&def_id);
    cache.insert(def_id, member_types.clone());
    Ok(Some(member_types))
}

fn resolve_component_type_for_modifier_members(
    component: &Component,
    ctx: &ComponentModifierMemberTypeContext<'_>,
) -> TypeCheckResult<Option<TypeId>> {
    if let Some(type_def_id) = component.type_def_id
        && let Some(type_id) = ctx.type_ids_by_def_id.get(&type_def_id)
    {
        return Ok(Some(*type_id));
    }
    if component.type_def_id.is_none() && component.type_name.name.len() > 1 {
        if let Some(member_def_id) =
            resolve_dotted_member_def_id(ctx.tree, component.type_name.def_id, &component.type_name)
        {
            return Ok(ctx.type_ids_by_def_id.get(&member_def_id).copied());
        }
        return Ok(None);
    }

    let type_name = component.type_name.to_string();
    let span = name_span(ctx.source_map, &component.type_name)?;
    ctx.type_table
        .lookup(&type_name)
        .map(Some)
        .ok_or_else(|| Box::new(TypeCheckError::undefined_type(type_name, span)))
}

/// Resolve a dotted member identity through class scopes and inherited members.
/// A replaceable package such as `FluidPort.Medium` exposes types inherited from
/// its constraining package, including types inherited by that package through
/// `Types`. Resolve records the first segment's `DefId`, while later segments
/// must be resolved from each class scope. Scope lookup returns no identity for
/// an ambiguous inherited member, so this cannot select the first base visited.
pub(crate) fn resolve_dotted_member_def_id(
    tree: &ClassTree,
    anchor_def_id: Option<DefId>,
    name: &rumoca_ir_ast::Name,
) -> Option<DefId> {
    let mut current_def_id = anchor_def_id?;
    for part in name.name.iter().skip(1) {
        let class = tree.get_class_by_def_id(current_def_id)?;
        let scope_id = class.scope_id?;
        current_def_id = tree
            .scope_tree
            .lookup_member(scope_id, &ComponentPath::from_parts([part.text.as_ref()]))?;
    }
    (name.name.len() > 1).then_some(current_def_id)
}

fn name_span(
    source_map: &SourceMap,
    name: &rumoca_ir_ast::Name,
) -> TypeCheckResult<rumoca_core::Span> {
    let Some(first) = name.name.first() else {
        return Err(Box::new(TypeCheckError::missing_source_context(
            "component modifier member type name has no source path segments",
        )));
    };
    let last = name.name.last().unwrap_or(first);
    let source = if first.location.source != rumoca_core::SourceId::DUMMY {
        first.location.source
    } else {
        last.location.source
    };
    source_map
        .try_span(
            source,
            first.location.start as usize,
            last.location.end as usize,
        )
        .ok_or_else(|| {
            let file_name = source_map
                .name(source)
                .unwrap_or(crate::UNKNOWN_SOURCE_DISPLAY_NAME);
            Box::new(TypeCheckError::missing_source_context(format!(
                "source file `{file_name}` for component modifier member type name was not found"
            )))
        })
}
