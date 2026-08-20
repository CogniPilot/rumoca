//! Component override map construction: one alias table per component
//! path for the whole instance overlay.

use super::*;

type ConstructorOverrideCache =
    rustc_hash::FxHashMap<rumoca_core::DefId, rustc_hash::FxHashMap<String, OverrideTarget>>;

#[cfg(test)]
pub(crate) fn component_overrides(
    instance: &rumoca_ir_ast::InstanceData,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> rustc_hash::FxHashMap<String, OverrideTarget> {
    let mut cache = ConstructorOverrideCache::default();
    component_overrides_with_cache(instance, tree, class_index, &mut cache)
}

fn component_overrides_with_cache(
    instance: &rumoca_ir_ast::InstanceData,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    constructor_cache: &mut ConstructorOverrideCache,
) -> rustc_hash::FxHashMap<String, OverrideTarget> {
    let mut overrides =
        cached_component_constructor_aliases(instance, tree, class_index, constructor_cache);
    for class_override in instance.class_overrides.values() {
        if let Some(target_ref) =
            resolve_package_alias_chain(tree, class_index, class_override.target_def_id)
        {
            let active = component_class_override_is_active(
                class_override,
                overrides.get(&class_override.alias),
                &target_ref,
            );
            overrides.insert(
                class_override.alias.clone(),
                OverrideTarget::from_resolved_with_modifier_args(
                    class_override.alias.clone(),
                    target_ref,
                    active,
                    class_override_modifier_args(&class_override.modifier_args),
                ),
            );
        }
    }
    overrides
}

fn class_override_modifier_args(args: &[rumoca_ir_ast::Expression]) -> Vec<FunctionModifierArg> {
    args.iter()
        .filter_map(function_modifier_arg_from_ast)
        .collect()
}

fn cached_component_constructor_aliases(
    instance: &rumoca_ir_ast::InstanceData,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    constructor_cache: &mut ConstructorOverrideCache,
) -> rustc_hash::FxHashMap<String, OverrideTarget> {
    let Some(type_def_id) = instance.type_def_id else {
        return rustc_hash::FxHashMap::default();
    };
    if let Some(cached) = constructor_cache.get(&type_def_id) {
        return cached.clone();
    }
    let mut overrides = rustc_hash::FxHashMap::default();
    collect_component_constructor_aliases(instance, tree, class_index, &mut overrides);
    constructor_cache.insert(type_def_id, overrides.clone());
    overrides
}

pub(super) fn component_class_override_is_active(
    class_override: &rumoca_ir_ast::ClassOverride,
    inherited_default: Option<&OverrideTarget>,
    target_ref: &ResolvedClassRef<'_>,
) -> bool {
    if inherited_default.is_some_and(|default| default.def_id == target_ref.def_id) {
        return false;
    }
    let redeclare_value_leaf = class_override
        .target_ref
        .as_ref()
        .and_then(|target_ref| target_ref.parts.last())
        .map(|part| part.ident.text.as_ref());
    redeclare_value_leaf != Some(class_override.alias.as_str())
        || inherited_default.is_some_and(|default| default.def_id != target_ref.def_id)
        || leaf_segment(&target_ref.name) != class_override.alias.as_str()
}

pub(crate) fn class_instance_component_overrides(
    class_data: &rumoca_ir_ast::ClassInstanceData,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> Result<rustc_hash::FxHashMap<String, OverrideTarget>, FlattenError> {
    let mut overrides = rustc_hash::FxHashMap::default();
    let class_scope = class_data.source_scope.as_ref().ok_or_else(|| {
        missing_class_instance_override_scope_error(class_data, tree, "class function overrides")
    })?;
    let class_scope_id = class_data.source_scope_id.ok_or_else(|| {
        missing_class_instance_override_scope_error(class_data, tree, "class function overrides")
    })?;
    if tree.scope_tree.get(class_scope_id).is_none() {
        return Err(missing_class_instance_override_scope_error(
            class_data,
            tree,
            "class function overrides",
        ));
    }
    let class_scope_name = class_scope.to_flat_string();
    let Some(class_def) = class_index.get_by_qualified_name(&class_scope_name) else {
        return Ok(overrides);
    };
    let mut visited_classes = FxHashSet::default();
    collect_component_constructor_aliases_for_class(
        tree,
        class_index,
        class_def,
        &class_scope_name,
        false,
        &mut visited_classes,
        &mut overrides,
    );
    Ok(overrides)
}

pub(crate) fn build_component_override_map(
    overlay: &InstanceOverlay,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    model_name: &str,
) -> Result<ComponentOverrideMap, FlattenError> {
    let mut map = ComponentOverrideMap::default();
    insert_component_overrides(
        &mut map,
        ComponentPath::root(),
        root_class_component_overrides(tree, class_index, model_name),
    );
    for class_data in overlay.classes.values() {
        insert_component_overrides(
            &mut map,
            class_data.qualified_name.to_component_path(),
            class_instance_component_overrides(class_data, tree, class_index)?,
        );
    }
    let mut constructor_cache = ConstructorOverrideCache::default();
    for instance in overlay.components.values() {
        insert_component_overrides(
            &mut map,
            instance.qualified_name.to_component_path(),
            component_overrides_with_cache(instance, tree, class_index, &mut constructor_cache),
        );
    }
    Ok(map)
}

fn missing_class_instance_override_scope_error(
    class_data: &rumoca_ir_ast::ClassInstanceData,
    tree: &ClassTree,
    context: &str,
) -> FlattenError {
    if let Some(span) = class_data
        .class_def_id
        .and_then(|def_id| class_index_span(tree, def_id))
        .or_else(|| class_data.equations.first().map(|eq| eq.span))
        .or_else(|| class_data.initial_equations.first().map(|eq| eq.span))
        .or_else(|| {
            class_data
                .algorithms
                .first()
                .and_then(|alg| alg.first().map(|stmt| stmt.span))
        })
        .or_else(|| {
            class_data
                .initial_algorithms
                .first()
                .and_then(|alg| alg.first().map(|stmt| stmt.span))
        })
        .filter(|span| !span.is_dummy())
    {
        return FlattenError::missing_source_scope(
            class_data.qualified_name.to_flat_string(),
            context,
            span,
        );
    }
    FlattenError::missing_source_context(format!(
        "class instance `{}` for {context} has no source provenance",
        class_data.qualified_name.to_flat_string()
    ))
}

fn class_index_span(tree: &ClassTree, def_id: rumoca_core::DefId) -> Option<rumoca_core::Span> {
    let class_def = tree.get_class_by_def_id(def_id)?;
    required_location_span(
        &tree.source_map,
        &class_def.location,
        "class instance override scope",
    )
    .ok()
}

fn insert_component_overrides(
    map: &mut ComponentOverrideMap,
    path: ComponentPath,
    overrides: rustc_hash::FxHashMap<String, OverrideTarget>,
) {
    if !overrides.is_empty() {
        map.insert(path, overrides);
    }
}

fn root_class_component_overrides(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    model_name: &str,
) -> rustc_hash::FxHashMap<String, OverrideTarget> {
    let mut overrides = rustc_hash::FxHashMap::default();
    let Some(class_def) = class_index.get_by_qualified_name(model_name) else {
        return overrides;
    };
    let mut visited_classes = FxHashSet::default();
    collect_component_constructor_aliases_for_class(
        tree,
        class_index,
        class_def,
        model_name,
        true,
        &mut visited_classes,
        &mut overrides,
    );
    overrides
}
