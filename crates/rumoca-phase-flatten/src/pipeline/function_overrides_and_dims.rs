use super::*;
use crate::path_utils::{parent_scope, top_level_last_segment};
use rumoca_core::{ComponentPath, ExpressionRewriter, StatementRewriter, Token};
use rumoca_ir_ast::ExpressionTransformer;
use rustc_hash::FxHashSet;

mod flat_rewrite;
mod member_calls;
mod named_args;

pub(crate) use flat_rewrite::*;
pub(crate) use member_calls::*;
use named_args::{named_function_arg, named_function_arg_names};

pub(crate) type ComponentOverrideMap =
    rustc_hash::FxHashMap<ComponentPath, rustc_hash::FxHashMap<String, OverrideTarget>>;
type OverrideFunctionMap = rustc_hash::FxHashMap<String, OverrideTarget>;
type OverrideContext = (Vec<OverrideTarget>, OverrideFunctionMap);
type ConstructorOverrideCache =
    rustc_hash::FxHashMap<rumoca_core::DefId, rustc_hash::FxHashMap<String, OverrideTarget>>;

#[derive(Clone, Debug)]
struct FunctionModifierArg {
    name: String,
    value: rumoca_ir_ast::Expression,
    span: rumoca_core::Span,
}

struct ResolvedClassRef<'a> {
    name: String,
    def_id: rumoca_core::DefId,
    class_def: &'a rumoca_ir_ast::ClassDef,
}

#[derive(Clone, Debug)]
pub(crate) struct OverrideTarget {
    alias: String,
    pub(crate) name: String,
    def_id: rumoca_core::DefId,
    class_type: rumoca_core::ClassType,
    active: bool,
    modifier_args: Vec<FunctionModifierArg>,
}

impl OverrideTarget {
    fn from_resolved(alias: impl Into<String>, target: ResolvedClassRef<'_>, active: bool) -> Self {
        Self::from_resolved_with_modifier_args(alias, target, active, Vec::new())
    }

    fn from_resolved_with_modifier_args(
        alias: impl Into<String>,
        target: ResolvedClassRef<'_>,
        active: bool,
        modifier_args: Vec<FunctionModifierArg>,
    ) -> Self {
        Self {
            alias: alias.into(),
            name: target.name,
            def_id: target.def_id,
            class_type: target.class_def.class_type.clone(),
            active,
            modifier_args,
        }
    }

    fn is_package(&self) -> bool {
        self.class_type == rumoca_core::ClassType::Package
    }
}

fn is_receiver_alias_type(class_type: &rumoca_core::ClassType) -> bool {
    matches!(
        class_type,
        rumoca_core::ClassType::Package
            | rumoca_core::ClassType::Function
            | rumoca_core::ClassType::Record
            | rumoca_core::ClassType::Model
            | rumoca_core::ClassType::Block
            | rumoca_core::ClassType::Class
    )
}

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
            let active_alias = active_aliases && top_level_last_segment(&target_ref.name) != alias;
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

fn collect_extends_redeclare_aliases_for_class(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    class_def: &rumoca_ir_ast::ClassDef,
    class_scope: &str,
    overrides: &mut rustc_hash::FxHashMap<String, OverrideTarget>,
) {
    for ext in &class_def.extends {
        for modification in &ext.modifications {
            if !modification.redeclare {
                continue;
            }
            let Some((alias, value)) = redeclare_alias_and_value(&modification.expr) else {
                continue;
            };
            let Some(target_ref) = redeclare_value_type_ref(tree, class_index, class_scope, value)
            else {
                continue;
            };
            if is_receiver_alias_type(&target_ref.class_def.class_type) {
                let active_redeclare = top_level_last_segment(&target_ref.name) != alias;
                overrides.insert(
                    alias.clone(),
                    OverrideTarget::from_resolved_with_modifier_args(
                        alias,
                        target_ref,
                        active_redeclare,
                        redeclare_value_modifier_args(value),
                    ),
                );
            }
        }
    }
}

fn redeclare_alias_and_value(
    expr: &rumoca_ir_ast::Expression,
) -> Option<(String, &rumoca_ir_ast::Expression)> {
    match expr {
        rumoca_ir_ast::Expression::Modification { target, value, .. } => {
            Some((single_component_ref_name(target)?, value.as_ref()))
        }
        rumoca_ir_ast::Expression::Binary {
            op: rumoca_core::OpBinary::Assign,
            lhs,
            rhs,
            ..
        } => Some((redeclare_lhs_alias(lhs)?, rhs.as_ref())),
        _ => None,
    }
}

fn redeclare_lhs_alias(expr: &rumoca_ir_ast::Expression) -> Option<String> {
    match expr {
        rumoca_ir_ast::Expression::ComponentReference(target) => single_component_ref_name(target),
        rumoca_ir_ast::Expression::ClassModification { target, .. } => {
            single_component_ref_name(target)
        }
        _ => None,
    }
}

fn redeclare_value_type_ref<'a>(
    tree: &'a ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'a>,
    class_scope: &str,
    value: &rumoca_ir_ast::Expression,
) -> Option<ResolvedClassRef<'a>> {
    let cref = match value {
        rumoca_ir_ast::Expression::ComponentReference(cref) => cref,
        rumoca_ir_ast::Expression::FunctionCall { comp, .. } => comp,
        rumoca_ir_ast::Expression::ClassModification { target, .. } => target,
        _ => return None,
    };
    if let Some(def_id) = cref.def_id {
        return Some(ResolvedClassRef {
            name: tree.def_map.get(&def_id)?.clone(),
            def_id,
            class_def: class_index.get(def_id)?,
        });
    }
    let name = resolve_class_ref_name(tree, cref).or_else(|| {
        resolve_class_in_scope_indexed(class_index, &cref.to_string(), class_scope).1
    })?;
    let class_def = class_index.get_by_qualified_name(&name)?;
    Some(ResolvedClassRef {
        def_id: class_def.def_id?,
        class_def,
        name,
    })
}

fn redeclare_value_modifier_args(value: &rumoca_ir_ast::Expression) -> Vec<FunctionModifierArg> {
    let args = match value {
        rumoca_ir_ast::Expression::FunctionCall { args, .. } => args,
        rumoca_ir_ast::Expression::ClassModification { modifications, .. } => modifications,
        _ => return Vec::new(),
    };
    args.iter()
        .filter_map(function_modifier_arg_from_ast)
        .collect()
}

fn function_modifier_arg_from_ast(expr: &rumoca_ir_ast::Expression) -> Option<FunctionModifierArg> {
    match expr {
        rumoca_ir_ast::Expression::NamedArgument { name, value, span } => {
            Some(FunctionModifierArg {
                name: name.text.to_string(),
                value: value.as_ref().clone(),
                span: *span,
            })
        }
        rumoca_ir_ast::Expression::Modification {
            target,
            value,
            span,
        } => Some(FunctionModifierArg {
            name: single_component_ref_name(target)?,
            value: value.as_ref().clone(),
            span: *span,
        }),
        _ => None,
    }
}

fn resolved_class_ref_for_def_id<'a>(
    tree: &'a ClassTree,
    class_index: &'a rumoca_ir_ast::ClassDefIndex<'a>,
    def_id: rumoca_core::DefId,
) -> Option<ResolvedClassRef<'a>> {
    Some(ResolvedClassRef {
        name: tree.def_map.get(&def_id)?.clone(),
        def_id,
        class_def: class_index.get(def_id)?,
    })
}

fn resolve_package_alias_chain<'a>(
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

fn resolve_class_ref_name(
    tree: &ClassTree,
    cref: &rumoca_ir_ast::ComponentReference,
) -> Option<String> {
    if let Some(name) = cref.def_id.and_then(|def_id| tree.def_map.get(&def_id)) {
        return Some(name.clone());
    }

    let first = cref.parts.first()?;
    let mut current = tree
        .definitions
        .classes
        .get(first.ident.text.as_ref())
        .or_else(|| {
            cref.def_id
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

fn component_class_override_is_active(
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
        || top_level_last_segment(&target_ref.name) != class_override.alias.as_str()
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
    let span = class_data
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
        .unwrap_or(rumoca_core::Span::DUMMY);
    FlattenError::missing_source_scope(class_data.qualified_name.to_flat_string(), context, span)
}

fn class_index_span(tree: &ClassTree, def_id: rumoca_core::DefId) -> Option<rumoca_core::Span> {
    let class_def = tree.get_class_by_def_id(def_id)?;
    let location = &class_def.location;
    if location.file_name.is_empty() || location.start >= location.end {
        return None;
    }
    Some(tree.source_map.location_to_span(
        &location.file_name,
        location.start as usize,
        location.end as usize,
    ))
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

pub(crate) fn override_context_for_scope(
    scope: &str,
    component_override_map: &ComponentOverrideMap,
) -> (Vec<OverrideTarget>, OverrideFunctionMap) {
    let scope_path = ComponentPath::from_flat_path(scope);
    override_context_for_component_path(&scope_path, component_override_map)
}

pub(crate) fn override_context_for_component_path(
    scope_path: &ComponentPath,
    component_override_map: &ComponentOverrideMap,
) -> (Vec<OverrideTarget>, OverrideFunctionMap) {
    fn apply_scope_override<'a>(
        alias: &'a str,
        target: &OverrideTarget,
        packages: &mut Vec<OverrideTarget>,
        package_aliases: &mut rustc_hash::FxHashMap<&'a str, usize>,
        function_overrides: &mut OverrideFunctionMap,
    ) {
        if target.is_package() {
            if let Some(index) = package_aliases.get(alias).copied() {
                update_package_override_slot(packages, index, target);
            } else {
                package_aliases.insert(alias, packages.len());
                packages.push(target.clone());
            }
        }
        update_function_override_entry(function_overrides, alias, target);
    }

    let estimated_overrides = override_scope_entry_count(scope_path, component_override_map);
    let mut packages = Vec::new();
    let mut package_aliases = rustc_hash::FxHashMap::default();
    let mut function_overrides = OverrideFunctionMap::default();
    packages.reserve(estimated_overrides);
    package_aliases.reserve(estimated_overrides);
    function_overrides.reserve(estimated_overrides);
    for path_parts in scope_chain_inner_to_outer_parts(scope_path) {
        if let Some(path_overrides) = component_override_map.get(path_parts) {
            for (alias, target) in path_overrides {
                apply_scope_override(
                    alias,
                    target,
                    &mut packages,
                    &mut package_aliases,
                    &mut function_overrides,
                );
            }
        }
    }
    if let Some(path_overrides) = root_override_entries(component_override_map) {
        for (alias, target) in path_overrides {
            apply_scope_override(
                alias,
                target,
                &mut packages,
                &mut package_aliases,
                &mut function_overrides,
            );
        }
    }
    (packages, function_overrides)
}

fn update_function_override_entry(
    function_overrides: &mut OverrideFunctionMap,
    alias: &str,
    target: &OverrideTarget,
) {
    match function_overrides.get(alias) {
        Some(existing) if target.active && !existing.active => {
            function_overrides.insert(alias.to_string(), target.clone());
        }
        Some(_) => {}
        None => {
            function_overrides.insert(alias.to_string(), target.clone());
        }
    }
}

fn update_package_override_slot(
    packages: &mut [OverrideTarget],
    index: usize,
    target: &OverrideTarget,
) {
    if target.active && !packages[index].active {
        packages[index] = target.clone();
    }
}

pub(crate) fn override_aliases_for_component_path(
    scope_path: &ComponentPath,
    component_override_map: &ComponentOverrideMap,
) -> Vec<(String, String)> {
    let (packages, _) = override_context_for_component_path(scope_path, component_override_map);
    packages
        .into_iter()
        .map(|target| (target.alias, target.name))
        .collect()
}

pub(crate) fn override_package_names(override_packages: &[OverrideTarget]) -> Vec<String> {
    override_package_names_with_preferred_aliases(override_packages, &[])
}

pub(crate) fn override_package_names_with_preferred_aliases(
    override_packages: &[OverrideTarget],
    preferred_aliases: &[String],
) -> Vec<String> {
    let mut names = Vec::with_capacity(override_packages.len());
    for alias in preferred_aliases {
        names.extend(
            override_packages
                .iter()
                .filter(|target| &target.alias == alias)
                .map(|target| target.name.clone()),
        );
    }
    override_packages
        .iter()
        .filter(|target| !preferred_aliases.iter().any(|alias| alias == &target.alias))
        .map(|target| target.name.clone())
        .for_each(|name| names.push(name));
    names
}

fn scope_chain_inner_to_outer_parts(
    scope_path: &ComponentPath,
) -> impl Iterator<Item = &[String]> + '_ {
    (1..=scope_path.len())
        .rev()
        .map(|end| &scope_path.parts()[..end])
}

fn root_override_entries(
    component_override_map: &ComponentOverrideMap,
) -> Option<&rustc_hash::FxHashMap<String, OverrideTarget>> {
    let root: &[String] = &[];
    component_override_map.get(root)
}

fn override_scope_entry_count(
    scope_path: &ComponentPath,
    component_override_map: &ComponentOverrideMap,
) -> usize {
    let scoped_count = scope_chain_inner_to_outer_parts(scope_path)
        .filter_map(|path_parts| component_override_map.get(path_parts))
        .map(rustc_hash::FxHashMap::len)
        .sum::<usize>();
    scoped_count
        + root_override_entries(component_override_map)
            .map(rustc_hash::FxHashMap::len)
            .unwrap_or(0)
}

fn override_context_cache_key(
    scope_path: &ComponentPath,
    component_override_map: &ComponentOverrideMap,
) -> ComponentPath {
    for end in (1..=scope_path.len()).rev() {
        let Some(prefix) = scope_path.prefix(end) else {
            continue;
        };
        if component_override_map.contains_key(prefix.parts()) {
            return prefix;
        }
    }
    ComponentPath::root()
}

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

fn package_chain_contains_def_id(
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

fn resolve_member_in_package_chain_exposed(
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

pub(crate) fn resolve_override_function_name(
    reference: &rumoca_core::Reference,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> Option<String> {
    let scope = reference.component_scope()?;
    let function_leaf = scope.leaf_ident()?;
    if let Some(source_package_def_id) = reference_source_package_def_id(reference, ctx)
        && let Some(package) = ctx.lexical_package_target()
        && ctx.package_chain_contains_def_id(&package, source_package_def_id)
        && let Some(resolved) = resolve_function_in_package_chain_exposed(
            ctx.tree,
            ctx.class_index,
            &package,
            function_leaf,
        )
        .filter(|resolved| resolved != reference.as_str())
    {
        return Some(resolved);
    }
    if scope.parent_ident().is_none() {
        if let Some(source_package_def_id) = reference_source_package_def_id(reference, ctx)
            && let Some(package) =
                ctx.concrete_override_package_for_source_package(source_package_def_id)
        {
            return resolve_function_in_package_chain_exposed(
                ctx.tree,
                ctx.class_index,
                package,
                function_leaf,
            )
            .filter(|resolved| resolved != reference.as_str());
        }
        if let Some(package) = ctx.lexical_package_target()
            && let Some(resolved) = resolve_function_in_package_chain(
                ctx.tree,
                ctx.class_index,
                &package,
                function_leaf,
            )
            && resolved != reference.as_str()
        {
            return Some(resolved);
        }
        let package = ctx.lexical_override_package()?;
        return resolve_function_in_package_chain_exposed(
            ctx.tree,
            ctx.class_index,
            package,
            function_leaf,
        )
        .filter(|resolved| resolved != reference.as_str());
    }

    if let Some(source_package_def_id) = reference_source_package_def_id(reference, ctx)
        && let Some(package) =
            ctx.concrete_override_package_for_source_package(source_package_def_id)
    {
        return resolve_function_in_package_chain_exposed(
            ctx.tree,
            ctx.class_index,
            package,
            function_leaf,
        )
        .filter(|resolved| resolved != reference.as_str());
    }

    if let Some(package_scope) = reference_package_scope(reference)
        && let Some(package) = ctx
            .override_packages
            .iter()
            .find(|package| package.name == package_scope)
    {
        return resolve_function_in_package_chain_exposed(
            ctx.tree,
            ctx.class_index,
            package,
            function_leaf,
        )
        .filter(|resolved| resolved != reference.as_str());
    }

    let package_alias = scope.parent_ident()?;
    let package = ctx.override_package(package_alias)?;
    // Only resolve through the alias-matched package when the call's actual
    // source package is unknown (a genuinely relative reference) or is part of
    // that package's chain. A fully-qualified call into a *different* package
    // that merely shares its leaf identifier with the alias (e.g.
    // `A.Quat.f` calling `B.Quat.f` from within `A.Quat`) must NOT be rewritten
    // to the caller's own package — that would alias the call to itself. The
    // earlier resolution blocks apply this same chain guard.
    if let Some(source_package_def_id) = reference_source_package_def_id(reference, ctx)
        && !ctx.package_chain_contains_def_id(package, source_package_def_id)
    {
        return None;
    }
    resolve_function_in_package_chain_exposed(ctx.tree, ctx.class_index, package, function_leaf)
        .filter(|resolved| resolved != reference.as_str())
}

fn reference_package_scope(reference: &rumoca_core::Reference) -> Option<String> {
    let scope = reference.component_scope()?;
    let prefix_parts = scope.prefix_parts();
    (!prefix_parts.is_empty()).then(|| {
        ComponentPath::from_parts(prefix_parts.iter().map(|part| part.ident.as_str()))
            .to_flat_string()
    })
}

fn reference_source_package_def_id(
    reference: &rumoca_core::Reference,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> Option<rumoca_core::DefId> {
    reference_source_package_def_id_from_index(reference, ctx.class_index)
}

fn reference_source_package_def_id_from_index(
    reference: &rumoca_core::Reference,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> Option<rumoca_core::DefId> {
    if let Some(source_package_def_id) = reference
        .target_def_id()
        .and_then(|def_id| class_index.parent_def_id(def_id))
    {
        return Some(source_package_def_id);
    }
    let package_name = parent_scope(reference.as_str())?;
    class_index
        .get_by_qualified_name(package_name)
        .and_then(|class_def| class_def.def_id)
}

fn resolve_override_member_name(
    reference: &rumoca_core::Reference,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> Option<String> {
    if reference_component_ref_is_instance_path(reference, ctx) {
        return None;
    }
    if let Some(resolved) = resolve_override_member_projection_name(reference, ctx) {
        return Some(resolved);
    }
    let scope = reference.component_scope()?;
    let member_leaf = scope.leaf_ident()?;
    let package = if let Some(source_package_def_id) = reference
        .target_def_id()
        .and_then(|def_id| ctx.class_index.parent_def_id(def_id))
    {
        ctx.active_override_package_for_source_package(source_package_def_id)?
    } else {
        ctx.unique_active_override_package()?
    };
    resolve_member_in_package_chain_exposed(ctx.tree, ctx.class_index, package, member_leaf)
        .filter(|resolved| resolved != reference.as_str())
}

fn resolve_override_member_projection_name(
    reference: &rumoca_core::Reference,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> Option<String> {
    let path = ComponentPath::from_reference(reference);
    let parts = path.parts();
    if parts.len() < 2 {
        return None;
    }

    for member_index in (1..parts.len()).rev() {
        let source_package = path.prefix(member_index)?.to_flat_string();
        let Some(source_package_def_id) = ctx
            .class_index
            .get_by_qualified_name(&source_package)
            .and_then(|class_def| class_def.def_id)
        else {
            continue;
        };
        let Some(package) = ctx.concrete_override_package_for_source_package(source_package_def_id)
        else {
            continue;
        };
        let member_leaf = parts[member_index].as_str();
        let Some(resolved_member) = resolve_member_in_package_chain_exposed(
            ctx.tree,
            ctx.class_index,
            package,
            member_leaf,
        ) else {
            continue;
        };
        let resolved = ComponentPath::from_flat_path(&resolved_member)
            .join_part_slice(&parts[member_index + 1..])
            .to_flat_string();
        if resolved != reference.as_str() {
            return Some(resolved);
        }
    }

    None
}

fn reference_component_ref_is_instance_path(
    reference: &rumoca_core::Reference,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> bool {
    canonical_instance_reference_name(reference, ctx).is_some()
}

fn canonical_instance_reference_name(
    reference: &rumoca_core::Reference,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> Option<rumoca_core::Reference> {
    let component_ref = reference.component_ref()?;
    let component_path = ComponentPath::from_component_reference(component_ref);
    let component_name = component_path.to_flat_string();
    let is_known_instance_path = ctx
        .component_members
        .is_some_and(|scope| scope.contains_component_path(&component_path));
    ((is_known_instance_path || component_name != reference.as_str())
        && ctx
            .class_index
            .get_by_qualified_name(&component_name)
            .is_none()
        && parent_scope(&component_name)
            .is_none_or(|scope| ctx.class_index.get_by_qualified_name(scope).is_none()))
    .then(|| {
        rumoca_core::Reference::with_component_reference(component_name, component_ref.clone())
    })
}

pub(crate) fn resolve_function_extends_target(
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    current_name: &str,
) -> Option<String> {
    let mut current = current_name.to_string();
    let mut visited = FxHashSet::default();

    loop {
        if !visited.insert(current.clone()) {
            return None;
        }
        let class_def = class_index.get_by_qualified_name(&current)?;
        if class_def.class_type != rumoca_core::ClassType::Function {
            return None;
        }
        if !class_def.algorithms.is_empty() || class_def.external.is_some() {
            return (current != current_name).then_some(current);
        }

        let mut next_function: Option<String> = None;
        for ext in &class_def.extends {
            let base_name = ext.base_name.to_string();
            let resolved_base = resolve_class_in_scope_indexed(class_index, &base_name, &current)
                .1
                .or_else(|| {
                    class_index
                        .get_by_qualified_name(&base_name)
                        .map(|_| base_name.clone())
                });
            let Some(candidate) = resolved_base else {
                continue;
            };
            let Some(candidate_class) = class_index.get_by_qualified_name(&candidate) else {
                continue;
            };
            if candidate_class.class_type != rumoca_core::ClassType::Function {
                continue;
            }
            match &next_function {
                Some(existing) if existing != &candidate => return None,
                Some(_) => {}
                None => next_function = Some(candidate),
            }
        }
        current = next_function?;
    }
}

pub(crate) struct FunctionOverrideRewriteContext<'a> {
    tree: &'a ClassTree,
    class_index: &'a rumoca_ir_ast::ClassDefIndex<'a>,
    override_packages: &'a [OverrideTarget],
    override_functions: &'a OverrideFunctionMap,
    component_members: Option<&'a component_member_scope::ComponentMemberScopes>,
    active_scope: ComponentPath,
    local_def_ids: FxHashSet<rumoca_core::DefId>,
    lexical_package_def_id: Option<rumoca_core::DefId>,
    package_chain_cache: std::cell::RefCell<
        rustc_hash::FxHashMap<rumoca_core::DefId, rustc_hash::FxHashSet<rumoca_core::DefId>>,
    >,
}

impl<'a> FunctionOverrideRewriteContext<'a> {
    fn new(
        tree: &'a ClassTree,
        class_index: &'a rumoca_ir_ast::ClassDefIndex<'a>,
        override_packages: &'a [OverrideTarget],
        override_functions: &'a OverrideFunctionMap,
    ) -> Self {
        Self {
            tree,
            class_index,
            override_packages,
            override_functions,
            component_members: None,
            active_scope: ComponentPath::root(),
            local_def_ids: FxHashSet::default(),
            lexical_package_def_id: None,
            package_chain_cache: std::cell::RefCell::new(rustc_hash::FxHashMap::default()),
        }
    }

    fn with_active_scope(mut self, active_scope: ComponentPath) -> Self {
        self.active_scope = active_scope;
        self
    }

    fn with_component_member_scope(
        mut self,
        component_members: &'a component_member_scope::ComponentMemberScopes,
    ) -> Self {
        self.component_members = Some(component_members);
        self
    }

    fn with_local_def_ids(mut self, local_def_ids: FxHashSet<rumoca_core::DefId>) -> Self {
        self.local_def_ids = local_def_ids;
        self
    }

    fn with_lexical_package_def_id(
        mut self,
        lexical_package_def_id: Option<rumoca_core::DefId>,
    ) -> Self {
        self.lexical_package_def_id = lexical_package_def_id;
        self
    }

    fn override_package(&self, alias: &str) -> Option<&'a OverrideTarget> {
        self.override_packages
            .iter()
            .find(|package| package.alias == alias)
    }

    fn package_chain_contains_def_id(
        &self,
        package: &OverrideTarget,
        query_def_id: rumoca_core::DefId,
    ) -> bool {
        if !self
            .package_chain_cache
            .borrow()
            .contains_key(&package.def_id)
        {
            let mut chain = Vec::new();
            let mut visited = FxHashSet::default();
            collect_package_chain(
                self.tree,
                self.class_index,
                &package.name,
                &mut chain,
                &mut visited,
            );
            self.package_chain_cache
                .borrow_mut()
                .insert(package.def_id, chain.into_iter().collect());
        }
        self.package_chain_cache
            .borrow()
            .get(&package.def_id)
            .is_some_and(|chain| chain.contains(&query_def_id))
    }

    fn lexical_package_target(&self) -> Option<OverrideTarget> {
        let def_id = self.lexical_package_def_id?;
        let name = self.tree.def_map.get(&def_id)?.clone();
        let class_def = self.class_index.get(def_id)?;
        Some(OverrideTarget {
            alias: top_level_last_segment(&name).to_string(),
            name,
            def_id,
            class_type: class_def.class_type.clone(),
            active: false,
            modifier_args: Vec::new(),
        })
    }

    fn lexical_override_package(&self) -> Option<&'a OverrideTarget> {
        let lexical_package_def_id = self.lexical_package_def_id?;
        if let Some(lexical_alias) = self
            .tree
            .def_map
            .get(&lexical_package_def_id)
            .map(|name| top_level_last_segment(name))
        {
            let mut matches = self.override_packages.iter().filter(|package| {
                package.alias == lexical_alias && package.def_id != lexical_package_def_id
            });
            let package = matches.next();
            if package.is_some() && matches.next().is_none() {
                return package;
            }
        }
        let mut matches = self
            .override_packages
            .iter()
            .filter(|package| self.package_chain_contains_def_id(package, lexical_package_def_id));
        let package = matches.next()?;
        matches.next().is_none().then_some(package)
    }

    fn active_override_package_for_source_package(
        &self,
        source_package_def_id: rumoca_core::DefId,
    ) -> Option<&'a OverrideTarget> {
        let mut matches = self.override_packages.iter().filter(|package| {
            package.active && self.package_chain_contains_def_id(package, source_package_def_id)
        });
        let package = matches.next()?;
        matches.next().is_none().then_some(package)
    }

    fn concrete_override_package_for_source_package(
        &self,
        source_package_def_id: rumoca_core::DefId,
    ) -> Option<&'a OverrideTarget> {
        if let Some(package) =
            self.active_override_package_for_source_package(source_package_def_id)
        {
            return Some(package);
        }
        let mut matches = self
            .override_packages
            .iter()
            .filter(|package| self.package_chain_contains_def_id(package, source_package_def_id));
        let package = matches.next()?;
        matches.next().is_none().then_some(package)
    }

    fn unique_active_override_package(&self) -> Option<&'a OverrideTarget> {
        let mut matches = self
            .override_packages
            .iter()
            .filter(|package| package.active);
        let package = matches.next()?;
        matches.next().is_none().then_some(package)
    }
}

pub(crate) fn resolve_function_override_name(
    reference: &rumoca_core::Reference,
    is_constructor: bool,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> Option<String> {
    let current_name = reference.as_str();
    let leaf = reference.last_segment();
    let mut resolved = ctx.override_functions.get(leaf).and_then(|candidate| {
        let class_def = ctx.class_index.get(candidate.def_id)?;
        let can_rewrite = if is_constructor {
            !matches!(class_def.class_type, rumoca_core::ClassType::Package)
        } else {
            class_def.class_type == rumoca_core::ClassType::Function
        };
        can_rewrite.then(|| candidate.name.clone())
    });
    if resolved.as_deref() == Some(current_name) {
        resolved = None;
    }
    if !is_constructor {
        if resolved.is_none()
            && let Some(candidate) = resolve_override_function_name(reference, ctx)
            && candidate != current_name
        {
            resolved = Some(candidate);
        }
        if resolved.is_none()
            && let Some(candidate) = canonical_function_name_from_component_ref(reference, ctx)
            && candidate != current_name
        {
            resolved = Some(candidate);
        }
        if resolved.is_none()
            && let Some(canonical_name) =
                canonical_function_name_from_target_def_id(reference, is_constructor, ctx)
            && canonical_name != current_name
        {
            resolved = Some(canonical_name);
        }
        if resolved.is_none() {
            resolved = resolve_function_extends_target(ctx.class_index, current_name);
        }
    }
    resolved
}

fn canonical_function_name_from_component_ref(
    reference: &rumoca_core::Reference,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> Option<String> {
    let component_ref = reference.component_ref()?;
    let component_name = ComponentPath::from_component_reference(component_ref).to_flat_string();
    if component_name == reference.as_str() {
        return None;
    }
    if let Some(class_def) = ctx.class_index.get_by_qualified_name(&component_name)
        && class_def.class_type == rumoca_core::ClassType::Function
        && !class_def.partial
    {
        return Some(component_name);
    }

    let (package_name, function_leaf) = component_ref_package_and_leaf(component_ref)?;
    let package_def = ctx.class_index.get_by_qualified_name(&package_name)?;
    let package = OverrideTarget {
        alias: String::new(),
        name: package_name,
        def_id: package_def.def_id?,
        class_type: package_def.class_type.clone(),
        active: false,
        modifier_args: Vec::new(),
    };
    resolve_function_in_package_chain_exposed(ctx.tree, ctx.class_index, &package, function_leaf)
}

fn component_ref_package_and_leaf(
    component_ref: &rumoca_core::ComponentReference,
) -> Option<(String, &str)> {
    let mut parts = component_ref.parts.as_slice();
    let leaf = parts.last()?.ident.as_str();
    parts = &parts[..parts.len().checked_sub(1)?];
    (!parts.is_empty()).then(|| {
        (
            ComponentPath::from_parts(parts.iter().map(|part| part.ident.as_str()))
                .to_flat_string(),
            leaf,
        )
    })
}

fn canonical_function_name_from_target_def_id(
    reference: &rumoca_core::Reference,
    is_constructor: bool,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> Option<String> {
    if component_ref_names_exposed_function(reference, ctx) {
        return None;
    }
    let def_id = reference.target_def_id()?;
    let class_def = ctx.class_index.get(def_id)?;
    let can_canonicalize = if is_constructor {
        !matches!(class_def.class_type, rumoca_core::ClassType::Package)
    } else {
        class_def.class_type == rumoca_core::ClassType::Function && !class_def.partial
    };
    can_canonicalize.then(|| ctx.tree.def_map.get(&def_id).cloned())?
}

fn component_ref_names_exposed_function(
    reference: &rumoca_core::Reference,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> bool {
    let Some(component_ref) = reference.component_ref() else {
        return false;
    };
    let component_name = ComponentPath::from_component_reference(component_ref).to_flat_string();
    if component_name != reference.as_str() {
        return false;
    }
    if let Some(class_def) = ctx.class_index.get_by_qualified_name(&component_name) {
        return class_def.class_type == rumoca_core::ClassType::Function;
    }

    let Some((package_name, function_leaf)) = component_ref_package_and_leaf(component_ref) else {
        return false;
    };
    let Some(package_def) = ctx.class_index.get_by_qualified_name(&package_name) else {
        return false;
    };
    let Some(package_def_id) = package_def.def_id else {
        return false;
    };
    let package = OverrideTarget {
        alias: String::new(),
        name: package_name,
        def_id: package_def_id,
        class_type: package_def.class_type.clone(),
        active: false,
        modifier_args: Vec::new(),
    };
    resolve_function_in_package_chain_exposed(ctx.tree, ctx.class_index, &package, function_leaf)
        .as_deref()
        == Some(component_name.as_str())
}

pub(crate) fn rewrite_function_overrides_in_expression_with_ctx(
    expr: &mut Expression,
    ctx: &FunctionOverrideRewriteContext<'_>,
) {
    if ctx.override_packages.is_empty()
        && ctx.override_functions.is_empty()
        && !expression_contains_function_call(expr)
    {
        return;
    }
    *expr = FunctionOverrideExpressionRewriter { ctx }.rewrite_expression(expr);
}

fn expression_contains_function_call(expr: &Expression) -> bool {
    match expr {
        Expression::FunctionCall { .. } => true,
        Expression::Binary { lhs, rhs, .. } => {
            expression_contains_function_call(lhs) || expression_contains_function_call(rhs)
        }
        Expression::Unary { rhs, .. } => expression_contains_function_call(rhs),
        Expression::BuiltinCall { args, .. }
        | Expression::Array { elements: args, .. }
        | Expression::Tuple { elements: args, .. } => {
            args.iter().any(expression_contains_function_call)
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, value)| {
                expression_contains_function_call(condition)
                    || expression_contains_function_call(value)
            }) || expression_contains_function_call(else_branch)
        }
        Expression::Range {
            start, step, end, ..
        } => {
            expression_contains_function_call(start)
                || step
                    .as_deref()
                    .is_some_and(expression_contains_function_call)
                || expression_contains_function_call(end)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            expression_contains_function_call(expr)
                || indices
                    .iter()
                    .any(|index| expression_contains_function_call(&index.range))
                || filter
                    .as_deref()
                    .is_some_and(expression_contains_function_call)
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            expression_contains_function_call(base)
                || subscripts.iter().any(subscript_contains_function_call)
        }
        Expression::FieldAccess { base, .. } => expression_contains_function_call(base),
        Expression::VarRef { subscripts, .. } => {
            subscripts.iter().any(subscript_contains_function_call)
        }
        Expression::Literal { .. } | Expression::Empty { .. } => false,
    }
}

fn subscript_contains_function_call(subscript: &rumoca_core::Subscript) -> bool {
    match subscript {
        rumoca_core::Subscript::Expr { expr, .. } => expression_contains_function_call(expr),
        rumoca_core::Subscript::Index { .. } | rumoca_core::Subscript::Colon { .. } => false,
    }
}

struct FunctionOverrideExpressionRewriter<'a> {
    ctx: &'a FunctionOverrideRewriteContext<'a>,
}

impl ExpressionRewriter for FunctionOverrideExpressionRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        if let Expression::VarRef {
            name,
            subscripts,
            span,
        } = expr
        {
            let rewritten_subscripts = self.rewrite_subscripts(subscripts);
            if reference_targets_function_local_def(name, self.ctx) {
                return Expression::VarRef {
                    name: name.clone(),
                    subscripts: rewritten_subscripts,
                    span: *span,
                };
            }
            if let Some(resolved_name) = resolve_override_member_name(name, self.ctx) {
                return Expression::VarRef {
                    name: rewritten_reference(name, resolved_name, self.ctx),
                    subscripts: rewritten_subscripts,
                    span: *span,
                };
            }
            if let Some(canonical_name) = canonical_instance_reference_name(name, self.ctx) {
                return Expression::VarRef {
                    name: canonical_name,
                    subscripts: rewritten_subscripts,
                    span: *span,
                };
            }
            return Expression::VarRef {
                name: name.clone(),
                subscripts: rewritten_subscripts,
                span: *span,
            };
        }

        if let Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span,
        } = expr
        {
            let rewritten_args = self.rewrite_expressions(args);
            if reference_targets_function_local_def(name, self.ctx) {
                return Expression::FunctionCall {
                    name: name.clone(),
                    args: rewritten_args,
                    is_constructor: *is_constructor,
                    span: *span,
                };
            }
            let Some(resolved_name) =
                resolve_function_override_name(name, *is_constructor, self.ctx)
            else {
                return Expression::FunctionCall {
                    name: name.clone(),
                    args: rewritten_args,
                    is_constructor: *is_constructor,
                    span: *span,
                };
            };
            let args = append_replaceable_function_modifier_args(
                name,
                &resolved_name,
                rewritten_args,
                self.ctx,
            );
            return Expression::FunctionCall {
                name: rewritten_reference(name, resolved_name, self.ctx),
                args,
                is_constructor: *is_constructor,
                span: *span,
            };
        }
        self.walk_expression(expr)
    }
}

impl StatementRewriter for FunctionOverrideExpressionRewriter<'_> {}

fn function_local_def_ids(function: &rumoca_core::Function) -> FxHashSet<rumoca_core::DefId> {
    function
        .inputs
        .iter()
        .chain(function.outputs.iter())
        .chain(function.locals.iter())
        .filter_map(|param| param.def_id)
        .collect()
}

fn reference_targets_function_local_def(
    reference: &rumoca_core::Reference,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> bool {
    reference
        .target_def_id()
        .is_some_and(|def_id| ctx.local_def_ids.contains(&def_id))
}

fn rewritten_reference(
    original: &rumoca_core::Reference,
    resolved_name: String,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> rumoca_core::Reference {
    rewritten_function_reference(original, resolved_name, ctx.tree, ctx.class_index)
}

fn rewritten_function_reference(
    original: &rumoca_core::Reference,
    resolved_name: String,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> rumoca_core::Reference {
    let Some(mut component_ref) = original.component_ref().cloned() else {
        return rumoca_core::Reference::new(resolved_name);
    };
    component_ref.def_id = tree.name_map.get(&resolved_name).copied().or_else(|| {
        class_index
            .get_by_qualified_name(&resolved_name)
            .and_then(|class_def| class_def.def_id)
    });
    component_ref.parts = ComponentPath::from_flat_path(&resolved_name)
        .parts()
        .iter()
        .map(|part| rumoca_core::ComponentRefPart {
            ident: part.clone(),
            span: component_ref.span,
            subs: Vec::new(),
        })
        .collect();
    rumoca_core::Reference::with_component_reference(resolved_name, component_ref)
}

mod replaceable_modifiers;
use replaceable_modifiers::{append_replaceable_function_modifier_args, single_component_ref_name};

#[cfg(test)]
mod tests;
