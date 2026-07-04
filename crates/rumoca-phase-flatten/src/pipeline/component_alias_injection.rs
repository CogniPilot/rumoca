use super::*;
use crate::path_utils::{root_split, segments as path_segments_of};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct ComponentStaticConstantKey {
    class_def_id: rumoca_core::DefId,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct AliasPackageInjectionKey {
    scope: String,
    package_def_id: Option<rumoca_core::DefId>,
    package_context: String,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct AliasPackageStaticKey {
    package_def_id: rumoca_core::DefId,
    package_context: String,
}

enum ComponentStaticConstantCacheEntry {
    Cacheable(Box<ScopedConstantDelta>),
    Uncacheable,
}

#[derive(Default)]
struct ScopedKeySnapshot {
    parameter_values: rustc_hash::FxHashSet<String>,
    real_parameter_values: rustc_hash::FxHashSet<String>,
    boolean_parameter_values: rustc_hash::FxHashSet<String>,
    string_parameter_values: rustc_hash::FxHashSet<String>,
    enum_parameter_values: rustc_hash::FxHashSet<String>,
    constant_values: rustc_hash::FxHashSet<String>,
    class_constant_keys: rustc_hash::FxHashSet<String>,
    array_dimensions: rustc_hash::FxHashSet<String>,
    modified_constant_keys: rustc_hash::FxHashSet<String>,
}

impl ScopedKeySnapshot {
    fn capture(ctx: &Context, scope: &str) -> Self {
        Self {
            parameter_values: scoped_keys(&ctx.parameter_values, scope),
            real_parameter_values: scoped_keys(&ctx.real_parameter_values, scope),
            boolean_parameter_values: scoped_keys(&ctx.boolean_parameter_values, scope),
            string_parameter_values: scoped_keys(&ctx.string_parameter_values, scope),
            enum_parameter_values: scoped_keys(&ctx.enum_parameter_values, scope),
            constant_values: scoped_keys(&ctx.constant_values, scope),
            class_constant_keys: scoped_set_keys(&ctx.class_constant_keys, scope),
            array_dimensions: scoped_keys(&ctx.array_dimensions, scope),
            modified_constant_keys: ctx
                .modified_constant_keys
                .iter()
                .filter(|key| is_scoped_key(key, scope))
                .cloned()
                .collect(),
        }
    }
}

#[derive(Clone, Default)]
struct ScopedConstantDelta {
    parameter_values: Vec<(String, i64)>,
    real_parameter_values: Vec<(String, f64)>,
    boolean_parameter_values: Vec<(String, bool)>,
    string_parameter_values: Vec<(String, String)>,
    enum_parameter_values: Vec<(String, String)>,
    constant_values: Vec<(String, rumoca_core::Expression)>,
    class_constant_keys: Vec<String>,
    array_dimensions: Vec<(String, Vec<i64>)>,
    modified_constant_keys: Vec<String>,
}

impl ScopedConstantDelta {
    fn capture(ctx: &Context, scope: &str, before: &ScopedKeySnapshot) -> Option<Self> {
        let delta = Self {
            parameter_values: capture_map_delta(
                &ctx.parameter_values,
                scope,
                &before.parameter_values,
            ),
            real_parameter_values: capture_map_delta(
                &ctx.real_parameter_values,
                scope,
                &before.real_parameter_values,
            ),
            boolean_parameter_values: capture_map_delta(
                &ctx.boolean_parameter_values,
                scope,
                &before.boolean_parameter_values,
            ),
            string_parameter_values: capture_string_map_delta(
                &ctx.string_parameter_values,
                scope,
                &before.string_parameter_values,
            ),
            enum_parameter_values: capture_string_map_delta(
                &ctx.enum_parameter_values,
                scope,
                &before.enum_parameter_values,
            ),
            constant_values: capture_expression_map_delta(
                &ctx.constant_values,
                scope,
                &before.constant_values,
            )?,
            class_constant_keys: capture_set_delta(
                &ctx.class_constant_keys,
                scope,
                &before.class_constant_keys,
            ),
            array_dimensions: capture_map_delta(
                &ctx.array_dimensions,
                scope,
                &before.array_dimensions,
            ),
            modified_constant_keys: ctx
                .modified_constant_keys
                .iter()
                .filter(|key| {
                    is_scoped_key(key, scope) && !before.modified_constant_keys.contains(*key)
                })
                .map(|key| scoped_key_suffix(key, scope).to_string())
                .collect(),
        };
        Some(delta)
    }

    fn replay(&self, scope: &str, ctx: &mut Context) {
        replay_map_delta(
            &self.parameter_values,
            scope,
            &ctx.flat_parameter_constant_keys,
            &mut ctx.parameter_values,
        );
        replay_map_delta(
            &self.real_parameter_values,
            scope,
            &ctx.flat_parameter_constant_keys,
            &mut ctx.real_parameter_values,
        );
        replay_map_delta(
            &self.boolean_parameter_values,
            scope,
            &ctx.flat_parameter_constant_keys,
            &mut ctx.boolean_parameter_values,
        );
        replay_map_delta(
            &self.string_parameter_values,
            scope,
            &ctx.flat_parameter_constant_keys,
            &mut ctx.string_parameter_values,
        );
        replay_map_delta(
            &self.enum_parameter_values,
            scope,
            &ctx.flat_parameter_constant_keys,
            &mut ctx.enum_parameter_values,
        );
        replay_map_delta(
            &self.constant_values,
            scope,
            &ctx.flat_parameter_constant_keys,
            &mut ctx.constant_values,
        );
        replay_set_delta(
            &self.class_constant_keys,
            scope,
            &mut ctx.class_constant_keys,
        );
        replay_map_delta(
            &self.array_dimensions,
            scope,
            &ctx.flat_parameter_constant_keys,
            &mut ctx.array_dimensions,
        );
        for suffix in &self.modified_constant_keys {
            ctx.modified_constant_keys
                .insert(rebase_scoped_key(scope, suffix));
        }
    }
}

fn scoped_keys<V>(
    map: &rustc_hash::FxHashMap<String, V>,
    scope: &str,
) -> rustc_hash::FxHashSet<String> {
    map.keys()
        .filter(|key| is_scoped_key(key, scope))
        .cloned()
        .collect()
}

fn scoped_set_keys(
    set: &rustc_hash::FxHashSet<String>,
    scope: &str,
) -> rustc_hash::FxHashSet<String> {
    set.iter()
        .filter(|key| is_scoped_key(key, scope))
        .cloned()
        .collect()
}

fn is_scoped_key(key: &str, scope: &str) -> bool {
    key == scope
        || key
            .strip_prefix(scope)
            .is_some_and(|suffix| suffix.starts_with('.'))
}

fn scoped_key_suffix<'a>(key: &'a str, scope: &str) -> &'a str {
    key.strip_prefix(scope)
        .expect("scoped delta key must use the captured scope prefix")
}

fn rebase_scoped_key(scope: &str, suffix: &str) -> String {
    let mut key = String::with_capacity(scope.len() + suffix.len());
    key.push_str(scope);
    key.push_str(suffix);
    key
}

fn capture_map_delta<V: Clone>(
    map: &rustc_hash::FxHashMap<String, V>,
    scope: &str,
    before: &rustc_hash::FxHashSet<String>,
) -> Vec<(String, V)> {
    map.iter()
        .filter(|(key, _)| is_scoped_key(key, scope) && !before.contains(*key))
        .map(|(key, value)| (scoped_key_suffix(key, scope).to_string(), value.clone()))
        .collect()
}

fn capture_string_map_delta(
    map: &rustc_hash::FxHashMap<String, String>,
    scope: &str,
    before: &rustc_hash::FxHashSet<String>,
) -> Vec<(String, String)> {
    map.iter()
        .filter(|(key, value)| {
            is_scoped_key(key, scope)
                && !before.contains(*key)
                && !string_mentions_scope(value, scope)
        })
        .map(|(key, value)| (scoped_key_suffix(key, scope).to_string(), value.clone()))
        .collect()
}

fn capture_set_delta(
    set: &rustc_hash::FxHashSet<String>,
    scope: &str,
    before: &rustc_hash::FxHashSet<String>,
) -> Vec<String> {
    set.iter()
        .filter(|key| is_scoped_key(key, scope) && !before.contains(*key))
        .map(|key| scoped_key_suffix(key, scope).to_string())
        .collect()
}

fn capture_expression_map_delta(
    map: &rustc_hash::FxHashMap<String, rumoca_core::Expression>,
    scope: &str,
    before: &rustc_hash::FxHashSet<String>,
) -> Option<Vec<(String, rumoca_core::Expression)>> {
    let mut delta = Vec::new();
    for (key, value) in map {
        if !is_scoped_key(key, scope) || before.contains(key) {
            continue;
        }
        if expression_mentions_scope(value, scope) {
            return None;
        }
        delta.push((scoped_key_suffix(key, scope).to_string(), value.clone()));
    }
    Some(delta)
}

fn replay_map_delta<V: Clone>(
    delta: &[(String, V)],
    scope: &str,
    flat_parameter_constant_keys: &rustc_hash::FxHashSet<String>,
    map: &mut rustc_hash::FxHashMap<String, V>,
) {
    for (suffix, value) in delta {
        let key = rebase_scoped_key(scope, suffix);
        if flat_parameter_constant_keys.contains(&key) {
            continue;
        }
        map.entry(key).or_insert_with(|| value.clone());
    }
}

fn replay_set_delta(delta: &[String], scope: &str, set: &mut rustc_hash::FxHashSet<String>) {
    for suffix in delta {
        set.insert(rebase_scoped_key(scope, suffix));
    }
}

fn string_mentions_scope(value: &str, scope: &str) -> bool {
    is_scoped_key(value, scope)
}

fn expression_mentions_scope(expr: &rumoca_core::Expression, scope: &str) -> bool {
    struct PrefixChecker<'a> {
        scope: &'a str,
        found: bool,
    }

    impl rumoca_core::ExpressionVisitor for PrefixChecker<'_> {
        fn visit_var_ref(
            &mut self,
            name: &rumoca_core::Reference,
            subscripts: &[rumoca_core::Subscript],
        ) {
            if string_mentions_scope(name.as_str(), self.scope) {
                self.found = true;
                return;
            }
            self.walk_var_ref(name, subscripts);
        }

        fn visit_function_call(
            &mut self,
            name: &rumoca_core::Reference,
            args: &[rumoca_core::Expression],
            is_constructor: bool,
        ) {
            if string_mentions_scope(name.as_str(), self.scope) {
                self.found = true;
                return;
            }
            self.walk_function_call(name, args, is_constructor);
        }
    }

    let mut checker = PrefixChecker {
        scope,
        found: false,
    };
    rumoca_core::ExpressionVisitor::visit_expression(&mut checker, expr);
    checker.found
}

/// Inject nested package constants for every instantiated component scope.
///
/// Equations flattened under a component prefix (e.g., `tank`) can reference
/// nested package constants unqualified (`nX`) or qualified (`Medium.nX`).
/// This pass mirrors model-level nested constant extraction and emits scoped keys
/// like `tank.nX` and `tank.Medium.nX`.
pub(crate) fn inject_component_instance_nested_class_constants(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    overlay: &InstanceOverlay,
    ctx: &mut Context,
) {
    const MAX_PASSES: usize = 5;
    let component_scopes = component_scopes_from_overlay(overlay);
    let component_index = component_index_from_scopes(&component_scopes);
    let mut static_cache: rustc_hash::FxHashMap<
        ComponentStaticConstantKey,
        ComponentStaticConstantCacheEntry,
    > = rustc_hash::FxHashMap::default();
    let mut alias_package_injections = rustc_hash::FxHashSet::default();
    let mut alias_package_cache: rustc_hash::FxHashMap<
        AliasPackageStaticKey,
        ComponentStaticConstantCacheEntry,
    > = rustc_hash::FxHashMap::default();
    for _ in 0..MAX_PASSES {
        let mut cache_rejected = 0usize;
        let mut uncached = 0usize;
        let mut specialized_delta = 0usize;
        let prev = component_constant_footprint(ctx);

        for (comp_scope_path, comp_scope, comp) in &component_scopes {
            if comp_scope.is_empty() {
                continue;
            }
            let type_name = comp.type_name.to_string();

            let class_def = comp
                .type_def_id
                .and_then(|def_id| class_index.get(def_id))
                .or_else(|| resolve_class_in_scope_indexed(class_index, &type_name, comp_scope).0)
                .or_else(|| class_index.get_by_qualified_name(&type_name));
            let Some(class_def) = class_def else {
                continue;
            };

            // Resolve relative names using the component class context, not the
            // instance path (e.g. `sweptVolume`), which has no package nesting.
            let class_context = class_def
                .def_id
                .and_then(|id| tree.def_map.get(&id).cloned())
                .unwrap_or_else(|| type_name.clone());

            // Scan the full inheritance chain so package aliases/constants
            // declared in base classes are visible in component scope.
            // Process base->derived so derived declarations/redeclarations win.
            let mut classes_to_scan =
                collect_ancestor_classes_with_index(tree, class_index, &class_context);
            if classes_to_scan.is_empty() {
                classes_to_scan.push(class_def);
            }

            let static_result =
                inject_cached_component_static_constants(ComponentStaticInjectCtx {
                    tree,
                    class_index,
                    comp,
                    class_def,
                    classes_to_scan: &classes_to_scan,
                    class_context: &class_context,
                    comp_scope,
                    static_cache: &mut static_cache,
                    ctx,
                });
            cache_rejected += static_result.cache_rejected;
            let static_injected = static_result.injected;
            if !static_injected {
                uncached += 1;
                inject_component_static_constants(
                    tree,
                    class_index,
                    comp,
                    &classes_to_scan,
                    &class_context,
                    comp_scope,
                    ctx,
                );
            }

            for scan_class in classes_to_scan.into_iter().rev() {
                let scan_context = scan_class
                    .def_id
                    .and_then(|id| tree.def_map.get(&id).cloned())
                    .unwrap_or_else(|| class_context.clone());
                let before_specialized = component_constant_footprint(ctx);
                inject_alias_constants_from_specialized_child_components(
                    SpecializedChildAliasCtx {
                        tree,
                        class_index,
                        component_index: &component_index,
                        comp_scope_path,
                        comp_scope,
                        scan_class,
                        scan_context: &scan_context,
                        alias_package_injections: &mut alias_package_injections,
                        alias_package_cache: &mut alias_package_cache,
                        ctx,
                    },
                );
                specialized_delta +=
                    component_constant_footprint(ctx).saturating_sub(before_specialized);
            }
        }

        let new = component_constant_footprint(ctx);
        if new == prev {
            break;
        }
        if cache_rejected == 0 && uncached == 0 && specialized_delta == 0 {
            break;
        }
    }
}

type ComponentScopeEntry<'a> = (
    rumoca_core::ComponentPath,
    String,
    &'a rumoca_ir_ast::InstanceData,
);

fn component_scopes_from_overlay(overlay: &InstanceOverlay) -> Vec<ComponentScopeEntry<'_>> {
    overlay
        .components
        .values()
        .map(|comp| {
            let scope_path = comp.qualified_name.to_component_path();
            let scope = scope_path.to_flat_string();
            (scope_path, scope, comp)
        })
        .collect()
}

fn component_index_from_scopes<'a>(
    component_scopes: &'a [ComponentScopeEntry<'a>],
) -> rustc_hash::FxHashMap<rumoca_core::ComponentPath, &'a rumoca_ir_ast::InstanceData> {
    component_scopes
        .iter()
        .map(|(scope_path, _, comp)| (scope_path.clone(), *comp))
        .collect()
}

struct ComponentStaticInjectCtx<'a, 'tree> {
    tree: &'a ClassTree,
    class_index: &'a rumoca_ir_ast::ClassDefIndex<'tree>,
    comp: &'a rumoca_ir_ast::InstanceData,
    class_def: &'a ClassDef,
    classes_to_scan: &'a [&'a ClassDef],
    class_context: &'a str,
    comp_scope: &'a str,
    static_cache: &'a mut rustc_hash::FxHashMap<
        ComponentStaticConstantKey,
        ComponentStaticConstantCacheEntry,
    >,
    ctx: &'a mut Context,
}

struct StaticInjectResult {
    injected: bool,
    cache_rejected: usize,
}

fn inject_cached_component_static_constants(
    mut request: ComponentStaticInjectCtx<'_, '_>,
) -> StaticInjectResult {
    let Some(cache_key) = component_static_cache_key(request.comp, request.class_def) else {
        return StaticInjectResult {
            injected: false,
            cache_rejected: 0,
        };
    };
    if let Some(result) = replay_component_static_cache(&mut request, cache_key) {
        return result;
    }
    let before = ScopedKeySnapshot::capture(request.ctx, request.comp_scope);
    inject_component_static_constants(
        request.tree,
        request.class_index,
        request.comp,
        request.classes_to_scan,
        request.class_context,
        request.comp_scope,
        request.ctx,
    );
    cache_component_static_delta(request, cache_key, &before)
}

fn component_static_cache_key(
    comp: &rumoca_ir_ast::InstanceData,
    class_def: &ClassDef,
) -> Option<ComponentStaticConstantKey> {
    if comp.class_overrides.is_empty()
        && !component_scope_has_array_index(comp)
        && !component_has_instance_specific_static_context(comp)
    {
        class_def
            .def_id
            .map(|class_def_id| ComponentStaticConstantKey { class_def_id })
    } else {
        None
    }
}

fn component_has_instance_specific_static_context(comp: &rumoca_ir_ast::InstanceData) -> bool {
    comp.binding.is_some()
        || comp.binding_source.is_some()
        || comp.binding_source_scope.is_some()
        || comp.binding_from_modification
        || !comp.attribute_source_scopes.is_empty()
        || comp.start.is_some()
        || comp.fixed.is_some()
        || comp.min.is_some()
        || comp.max.is_some()
        || comp.nominal.is_some()
}

fn component_scope_has_array_index(comp: &rumoca_ir_ast::InstanceData) -> bool {
    comp.qualified_name.parts.iter().any(|(name, subs)| {
        !subs.is_empty() || rumoca_core::split_trailing_subscript_suffix(name).is_some()
    })
}

fn replay_component_static_cache(
    request: &mut ComponentStaticInjectCtx<'_, '_>,
    cache_key: ComponentStaticConstantKey,
) -> Option<StaticInjectResult> {
    match request.static_cache.get(&cache_key)? {
        ComponentStaticConstantCacheEntry::Cacheable(delta) => {
            delta.replay(request.comp_scope, &mut *request.ctx);
            Some(StaticInjectResult {
                injected: true,
                cache_rejected: 0,
            })
        }
        ComponentStaticConstantCacheEntry::Uncacheable => Some(StaticInjectResult {
            injected: false,
            cache_rejected: 1,
        }),
    }
}

fn cache_component_static_delta(
    request: ComponentStaticInjectCtx<'_, '_>,
    cache_key: ComponentStaticConstantKey,
    before: &ScopedKeySnapshot,
) -> StaticInjectResult {
    if let Some(delta) = ScopedConstantDelta::capture(request.ctx, request.comp_scope, before) {
        request.static_cache.insert(
            cache_key,
            ComponentStaticConstantCacheEntry::Cacheable(Box::new(delta)),
        );
        StaticInjectResult {
            injected: true,
            cache_rejected: 0,
        }
    } else {
        request
            .static_cache
            .insert(cache_key, ComponentStaticConstantCacheEntry::Uncacheable);
        StaticInjectResult {
            injected: true,
            cache_rejected: 1,
        }
    }
}

fn component_constant_footprint(ctx: &Context) -> usize {
    ctx.parameter_values.len()
        + ctx.array_dimensions.len()
        + ctx.boolean_parameter_values.len()
        + ctx.string_parameter_values.len()
        + ctx.real_parameter_values.len()
        + ctx.enum_parameter_values.len()
        + ctx.constant_values.len()
}

fn inject_component_static_constants(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    comp: &rumoca_ir_ast::InstanceData,
    classes_to_scan: &[&ClassDef],
    class_context: &str,
    comp_scope: &str,
    ctx: &mut Context,
) {
    inject_component_declared_class_overrides(
        tree,
        class_index,
        comp_scope,
        comp,
        class_context,
        ctx,
    );
    inject_component_enclosing_class_constants(tree, class_index, comp_scope, class_context, ctx);

    for scan_class in classes_to_scan.iter().rev().copied() {
        let scan_context = scan_class
            .def_id
            .and_then(|id| tree.def_map.get(&id).cloned())
            .unwrap_or_else(|| class_context.to_string());
        inject_scan_class_static_constants(
            tree,
            class_index,
            comp_scope,
            scan_class,
            &scan_context,
            ctx,
        );
    }
}

fn inject_scan_class_static_constants(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    comp_scope: &str,
    scan_class: &ClassDef,
    scan_context: &str,
    ctx: &mut Context,
) {
    inject_class_extends_constants(tree, class_index, comp_scope, scan_class, scan_context, ctx);

    for (nested_name, nested_class) in &scan_class.classes {
        let nested_scope = format!("{comp_scope}.{nested_name}");
        inject_nested_class_constants(
            tree,
            class_index,
            comp_scope,
            &nested_scope,
            nested_class,
            scan_context,
            ctx,
        );
    }

    for (alias_name, alias_comp) in &scan_class.components {
        inject_alias_component_package_constants(
            tree,
            class_index,
            comp_scope,
            alias_name,
            alias_comp,
            scan_context,
            ctx,
        );
    }
}

pub(crate) fn inject_component_declared_class_overrides(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    comp_scope: &str,
    comp: &rumoca_ir_ast::InstanceData,
    _resolve_context: &str,
    ctx: &mut Context,
) {
    let active_alias = active_component_alias(&comp.type_name);
    let lower_alias = |name: &str| {
        let mut chars = name.chars();
        let Some(first) = chars.next() else {
            return String::new();
        };
        let mut lowered = first.to_lowercase().collect::<String>();
        lowered.push_str(chars.as_str());
        lowered
    };

    for class_override in comp.class_overrides.values() {
        let alias_name = &class_override.alias;
        let def_id = class_override.target_def_id;
        let Some(alias_class) = class_index.get(def_id) else {
            continue;
        };
        if !matches!(alias_class.class_type, rumoca_core::ClassType::Package) {
            continue;
        }

        let alias_resolve_context = tree.def_map.get(&def_id).map(String::as_str).unwrap_or("");

        let alias_scope = format!("{comp_scope}.{alias_name}");
        extract_constants_from_class_with_prefix_and_imports(
            tree,
            class_index,
            &alias_scope,
            alias_class,
            alias_resolve_context,
            ctx,
        );
        let lowered_alias_name = lower_alias(alias_name);
        if lowered_alias_name != *alias_name {
            let lowered_alias_scope = format!("{comp_scope}.{lowered_alias_name}");
            extract_constants_from_class_with_prefix_and_imports(
                tree,
                class_index,
                &lowered_alias_scope,
                alias_class,
                alias_resolve_context,
                ctx,
            );
            for ext in &alias_class.extends {
                apply_extends_constants_for_scope(
                    tree,
                    class_index,
                    &lowered_alias_scope,
                    ext,
                    alias_resolve_context,
                    ctx,
                );
            }
        }
        for ext in &alias_class.extends {
            apply_extends_constants_for_scope(
                tree,
                class_index,
                &alias_scope,
                ext,
                alias_resolve_context,
                ctx,
            );
        }

        // Only expose unqualified component-scope constants (`comp_scope.nX`) for
        // the alias actually used by this component's declared type. This avoids
        // collisions from unrelated package aliases that happen to define the same
        // constant names (e.g., fixedX, nX) in different media packages.
        if active_alias != Some(alias_name.as_str()) {
            continue;
        }

        extract_constants_from_class_with_prefix_and_imports(
            tree,
            class_index,
            comp_scope,
            alias_class,
            alias_resolve_context,
            ctx,
        );
        for ext in &alias_class.extends {
            apply_extends_constants_for_scope(
                tree,
                class_index,
                comp_scope,
                ext,
                alias_resolve_context,
                ctx,
            );
        }
    }
}

pub(crate) fn active_component_alias(type_name: &str) -> Option<&str> {
    crate::path_utils::first_path_segment_without_index(type_name).filter(|name| !name.is_empty())
}

fn class_override_by_alias<'a>(
    overrides: &'a rumoca_ir_ast::ClassOverrideMap,
    alias: &str,
) -> Option<&'a rumoca_ir_ast::ClassOverride> {
    overrides
        .values()
        .find(|class_override| class_override.alias == alias)
}

pub(crate) fn inject_component_enclosing_class_constants(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    comp_scope: &str,
    class_context: &str,
    ctx: &mut Context,
) {
    let Some(enclosing_name) = crate::path_utils::enclosing_scope(class_context) else {
        return;
    };
    let ancestors = collect_ancestor_classes_with_index(tree, class_index, enclosing_name);
    if ancestors.is_empty() {
        return;
    }
    let shadowed_names = class_index
        .get_by_qualified_name(class_context)
        .or_else(|| {
            class_index.get_by_qualified_name(crate::path_utils::leaf_segment(class_context))
        })
        .map(|class| class_member_names_with_bases(tree, class))
        .unwrap_or_default();

    const MAX_PASSES: usize = 5;
    for _pass in 0..MAX_PASSES {
        let prev = ctx.parameter_values.len()
            + ctx.array_dimensions.len()
            + ctx.boolean_parameter_values.len()
            + ctx.string_parameter_values.len()
            + ctx.real_parameter_values.len()
            + ctx.enum_parameter_values.len()
            + ctx.constant_values.len();

        for ancestor in &ancestors {
            let resolve_context = ancestor
                .def_id
                .and_then(|id| tree.def_map.get(&id).cloned())
                .unwrap_or_else(|| enclosing_name.to_string());
            for ext in &ancestor.extends {
                apply_extends_constants_for_scope(
                    tree,
                    class_index,
                    comp_scope,
                    ext,
                    &resolve_context,
                    ctx,
                );
            }
            extract_constants_from_class_with_prefix_and_imports_shadowed(
                tree,
                class_index,
                comp_scope,
                ancestor,
                &resolve_context,
                ctx,
                &shadowed_names,
            );
        }

        let new = ctx.parameter_values.len()
            + ctx.array_dimensions.len()
            + ctx.boolean_parameter_values.len()
            + ctx.string_parameter_values.len()
            + ctx.real_parameter_values.len()
            + ctx.enum_parameter_values.len()
            + ctx.constant_values.len();
        if new == prev {
            break;
        }
    }
}

fn class_member_names_with_bases(
    tree: &rumoca_ir_ast::ClassTree,
    class: &rumoca_ir_ast::ClassDef,
) -> rustc_hash::FxHashSet<String> {
    let mut names = rustc_hash::FxHashSet::default();
    let mut visited = rustc_hash::FxHashSet::default();
    collect_class_member_names_recursive(tree, class, &mut names, &mut visited);
    names
}

fn collect_class_member_names_recursive(
    tree: &rumoca_ir_ast::ClassTree,
    class: &rumoca_ir_ast::ClassDef,
    names: &mut rustc_hash::FxHashSet<String>,
    visited: &mut rustc_hash::FxHashSet<rumoca_core::DefId>,
) {
    if let Some(def_id) = class.def_id
        && !visited.insert(def_id)
    {
        return;
    }
    names.extend(class.components.keys().cloned());
    for ext in &class.extends {
        let base_class = ext
            .base_def_id
            .and_then(|def_id| tree.get_class_by_def_id(def_id))
            .or_else(|| tree.get_class_by_qualified_name(&ext.base_name.to_string()));
        if let Some(base_class) = base_class {
            collect_class_member_names_recursive(tree, base_class, names, visited);
        }
    }
}

/// Inject alias package constants by matching declared child component types
/// against their instantiated specialized types in the overlay.
///
/// MLS §7.3: component-level redeclare package modifiers specialize nested
/// package aliases for that component instance. Flatten branch selection must
/// observe these effective alias constants (e.g., `Medium.ThermoStates`).
pub(crate) struct SpecializedChildAliasCtx<'a, 'tree> {
    tree: &'a ClassTree,
    class_index: &'a rumoca_ir_ast::ClassDefIndex<'tree>,
    component_index:
        &'a rustc_hash::FxHashMap<rumoca_core::ComponentPath, &'a rumoca_ir_ast::InstanceData>,
    comp_scope_path: &'a rumoca_core::ComponentPath,
    comp_scope: &'a str,
    scan_class: &'a ClassDef,
    scan_context: &'a str,
    alias_package_injections: &'a mut rustc_hash::FxHashSet<AliasPackageInjectionKey>,
    alias_package_cache:
        &'a mut rustc_hash::FxHashMap<AliasPackageStaticKey, ComponentStaticConstantCacheEntry>,
    ctx: &'a mut Context,
}

pub(crate) fn inject_alias_constants_from_specialized_child_components(
    request: SpecializedChildAliasCtx<'_, '_>,
) {
    let parent_class_overrides = request
        .component_index
        .get(request.comp_scope_path)
        .map(|inst| &inst.class_overrides);

    for (child_name, child_comp) in &request.scan_class.components {
        let declared_type = child_comp.type_name.to_string();
        let Some((alias_name, declared_tail)) = split_alias_declared_type(&declared_type) else {
            continue;
        };

        let child_scope_path =
            request
                .comp_scope_path
                .join(&rumoca_core::ComponentPath::from_parts(
                    [child_name.clone()],
                ));
        let child_scope = child_scope_path.to_flat_string();
        let Some(child_inst) = request.component_index.get(&child_scope_path) else {
            continue;
        };

        // Prefer explicit parent instance class-overrides for alias package resolution.
        // MLS §7.3: component-level redeclare bindings define effective alias packages
        // even when child type_def_id remains the inherited base member class.
        if let Some(class_override) = parent_class_overrides
            .and_then(|overrides| class_override_by_alias(overrides, alias_name))
        {
            let package_def_id = class_override.target_def_id;
            if let Some(package_class) = request.class_index.get(package_def_id)
                && matches!(package_class.class_type, rumoca_core::ClassType::Package)
            {
                let alias_scope = format!("{}.{alias_name}", request.comp_scope);
                let package_context = request
                    .tree
                    .def_map
                    .get(&package_def_id)
                    .cloned()
                    .unwrap_or_else(|| request.scan_context.to_string());
                inject_alias_package_constants(
                    AliasPackageConstantCtx {
                        tree: request.tree,
                        class_index: request.class_index,
                        comp_scope: request.comp_scope,
                        child_scope: &child_scope,
                        alias_scope: &alias_scope,
                        package_context: &package_context,
                        alias_package_injections: &mut *request.alias_package_injections,
                        alias_package_cache: &mut *request.alias_package_cache,
                        ctx: &mut *request.ctx,
                    },
                    package_class,
                );
                continue;
            }
        }

        let actual_type_name = child_inst
            .type_def_id
            .and_then(|def_id| request.tree.def_map.get(&def_id).cloned())
            .unwrap_or_else(|| child_inst.type_name.clone());

        let Some(package_name) = strip_declared_suffix(&actual_type_name, declared_tail) else {
            continue;
        };

        let package_class = resolve_class_in_scope_indexed(
            request.class_index,
            &package_name,
            request.scan_context,
        )
        .0
        .or_else(|| request.class_index.get_by_qualified_name(&package_name));
        let Some(package_class) = package_class else {
            continue;
        };
        if !matches!(package_class.class_type, rumoca_core::ClassType::Package) {
            continue;
        }
        let package_context = package_class
            .def_id
            .and_then(|id| request.tree.def_map.get(&id).cloned())
            .unwrap_or_else(|| package_name.clone());

        let alias_scope = format!("{}.{alias_name}", request.comp_scope);
        inject_alias_package_constants(
            AliasPackageConstantCtx {
                tree: request.tree,
                class_index: request.class_index,
                comp_scope: request.comp_scope,
                child_scope: &child_scope,
                alias_scope: &alias_scope,
                package_context: &package_context,
                alias_package_injections: &mut *request.alias_package_injections,
                alias_package_cache: &mut *request.alias_package_cache,
                ctx: &mut *request.ctx,
            },
            package_class,
        );
    }
}

struct AliasPackageConstantCtx<'a, 'tree> {
    tree: &'a ClassTree,
    class_index: &'a rumoca_ir_ast::ClassDefIndex<'tree>,
    comp_scope: &'a str,
    child_scope: &'a str,
    alias_scope: &'a str,
    package_context: &'a str,
    alias_package_injections: &'a mut rustc_hash::FxHashSet<AliasPackageInjectionKey>,
    alias_package_cache:
        &'a mut rustc_hash::FxHashMap<AliasPackageStaticKey, ComponentStaticConstantCacheEntry>,
    ctx: &'a mut Context,
}

fn inject_alias_package_constants(
    mut request: AliasPackageConstantCtx<'_, '_>,
    package_class: &ClassDef,
) {
    for scope in [request.alias_scope, request.comp_scope, request.child_scope] {
        if !request
            .alias_package_injections
            .insert(AliasPackageInjectionKey {
                scope: scope.to_string(),
                package_def_id: package_class.def_id,
                package_context: request.package_context.to_string(),
            })
        {
            continue;
        }
        inject_alias_package_constants_for_scope(&mut request, package_class, scope);
    }
}

fn inject_alias_package_constants_for_scope(
    request: &mut AliasPackageConstantCtx<'_, '_>,
    package_class: &ClassDef,
    scope: &str,
) {
    let Some(package_def_id) = package_class.def_id else {
        return inject_uncached_alias_package_constants(request, package_class, scope);
    };
    let cache_key = AliasPackageStaticKey {
        package_def_id,
        package_context: request.package_context.to_string(),
    };
    match request.alias_package_cache.get(&cache_key) {
        Some(ComponentStaticConstantCacheEntry::Cacheable(delta)) => {
            delta.replay(scope, &mut *request.ctx);
            return;
        }
        Some(ComponentStaticConstantCacheEntry::Uncacheable) => {
            return inject_uncached_alias_package_constants(request, package_class, scope);
        }
        None => {}
    }

    let before = ScopedKeySnapshot::capture(request.ctx, scope);
    inject_uncached_alias_package_constants(request, package_class, scope);
    if let Some(delta) = ScopedConstantDelta::capture(request.ctx, scope, &before) {
        request.alias_package_cache.insert(
            cache_key,
            ComponentStaticConstantCacheEntry::Cacheable(Box::new(delta)),
        );
    } else {
        request
            .alias_package_cache
            .insert(cache_key, ComponentStaticConstantCacheEntry::Uncacheable);
    }
}

fn inject_uncached_alias_package_constants(
    request: &mut AliasPackageConstantCtx<'_, '_>,
    package_class: &ClassDef,
    scope: &str,
) {
    extract_constants_from_class_with_prefix_and_imports(
        request.tree,
        request.class_index,
        scope,
        package_class,
        request.package_context,
        &mut *request.ctx,
    );
    for ext in &package_class.extends {
        apply_extends_constants_for_scope(
            request.tree,
            request.class_index,
            scope,
            ext,
            request.package_context,
            &mut *request.ctx,
        );
    }
}

pub(crate) fn split_alias_declared_type(type_name: &str) -> Option<(&str, &str)> {
    let (alias_name, declared_tail) = root_split(type_name)?;
    if alias_name.starts_with(char::is_uppercase) && !declared_tail.is_empty() {
        Some((alias_name, declared_tail))
    } else {
        None
    }
}

pub(crate) fn strip_declared_suffix(actual_type: &str, declared_tail: &str) -> Option<String> {
    let actual_parts = path_segments_of(actual_type);
    let declared_parts = path_segments_of(declared_tail);
    if declared_parts.is_empty()
        || actual_parts.len() <= declared_parts.len()
        || actual_parts[actual_parts.len() - declared_parts.len()..] != declared_parts[..]
    {
        return None;
    }

    let package_parts = &actual_parts[..actual_parts.len() - declared_parts.len()];
    if package_parts.is_empty() {
        None
    } else {
        Some(package_parts.join("."))
    }
}

#[cfg(test)]
mod active_component_alias_tests {
    use super::active_component_alias;

    #[test]
    fn resolves_first_segment_for_dotted_type_name() {
        assert_eq!(
            active_component_alias("Medium.BaseProperties"),
            Some("Medium")
        );
    }

    #[test]
    fn ignores_dot_inside_subscript_expression() {
        assert_eq!(
            active_component_alias("Medium[data.medium].BaseProperties"),
            Some("Medium")
        );
    }

    #[test]
    fn returns_none_for_empty_name() {
        assert_eq!(active_component_alias(""), None);
    }
}

#[cfg(test)]
mod strip_declared_suffix_tests {
    use super::strip_declared_suffix;

    #[test]
    fn returns_package_prefix_for_whole_segment_tail_match() {
        assert_eq!(
            strip_declared_suffix("Modelica.Media.Air.BaseProperties", "BaseProperties"),
            Some("Modelica.Media.Air".to_string())
        );
        assert_eq!(
            strip_declared_suffix("Modelica.Media.Air.BaseProperties", "Air.BaseProperties"),
            Some("Modelica.Media".to_string())
        );
    }

    #[test]
    fn rejects_byte_suffix_match_without_segment_boundary() {
        assert_eq!(
            strip_declared_suffix("Pkg.MediumBaseProperties", "BaseProperties"),
            None
        );
        assert_eq!(
            strip_declared_suffix("Pkg.BaseProperties", "Air.BaseProperties"),
            None
        );
    }

    #[test]
    fn preserves_dots_inside_subscript_segments() {
        assert_eq!(
            strip_declared_suffix("Pkg[data.medium].Air.BaseProperties", "Air.BaseProperties"),
            Some("Pkg[data.medium]".to_string())
        );
    }
}
