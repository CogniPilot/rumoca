use super::*;
use crate::source_spans::required_location_span;

pub(crate) struct FlattenGraphData {
    pub(crate) vcg_data: vcg::VcgPreScanData,
    pub(crate) optional_edges: Vec<(String, String)>,
}

pub(crate) struct OverlayScopeIndex<'a> {
    classes: rustc_hash::FxHashMap<ast::QualifiedName, &'a ast::ClassInstanceData>,
    components: rustc_hash::FxHashMap<ast::QualifiedName, &'a ast::InstanceData>,
}

#[derive(Default)]
pub(crate) struct ImportCaches<'tree> {
    instance: rustc_hash::FxHashMap<ast::QualifiedName, qualify::ImportMap>,
    source: rustc_hash::FxHashMap<ast::QualifiedName, qualify::ImportMap>,
    lexical_packages: rustc_hash::FxHashMap<String, qualify::ImportMap>,
    member_def_ids: qualify::MemberDefIdCache<'tree>,
}

impl<'a> OverlayScopeIndex<'a> {
    pub(crate) fn new(overlay: &'a ast::InstanceOverlay) -> Self {
        let classes = overlay
            .classes
            .values()
            .map(|class_data| (class_data.qualified_name.clone(), class_data))
            .collect();
        let components = overlay
            .components
            .values()
            .map(|component| (component.qualified_name.clone(), component))
            .collect();
        Self {
            classes,
            components,
        }
    }
}

pub(crate) fn initialize_flat_metadata(flat: &mut flat::Model, overlay: &ast::InstanceOverlay) {
    // MLS §4.7: Propagate partial status and class type from overlay
    flat.is_partial = overlay.is_partial;
    flat.class_type = overlay.class_type.clone();
    flat.model_description = overlay.root_description.clone();
}

pub(crate) fn variable_import_context_for_instance<'tree>(
    instance: &ast::InstanceData,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    import_cache: &mut ImportCaches<'tree>,
    scope_index: &OverlayScopeIndex<'_>,
    component_override_map: &ComponentOverrideMap,
) -> Result<variables::VariableImportContext, FlattenError> {
    let instance_scope = instance.qualified_name.to_component_path();
    let declaration_override_imports = override_import_data_for_instance(
        instance,
        scope_index,
        &instance_scope,
        component_override_map,
    );
    let declaration_scope = instance
        .declaration_source_scope
        .as_ref()
        .ok_or_else(|| missing_source_scope_error(instance, tree, "declaration"))?;
    let mut declaration = cached_import_map_for_instance_scope(
        declaration_scope,
        tree,
        class_index,
        import_cache,
        scope_index,
    );
    add_package_override_aliases(
        class_index,
        &declaration_override_imports.aliases,
        &mut declaration,
    );
    collect_lexical_constant_aliases_for_scope(
        tree,
        class_index,
        declaration_scope,
        &declaration_override_imports.package_names,
        &mut declaration,
        &mut import_cache.member_def_ids,
    );
    augment_imports_for_instance_exprs(
        declaration_scope,
        tree,
        class_index,
        instance,
        &mut declaration,
        scope_index,
    );
    let binding_expr = instance
        .binding_source
        .as_ref()
        .or(instance.binding.as_ref());
    let mut binding = binding_import_context_for_instance(ImportContextBuild {
        instance,
        tree,
        class_index,
        import_cache,
        scope_index,
        component_override_map,
        declaration: &declaration,
        binding_expr,
    })?;
    if let Some(binding_scope) = instance.binding_source_scope.as_ref()
        && let Some(binding_expr) = binding_expr
    {
        augment_imports_for_expr(
            binding_scope,
            tree,
            class_index,
            binding_expr,
            &mut binding,
            scope_index,
        );
    }
    let attributes = attribute_import_contexts_for_instance(ImportContextBuild {
        instance,
        tree,
        class_index,
        import_cache,
        scope_index,
        component_override_map,
        declaration: &declaration,
        binding_expr,
    });
    let attribute_function_scopes = instance
        .attribute_source_scopes
        .iter()
        .filter_map(|(attr_name, scope)| {
            semantic_function_scope_for_instance_scope(scope, class_index, scope_index)
                .map(|function_scope| (attr_name.clone(), function_scope))
        })
        .collect();

    Ok(variables::VariableImportContext {
        declaration,
        binding,
        attributes,
        declaration_function_scope: semantic_function_scope_for_instance_scope(
            declaration_scope,
            class_index,
            scope_index,
        ),
        binding_function_scope: instance.binding_source_scope.as_ref().and_then(|scope| {
            semantic_function_scope_for_instance_scope(scope, class_index, scope_index)
        }),
        attribute_function_scopes,
    })
}

struct ImportContextBuild<'a, 'tree> {
    instance: &'a ast::InstanceData,
    tree: &'a ast::ClassTree,
    class_index: &'a ast::ClassDefIndex<'tree>,
    import_cache: &'a mut ImportCaches<'tree>,
    scope_index: &'a OverlayScopeIndex<'a>,
    component_override_map: &'a ComponentOverrideMap,
    declaration: &'a qualify::ImportMap,
    binding_expr: Option<&'a ast::Expression>,
}

fn binding_import_context_for_instance(
    request: ImportContextBuild<'_, '_>,
) -> Result<qualify::ImportMap, FlattenError> {
    if !(request.instance.binding_from_modification && request.binding_expr.is_some()) {
        return Ok(request.declaration.clone());
    }
    let binding_scope = request
        .instance
        .binding_source_scope
        .as_ref()
        .ok_or_else(|| {
            missing_source_scope_error(request.instance, request.tree, "modifier binding")
        })?;
    let mut imports = cached_import_map_for_instance_scope(
        binding_scope,
        request.tree,
        request.class_index,
        request.import_cache,
        request.scope_index,
    );
    let binding_override_imports =
        override_import_data_for_qualified_scope(binding_scope, request.component_override_map);
    add_package_override_aliases(
        request.class_index,
        &binding_override_imports.aliases,
        &mut imports,
    );
    collect_lexical_constant_aliases_for_scope(
        request.tree,
        request.class_index,
        binding_scope,
        &binding_override_imports.package_names,
        &mut imports,
        &mut request.import_cache.member_def_ids,
    );
    Ok(imports)
}

fn attribute_import_contexts_for_instance(
    request: ImportContextBuild<'_, '_>,
) -> rustc_hash::FxHashMap<String, qualify::ImportMap> {
    request
        .instance
        .attribute_source_scopes
        .iter()
        .map(|(attr_name, scope)| {
            let mut imports = cached_import_map_for_instance_scope(
                scope,
                request.tree,
                request.class_index,
                request.import_cache,
                request.scope_index,
            );
            let override_imports =
                override_import_data_for_qualified_scope(scope, request.component_override_map);
            add_package_override_aliases(
                request.class_index,
                &override_imports.aliases,
                &mut imports,
            );
            collect_lexical_constant_aliases_for_scope(
                request.tree,
                request.class_index,
                scope,
                &override_imports.package_names,
                &mut imports,
                &mut request.import_cache.member_def_ids,
            );
            if let Some(expr) = instance_attribute_expr(request.instance, attr_name) {
                augment_imports_for_expr(
                    scope,
                    request.tree,
                    request.class_index,
                    expr,
                    &mut imports,
                    request.scope_index,
                );
            }
            (attr_name.clone(), imports)
        })
        .collect()
}

fn semantic_function_scope_for_instance_scope(
    scope: &ast::QualifiedName,
    class_index: &ast::ClassDefIndex<'_>,
    scope_index: &OverlayScopeIndex<'_>,
) -> Option<String> {
    let scope_name = scope.to_flat_string();
    if class_index.get_by_qualified_name(&scope_name).is_some() {
        return Some(scope_name);
    }
    if let Some(class_data) = scope_index.classes.get(scope)
        && let Some(source_scope) = class_data.source_scope.as_ref()
    {
        return Some(source_scope.to_flat_string());
    }
    let component = scope_index.components.get(scope)?;
    component
        .type_def_id
        .and_then(|def_id| class_index.qualified_name(def_id).map(str::to_string))
        .or_else(|| {
            component
                .declaration_source_scope
                .as_ref()
                .map(|s| s.to_flat_string())
        })
}

struct OverrideImportData {
    package_names: Vec<String>,
    aliases: Vec<(String, String)>,
}

fn override_import_data_for_instance(
    instance: &ast::InstanceData,
    scope_index: &OverlayScopeIndex<'_>,
    scope: &rumoca_core::ComponentPath,
    component_override_map: &ComponentOverrideMap,
) -> OverrideImportData {
    let preferred_aliases = preferred_package_aliases_for_instance(instance, scope_index);
    override_import_data_for_component_path_with_preferred_aliases(
        scope,
        component_override_map,
        &preferred_aliases,
    )
}

fn override_import_data_for_qualified_scope(
    scope: &ast::QualifiedName,
    component_override_map: &ComponentOverrideMap,
) -> OverrideImportData {
    override_import_data_for_component_path(&scope.to_component_path(), component_override_map)
}

fn override_import_data_for_component_path(
    scope: &rumoca_core::ComponentPath,
    component_override_map: &ComponentOverrideMap,
) -> OverrideImportData {
    override_import_data_for_component_path_with_preferred_aliases(
        scope,
        component_override_map,
        &[],
    )
}

fn override_import_data_for_component_path_with_preferred_aliases(
    scope: &rumoca_core::ComponentPath,
    component_override_map: &ComponentOverrideMap,
    preferred_aliases: &[String],
) -> OverrideImportData {
    let (packages, _) = override_context_for_component_path(scope, component_override_map);
    OverrideImportData {
        package_names: override_package_names_with_preferred_aliases(&packages, preferred_aliases),
        aliases: override_aliases_for_component_path(scope, component_override_map),
    }
}

fn preferred_package_aliases_for_instance(
    instance: &ast::InstanceData,
    scope_index: &OverlayScopeIndex<'_>,
) -> Vec<String> {
    let mut aliases = Vec::new();
    push_type_root_alias(&mut aliases, &instance.type_name);

    let mut path = instance.qualified_name.to_component_path();
    while let Some(parent) = path.parent() {
        let parent_qn = qualified_name_from_component_path(&parent);
        if let Some(component) = scope_index.components.get(&parent_qn) {
            for class_override in component.class_overrides.values() {
                push_unique_alias(&mut aliases, &class_override.alias);
            }
            push_type_root_alias(&mut aliases, &component.type_name);
        }
        path = parent;
    }
    aliases
}

fn qualified_name_from_component_path(path: &rumoca_core::ComponentPath) -> ast::QualifiedName {
    ast::QualifiedName {
        parts: path
            .parts()
            .iter()
            .map(|part| (part.clone(), Vec::new()))
            .collect(),
    }
}

fn push_type_root_alias(aliases: &mut Vec<String>, type_name: &str) {
    push_unique_alias(aliases, first_flat_type_segment(type_name));
}

fn first_flat_type_segment(type_name: &str) -> &str {
    let mut bracket_depth = 0_u32;
    for (idx, byte) in type_name.bytes().enumerate() {
        match byte {
            b'[' => bracket_depth += 1,
            b']' => bracket_depth = bracket_depth.saturating_sub(1),
            b'.' if bracket_depth == 0 => return &type_name[..idx],
            _ => {}
        }
    }
    type_name
}

fn push_unique_alias(aliases: &mut Vec<String>, alias: &str) {
    if !alias.is_empty() && !aliases.iter().any(|existing| existing == alias) {
        aliases.push(alias.to_string());
    }
}

pub(crate) fn add_package_override_aliases(
    class_index: &ast::ClassDefIndex<'_>,
    override_aliases: &[(String, String)],
    imports: &mut qualify::ImportMap,
) {
    for (alias, target_name) in override_aliases {
        let Some(target_class) = class_index.get_by_qualified_name(target_name) else {
            continue;
        };
        if target_class.class_type == rumoca_core::ClassType::Package {
            imports.insert(alias.clone(), target_name.clone());
        }
    }
}

fn missing_source_scope_error(
    instance: &ast::InstanceData,
    tree: &ast::ClassTree,
    context: &str,
) -> FlattenError {
    match required_location_span(&tree.source_map, &instance.source_location, context) {
        Ok(span) => FlattenError::missing_source_scope(
            instance.qualified_name.to_flat_string(),
            context,
            span,
        ),
        Err(error) => error,
    }
}

fn augment_imports_for_instance_exprs(
    scope: &ast::QualifiedName,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    instance: &ast::InstanceData,
    imports: &mut qualify::ImportMap,
    scope_index: &OverlayScopeIndex<'_>,
) {
    for expr in [
        instance.binding_source.as_ref(),
        instance.binding.as_ref(),
        instance.start.as_ref(),
        instance.min.as_ref(),
        instance.max.as_ref(),
        instance.nominal.as_ref(),
    ]
    .into_iter()
    .flatten()
    {
        augment_imports_for_expr(scope, tree, class_index, expr, imports, scope_index);
    }
}

fn instance_attribute_expr<'a>(
    instance: &'a ast::InstanceData,
    attr_name: &str,
) -> Option<&'a ast::Expression> {
    match attr_name {
        "start" => instance.start.as_ref(),
        "min" => instance.min.as_ref(),
        "max" => instance.max.as_ref(),
        "nominal" => instance.nominal.as_ref(),
        _ => None,
    }
}

fn augment_imports_for_expr(
    scope: &ast::QualifiedName,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    expr: &ast::Expression,
    imports: &mut qualify::ImportMap,
    scope_index: &OverlayScopeIndex<'_>,
) {
    let Some(class_name) = class_scope_name_for_scope(scope, tree, class_index, scope_index) else {
        return;
    };
    let Some(class_def) = class_index.get_by_qualified_name(&class_name) else {
        return;
    };
    let Some(scope_id) = class_def.scope_id else {
        return;
    };
    collect_expr_first_segment_aliases(tree, class_index, scope_id, &class_name, expr, imports);
}

fn class_scope_name_for_scope(
    scope: &ast::QualifiedName,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    scope_index: &OverlayScopeIndex<'_>,
) -> Option<String> {
    let scope_name = scope.to_flat_string();
    if class_index.get_by_qualified_name(&scope_name).is_some() {
        return Some(scope_name);
    }
    if let Some(class_name) = scope_index
        .classes
        .get(scope)
        .and_then(|class_data| class_data.source_scope.as_ref())
        .map(ast::QualifiedName::to_flat_string)
    {
        return Some(class_name);
    }
    scope_index.components.get(scope).and_then(|component| {
        component
            .type_def_id
            .and_then(|def_id| tree.def_map.get(&def_id).cloned())
            .or_else(|| (!component.type_name.is_empty()).then(|| component.type_name.clone()))
    })
}

fn collect_expr_first_segment_aliases(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    scope_id: rumoca_core::ScopeId,
    class_name: &str,
    expr: &ast::Expression,
    imports: &mut qualify::ImportMap,
) {
    let mut collector = FirstSegmentAliasCollector {
        tree,
        class_index,
        scope_id,
        class_name,
        imports,
    };
    let _ = rumoca_ir_ast::Visitor::visit_expression(&mut collector, expr);
}

struct FirstSegmentAliasCollector<'a, 'tree> {
    tree: &'tree ast::ClassTree,
    class_index: &'a ast::ClassDefIndex<'tree>,
    scope_id: rumoca_core::ScopeId,
    class_name: &'a str,
    imports: &'a mut qualify::ImportMap,
}

impl rumoca_ir_ast::Visitor for FirstSegmentAliasCollector<'_, '_> {
    fn visit_component_reference_ctx(
        &mut self,
        cr: &ast::ComponentReference,
        _ctx: rumoca_ir_ast::ComponentReferenceContext,
    ) -> std::ops::ControlFlow<()> {
        collect_component_ref_first_segment_alias(
            self.tree,
            self.class_index,
            self.scope_id,
            self.class_name,
            cr,
            self.imports,
        );
        self.visit_component_reference(cr)
    }
}

fn collect_component_ref_first_segment_alias(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    scope_id: rumoca_core::ScopeId,
    class_name: &str,
    cr: &ast::ComponentReference,
    imports: &mut qualify::ImportMap,
) {
    let Some(first_part) = cr.parts.first() else {
        return;
    };
    let alias = first_part.ident.text.as_ref();
    if imports.contains_key(alias) {
        return;
    }
    if class_declares_member(tree, class_index, class_name, alias) {
        return;
    }
    let alias_path = rumoca_core::ComponentPath::from_flat_path(alias);
    let Some(def_id) = tree.scope_tree.lookup(scope_id, &alias_path) else {
        return;
    };
    let Some(class_def) = class_index.get(def_id) else {
        return;
    };
    if !matches!(class_def.class_type, rumoca_core::ClassType::Package) {
        return;
    }
    let Some(path) = tree.def_map.get(&def_id) else {
        return;
    };
    if let Some(next_part) = cr.parts.get(1)
        && !class_has_referenced_member(tree, class_index, path, next_part.ident.text.as_ref())
    {
        return;
    }
    imports.insert(alias.to_string(), path.clone());
}

fn class_declares_member(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    class_name: &str,
    alias: &str,
) -> bool {
    collect_ancestor_classes_with_index(tree, class_index, class_name)
        .into_iter()
        .any(|class_def| {
            class_def.components.contains_key(alias) || class_def.classes.contains_key(alias)
        })
}

fn class_has_referenced_member(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    class_name: &str,
    member: &str,
) -> bool {
    collect_ancestor_classes_with_index(tree, class_index, class_name)
        .into_iter()
        .any(|class_def| {
            class_def.components.contains_key(member) || class_def.classes.contains_key(member)
        })
}

fn cached_import_map_for_instance_scope<'tree>(
    scope: &ast::QualifiedName,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    cache: &mut ImportCaches<'tree>,
    scope_index: &OverlayScopeIndex<'_>,
) -> qualify::ImportMap {
    if let Some(imports) = cache.instance.get(scope) {
        return imports.clone();
    }
    let imports = import_map_for_instance_scope(scope, tree, class_index, cache, scope_index);
    cache.instance.insert(scope.clone(), imports.clone());
    imports
}

fn import_map_for_instance_scope<'tree>(
    scope: &ast::QualifiedName,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    cache: &mut ImportCaches<'tree>,
    scope_index: &OverlayScopeIndex<'_>,
) -> qualify::ImportMap {
    let scope_name = scope.to_flat_string();
    let mut imports = scope_index
        .classes
        .get(scope)
        .map(|class_data| {
            let mut imports: qualify::ImportMap =
                class_data.resolved_imports.iter().cloned().collect();
            if let Some(source_scope) = class_data.source_scope.as_ref() {
                imports.extend(cached_source_import_map(
                    source_scope,
                    tree,
                    class_index,
                    &mut cache.source,
                    &mut cache.member_def_ids,
                ));
            }
            imports
        })
        .unwrap_or_default();
    if class_index.get_by_qualified_name(&scope_name).is_some() {
        qualify::collect_imports_for_source_scope(class_index, scope, &mut imports);
        collect_lexical_constant_aliases_for_scope(
            tree,
            class_index,
            scope,
            &[],
            &mut imports,
            &mut cache.member_def_ids,
        );
    }
    if !scope_name.is_empty() {
        extend_imports_if_absent(
            &mut imports,
            cached_lexical_package_aliases(
                &scope_name,
                tree,
                class_index,
                &mut cache.lexical_packages,
                &mut cache.member_def_ids,
            ),
        );
    }
    imports
}

fn extend_imports_if_absent(imports: &mut qualify::ImportMap, aliases: qualify::ImportMap) {
    for (name, target) in aliases {
        imports.entry(name).or_insert(target);
    }
}

fn cached_lexical_package_aliases<'tree>(
    class_name: &str,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    cache: &mut rustc_hash::FxHashMap<String, qualify::ImportMap>,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
) -> qualify::ImportMap {
    if let Some(imports) = cache.get(class_name) {
        return imports.clone();
    }

    let mut imports = qualify::ImportMap::default();
    if let Some(source_def_id) = class_index.def_id_by_qualified_name(class_name) {
        qualify::collect_lexical_package_aliases_for_def_id_with_member_cache(
            tree,
            class_index,
            source_def_id,
            &mut imports,
            Some(member_cache),
        );
    }
    cache.insert(class_name.to_string(), imports.clone());
    imports
}

fn cached_source_import_map<'tree>(
    source_scope: &ast::QualifiedName,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    cache: &mut rustc_hash::FxHashMap<ast::QualifiedName, qualify::ImportMap>,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
) -> qualify::ImportMap {
    if let Some(imports) = cache.get(source_scope) {
        return imports.clone();
    }
    let mut imports = qualify::ImportMap::default();
    qualify::collect_imports_for_source_scope(class_index, source_scope, &mut imports);
    collect_lexical_constant_aliases_for_scope(
        tree,
        class_index,
        source_scope,
        &[],
        &mut imports,
        member_cache,
    );
    if let Some(source_def_id) = source_scope_def_id(class_index, source_scope) {
        qualify::collect_lexical_package_aliases_for_def_id_with_member_cache(
            tree,
            class_index,
            source_def_id,
            &mut imports,
            Some(member_cache),
        );
    }
    cache.insert(source_scope.clone(), imports.clone());
    imports
}

fn collect_lexical_constant_aliases_for_scope<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    source_scope: &ast::QualifiedName,
    active_packages: &[String],
    imports: &mut qualify::ImportMap,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
) {
    if let Some(source_def_id) = source_scope_def_id(class_index, source_scope) {
        qualify::collect_lexical_constant_aliases_for_def_id_with_packages_and_member_cache(
            tree,
            class_index,
            source_def_id,
            active_packages,
            imports,
            Some(member_cache),
        );
    }
}

fn source_scope_def_id(
    class_index: &ast::ClassDefIndex<'_>,
    source_scope: &ast::QualifiedName,
) -> Option<rumoca_core::DefId> {
    class_index.def_id_by_qualified_name(&source_scope.to_flat_string())
}

pub(crate) fn process_component_instances_for_flatten(
    flat: &mut flat::Model,
    overlay: &ast::InstanceOverlay,
    component_override_map: &ComponentOverrideMap,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    component_members: &component_member_scope::ComponentMemberScopes,
    simulated_root_name: Option<&str>,
) -> Result<(), FlattenError> {
    let mut import_cache = ImportCaches::default();
    let scope_index = OverlayScopeIndex::new(overlay);
    for instance_data in overlay.components.values() {
        if is_in_disabled_component(&instance_data.qualified_name, &overlay.disabled_components) {
            continue;
        }
        process_component_instance(ComponentInstanceProcess {
            flat,
            instance_data,
            simulated_root_name,
            component_override_map,
            tree,
            class_index,
            import_cache: &mut import_cache,
            scope_index: &scope_index,
            component_members,
        })?;
        track_top_level_component_markers(flat, instance_data);
    }
    Ok(())
}

fn track_top_level_component_markers(flat: &mut flat::Model, instance_data: &ast::InstanceData) {
    if instance_data.qualified_name.parts.len() != 1 {
        return;
    }
    let name = &instance_data.qualified_name.parts[0].0;
    if instance_data.is_connector_type {
        flat.top_level_connectors.insert(name.clone());
    }
    if matches!(&instance_data.causality, rumoca_core::Causality::Input(_)) {
        flat.top_level_input_components.insert(name.clone());
    }
}

pub(crate) fn prepare_context_for_equation_flattening(
    ctx: &mut Context,
    flat: &mut flat::Model,
    overlay: &ast::InstanceOverlay,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    model_name: &str,
    component_override_map: &ComponentOverrideMap,
) -> Result<FlattenGraphData, FlattenError> {
    pre_collect_functions(ctx, overlay, tree, class_index)?;
    extract_record_aliases(ctx, overlay, tree)?;
    for (outer, inner) in &overlay.outer_prefix_to_inner {
        ctx.record_aliases.insert(
            rumoca_core::ComponentPath::from_flat_path(outer),
            rumoca_core::ComponentPath::from_flat_path(inner),
        );
    }
    compute_transitive_alias_closure(&mut ctx.record_aliases);
    array_comprehension::extract_component_array_dimensions(ctx, overlay);
    let expanded_array_comprehension_bindings =
        if array_comprehension::has_expandable_array_comprehension_bindings(overlay) {
            ctx.build_parameter_lookup(flat, tree);
            array_comprehension::expand_array_comprehension_bindings(
                ctx,
                flat,
                overlay,
                tree,
                class_index,
                component_override_map,
            )?
        } else {
            false
        };
    if expanded_array_comprehension_bindings {
        ctx.build_parameter_lookup(flat, tree);
    }
    ctx.seed_flat_parameter_constant_keys(flat);
    inject_model_nested_class_constants(tree, class_index, model_name, ctx);
    inject_model_extends_redeclare_constants(tree, class_index, model_name, ctx);
    inject_enclosing_class_constants(tree, class_index, model_name, ctx)?;
    inject_component_instance_nested_class_constants(tree, class_index, overlay, ctx);
    // Re-apply parameter lookup from materialized flat variables after
    // class/package constant injection so record rebindings override injected
    // declaration defaults (MLS §7.2.3/§7.2.4, §8.3.3 structural ranges).
    inject_referenced_qualified_class_constants(tree, class_index, model_name, flat, overlay, ctx)?;
    stabilize_symbolic_component_dimensions(ctx, flat, overlay, tree)?;
    inject_referenced_qualified_class_constants(tree, class_index, model_name, flat, overlay, ctx)?;
    stabilize_symbolic_component_dimensions(ctx, flat, overlay, tree)?;
    ctx.refresh_enum_parameter_lookup(flat);
    pre_evaluate_structural_equations(ctx, overlay, tree)?;

    // Classify parameter-variability for-equation families now that the
    // parameter/constant key set is stable, so the cheapen gate in equation
    // expansion can drop their time-invariant per-cell bodies (the DAE promotes them
    // array-natively from the comprehension template).
    ctx.param_variability_family_bases =
        crate::param_variability::parameter_variability_family_bases(
            overlay,
            &ctx.flat_parameter_constant_keys,
        );

    let vcg_data = vcg::pre_collect_vcg_data(overlay, ctx)?;
    let optional_edges = vcg::derive_optional_edges(overlay, &vcg_data);
    flat.optional_edges = optional_edges.clone();
    let vcg_result = vcg::build_vcg(
        &vcg_data.definite_roots,
        &vcg_data.potential_roots,
        &vcg_data.branches,
        &optional_edges,
    );
    ctx.vcg_is_root = vcg_result.is_root;
    ctx.vcg_rooted = vcg_result.rooted;
    compute_cardinality_counts(ctx, overlay);

    Ok(FlattenGraphData {
        vcg_data,
        optional_edges,
    })
}

pub(crate) fn process_class_instances_for_flatten(
    ctx: &mut Context,
    flat: &mut flat::Model,
    overlay: &ast::InstanceOverlay,
    component_override_map: &ComponentOverrideMap,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
) -> Result<(), FlattenError> {
    for class_data in overlay.classes.values() {
        if is_in_disabled_component(&class_data.qualified_name, &overlay.disabled_components) {
            continue;
        }
        process_class_instance(
            ctx,
            flat,
            class_data,
            class_data.class_def_id,
            component_override_map,
            tree,
            class_index,
        )?;
    }
    Ok(())
}

fn stabilize_symbolic_component_dimensions(
    ctx: &mut Context,
    flat: &mut flat::Model,
    overlay: &ast::InstanceOverlay,
    tree: &ast::ClassTree,
) -> Result<(), FlattenError> {
    let max_passes = overlay.components.len().max(1) + 1;
    let mut parameter_lookup = ctx.collect_parameter_lookup_session(flat);
    for _ in 0..max_passes {
        ctx.build_parameter_lookup_with_session(flat, tree, &mut parameter_lookup);
        let reconciled = ctx.reconcile_modified_binding_dimensions(flat);
        let recomputed = ctx.recompute_symbolic_component_dimensions(flat, overlay, tree)?;
        if !reconciled && !recomputed {
            return Ok(());
        }
    }
    Ok(())
}

pub(crate) struct FinalizeFlatModelInput<'a, 'tree> {
    pub(crate) ctx: &'a mut Context,
    pub(crate) flat: &'a mut flat::Model,
    pub(crate) overlay: &'a ast::InstanceOverlay,
    pub(crate) tree: &'a ast::ClassTree,
    pub(crate) class_index: &'a ast::ClassDefIndex<'tree>,
    pub(crate) model_name: &'a str,
    pub(crate) options: FlattenOptions,
    pub(crate) flatten_graph: &'a FlattenGraphData,
    pub(crate) component_override_map: &'a ComponentOverrideMap,
}

pub(crate) fn finalize_flat_model(
    input: FinalizeFlatModelInput<'_, '_>,
) -> Result<(), FlattenError> {
    let FinalizeFlatModelInput {
        ctx,
        flat,
        overlay,
        tree,
        class_index,
        model_name,
        options,
        flatten_graph,
        component_override_map,
    } = input;
    outer_refs::redirect_outer_refs(flat, &overlay.outer_prefix_to_inner);
    materialize_referenced_expandable_connector_members(flat, overlay, tree, class_index)?;
    propagate_unexpanded_record_array_dims(flat, overlay);

    let connections_start = maybe_start_timer();
    let connections_result =
        connections::process_connections(flat, overlay, options.strict_connection_validation);
    maybe_record_connections_timing(connections_start);
    connections_result?;

    seed_flat_functions_from_context(ctx, flat);
    functions::collect_functions(flat, tree, class_index, Some(model_name))?;
    rewrite_function_extends_aliases_in_flat_functions(flat, tree, class_index)?;
    functions::collect_functions(flat, tree, class_index, Some(model_name))?;
    mark_record_constructor_calls(flat, tree);
    functions::lower_record_function_params(flat)?;
    functions::specialize_static_function_params(flat);
    mark_record_constructor_calls(flat, tree);
    canonicalize_varrefs_via_record_aliases(flat, ctx);
    canonicalize_varrefs_via_instantiated_def_ids(flat);
    drop_invalid_field_access_bindings(flat);
    propagate_unexpanded_record_array_dims(flat, overlay);
    flat.oc_break_edge_scalar_count = vcg::compute_break_edge_scalar_count(
        &flatten_graph.vcg_data.branches,
        &flatten_graph.optional_edges,
        &flatten_graph.vcg_data.definite_roots,
        &flatten_graph.vcg_data.potential_roots,
        flat,
    );

    collapse_index_refs_to_known_varrefs(flat);
    inject_referenced_qualified_class_constants(tree, class_index, model_name, flat, overlay, ctx)?;
    substitute_known_constants_in_flat(flat, ctx)?;
    stabilize_symbolic_component_dimensions(ctx, flat, overlay, tree)?;
    recover_indexed_lhs_dimensions(flat);
    mark_record_constructor_calls(flat, tree);
    let collected_new_functions = collect_rewritten_functions_to_fixed_point(
        flat,
        tree,
        class_index,
        model_name,
        component_override_map,
        &ctx.component_members,
    )?;
    if collected_new_functions {
        mark_record_constructor_calls(flat, tree);
        functions::lower_record_function_params(flat)?;
    }
    mark_record_constructor_calls(flat, tree);
    inject_referenced_qualified_class_constants(tree, class_index, model_name, flat, overlay, ctx)?;
    substitute_known_constants_in_flat(flat, ctx)?;
    mark_record_constructor_calls(flat, tree);
    collapse_index_refs_to_known_varrefs(flat);
    canonicalize_varrefs_via_instantiated_def_ids(flat);
    functions::canonicalize_collected_function_calls(flat);
    functions::lower_record_function_params(flat)?;
    mark_record_constructor_calls(flat, tree);
    substitute_known_constants_in_flat(flat, ctx)?;
    canonicalize_varrefs_via_record_aliases(flat, ctx);
    resolve_nested_constructor_field_access_bindings(flat);
    collapse_index_refs_to_known_varrefs(flat);
    functions::prune_unreachable_functions(flat);
    functions::validate_flat_function_bindings(flat)?;
    functions::validate_flat_function_call_args(flat)?;
    validate_overconstrained_roots(flat)?;

    ctx.refresh_enum_parameter_lookup(flat);
    enum_literals::canonicalize_flat_enum_literals(flat, tree, &ctx.enum_parameter_values);
    flat.enum_literal_ordinals = collect_enum_literal_ordinals(tree);
    if options.simplify_variable_names {
        name_simplify::simplify_flat_names(flat)?;
    }
    collapse_index_refs_to_known_varrefs(flat);
    // Final boundary pass: every rendered variable reference leaves flatten
    // with its structured component reference attached, so downstream phases
    // never re-derive structure from names.
    crate::structured_refs::attach_structured_references(flat)?;

    Ok(())
}

fn materialize_referenced_expandable_connector_members(
    flat: &mut flat::Model,
    overlay: &ast::InstanceOverlay,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
) -> Result<(), FlattenError> {
    let prefixes = overlay
        .components
        .values()
        .filter(|instance| instance.is_connector_type)
        .filter(|instance| {
            instance
                .type_def_id
                .and_then(|def_id| class_index.get(def_id))
                .is_some_and(|class_def| class_def.expandable)
        })
        .map(|instance| {
            let span = required_location_span(
                &tree.source_map,
                &instance.source_location,
                "expandable connector member",
            )?;
            Ok((instance.qualified_name.to_flat_string(), span))
        })
        .collect::<Result<Vec<_>, FlattenError>>()?;
    let referenced_names = overlay
        .classes
        .values()
        .flat_map(|class_data| &class_data.connections)
        .flat_map(|connection| [connection.a.to_flat_string(), connection.b.to_flat_string()])
        .chain(direct_equation_endpoint_varrefs(flat));
    materialize_referenced_expandable_connector_members_for_prefixes(
        flat,
        &prefixes,
        referenced_names,
    );
    Ok(())
}

fn direct_equation_endpoint_varrefs(flat: &flat::Model) -> Vec<String> {
    flat.equations
        .iter()
        .flat_map(|equation| match &equation.residual {
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs,
                rhs,
                ..
            } => [direct_varref_name(lhs), direct_varref_name(rhs)],
            _ => [None, None],
        })
        .flatten()
        .map(ToOwned::to_owned)
        .collect()
}

fn direct_varref_name(expr: &rumoca_core::Expression) -> Option<&str> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(name.as_str()),
        _ => None,
    }
}

fn materialize_referenced_expandable_connector_members_for_prefixes(
    flat: &mut flat::Model,
    prefixes: &[(String, rumoca_core::Span)],
    referenced_names: impl IntoIterator<Item = String>,
) {
    if prefixes.is_empty() {
        return;
    }

    for name in referenced_names {
        let name = rumoca_core::VarName::new(name);
        if flat.variables.contains_key(&name) {
            continue;
        }
        let Some((_, span)) = prefixes
            .iter()
            .filter(|(prefix, _)| is_expandable_member_ref(name.as_str(), prefix))
            .max_by_key(|(prefix, _)| prefix.len())
        else {
            continue;
        };
        let mut var = flat::Variable::empty_with_span(*span);
        var.name = name.clone();
        var.component_ref = Some(rumoca_core::ComponentReference::from_flat_segments(
            name.as_str(),
            *span,
            None,
        ));
        var.type_id = rumoca_core::TypeId::UNKNOWN;
        var.is_primitive = true;
        var.from_expandable_connector = true;
        flat.add_variable(name, var);
    }
}

fn is_expandable_member_ref(name: &str, prefix: &str) -> bool {
    name.len() > prefix.len()
        && name.as_bytes().get(prefix.len()) == Some(&b'.')
        && name.starts_with(prefix)
}

#[cfg(test)]
mod expandable_connector_member_tests {
    use super::*;

    fn var_ref(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(name),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }
    }

    #[test]
    fn materializes_referenced_expandable_connector_members_only_under_prefix() {
        let mut flat = flat::Model::new();
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var_ref("weaBus.TDryBul")),
                rhs: Box::new(var_ref("otherBus.TDryBul")),
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        let referenced_names = ["weaBus.TDryBul".to_string(), "otherBus.TDryBul".to_string()];
        materialize_referenced_expandable_connector_members_for_prefixes(
            &mut flat,
            &[("weaBus".to_string(), rumoca_core::Span::DUMMY)],
            referenced_names,
        );

        let var = flat
            .variables
            .get(&rumoca_core::VarName::new("weaBus.TDryBul"))
            .expect("referenced expandable connector member is materialized");
        assert!(var.from_expandable_connector);
        assert!(var.is_primitive);
        assert!(var.type_id.is_unknown());
        assert!(!flat.variable_type_names.contains_key(&var.name));
        assert!(
            !flat
                .variables
                .contains_key(&rumoca_core::VarName::new("otherBus.TDryBul"))
        );
    }

    #[test]
    fn does_not_materialize_non_connection_expandable_member_refs() {
        let mut flat = flat::Model::new();
        flat.add_equation(flat::Equation::new(
            var_ref("weaBus.TDryBul"),
            rumoca_core::Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        materialize_referenced_expandable_connector_members_for_prefixes(
            &mut flat,
            &[("weaBus".to_string(), rumoca_core::Span::DUMMY)],
            std::iter::empty(),
        );

        assert!(
            !flat
                .variables
                .contains_key(&rumoca_core::VarName::new("weaBus.TDryBul"))
        );
    }

    #[test]
    fn materializes_direct_equation_expandable_member_refs() {
        let mut flat = flat::Model::new();
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var_ref("weaBus.TWetBul")),
                rhs: Box::new(var_ref("building.weaBus.TWetBul")),
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        let referenced_names = direct_equation_endpoint_varrefs(&flat);
        materialize_referenced_expandable_connector_members_for_prefixes(
            &mut flat,
            &[
                ("weaBus".to_string(), rumoca_core::Span::DUMMY),
                ("building.weaBus".to_string(), rumoca_core::Span::DUMMY),
            ],
            referenced_names,
        );

        assert!(
            flat.variables
                .get(&rumoca_core::VarName::new("weaBus.TWetBul"))
                .is_some_and(|var| var.from_expandable_connector)
        );
        assert!(
            flat.variables
                .get(&rumoca_core::VarName::new("building.weaBus.TWetBul"))
                .is_some_and(|var| var.from_expandable_connector)
        );
    }
}

/// MLS §9.4 / CONN-013: every subgraph of the virtual connection graph needs
/// at least one definite or potential root. Tier-1 check: a model that uses
/// Connections.branch() but declares no root anywhere cannot satisfy this.
fn validate_overconstrained_roots(flat: &flat::Model) -> Result<(), FlattenError> {
    if !flat.branches.is_empty()
        && flat.definite_roots.is_empty()
        && flat.potential_roots.is_empty()
    {
        let (from, to) = &flat.branches[0];
        // Point the user at the branch endpoint: branch names are connector
        // paths, so the variable declared under that prefix carries the span.
        let span = flat
            .variables
            .values()
            .find(|var| {
                var.name.as_str().starts_with(from.as_str())
                    || var.name.as_str().starts_with(to.as_str())
            })
            .and_then(|var| (!var.source_span.is_dummy()).then_some(var.source_span))
            .ok_or_else(|| {
                FlattenError::missing_source_context(format!(
                    "Connections.branch({from}, {to}) has no source span on either endpoint"
                ))
            })?;
        return Err(FlattenError::UnsupportedEquation {
            description: format!(
                "Connections.branch({from}, {to}) is used but no Connections.root() or \
                 Connections.potentialRoot() is declared; every subgraph needs a root (MLS §9.4)"
            ),
            span: rumoca_core::span_to_source_span(span),
        });
    }
    Ok(())
}

fn collect_rewritten_functions_to_fixed_point(
    flat: &mut flat::Model,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    model_name: &str,
    component_override_map: &ComponentOverrideMap,
    component_members: &component_member_scope::ComponentMemberScopes,
) -> Result<bool, FlattenError> {
    const FUNCTION_REWRITE_FIXED_POINT_LIMIT: usize = 8;

    let initial_function_count = flat.functions.len();
    for _ in 0..FUNCTION_REWRITE_FIXED_POINT_LIMIT {
        let function_count_before = flat.functions.len();
        rewrite_function_overrides_in_flat_model(
            flat,
            tree,
            class_index,
            component_override_map,
            component_members,
        )?;
        functions::collect_functions(flat, tree, class_index, Some(model_name))?;
        if flat.functions.len() == function_count_before {
            return Ok(flat.functions.len() != initial_function_count);
        }
    }

    Err(FlattenError::function_rewrite_no_converge(
        FUNCTION_REWRITE_FIXED_POINT_LIMIT,
        flat.functions.len(),
    ))
}
