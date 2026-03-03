use super::*;

pub(crate) struct FlattenGraphData {
    pub(crate) vcg_data: vcg::VcgPreScanData,
    pub(crate) optional_edges: Vec<(String, String)>,
}

pub(crate) fn initialize_flat_metadata(flat: &mut flat::Model, overlay: &ast::InstanceOverlay) {
    // MLS §4.7: Propagate partial status and class type from overlay
    flat.is_partial = overlay.is_partial;
    flat.class_type = overlay.class_type.clone();
    flat.model_description = overlay.root_description.clone();
}

pub(crate) fn collect_global_imports(overlay: &ast::InstanceOverlay) -> qualify::ImportMap {
    // MLS §13.2: Build a global import map from ALL class instances.
    // Modification bindings can be written at any ancestor scope, so we need
    // imports from all scopes available. Since Modelica imports are unambiguous
    // (the same short name always maps to the same FQN), a global merge is safe.
    overlay
        .classes
        .values()
        .flat_map(|class_data| class_data.resolved_imports.iter().cloned())
        .collect()
}

pub(crate) fn process_component_instances_for_flatten(
    ctx: &mut Context,
    flat: &mut flat::Model,
    overlay: &ast::InstanceOverlay,
    component_override_map: &ComponentOverrideMap,
    tree: &ast::ClassTree,
    global_imports: &qualify::ImportMap,
) -> Result<(), FlattenError> {
    for instance_data in overlay.components.values() {
        if is_in_disabled_component(&instance_data.qualified_name, &overlay.disabled_components) {
            continue;
        }
        process_component_instance(
            ctx,
            flat,
            instance_data,
            component_override_map,
            tree,
            global_imports,
        )?;
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
    if matches!(&instance_data.causality, rumoca_ir_ast::Causality::Input(_)) {
        flat.top_level_input_components.insert(name.clone());
    }
}

pub(crate) fn prepare_context_for_equation_flattening(
    ctx: &mut Context,
    flat: &mut flat::Model,
    overlay: &ast::InstanceOverlay,
    tree: &ast::ClassTree,
    model_name: &str,
) -> FlattenGraphData {
    pre_collect_functions(ctx, overlay, tree);
    extract_record_aliases(ctx, overlay);
    for (outer, inner) in &overlay.outer_prefix_to_inner {
        ctx.record_aliases.insert(outer.clone(), inner.clone());
    }
    compute_transitive_alias_closure(&mut ctx.record_aliases);
    ctx.build_parameter_lookup(flat, tree);
    array_comprehension::extract_component_array_dimensions(ctx, overlay);
    array_comprehension::expand_array_comprehension_bindings(ctx, flat, overlay, tree);
    ctx.build_parameter_lookup(flat, tree);
    inject_model_nested_class_constants(tree, model_name, ctx);
    inject_model_extends_redeclare_constants(tree, model_name, ctx);
    inject_enclosing_class_constants(tree, model_name, ctx);
    inject_component_instance_nested_class_constants(tree, overlay, ctx);
    // Re-apply parameter lookup from materialized flat variables after
    // class/package constant injection so record rebindings override injected
    // declaration defaults (MLS §7.2.3/§7.2.4, §8.3.3 structural ranges).
    ctx.build_parameter_lookup(flat, tree);
    ctx.refresh_enum_parameter_lookup(flat);
    pre_evaluate_structural_equations(ctx, overlay, tree);

    let vcg_data = vcg::pre_collect_vcg_data(overlay, ctx);
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

    FlattenGraphData {
        vcg_data,
        optional_edges,
    }
}

pub(crate) fn process_class_instances_for_flatten(
    ctx: &mut Context,
    flat: &mut flat::Model,
    overlay: &ast::InstanceOverlay,
    component_override_map: &ComponentOverrideMap,
    tree: &ast::ClassTree,
) -> Result<(), FlattenError> {
    for class_data in overlay.classes.values() {
        if is_in_disabled_component(&class_data.qualified_name, &overlay.disabled_components) {
            continue;
        }
        process_class_instance(ctx, flat, class_data, component_override_map, tree)?;
    }
    Ok(())
}

pub(crate) fn finalize_flat_model(
    ctx: &mut Context,
    flat: &mut flat::Model,
    overlay: &ast::InstanceOverlay,
    tree: &ast::ClassTree,
    model_name: &str,
    options: FlattenOptions,
    flatten_graph: &FlattenGraphData,
) -> Result<(), FlattenError> {
    outer_refs::redirect_outer_refs(flat, &overlay.outer_prefix_to_inner);

    let connections_start = maybe_start_timer();
    let connections_result =
        connections::process_connections(flat, overlay, options.strict_connection_validation);
    maybe_record_connections_timing(connections_start);
    connections_result?;

    seed_flat_functions_from_context(ctx, flat);
    functions::collect_functions(flat, tree)?;
    rewrite_function_extends_aliases_in_flat_functions(flat, tree);
    functions::collect_functions(flat, tree)?;
    mark_record_constructor_calls(flat, tree);
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
    inject_referenced_qualified_class_constants(tree, model_name, flat, ctx);
    substitute_known_constants_in_flat(flat, ctx);
    functions::collect_functions(flat, tree)?;
    mark_record_constructor_calls(flat, tree);
    substitute_known_constants_in_flat(flat, ctx);

    ctx.refresh_enum_parameter_lookup(flat);
    enum_literals::canonicalize_flat_enum_literals(flat, tree, &ctx.enum_parameter_values);
    flat.enum_literal_ordinals = collect_enum_literal_ordinals(tree);
    Ok(())
}
