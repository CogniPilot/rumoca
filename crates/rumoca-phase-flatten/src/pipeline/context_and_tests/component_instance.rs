//! Component instance lowering: primitive components become flat variables
//! and record components become flat record instances, with override rewriting
//! applied to their modifier expressions.

use super::*;

/// Process a component instance to create a flat variable.
///
/// Only primitive types (Real, Integer, Boolean, String) become flat variables.
/// Class types (connectors, models, records) are containers and are skipped.
pub(crate) struct ComponentInstanceProcess<'a, 'tree> {
    pub(crate) flat: &'a mut Model,
    pub(crate) instance_data: &'a rumoca_ir_ast::InstanceData,
    pub(crate) effective_type_id: rumoca_core::TypeId,
    pub(crate) canonical_type_id: rumoca_core::TypeId,
    pub(crate) component_override_map: &'a ComponentOverrideMap,
    pub(crate) tree: &'a rumoca_ir_ast::ClassTree,
    pub(crate) class_index: &'a rumoca_ir_ast::ClassDefIndex<'tree>,
    pub(crate) import_cache: &'a mut ImportCaches<'tree>,
    pub(crate) scope_index: &'a OverlayScopeIndex<'a>,
    pub(crate) component_members: &'a component_member_scope::ComponentMemberScopes,
    pub(crate) function_types: functions::FunctionTypeCatalog<'a>,
}

pub(crate) fn process_component_instance(
    request: ComponentInstanceProcess<'_, '_>,
) -> Result<(), FlattenError> {
    // Skip if this is an empty path (root)
    let var_name = qualified_to_var_name(&request.instance_data.qualified_name);
    if var_name.as_str().is_empty() {
        return Ok(());
    }

    // Record fields are Flat variables; retain only their container's resolved
    // identity so downstream record equations can expand without name recovery.
    if !request.instance_data.is_primitive {
        if let Some(record) = variables::create_record_instance(
            request.instance_data,
            request.tree,
            request.class_index,
            request.effective_type_id,
        )? {
            if !request.flat.record_types.contains_key(&record.type_def_id) {
                let record_type = variables::create_record_type(
                    record.type_def_id,
                    request.tree,
                    request.class_index,
                    request.function_types,
                )?;
                request
                    .flat
                    .record_types
                    .insert(record.type_def_id, record_type);
            }
            request.flat.record_instances.insert(var_name, record);
        }
        return Ok(());
    }

    let import_context = variable_import_context_for_instance(
        request.instance_data,
        request.tree,
        request.class_index,
        request.import_cache,
        request.scope_index,
        request.component_override_map,
    )?;
    let mut flat_var = variables::create_flat_variable(
        request.instance_data,
        request.effective_type_id,
        request.tree,
        request.class_index,
        &import_context,
    )?;
    for expression in [
        &mut flat_var.binding,
        &mut flat_var.start,
        &mut flat_var.min,
        &mut flat_var.max,
        &mut flat_var.nominal,
    ]
    .into_iter()
    .flatten()
    {
        attach_reference_scope(
            expression,
            request.instance_data.owner_class_id.ok_or_else(|| {
                FlattenError::internal("Flat variable has no instantiated class owner")
            })?,
        )?;
    }
    let instance_scope = request.instance_data.qualified_name.to_component_path();
    let (override_packages, override_functions) =
        override_context_for_component_path(&instance_scope, request.component_override_map);
    let receiver_scope = instance_scope
        .parent()
        .unwrap_or_else(rumoca_core::ComponentPath::root);
    rewrite_function_overrides_in_flat_variable(
        &mut flat_var,
        request.tree,
        request.class_index,
        &override_packages,
        &override_functions,
        &receiver_scope,
        request.component_members,
    )?;
    request.flat.variable_type_names.insert(
        var_name.clone(),
        variables::flat_output_type_name(
            request.instance_data,
            request.canonical_type_id,
            request.tree,
        )?,
    );
    if request.instance_data.is_final {
        request
            .flat
            .variable_final_flags
            .insert(var_name.clone(), true);
    }
    request.flat.add_variable(var_name, flat_var);

    Ok(())
}
