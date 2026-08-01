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

/// Class-body occurrence that scopes the references in a modifier binding
/// (MLS §7.2.4).
///
/// A modification's right-hand side is written in the *modifier's* class body,
/// not in the body of the component it modifies: `stateGraphRoot(suspend =
/// anyTrue(suspend.reset))` names the `suspend` that the enclosing class owns.
/// `variables::qualify_variable_binding` already spells such a binding with the
/// modifier's instance path (`InstanceData::binding_source_scope`), so the
/// occurrence identity carried alongside that spelling must be the class body
/// standing at the same path — the declaring component's own body owns none of
/// what the modifier named.
///
/// Accepted: a variable whose binding did not come from a modification (answers
/// `None`, leaving the declaring body as the scope, which is where a
/// declaration binding is written); and a modifier binding whose recorded scope
/// is an instantiated class body, including the root model's empty path.
/// Rejected: a modifier binding whose recorded scope names no class body, which
/// would otherwise scope the reference to a body that cannot contain it.
/// A modifier binding with no recorded scope at all is already rejected by
/// `variables::qualify_variable_binding`, which needs the same scope to spell
/// the reference.
fn modifier_binding_scope(
    instance_data: &rumoca_ir_ast::InstanceData,
    scope_index: &OverlayScopeIndex<'_>,
    source_span: rumoca_core::Span,
) -> Result<Option<rumoca_core::InstanceId>, FlattenError> {
    if !instance_data.binding_from_modification {
        return Ok(None);
    }
    let Some(scope) = instance_data.binding_source_scope.as_ref() else {
        return Ok(None);
    };
    scope_index
        .class_occurrence(scope)
        .map(Some)
        .ok_or_else(|| {
            FlattenError::missing_source_scope(
                instance_data.qualified_name.to_flat_string(),
                format!("modifier binding written in `{}`", scope.to_flat_string()),
                source_span,
            )
        })
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
    let declaration_scope = request
        .instance_data
        .owner_class_id
        .ok_or_else(|| FlattenError::internal("Flat variable has no instantiated class owner"))?;
    let binding_scope = modifier_binding_scope(
        request.instance_data,
        request.scope_index,
        flat_var.source_span,
    )?
    .unwrap_or(declaration_scope);
    if let Some(expression) = flat_var.binding.as_mut() {
        attach_reference_scope(expression, binding_scope)?;
    }
    for expression in [
        &mut flat_var.start,
        &mut flat_var.min,
        &mut flat_var.max,
        &mut flat_var.nominal,
    ]
    .into_iter()
    .flatten()
    {
        attach_reference_scope(expression, declaration_scope)?;
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
