use super::*;

pub(super) fn validate_function_value_type(
    value: &rumoca_core::FunctionParam,
    function: &rumoca_core::Function,
    flat: &flat::Model,
    active_records: &mut HashSet<rumoca_core::DefId>,
) -> Result<(), ToDaeError> {
    if effective_function_scalar_type(flat, value).is_some() {
        return Ok(());
    }
    if value.type_class != Some(rumoca_core::ClassType::Record) {
        return Err(unsupported_type(value, function));
    }
    let type_def_id = value.type_def_id.ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "function value type",
            format!(
                "`{}.{}` has no resolved record type identity",
                function.name, value.name
            ),
            value.span,
        )
    })?;
    if !active_records.insert(type_def_id) {
        return Err(ToDaeError::unsupported_flat(
            "function value type",
            format!(
                "`{}.{}` belongs to a recursive value-record cycle",
                function.name, value.name
            ),
            value.span,
        ));
    }
    let constructor = rumoca_core::resolve_record_constructor(
        flat.functions.values(),
        &value.type_name,
        type_def_id,
    )
    .map_err(|error| {
        ToDaeError::unsupported_flat(
            "function value type",
            format!(
                "`{}.{}` has no resolved record constructor: {error}",
                function.name, value.name
            ),
            value.span,
        )
    })?;
    for field in &constructor.inputs {
        validate_function_value_type(field, function, flat, active_records)?;
    }
    active_records.remove(&type_def_id);
    Ok(())
}

fn unsupported_type(
    value: &rumoca_core::FunctionParam,
    function: &rumoca_core::Function,
) -> ToDaeError {
    ToDaeError::unsupported_flat(
        "function value type",
        format!(
            "`{}.{}` has unsupported type `{}`",
            function.name, value.name, value.type_name
        ),
        value.span,
    )
}

/// Leaf field projections of one record-typed function value.
///
/// MLS §12.2 lets a function body read a declared field of a record value
/// (`r.x`). Flat renders that read as one reference whose `VarName` joins the
/// declared value name with the field path, so the DAE symbol tables — planned
/// roles and the shape environment — need the same joined identities the read
/// carries. The paths are derived from the record constructor's declared field
/// order, which is the same order `function_value_type` interns into the DAE
/// record layout, so a projection name and its DAE field ordinal always agree.
///
/// A value that is not a record contributes nothing. Recursion is bounded by
/// `validate_function_value_type`, which rejects recursive value records before
/// any caller reaches this function.
pub(in crate::construction) fn record_field_projections<'flat>(
    value: &rumoca_core::FunctionParam,
    flat: &'flat flat::Model,
) -> Vec<(VarName, VarName, &'flat rumoca_core::FunctionParam)> {
    let mut projections = Vec::new();
    collect_record_field_projections(value, value.name.as_str(), flat, &mut projections);
    projections
}

fn collect_record_field_projections<'flat>(
    value: &rumoca_core::FunctionParam,
    path: &str,
    flat: &'flat flat::Model,
    projections: &mut Vec<(VarName, VarName, &'flat rumoca_core::FunctionParam)>,
) {
    if value.type_class != Some(rumoca_core::ClassType::Record) {
        return;
    }
    let Some(type_def_id) = value.type_def_id else {
        return;
    };
    let Ok(constructor) = rumoca_core::resolve_record_constructor(
        flat.functions.values(),
        &value.type_name,
        type_def_id,
    ) else {
        return;
    };
    for field in &constructor.inputs {
        let field_path = format!("{path}.{}", field.name);
        projections.push((VarName::new(&field_path), VarName::new(path), field));
        collect_record_field_projections(field, &field_path, flat, projections);
    }
}
