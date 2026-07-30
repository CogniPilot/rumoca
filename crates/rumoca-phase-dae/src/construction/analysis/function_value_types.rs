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
    if !value.dimensions().is_empty() {
        return Err(ToDaeError::unsupported_flat(
            "function value type",
            format!(
                "`{}.{}` is an array of records; aggregate arrays require a compact typed owner",
                function.name, value.name
            ),
            value.span,
        ));
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
