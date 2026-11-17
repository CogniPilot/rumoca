//! Canonical scalar types of effective function and variable declarations.

use super::*;

pub(in crate::construction) fn effective_function_scalar_type(
    flat: &flat::Model,
    value: &rumoca_core::FunctionParam,
) -> Option<dae::ScalarType> {
    let canonical = value.effective_type.canonical_type();
    if canonical == flat.predefined_types.real {
        Some(dae::ScalarType::Real)
    } else if canonical == flat.predefined_types.integer {
        Some(dae::ScalarType::Integer)
    } else if canonical == flat.predefined_types.boolean {
        Some(dae::ScalarType::Boolean)
    } else if canonical == flat.predefined_types.string {
        Some(dae::ScalarType::String)
    } else if flat.enumeration_type_roots.contains(&canonical) {
        Some(dae::ScalarType::Enumeration)
    } else {
        None
    }
}

pub(in crate::construction) fn effective_variable_scalar_type(
    flat: &flat::Model,
    variable: &flat::Variable,
) -> Option<dae::ScalarType> {
    let canonical = flat
        .effective_types
        .get(&variable.type_id)?
        .canonical_type();
    if canonical == flat.predefined_types.real {
        Some(dae::ScalarType::Real)
    } else if canonical == flat.predefined_types.integer {
        Some(dae::ScalarType::Integer)
    } else if canonical == flat.predefined_types.boolean {
        Some(dae::ScalarType::Boolean)
    } else if canonical == flat.predefined_types.string {
        Some(dae::ScalarType::String)
    } else if flat.enumeration_types.contains(&variable.type_id) {
        Some(dae::ScalarType::Enumeration)
    } else if crate::construction::native_tables::native_table_family_of_variable(flat, variable)
        .is_some()
    {
        // MLS §12.9.7: a native standard-library table handle is an opaque
        // ExternalObject, modeled as an integer table id (never a Real).
        Some(dae::ScalarType::Integer)
    } else {
        None
    }
}

/// `value` as a value of `variable`'s declared predefined scalar type: an
/// Integer settled for a Real declaration is the Real it denotes (MLS 3.7
/// §10.6.13), so every later evaluation that reads it computes in Real
/// arithmetic; a value of another type is refused.
pub(in crate::construction) fn declared_parameter_value(
    flat: &flat::Model,
    variable: &flat::Variable,
    value: EvalValue,
) -> Result<EvalValue, rumoca_eval_flat::constant::EvalError> {
    let declared = flat
        .predefined_types
        .is_complete()
        .then(|| flat.effective_types.get(&variable.type_id))
        .flatten()
        .and_then(|effective| {
            rumoca_eval_flat::constant::DeclaredScalar::of(effective, &flat.predefined_types)
        });
    match declared {
        Some(declared) => declared.convert(value, variable.source_span),
        None => Ok(value),
    }
}
