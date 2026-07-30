//! Integer-to-enumeration conversion at the DAE boundary (MLS §4.9.5.2).
//!
//! `E(i)` is written with call syntax but is a type conversion, not a call: `E`
//! names an enumeration type, so no `Function` is ever registered for it and the
//! Flat call carries no resolved callee. DAE construction already owns the
//! enumeration *value* representation — an enumeration literal reference lowers
//! to `enumeration_literal(ordinal)` through `Model::enum_literal_ordinals` — so
//! the conversion that produces the same value belongs to the same owner.
//!
//! Recognition uses the exact identity Flat carries, never the spelling of the
//! callee: the reference's resolved declaration identity is mapped through
//! `Model::type_ids_by_def_id` and accepted only when the resulting canonical
//! type is in `Model::enumeration_type_roots`.

use super::*;

/// One recognized `E(i)` conversion.
pub(super) struct EnumerationConversion {
    pub(super) ordinal: i64,
}

/// Recognize `name(args)` as an Integer-to-enumeration conversion.
///
/// `Ok(None)` means the callee is not an enumeration type, so the call remains a
/// function call for its own owner to resolve. An error means the callee *is* an
/// enumeration type but the conversion has no checked DAE owner.
pub(super) fn enumeration_conversion(
    flat: &flat::Model,
    name: &rumoca_core::Reference,
    args: &[Expression],
    span: Span,
) -> Result<Option<EnumerationConversion>, ToDaeError> {
    if !names_enumeration_type(flat, name) {
        return Ok(None);
    }
    let [argument] = args else {
        return Err(ToDaeError::unsupported_flat(
            "enumeration conversion",
            format!(
                "`{}` converts exactly one Integer value but receives {}",
                name.as_str(),
                args.len()
            ),
            span,
        ));
    };
    let Expression::Literal {
        value: rumoca_core::Literal::Integer(ordinal),
        ..
    } = argument
    else {
        return Err(ToDaeError::unsupported_flat(
            "enumeration conversion",
            format!(
                "`{}` requires an Integer ordinal proven constant before Flat",
                name.as_str()
            ),
            span,
        ));
    };
    Ok(Some(EnumerationConversion { ordinal: *ordinal }))
}

/// Whether `name` denotes an enumeration type declaration.
fn names_enumeration_type(flat: &flat::Model, name: &rumoca_core::Reference) -> bool {
    // A resolved callable always wins: a user function may legally share a
    // declaration identity table entry with nothing here, and the function
    // registry is the authoritative owner for calls it can answer.
    if flat.functions.contains_key(name.var_name()) {
        return false;
    }
    let Some(def_id) = name.target_def_id() else {
        return false;
    };
    let Some(type_id) = flat.type_ids_by_def_id.get(&def_id) else {
        return false;
    };
    flat.enumeration_type_roots.contains(type_id)
}
