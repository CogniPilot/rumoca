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

/// The enumeration type declaration that owns both bounds of a compact range
/// (MLS §10.4.1: `e1:e2` over one enumeration type yields the values from `e1`
/// to `e2`).
///
/// Recognition is identity-only, so the two owners that must agree reach the
/// same answer from the same exact identities: the model validator decides
/// "is an enumeration literal" from its planned roles, and expression lowering
/// decides it from the Flat literal catalog. `None` means the range is not an
/// enumeration range and belongs to the Integer compact-range owner.
///
/// MLS §10.4.1 gives an enumeration range no step, so an explicit step is never
/// an enumeration range.
pub(super) fn enumeration_range_type(
    start: &Expression,
    step: Option<&Expression>,
    end: &Expression,
    is_enumeration_literal: &dyn Fn(&rumoca_core::Reference) -> bool,
) -> Option<rumoca_core::DefId> {
    if step.is_some() {
        return None;
    }
    let start = enumeration_bound_type(start, is_enumeration_literal)?;
    let end = enumeration_bound_type(end, is_enumeration_literal)?;
    (start == end).then_some(start)
}

/// Whether either compact-range bound is an enumeration literal.
///
/// A range that mixes an enumeration literal with any other bound has no
/// checked DAE owner, and its rejection must name that cause instead of the
/// Integer compact-range requirement.
pub(super) fn has_enumeration_range_bound(
    start: &Expression,
    end: &Expression,
    is_enumeration_literal: &dyn Fn(&rumoca_core::Reference) -> bool,
) -> bool {
    enumeration_bound_type(start, is_enumeration_literal).is_some()
        || enumeration_bound_type(end, is_enumeration_literal).is_some()
}

/// The declaration identity of one enumeration-literal compact-range bound.
fn enumeration_bound_type(
    expression: &Expression,
    is_enumeration_literal: &dyn Fn(&rumoca_core::Reference) -> bool,
) -> Option<rumoca_core::DefId> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expression
    else {
        return None;
    };
    if !subscripts.is_empty() || !is_enumeration_literal(name) {
        return None;
    }
    name.target_def_id()
}

/// Whether `name` is an enumeration literal declared by an enumeration type.
///
/// Flat's literal catalog answers "is a literal" and the declaration-identity
/// tables answer "declared by an enumeration type"; both must hold, so a model
/// coordinate that happens to share a leaf spelling is never accepted.
pub(super) fn is_flat_enumeration_literal(
    flat: &flat::Model,
    name: &rumoca_core::Reference,
) -> bool {
    flat.enum_literal_ordinals.contains_key(name.as_str()) && names_enumeration_type(flat, name)
}

/// The inclusive ordinal bounds of a recognized enumeration compact range.
///
/// `None` means at least one bound has no ordinal in the Flat literal catalog,
/// which its caller rejects at the range's own provenance.
pub(super) fn enumeration_range_ordinals(
    flat: &flat::Model,
    start: &Expression,
    end: &Expression,
) -> Option<(i64, i64)> {
    let start = enumeration_bound_ordinal(flat, start)?;
    let end = enumeration_bound_ordinal(flat, end)?;
    Some((start, end))
}

fn enumeration_bound_ordinal(flat: &flat::Model, expression: &Expression) -> Option<i64> {
    let Expression::VarRef { name, .. } = expression else {
        return None;
    };
    flat.enum_literal_ordinals.get(name.as_str()).copied()
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
