//! Typed leaves of DAE value types: the interface width every pure-call
//! consumer agrees on.

use super::arithmetic_profile;
use super::leaf_count::value_type_leaf_count;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use std::ops::Range;

/// Whether a value is text: a `String` scalar or array (MLS §4.9.4).
///
/// Text carries no numeric value, so it occupies no leaf of a pure-call
/// interface; a record such as `IdealGases.Common.DataRecord` passes its
/// numeric fields and leaves its `name` out. Every numeric consumer of a value
/// demands a register, so a body or call site that computes with a text value
/// is refused at construction rather than handed an empty one.
pub(crate) fn is_text_value(value_type: &dae::ValueType) -> bool {
    !value_type.is_record() && value_type.scalar_type() == dae::ScalarType::String
}

/// Typed leaves one DAE value type occupies in a pure-call interface.
///
/// A leaf holds scalars, so a value type holds exactly as many leaves as it
/// takes to hold its scalars. MLS 3.6 §10.3.1 admits a zero-size array
/// dimension, and such a value holds no scalars at all: it occupies no leaf.
/// That is the same rule the record arm applies field by field, so a record
/// with a zero-size field is narrower than its field count by construction and
/// every consumer that walks leaves by width - the interface, the call site's
/// packing, and record-field projection - agrees without a second convention.
pub(super) fn lower_value_type_leaves<'dae>(
    view: dae::DaeView<'dae>,
    id: dae::ValueTypeId<'dae>,
    arithmetic: solve::SolveArithmeticProfile,
) -> Result<Vec<solve::SolveValueType>, solve::SolveProgramConstructionError> {
    let value_type = view
        .value_type(id)
        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
    if value_type.dimensions().contains(&0) || is_text_value(value_type) {
        return Ok(Vec::new());
    }
    if !value_type.is_record() {
        return Ok(vec![lower_primitive_type(view, id, arithmetic)?]);
    }
    // A record the DAE resolved names its fields. A record that names none is
    // an unresolved type, not an empty one, so it never reaches an interface.
    if value_type.record_field_count() == 0 {
        return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
            provenance: value_type_provenance(view, id),
        });
    }
    let mut leaves = Vec::new();
    for ordinal in 0..value_type.record_field_count() {
        let (_, field_type) = view.record_field(id, ordinal).ok_or(
            solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: value_type_provenance(view, id),
            },
        )?;
        for field_leaf in lower_value_type_leaves(view, field_type, arithmetic)? {
            if value_type.dimensions().is_empty() {
                leaves.push(field_leaf);
                continue;
            }
            let mut dimensions = value_type.dimensions().to_vec();
            dimensions.extend_from_slice(field_leaf.dimensions());
            leaves.push(
                solve::SolveValueType::tensor(field_leaf.element_type(), dimensions).map_err(
                    |_| solve::SolveProgramConstructionError::InvalidCallInterface {
                        provenance: value_type_provenance(view, id),
                    },
                )?,
            );
        }
    }
    Ok(leaves)
}

pub(super) fn value_type_provenance<'dae>(
    view: dae::DaeView<'dae>,
    id: dae::ValueTypeId<'dae>,
) -> rumoca_core::Span {
    view.value_type_provenance(id)
        .expect("final DAE value type carries provenance")
        .span()
}

pub(super) fn record_field_leaf_range<'dae>(
    view: dae::DaeView<'dae>,
    record: dae::ValueTypeId<'dae>,
    field: usize,
    arithmetic: solve::SolveArithmeticProfile,
) -> Result<Range<usize>, solve::SolveProgramConstructionError> {
    let value_type = view
        .value_type(record)
        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
    if !value_type.is_record() {
        return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
            provenance: value_type_provenance(view, record),
        });
    }
    let mut start = 0usize;
    for ordinal in 0..value_type.record_field_count() {
        let (_, field_type) = view.record_field(record, ordinal).ok_or(
            solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: value_type_provenance(view, record),
            },
        )?;
        let width = value_type_leaf_count(view, field_type, arithmetic)?;
        if ordinal == field {
            return Ok(start..start + width);
        }
        start = start.checked_add(width).ok_or(
            solve::SolveProgramConstructionError::IdentityOverflow {
                provenance: value_type_provenance(view, record),
            },
        )?;
    }
    Err(solve::SolveProgramConstructionError::InvalidCallInterface {
        provenance: value_type_provenance(view, record),
    })
}

/// The result leaf and the scalar within it that hold scalar `scalar` of
/// field `field` of a value of record type `record`, relative to the
/// record's own leaves.
///
/// Leaves are the record's fields depth first, each one tensor over the
/// enclosing record extents (struct of arrays). A non-record field's scalar
/// is row major over those extents and its own; a record-typed field's
/// scalar is a packed lane, element major over its records (the DAE record
/// field layout), and selects a field of its element in turn.
pub(crate) fn record_field_scalar_leaf<'dae>(
    view: dae::DaeView<'dae>,
    record: dae::ValueTypeId<'dae>,
    field: usize,
    scalar: usize,
) -> Result<(usize, usize), solve::SolveProgramConstructionError> {
    let invalid = || solve::SolveProgramConstructionError::InvalidCallInterface {
        provenance: value_type_provenance(view, record),
    };
    let leaves = record_field_leaf_range(view, record, field, arithmetic_profile())?;
    let (_, field_type) = view.record_field(record, field).ok_or_else(invalid)?;
    let nested = view.value_type(field_type).ok_or_else(invalid)?;
    if !nested.is_record() {
        return Ok((leaves.start, scalar));
    }
    for ordinal in 0..nested.record_field_count() {
        let layout = view
            .record_field_layout(field_type, ordinal)
            .ok_or_else(invalid)?;
        if layout.record_width() == 0 {
            return Err(invalid());
        }
        let element = scalar / layout.record_width();
        let Some(lane) = (scalar % layout.record_width()).checked_sub(layout.field_offset()) else {
            continue;
        };
        if lane < layout.field_width() {
            let (leaf, leaf_scalar) = record_field_scalar_leaf(
                view,
                field_type,
                ordinal,
                element * layout.field_width() + lane,
            )?;
            return Ok((leaves.start + leaf, leaf_scalar));
        }
    }
    Err(invalid())
}

pub(super) fn lower_primitive_type<'dae>(
    view: dae::DaeView<'dae>,
    id: dae::ValueTypeId<'dae>,
    arithmetic: solve::SolveArithmeticProfile,
) -> Result<solve::SolveValueType, solve::SolveProgramConstructionError> {
    let value_type = view
        .value_type(id)
        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
    let scalar = match value_type.scalar_type() {
        dae::ScalarType::Real => solve::SolveScalarType::real(arithmetic),
        dae::ScalarType::Integer | dae::ScalarType::Enumeration => {
            solve::SolveScalarType::integer(arithmetic)
        }
        dae::ScalarType::Boolean => solve::SolveScalarType::Boolean,
        dae::ScalarType::String | dae::ScalarType::Record => {
            return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: value_type_provenance(view, id),
            });
        }
    };
    if value_type.dimensions().is_empty() {
        Ok(solve::SolveValueType::scalar(scalar))
    } else {
        solve::SolveValueType::tensor(scalar, value_type.dimensions().to_vec()).map_err(|_| {
            solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: value_type_provenance(view, id),
            }
        })
    }
}
