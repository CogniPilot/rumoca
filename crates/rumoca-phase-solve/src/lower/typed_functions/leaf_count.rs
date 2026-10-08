//! Interface leaf counts of value types.

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::{is_text_value, value_type_provenance};

/// The number of interface leaves of a value type: the length of
/// [`lower_value_type_leaves`] without building the leaves, so a consumer that
/// walks the fields of a large record pays for counts, not for a vector of
/// tensor types per field.
pub(super) fn value_type_leaf_count<'dae>(
    view: dae::DaeView<'dae>,
    id: dae::ValueTypeId<'dae>,
) -> Result<usize, solve::SolveProgramConstructionError> {
    let value_type = view
        .value_type(id)
        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
    if value_type.dimensions().contains(&0) || is_text_value(value_type) {
        return Ok(0);
    }
    if !value_type.is_record() {
        return Ok(1);
    }
    if value_type.record_field_count() == 0 {
        return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
            provenance: value_type_provenance(view, id),
        });
    }
    (0..value_type.record_field_count()).try_fold(0usize, |count, ordinal| {
        let (_, field_type) = view.record_field(id, ordinal).ok_or(
            solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: value_type_provenance(view, id),
            },
        )?;
        count
            .checked_add(value_type_leaf_count(view, field_type)?)
            .ok_or(solve::SolveProgramConstructionError::IdentityOverflow {
                provenance: value_type_provenance(view, id),
            })
    })
}
