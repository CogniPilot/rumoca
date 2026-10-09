//! Typed leaves of DAE value types: the interface width every pure-call
//! consumer agrees on.

use super::arithmetic_profile;
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
    Ok(lower_value_type_layout(view, id, arithmetic, false)?.leaves)
}

struct ValueTypeLayout {
    leaves: Vec<solve::SolveValueType>,
    projection: Projection,
}

enum Projection {
    Leaf,
    Record(Vec<ProjectionField>),
}

struct ProjectionField {
    leaves: Range<usize>,
    // An interface can be valid without supporting a packed-lane projection.
    // Demand of that projection, rather than construction, refuses absence.
    packing: Option<dae::RecordFieldLayout>,
    projection: Projection,
}

impl Projection {
    fn field_scalar(&self, field: usize, scalar: usize) -> Option<(usize, usize)> {
        let Self::Record(fields) = self else {
            return None;
        };
        let selected = fields.get(field)?;
        let (leaf, scalar) = selected.projection.packed_scalar(scalar)?;
        let leaf = selected.leaves.start.checked_add(leaf)?;
        (leaf < selected.leaves.end).then_some((leaf, scalar))
    }

    fn packed_scalar(&self, scalar: usize) -> Option<(usize, usize)> {
        let Self::Record(fields) = self else {
            return Some((0, scalar));
        };
        for (ordinal, field) in fields.iter().enumerate() {
            let layout = field.packing?;
            if layout.record_width() == 0 {
                return None;
            }
            let element = scalar / layout.record_width();
            let Some(lane) = (scalar % layout.record_width()).checked_sub(layout.field_offset())
            else {
                continue;
            };
            if lane < layout.field_width() {
                let scalar = element
                    .checked_mul(layout.field_width())?
                    .checked_add(lane)?;
                return self.field_scalar(ordinal, scalar);
            }
        }
        None
    }
}

fn lower_value_type_layout<'dae>(
    view: dae::DaeView<'dae>,
    id: dae::ValueTypeId<'dae>,
    arithmetic: solve::SolveArithmeticProfile,
    project: bool,
) -> Result<ValueTypeLayout, solve::SolveProgramConstructionError> {
    let value_type = view
        .value_type(id)
        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
    if value_type.dimensions().contains(&0) || is_text_value(value_type) {
        return Ok(ValueTypeLayout {
            leaves: Vec::new(),
            projection: Projection::Leaf,
        });
    }
    if !value_type.is_record() {
        return Ok(ValueTypeLayout {
            leaves: vec![lower_primitive_type(view, id, arithmetic)?],
            projection: Projection::Leaf,
        });
    }
    // A record the DAE resolved names its fields. A record that names none is
    // an unresolved type, not an empty one, so it never reaches an interface.
    if value_type.record_field_count() == 0 {
        return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
            provenance: value_type_provenance(view, id),
        });
    }
    let mut leaves = Vec::new();
    let mut fields = Vec::with_capacity(if project {
        value_type.record_field_count()
    } else {
        0
    });
    for ordinal in 0..value_type.record_field_count() {
        let (_, field_type) = view.record_field(id, ordinal).ok_or(
            solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: value_type_provenance(view, id),
            },
        )?;
        let child = lower_value_type_layout(view, field_type, arithmetic, project)?;
        let start = leaves.len();
        for field_leaf in child.leaves {
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
        if project {
            fields.push(ProjectionField {
                leaves: start..leaves.len(),
                packing: view.record_field_layout(id, ordinal),
                projection: child.projection,
            });
        }
    }
    Ok(ValueTypeLayout {
        leaves,
        projection: Projection::Record(fields),
    })
}

/// Immutable result layout of one exact function signature and arithmetic.
/// Scalar prefixes and child leaf ranges come from the emitted leaf vector.
pub(crate) struct FunctionResultsLayout<'dae> {
    function: dae::FunctionId<'dae>,
    pub(super) signature: Vec<dae::ValueTypeId<'dae>>,
    arithmetic: solve::SolveArithmeticProfile,
    pub(super) leaves: Vec<solve::SolveValueType>,
    pub(super) ranges: Vec<Range<usize>>,
    projections: Vec<Projection>,
    scalar_offsets: Vec<usize>,
    scalar_records: Vec<bool>,
}

impl<'dae> FunctionResultsLayout<'dae> {
    pub(super) fn lower(
        view: dae::DaeView<'dae>,
        function: dae::FunctionView<'dae>,
        arithmetic: solve::SolveArithmeticProfile,
    ) -> Result<Self, solve::SolveProgramConstructionError> {
        let signature = function.result_types().iter().collect::<Vec<_>>();
        let mut leaves = Vec::new();
        let mut ranges = Vec::with_capacity(signature.len());
        let mut projections = Vec::with_capacity(signature.len());
        let mut scalar_records = Vec::with_capacity(signature.len());
        for &id in &signature {
            let layout = lower_value_type_layout(view, id, arithmetic, true)?;
            let start = leaves.len();
            leaves.extend(layout.leaves);
            ranges.push(start..leaves.len());
            projections.push(layout.projection);
            let value_type = view
                .value_type(id)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
            scalar_records.push(value_type.is_record() && value_type.dimensions().is_empty());
        }
        let mut scalar_offsets = Vec::with_capacity(leaves.len() + 1);
        let mut scalar_offset = 0usize;
        scalar_offsets.push(scalar_offset);
        for leaf in &leaves {
            scalar_offset = scalar_offset
                .checked_add(leaf.scalar_count() as usize)
                .ok_or(solve::SolveProgramConstructionError::IdentityOverflow {
                    provenance: function.declaration().span(),
                })?;
            scalar_offsets.push(scalar_offset);
        }
        Ok(Self {
            function: function.id(),
            signature,
            arithmetic,
            leaves,
            ranges,
            projections,
            scalar_offsets,
            scalar_records,
        })
    }

    pub(crate) fn scalar_range(&self, output: usize) -> Option<Range<usize>> {
        let range = self.ranges.get(output)?;
        Some(*self.scalar_offsets.get(range.start)?..*self.scalar_offsets.get(range.end)?)
    }

    pub(crate) fn record_scalar(
        &self,
        function: dae::FunctionId<'dae>,
        output: usize,
        field: usize,
        scalar: usize,
    ) -> Option<usize> {
        if function != self.function
            || self.arithmetic != arithmetic_profile()
            || !*self.scalar_records.get(output)?
        {
            return None;
        }
        let range = self.ranges.get(output)?;
        let (leaf, scalar) = self.projections.get(output)?.field_scalar(field, scalar)?;
        let leaf = range.start.checked_add(leaf)?;
        if leaf >= range.end || scalar >= self.leaves.get(leaf)?.scalar_count() as usize {
            return None;
        }
        self.scalar_offsets.get(leaf)?.checked_add(scalar)
    }
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
        let width = lower_value_type_leaves(view, field_type, arithmetic)?.len();
        let end = start.checked_add(width).ok_or(
            solve::SolveProgramConstructionError::IdentityOverflow {
                provenance: value_type_provenance(view, record),
            },
        )?;
        if ordinal == field {
            return Ok(start..end);
        }
        start = end;
    }
    Err(solve::SolveProgramConstructionError::InvalidCallInterface {
        provenance: value_type_provenance(view, record),
    })
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

#[cfg(test)]
mod tests;
