//! The one issued call-interface packing layout for DAE value types.

use std::ops::Range;

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::invalid_call_interface;

/// The single issued packing layout for one DAE call-interface value type.
///
/// Every consumer that needs to know how a value type occupies typed leaves
/// reads this layout instead of recomputing the decomposition: the owner
/// interface walk, the record-field projection, and the expression lowerer's
/// aggregate, index, element-write, and field arms. One value type therefore
/// owns one leaf order, and a consumer cannot drift from the interface its
/// owner was issued against.
///
/// Two shapes only the DAE can state are carried here.
///
/// A record array keeps its outer extents on every leaf, so `Candidate[2]`
/// owns `length: Real[2]` and `feasible: Boolean[2]` rather than one leaf per
/// element. An element write or read therefore projects the outer axes of each
/// leaf and never expands the array.
///
/// A zero-width component owns an empty leaf range. `Real m[0]` declares
/// storage that holds no value, and the typed vocabulary has no zero-extent
/// tensor, so contributing nothing is the only representation that keeps the
/// declaration inside the checked interface instead of rejecting it.
pub(super) struct CallAbiLayout {
    leaves: Vec<solve::SolveValueType>,
    field_ranges: Vec<Range<usize>>,
    outer_rank: usize,
}

impl CallAbiLayout {
    /// Issue the layout `id` occupies at a call interface.
    pub(super) fn issue<'dae>(
        view: dae::DaeView<'dae>,
        id: dae::ValueTypeId<'dae>,
        arithmetic: solve::SolveArithmeticProfile,
    ) -> Result<Self, solve::SolveProgramConstructionError> {
        Self::issue_under(view, id, arithmetic, &[])
    }

    fn issue_under<'dae>(
        view: dae::DaeView<'dae>,
        id: dae::ValueTypeId<'dae>,
        arithmetic: solve::SolveArithmeticProfile,
        outer: &[u32],
    ) -> Result<Self, solve::SolveProgramConstructionError> {
        let value_type = view
            .value_type(id)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        if !value_type.is_record() {
            let mut dimensions = outer.to_vec();
            dimensions.extend_from_slice(value_type.dimensions());
            return Ok(Self {
                leaves: primitive_leaves(view, id, arithmetic, dimensions)?,
                field_ranges: Vec::new(),
                outer_rank: outer.len(),
            });
        }
        let mut nested = outer.to_vec();
        nested.extend_from_slice(value_type.dimensions());
        let mut leaves = Vec::new();
        let mut field_ranges = Vec::with_capacity(value_type.record_field_count());
        for ordinal in 0..value_type.record_field_count() {
            let (_, field_type) = view
                .record_field(id, ordinal)
                .ok_or_else(|| invalid_call_interface(view, id))?;
            let start = leaves.len();
            let field = Self::issue_under(view, field_type, arithmetic, &nested)?;
            leaves.extend(field.leaves);
            field_ranges.push(start..leaves.len());
        }
        Ok(Self {
            leaves,
            field_ranges,
            outer_rank: nested.len(),
        })
    }

    pub(super) fn leaves(&self) -> &[solve::SolveValueType] {
        &self.leaves
    }

    pub(super) fn into_leaves(self) -> Vec<solve::SolveValueType> {
        self.leaves
    }

    pub(super) fn len(&self) -> usize {
        self.leaves.len()
    }

    /// Leaf range field `ordinal` occupies inside this layout.
    pub(super) fn field_range<'dae>(
        &self,
        view: dae::DaeView<'dae>,
        id: dae::ValueTypeId<'dae>,
        ordinal: usize,
    ) -> Result<Range<usize>, solve::SolveProgramConstructionError> {
        self.field_ranges
            .get(ordinal)
            .cloned()
            .ok_or_else(|| invalid_call_interface(view, id))
    }

    /// Number of leading axes on every leaf that belong to the value's own
    /// array extents rather than to a packed record field.
    pub(super) const fn outer_rank(&self) -> usize {
        self.outer_rank
    }

    /// Extents leaf `ordinal` carries beyond the outer axes.
    ///
    /// These are the field's own array extents, which a projection over the
    /// outer axes has to retain in full.
    pub(super) fn trailing_dimensions(&self, ordinal: usize) -> &[u32] {
        self.leaves.get(ordinal).map_or(&[][..], |leaf| {
            &leaf.dimensions()[self.outer_rank.min(leaf.dimensions().len())..]
        })
    }
}

fn primitive_leaves<'dae>(
    view: dae::DaeView<'dae>,
    id: dae::ValueTypeId<'dae>,
    arithmetic: solve::SolveArithmeticProfile,
    dimensions: Vec<u32>,
) -> Result<Vec<solve::SolveValueType>, solve::SolveProgramConstructionError> {
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
            return Err(invalid_call_interface(view, id));
        }
    };
    if dimensions.contains(&0) {
        return Ok(Vec::new());
    }
    if dimensions.is_empty() {
        return Ok(vec![solve::SolveValueType::scalar(scalar)]);
    }
    Ok(vec![
        solve::SolveValueType::tensor(scalar, dimensions)
            .map_err(|_| invalid_call_interface(view, id))?,
    ])
}
