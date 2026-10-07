//! Comprehensions over one constant record.

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::{ExpressionLowerer, LoweredValue, arithmetic_profile, lower_value_type_leaves};

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
    /// A comprehension whose body is one record that no binder selects: every
    /// element is the same record, so each scalar field leaf of the record is
    /// filled over the comprehension's dimensions (MLS 3.7 section 10.4.1).
    pub(super) fn filled_record_comprehension(
        &mut self,
        value_type: dae::ValueTypeId<'dae>,
        body_type: dae::ValueTypeId<'dae>,
        body: LoweredValue<'program, 'dae>,
        at: rumoca_core::Span,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        let leaf_types = lower_value_type_leaves(self.view, body_type, arithmetic_profile())?;
        let scalar_fields = leaf_types.len() == body.leaves.len()
            && leaf_types.iter().all(|leaf| leaf.dimensions().is_empty());
        if !scalar_fields {
            return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                provenance: at,
            });
        }
        let dimensions = self
            .view
            .value_type(value_type)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
            .dimensions()
            .to_vec();
        let leaves = body
            .leaves
            .iter()
            .map(|leaf| self.builder.fill(*leaf, dimensions.clone(), at))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(LoweredValue { value_type, leaves })
    }
}
