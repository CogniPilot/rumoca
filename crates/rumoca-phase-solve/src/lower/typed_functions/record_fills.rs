//! Comprehensions over one constant record.

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::{ExpressionLowerer, LoweredValue, arithmetic_profile, lower_value_type_leaves};

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
    /// A comprehension whose body is one record that no binder selects: every
    /// element is the same record, so each field leaf of the record is
    /// repeated over the comprehension's dimensions (MLS 3.7 section 10.4.1).
    /// A scalar leaf is filled; a leaf with dimensions of its own is mapped
    /// over the domain, giving the comprehension's dimensions followed by its
    /// own, like any array-valued comprehension body.
    pub(super) fn filled_record_comprehension(
        &mut self,
        (value_type, body_type): (dae::ValueTypeId<'dae>, dae::ValueTypeId<'dae>),
        domain: rumoca_core::StructuredIndexDomain,
        body: LoweredValue<'program, 'dae>,
        at: rumoca_core::Span,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        let leaf_types = lower_value_type_leaves(self.view, body_type, arithmetic_profile())?;
        if leaf_types.len() != body.leaves.len() {
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
        let mut leaves = Vec::with_capacity(body.leaves.len());
        for (leaf, leaf_type) in body.leaves.iter().zip(leaf_types) {
            leaves.push(if leaf_type.dimensions().is_empty() {
                self.builder.fill(*leaf, dimensions.clone(), at)?
            } else {
                self.builder.map(
                    domain.clone(),
                    &[*leaf],
                    leaf_type,
                    at,
                    |builder, captures, _, output| {
                        let value = builder.load(captures[0], at)?;
                        builder.store(output, value, at)
                    },
                )?
            });
        }
        Ok(LoweredValue { value_type, leaves })
    }
}
