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
        domain: &rumoca_core::StructuredIndexDomain,
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
            leaves.push(self.repeat_leaf(*leaf, leaf_type, (domain, &dimensions), at)?);
        }
        Ok(LoweredValue { value_type, leaves })
    }

    /// One field leaf repeated over the comprehension: a scalar is filled over
    /// the dimensions, an array leaf is mapped over the domain.
    fn repeat_leaf(
        &mut self,
        leaf: solve::ProgramRegister<'program>,
        leaf_type: solve::SolveValueType,
        (domain, dimensions): (&rumoca_core::StructuredIndexDomain, &[u32]),
        at: rumoca_core::Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        if leaf_type.dimensions().is_empty() {
            return self.builder.fill(leaf, dimensions.to_vec(), at);
        }
        map_copy(self.builder, domain.clone(), (leaf, leaf_type), at)
    }
}

/// A map over `domain` whose every element is the one captured `leaf`.
fn map_copy<'program>(
    builder: &mut solve::TypedProgramBuilder<'program>,
    domain: rumoca_core::StructuredIndexDomain,
    (leaf, leaf_type): (solve::ProgramRegister<'program>, solve::SolveValueType),
    at: rumoca_core::Span,
) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
    builder.map(
        domain,
        &[leaf],
        leaf_type,
        at,
        |inner, captures, _, output| {
            let value = inner.load(captures[0], at)?;
            inner.store(output, value, at)
        },
    )
}
