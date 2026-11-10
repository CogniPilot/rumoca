//! Address-free scalar homes issued after alias and fusion ownership is known.
//!
//! A unique scalar definition can stay a value only when its entire live
//! interval contains scalar steps, no deferred reader, and no alias member.
//! Every other definition keeps its existing addressable home. Prefix counts
//! check the intervals in constant time; construction is linear in the source.

use super::{
    SolveFusionPlan, SolveOperation, SolveRegisterHome, SolveSpannedOperation, SolveValueType,
};

/// A source definition and its final reader, proved to need no scratch cell.
/// Only the storage constructor can issue this capability for a resolved home.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SolveScalarLifetime {
    definition: usize,
    last_read: usize,
}

impl SolveScalarLifetime {
    #[must_use]
    pub const fn definition(&self) -> usize {
        self.definition
    }

    #[must_use]
    pub const fn last_read(&self) -> usize {
        self.last_read
    }
}

pub(super) fn issue(
    types: &[SolveValueType],
    operations: &[SolveSpannedOperation],
    fusion: &SolveFusionPlan,
    homes: &mut [SolveRegisterHome],
) {
    let mut writes = vec![0_usize; types.len()];
    let mut barriers = Vec::with_capacity(operations.len() + 1);
    barriers.push(0_usize);
    for issued in operations {
        issued
            .operation()
            .visit_output_registers(|r| writes[r.index()] += 1);
        barriers.push(
            barriers[barriers.len() - 1]
                + usize::from(!scalar_step(issued.operation(), types, fusion)),
        );
    }
    let members = alias_members(homes);
    for (definition, issued) in operations.iter().enumerate() {
        issued.operation().visit_output_registers(|r| {
            let r = r.index();
            let last_read = fusion.last_reads()[r].unwrap_or(definition);
            if matches!(homes[r], SolveRegisterHome::Own)
                && writes[r] == 1
                && members[r] == 1
                && types[r].dimensions().is_empty()
                && types[r].scalar_count() == 1
                && fusion.registers()[r].is_none()
                && last_read >= definition
                && barriers.get(last_read + 1) == Some(&barriers[definition])
            {
                homes[r] = SolveRegisterHome::Scalar {
                    lifetime: SolveScalarLifetime {
                        definition,
                        last_read,
                    },
                };
            }
        });
    }
}

fn alias_members(homes: &[SolveRegisterHome]) -> Vec<usize> {
    let mut members = vec![0_usize; homes.len()];
    for (r, home) in homes.iter().enumerate() {
        match home {
            SolveRegisterHome::Own => members[r] += 1,
            SolveRegisterHome::Register { register } => members[*register] += 1,
            SolveRegisterHome::Scalar { .. }
            | SolveRegisterHome::InputSlot { .. }
            | SolveRegisterHome::OutputSlot { .. } => {}
        }
    }
    members
}

fn scalar_step(
    operation: &SolveOperation,
    types: &[SolveValueType],
    fusion: &SolveFusionPlan,
) -> bool {
    use SolveOperation as O;
    let mut scalar = matches!(
        operation,
        O::Load { .. }
            | O::Constant { .. }
            | O::Unary { .. }
            | O::Binary { .. }
            | O::Compare { .. }
            | O::Convert { .. }
            | O::Select { .. }
            | O::CheckFinite { .. }
    );
    let mut check = |r: crate::SolveRegisterId| {
        scalar &= types[r.index()].dimensions().is_empty()
            && types[r.index()].scalar_count() == 1
            && fusion.registers()[r.index()].is_none();
    };
    operation.visit_input_registers(&mut check);
    operation.visit_output_registers(check);
    scalar
}
