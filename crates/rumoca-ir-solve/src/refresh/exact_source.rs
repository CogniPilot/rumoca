//! One borrowed join of an issued assignment tuple and its original program.

use super::*;

/// Exact source/tuple handover for construction. It borrows the original
/// operations, target order and provenance; it carries no scalar expansion or
/// detached structural-pattern authority.
///
/// The source cannot be changed while this view is used:
/// ```compile_fail
/// use rumoca_ir_solve::{ComputeBlock, ExactRefreshAssignmentProgram};
/// fn replace_source(owner: &ExactRefreshAssignmentProgram, source: &mut ComputeBlock) {
///     let bound = owner.bind_source(source).unwrap();
///     source.nodes.clear();
///     let _ = bound.final_scalar_program();
/// }
/// ```
#[derive(Clone, Copy, Debug)]
pub struct BoundExactRefreshAssignment<'source> {
    owner: &'source ExactRefreshAssignmentProgram,
    operations: &'source [LinearOp],
    provenance: rumoca_core::Span,
}

impl ExactRefreshAssignmentProgram {
    /// Join this assignment to its original immutable program once. An equal
    /// independent program or wire replay has another owner and is refused.
    pub fn bind_source<'source>(
        &'source self,
        source: &'source ComputeBlock,
    ) -> Result<BoundExactRefreshAssignment<'source>, ContinuousRefreshConstructionError> {
        let source_owner = scalar_source_owner(source, self.source)?;
        if source_owner.is_none_or(|owner| !owner.shares_program_owner(&self.source_owner)) {
            return refresh_error(
                "exact continuous refresh source no longer has its issued owner".to_string(),
            );
        }
        let (operations, provenance) =
            scalar_source_program(source, self.source)?.ok_or_else(|| {
                ContinuousRefreshConstructionError {
                    reason: "exact continuous refresh source program is missing".to_string(),
                }
            })?;
        Ok(BoundExactRefreshAssignment {
            owner: self,
            operations,
            provenance,
        })
    }
}

impl<'source> BoundExactRefreshAssignment<'source> {
    pub fn assignment(self) -> &'source ExactRefreshAssignmentProgram {
        self.owner
    }

    pub fn source_operations(self) -> &'source [LinearOp] {
        self.operations
    }

    pub fn provenance(self) -> rumoca_core::Span {
        self.provenance
    }

    /// Explicit scalar execution boundary using the already-bound source.
    /// Construction reuses the canonical materializer, with no second source
    /// argument or source-identity probe.
    pub fn final_scalar_program(
        self,
    ) -> Result<ScalarProgramBlock, ContinuousRefreshConstructionError> {
        materialize_exact_assignment_program(self)
    }
}

fn materialize_exact_assignment_program(
    bound: BoundExactRefreshAssignment<'_>,
) -> Result<ScalarProgramBlock, ContinuousRefreshConstructionError> {
    let owner = bound.owner;
    let source_program = bound.operations;
    let span = bound.provenance;
    let prefix_len = owner
        .assignment_shapes
        .iter()
        .map(|shape| shape.expr_eval_len())
        .max()
        .unwrap_or(0);
    let prefix =
        source_program
            .get(..prefix_len)
            .ok_or_else(|| ContinuousRefreshConstructionError {
                reason: "exact continuous refresh assignment prefix exceeds its canonical source"
                    .to_string(),
            })?;
    let mut operations = prefix
        .iter()
        .filter(|operation| {
            !matches!(
                operation,
                LinearOp::StoreOutput { .. } | LinearOp::StoreOutputRange { .. }
            )
        })
        .cloned()
        .collect::<Vec<_>>();
    let overflow = || ContinuousRefreshConstructionError {
        reason: "exact continuous refresh assignment program overflows registers".to_string(),
    };
    let mut program = ExactAssignmentProgramBuilder::new(&mut operations).ok_or_else(overflow)?;
    for store in &owner.assignment_stores {
        match *store {
            ExactAssignmentStore::Scalar(index) => {
                let (src, _) = program
                    .materialize_guarded(&owner.assignment_shapes[index])
                    .ok_or_else(overflow)?;
                program
                    .push(LinearOp::StoreOutput { src })
                    .ok_or_else(overflow)?;
            }
            ExactAssignmentStore::Range {
                start,
                stride,
                targets,
            } => program
                .push_range_output(start, targets.count(), stride)
                .ok_or_else(overflow)?,
        }
    }
    let provenance = rumoca_core::ProvenanceSpan::new(span, "continuous refresh assignment")
        .map_err(|error| ContinuousRefreshConstructionError {
            reason: error.to_string(),
        })?;
    ScalarProgramBlock::with_source_span(vec![operations], provenance).map_err(|error| {
        ContinuousRefreshConstructionError {
            reason: error.to_string(),
        }
    })
}

impl ContinuousRefreshOwners {
    /// Retain one original source join per issued assignment, independently of
    /// how many schedules refer to it. This is a construction-only handover.
    pub(crate) fn exact_assignment_programs_for_source<'source>(
        &'source self,
        source: &'source ComputeBlock,
    ) -> Result<Box<[BoundExactRefreshAssignment<'source>]>, ContinuousRefreshConstructionError>
    {
        self.exact_assignment_programs
            .iter()
            .map(|owner| owner.bind_source(source))
            .collect()
    }
}
