//! Resource admission before scalar frames, domain traversal and allocation.
use super::*;

/// Frame metadata and newly allocated output elements. Loads/stores and
/// carried/called results share immutable payloads instead of copying tensors.
pub(super) fn program_work(program: &TypedProgram) -> Option<usize> {
    let metadata = program
        .operations()
        .len()
        .checked_add(program.slots().len())?
        .checked_add(program.register_types().len())?;
    let mut work = Some(metadata);
    for operation in program.operations() {
        operation.operation().visit_input_registers(|_| {
            work = work.and_then(|total| total.checked_add(1));
        });
        if matches!(
            operation.operation(),
            SolveOperation::Load { .. }
                | SolveOperation::Store { .. }
                | SolveOperation::Fold { .. }
                | SolveOperation::Call { .. }
        ) {
            continue;
        }
        operation.operation().visit_output_registers(|register| {
            work = work.and_then(|total| {
                let value = program.register_types().get(register.index())?;
                // Coordinate views traverse every retained axis for each point.
                let width = value.dimensions().len().checked_add(1)?;
                total
                    .checked_add(value.dimensions().len())?
                    .checked_add((value.scalar_count() as usize).checked_mul(width)?)
            });
        });
    }
    work
}

impl Inliner<'_> {
    pub(super) fn check_domain(
        &mut self,
        domain: &rumoca_core::StructuredIndexDomain,
        body: &TypedProgram,
    ) -> Option<()> {
        let metadata = domain.binders.len().checked_mul(2)?;
        self.budget.reserve(metadata)?;
        let count = domain.scalar_count().ok()?;
        let point_work = domain.binders.len().checked_add(1)?;
        self.budget
            .check(count.checked_mul(program_work(body)?.checked_add(point_work)?)?)?;
        self.budget.reserve(count.checked_mul(point_work)?)
    }
}
