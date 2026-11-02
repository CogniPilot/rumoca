//! Attribute emission to the original program/operation before dispatch. Fused
//! producers remain visible with zero emitted instructions; their consumer
//! owns the actual code. Canonical retained programs preserve their operands.

use super::{CompileError, ProgramLowerer};
use rumoca_ir_solve as solve;

impl ProgramLowerer<'_, '_> {
    pub(super) fn lower_recorded(
        &mut self,
        operation: &solve::SolveSpannedOperation,
        (program, ordinal): (Option<usize>, usize),
        original: &solve::TypedProgram,
    ) -> Result<(), CompileError> {
        if program.is_none() {
            return self.lower_operation(operation.operation());
        }
        let opened = program.and_then(|program| {
            crate::expansion::open(
                crate::ExpansionSource::Typed {
                    program,
                    operation: ordinal,
                },
                operation_kind(operation.operation()),
                Some(operation.provenance()),
                self.defined_scalars(operation.operation()),
                (
                    std::ptr::from_ref(&*self.builder.func) as usize,
                    self.builder.func.dfg.num_insts(),
                ),
            )
        });
        crate::expansion::phase(if self.emission == super::Emission::Probe {
            "admission-probe"
        } else {
            "definition"
        });
        if program.is_some() {
            crate::expansion::fused_operations(
                self.fused_source_operations(operation.operation(), original),
            );
        }
        let lowered = self.lower_operation(operation.operation());
        crate::expansion::finish(opened, self.builder.func.dfg.num_insts(), lowered.is_ok());
        lowered
    }

    /// Follow only the existing fusion plan's register edges. No operation is
    /// newly classified or moved by this diagnostic walk.
    fn fused_source_operations(
        &self,
        operation: &solve::SolveOperation,
        original: &solve::TypedProgram,
    ) -> Vec<usize> {
        let mut pending = Vec::new();
        operation.visit_input_registers(|register| pending.push(register));
        let mut seen = std::collections::BTreeSet::new();
        let mut members = std::collections::BTreeSet::new();
        while let Some(register) = pending.pop() {
            if !seen.insert(register.index()) {
                continue;
            }
            let Some(Some(source)) = self.layout.fused.get(register.index()) else {
                continue;
            };
            if let Some(ordinal) = original
                .operations()
                .iter()
                .position(|operation| operation.operation() == source)
            {
                members.insert(ordinal);
            }
            source.visit_input_registers(|register| pending.push(register));
        }
        members.into_iter().collect()
    }

    fn defined_scalars(&self, operation: &solve::SolveOperation) -> u64 {
        let mut elements = 0;
        operation.visit_output_registers(|register| {
            if let Ok(location) = self.register(register) {
                elements += u64::from(location.value_type.scalar_count());
            }
        });
        elements
    }
}

fn operation_kind(operation: &solve::SolveOperation) -> &'static str {
    match operation {
        solve::SolveOperation::AssignedConditional { .. } => "AssignedConditional",
        solve::SolveOperation::AssignedState { .. } => "AssignedState",
        solve::SolveOperation::CheckAssertion { .. } => "CheckAssertion",
        solve::SolveOperation::Constant { .. } => "Constant",
        solve::SolveOperation::Load { .. } => "Load",
        solve::SolveOperation::Store { .. } => "Store",
        solve::SolveOperation::Unary { .. } => "Unary",
        solve::SolveOperation::Binary { .. } => "Binary",
        solve::SolveOperation::Compare { .. } => "Compare",
        solve::SolveOperation::Convert { .. } => "Convert",
        solve::SolveOperation::Select { .. } => "Select",
        solve::SolveOperation::Conditional { .. } => "Conditional",
        solve::SolveOperation::Map { .. } => "Map",
        solve::SolveOperation::Fold { .. } => "Fold",
        solve::SolveOperation::Scale { .. } => "Scale",
        solve::SolveOperation::BroadcastBinary { .. } => "BroadcastBinary",
        solve::SolveOperation::Transpose { .. } => "Transpose",
        solve::SolveOperation::MatrixMultiply { .. } => "MatrixMultiply",
        solve::SolveOperation::Cross { .. } => "Cross",
        solve::SolveOperation::Reduce { .. } => "Reduce",
        solve::SolveOperation::Identity { .. } => "Identity",
        solve::SolveOperation::Diagonal { .. } => "Diagonal",
        solve::SolveOperation::Concatenate { .. } => "Concatenate",
        solve::SolveOperation::Fill { .. } => "Fill",
        solve::SolveOperation::ConstructAggregate { .. } => "ConstructAggregate",
        solve::SolveOperation::ProjectElement { .. } => "ProjectElement",
        solve::SolveOperation::ProjectElementDynamic { .. } => "ProjectElementDynamic",
        solve::SolveOperation::ProjectSlice { .. } => "ProjectSlice",
        solve::SolveOperation::ProjectView { .. } => "ProjectView",
        solve::SolveOperation::SelectElement { .. } => "SelectElement",
        solve::SolveOperation::UpdateElement { .. } => "UpdateElement",
        solve::SolveOperation::UpdateSlice { .. } => "UpdateSlice",
        solve::SolveOperation::UpdateView { .. } => "UpdateView",
        solve::SolveOperation::Call { .. } => "Call",
        solve::SolveOperation::LinearSolve { .. } => "LinearSolve",
        solve::SolveOperation::Native { .. } => "Native",
    }
}
