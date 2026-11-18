//! Non-wire execution obligations over the dependency owner's checked flow.
//!
//! Inputs are typed leaves, never scalar-expanded. Nested regions conservatively
//! cover their captures and control when they can fail or observe an assertion.
//! Presence is retained even for a constant guard: its enclosing branch choice
//! still decides whether execution encounters that guard.

use super::dependency::SolveCallDependency;
use super::{SolveOperation, SolveProgramConstructionError, SolvePureCallTableView, TypedProgram};
use crate::{FaultClass, SolveAssignedStateOperation};
use std::collections::BTreeSet;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(in crate::typed_program) struct ExecutionReads {
    inputs: Box<[usize]>,
    present: bool,
}

impl ExecutionReads {
    pub(in crate::typed_program) fn recursive() -> Self {
        // Even a value-only recursive group can exhaust its issued depth bound.
        Self {
            present: true,
            ..Self::default()
        }
    }

    pub(in crate::typed_program) fn inputs(&self) -> &[usize] {
        &self.inputs
    }

    pub(super) fn retain_recursive_obligation(&mut self) {
        self.present = true;
    }

    pub(in crate::typed_program) fn has_obligations(&self) -> bool {
        self.present
    }
}

/// Construction-only union: at most one entry per typed input leaf, even when
/// many guards read the same wide input run. Sealing is the sole conversion.
#[derive(Default)]
pub(super) struct ExecutionReadBuilder {
    inputs: BTreeSet<usize>,
    present: bool,
}

impl ExecutionReadBuilder {
    pub(super) fn finish(self) -> ExecutionReads {
        ExecutionReads {
            inputs: self.inputs.into_iter().collect(),
            present: self.present,
        }
    }

    pub(super) fn observe(
        &mut self,
        body: &TypedProgram,
        index: usize,
        registers: &[Vec<SolveCallDependency>],
        assigned: &[Vec<SolveCallDependency>],
        available: SolvePureCallTableView<'_>,
    ) -> Result<(), SolveProgramConstructionError> {
        let operation = body.operations()[index].operation();
        if let SolveOperation::Call {
            owner, arguments, ..
        } = operation
        {
            let callee = available.get(owner.index() as usize).ok_or(
                SolveProgramConstructionError::InvalidCallInterface {
                    provenance: body.operations()[index].provenance(),
                },
            )?;
            self.present |= callee.execution_reads.present;
            for &input in callee.execution_reads.inputs() {
                self.extend(&registers[arguments[input].index()]);
            }
            return Ok(());
        }
        if !has_operation_obligation(body, index, available) {
            return Ok(());
        }
        self.present = true;
        operation.visit_input_registers(|register| self.extend(&registers[register.index()]));
        match operation {
            SolveOperation::AssignedConditional { program } => {
                for state in program.state_captures() {
                    self.extend(&assigned[state.index()]);
                }
            }
            SolveOperation::Fold { state_captures, .. } => {
                for state in state_captures {
                    self.extend(&assigned[state.index()]);
                }
            }
            SolveOperation::AssignedState {
                operation: SolveAssignedStateOperation::ProjectChecked { source, .. },
            } => {
                self.extend(&assigned[source.index()]);
            }
            _ => {}
        }
        Ok(())
    }

    fn extend(&mut self, sources: &[SolveCallDependency]) {
        self.inputs
            .extend(sources.iter().flat_map(SolveCallDependency::inputs));
    }
}

fn has_obligations(body: &TypedProgram, available: SolvePureCallTableView<'_>) -> bool {
    body.operations()
        .iter()
        .enumerate()
        .any(|(index, _)| has_operation_obligation(body, index, available))
}

fn has_operation_obligation(
    body: &TypedProgram,
    index: usize,
    available: SolvePureCallTableView<'_>,
) -> bool {
    match body.operations()[index].operation() {
        SolveOperation::Call { owner, .. } => available
            .get(owner.index() as usize)
            .is_none_or(|callee| callee.execution_reads.present),
        SolveOperation::CheckAssertion { .. } => true,
        SolveOperation::AssignedConditional { program } => {
            has_obligations(program.if_true().body(), available)
                || has_obligations(program.if_false().body(), available)
        }
        SolveOperation::Conditional {
            if_true, if_false, ..
        } => {
            has_obligations(if_true.body(), available)
                || has_obligations(if_false.body(), available)
        }
        SolveOperation::Map { body, .. } => has_obligations(body.body(), available),
        SolveOperation::Fold {
            transition,
            continuation,
            ..
        } => {
            has_obligations(transition.body(), available)
                || continuation
                    .as_ref()
                    .is_some_and(|region| has_obligations(region.body(), available))
        }
        // Local stores and assigned-state bookkeeping are not observable owner
        // effects. A checked projection can fail; its state is read above.
        SolveOperation::Store { .. } => false,
        SolveOperation::AssignedState { operation } => matches!(
            operation,
            SolveAssignedStateOperation::ProjectChecked { .. }
        ),
        _ => !matches!(body.fault_class(index), FaultClass::CannotFault),
    }
}
