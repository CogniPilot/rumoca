//! Compact internal assigned-state transport; payload reads require checked presence.

use super::TypedCallCompileError;
use super::control::interface;
use super::emit::{CELL, Emitter};
use super::layout::CellRange;
use rumoca_ir_solve as solve;
use wasm_encoder::{BlockType, Instruction as I};

impl<'a> Emitter<'a> {
    pub(super) fn assigned_read_check(
        &mut self,
        index: usize,
        operation: &solve::SolveSpannedOperation,
    ) -> Result<(), TypedCallCompileError> {
        let solve::SolveOperation::CheckAssertion {
            predicate_output,
            condition,
            captures,
            destinations,
            message: solve::SolveAssertionMessage::NoCaptures,
            ..
        } = operation.operation()
        else {
            return Err(self.unsupported(index, "CheckAssertion", operation.provenance()));
        };
        if !captures.is_empty()
            || !destinations.is_empty()
            || !self
                .owner
                .assertion_flow()
                .is_some_and(|flow| flow.assigned_read_predicates().contains(predicate_output))
        {
            return Err(self.unsupported(index, "CheckAssertion", operation.provenance()));
        }
        let status = self.fault(
            Some(index),
            "CheckAssertion",
            super::TypedCallFaultKind::AssertionFailed,
            operation.provenance(),
        );
        self.address(self.reg(*condition));
        self.push(I::I64Load(CELL));
        self.push(I::I64Eqz);
        self.push(I::If(BlockType::Empty));
        self.push(I::I32Const(status as i32));
        self.push(I::Return);
        self.push(I::End);
        Ok(())
    }

    pub(super) fn assigned_operation(
        &mut self,
        operation: &solve::SolveAssignedStateOperation,
    ) -> Result<(), TypedCallCompileError> {
        use solve::SolveAssignedStateOperation as O;
        match operation {
            O::Absent { destination, .. } => {
                self.store_integer(self.state_cells[destination.index()], 0);
            }
            O::Present {
                destination,
                source,
            } => {
                let state = self.state_cells[destination.index()];
                self.copy(payload(state), self.reg(*source));
                self.store_integer(state, 1);
            }
            O::Input { destination, input } => {
                // The input cell is written before the region runs and never
                // while it does, so the state reads it where it is.
                self.state_cells[destination.index()] = self.plan.assigned_inputs[*input];
            }
            O::Output { output, source } => {
                self.copy_assigned_state(
                    self.plan.assigned_outputs[*output],
                    self.state_cells[source.index()],
                );
            }
            O::IsPresent {
                destination,
                source,
            } => {
                self.address(self.reg(*destination));
                self.address(self.state_cells[source.index()]);
                self.push(I::I64Load(CELL));
                self.push(I::I64Store(CELL));
            }
            O::AllPresent {
                destination,
                sources,
            } => {
                self.address(self.reg(*destination));
                self.push(I::I64Const(1));
                for source in sources {
                    self.address(self.state_cells[source.index()]);
                    self.push(I::I64Load(CELL));
                    self.push(I::I64And);
                }
                self.push(I::I64Store(CELL));
            }
            O::ProjectChecked {
                destination,
                source,
                ..
            } => {
                if self.linked.assertion_mode == super::link::AssertionMode::Value {
                    self.project_value_assigned(*destination, *source);
                    return Ok(());
                }
                // The canonical constructor requires the preceding error-level
                // presence check. Its effect ABI must be admitted before this body.
                self.copy(
                    self.reg(*destination),
                    payload(self.state_cells[source.index()]),
                );
            }
        }
        Ok(())
    }

    pub(super) fn copy_assigned_state(&mut self, destination: CellRange, source: CellRange) {
        self.address(source);
        self.push(I::I64Load(CELL));
        self.push(I::I64Eqz);
        self.push(I::If(BlockType::Empty));
        self.store_integer(destination, 0);
        self.push(I::Else);
        self.copy(payload(destination), payload(source));
        self.store_integer(destination, 1);
        self.push(I::End);
    }

    pub(super) fn assigned_conditional(
        &mut self,
        index: usize,
        conditional: &'a solve::SolveAssignedConditional,
    ) -> Result<(), TypedCallCompileError> {
        self.address(self.reg(conditional.condition()));
        self.push(I::I64Load(CELL));
        self.push(I::I64Eqz);
        self.push(I::I32Eqz);
        self.push(I::If(BlockType::Empty));
        self.assigned_branch(index, 0, conditional, conditional.if_true())?;
        self.push(I::Else);
        self.assigned_branch(index, 1, conditional, conditional.if_false())?;
        self.push(I::End);
        Ok(())
    }

    fn assigned_branch(
        &mut self,
        index: usize,
        arm: usize,
        conditional: &solve::SolveAssignedConditional,
        region: &'a solve::SolveProgramRegion,
    ) -> Result<(), TypedCallCompileError> {
        let plan = &self.plan.regions[index][arm];
        let inputs = interface(region.body(), plan, solve::SolveStorageClass::Input);
        let sources = conditional
            .captures()
            .iter()
            .map(|r| self.reg(*r))
            .collect::<Vec<_>>();
        self.copy_pairs(&inputs, &sources);
        let states = conditional
            .state_captures()
            .iter()
            .map(|state| self.state_cells[state.index()])
            .collect::<Vec<_>>();
        self.copy_read_state_inputs(region.body(), plan, &states);
        self.region_body(index, arm, region, plan)?;
        let outputs = interface(region.body(), plan, solve::SolveStorageClass::Output);
        self.publish_results(index, arm, conditional.destinations(), outputs);
        let state_targets = conditional
            .state_destinations()
            .iter()
            .map(|state| self.state_cells[state.index()])
            .collect::<Vec<_>>();
        for (destination, source) in state_targets.iter().zip(&plan.assigned_outputs) {
            self.copy_assigned_state(*destination, *source);
        }
        Ok(())
    }
}

pub(super) fn payload(state: CellRange) -> CellRange {
    CellRange {
        offset: state.offset + 8,
        bytes: state.bytes - 8,
        ..state
    }
}

/// Whether an operation of `program` reads each assigned-state input: as a
/// presence or payload, as a forwarded output, or as a capture of a nested
/// region.
fn read_state_inputs(program: &solve::TypedProgram) -> Vec<bool> {
    use solve::SolveAssignedStateOperation as S;
    let mut input_of = vec![None; program.assigned_state_types().len()];
    let mut read = vec![false; program.assigned_state_inputs().len()];
    for operation in program.operations() {
        if let solve::SolveOperation::AssignedState {
            operation: S::Input { destination, input },
        } = operation.operation()
        {
            input_of[destination.index()] = Some(*input);
        }
    }
    let mut mark = |state: &solve::SolveAssignedStateId| {
        if let Some(input) = input_of[state.index()] {
            read[input] = true;
        }
    };
    for operation in program.operations() {
        match operation.operation() {
            solve::SolveOperation::AssignedState { operation } => match operation {
                S::Output { source, .. }
                | S::IsPresent { source, .. }
                | S::ProjectChecked { source, .. } => mark(source),
                S::AllPresent { sources, .. } => sources.iter().for_each(&mut mark),
                S::Absent { .. } | S::Present { .. } | S::Input { .. } => {}
            },
            solve::SolveOperation::AssignedConditional { program } => {
                program.state_captures().iter().for_each(&mut mark);
            }
            solve::SolveOperation::Fold { state_captures, .. } => {
                state_captures.iter().for_each(&mut mark);
            }
            _ => {}
        }
    }
    read
}

impl Emitter<'_> {
    /// Copy the states `region` reads into its input cells; an input no
    /// operation of the region reads is never copied.
    pub(super) fn copy_read_state_inputs(
        &mut self,
        region: &solve::TypedProgram,
        plan: &super::layout::FramePlan,
        sources: &[CellRange],
    ) {
        let read = read_state_inputs(region);
        for ((destination, source), read) in plan.assigned_inputs.iter().zip(sources).zip(read) {
            if read {
                self.copy_assigned_state(*destination, *source);
            }
        }
    }
}
