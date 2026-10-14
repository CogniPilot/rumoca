//! Canonical numeric rows leave assertion observation to its checked owner.
use super::{
    TypedCallCompileError,
    emit::{CELL, Emitter},
};
use rumoca_ir_solve as solve;
use wasm_encoder::{BlockType, Instruction as I};

impl<'a> Emitter<'a> {
    pub(super) fn project_value_assigned(
        &mut self,
        destination: solve::SolveRegisterId,
        source: solve::SolveAssignedStateId,
    ) {
        let state = self.state_cells[source.index()];
        self.address(state);
        self.push(I::I64Load(CELL));
        self.push(I::I64Eqz);
        self.push(I::If(BlockType::Empty));
        self.fill_assertion_cells(self.reg(destination), 0);
        self.push(I::Else);
        self.copy(self.reg(destination), super::assigned_state::payload(state));
        self.push(I::End);
    }

    pub(super) fn value_assertion(
        &mut self,
        index: usize,
        spanned: &'a solve::SolveSpannedOperation,
    ) -> Result<(), TypedCallCompileError> {
        let solve::SolveOperation::CheckAssertion {
            condition,
            captures,
            destinations,
            message,
            ..
        } = spanned.operation()
        else {
            return Err(self.unsupported(index, "CheckAssertion", spanned.provenance()));
        };
        self.address(self.reg(*condition));
        self.push(I::I64Load(CELL));
        self.push(I::I64Eqz);
        self.push(I::If(BlockType::Empty));
        self.assertion_failed_payload(index, captures, destinations, message)?;
        self.push(I::Else);
        self.assertion_inactive_payload(destinations)?;
        self.push(I::End);
        Ok(())
    }

    pub(super) fn assertion_failed_payload(
        &mut self,
        index: usize,
        captures: &[solve::SolveRegisterId],
        destinations: &[solve::SolveRegisterId],
        message: &'a solve::SolveAssertionMessage,
    ) -> Result<(), TypedCallCompileError> {
        let solve::SolveAssertionMessage::Captures { program } = message else {
            return Ok(());
        };
        let plan = &self.plan.regions[index][0];
        let inputs =
            super::control::interface(program.body(), plan, solve::SolveStorageClass::Input);
        let arguments = captures
            .iter()
            .map(|register| self.reg(*register))
            .collect::<Vec<_>>();
        self.copy_pairs(&inputs, &arguments);
        self.region_body(index, 0, program, plan)?;
        let outputs =
            super::control::interface(program.body(), plan, solve::SolveStorageClass::Output);
        let targets = destinations
            .iter()
            .map(|register| self.reg(*register))
            .collect::<Vec<_>>();
        self.copy_pairs(&targets, &outputs);
        Ok(())
    }

    pub(super) fn assertion_inactive_payload(
        &mut self,
        destinations: &[solve::SolveRegisterId],
    ) -> Result<(), TypedCallCompileError> {
        for destination in destinations {
            let inactive = solve::SolveValue::inactive_assertion_message(self.scalar(*destination));
            let bits = match inactive.kind() {
                solve::SolveValueKind::Real64(bits) => bits as i64,
                solve::SolveValueKind::Integer(value) => value,
                solve::SolveValueKind::Boolean(value) => i64::from(value),
                solve::SolveValueKind::Real32(_) => {
                    return Err(TypedCallCompileError::UnsupportedArithmetic);
                }
            };
            self.fill_assertion_cells(self.reg(*destination), bits);
        }
        Ok(())
    }

    fn fill_assertion_cells(&mut self, target: super::layout::CellRange, bits: i64) {
        self.cells(target.bytes / 8, |emitter| {
            emitter.cell_address(target);
            emitter.push(I::I64Const(bits));
            emitter.push(I::I64Store(CELL));
        });
    }
}
