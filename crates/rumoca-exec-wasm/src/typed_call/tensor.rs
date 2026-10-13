//! Source-issued tensor construction and typed scaling in private SSA storage.
use super::emit::{CELL, Emitter};
use super::numbers::Operand;
use super::{TypedCallCompileError, TypedCallFaultKind};
use rumoca_ir_solve as solve;
use wasm_encoder::Instruction as I;

impl Emitter<'_> {
    pub(super) fn tensor_operation(
        &mut self,
        index: usize,
        operation: &solve::SolveSpannedOperation,
    ) -> Result<(), TypedCallCompileError> {
        match operation.operation() {
            solve::SolveOperation::Scale {
                destination,
                aggregate,
                scalar,
            } => {
                let status = self.fault(
                    Some(index),
                    "scale",
                    TypedCallFaultKind::IntegerArithmetic,
                    operation.provenance(),
                );
                self.scale(*destination, *aggregate, *scalar, status);
            }
            solve::SolveOperation::Identity { destination } => self.identity(*destination),
            _ => unreachable!("checked tensor dispatch"),
        }
        Ok(())
    }

    fn scale(
        &mut self,
        destination: solve::SolveRegisterId,
        aggregate: solve::SolveRegisterId,
        scalar: solve::SolveRegisterId,
        status: u32,
    ) {
        let element = self.scalar(aggregate);
        let real = matches!(element, solve::SolveScalarType::Real { .. });
        let (destination, aggregate, scalar) =
            (self.reg(destination), self.reg(aggregate), self.reg(scalar));
        self.cells(destination.bytes / 8, |e| {
            e.cell_address(destination);
            e.binary(
                solve::SolveBinaryOperator::Multiply,
                element,
                (Operand::Cells(aggregate), Operand::Fixed(scalar)),
                false,
                status,
            );
            e.push(if real {
                I::F64Store(CELL)
            } else {
                I::I64Store(CELL)
            });
        });
    }

    fn identity(&mut self, destination: solve::SolveRegisterId) {
        let extent = self.program.register_types()[destination.index()].dimensions()[0];
        let one = if matches!(
            self.scalar(destination),
            solve::SolveScalarType::Real { .. }
        ) {
            1.0_f64.to_bits() as i64
        } else {
            1
        };
        let destination = self.reg(destination);
        self.cells(destination.bytes / 8, |e| {
            e.cell_address(destination);
            e.push(I::I64Const(one));
            e.push(I::I64Const(0));
            e.push(I::LocalGet(3));
            e.push(I::I32Const(extent as i32));
            e.push(I::I32DivU);
            e.push(I::LocalGet(3));
            e.push(I::I32Const(extent as i32));
            e.push(I::I32RemU);
            e.push(I::I32Eq);
            e.push(I::Select);
            e.push(I::I64Store(CELL));
        });
    }
}
