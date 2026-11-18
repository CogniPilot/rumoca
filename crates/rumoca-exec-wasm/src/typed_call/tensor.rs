//! Source-issued tensor construction and typed scaling in private SSA storage.
use super::cells::Index;
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

    /// Push the cell value `on` pushes on the diagonal of a square matrix
    /// iteration (axes 0 and 1) and zero bits elsewhere.
    pub(super) fn diagonal_select(&mut self, on: impl FnOnce(&mut Self)) {
        match (self.axis(0), self.axis(1)) {
            (Index::At(row), Index::At(column)) if row == column => on(self),
            (Index::At(_), Index::At(_)) => self.push(I::I64Const(0)),
            (row, column) => {
                on(self);
                self.push(I::I64Const(0));
                self.push_index(row);
                self.push_index(column);
                self.push(I::I32Eq);
                self.push(I::Select);
            }
        }
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
        self.cells_shaped(&[extent, extent], |e| {
            e.cell_address(destination);
            e.diagonal_select(|e| e.push(I::I64Const(one)));
            e.push(I::I64Store(CELL));
        });
    }
}
