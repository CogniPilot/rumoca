//! Source-issued tensor construction and Real scaling in private SSA storage.
use super::TypedCallCompileError;
use super::emit::{CELL, Emitter};
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
                if !matches!(self.scalar(*scalar), solve::SolveScalarType::Real { .. }) {
                    return Err(self.unsupported(index, "integer scale", operation.provenance()));
                }
                self.scale(*destination, *aggregate, *scalar);
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
    ) {
        let (destination, aggregate, scalar) =
            (self.reg(destination), self.reg(aggregate), self.reg(scalar));
        self.address(scalar);
        self.push(I::F64Load(CELL));
        self.push(I::LocalSet(8));
        self.cells(destination.bytes / 8, |e| {
            e.cell_address(destination);
            e.load_cell(aggregate, true);
            e.push(I::LocalGet(8));
            e.push(I::F64Mul);
            e.push(I::F64Store(CELL));
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
