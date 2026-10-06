//! Ordered Real dot products from the original checked tensor operation.
use super::TypedCallCompileError;
use super::emit::{CELL, Emitter};
use super::layout::CellRange;
use rumoca_ir_solve as solve;
use wasm_encoder::{BlockType, Instruction as I};

impl Emitter<'_> {
    pub(super) fn matrix_operation(
        &mut self,
        index: usize,
        operation: &solve::SolveSpannedOperation,
    ) -> Result<(), TypedCallCompileError> {
        let solve::SolveOperation::MatrixMultiply {
            destination,
            lhs,
            rhs,
        } = *operation.operation()
        else {
            unreachable!("checked matrix dispatch")
        };
        if !matches!(self.scalar(lhs), solve::SolveScalarType::Real { .. }) {
            return Err(self.unsupported(index, "integer matrix multiply", operation.provenance()));
        }
        let left_shape = self.program.register_types()[lhs.index()].dimensions();
        let right_shape = self.program.register_types()[rhs.index()].dimensions();
        let inner = *left_shape.last().expect("checked matrix rank");
        let columns = if right_shape.len() == 1 {
            1
        } else {
            right_shape[1]
        };
        let left = self.reg(lhs);
        let right = self.reg(rhs);
        let output = self.reg(destination);
        self.cells(output.bytes / 8, |e| {
            // The canonical evaluator initializes from the first product, not +0.
            e.push(I::I64Const(0));
            e.push(I::LocalSet(4));
            e.matrix_term(left, right, inner, columns);
            e.push(I::LocalSet(8));
            e.push(I::I64Const(1));
            e.push(I::LocalSet(4));
            e.push(I::Block(BlockType::Empty));
            e.push(I::Loop(BlockType::Empty));
            e.push(I::LocalGet(4));
            e.push(I::I64Const(i64::from(inner)));
            e.push(I::I64GeU);
            e.push(I::BrIf(1));
            e.push(I::LocalGet(8));
            e.matrix_term(left, right, inner, columns);
            e.push(I::F64Add);
            e.push(I::LocalSet(8));
            e.push(I::LocalGet(4));
            e.push(I::I64Const(1));
            e.push(I::I64Add);
            e.push(I::LocalSet(4));
            e.push(I::Br(0));
            e.push(I::End);
            e.push(I::End);
            e.cell_address(output);
            e.push(I::LocalGet(8));
            e.push(I::F64Store(CELL));
        });
        Ok(())
    }

    fn matrix_term(&mut self, left: CellRange, right: CellRange, inner: u32, columns: u32) {
        self.address(left);
        self.push(I::LocalGet(3));
        self.push(I::I32Const(columns as i32));
        self.push(I::I32DivU);
        self.push(I::I32Const(inner as i32));
        self.push(I::I32Mul);
        self.push(I::LocalGet(4));
        self.push(I::I32WrapI64);
        self.push(I::I32Add);
        self.push(I::I32Const(8));
        self.push(I::I32Mul);
        self.push(I::I32Add);
        self.push(I::F64Load(CELL));
        self.address(right);
        self.push(I::LocalGet(4));
        self.push(I::I32WrapI64);
        self.push(I::I32Const(columns as i32));
        self.push(I::I32Mul);
        self.push(I::LocalGet(3));
        self.push(I::I32Const(columns as i32));
        self.push(I::I32RemU);
        self.push(I::I32Add);
        self.push(I::I32Const(8));
        self.push(I::I32Mul);
        self.push(I::I32Add);
        self.push(I::F64Load(CELL));
        self.push(I::F64Mul);
    }
}
