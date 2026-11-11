//! Ordered Real dot products from the original checked tensor operation.
use super::TypedCallCompileError;
use super::cells::Index;
use super::emit::{CELL, Emitter};
use super::layout::CellRange;
use rumoca_ir_solve as solve;
use wasm_encoder::Instruction as I;

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
        let rows = output.bytes / 8 / columns;
        // Output axes 0 (row) and 1 (column); the summed axis is axis 2.
        self.cells_shaped(&[rows, columns], |e| {
            // The canonical evaluator initializes from the first product, not +0.
            e.matrix_term(left, right, (inner, columns), Index::At(0));
            e.push(I::LocalSet(8));
            e.cells_from(1, inner, |e| {
                e.push(I::LocalGet(8));
                let term = e.axis(2);
                e.matrix_term(left, right, (inner, columns), term);
                e.push(I::F64Add);
                e.push(I::LocalSet(8));
            });
            e.cell_address(output);
            e.push(I::LocalGet(8));
            e.push(I::F64Store(CELL));
        });
        Ok(())
    }

    /// Push `left[row, term] * right[term, column]`.
    fn matrix_term(
        &mut self,
        left: CellRange,
        right: CellRange,
        (inner, columns): (u32, u32),
        term: Index,
    ) {
        let (row, column) = (self.axis(0), self.axis(1));
        self.cell_at(left, 0, &[(row, inner), (term, 1)]);
        self.push(I::F64Load(CELL));
        self.cell_at(right, 0, &[(term, columns), (column, 1)]);
        self.push(I::F64Load(CELL));
        self.push(I::F64Mul);
    }
}
