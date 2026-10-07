//! Numeric extrema follow `rumoca_ir_solve::real_extremum`: a single NaN is ignored.
use super::*;

impl Emitter<'_> {
    pub(super) fn real_extremum(
        &mut self,
        operator: solve::SolveBinaryOperator,
        lhs: Operand,
        rhs: Operand,
    ) {
        self.load_operand(lhs, true);
        self.push(I::LocalSet(8));
        self.load_operand(rhs, true);
        self.push(I::LocalSet(9));
        self.is_nan(8);
        self.is_nan(9);
        self.push(I::I32And);
        self.push(I::If(BlockType::Result(wasm_encoder::ValType::F64)));
        self.push(I::LocalGet(8));
        self.push(I::LocalGet(9));
        self.push(I::F64Add);
        self.push(I::Else);
        self.push(I::LocalGet(8));
        self.push(I::LocalGet(9));
        self.push(I::LocalGet(8));
        self.push(I::LocalGet(9));
        self.push(if operator == solve::SolveBinaryOperator::Min {
            I::F64Lt
        } else {
            I::F64Gt
        });
        self.is_nan(9);
        self.push(I::I32Or);
        // either equal input (including signed zeros), not portable tie bits.
        self.push(I::Select);
        self.push(I::End);
    }

    fn is_nan(&mut self, local: u32) {
        self.push(I::LocalGet(local));
        self.push(I::LocalGet(local));
        self.push(I::F64Ne);
    }
}
