//! Exact MLS Integer quotients. Locals 5/6 are inputs; local 7 owns the result.
use super::{Emitter, I, solve};
use wasm_encoder::BlockType;

impl Emitter<'_> {
    pub(super) fn integer_quotient(&mut self, operator: solve::SolveBinaryOperator, status: u32) {
        use solve::SolveBinaryOperator as B;
        self.push(I::LocalGet(6));
        self.push(I::I64Eqz);
        self.fail_if(status);
        if operator == B::IntegerQuotient {
            self.push(I::LocalGet(5));
            self.push(I::I64Const(i64::MIN));
            self.push(I::I64Eq);
            self.push(I::LocalGet(6));
            self.push(I::I64Const(-1));
            self.push(I::I64Eq);
            self.push(I::I32And);
            self.fail_if(status);
        }
        self.push(I::LocalGet(5));
        self.push(I::LocalGet(6));
        self.push(if operator == B::IntegerQuotient {
            I::I64DivS
        } else {
            I::I64RemS
        });
        self.push(I::LocalSet(7));
        if operator == B::IntegerModulo {
            // Nonzero truncated remainder with a different sign needs one divisor.
            // The adjusted remainder is strictly within the divisor's magnitude.
            self.push(I::LocalGet(7));
            self.push(I::I64Eqz);
            self.push(I::I32Eqz);
            self.push(I::LocalGet(7));
            self.push(I::LocalGet(6));
            self.push(I::I64Xor);
            self.push(I::I64Const(0));
            self.push(I::I64LtS);
            self.push(I::I32And);
            self.push(I::If(BlockType::Empty));
            self.push(I::LocalGet(7));
            self.push(I::LocalGet(6));
            self.push(I::I64Add);
            self.push(I::LocalSet(7));
            self.push(I::End);
        }
    }
}
