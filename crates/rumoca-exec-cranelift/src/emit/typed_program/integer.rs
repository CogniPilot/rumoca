//! Checked Integer arithmetic at the native emission boundary.

use super::{CompileError, ProgramLowerer, status};
use cranelift_codegen::ir::{InstBuilder, Value, condcodes::IntCC, types};
use rumoca_ir_solve as solve;

impl ProgramLowerer<'_, '_> {
    pub(super) fn integer_unary(
        &mut self,
        operator: solve::SolveUnaryOperator,
        domain: solve::SolveIntegerDomain,
        value: Value,
    ) -> Result<Value, CompileError> {
        let result = match operator {
            solve::SolveUnaryOperator::Negate => {
                let zero = self.builder.ins().iconst(types::I64, 0);
                let (result, overflow) = self.builder.ins().ssub_overflow(zero, value);
                self.require_no_integer_overflow(overflow);
                result
            }
            solve::SolveUnaryOperator::Abs => {
                let valid = self
                    .builder
                    .ins()
                    .icmp_imm(IntCC::NotEqual, value, i64::MIN);
                status::require_integer_arithmetic(self.builder, valid);
                let negative = self.builder.ins().icmp_imm(IntCC::SignedLessThan, value, 0);
                let negated = self.builder.ins().ineg(value);
                self.builder.ins().select(negative, negated, value)
            }
            solve::SolveUnaryOperator::Sign => {
                let zero = self.builder.ins().iconst(types::I64, 0);
                let one = self.builder.ins().iconst(types::I64, 1);
                let minus_one = self.builder.ins().iconst(types::I64, -1);
                let positive = self
                    .builder
                    .ins()
                    .icmp_imm(IntCC::SignedGreaterThan, value, 0);
                let negative = self.builder.ins().icmp_imm(IntCC::SignedLessThan, value, 0);
                let nonpositive = self.builder.ins().select(negative, minus_one, zero);
                self.builder.ins().select(positive, one, nonpositive)
            }
            _ => {
                return Err(CompileError::Backend(
                    "invalid typed Integer unary operator".into(),
                ));
            }
        };
        self.require_integer_domain(result, domain);
        Ok(result)
    }

    pub(super) fn integer_binary(
        &mut self,
        operator: solve::SolveBinaryOperator,
        domain: solve::SolveIntegerDomain,
        lhs: Value,
        rhs: Value,
    ) -> Result<Value, CompileError> {
        let result = match operator {
            solve::SolveBinaryOperator::Add
            | solve::SolveBinaryOperator::Subtract
            | solve::SolveBinaryOperator::Multiply => {
                let (result, overflow) = match operator {
                    solve::SolveBinaryOperator::Add => self.builder.ins().sadd_overflow(lhs, rhs),
                    solve::SolveBinaryOperator::Subtract => {
                        self.builder.ins().ssub_overflow(lhs, rhs)
                    }
                    _ => self.builder.ins().smul_overflow(lhs, rhs),
                };
                self.require_no_integer_overflow(overflow);
                result
            }
            solve::SolveBinaryOperator::IntegerQuotient => {
                let nonzero = self.builder.ins().icmp_imm(IntCC::NotEqual, rhs, 0);
                let minus_one = self.builder.ins().icmp_imm(IntCC::Equal, rhs, -1);
                let minimum = self.builder.ins().icmp_imm(IntCC::Equal, lhs, i64::MIN);
                let overflow = self.builder.ins().band(minus_one, minimum);
                let representable = self.builder.ins().icmp_imm(IntCC::Equal, overflow, 0);
                let valid = self.builder.ins().band(nonzero, representable);
                status::require_integer_quotient(self.builder, valid);
                self.builder.ins().sdiv(lhs, rhs)
            }
            solve::SolveBinaryOperator::IntegerRemainder => self.integer_remainder(lhs, rhs),
            solve::SolveBinaryOperator::IntegerModulo => {
                let remainder = self.integer_remainder(lhs, rhs);
                self.integer_modulo(remainder, rhs)
            }
            solve::SolveBinaryOperator::Min | solve::SolveBinaryOperator::Max => {
                let code = if operator == solve::SolveBinaryOperator::Min {
                    IntCC::SignedLessThan
                } else {
                    IntCC::SignedGreaterThan
                };
                let condition = self.builder.ins().icmp(code, lhs, rhs);
                self.builder.ins().select(condition, lhs, rhs)
            }
            _ => {
                return Err(CompileError::Backend(
                    "typed Integer binary operator is unsupported".into(),
                ));
            }
        };
        self.require_integer_domain(result, domain);
        Ok(result)
    }

    fn integer_remainder(&mut self, lhs: Value, rhs: Value) -> Value {
        let nonzero = self.builder.ins().icmp_imm(IntCC::NotEqual, rhs, 0);
        status::require_integer_arithmetic(self.builder, nonzero);
        // The exact remainder is zero for divisor -1, including MIN/-1.
        // Use divisor 1 so the machine remainder never traps on that pair.
        let minus_one = self.builder.ins().icmp_imm(IntCC::Equal, rhs, -1);
        let one = self.builder.ins().iconst(types::I64, 1);
        let divisor = self.builder.ins().select(minus_one, one, rhs);
        self.builder.ins().srem(lhs, divisor)
    }

    fn integer_modulo(&mut self, remainder: Value, rhs: Value) -> Value {
        let nonzero = self.builder.ins().icmp_imm(IntCC::NotEqual, remainder, 0);
        let negative_remainder = self
            .builder
            .ins()
            .icmp_imm(IntCC::SignedLessThan, remainder, 0);
        let negative_divisor = self.builder.ins().icmp_imm(IntCC::SignedLessThan, rhs, 0);
        let opposite_signs = self
            .builder
            .ins()
            .bxor(negative_remainder, negative_divisor);
        let adjust = self.builder.ins().band(nonzero, opposite_signs);
        let zero = self.builder.ins().iconst(types::I64, 0);
        let adjustment = self.builder.ins().select(adjust, rhs, zero);
        let (result, overflow) = self.builder.ins().sadd_overflow(remainder, adjustment);
        self.require_no_integer_overflow(overflow);
        result
    }

    fn require_no_integer_overflow(&mut self, overflow: Value) {
        let valid = self.builder.ins().icmp_imm(IntCC::Equal, overflow, 0);
        status::require_integer_arithmetic(self.builder, valid);
    }

    fn require_integer_domain(&mut self, value: Value, domain: solve::SolveIntegerDomain) {
        let minimum =
            self.builder
                .ins()
                .icmp_imm(IntCC::SignedGreaterThanOrEqual, value, domain.minimum());
        let maximum =
            self.builder
                .ins()
                .icmp_imm(IntCC::SignedLessThanOrEqual, value, domain.maximum());
        let valid = self.builder.ins().band(minimum, maximum);
        status::require_integer_arithmetic(self.builder, valid);
    }
}
