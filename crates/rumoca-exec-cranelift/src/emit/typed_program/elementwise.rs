//! Elementwise lowering: the scalar operator forms, fused element trees and
//! their two-lane binary64 loops (see [`super::fusion`]).

use super::*;

#[cfg(test)]
mod tests;

impl ProgramLowerer<'_, '_> {
    /// One loop storing `operation`'s result, with every fused operand
    /// evaluated per element in place of a load.
    pub(super) fn lower_fused(
        &mut self,
        operation: &solve::SolveOperation,
        destination: solve::SolveRegisterId,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let count = destination.value_type.scalar_count();
        if count >= 4 && self.vectorizes(operation)? {
            return self.lower_fused_pairs(operation, &destination, count);
        }
        self.for_each_element(destination.value_type.scalar_count(), |this, index| {
            let value = this.fused_element(operation, index)?;
            this.store_scalar(&destination, index, value)
        })
    }

    /// Whether `operation` and everything fused into it are binary64
    /// operators with an identical two-lane form.
    pub(super) fn vectorizes(
        &self,
        operation: &solve::SolveOperation,
    ) -> Result<bool, CompileError> {
        use solve::SolveBinaryOperator as B;
        use solve::SolveOperation as Op;
        use solve::SolveUnaryOperator as U;
        let binary64 = |register: solve::SolveRegisterId| -> Result<bool, CompileError> {
            Ok(is_binary64(
                self.register(register)?.value_type.element_type(),
            ))
        };
        if let Some(source) = self.sign_round_trip(operation) {
            return Ok(
                match self.layout.fused.get(source.index()).cloned().flatten() {
                    Some(deferred) => self.vectorizes(&deferred)?,
                    None => binary64(source)?,
                },
            );
        }
        let operands = match operation {
            Op::Unary {
                destination,
                operator: U::Negate | U::Abs | U::Sqrt,
                operand,
            } => {
                if !binary64(*destination)? {
                    return Ok(false);
                }
                vec![*operand]
            }
            Op::Binary {
                destination,
                operator: B::Add | B::Subtract | B::Multiply | B::Divide,
                lhs,
                rhs,
            } => {
                if !binary64(*destination)? {
                    return Ok(false);
                }
                vec![*lhs, *rhs]
            }
            Op::Scale {
                destination,
                aggregate,
                scalar,
            }
            | Op::BroadcastBinary {
                destination,
                operator: B::Add | B::Subtract | B::Multiply | B::Divide,
                aggregate,
                scalar,
                ..
            } => {
                if !binary64(*destination)? || !binary64(*scalar)? {
                    return Ok(false);
                }
                vec![*aggregate]
            }
            _ => return Ok(false),
        };
        for operand in operands {
            let vectorizes = match self.layout.fused.get(operand.index()).cloned().flatten() {
                Some(Op::Map { captures, body, .. }) => self.broadcast_pairs(&captures, &body)?,
                Some(deferred) => self.vectorizes(&deferred)?,
                None => binary64(operand)?,
            };
            if !vectorizes {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// The Real `x` of a fused `Real(integer(sign(x)))`, whose value is
    /// `sign(x)` itself: -1, 0 or 1 convert exactly both ways.
    pub(super) fn sign_round_trip(
        &self,
        operation: &solve::SolveOperation,
    ) -> Option<solve::SolveRegisterId> {
        use solve::SolveConversionOperator as C;
        use solve::SolveOperation as Op;
        let Op::Convert {
            operator: C::IntegerToReal,
            operand,
            ..
        } = operation
        else {
            return None;
        };
        let Some(Op::Convert {
            operator: C::RealToIntegerTowardZero | C::RealToIntegerTowardNegativeInfinity,
            operand,
            ..
        }) = self.layout.fused.get(operand.index()).cloned().flatten()
        else {
            return None;
        };
        let Some(Op::Unary {
            operator: solve::SolveUnaryOperator::Sign,
            operand,
            ..
        }) = self.layout.fused.get(operand.index()).cloned().flatten()
        else {
            return None;
        };
        Some(operand)
    }

    /// [`Self::lower_fused`] two elements at a time; an odd last element
    /// takes the scalar form.
    pub(super) fn lower_fused_pairs(
        &mut self,
        operation: &solve::SolveOperation,
        destination: &ValueLocation,
        count: u32,
    ) -> Result<(), CompileError> {
        let pairs = i64::from(count / 2) * 2;
        let header = self.builder.create_block();
        let body = self.builder.create_block();
        let exit = self.builder.create_block();
        self.builder.append_block_param(header, types::I64);
        let zero = self.builder.ins().iconst(types::I64, 0);
        self.builder.ins().jump(header, &[zero.into()]);
        self.builder.switch_to_block(header);
        let index = self.builder.block_params(header)[0];
        let in_range = self
            .builder
            .ins()
            .icmp_imm(IntCC::SignedLessThan, index, pairs);
        self.builder.ins().brif(in_range, body, &[], exit, &[]);
        self.builder.switch_to_block(body);
        let value = self.pair_value(operation, index)?;
        let base = self.base(destination);
        let address = self.cell_address(base, destination.cell, index);
        self.builder.ins().store(self.flags, value, address, 0);
        let next = self.builder.ins().iadd_imm(index, 2);
        self.builder.ins().jump(header, &[next.into()]);
        self.builder.seal_block(body);
        self.builder.seal_block(header);
        self.builder.switch_to_block(exit);
        self.builder.seal_block(exit);
        if count % 2 == 1 {
            let last = self.builder.ins().iconst(types::I64, i64::from(count - 1));
            let value = self.fused_element(operation, last)?;
            self.store_scalar(destination, last, value)?;
        }
        Ok(())
    }

    /// Whether a fused broadcast map vectorizes: a binary64 row of even
    /// width holds both lanes of every even index.
    fn broadcast_pairs(
        &self,
        captures: &[solve::SolveRegisterId],
        body: &solve::SolveProgramRegion,
    ) -> Result<bool, CompileError> {
        let Some(capture) = fusion::broadcast_capture(captures, body) else {
            return Ok(false);
        };
        let location = self.register(capture)?;
        Ok(is_binary64(location.value_type.element_type())
            && location.value_type.scalar_count() % 2 == 0)
    }

    /// Elements `index` and `index + 1` of a fused broadcast map.
    fn broadcast_pair(
        &mut self,
        captures: &[solve::SolveRegisterId],
        body: &solve::SolveProgramRegion,
        index: Value,
    ) -> Result<Value, CompileError> {
        let capture = fusion::broadcast_capture(captures, body)
            .ok_or_else(|| CompileError::Backend("fused map is not a broadcast".into()))?;
        let location = self.register(capture)?.clone();
        let width = i64::from(location.value_type.scalar_count());
        let element = self.builder.ins().urem_imm(index, width);
        let base = self.base(&location);
        let address = self.cell_address(base, location.cell, element);
        Ok(self
            .builder
            .ins()
            .load(types::F64X2, self.flags, address, 0))
    }

    pub(super) fn pair_register(
        &mut self,
        register: solve::SolveRegisterId,
        index: Value,
    ) -> Result<Value, CompileError> {
        match self.layout.fused.get(register.index()).cloned().flatten() {
            Some(solve::SolveOperation::Map { captures, body, .. }) => {
                return self.broadcast_pair(&captures, &body, index);
            }
            Some(operation) => return self.pair_value(&operation, index),
            None => {}
        }
        let location = self.register(register)?.clone();
        let base = self.base(&location);
        let address = self.cell_address(base, location.cell, index);
        Ok(self
            .builder
            .ins()
            .load(types::F64X2, self.flags, address, 0))
    }

    /// Elements `index` and `index + 1` of a vectorizable operation.
    pub(super) fn pair_value(
        &mut self,
        operation: &solve::SolveOperation,
        index: Value,
    ) -> Result<Value, CompileError> {
        use solve::SolveBinaryOperator as B;
        use solve::SolveOperation as Op;
        use solve::SolveUnaryOperator as U;
        let binary = |this: &mut Self, operator: B, lhs: Value, rhs: Value| match operator {
            B::Add => Ok(this.builder.ins().fadd(lhs, rhs)),
            B::Subtract => Ok(this.builder.ins().fsub(lhs, rhs)),
            B::Multiply => Ok(this.builder.ins().fmul(lhs, rhs)),
            B::Divide => Ok(this.builder.ins().fdiv(lhs, rhs)),
            _ => Err(CompileError::Backend("unvectorized binary operator".into())),
        };
        if let Some(source) = self.sign_round_trip(operation) {
            let value = self.pair_register(source, index)?;
            let zero = self.builder.ins().f64const(0.0);
            let zero = self.builder.ins().splat(types::F64X2, zero);
            let one = self.builder.ins().f64const(1.0);
            let one = self.builder.ins().splat(types::F64X2, one);
            let negative_one = self.builder.ins().f64const(-1.0);
            let negative_one = self.builder.ins().splat(types::F64X2, negative_one);
            let flags = MemFlags::new();
            let greater = self.builder.ins().fcmp(FloatCC::GreaterThan, value, zero);
            let greater = self.builder.ins().bitcast(types::F64X2, flags, greater);
            let less = self.builder.ins().fcmp(FloatCC::LessThan, value, zero);
            let less = self.builder.ins().bitcast(types::F64X2, flags, less);
            let below = self.builder.ins().bitselect(less, negative_one, zero);
            return Ok(self.builder.ins().bitselect(greater, one, below));
        }
        match operation {
            Op::Unary {
                operator, operand, ..
            } => {
                let value = self.pair_register(*operand, index)?;
                match operator {
                    U::Negate => Ok(self.builder.ins().fneg(value)),
                    U::Abs => Ok(self.builder.ins().fabs(value)),
                    U::Sqrt => Ok(self.builder.ins().sqrt(value)),
                    _ => Err(CompileError::Backend("unvectorized unary operator".into())),
                }
            }
            Op::Binary {
                operator, lhs, rhs, ..
            } => {
                let lhs = self.pair_register(*lhs, index)?;
                let rhs = self.pair_register(*rhs, index)?;
                binary(self, *operator, lhs, rhs)
            }
            Op::Scale {
                aggregate, scalar, ..
            } => self.pair_broadcast(B::Multiply, (*aggregate, *scalar, false), index, binary),
            Op::BroadcastBinary {
                operator,
                aggregate,
                scalar,
                scalar_on_lhs,
                ..
            } => self.pair_broadcast(
                *operator,
                (*aggregate, *scalar, *scalar_on_lhs),
                index,
                binary,
            ),
            _ => Err(CompileError::Backend("unvectorized operation".into())),
        }
    }

    pub(super) fn pair_broadcast(
        &mut self,
        operator: solve::SolveBinaryOperator,
        (aggregate, scalar, scalar_on_lhs): (solve::SolveRegisterId, solve::SolveRegisterId, bool),
        index: Value,
        binary: impl Fn(
            &mut Self,
            solve::SolveBinaryOperator,
            Value,
            Value,
        ) -> Result<Value, CompileError>,
    ) -> Result<Value, CompileError> {
        let scalar = self.register(scalar)?.clone();
        let zero = self.builder.ins().iconst(types::I64, 0);
        let scalar = self.load_scalar(&scalar, zero)?;
        let scalar = self.builder.ins().splat(types::F64X2, scalar);
        let aggregate = self.pair_register(aggregate, index)?;
        if scalar_on_lhs {
            binary(self, operator, scalar, aggregate)
        } else {
            binary(self, operator, aggregate, scalar)
        }
    }

    pub(super) fn element_value(
        &mut self,
        register: solve::SolveRegisterId,
        index: Value,
    ) -> Result<Value, CompileError> {
        if let Some(operation) = self.layout.fused.get(register.index()).cloned().flatten() {
            return self.fused_element(&operation, index);
        }
        let location = self.register(register)?.clone();
        self.load_scalar(&location, index)
    }

    /// Element `index` of an elementwise operation's result.
    pub(super) fn fused_element(
        &mut self,
        operation: &solve::SolveOperation,
        index: Value,
    ) -> Result<Value, CompileError> {
        use solve::SolveOperation as Op;
        match operation {
            Op::Unary {
                operator, operand, ..
            } => {
                let scalar = self.register(*operand)?.value_type.element_type();
                let value = self.element_value(*operand, index)?;
                self.unary_element(*operator, scalar, value)
            }
            Op::Binary {
                destination,
                operator,
                lhs,
                rhs,
            } => {
                let scalar = self.register(*destination)?.value_type.element_type();
                let lhs = self.element_value(*lhs, index)?;
                let rhs = self.element_value(*rhs, index)?;
                self.binary_element(*operator, scalar, lhs, rhs)
            }
            Op::Compare {
                operator, lhs, rhs, ..
            } => {
                let scalar = self.register(*lhs)?.value_type.element_type();
                let lhs = self.element_value(*lhs, index)?;
                let rhs = self.element_value(*rhs, index)?;
                let condition = self.compare_element(*operator, scalar, lhs, rhs);
                Ok(self.builder.ins().uextend(types::I64, condition))
            }
            Op::Convert {
                destination,
                operator,
                operand,
            } => {
                let scalar = self.register(*operand)?.value_type.element_type();
                let destination = self.register(*destination)?.value_type.clone();
                let value = self.element_value(*operand, index)?;
                self.convert_element(*operator, scalar, &destination, value)
            }
            Op::Select {
                condition,
                if_true,
                if_false,
                ..
            } => {
                let condition = self.register(*condition)?.clone();
                let zero = self.builder.ins().iconst(types::I64, 0);
                let condition = self.load_scalar(&condition, zero)?;
                let condition = self.builder.ins().icmp_imm(IntCC::NotEqual, condition, 0);
                let when_true = self.element_value(*if_true, index)?;
                let when_false = self.element_value(*if_false, index)?;
                Ok(self.builder.ins().select(condition, when_true, when_false))
            }
            Op::Scale {
                destination,
                aggregate,
                scalar,
            } => self.broadcast_element(
                *destination,
                solve::SolveBinaryOperator::Multiply,
                (*aggregate, *scalar, false),
                index,
            ),
            Op::BroadcastBinary {
                destination,
                operator,
                aggregate,
                scalar,
                scalar_on_lhs,
            } => self.broadcast_element(
                *destination,
                *operator,
                (*aggregate, *scalar, *scalar_on_lhs),
                index,
            ),
            Op::Map { captures, body, .. } => {
                let capture = fusion::broadcast_capture(captures, body)
                    .ok_or_else(|| CompileError::Backend("fused map is not a broadcast".into()))?;
                let location = self.register(capture)?.clone();
                let width = i64::from(location.value_type.scalar_count());
                let element = self.builder.ins().urem_imm(index, width);
                self.load_scalar(&location, element)
            }
            _ => Err(CompileError::Backend(
                "fused operation is not elementwise".into(),
            )),
        }
    }

    pub(super) fn broadcast_element(
        &mut self,
        destination: solve::SolveRegisterId,
        operator: solve::SolveBinaryOperator,
        (aggregate, scalar, scalar_on_lhs): (solve::SolveRegisterId, solve::SolveRegisterId, bool),
        index: Value,
    ) -> Result<Value, CompileError> {
        let element_type = self.register(destination)?.value_type.element_type();
        let scalar = self.register(scalar)?.clone();
        let zero = self.builder.ins().iconst(types::I64, 0);
        let scalar_value = self.load_scalar(&scalar, zero)?;
        let aggregate_value = self.element_value(aggregate, index)?;
        let (lhs, rhs) = if scalar_on_lhs {
            (scalar_value, aggregate_value)
        } else {
            (aggregate_value, scalar_value)
        };
        self.binary_element(operator, element_type, lhs, rhs)
    }

    pub(super) fn unary_element(
        &mut self,
        operator: solve::SolveUnaryOperator,
        scalar: solve::SolveScalarType,
        value: Value,
    ) -> Result<Value, CompileError> {
        match scalar {
            solve::SolveScalarType::Real { format, .. } => {
                let promoted = promote_real(self.builder, format, value);
                let result = emit_unary_op(
                    self.builder,
                    self.module,
                    self.math,
                    map_unary(operator),
                    promoted,
                )?;
                Ok(demote_real(self.builder, format, result))
            }
            solve::SolveScalarType::Integer(domain) => self.integer_unary(operator, domain, value),
            solve::SolveScalarType::Boolean => match operator {
                solve::SolveUnaryOperator::Not => {
                    let condition = self.builder.ins().icmp_imm(IntCC::Equal, value, 0);
                    Ok(self.builder.ins().uextend(types::I64, condition))
                }
                _ => Err(CompileError::Backend(
                    "invalid typed Boolean unary operator".into(),
                )),
            },
        }
    }

    pub(super) fn binary_element(
        &mut self,
        operator: solve::SolveBinaryOperator,
        scalar: solve::SolveScalarType,
        lhs: Value,
        rhs: Value,
    ) -> Result<Value, CompileError> {
        match scalar {
            solve::SolveScalarType::Real { format, .. } => {
                let lhs = promote_real(self.builder, format, lhs);
                let rhs = promote_real(self.builder, format, rhs);
                let result = emit_binary_op(
                    self.builder,
                    self.module,
                    self.math,
                    map_binary(operator).ok_or_else(|| {
                        CompileError::Backend("invalid typed Real binary operator".into())
                    })?,
                    lhs,
                    rhs,
                )?;
                Ok(demote_real(self.builder, format, result))
            }
            solve::SolveScalarType::Integer(domain) => {
                self.integer_binary(operator, domain, lhs, rhs)
            }
            solve::SolveScalarType::Boolean => Ok(match operator {
                // MLS §10.3.4 orders `false < true`: the least of two Booleans
                // is their conjunction and the greatest their disjunction.
                solve::SolveBinaryOperator::And | solve::SolveBinaryOperator::Min => {
                    self.builder.ins().band(lhs, rhs)
                }
                solve::SolveBinaryOperator::Or | solve::SolveBinaryOperator::Max => {
                    self.builder.ins().bor(lhs, rhs)
                }
                _ => {
                    return Err(CompileError::Backend(
                        "invalid typed Boolean binary operator".into(),
                    ));
                }
            }),
        }
    }

    pub(super) fn compare_element(
        &mut self,
        operator: solve::SolveCompareOperator,
        scalar: solve::SolveScalarType,
        lhs: Value,
        rhs: Value,
    ) -> Value {
        match scalar {
            solve::SolveScalarType::Real { format, .. } => {
                let lhs = promote_real(self.builder, format, lhs);
                let rhs = promote_real(self.builder, format, rhs);
                self.builder
                    .ins()
                    .fcmp(map_float_compare(operator), lhs, rhs)
            }
            solve::SolveScalarType::Integer(_) => {
                self.builder.ins().icmp(map_int_compare(operator), lhs, rhs)
            }
            solve::SolveScalarType::Boolean => {
                let code = if operator == solve::SolveCompareOperator::Equal {
                    IntCC::Equal
                } else {
                    IntCC::NotEqual
                };
                self.builder.ins().icmp(code, lhs, rhs)
            }
        }
    }

    pub(super) fn convert_element(
        &mut self,
        operator: solve::SolveConversionOperator,
        source: solve::SolveScalarType,
        destination: &solve::SolveValueType,
        value: Value,
    ) -> Result<Value, CompileError> {
        Ok(match operator {
            solve::SolveConversionOperator::IntegerToReal => {
                let value = self.builder.ins().fcvt_from_sint(types::F64, value);
                match destination.element_type() {
                    solve::SolveScalarType::Real { format, .. } => {
                        demote_real(self.builder, format, value)
                    }
                    _ => {
                        return Err(CompileError::Backend(
                            "invalid Integer-to-Real destination".into(),
                        ));
                    }
                }
            }
            solve::SolveConversionOperator::RealToIntegerTowardZero => {
                let source = promote_conversion_source(self.builder, source, value)?;
                self.builder.ins().fcvt_to_sint(types::I64, source)
            }
            solve::SolveConversionOperator::RealToIntegerTowardNegativeInfinity => {
                let source = promote_conversion_source(self.builder, source, value)?;
                let floored = emit_unary_op(
                    self.builder,
                    self.module,
                    self.math,
                    rumoca_ir_solve::UnaryOp::Floor,
                    source,
                )?;
                self.builder.ins().fcvt_to_sint(types::I64, floored)
            }
        })
    }
}
