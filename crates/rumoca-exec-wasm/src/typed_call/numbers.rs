mod extrema;
mod quotients;
#[cfg(test)]
mod tests;
use super::emit::{CELL, Emitter};
use super::layout::CellRange;
use super::{TypedCallCompileError, TypedCallFaultKind, math};
use rumoca_ir_solve as solve;
use wasm_encoder::{BlockType, Instruction as I};

/// Where one binary operand cell is read from.
#[derive(Clone, Copy)]
pub(super) enum Operand {
    /// The cell of a register at the current cell index (local 3).
    Cells(CellRange),
    /// The first cell of a range, whatever the cell index.
    Fixed(CellRange),
}

impl Emitter<'_> {
    pub(super) fn number_operation(
        &mut self,
        index: usize,
        spanned: &solve::SolveSpannedOperation,
    ) -> Result<(), TypedCallCompileError> {
        use solve::SolveOperation as O;
        let (destination, opcode, kind) = match spanned.operation() {
            O::Unary {
                destination,
                operator,
                operand,
            } => {
                if !unary_supported(*operator, self.scalar(*operand)) {
                    return Err(self.unsupported(index, "unary", spanned.provenance()));
                }
                (*destination, "unary", TypedCallFaultKind::IntegerArithmetic)
            }
            O::Binary {
                destination,
                operator,
                lhs,
                ..
            } => {
                if !binary_supported(*operator, self.scalar(*lhs)) {
                    return Err(self.unsupported(index, "binary", spanned.provenance()));
                }
                (
                    *destination,
                    "binary",
                    TypedCallFaultKind::IntegerArithmetic,
                )
            }
            O::Compare { destination, .. } => (
                *destination,
                "compare",
                TypedCallFaultKind::IntegerArithmetic,
            ),
            O::Convert { destination, .. } => (
                *destination,
                "convert",
                TypedCallFaultKind::IntegerConversion,
            ),
            _ => unreachable!("numeric operation dispatch"),
        };
        let status = self.fault(Some(index), opcode, kind, spanned.provenance());
        let real_result = matches!(
            self.scalar(destination),
            solve::SolveScalarType::Real { .. }
        );
        let destination = self.reg(destination);
        self.cells(destination.bytes / 8, |e| {
            e.cell_address(destination);
            match spanned.operation() {
                O::Unary {
                    operator, operand, ..
                } => e.unary(*operator, *operand, status),
                O::Binary {
                    operator, lhs, rhs, ..
                } => {
                    let scalar = e.scalar(*lhs);
                    let (lhs, rhs) = (Operand::Cells(e.reg(*lhs)), Operand::Cells(e.reg(*rhs)));
                    e.binary(*operator, scalar, (lhs, rhs), status);
                }
                O::Compare {
                    operator, lhs, rhs, ..
                } => e.compare(*operator, *lhs, *rhs),
                O::Convert {
                    operator, operand, ..
                } => e.convert(*operator, *operand, status),
                _ => unreachable!("numeric operation dispatch"),
            }
            // Both stores retain the same eight-byte ABI cell representation.
            e.push(if real_result {
                I::F64Store(CELL)
            } else {
                I::I64Store(CELL)
            });
        });
        Ok(())
    }

    pub(super) fn broadcast_operation(
        &mut self,
        index: usize,
        spanned: &solve::SolveSpannedOperation,
    ) -> Result<(), TypedCallCompileError> {
        let solve::SolveOperation::BroadcastBinary {
            destination,
            operator,
            aggregate,
            scalar,
            scalar_on_lhs,
        } = spanned.operation()
        else {
            unreachable!("checked broadcast dispatch");
        };
        if !binary_supported(*operator, self.scalar(*aggregate)) {
            return Err(self.unsupported(index, "broadcast binary", spanned.provenance()));
        }
        let status = self.fault(
            Some(index),
            "broadcast binary",
            TypedCallFaultKind::IntegerArithmetic,
            spanned.provenance(),
        );
        let real_result = matches!(
            self.scalar(*destination),
            solve::SolveScalarType::Real { .. }
        );
        let destination = self.reg(*destination);
        let (lhs, rhs) = if *scalar_on_lhs {
            (*scalar, *aggregate)
        } else {
            (*aggregate, *scalar)
        };
        // The scalar operand is one fixed cell for every element.
        let operand = |register: solve::SolveRegisterId| {
            if register == *scalar {
                Operand::Fixed(self.reg(register))
            } else {
                Operand::Cells(self.reg(register))
            }
        };
        let operands = (operand(lhs), operand(rhs));
        let element = self.scalar(*aggregate);
        self.cells(destination.bytes / 8, |e| {
            e.cell_address(destination);
            e.binary(*operator, element, operands, status);
            e.push(if real_result {
                I::F64Store(CELL)
            } else {
                I::I64Store(CELL)
            });
        });
        Ok(())
    }

    pub(super) fn integer_domain(&mut self, domain: solve::SolveIntegerDomain, status: u32) {
        self.push(I::LocalGet(7));
        self.push(I::I64Const(domain.minimum()));
        self.push(I::I64LtS);
        self.fail_if(status);
        self.push(I::LocalGet(7));
        self.push(I::I64Const(domain.maximum()));
        self.push(I::I64GtS);
        self.fail_if(status);
    }

    fn unary(
        &mut self,
        operator: solve::SolveUnaryOperator,
        operand: solve::SolveRegisterId,
        status: u32,
    ) {
        use solve::SolveUnaryOperator as U;
        match self.scalar(operand) {
            solve::SolveScalarType::Real { .. } => {
                self.load_cell(self.reg(operand), true);
                match operator {
                    U::Negate => self.push(I::F64Neg),
                    U::Abs => self.push(I::F64Abs),
                    U::Sqrt => self.push(I::F64Sqrt),
                    U::Floor => self.push(I::F64Floor),
                    U::Ceiling => self.push(I::F64Ceil),
                    U::Truncate => self.push(I::F64Trunc),
                    U::Sign => self.real_sign(),
                    _ => self.push(I::Call(
                        self.linked.math_indices[&math::unary(operator)
                            .expect("checked imported Real unary operation")],
                    )),
                }
            }
            solve::SolveScalarType::Boolean => {
                self.load_cell(self.reg(operand), false);
                self.push(I::I64Eqz);
                self.push(I::I64ExtendI32U);
            }
            solve::SolveScalarType::Integer(domain) => {
                self.integer_unary(operator, operand, domain, status);
            }
        }
    }

    fn integer_unary(
        &mut self,
        operator: solve::SolveUnaryOperator,
        operand: solve::SolveRegisterId,
        domain: solve::SolveIntegerDomain,
        status: u32,
    ) {
        use solve::SolveUnaryOperator as U;
        self.load_cell(self.reg(operand), false);
        self.push(I::LocalSet(5));
        match operator {
            U::Negate | U::Abs => {
                self.push(I::LocalGet(5));
                self.push(I::I64Const(i64::MIN));
                self.push(I::I64Eq);
                self.fail_if(status);
                self.push(I::I64Const(0));
                self.push(I::LocalGet(5));
                self.push(I::I64Sub);
                if operator == U::Abs {
                    self.push(I::LocalGet(5));
                    self.push(I::LocalGet(5));
                    self.push(I::I64Const(0));
                    self.push(I::I64LtS);
                    self.push(I::Select);
                }
            }
            U::Sign => {
                self.push(I::LocalGet(5));
                self.push(I::I64Const(0));
                self.push(I::I64GtS);
                self.push(I::I64ExtendI32U);
                self.push(I::LocalGet(5));
                self.push(I::I64Const(0));
                self.push(I::I64LtS);
                self.push(I::I64ExtendI32U);
                self.push(I::I64Sub);
            }
            _ => unreachable!("checked unary admission"),
        }
        self.push(I::LocalSet(7));
        self.integer_domain(domain, status);
        self.push(I::LocalGet(7));
    }

    fn real_sign(&mut self) {
        self.push(I::LocalSet(8));
        self.push(I::F64Const(1.0.into()));
        self.push(I::F64Const((-1.0).into()));
        self.push(I::F64Const(0.0.into()));
        self.push(I::LocalGet(8));
        self.push(I::F64Const(0.0.into()));
        self.push(I::F64Lt);
        self.push(I::Select);
        self.push(I::LocalGet(8));
        self.push(I::F64Const(0.0.into()));
        self.push(I::F64Gt);
        self.push(I::Select);
    }

    /// One element of `lhs <operator> rhs` over `scalar` on the operand stack,
    /// with the checked Integer overflow and domain faults at `status`.
    pub(super) fn binary(
        &mut self,
        operator: solve::SolveBinaryOperator,
        scalar: solve::SolveScalarType,
        (lhs, rhs): (Operand, Operand),
        status: u32,
    ) {
        use solve::SolveBinaryOperator as B;
        match scalar {
            solve::SolveScalarType::Real { .. } => {
                if matches!(operator, B::Min | B::Max) {
                    self.real_extremum(operator, lhs, rhs);
                } else {
                    self.load_operand(lhs, true);
                    self.load_operand(rhs, true);
                    self.push(match operator {
                        B::Add => I::F64Add,
                        B::Subtract => I::F64Sub,
                        B::Multiply => I::F64Mul,
                        B::Divide => I::F64Div,
                        B::Power | B::Atan2 => I::Call(
                            self.linked.math_indices[&math::binary(operator)
                                .expect("checked imported Real binary operation")],
                        ),
                        _ => unreachable!("checked binary admission"),
                    });
                }
            }
            solve::SolveScalarType::Boolean => {
                self.load_operand(lhs, false);
                self.load_operand(rhs, false);
                self.push(if operator == B::And {
                    I::I64And
                } else {
                    I::I64Or
                });
            }
            solve::SolveScalarType::Integer(domain) => {
                self.load_operand(lhs, false);
                self.push(I::LocalSet(5));
                self.load_operand(rhs, false);
                self.push(I::LocalSet(6));
                self.integer_binary(operator, status);
                self.integer_domain(domain, status);
                self.push(I::LocalGet(7));
            }
        }
    }

    fn load_operand(&mut self, operand: Operand, real: bool) {
        let load = if real {
            I::F64Load(CELL)
        } else {
            I::I64Load(CELL)
        };
        match operand {
            Operand::Cells(range) => self.load_cell(range, real),
            Operand::Fixed(range) => {
                self.address(range);
                self.push(load);
            }
        }
    }

    fn integer_binary(&mut self, operator: solve::SolveBinaryOperator, status: u32) {
        use solve::SolveBinaryOperator as B;
        if matches!(
            operator,
            B::IntegerQuotient | B::IntegerModulo | B::IntegerRemainder
        ) {
            self.integer_quotient(operator, status);
            return;
        }
        self.push(I::LocalGet(5));
        self.push(I::LocalGet(6));
        match operator {
            B::Add => self.push(I::I64Add),
            B::Subtract => self.push(I::I64Sub),
            B::Multiply => self.push(I::I64Mul),
            B::Min | B::Max => {
                self.push(I::LocalGet(5));
                self.push(I::LocalGet(6));
                self.push(if operator == B::Min {
                    I::I64LtS
                } else {
                    I::I64GtS
                });
                self.push(I::Select);
            }
            _ => unreachable!("checked integer admission"),
        }
        self.push(I::LocalSet(7));
        match operator {
            B::Add | B::Subtract => self.integer_add_overflow(operator, status),
            B::Multiply => self.integer_multiply_overflow(status),
            _ => {}
        }
    }

    fn integer_add_overflow(&mut self, operator: solve::SolveBinaryOperator, status: u32) {
        self.push(I::LocalGet(5));
        self.push(I::LocalGet(
            if operator == solve::SolveBinaryOperator::Subtract {
                6
            } else {
                7
            },
        ));
        self.push(I::I64Xor);
        self.push(I::LocalGet(
            if operator == solve::SolveBinaryOperator::Subtract {
                5
            } else {
                6
            },
        ));
        self.push(I::LocalGet(7));
        self.push(I::I64Xor);
        self.push(I::I64And);
        self.push(I::I64Const(0));
        self.push(I::I64LtS);
        self.fail_if(status);
    }

    fn integer_multiply_overflow(&mut self, status: u32) {
        self.push(I::LocalGet(5));
        self.push(I::I64Eqz);
        self.push(I::LocalGet(6));
        self.push(I::I64Eqz);
        self.push(I::I32Or);
        self.push(I::I32Eqz);
        self.push(I::If(BlockType::Empty));
        for (a, b) in [(5, 6), (6, 5)] {
            self.push(I::LocalGet(a));
            self.push(I::I64Const(i64::MIN));
            self.push(I::I64Eq);
            self.push(I::LocalGet(b));
            self.push(I::I64Const(-1));
            self.push(I::I64Eq);
            self.push(I::I32And);
            self.fail_if(status);
        }
        self.push(I::LocalGet(7));
        self.push(I::LocalGet(6));
        self.push(I::I64DivS);
        self.push(I::LocalGet(5));
        self.push(I::I64Ne);
        self.fail_if(status);
        self.push(I::End);
    }

    fn compare(
        &mut self,
        operator: solve::SolveCompareOperator,
        lhs: solve::SolveRegisterId,
        rhs: solve::SolveRegisterId,
    ) {
        let real = matches!(self.scalar(lhs), solve::SolveScalarType::Real { .. });
        self.load_cell(self.reg(lhs), real);
        self.load_cell(self.reg(rhs), real);
        self.push(compare_instruction(operator, real));
        self.push(I::I64ExtendI32U);
    }

    fn convert(
        &mut self,
        operator: solve::SolveConversionOperator,
        operand: solve::SolveRegisterId,
        status: u32,
    ) {
        use solve::SolveConversionOperator as C;
        if operator == C::IntegerToReal {
            self.load_cell(self.reg(operand), false);
            self.push(I::F64ConvertI64S);
            return;
        }
        self.load_cell(self.reg(operand), true);
        self.push(if operator == C::RealToIntegerTowardZero {
            I::F64Trunc
        } else {
            I::F64Floor
        });
        self.push(I::LocalSet(9));
        self.push(I::LocalGet(9));
        self.push(I::LocalGet(9));
        self.push(I::F64Ne);
        self.fail_if(status);
        self.push(I::LocalGet(9));
        self.push(I::F64Const((-9223372036854775808.0).into()));
        self.push(I::F64Lt);
        self.fail_if(status);
        self.push(I::LocalGet(9));
        self.push(I::F64Const(9223372036854775808.0.into()));
        self.push(I::F64Ge);
        self.fail_if(status);
        self.push(I::LocalGet(9));
        self.push(I::I64TruncF64S);
        self.push(I::LocalSet(7));
        self.integer_domain(self.owner.body().arithmetic().integer_domain(), status);
        self.push(I::LocalGet(7));
    }
}

fn unary_supported(operator: solve::SolveUnaryOperator, scalar: solve::SolveScalarType) -> bool {
    use solve::SolveUnaryOperator as U;
    match scalar {
        solve::SolveScalarType::Real { .. } => {
            math::unary(operator).is_some()
                || matches!(
                    operator,
                    U::Negate | U::Abs | U::Sign | U::Sqrt | U::Floor | U::Ceiling | U::Truncate
                )
        }
        solve::SolveScalarType::Integer(_) => matches!(operator, U::Negate | U::Abs | U::Sign),
        solve::SolveScalarType::Boolean => operator == U::Not,
    }
}

pub(super) fn binary_supported(
    operator: solve::SolveBinaryOperator,
    scalar: solve::SolveScalarType,
) -> bool {
    use solve::SolveBinaryOperator as B;
    match scalar {
        solve::SolveScalarType::Real { .. } => {
            matches!(
                operator,
                B::Add
                    | B::Subtract
                    | B::Multiply
                    | B::Divide
                    | B::Min
                    | B::Max
                    | B::Power
                    | B::Atan2
            )
        }
        solve::SolveScalarType::Integer(_) => matches!(
            operator,
            B::Add
                | B::Subtract
                | B::Multiply
                | B::Min
                | B::Max
                | B::IntegerQuotient
                | B::IntegerModulo
                | B::IntegerRemainder
        ),
        solve::SolveScalarType::Boolean => matches!(operator, B::And | B::Or),
    }
}

fn compare_instruction(operator: solve::SolveCompareOperator, real: bool) -> I<'static> {
    use solve::SolveCompareOperator as C;
    match (operator, real) {
        (C::Equal, true) => I::F64Eq,
        (C::NotEqual, true) => I::F64Ne,
        (C::Less, true) => I::F64Lt,
        (C::LessEqual, true) => I::F64Le,
        (C::Greater, true) => I::F64Gt,
        (C::GreaterEqual, true) => I::F64Ge,
        (C::Equal, false) => I::I64Eq,
        (C::NotEqual, false) => I::I64Ne,
        (C::Less, false) => I::I64LtS,
        (C::LessEqual, false) => I::I64LeS,
        (C::Greater, false) => I::I64GtS,
        (C::GreaterEqual, false) => I::I64GeS,
    }
}
