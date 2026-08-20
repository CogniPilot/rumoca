//! Unary, binary, and conditional operator lowering.
//!
//! Each Modelica operator maps to the Solve op with the same scalar meaning.
//! Multiplication is the one shape-sensitive case: its scalar projection picks
//! the dot product the operand ranks call for.

use super::*;

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    pub(super) fn unary(
        &mut self,
        operator: dae::UnaryOperator,
        operand: solve::Reg,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if operator == dae::UnaryOperator::Plus {
            return Ok(operand);
        }
        let op = match operator {
            dae::UnaryOperator::Plus => unreachable!(),
            dae::UnaryOperator::Negate => solve::UnaryOp::Neg,
            dae::UnaryOperator::Not => solve::UnaryOp::Not,
        };
        let dst = self.register(span)?;
        self.ops.push(solve::LinearOp::Unary {
            dst,
            op,
            arg: operand,
        });
        let integer = self
            .integer_register(operand)
            .and_then(|value| match operator {
                dae::UnaryOperator::Plus => unreachable!(),
                dae::UnaryOperator::Negate => value.checked_neg(),
                dae::UnaryOperator::Not => Some(i64::from(value == 0)),
            });
        self.set_integer_register(dst, integer);
        Ok(dst)
    }

    pub(super) fn binary(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: solve::Reg,
        rhs: solve::Reg,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let dst = self.register(span)?;
        let operation = match operator {
            dae::BinaryOperator::Add | dae::BinaryOperator::ElementwiseAdd => {
                solve::LinearOp::Binary {
                    dst,
                    op: solve::BinaryOp::Add,
                    lhs,
                    rhs,
                }
            }
            dae::BinaryOperator::Subtract | dae::BinaryOperator::ElementwiseSubtract => {
                solve::LinearOp::Binary {
                    dst,
                    op: solve::BinaryOp::Sub,
                    lhs,
                    rhs,
                }
            }
            dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply => {
                solve::LinearOp::Binary {
                    dst,
                    op: solve::BinaryOp::Mul,
                    lhs,
                    rhs,
                }
            }
            dae::BinaryOperator::Divide | dae::BinaryOperator::ElementwiseDivide => {
                solve::LinearOp::Binary {
                    dst,
                    op: solve::BinaryOp::Div,
                    lhs,
                    rhs,
                }
            }
            dae::BinaryOperator::Power | dae::BinaryOperator::ElementwisePower => {
                match self.integer_register(rhs) {
                    Some(2) => solve::LinearOp::Binary {
                        dst,
                        op: solve::BinaryOp::Mul,
                        lhs,
                        rhs: lhs,
                    },
                    Some(3) => {
                        let square = self.register(span)?;
                        self.ops.push(solve::LinearOp::Binary {
                            dst: square,
                            op: solve::BinaryOp::Mul,
                            lhs,
                            rhs: lhs,
                        });
                        solve::LinearOp::Binary {
                            dst,
                            op: solve::BinaryOp::Mul,
                            lhs: square,
                            rhs: lhs,
                        }
                    }
                    _ => solve::LinearOp::Binary {
                        dst,
                        op: solve::BinaryOp::Pow,
                        lhs,
                        rhs,
                    },
                }
            }
            dae::BinaryOperator::And => solve::LinearOp::Binary {
                dst,
                op: solve::BinaryOp::And,
                lhs,
                rhs,
            },
            dae::BinaryOperator::Or => solve::LinearOp::Binary {
                dst,
                op: solve::BinaryOp::Or,
                lhs,
                rhs,
            },
            comparison => solve::LinearOp::Compare {
                dst,
                op: compare_operator(comparison),
                lhs,
                rhs,
            },
        };
        self.ops.push(operation);
        let integer = self.integer_binary_result(operator, lhs, rhs);
        self.set_integer_register(dst, integer);
        Ok(dst)
    }

    fn integer_binary_result(
        &self,
        operator: dae::BinaryOperator,
        lhs: solve::Reg,
        rhs: solve::Reg,
    ) -> Option<i64> {
        let lhs = self.integer_register(lhs)?;
        let rhs = self.integer_register(rhs)?;
        match operator {
            dae::BinaryOperator::Add | dae::BinaryOperator::ElementwiseAdd => lhs.checked_add(rhs),
            dae::BinaryOperator::Subtract | dae::BinaryOperator::ElementwiseSubtract => {
                lhs.checked_sub(rhs)
            }
            dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply => {
                lhs.checked_mul(rhs)
            }
            dae::BinaryOperator::And => Some(i64::from(lhs != 0 && rhs != 0)),
            dae::BinaryOperator::Or => Some(i64::from(lhs != 0 || rhs != 0)),
            dae::BinaryOperator::Less => Some(i64::from(lhs < rhs)),
            dae::BinaryOperator::LessEqual => Some(i64::from(lhs <= rhs)),
            dae::BinaryOperator::Greater => Some(i64::from(lhs > rhs)),
            dae::BinaryOperator::GreaterEqual => Some(i64::from(lhs >= rhs)),
            dae::BinaryOperator::Equal => Some(i64::from(lhs == rhs)),
            dae::BinaryOperator::NotEqual => Some(i64::from(lhs != rhs)),
            dae::BinaryOperator::Divide
            | dae::BinaryOperator::ElementwiseDivide
            | dae::BinaryOperator::Power
            | dae::BinaryOperator::ElementwisePower => None,
        }
    }

    pub(super) fn binary_expression(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if let Some(output) =
            self.compact_tensor_binary_expression(operator, lhs, rhs, scalar, span)?
        {
            return Ok(output);
        }
        if operator == dae::BinaryOperator::Multiply {
            return self.multiply_expression(lhs, rhs, scalar, span);
        }
        if operator == dae::BinaryOperator::Power && !self.node(lhs).value_type().is_scalar() {
            return Err(LowerError::unsupported(
                "matrix power does not yet have checked Solve lowering",
                span,
            ));
        }
        let lhs_scalar = if scalar_count(self.view, lhs) == 1 {
            0
        } else {
            scalar
        };
        let rhs_scalar = if scalar_count(self.view, rhs) == 1 {
            0
        } else {
            scalar
        };
        let lhs = self.expression(lhs, lhs_scalar)?;
        let rhs = self.expression(rhs, rhs_scalar)?;
        self.binary(operator, lhs, rhs, span)
    }

    fn compact_tensor_binary_expression(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<Option<solve::Reg>, LowerError> {
        let lhs_count = scalar_count(self.view, lhs);
        let rhs_count = scalar_count(self.view, rhs);
        let count = lhs_count.max(rhs_count);
        if count <= 1 {
            return Ok(None);
        }
        let op = match operator {
            dae::BinaryOperator::Add | dae::BinaryOperator::ElementwiseAdd => solve::BinaryOp::Add,
            dae::BinaryOperator::Subtract | dae::BinaryOperator::ElementwiseSubtract => {
                solve::BinaryOp::Sub
            }
            dae::BinaryOperator::ElementwiseMultiply => solve::BinaryOp::Mul,
            dae::BinaryOperator::Multiply if lhs_count == 1 || rhs_count == 1 => {
                solve::BinaryOp::Mul
            }
            dae::BinaryOperator::ElementwiseDivide => solve::BinaryOp::Div,
            dae::BinaryOperator::Divide if lhs_count == 1 || rhs_count == 1 => solve::BinaryOp::Div,
            _ => return Ok(None),
        };
        let key = (self.context_id, op, lhs, rhs);
        if let Some(&(start, cached_count)) = self.tensor_binary_cache.get(&key) {
            return (scalar < cached_count)
                .then(|| start + scalar as solve::Reg)
                .map(Some)
                .ok_or_else(|| LowerError::contract("tensor binary scalar is out of range", span));
        }
        let lhs_start = self.pack_expression(lhs)?;
        let rhs_start = self.pack_expression(rhs)?;
        let dst_start = self.next_register;
        for _ in 0..count {
            self.register(span)?;
        }
        self.ops.push(solve::LinearOp::TensorBinary {
            dst_start,
            op,
            lhs_start,
            rhs_start,
            count,
            lhs_stride: usize::from(lhs_count != 1),
            rhs_stride: usize::from(rhs_count != 1),
            lanes: 1,
        });
        self.tensor_binary_cache.insert(key, (dst_start, count));
        (scalar < count)
            .then(|| Some(dst_start + scalar as solve::Reg))
            .ok_or_else(|| LowerError::contract("tensor binary scalar is out of range", span))
    }

    fn multiply_expression(
        &mut self,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let lhs_dimensions = self.node(lhs).value_type().dimensions().to_vec();
        let rhs_dimensions = self.node(rhs).value_type().dimensions().to_vec();
        match (lhs_dimensions.as_slice(), rhs_dimensions.as_slice()) {
            ([], _) => {
                let lhs = self.expression(lhs, 0)?;
                let rhs = self.expression(rhs, scalar)?;
                self.binary(dae::BinaryOperator::Multiply, lhs, rhs, span)
            }
            (_, []) => {
                let lhs = self.expression(lhs, scalar)?;
                let rhs = self.expression(rhs, 0)?;
                self.binary(dae::BinaryOperator::Multiply, lhs, rhs, span)
            }
            ([inner], [rhs_inner]) if inner == rhs_inner => {
                self.packed_multiply_outputs(lhs, rhs, *inner as usize, 1, 1, scalar, span)
            }
            ([rows, inner], [rhs_inner]) if inner == rhs_inner => self.packed_multiply_outputs(
                lhs,
                rhs,
                *inner as usize,
                *rows as usize,
                1,
                scalar,
                span,
            ),
            ([inner], [rhs_inner, columns]) if inner == rhs_inner => self.packed_multiply_outputs(
                lhs,
                rhs,
                *inner as usize,
                1,
                *columns as usize,
                scalar,
                span,
            ),
            ([rows, inner], [rhs_inner, columns]) if inner == rhs_inner => self
                .packed_multiply_outputs(
                    lhs,
                    rhs,
                    *inner as usize,
                    *rows as usize,
                    *columns as usize,
                    scalar,
                    span,
                ),
            _ => Err(LowerError::contract(
                "checked multiplication shape has no scalar projection",
                span,
            )),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn packed_multiply_outputs(
        &mut self,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        inner: usize,
        rows: usize,
        columns: usize,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let key = (self.context_id, lhs, rhs);
        if let Some(&(start, count)) = self.matrix_multiply_cache.get(&key) {
            return (scalar < count)
                .then(|| start + scalar as solve::Reg)
                .ok_or_else(|| {
                    LowerError::contract("matrix product scalar is out of range", span)
                });
        }
        let lhs_start = self.pack_expression(lhs)?;
        let rhs_start = self.pack_expression(rhs)?;
        let count = rows
            .checked_mul(columns)
            .ok_or_else(|| LowerError::contract("matrix product output extent overflow", span))?;
        let dst_start = self.next_register;
        for _ in 0..count {
            self.register(span)?;
        }
        self.ops.push(solve::LinearOp::MatrixMultiply {
            dst_start,
            lhs_start,
            rhs_start,
            rows,
            inner,
            columns,
            lanes: 1,
        });
        self.matrix_multiply_cache.insert(key, (dst_start, count));
        (scalar < count)
            .then(|| dst_start + scalar as solve::Reg)
            .ok_or_else(|| LowerError::contract("matrix product scalar is out of range", span))
    }

    pub(super) fn conditional(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let fallback_index = operands.len() - 1;
        let mut conditions = Vec::with_capacity(fallback_index / 2);
        let mut fallback = operands
            .get(fallback_index)
            .expect("checked conditional has a fallback");
        for index in (0..fallback_index).step_by(2) {
            let condition = operands.get(index).expect("checked condition ordinal");
            let condition_value = self.expression(condition, 0)?;
            match self.integer_register(condition_value) {
                Some(0) => {}
                Some(_) => {
                    fallback = operands
                        .get(index + 1)
                        .expect("checked conditional value ordinal");
                    break;
                }
                None => conditions.push((
                    condition,
                    condition_value,
                    operands
                        .get(index + 1)
                        .expect("checked conditional value ordinal"),
                )),
            }
        }
        for (condition, condition_value, _) in &conditions {
            self.push_activation(*condition, *condition_value, false);
        }
        let mut selected = self.expression(fallback, scalar)?;
        for _ in 0..conditions.len() {
            self.pop_activation();
        }
        for (branch, (condition, condition_value, value)) in
            conditions.iter().copied().enumerate().rev()
        {
            for (previous, previous_value, _) in conditions.iter().copied().take(branch) {
                self.push_activation(previous, previous_value, false);
            }
            self.push_activation(condition, condition_value, true);
            let value = self.expression(value, scalar)?;
            self.pop_activation();
            for _ in 0..branch {
                self.pop_activation();
            }
            selected = self.select(condition_value, value, selected, span)?;
        }
        Ok(selected)
    }

    pub(super) fn solve_unary(
        &mut self,
        op: solve::UnaryOp,
        argument: solve::Reg,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let dst = self.register(span)?;
        self.ops.push(solve::LinearOp::Unary {
            dst,
            op,
            arg: argument,
        });
        Ok(dst)
    }
}
