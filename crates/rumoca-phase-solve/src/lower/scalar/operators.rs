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
                solve::LinearOp::Binary {
                    dst,
                    op: solve::BinaryOp::Pow,
                    lhs,
                    rhs,
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
                self.dot_product(lhs, rhs, *inner as usize, 0, 1, 0, 1, span)
            }
            ([_, inner], [rhs_inner]) if inner == rhs_inner => {
                let row_start = scalar
                    .checked_mul(*inner as usize)
                    .ok_or_else(|| LowerError::contract("matrix row offset overflow", span))?;
                self.dot_product(lhs, rhs, *inner as usize, row_start, 1, 0, 1, span)
            }
            ([inner], [rhs_inner, columns]) if inner == rhs_inner => self.dot_product(
                lhs,
                rhs,
                *inner as usize,
                0,
                1,
                scalar,
                *columns as usize,
                span,
            ),
            ([_, inner], [rhs_inner, columns]) if inner == rhs_inner => {
                let columns = *columns as usize;
                let row = scalar / columns;
                let column = scalar % columns;
                let row_start = row
                    .checked_mul(*inner as usize)
                    .ok_or_else(|| LowerError::contract("matrix row offset overflow", span))?;
                self.dot_product(
                    lhs,
                    rhs,
                    *inner as usize,
                    row_start,
                    1,
                    column,
                    columns,
                    span,
                )
            }
            _ => Err(LowerError::contract(
                "checked multiplication shape has no scalar projection",
                span,
            )),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn dot_product(
        &mut self,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        count: usize,
        lhs_start: usize,
        lhs_stride: usize,
        rhs_start: usize,
        rhs_stride: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let mut sum = None;
        for term in 0..count {
            let lhs_index =
                lhs_start
                    .checked_add(term.checked_mul(lhs_stride).ok_or_else(|| {
                        LowerError::contract("dot-product lhs offset overflow", span)
                    })?)
                    .ok_or_else(|| LowerError::contract("dot-product lhs offset overflow", span))?;
            let rhs_index =
                rhs_start
                    .checked_add(term.checked_mul(rhs_stride).ok_or_else(|| {
                        LowerError::contract("dot-product rhs offset overflow", span)
                    })?)
                    .ok_or_else(|| LowerError::contract("dot-product rhs offset overflow", span))?;
            let lhs_term = self.expression(lhs, lhs_index)?;
            let rhs_term = self.expression(rhs, rhs_index)?;
            let product = self.binary(dae::BinaryOperator::Multiply, lhs_term, rhs_term, span)?;
            sum = Some(match sum {
                Some(previous) => self.binary(dae::BinaryOperator::Add, previous, product, span)?,
                None => product,
            });
        }
        match sum {
            Some(sum) => Ok(sum),
            None => self.constant(0.0, span),
        }
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
        for (condition, _, _) in &conditions {
            self.push_activation(*condition, false);
        }
        let mut selected = self.expression(fallback, scalar)?;
        for _ in 0..conditions.len() {
            self.pop_activation();
        }
        for (branch, (condition, condition_value, value)) in
            conditions.iter().copied().enumerate().rev()
        {
            for (previous, _, _) in conditions.iter().copied().take(branch) {
                self.push_activation(previous, false);
            }
            self.push_activation(condition, true);
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
