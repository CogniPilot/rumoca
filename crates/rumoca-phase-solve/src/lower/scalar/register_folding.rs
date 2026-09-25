//! Exact register-level folding of scalar arithmetic.
//!
//! A fold issues no op only when the folded result is bitwise the value the
//! op would compute for every operand value: an operation on two literal
//! registers, `x * 1`, `1 * x`, `x / 1`, `x - (+0)`, and `-(-x)`. `x + 0` is not
//! folded, because `-0 + 0` is `+0`, and a product with a literal zero factor is
//! not folded here, because `0 * x` is NaN for a non-finite `x`; only the
//! structural incidence proof may drop such a term (see `sparse_product`).

use super::*;

impl<'dae> ScalarCompiler<'_, 'dae> {
    /// A small operand of constant variability, whose scalars lower to literal
    /// registers that fold exactly when products and sums read them one at a
    /// time instead of through a packed tensor op.
    pub(super) fn is_literal_operand(&self, expression: dae::ExprId<'dae>) -> bool {
        const MAXIMUM_LITERAL_SCALARS: usize = 16;
        let node = self
            .view
            .expression(expression)
            .expect("branded expression resolves");
        node.variability() == dae::ExpressionVariability::Constant
            && node
                .value_type()
                .scalar_count()
                .is_some_and(|count| count <= MAXIMUM_LITERAL_SCALARS)
    }

    pub(super) fn real_register(&self, register: solve::Reg) -> Option<f64> {
        self.real_registers
            .get(register as usize)
            .copied()
            .flatten()
    }

    /// The register that already holds `lhs <operator> rhs`, or a new literal
    /// register holding it, without issuing the arithmetic op.
    pub(super) fn fold_binary(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: solve::Reg,
        rhs: solve::Reg,
        span: Span,
    ) -> Result<Option<solve::Reg>, LowerError> {
        let (left, right) = (self.real_register(lhs), self.real_register(rhs));
        let value = match (left, right) {
            (Some(left), Some(right)) => match operator {
                dae::BinaryOperator::Add | dae::BinaryOperator::ElementwiseAdd => left + right,
                dae::BinaryOperator::Subtract | dae::BinaryOperator::ElementwiseSubtract => {
                    left - right
                }
                dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply => {
                    left * right
                }
                dae::BinaryOperator::Divide | dae::BinaryOperator::ElementwiseDivide => {
                    left / right
                }
                _ => return Ok(None),
            },
            _ => return Ok(identity_operand(operator, lhs, rhs, left, right)),
        };
        if !value.is_finite() {
            return Ok(None);
        }
        self.constant(value, span).map(Some)
    }

    /// `-literal` as a literal register, and `-(-x)` as the register of `x`.
    pub(super) fn fold_negation(
        &mut self,
        operand: solve::Reg,
        span: Span,
    ) -> Result<Option<solve::Reg>, LowerError> {
        if let Some(value) = self.real_register(operand) {
            return self.constant(-value, span).map(Some);
        }
        Ok(self
            .negated_registers
            .get(operand as usize)
            .copied()
            .flatten())
    }

    pub(super) fn record_negation(&mut self, negated: solve::Reg, operand: solve::Reg) {
        self.negated_registers[negated as usize] = Some(operand);
    }
}

/// The operand an exact algebraic identity returns unchanged.
fn identity_operand(
    operator: dae::BinaryOperator,
    lhs: solve::Reg,
    rhs: solve::Reg,
    left: Option<f64>,
    right: Option<f64>,
) -> Option<solve::Reg> {
    let is_one = |value: Option<f64>| value == Some(1.0);
    let is_positive_zero = |value: Option<f64>| value.is_some_and(|value| value.to_bits() == 0);
    match operator {
        dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply
            if is_one(right) =>
        {
            Some(lhs)
        }
        dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply
            if is_one(left) =>
        {
            Some(rhs)
        }
        dae::BinaryOperator::Divide | dae::BinaryOperator::ElementwiseDivide if is_one(right) => {
            Some(lhs)
        }
        dae::BinaryOperator::Subtract | dae::BinaryOperator::ElementwiseSubtract
            if is_positive_zero(right) =>
        {
            Some(lhs)
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn reg(value: u32) -> solve::Reg {
        value as solve::Reg
    }

    #[test]
    fn exact_identities_return_the_other_operand() {
        let multiply = dae::BinaryOperator::Multiply;
        assert_eq!(
            identity_operand(multiply, reg(1), reg(2), None, Some(1.0)),
            Some(reg(1))
        );
        assert_eq!(
            identity_operand(multiply, reg(1), reg(2), Some(1.0), None),
            Some(reg(2))
        );
        assert_eq!(
            identity_operand(dae::BinaryOperator::Divide, reg(1), reg(2), None, Some(1.0)),
            Some(reg(1))
        );
        assert_eq!(
            identity_operand(
                dae::BinaryOperator::Subtract,
                reg(1),
                reg(2),
                None,
                Some(0.0)
            ),
            Some(reg(1))
        );
    }

    #[test]
    fn inexact_rewrites_are_refused() {
        // 0 * x is NaN for a non-finite x.
        assert_eq!(
            identity_operand(
                dae::BinaryOperator::Multiply,
                reg(1),
                reg(2),
                Some(0.0),
                None
            ),
            None
        );
        assert!((0.0 * f64::INFINITY).is_nan());
        // -0 + 0 is +0, so x + 0 is not x.
        assert_eq!(
            identity_operand(dae::BinaryOperator::Add, reg(1), reg(2), None, Some(0.0)),
            None
        );
        assert_eq!((-0.0_f64 + 0.0).to_bits(), 0);
        // x - (-0) is x + 0.
        assert_eq!(
            identity_operand(
                dae::BinaryOperator::Subtract,
                reg(1),
                reg(2),
                None,
                Some(-0.0)
            ),
            None
        );
        assert_eq!(
            identity_operand(dae::BinaryOperator::Less, reg(1), reg(2), None, Some(1.0)),
            None
        );
    }
}
