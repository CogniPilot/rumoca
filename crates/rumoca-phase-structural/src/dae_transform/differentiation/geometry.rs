//! Tensor geometry rules shared by first and second structural derivatives.

use super::*;

impl<'source, 'borrow, 'storage, 'target> ExpressionRebuilder<'source, 'borrow, 'storage, 'target> {
    pub(super) fn differentiate_bilinear_builtin(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        assert!((1..=2).contains(&order));
        let lhs = arguments.get(0).expect("checked bilinear operand");
        let rhs = arguments.get(1).expect("checked bilinear operand");
        let mut result = Derivative::Zero;
        for left_order in 0..=order {
            let left = self.derivative_or_value(lhs, left_order, provenance)?;
            let right = self.derivative_or_value(rhs, order - left_order, provenance)?;
            let (Derivative::Expression(left), Derivative::Expression(right)) = (left, right)
            else {
                continue;
            };
            let mut term =
                Derivative::Expression(self.target.at(provenance).builtin(builtin, [left, right])?);
            if order == 2 && left_order == 1 {
                term = self.twice(term, provenance)?;
            }
            result = self.combine_sum(dae::BinaryOperator::Add, result, term, provenance)?;
        }
        Ok(result)
    }

    pub(super) fn differentiate_unary_geometry(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        assert!((1..=2).contains(&order));
        let argument = arguments.get(0).expect("checked unary geometry operand");
        if builtin == dae::PureBuiltin::Sqrt {
            return self.differentiate_sqrt(argument, order, provenance);
        }
        let highest = self.differentiate_order(argument, order, provenance)?;
        let value = self.differentiation_value(argument, provenance)?;
        let other = if builtin == dae::PureBuiltin::Sin {
            dae::PureBuiltin::Cos
        } else {
            dae::PureBuiltin::Sin
        };
        let mut factor = self.target.at(provenance).builtin(other, [value])?;
        if builtin == dae::PureBuiltin::Cos {
            factor = self
                .target
                .at(provenance)
                .unary(dae::UnaryOperator::Negate, factor)?;
        }
        let mut result = self.derivative_product(
            dae::BinaryOperator::ElementwiseMultiply,
            Derivative::Expression(factor),
            highest,
            provenance,
        )?;
        if order == 2 {
            let first = self.differentiate_order(argument, 1, provenance)?;
            let squared = self.derivative_product(
                dae::BinaryOperator::ElementwiseMultiply,
                first,
                first,
                provenance,
            )?;
            let original = self.target.at(provenance).builtin(builtin, [value])?;
            let curvature = self.derivative_product(
                dae::BinaryOperator::ElementwiseMultiply,
                Derivative::Expression(original),
                squared,
                provenance,
            )?;
            result =
                self.combine_sum(dae::BinaryOperator::Subtract, result, curvature, provenance)?;
        }
        Ok(result)
    }

    fn differentiate_sqrt(
        &mut self,
        argument: dae::ExprId<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let mut numerator = self.differentiate_order(argument, order, provenance)?;
        if order == 2 {
            let first = self.differentiate_sqrt(argument, 1, provenance)?;
            let squared = self.derivative_product(
                dae::BinaryOperator::ElementwiseMultiply,
                first,
                first,
                provenance,
            )?;
            let squared = self.twice(squared, provenance)?;
            numerator = self.combine_sum(
                dae::BinaryOperator::Subtract,
                numerator,
                squared,
                provenance,
            )?;
        }
        let Derivative::Expression(numerator) = numerator else {
            return Ok(Derivative::Zero);
        };
        let value = self.differentiation_value(argument, provenance)?;
        let value = self
            .target
            .at(provenance)
            .builtin(dae::PureBuiltin::Sqrt, [value])?;
        let denominator = self.twice(Derivative::Expression(value), provenance)?;
        let Derivative::Expression(denominator) = denominator else {
            unreachable!("twice a nonzero expression")
        };
        self.target
            .at(provenance)
            .binary(
                dae::BinaryOperator::ElementwiseDivide,
                numerator,
                denominator,
            )
            .map(Derivative::Expression)
    }
}
