//! Shape-preserving product and quotient differentiation through order two.

use super::*;

impl<'source, 'borrow, 'storage, 'target> ExpressionRebuilder<'source, 'borrow, 'storage, 'target> {
    pub(super) fn differentiate_sum(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'source>,
        rhs: dae::ExprId<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let left = self.differentiate_order(lhs, order, provenance)?;
        let right = self.differentiate_order(rhs, order, provenance)?;
        let same_shape = self
            .source
            .expression(lhs)
            .unwrap()
            .value_type()
            .dimensions()
            == self
                .source
                .expression(rhs)
                .unwrap()
                .value_type()
                .dimensions();
        if same_shape || matches!((left, right), (Derivative::Zero, Derivative::Zero)) {
            return self.combine_sum(operator, left, right, provenance);
        }
        // A zero tensor still supplies the shape for scalar broadcasting.
        let left = self.materialize_derivative(left, lhs, provenance)?;
        let right = self.materialize_derivative(right, rhs, provenance)?;
        self.target
            .at(provenance)
            .binary(operator, left, right)
            .map(Derivative::Expression)
    }

    pub(super) fn differentiation_value(
        &mut self,
        expression: dae::ExprId<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        if self.state_only_derivative {
            self.materialize_exact_value(expression, provenance)
        } else {
            self.rebuild_instantiated(expression)
        }
    }

    pub(super) fn derivative_or_value(
        &mut self,
        expression: dae::ExprId<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        if order == 0 {
            self.differentiation_value(expression, provenance)
                .map(Derivative::Expression)
        } else {
            self.differentiate_order(expression, order, provenance)
        }
    }

    pub(super) fn derivative_product(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: Derivative<'target>,
        rhs: Derivative<'target>,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let (Derivative::Expression(lhs), Derivative::Expression(rhs)) = (lhs, rhs) else {
            return Ok(Derivative::Zero);
        };
        self.target
            .at(provenance)
            .binary(operator, lhs, rhs)
            .map(Derivative::Expression)
    }

    pub(super) fn twice(
        &mut self,
        value: Derivative<'target>,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let Derivative::Expression(value) = value else {
            return Ok(Derivative::Zero);
        };
        let two = self
            .target
            .at(provenance)
            .literal(dae::DaeLiteral::Real(2.0))?;
        self.target
            .at(provenance)
            .binary(dae::BinaryOperator::Multiply, two, value)
            .map(Derivative::Expression)
    }

    pub(super) fn differentiate_product(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'source>,
        rhs: dae::ExprId<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        assert!((1..=2).contains(&order));
        let mut result = Derivative::Zero;
        for left_order in 0..=order {
            let left = self.derivative_or_value(lhs, left_order, provenance)?;
            let right = self.derivative_or_value(rhs, order - left_order, provenance)?;
            let mut term = self.derivative_product(operator, left, right, provenance)?;
            if order == 2 && left_order == 1 {
                term = self.twice(term, provenance)?;
            }
            result = self.combine_sum(dae::BinaryOperator::Add, result, term, provenance)?;
        }
        Ok(result)
    }

    pub(super) fn differentiate_quotient(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'source>,
        rhs: dae::ExprId<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        assert!((1..=2).contains(&order));
        let left = self.differentiate_order(lhs, order, provenance)?;
        let right = self.differentiate_order(rhs, order, provenance)?;
        let lhs_value = self.differentiation_value(lhs, provenance)?;
        let rhs_value = self.differentiation_value(rhs, provenance)?;
        let value = self
            .target
            .at(provenance)
            .binary(operator, lhs_value, rhs_value)?;
        let multiply = if operator == dae::BinaryOperator::Divide {
            dae::BinaryOperator::Multiply
        } else {
            dae::BinaryOperator::ElementwiseMultiply
        };
        let subtract = if operator == dae::BinaryOperator::Divide {
            dae::BinaryOperator::Subtract
        } else {
            dae::BinaryOperator::ElementwiseSubtract
        };
        let product =
            self.derivative_product(multiply, Derivative::Expression(value), right, provenance)?;
        let mut numerator = self.combine_sum(subtract, left, product, provenance)?;
        if order == 2 {
            let first = self.differentiate_quotient(operator, lhs, rhs, 1, provenance)?;
            let rhs_first = self.differentiate_order(rhs, 1, provenance)?;
            let mixed = self.derivative_product(multiply, first, rhs_first, provenance)?;
            let mixed = self.twice(mixed, provenance)?;
            numerator = self.combine_sum(subtract, numerator, mixed, provenance)?;
        }
        let Derivative::Expression(numerator) = numerator else {
            return Ok(Derivative::Zero);
        };
        self.target
            .at(provenance)
            .binary(operator, numerator, rhs_value)
            .map(Derivative::Expression)
    }
}
