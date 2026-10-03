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

    pub(in crate::dae_transform) fn differentiation_value(
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

    pub(in crate::dae_transform) fn twice(
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

    /// `d(u^v)` for a scalar base `u` and a time-invariant exponent `v`
    /// (the preflight's `is_differentiable_power`): `v*u^(v-1)*du`, and at
    /// order two `v*(v-1)*u^(v-2)*du^2 + v*u^(v-1)*d2u`.
    pub(super) fn differentiate_power(
        &mut self,
        base: dae::ExprId<'source>,
        exponent: dae::ExprId<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        assert!((1..=2).contains(&order));
        let first = self.differentiate_order(base, 1, provenance)?;
        let u = self.differentiation_value(base, provenance)?;
        let v = self.differentiation_value(exponent, provenance)?;
        let scaled_power = |this: &mut Self, drop: f64| {
            let at = this.target.at(provenance);
            let drop = at.literal(dae::DaeLiteral::Real(drop))?;
            let reduced =
                this.target
                    .at(provenance)
                    .binary(dae::BinaryOperator::Subtract, v, drop)?;
            this.target
                .at(provenance)
                .binary(dae::BinaryOperator::Power, u, reduced)
        };
        let power_minus_one = scaled_power(self, 1.0)?;
        let coefficient =
            self.target
                .at(provenance)
                .binary(dae::BinaryOperator::Multiply, v, power_minus_one)?;
        if order == 1 {
            return self.derivative_product(
                dae::BinaryOperator::Multiply,
                Derivative::Expression(coefficient),
                first,
                provenance,
            );
        }
        let second = self.differentiate_order(base, 2, provenance)?;
        let along_second = self.derivative_product(
            dae::BinaryOperator::Multiply,
            Derivative::Expression(coefficient),
            second,
            provenance,
        )?;
        let square =
            self.derivative_product(dae::BinaryOperator::Multiply, first, first, provenance)?;
        let power_minus_two = scaled_power(self, 2.0)?;
        let one = self
            .target
            .at(provenance)
            .literal(dae::DaeLiteral::Real(1.0))?;
        let v_minus_one =
            self.target
                .at(provenance)
                .binary(dae::BinaryOperator::Subtract, v, one)?;
        let falling =
            self.target
                .at(provenance)
                .binary(dae::BinaryOperator::Multiply, v, v_minus_one)?;
        let curvature = self.target.at(provenance).binary(
            dae::BinaryOperator::Multiply,
            falling,
            power_minus_two,
        )?;
        let along_square = self.derivative_product(
            dae::BinaryOperator::Multiply,
            Derivative::Expression(curvature),
            square,
            provenance,
        )?;
        self.combine_sum(
            dae::BinaryOperator::Add,
            along_square,
            along_second,
            provenance,
        )
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

impl<'source, 'target> ExpressionRebuilder<'source, '_, '_, 'target> {
    /// `fill(s, n1, ..., nk)` is linear in its value `s`; its extents are
    /// checked structural integers, so `d/dt fill(s, n...) = fill(ds, n...)`.
    pub(super) fn differentiate_fill(
        &mut self,
        arguments: dae::ExpressionOperands<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let mut operands = arguments.iter();
        let value = operands.next().expect("a checked fill has a value operand");
        let Derivative::Expression(derivative) =
            self.differentiate_order(value, order, provenance)?
        else {
            return Ok(Derivative::Zero);
        };
        let mut rebuilt = vec![derivative];
        for extent in operands {
            rebuilt.push(self.materialize_exact_value(extent, provenance)?);
        }
        self.target
            .at(provenance)
            .builtin(dae::PureBuiltin::Fill, rebuilt)
            .map(Derivative::Expression)
    }
}
