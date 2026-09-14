//! Reconstruct coefficient values while retaining exact structural derivative zeros.

#[cfg(test)]
mod tests;

use super::super::differentiation::Derivative;
use super::super::expressions::{ExpressionRebuilder, shaped_zero};
use super::tensor_expression::{Product, SourceValue, TensorExpression};
use rumoca_ir_dae as dae;

enum CoefficientUnary<'a> {
    Negate,
    Transpose,
    Index(&'a SourceValue),
    Projection(u32),
}

impl<'source, 'borrow, 'storage, 'target> ExpressionRebuilder<'source, 'borrow, 'storage, 'target> {
    pub(super) fn tensor_coefficient(
        &mut self,
        expression: &TensorExpression,
        order: u8,
        at: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        match self.tensor_coefficient_value(expression, order, at)? {
            Derivative::Expression(value) => Ok(value),
            Derivative::Zero => {
                // Auxiliary admission proves every primal coefficient is materializable.
                let primal = self.tensor_coefficient(expression, 0, at)?;
                let dimensions = self.target.value_type(primal, at)?.dimensions().to_vec();
                shaped_zero(self.target, &dimensions, at)
            }
        }
    }

    pub(super) fn tensor_coefficient_value(
        &mut self,
        expression: &TensorExpression,
        order: u8,
        at: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        match expression {
            TensorExpression::Source(value) if order > 0 => {
                self.auxiliary_derivative(value, order, at)
            }
            TensorExpression::Source(value) => self
                .auxiliary_operand(value, 0, at)
                .map(Derivative::Expression),
            TensorExpression::Shared {
                source,
                variable,
                offset,
                value,
            } => {
                let expression = self
                    .source
                    .expression_id(source.expression as usize)
                    .unwrap();
                let previous =
                    std::mem::replace(&mut self.function_context, source.context(self.source));
                let key = (
                    self.scoped_reconstruction_key(expression, order, at),
                    *variable,
                    *offset,
                );
                self.function_context = previous;
                if let Some(&value) = self.scoped_cache.coefficients.get(&key) {
                    return Ok(value);
                }
                let result = self.tensor_coefficient_value(value, order, at)?;
                self.scoped_cache.coefficients.insert(key, result);
                Ok(result)
            }
            TensorExpression::One | TensorExpression::Identity(_) | TensorExpression::Zero(_)
                if order > 0 =>
            {
                Ok(Derivative::Zero)
            }
            TensorExpression::One => self
                .target
                .at(at)
                .literal(dae::DaeLiteral::Real(1.0))
                .map(Derivative::Expression),
            TensorExpression::Identity(extent) => {
                let size = self
                    .target
                    .at(at)
                    .literal(dae::DaeLiteral::Integer(i64::from(*extent)))?;
                self.target
                    .at(at)
                    .builtin(dae::PureBuiltin::Identity, [size])
                    .map(Derivative::Expression)
            }
            TensorExpression::Zero(dimensions) => {
                shaped_zero(self.target, dimensions, at).map(Derivative::Expression)
            }
            TensorExpression::Sum(operator, lhs, rhs) => {
                self.coefficient_sum(*operator, lhs, rhs, order, at)
            }
            TensorExpression::Product(kind, lhs, rhs) => {
                self.coefficient_product(*kind, lhs, rhs, order, at)
            }
            TensorExpression::Array(elements) => self.coefficient_array(elements, order, at),
            TensorExpression::Negate(base) => {
                self.coefficient_unary(CoefficientUnary::Negate, base, order, at)
            }
            TensorExpression::Transpose(base) => {
                self.coefficient_unary(CoefficientUnary::Transpose, base, order, at)
            }
            TensorExpression::Index(base, index) => {
                self.coefficient_unary(CoefficientUnary::Index(index), base, order, at)
            }
            TensorExpression::Projection(base, ordinal) => {
                self.coefficient_unary(CoefficientUnary::Projection(*ordinal), base, order, at)
            }
        }
    }

    fn coefficient_sum(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: &TensorExpression,
        rhs: &TensorExpression,
        order: u8,
        at: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        if order > 0 {
            let lhs = self.tensor_coefficient_value(lhs, order, at)?;
            let rhs = self.tensor_coefficient_value(rhs, order, at)?;
            return self.combine_sum(operator, lhs, rhs, at);
        }
        let lhs = self.tensor_coefficient(lhs, 0, at)?;
        let rhs = self.tensor_coefficient(rhs, 0, at)?;
        let value = match self.combine_sum(
            operator,
            Derivative::Expression(lhs),
            Derivative::Expression(rhs),
            at,
        )? {
            Derivative::Expression(value) => value,
            Derivative::Zero => {
                // Primal cancellation is between identical checked operands.
                let dimensions = self.target.value_type(lhs, at)?.dimensions().to_vec();
                shaped_zero(self.target, &dimensions, at)?
            }
        };
        Ok(Derivative::Expression(value))
    }

    fn coefficient_unary(
        &mut self,
        operation: CoefficientUnary<'_>,
        base: &TensorExpression,
        order: u8,
        at: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let Derivative::Expression(base) = self.tensor_coefficient_value(base, order, at)? else {
            return Ok(Derivative::Zero);
        };
        let result = match operation {
            CoefficientUnary::Negate => self.target.at(at).unary(dae::UnaryOperator::Negate, base),
            CoefficientUnary::Transpose => self
                .target
                .at(at)
                .builtin(dae::PureBuiltin::Transpose, [base]),
            CoefficientUnary::Index(index) => {
                let index = self.coefficient_index(index, at)?;
                self.target.at(at).index(
                    base,
                    [dae::Subscript::Index {
                        expression: index,
                        provenance: at,
                    }],
                )
            }
            CoefficientUnary::Projection(ordinal) => {
                let index = self
                    .target
                    .at(at)
                    .literal(dae::DaeLiteral::Integer(i64::from(ordinal) + 1))?;
                self.target.at(at).index(
                    base,
                    [dae::Subscript::Index {
                        expression: index,
                        provenance: at,
                    }],
                )
            }
        };
        result.map(Derivative::Expression)
    }

    fn coefficient_array(
        &mut self,
        elements: &[TensorExpression],
        order: u8,
        at: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let values = elements
            .iter()
            .map(|e| self.tensor_coefficient_value(e, order, at))
            .collect::<Result<Vec<_>, _>>()?;
        if order > 0 && values.iter().all(|v| matches!(v, Derivative::Zero)) {
            return Ok(Derivative::Zero);
        }
        let values = elements
            .iter()
            .zip(values)
            .map(|(element, value)| match value {
                Derivative::Expression(value) => Ok(value),
                Derivative::Zero => self.tensor_coefficient(element, order, at),
            })
            .collect::<Result<Vec<_>, _>>()?;
        self.target.at(at).array(values).map(Derivative::Expression)
    }

    fn coefficient_index(
        &mut self,
        index: &SourceValue,
        at: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        let source = self
            .source
            .expression_id(index.expression as usize)
            .unwrap();
        let previous = std::mem::replace(&mut self.function_context, index.context(self.source));
        let result = self.materialize_exact_value(source, at);
        self.function_context = previous;
        result
    }

    fn coefficient_product(
        &mut self,
        kind: Product,
        lhs: &TensorExpression,
        rhs: &TensorExpression,
        order: u8,
        at: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        assert!(order <= 2, "coefficient derivative profile");
        let mut result = Derivative::Zero;
        for left_order in 0..=order {
            let a = self.tensor_coefficient_value(lhs, left_order, at)?;
            let b = self.tensor_coefficient_value(rhs, order - left_order, at)?;
            let (Derivative::Expression(a), Derivative::Expression(b)) = (a, b) else {
                continue;
            };
            let term = match kind {
                Product::Multiply => {
                    self.target
                        .at(at)
                        .binary(dae::BinaryOperator::Multiply, a, b)?
                }
                Product::Outer => self
                    .target
                    .at(at)
                    .builtin(dae::PureBuiltin::OuterProduct, [a, b])?,
            };
            let mut term = Derivative::Expression(term);
            if order == 2 && left_order == 1 {
                term = self.twice(term, at)?;
            }
            result = self.combine_sum(dae::BinaryOperator::Add, result, term, at)?;
        }
        Ok(result)
    }
}
