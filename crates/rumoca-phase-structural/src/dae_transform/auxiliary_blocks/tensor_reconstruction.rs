//! Reconstruct coefficient values and their derivatives as aggregate operations.

use super::super::expressions::ExpressionRebuilder;
use super::tensor_expression::{Product, SourceValue, TensorExpression};
use rumoca_ir_dae as dae;

impl<'source, 'borrow, 'storage, 'target> ExpressionRebuilder<'source, 'borrow, 'storage, 'target> {
    pub(super) fn tensor_coefficient(
        &mut self,
        expression: &TensorExpression,
        order: u8,
        at: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        match expression {
            TensorExpression::Source(value) => self.auxiliary_operand(value, order, at),
            TensorExpression::Identity(extent) if order == 0 => {
                let size = self
                    .target
                    .at(at)
                    .literal(dae::DaeLiteral::Integer(i64::from(*extent)))?;
                self.target
                    .at(at)
                    .builtin(dae::PureBuiltin::Identity, [size])
            }
            TensorExpression::Identity(extent) => self.coefficient_zero(&[*extent, *extent], at),
            TensorExpression::Zero(dimensions) => self.coefficient_zero(dimensions, at),
            TensorExpression::Negate(value) => {
                let value = self.tensor_coefficient(value, order, at)?;
                self.target.at(at).unary(dae::UnaryOperator::Negate, value)
            }
            TensorExpression::Sum(operator, lhs, rhs) => {
                let lhs = self.tensor_coefficient(lhs, order, at)?;
                let rhs = self.tensor_coefficient(rhs, order, at)?;
                self.target.at(at).binary(*operator, lhs, rhs)
            }
            TensorExpression::Product(kind, lhs, rhs) => {
                self.coefficient_product(*kind, lhs, rhs, order, at)
            }
            TensorExpression::Index(base, index) => {
                let base = self.tensor_coefficient(base, order, at)?;
                let index = self.coefficient_index(index, at)?;
                self.target.at(at).index(
                    base,
                    [dae::Subscript::Index {
                        expression: index,
                        provenance: at,
                    }],
                )
            }
            TensorExpression::Array(elements) => {
                let elements = elements
                    .iter()
                    .map(|value| self.tensor_coefficient(value, order, at))
                    .collect::<Result<Vec<_>, _>>()?;
                self.target.at(at).array(elements)
            }
        }
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

    fn coefficient_zero(
        &mut self,
        dimensions: &[u32],
        at: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        if dimensions.is_empty() {
            return self.target.at(at).literal(dae::DaeLiteral::Real(0.0));
        }
        let sizes = dimensions
            .iter()
            .map(|&size| {
                self.target
                    .at(at)
                    .literal(dae::DaeLiteral::Integer(i64::from(size)))
            })
            .collect::<Result<Vec<_>, _>>()?;
        self.target.at(at).builtin(dae::PureBuiltin::Zeros, sizes)
    }

    fn coefficient_product(
        &mut self,
        kind: Product,
        lhs: &TensorExpression,
        rhs: &TensorExpression,
        order: u8,
        at: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        assert!(order <= 2, "coefficient derivative profile");
        let mut result = None;
        for left_order in 0..=order {
            let a = self.tensor_coefficient(lhs, left_order, at)?;
            let b = self.tensor_coefficient(rhs, order - left_order, at)?;
            let mut term = match kind {
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
            if order == 2 && left_order == 1 {
                let two = self.target.at(at).literal(dae::DaeLiteral::Real(2.0))?;
                term = self
                    .target
                    .at(at)
                    .binary(dae::BinaryOperator::Multiply, two, term)?;
            }
            result = Some(match result {
                Some(sum) => self
                    .target
                    .at(at)
                    .binary(dae::BinaryOperator::Add, sum, term)?,
                None => term,
            });
        }
        Ok(result.expect("at least the primal product"))
    }
}
