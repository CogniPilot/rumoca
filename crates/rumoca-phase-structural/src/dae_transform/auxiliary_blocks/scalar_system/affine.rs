//! Exact scalar affine decomposition over a closed arithmetic profile.

use std::collections::BTreeMap;
use std::sync::Arc;

use rumoca_ir_dae as dae;

use super::super::super::constraints::DifferentiationFacts;
use super::super::tensor_expression::{Product, SourceValue, TensorExpression};

#[derive(Clone)]
pub(super) struct AffineExpression {
    pub(super) terms: BTreeMap<u32, TensorExpression>,
    pub(super) offset: TensorExpression,
}

pub(super) struct AffineProof<'dae, 'facts> {
    view: dae::DaeView<'dae>,
    facts: &'facts DifferentiationFacts,
    cache: BTreeMap<u32, Option<AffineExpression>>,
}

impl<'dae, 'facts> AffineProof<'dae, 'facts> {
    pub(super) fn new(view: dae::DaeView<'dae>, facts: &'facts DifferentiationFacts) -> Self {
        Self {
            view,
            facts,
            cache: BTreeMap::new(),
        }
    }

    pub(super) fn expression(&mut self, root: dae::ExprId<'dae>) -> Option<AffineExpression> {
        let mut pending = vec![(root, false)];
        while let Some((expression, operands_ready)) = pending.pop() {
            if self.cache.contains_key(&expression.index()) {
                continue;
            }
            let node = self.view.expression(expression).unwrap();
            if !node.value_type().is_scalar() || node.binder_domain().is_some() {
                self.cache.insert(expression.index(), None);
                continue;
            }
            if !operands_ready {
                pending.push((expression, true));
                match node.operation() {
                    dae::ExpressionOperation::Unary {
                        operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
                        operand,
                    } => pending.push((operand, false)),
                    dae::ExpressionOperation::Binary {
                        operator:
                            dae::BinaryOperator::Add
                            | dae::BinaryOperator::Subtract
                            | dae::BinaryOperator::Multiply,
                        lhs,
                        rhs,
                    } => pending.extend([(rhs, false), (lhs, false)]),
                    _ => (),
                }
                continue;
            }
            let value = self
                .operation(expression, node.operation())
                .map(|value| value.shared(expression.index()));
            self.cache.insert(expression.index(), value);
        }
        self.cache.get(&root.index()).cloned().flatten()
    }

    fn operation(
        &self,
        expression: dae::ExprId<'dae>,
        operation: dae::ExpressionOperation<'dae>,
    ) -> Option<AffineExpression> {
        match operation {
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
                operand,
            } => {
                let value = self.cache[&operand.index()].clone()?;
                if matches!(
                    operation,
                    dae::ExpressionOperation::Unary {
                        operator: dae::UnaryOperator::Negate,
                        ..
                    }
                ) {
                    Some(value.scale(TensorExpression::Negate(Box::new(TensorExpression::One))))
                } else {
                    Some(value)
                }
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs }
                if matches!(
                    operator,
                    dae::BinaryOperator::Add
                        | dae::BinaryOperator::Subtract
                        | dae::BinaryOperator::Multiply
                ) =>
            {
                let lhs = self.cache[&lhs.index()].clone()?;
                let rhs = self.cache[&rhs.index()].clone()?;
                if lhs.terms.is_empty() && rhs.terms.is_empty() {
                    return Some(constant(expression));
                }
                match operator {
                    dae::BinaryOperator::Multiply if lhs.terms.is_empty() => {
                        Some(rhs.scale(lhs.offset))
                    }
                    dae::BinaryOperator::Multiply if rhs.terms.is_empty() => {
                        Some(lhs.scale(rhs.offset))
                    }
                    dae::BinaryOperator::Multiply => None,
                    _ => Some(lhs.sum(operator, rhs)),
                }
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(variable))
                if !self
                    .facts
                    .can_materialize_value(self.view, expression.index()) =>
            {
                let declaration = self.view.variable(variable.into())?;
                (declaration.value_type().scalar_type() == dae::ScalarType::Real
                    && declaration.variability() == dae::ExpressionVariability::Continuous)
                    .then(|| AffineExpression {
                        terms: BTreeMap::from([(variable.index(), TensorExpression::One)]),
                        offset: zero(),
                    })
            }
            _ => self
                .facts
                .can_materialize_value(self.view, expression.index())
                .then(|| constant(expression)),
        }
    }
}

fn constant(expression: dae::ExprId<'_>) -> AffineExpression {
    AffineExpression {
        terms: BTreeMap::new(),
        offset: TensorExpression::Source(SourceValue::model(expression.index())),
    }
}

pub(super) fn zero() -> TensorExpression {
    TensorExpression::Zero(Box::new([]))
}

impl AffineExpression {
    fn shared(self, source: u32) -> Self {
        let share = |variable, value| TensorExpression::Shared {
            source,
            variable,
            value: Arc::new(value),
        };
        Self {
            terms: self
                .terms
                .into_iter()
                .map(|(variable, value)| (variable, share(Some(variable), value)))
                .collect(),
            offset: share(None, self.offset),
        }
    }

    fn scale(self, factor: TensorExpression) -> Self {
        let product = |value| TensorExpression::product(Product::Multiply, factor.clone(), value);
        Self {
            terms: self
                .terms
                .into_iter()
                .map(|(v, c)| (v, product(c)))
                .collect(),
            offset: product(self.offset),
        }
    }

    fn sum(mut self, operator: dae::BinaryOperator, rhs: Self) -> Self {
        for (variable, coefficient) in rhs.terms {
            let lhs = self.terms.remove(&variable).unwrap_or_else(zero);
            self.terms.insert(
                variable,
                TensorExpression::Sum(operator, Box::new(lhs), Box::new(coefficient)),
            );
        }
        self.offset = TensorExpression::Sum(operator, Box::new(self.offset), Box::new(rhs.offset));
        self
    }
}
