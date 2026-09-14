//! A source expression's affine map in one complete vector coordinate.

use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use rumoca_eval_dae::FunctionCallContext;
use rumoca_ir_dae as dae;

use super::super::super::constraints::{DifferentiationFacts, exact_state_anchor};
use super::super::super::equalities::EqualitySign;
use super::super::tensor_expression::{Product, SourceValue, TensorExpression};

#[derive(Clone)]
pub(super) struct AffineValue {
    pub(super) coefficient: Option<TensorExpression>,
    pub(super) offset: TensorExpression,
}

pub(super) struct AffineMap<'dae, 'facts> {
    view: dae::DaeView<'dae>,
    facts: &'facts DifferentiationFacts,
    variable: u32,
    extent: u32,
    active: BTreeSet<SourceValue>,
    cache: BTreeMap<SourceValue, Option<AffineValue>>,
}

impl<'dae, 'facts> AffineMap<'dae, 'facts> {
    pub(super) fn new(
        view: dae::DaeView<'dae>,
        facts: &'facts DifferentiationFacts,
        variable: u32,
        extent: u32,
    ) -> Self {
        Self {
            view,
            facts,
            variable,
            extent,
            active: BTreeSet::new(),
            cache: BTreeMap::new(),
        }
    }

    pub(super) fn expression(
        &mut self,
        expression: dae::ExprId<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> Option<AffineValue> {
        let context = context.scoped_to_expression(self.view, expression);
        let key = SourceValue::new(expression, &context);
        if let Some(value) = self.cache.get(&key) {
            return value.clone();
        }
        if !self.active.insert(key.clone()) {
            return None;
        }
        let value = self.operation(expression, &context);
        self.active.remove(&key);
        let value = value.map(|mut value| {
            if value.offset.is_zero() {
                value.offset = TensorExpression::Zero(
                    self.view
                        .expression(expression)
                        .unwrap()
                        .value_type()
                        .dimensions()
                        .into(),
                );
            }
            let shared = |coefficient, offset| TensorExpression::Shared {
                source: key.clone(),
                variable: Some(self.variable),
                offset,
                value: Arc::new(coefficient),
            };
            AffineValue {
                coefficient: value.coefficient.map(|c| shared(c, false)),
                offset: shared(value.offset, true),
            }
        });
        self.cache.insert(key, value.clone());
        value
    }

    pub(super) fn is_value_definition(&self, residual: dae::ExprId<'dae>) -> bool {
        let Some((lhs, rhs)) = crate::residual_normalization::equation_sides(self.view, residual)
        else {
            return false;
        };
        [(lhs, rhs), (rhs, lhs)].into_iter().any(|(target, value)| {
            let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(variable)) =
                self.view.expression(target).unwrap().operation()
            else {
                return false;
            };
            self.facts.algebraic_definition(self.view, variable) == Some(value)
        })
    }

    fn operation(
        &mut self,
        expression: dae::ExprId<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> Option<AffineValue> {
        if let Some(value) = self.independent(expression, context) {
            return Some(AffineValue {
                coefficient: None,
                offset: value,
            });
        }
        if let Some(branch) = context.selected_branch(self.view, expression) {
            return self.expression(branch, context);
        }
        let node = self.view.expression(expression)?;
        match node.operation() {
            dae::ExpressionOperation::Coordinate(coordinate) => {
                self.coordinate(expression, coordinate, context)
            }
            dae::ExpressionOperation::Call { .. } => {
                let (value, nested) = context.call_result(self.view, expression)?;
                self.expression(value, &nested)
            }
            dae::ExpressionOperation::Field { base, field } => {
                let (value, nested) = context.projected_field(self.view, base, field)?;
                self.expression(value, &nested)
            }
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus,
                operand,
            } => self.expression(operand, context),
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Negate,
                operand,
            } => {
                let value = self.expression(operand, context)?;
                Some(AffineValue {
                    coefficient: value
                        .coefficient
                        .map(|c| TensorExpression::Negate(Box::new(c))),
                    offset: TensorExpression::Negate(Box::new(value.offset)),
                })
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                self.binary(operator, lhs, rhs, context)
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                if subscripts.len() != 1 || !node.value_type().is_scalar() {
                    return None;
                }
                let dae::SubscriptView::Index {
                    expression: index, ..
                } = subscripts.get(0)?
                else {
                    return None;
                };
                if !self.invariant_index(index, context) {
                    return None;
                }
                let value = self.expression(base, context)?;
                let index = SourceValue::new(index, context);
                Some(AffineValue {
                    coefficient: value
                        .coefficient
                        .map(|c| TensorExpression::Index(Box::new(c), index.clone())),
                    offset: TensorExpression::Index(Box::new(value.offset), index),
                })
            }
            dae::ExpressionOperation::Array(elements)
                if node.value_type().dimensions().len() == 1 =>
            {
                let values = elements
                    .iter()
                    .map(|e| self.expression(e, context))
                    .collect::<Option<Vec<_>>>()?;
                let (coefficients, offsets): (Vec<_>, Vec<_>) = values
                    .into_iter()
                    .map(|v| {
                        (
                            v.coefficient
                                .unwrap_or_else(|| TensorExpression::Zero(Box::new([self.extent]))),
                            v.offset,
                        )
                    })
                    .unzip();
                Some(AffineValue {
                    coefficient: Some(TensorExpression::Array(coefficients.into_boxed_slice())),
                    offset: TensorExpression::Array(offsets.into_boxed_slice()),
                })
            }
            _ => None,
        }
    }

    fn coordinate(
        &mut self,
        expression: dae::ExprId<'dae>,
        coordinate: dae::CoordinateView<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> Option<AffineValue> {
        let variable = match coordinate {
            dae::CoordinateView::State(state) => Some(state.index()),
            dae::CoordinateView::Algebraic(variable) => Some(variable.index()),
            _ => None,
        };
        let sign = (variable == Some(self.variable))
            .then_some(EqualitySign::Same)
            .or_else(|| {
                exact_state_anchor(self.view, &self.facts.equalities, expression)
                    .filter(|(state, _)| state.index() == self.variable)
                    .map(|(_, sign)| sign)
            });
        if let Some(sign) = sign {
            let identity = TensorExpression::Identity(self.extent);
            return Some(AffineValue {
                coefficient: Some(if sign == EqualitySign::Opposite {
                    TensorExpression::Negate(Box::new(identity))
                } else {
                    identity
                }),
                offset: TensorExpression::Zero(Box::new([self.extent])),
            });
        }
        match coordinate {
            dae::CoordinateView::Algebraic(variable) => {
                let definition = self.facts.algebraic_definition(self.view, variable)?;
                self.expression(definition, context)
            }
            dae::CoordinateView::FunctionParameter(parameter) => {
                self.expression(context.parameter_argument(parameter)?, context)
            }
            _ => None,
        }
    }

    fn independent(
        &self,
        expression: dae::ExprId<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> Option<TensorExpression> {
        let anchors = self.facts.materialized_state_anchors_in_context(
            self.view,
            expression.index(),
            context,
        )?;
        (!anchors.contains(&self.variable)).then(|| {
            if self
                .facts
                .expression_is_zero(self.view, expression, context)
            {
                TensorExpression::Zero(
                    self.view
                        .expression(expression)
                        .unwrap()
                        .value_type()
                        .dimensions()
                        .into(),
                )
            } else {
                TensorExpression::Source(SourceValue::new(expression, context))
            }
        })
    }

    fn invariant_index(
        &self,
        expression: dae::ExprId<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> bool {
        let context = context.scoped_to_expression(self.view, expression);
        match self.view.expression(expression).unwrap().operation() {
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(_)) => true,
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(_)) => true,
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(p)) => {
                context
                    .parameter_argument(p)
                    .is_some_and(|a| self.invariant_index(a, &context))
            }
            _ => false,
        }
    }

    fn binary(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> Option<AffineValue> {
        use dae::BinaryOperator as B;
        if matches!(operator, B::Add | B::Subtract) {
            let lhs = self.expression(lhs, context)?;
            let rhs = self.expression(rhs, context)?;
            let coefficient = match (lhs.coefficient, rhs.coefficient) {
                (Some(a), Some(b)) => {
                    Some(TensorExpression::Sum(operator, Box::new(a), Box::new(b)))
                }
                (Some(a), None) => Some(a),
                (None, Some(b)) if operator == B::Subtract => {
                    Some(TensorExpression::Negate(Box::new(b)))
                }
                (None, b) => b,
            };
            return Some(AffineValue {
                coefficient,
                offset: TensorExpression::Sum(operator, Box::new(lhs.offset), Box::new(rhs.offset)),
            });
        }
        if operator != B::Multiply {
            return None;
        }
        for (dependent, independent, left) in [(lhs, rhs, true), (rhs, lhs, false)] {
            let Some(value) = self.independent(independent, context) else {
                continue;
            };
            let Some(dependent_value) = self.expression(dependent, context) else {
                continue;
            };
            let coefficient = dependent_value.coefficient?;
            let rank = self
                .view
                .expression(dependent)?
                .value_type()
                .dimensions()
                .len();
            let independent_rank = self
                .view
                .expression(independent)?
                .value_type()
                .dimensions()
                .len();
            let matrix = match (rank, independent_rank, left) {
                (0 | 1, 0, _) | (1, 2, false) => {
                    TensorExpression::product(Product::Multiply, value.clone(), coefficient)
                }
                (0, 1, _) => TensorExpression::product(Product::Outer, value.clone(), coefficient),
                (1, 1, _) => TensorExpression::product(
                    Product::Multiply,
                    TensorExpression::Transpose(Box::new(coefficient)),
                    value.clone(),
                ),
                _ => return None,
            };
            let offset = if left {
                TensorExpression::product(Product::Multiply, dependent_value.offset, value)
            } else {
                TensorExpression::product(Product::Multiply, value, dependent_value.offset)
            };
            return Some(AffineValue {
                coefficient: Some(matrix),
                offset,
            });
        }
        None
    }
}
