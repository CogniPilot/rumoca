//! Exact values of literal-only scalar expressions.
//!
//! A scalar of constant variability built from literals, constants, and the
//! arithmetic and generator builtins below has one IEEE value independent of
//! every run, so lowering stores that value once instead of materializing
//! each literal operand and the ops combining them. Transcendental builtins
//! are not evaluated here: the emitted C and the host library may round them
//! differently.

use super::*;

const MAXIMUM_DEPTH: usize = 64;

impl<'dae> ScalarCompiler<'_, 'dae> {
    /// The exact value of scalar `scalar` of a literal-only `expression`.
    pub(super) fn exact_literal(
        &self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Option<f64> {
        let value = self.exact_literal_at(expression, scalar, 0)?;
        value.is_finite().then_some(value)
    }

    fn exact_literal_at(
        &self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
        depth: usize,
    ) -> Option<f64> {
        let node = self.node(expression);
        if depth >= MAXIMUM_DEPTH
            || node.variability() != dae::ExpressionVariability::Constant
            || node.binder_domain().is_some()
        {
            return None;
        }
        let depth = depth + 1;
        match node.operation() {
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Real(value)) => Some(*value),
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(value)) => {
                Some(*value as f64)
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(parameter)) => {
                let variable = self.view.variable(parameter.into())?;
                (variable.role() == dae::VariableRole::Constant)
                    .then(|| variable.binding())
                    .flatten()
                    .and_then(|binding| self.exact_literal_at(binding, scalar, depth))
            }
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus,
                operand,
            } => self.exact_literal_at(operand, scalar, depth),
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Negate,
                operand,
            } => self
                .exact_literal_at(operand, scalar, depth)
                .map(|value| -value),
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                self.exact_binary(operator, lhs, rhs, scalar, depth)
            }
            dae::ExpressionOperation::Array(elements) => {
                let (element, element_scalar) = self.select_array(elements, scalar);
                self.exact_literal_at(element, element_scalar, depth)
            }
            dae::ExpressionOperation::Builtin { builtin, arguments } => self.exact_builtin(
                builtin,
                arguments,
                node.value_type().dimensions(),
                scalar,
                depth,
            ),
            _ => None,
        }
    }

    /// The operand scalar an exact identity (`x * 1`, `1 * x`, `x / 1`,
    /// `x - (+0)`) leaves as the whole value of one broadcast or elementwise
    /// binary scalar.
    pub(super) fn identity_operand_scalar(
        &self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Option<(dae::ExprId<'dae>, usize)> {
        let (lhs_count, rhs_count) = (scalar_count(self.view, lhs), scalar_count(self.view, rhs));
        let elementwise = matches!(
            operator,
            dae::BinaryOperator::ElementwiseMultiply
                | dae::BinaryOperator::ElementwiseDivide
                | dae::BinaryOperator::ElementwiseSubtract
                | dae::BinaryOperator::Subtract
        );
        if !elementwise && lhs_count != 1 && rhs_count != 1 {
            return None;
        }
        let index = |count: usize| if count == 1 { 0 } else { scalar };
        let (lhs_index, rhs_index) = (index(lhs_count), index(rhs_count));
        let left = self.exact_literal(lhs, lhs_index);
        let right = self.exact_literal(rhs, rhs_index);
        match operator {
            dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply => {
                if right == Some(1.0) {
                    Some((lhs, lhs_index))
                } else if left == Some(1.0) {
                    Some((rhs, rhs_index))
                } else {
                    None
                }
            }
            dae::BinaryOperator::Divide | dae::BinaryOperator::ElementwiseDivide => {
                (right == Some(1.0)).then_some((lhs, lhs_index))
            }
            dae::BinaryOperator::Subtract | dae::BinaryOperator::ElementwiseSubtract => right
                .is_some_and(|value| value.to_bits() == 0)
                .then_some((lhs, lhs_index)),
            _ => None,
        }
    }

    fn exact_binary(
        &self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        scalar: usize,
        depth: usize,
    ) -> Option<f64> {
        if operator == dae::BinaryOperator::Multiply {
            let pairs = rumoca_eval_dae::multiplication_scalar_pairs(
                self.node(lhs).value_type().dimensions(),
                self.node(rhs).value_type().dimensions(),
                scalar,
            );
            let mut sum: Option<f64> = None;
            for (lhs_index, rhs_index) in pairs {
                let term = self.exact_literal_at(lhs, lhs_index, depth)?
                    * self.exact_literal_at(rhs, rhs_index, depth)?;
                sum = Some(sum.map_or(term, |partial| partial + term));
            }
            return sum;
        }
        let operand = |expression: dae::ExprId<'dae>| {
            let index = if scalar_count(self.view, expression) == 1 {
                0
            } else {
                scalar
            };
            self.exact_literal_at(expression, index, depth)
        };
        let (left, right) = (operand(lhs)?, operand(rhs)?);
        Some(match operator {
            dae::BinaryOperator::Add | dae::BinaryOperator::ElementwiseAdd => left + right,
            dae::BinaryOperator::Subtract | dae::BinaryOperator::ElementwiseSubtract => {
                left - right
            }
            dae::BinaryOperator::ElementwiseMultiply => left * right,
            dae::BinaryOperator::Divide | dae::BinaryOperator::ElementwiseDivide => left / right,
            _ => return None,
        })
    }

    fn exact_builtin(
        &self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        dimensions: &[u32],
        scalar: usize,
        depth: usize,
    ) -> Option<f64> {
        let argument = |ordinal: usize, index: usize| {
            self.exact_literal_at(arguments.get(ordinal)?, index, depth)
        };
        match builtin {
            dae::PureBuiltin::Zeros => Some(0.0),
            dae::PureBuiltin::Ones => Some(1.0),
            dae::PureBuiltin::Fill => argument(0, 0),
            dae::PureBuiltin::Identity => {
                let [_, columns] = dimensions else {
                    return None;
                };
                let columns = *columns as usize;
                Some(f64::from(u8::from(scalar / columns == scalar % columns)))
            }
            dae::PureBuiltin::OuterProduct => {
                let [_, columns] = dimensions else {
                    return None;
                };
                let columns = *columns as usize;
                Some(argument(0, scalar / columns)? * argument(1, scalar % columns)?)
            }
            dae::PureBuiltin::Transpose => {
                let operand = arguments.get(0)?;
                let selector = ScalarSelector::from_points(self.view, &self.domain_points);
                self.exact_literal_at(operand, selector.transpose_scalar(operand, scalar), depth)
            }
            dae::PureBuiltin::Skew => {
                let (index, negate) = match scalar {
                    0 | 4 | 8 => return Some(0.0),
                    1 => (2, true),
                    2 => (1, false),
                    3 => (2, false),
                    5 => (0, true),
                    6 => (1, true),
                    7 => (0, false),
                    _ => return None,
                };
                argument(0, index).map(|value| if negate { -value } else { value })
            }
            _ => None,
        }
    }
}
