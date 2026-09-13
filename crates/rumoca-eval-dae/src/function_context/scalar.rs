//! Partial scalar evaluation for compile-time function selectors.
//!
//! Unknown inputs stay symbolic. Array projections select existing source
//! elements; they never instantiate tensor coordinates or a function body.

use std::collections::BTreeSet;

use rumoca_core::{IntegerBinaryOperator, eval_integer_binary};
use rumoca_ir_dae as dae;

use super::FunctionCallContext;

type VisitKey = (u32, Vec<u32>, Vec<usize>);

struct ScalarSelection<'dae> {
    view: dae::DaeView<'dae>,
    active: BTreeSet<VisitKey>,
}

impl<'dae> FunctionCallContext<'dae> {
    /// Select a source branch only when its scalar guard is proved constant
    /// under this exact call context. Parameter bindings are not constants.
    pub fn selected_branch(
        &self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
    ) -> Option<dae::ExprId<'dae>> {
        let dae::ExpressionOperation::Conditional(operands) =
            view.expression(expression)?.operation()
        else {
            return None;
        };
        let context = self.scoped_to_expression(view, expression);
        ScalarSelection {
            view,
            active: BTreeSet::new(),
        }
        .branch(operands, &context)
    }
}

impl<'dae> ScalarSelection<'dae> {
    fn branch(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> Option<dae::ExprId<'dae>> {
        for ordinal in (0..operands.len().checked_sub(1)?).step_by(2) {
            let dae::DaeLiteral::Boolean(condition) =
                self.resolve(operands.get(ordinal)?, context, &[])?
            else {
                return None;
            };
            if condition {
                return operands.get(ordinal + 1);
            }
        }
        operands.get(operands.len() - 1)
    }

    fn resolve(
        &mut self,
        expression: dae::ExprId<'dae>,
        context: &FunctionCallContext<'dae>,
        indices: &[usize],
    ) -> Option<dae::DaeLiteral> {
        let context = context.scoped_to_expression(self.view, expression);
        let key = (
            expression.index(),
            context
                .frames
                .iter()
                .map(|frame| frame.call.index())
                .collect(),
            indices.to_vec(),
        );
        if !self.active.insert(key.clone()) {
            return None;
        }
        let value = self.resolve_operation(expression, &context, indices);
        self.active.remove(&key);
        value
    }

    fn resolve_operation(
        &mut self,
        expression: dae::ExprId<'dae>,
        context: &FunctionCallContext<'dae>,
        indices: &[usize],
    ) -> Option<dae::DaeLiteral> {
        if let Some((result, nested)) = context.call_result(self.view, expression) {
            return self.resolve(result, &nested, indices);
        }
        match self.view.expression(expression)?.operation() {
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => self.resolve(context.parameter_argument(parameter)?, context, indices),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(parameter)) => {
                let variable = self.view.variable(parameter.into())?;
                if variable.role() != dae::VariableRole::Constant {
                    return None;
                }
                self.resolve(variable.binding()?, context, indices)
            }
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                self.resolve(definition.rhs(), context, indices)
            }
            dae::ExpressionOperation::Conditional(operands) => {
                let branch = self.branch(operands, context)?;
                self.resolve(branch, context, indices)
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                let mut selected = self.indices(subscripts, context)?;
                selected.extend_from_slice(indices);
                self.resolve(base, context, &selected)
            }
            dae::ExpressionOperation::Field { base, field } => {
                let (field, nested) = context.projected_field(self.view, base, field)?;
                self.resolve(field, &nested, indices)
            }
            dae::ExpressionOperation::Array(elements) => {
                let (&index, remaining) = indices.split_first()?;
                self.resolve(elements.get(index)?, context, remaining)
            }
            operation if indices.is_empty() => self.scalar(operation, context),
            _ => None,
        }
    }

    fn indices(
        &mut self,
        subscripts: dae::SubscriptsView<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> Option<Vec<usize>> {
        subscripts
            .iter()
            .map(|subscript| {
                let dae::SubscriptView::Index { expression, .. } = subscript else {
                    return None;
                };
                let dae::DaeLiteral::Integer(index) = self.resolve(expression, context, &[])?
                else {
                    return None;
                };
                usize::try_from(index).ok()?.checked_sub(1)
            })
            .collect()
    }

    fn scalar(
        &mut self,
        operation: dae::ExpressionOperation<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> Option<dae::DaeLiteral> {
        match operation {
            dae::ExpressionOperation::Literal(literal) => Some(literal.clone()),
            dae::ExpressionOperation::Unary { operator, operand } => {
                unary(operator, self.resolve(operand, context, &[])?)
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => binary(
                operator,
                self.resolve(lhs, context, &[])?,
                self.resolve(rhs, context, &[])?,
            ),
            _ => None,
        }
    }
}

fn unary(operator: dae::UnaryOperator, value: dae::DaeLiteral) -> Option<dae::DaeLiteral> {
    use dae::{DaeLiteral as Value, UnaryOperator as Op};
    match (operator, value) {
        (Op::Plus, value @ (Value::Integer(_) | Value::Real(_))) => Some(value),
        (Op::Negate, Value::Integer(value)) => value.checked_neg().map(Value::Integer),
        (Op::Negate, Value::Real(value)) if value.is_finite() => Some(Value::Real(-value)),
        (Op::Not, Value::Boolean(value)) => Some(Value::Boolean(!value)),
        _ => None,
    }
}

fn binary(
    operator: dae::BinaryOperator,
    lhs: dae::DaeLiteral,
    rhs: dae::DaeLiteral,
) -> Option<dae::DaeLiteral> {
    use dae::{BinaryOperator as Op, DaeLiteral as Value};
    match (lhs, rhs) {
        (Value::Integer(lhs), Value::Integer(rhs)) => integer_binary(operator, lhs, rhs),
        (Value::Enumeration(lhs), Value::Enumeration(rhs)) => {
            comparison(operator, lhs.cmp(&rhs)).map(Value::Boolean)
        }
        (Value::Boolean(lhs), Value::Boolean(rhs)) => match operator {
            Op::And => Some(Value::Boolean(lhs && rhs)),
            Op::Or => Some(Value::Boolean(lhs || rhs)),
            _ => comparison(operator, lhs.cmp(&rhs)).map(Value::Boolean),
        },
        (Value::Real(lhs), Value::Real(rhs)) => real_binary(operator, lhs, rhs),
        (Value::Real(lhs), Value::Integer(rhs)) => real_binary(operator, lhs, rhs as f64),
        (Value::Integer(lhs), Value::Real(rhs)) => real_binary(operator, lhs as f64, rhs),
        (Value::String(lhs), Value::String(rhs)) => {
            comparison(operator, lhs.cmp(&rhs)).map(Value::Boolean)
        }
        _ => None,
    }
}

fn integer_binary(operator: dae::BinaryOperator, lhs: i64, rhs: i64) -> Option<dae::DaeLiteral> {
    use dae::BinaryOperator as Op;
    let arithmetic = match operator {
        Op::Add | Op::ElementwiseAdd => IntegerBinaryOperator::Add,
        Op::Subtract | Op::ElementwiseSubtract => IntegerBinaryOperator::Sub,
        Op::Multiply | Op::ElementwiseMultiply => IntegerBinaryOperator::Mul,
        Op::Divide | Op::ElementwiseDivide => return real_binary(operator, lhs as f64, rhs as f64),
        _ => return comparison(operator, lhs.cmp(&rhs)).map(dae::DaeLiteral::Boolean),
    };
    eval_integer_binary(arithmetic, lhs, rhs).map(dae::DaeLiteral::Integer)
}

fn real_binary(operator: dae::BinaryOperator, lhs: f64, rhs: f64) -> Option<dae::DaeLiteral> {
    use dae::BinaryOperator as Op;
    if !lhs.is_finite() || !rhs.is_finite() {
        return None;
    }
    if let Some(value) = comparison(operator, lhs.partial_cmp(&rhs)?) {
        return Some(dae::DaeLiteral::Boolean(value));
    }
    if !matches!(
        operator,
        Op::Add
            | Op::Subtract
            | Op::Multiply
            | Op::Divide
            | Op::ElementwiseAdd
            | Op::ElementwiseSubtract
            | Op::ElementwiseMultiply
            | Op::ElementwiseDivide
    ) {
        return None;
    }
    let value = crate::numeric::binary(operator, lhs, rhs);
    value.is_finite().then_some(dae::DaeLiteral::Real(value))
}

fn comparison(operator: dae::BinaryOperator, ordering: std::cmp::Ordering) -> Option<bool> {
    use dae::BinaryOperator as Op;
    match operator {
        Op::Equal => Some(ordering.is_eq()),
        Op::NotEqual => Some(!ordering.is_eq()),
        Op::Less => Some(ordering.is_lt()),
        Op::LessEqual => Some(!ordering.is_gt()),
        Op::Greater => Some(ordering.is_gt()),
        Op::GreaterEqual => Some(!ordering.is_lt()),
        _ => None,
    }
}
