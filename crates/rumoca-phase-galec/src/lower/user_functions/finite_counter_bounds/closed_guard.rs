//! Exact scalar evaluation restricted to literals and this domain's binders.
//! Function inputs, mutable values, floating arithmetic and calls are unknown.
//! Fuel/depth exhaustion declines the proof; it cannot admit an index.
use super::*;

#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum ClosedGuard {
    Integer(i64),
    Boolean(bool),
}

impl ClosedGuard {
    pub(super) fn boolean(self) -> Option<bool> {
        match self {
            Self::Boolean(value) => Some(value),
            _ => None,
        }
    }
    fn integer(self) -> Option<i64> {
        match self {
            Self::Integer(value) => Some(value),
            _ => None,
        }
    }
    pub(super) fn evaluate<'dae>(
        view: dae::DaeView<'dae>,
        domain: dae::DomainId<'dae>,
        coordinates: &[i64],
        expression: dae::ExprId<'dae>,
        fuel: &mut usize,
    ) -> Option<Self> {
        evaluate(view, domain, coordinates, expression, fuel, 0)
    }
}

fn evaluate<'dae>(
    view: dae::DaeView<'dae>,
    domain: dae::DomainId<'dae>,
    coordinates: &[i64],
    expression: dae::ExprId<'dae>,
    fuel: &mut usize,
    depth: usize,
) -> Option<ClosedGuard> {
    *fuel = fuel.checked_sub(1)?;
    if depth > 64 {
        return None;
    }
    use ClosedGuard::{Boolean, Integer};
    use dae::ExpressionOperation as E;
    match view.expression(expression)?.operation() {
        E::Literal(dae::DaeLiteral::Integer(value)) => Some(Integer(*value)),
        E::Literal(dae::DaeLiteral::Boolean(value)) => Some(Boolean(*value)),
        E::Coordinate(dae::CoordinateView::Binder(binder)) if binder.domain() == domain => {
            Some(Integer(*coordinates.get(binder.ordinal() as usize)?))
        }
        E::Unary { operator, operand } => {
            let value = evaluate(view, domain, coordinates, operand, fuel, depth + 1)?;
            match operator {
                dae::UnaryOperator::Plus => Some(Integer(value.integer()?)),
                dae::UnaryOperator::Negate => value.integer()?.checked_neg().map(Integer),
                dae::UnaryOperator::Not => Some(Boolean(!value.boolean()?)),
            }
        }
        E::Binary { operator, lhs, rhs } => binary(
            operator,
            evaluate(view, domain, coordinates, lhs, fuel, depth + 1)?,
            evaluate(view, domain, coordinates, rhs, fuel, depth + 1)?,
        ),
        E::Conditional(operands) => {
            let mut values = operands.iter();
            while let Some(condition) = values.next() {
                let Some(branch) = values.next() else {
                    return evaluate(view, domain, coordinates, condition, fuel, depth + 1);
                };
                if evaluate(view, domain, coordinates, condition, fuel, depth + 1)?.boolean()? {
                    return evaluate(view, domain, coordinates, branch, fuel, depth + 1);
                }
            }
            None
        }
        _ => None,
    }
}

fn binary(
    operator: dae::BinaryOperator,
    lhs: ClosedGuard,
    rhs: ClosedGuard,
) -> Option<ClosedGuard> {
    use ClosedGuard::{Boolean, Integer};
    use dae::BinaryOperator as B;
    match operator {
        B::Add => lhs.integer()?.checked_add(rhs.integer()?).map(Integer),
        B::Subtract => lhs.integer()?.checked_sub(rhs.integer()?).map(Integer),
        B::Multiply => lhs.integer()?.checked_mul(rhs.integer()?).map(Integer),
        B::Less => Some(Boolean(lhs.integer()? < rhs.integer()?)),
        B::LessEqual => Some(Boolean(lhs.integer()? <= rhs.integer()?)),
        B::Greater => Some(Boolean(lhs.integer()? > rhs.integer()?)),
        B::GreaterEqual => Some(Boolean(lhs.integer()? >= rhs.integer()?)),
        B::Equal => Some(Boolean(lhs == rhs)),
        B::NotEqual => Some(Boolean(lhs != rhs)),
        B::And => Some(Boolean(lhs.boolean()? && rhs.boolean()?)),
        B::Or => Some(Boolean(lhs.boolean()? || rhs.boolean()?)),
        _ => None,
    }
}
