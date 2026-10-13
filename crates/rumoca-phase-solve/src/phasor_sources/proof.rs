//! Query-local completed reductions over immutable original source identities.

use std::cell::RefCell;

use super::*;

type FrameKey<'dae> = Vec<dae::ExprId<'dae>>;
type ExpressionKey<'dae> = (dae::ExprId<'dae>, FrameKey<'dae>);
pub(super) type StateKey<'dae> = (
    dae::FunctionConditionalId<'dae>,
    dae::FunctionValueId<'dae>,
    FrameKey<'dae>,
);

pub(super) struct Memo<K, T> {
    completed: RefCell<HashMap<K, Option<T>>>,
    #[cfg(test)]
    reductions: std::cell::Cell<usize>,
}

impl<K, T> Default for Memo<K, T> {
    fn default() -> Self {
        Self {
            completed: RefCell::new(HashMap::new()),
            #[cfg(test)]
            reductions: std::cell::Cell::new(0),
        }
    }
}

impl<K: Eq + std::hash::Hash, T: Copy> Memo<K, T> {
    pub(super) fn get_or_compute(&self, key: K, compute: impl FnOnce() -> Option<T>) -> Option<T> {
        if let Some(completed) = self.completed(&key) {
            return completed;
        }
        let result = compute();
        self.complete(key, result);
        result
    }

    /// The completed reduction of `key`, when it has one.
    pub(super) fn completed(&self, key: &K) -> Option<Option<T>> {
        self.completed.borrow().get(key).copied()
    }

    /// Record the reduction of `key`.
    pub(super) fn complete(&self, key: K, result: Option<T>) {
        #[cfg(test)]
        self.reductions.set(self.reductions.get() + 1);
        self.completed.borrow_mut().insert(key, result);
    }

    #[cfg(test)]
    fn reductions(&self) -> usize {
        self.reductions.get()
    }
}

#[derive(Default)]
pub(super) struct ProofQuery<'dae> {
    pub(super) angle_operands: Memo<ExpressionKey<'dae>, AngleOperands>,
    pub(super) scalar_coordinate: Memo<ExpressionKey<'dae>, ScalarCoordinate>,
    pub(super) literal: Memo<ExpressionKey<'dae>, NumericSourceScalar>,
    pub(super) angle_states: Memo<StateKey<'dae>, AngleOperands>,
    pub(super) coordinate_states: Memo<StateKey<'dae>, ScalarCoordinate>,
    pub(super) literal_states: Memo<StateKey<'dae>, NumericSourceScalar>,
}

impl ProofQuery<'_> {
    #[cfg(test)]
    pub(super) fn completed_count(&self) -> usize {
        self.angle_operands.reductions()
            + self.scalar_coordinate.reductions()
            + self.literal.reductions()
            + self.angle_states.reductions()
            + self.coordinate_states.reductions()
            + self.literal_states.reductions()
    }
}

fn frame_key<'dae>(frame: Option<&CallFrame<'_, 'dae>>) -> FrameKey<'dae> {
    let mut calls = Vec::new();
    let mut frame = frame;
    while let Some(call) = frame {
        calls.push(call.call);
        frame = call.parent;
    }
    calls
}

pub(super) fn expression_key<'dae>(
    expression: dae::ExprId<'dae>,
    frame: Option<&CallFrame<'_, 'dae>>,
) -> ExpressionKey<'dae> {
    (expression, frame_key(frame))
}

pub(super) fn state_key<'dae>(
    conditional: dae::FunctionConditionalId<'dae>,
    value: dae::FunctionValueId<'dae>,
    frame: Option<&CallFrame<'_, 'dae>>,
) -> StateKey<'dae> {
    (conditional, value, frame_key(frame))
}

pub(super) trait SameProof: Copy {
    fn same(self, other: Self) -> bool;
}

impl SameProof for AngleOperands {
    fn same(self, other: Self) -> bool {
        self == other
    }
}
impl SameProof for ScalarCoordinate {
    fn same(self, other: Self) -> bool {
        self == other
    }
}
impl SameProof for NumericSourceScalar {
    fn same(self, other: Self) -> bool {
        std::mem::discriminant(&self) == std::mem::discriminant(&other) && self == other
    }
}

fn finite(value: NumericSourceScalar) -> Option<NumericSourceScalar> {
    match value {
        NumericSourceScalar::Invalid | NumericSourceScalar::Enumeration(_) => None,
        NumericSourceScalar::Real(value) if !value.is_finite() => None,
        value => Some(value),
    }
}

pub(super) fn typed_literal(
    value: NumericSourceScalar,
    scalar_type: dae::ScalarType,
) -> Option<NumericSourceScalar> {
    use NumericSourceScalar::{Boolean, Integer, Real};
    match (scalar_type, value) {
        (dae::ScalarType::Real, value @ (Real(_) | Integer(_))) => finite(Real(value.real())),
        (dae::ScalarType::Integer, value @ Integer(_))
        | (dae::ScalarType::Boolean, value @ Boolean(_)) => Some(value),
        _ => None,
    }
}

pub(super) fn unary_literal(
    operator: dae::UnaryOperator,
    value: NumericSourceScalar,
) -> Option<NumericSourceScalar> {
    match (operator, value) {
        (dae::UnaryOperator::Not, NumericSourceScalar::Boolean(value)) => {
            Some(NumericSourceScalar::Boolean(!value))
        }
        (
            dae::UnaryOperator::Plus,
            value @ (NumericSourceScalar::Integer(_) | NumericSourceScalar::Real(_)),
        ) => finite(value),
        (
            dae::UnaryOperator::Negate,
            value @ (NumericSourceScalar::Integer(_) | NumericSourceScalar::Real(_)),
        ) => finite(-value),
        _ => None,
    }
}

fn numeric_pair(lhs: NumericSourceScalar, rhs: NumericSourceScalar) -> bool {
    use NumericSourceScalar::{Integer, Real};
    match (lhs, rhs) {
        (Integer(_), Integer(_)) | (Real(_), Real(_)) => true,
        (Integer(value), Real(_)) | (Real(_), Integer(value)) => {
            (-9_007_199_254_740_992..=9_007_199_254_740_992).contains(&value)
        }
        _ => false,
    }
}

pub(super) fn binary_literal(
    operator: dae::BinaryOperator,
    lhs: NumericSourceScalar,
    rhs: NumericSourceScalar,
) -> Option<NumericSourceScalar> {
    use NumericSourceScalar::{Boolean, Integer};
    use dae::BinaryOperator as Op;
    if let (Boolean(lhs), Boolean(rhs)) = (lhs, rhs) {
        return Some(Boolean(match operator {
            Op::And => lhs && rhs,
            Op::Or => lhs || rhs,
            Op::Equal => lhs == rhs,
            Op::NotEqual => lhs != rhs,
            _ => return None,
        }));
    }
    if !numeric_pair(lhs, rhs) {
        return None;
    }
    finite(match operator {
        Op::Add => lhs + rhs,
        Op::Subtract => lhs - rhs,
        Op::Multiply => lhs * rhs,
        // Division's Real result requires conversion; never round a wide Integer to select an arm.
        Op::Divide
            if !matches!(lhs, Integer(value) if value.unsigned_abs() > 9_007_199_254_740_992)
                && !matches!(rhs, Integer(value) if value.unsigned_abs() > 9_007_199_254_740_992) =>
        {
            lhs / rhs
        }
        Op::Equal => Boolean(lhs == rhs),
        Op::NotEqual => Boolean(lhs != rhs),
        Op::Less => Boolean(lhs < rhs),
        Op::LessEqual => Boolean(lhs <= rhs),
        Op::Greater => Boolean(lhs > rhs),
        Op::GreaterEqual => Boolean(lhs >= rhs),
        _ => return None,
    })
}
