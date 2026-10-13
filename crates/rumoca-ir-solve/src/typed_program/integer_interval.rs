//! The one Integer value-interval analysis of the Solve IR (SPEC_0040
//! SOLVE-C102).
//!
//! An interval is bounded by an Integer domain, not by `i64`: a value or an
//! intermediate result outside the domain has no interval, because the checked
//! operation that computes it faults. Phase lowering over DAE expressions and
//! the fault flow of a typed program both build their intervals from these
//! operations, and the scalar folds below define what a constant operation
//! computes, so no consumer carries a second copy of the arithmetic.

use super::{
    SolveBinaryOperator, SolveCompareOperator, SolveIntegerDomain, SolveScalarType,
    SolveUnaryOperator,
};

/// A closed range of Integer values that lies inside an Integer domain.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SolveIntegerInterval {
    low: i64,
    high: i64,
}

impl SolveIntegerInterval {
    /// The one value `value`, when it lies in `domain`.
    #[must_use]
    pub fn point(domain: SolveIntegerDomain, value: i64) -> Option<Self> {
        Self::span(domain, value, value)
    }

    /// The values between `first` and `second` inclusive, in either order,
    /// when both lie in `domain`.
    #[must_use]
    pub fn span(domain: SolveIntegerDomain, first: i64, second: i64) -> Option<Self> {
        let (low, high) = (first.min(second), first.max(second));
        (domain.contains(low) && domain.contains(high)).then_some(Self { low, high })
    }

    #[must_use]
    pub const fn low(self) -> i64 {
        self.low
    }

    #[must_use]
    pub const fn high(self) -> i64 {
        self.high
    }

    /// Whether `value` is a member.
    #[must_use]
    pub const fn contains(self, value: i64) -> bool {
        self.low <= value && value <= self.high
    }

    /// Whether every member lies in `low..=high`.
    #[must_use]
    pub const fn within(self, low: i64, high: i64) -> bool {
        low <= self.low && self.high <= high
    }

    /// Negation, when no member overflows `domain`.
    #[must_use]
    pub fn negate(self, domain: SolveIntegerDomain) -> Option<Self> {
        Self::span(domain, self.high.checked_neg()?, self.low.checked_neg()?)
    }

    /// Absolute value, when no member overflows `domain`.
    #[must_use]
    pub fn abs(self, domain: SolveIntegerDomain) -> Option<Self> {
        let high = self.low.checked_abs()?.max(self.high.checked_abs()?);
        let low = if self.contains(0) {
            0
        } else {
            self.low.abs().min(self.high.abs())
        };
        Self::span(domain, low, high)
    }

    /// Sum, when no pair of members overflows `domain`.
    #[must_use]
    pub fn add(self, other: Self, domain: SolveIntegerDomain) -> Option<Self> {
        Self::span(
            domain,
            self.low.checked_add(other.low)?,
            self.high.checked_add(other.high)?,
        )
    }

    /// Difference, when no pair of members overflows `domain`.
    #[must_use]
    pub fn subtract(self, other: Self, domain: SolveIntegerDomain) -> Option<Self> {
        Self::span(
            domain,
            self.low.checked_sub(other.high)?,
            self.high.checked_sub(other.low)?,
        )
    }

    /// Product, when no pair of members overflows `domain`.
    #[must_use]
    pub fn multiply(self, other: Self, domain: SolveIntegerDomain) -> Option<Self> {
        let corners = [
            self.low.checked_mul(other.low)?,
            self.low.checked_mul(other.high)?,
            self.high.checked_mul(other.low)?,
            self.high.checked_mul(other.high)?,
        ];
        Self::span(domain, *corners.iter().min()?, *corners.iter().max()?)
    }

    /// The smaller of two values; it never leaves the operands' range.
    #[must_use]
    pub fn minimum(self, other: Self) -> Self {
        Self {
            low: self.low.min(other.low),
            high: self.high.min(other.high),
        }
    }

    /// The larger of two values; it never leaves the operands' range.
    #[must_use]
    pub fn maximum(self, other: Self) -> Self {
        Self {
            low: self.low.max(other.low),
            high: self.high.max(other.high),
        }
    }

    /// The interval a unary Integer operation yields over `self`, or `None`
    /// when the operation can fault or is not Integer valued.
    #[must_use]
    pub fn unary(self, operator: SolveUnaryOperator, domain: SolveIntegerDomain) -> Option<Self> {
        match operator {
            SolveUnaryOperator::Negate => self.negate(domain),
            SolveUnaryOperator::Abs => self.abs(domain),
            SolveUnaryOperator::Sign => Self::span(domain, self.low.signum(), self.high.signum()),
            _ => None,
        }
    }

    /// The interval a binary Integer operation yields over `self` and `other`,
    /// or `None` when the operation can fault or is not tracked.
    #[must_use]
    pub fn binary(
        self,
        operator: SolveBinaryOperator,
        other: Self,
        domain: SolveIntegerDomain,
    ) -> Option<Self> {
        match operator {
            SolveBinaryOperator::Add => self.add(other, domain),
            SolveBinaryOperator::Subtract => self.subtract(other, domain),
            SolveBinaryOperator::Multiply => self.multiply(other, domain),
            SolveBinaryOperator::Min => Some(self.minimum(other)),
            SolveBinaryOperator::Max => Some(self.maximum(other)),
            _ => None,
        }
    }
}

/// The value a unary operation over the known Integer or Boolean `value`
/// computes into a register of type `result`, or `None` when the checked
/// operation would fault, leaves the domain, or is not a scalar fold.
#[must_use]
pub fn fold_scalar_unary(
    result: SolveScalarType,
    operator: SolveUnaryOperator,
    value: i64,
) -> Option<i64> {
    use SolveUnaryOperator as U;
    let folded = match (result, operator) {
        (SolveScalarType::Boolean, U::Not) => i64::from(value == 0),
        (SolveScalarType::Integer(_), U::Negate) => value.checked_neg()?,
        (SolveScalarType::Integer(_), U::Abs) => value.checked_abs()?,
        (SolveScalarType::Integer(_), U::Sign) => value.signum(),
        _ => return None,
    };
    in_domain(result, folded)
}

/// The value a binary operation over the known Integer or Boolean operands
/// computes into a register of type `result`, or `None` when the checked
/// operation would fault, leaves the domain, or is not a scalar fold.
#[must_use]
pub fn fold_scalar_binary(
    result: SolveScalarType,
    operator: SolveBinaryOperator,
    (lhs, rhs): (i64, i64),
) -> Option<i64> {
    use SolveBinaryOperator as B;
    let folded = match (result, operator) {
        (SolveScalarType::Boolean, B::And) => lhs & rhs,
        (SolveScalarType::Boolean, B::Or) => lhs | rhs,
        (SolveScalarType::Integer(_), B::Add) => lhs.checked_add(rhs)?,
        (SolveScalarType::Integer(_), B::Subtract) => lhs.checked_sub(rhs)?,
        (SolveScalarType::Integer(_), B::Multiply) => lhs.checked_mul(rhs)?,
        (SolveScalarType::Integer(_), B::Min) => lhs.min(rhs),
        (SolveScalarType::Integer(_), B::Max) => lhs.max(rhs),
        (SolveScalarType::Integer(_), B::IntegerQuotient) => lhs.checked_div(rhs)?,
        (SolveScalarType::Integer(_), B::IntegerRemainder) => lhs.checked_rem(rhs)?,
        (SolveScalarType::Integer(_), B::IntegerModulo) => {
            let remainder = lhs.checked_rem(rhs)?;
            if remainder != 0 && (remainder ^ rhs) < 0 {
                remainder.checked_add(rhs)?
            } else {
                remainder
            }
        }
        _ => return None,
    };
    in_domain(result, folded)
}

/// The Boolean a comparison of two known Integer or Boolean values yields.
#[must_use]
pub const fn fold_integer_compare(operator: SolveCompareOperator, lhs: i64, rhs: i64) -> bool {
    match operator {
        SolveCompareOperator::Equal => lhs == rhs,
        SolveCompareOperator::NotEqual => lhs != rhs,
        SolveCompareOperator::Less => lhs < rhs,
        SolveCompareOperator::LessEqual => lhs <= rhs,
        SolveCompareOperator::Greater => lhs > rhs,
        SolveCompareOperator::GreaterEqual => lhs >= rhs,
    }
}

/// `value` when it lies in the domain of the Integer `result` (a Boolean
/// result is always 0 or 1).
fn in_domain(result: SolveScalarType, value: i64) -> Option<i64> {
    match result {
        SolveScalarType::Integer(domain) => domain.contains(value).then_some(value),
        _ => Some(value),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn domain(minimum: i64, maximum: i64) -> SolveIntegerDomain {
        SolveIntegerDomain::construct(minimum, maximum).unwrap()
    }

    fn span(domain: SolveIntegerDomain, first: i64, second: i64) -> SolveIntegerInterval {
        SolveIntegerInterval::span(domain, first, second).unwrap()
    }

    #[test]
    fn membership_includes_both_bounds_and_nothing_outside() {
        let interval = span(domain(-10, 10), 2, -3);
        assert!([-3, 0, 2].into_iter().all(|value| interval.contains(value)));
        assert!(![-4, 3].into_iter().any(|value| interval.contains(value)));
    }

    #[test]
    fn absolute_value_of_a_range_spanning_zero_reaches_zero_and_the_larger_magnitude() {
        let domain = domain(-100, 100);
        let abs = span(domain, -5, 3).abs(domain).unwrap();
        assert_eq!((abs.low(), abs.high()), (0, 5));
    }

    #[test]
    fn absolute_value_of_a_one_sided_range_keeps_the_nearer_magnitude_as_its_minimum() {
        let domain = domain(-100, 100);
        let negative = span(domain, -5, -2).abs(domain).unwrap();
        assert_eq!((negative.low(), negative.high()), (2, 5));
        let positive = span(domain, 2, 5).abs(domain).unwrap();
        assert_eq!((positive.low(), positive.high()), (2, 5));
    }

    #[test]
    fn absolute_value_has_no_interval_when_a_member_overflows() {
        let full = SolveIntegerDomain::FULL;
        assert_eq!(span(full, i64::MIN, 0).abs(full), None);
        let narrow = domain(-4, 5);
        assert_eq!(span(narrow, -4, 1).abs(narrow).map(|a| a.high()), Some(4));
        let asymmetric = domain(-5, 4);
        assert_eq!(span(asymmetric, -5, 1).abs(asymmetric), None);
    }
}
