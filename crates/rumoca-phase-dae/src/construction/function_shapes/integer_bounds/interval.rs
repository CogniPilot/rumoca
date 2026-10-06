//! Integer intervals whose endpoints may each be unproven.
//!
//! A guard such as `radius <= 4` proves an upper bound of a value whose lower
//! bound nothing proves. Such a half-bounded fact still bounds the envelope of
//! the range `-radius:radius` (MLS §10.4.1: its elements lie between its
//! start and end), so interval arithmetic keeps each endpoint separately and a
//! finite interval is the special case where both endpoints are proven.

/// A set of Integer values `lower <= v <= upper`; `None` leaves that side
/// unconstrained. Every operation is checked: an overflowing endpoint becomes
/// unconstrained rather than wrapping.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(in crate::construction) struct IntegerInterval {
    pub(in crate::construction) lower: Option<i64>,
    pub(in crate::construction) upper: Option<i64>,
}

impl IntegerInterval {
    pub(in crate::construction) const UNBOUNDED: Self = Self {
        lower: None,
        upper: None,
    };

    pub(in crate::construction) fn finite(lower: i64, upper: i64) -> Self {
        Self {
            lower: Some(lower.min(upper)),
            upper: Some(lower.max(upper)),
        }
    }

    pub(in crate::construction) fn exact(value: i64) -> Self {
        Self::finite(value, value)
    }

    /// Both endpoints, when both are proven.
    pub(in crate::construction) fn bounds(self) -> Option<(i64, i64)> {
        Some((self.lower?, self.upper?))
    }

    pub(in crate::construction) fn is_unbounded(self) -> bool {
        self.lower.is_none() && self.upper.is_none()
    }

    /// Values in both intervals (a conjunction of facts).
    pub(in crate::construction) fn meet(self, other: Self) -> Self {
        Self {
            lower: max_endpoint(self.lower, other.lower),
            upper: min_endpoint(self.upper, other.upper),
        }
    }

    /// The smallest interval holding both (a join of alternative paths).
    pub(in crate::construction) fn hull(self, other: Self) -> Self {
        Self {
            lower: self.lower.zip(other.lower).map(|(a, b)| a.min(b)),
            upper: self.upper.zip(other.upper).map(|(a, b)| a.max(b)),
        }
    }

    pub(in crate::construction) fn negate(self) -> Self {
        Self {
            lower: self.upper.and_then(i64::checked_neg),
            upper: self.lower.and_then(i64::checked_neg),
        }
    }

    pub(in crate::construction) fn add(self, other: Self) -> Self {
        Self {
            lower: checked(self.lower, other.lower, i64::checked_add),
            upper: checked(self.upper, other.upper, i64::checked_add),
        }
    }

    pub(in crate::construction) fn subtract(self, other: Self) -> Self {
        self.add(other.negate())
    }

    /// Products of finite intervals take the four corner products; a
    /// half-bounded factor is kept only when scaled by an exact constant.
    pub(in crate::construction) fn multiply(self, other: Self) -> Self {
        if let (Some(lhs), Some(rhs)) = (self.bounds(), other.bounds()) {
            return super::operations::multiply(lhs, rhs)
                .map_or(Self::UNBOUNDED, |(lower, upper)| Self::finite(lower, upper));
        }
        match (self.constant(), other.constant()) {
            (Some(factor), _) => other.scale(factor),
            (_, Some(factor)) => self.scale(factor),
            _ => Self::UNBOUNDED,
        }
    }

    fn constant(self) -> Option<i64> {
        self.bounds()
            .and_then(|(lower, upper)| (lower == upper).then_some(lower))
    }

    fn scale(self, factor: i64) -> Self {
        let scaled = Self {
            lower: self.lower.and_then(|value| value.checked_mul(factor)),
            upper: self.upper.and_then(|value| value.checked_mul(factor)),
        };
        if factor < 0 {
            Self {
                lower: scaled.upper,
                upper: scaled.lower,
            }
        } else {
            scaled
        }
    }
}

fn checked(
    lhs: Option<i64>,
    rhs: Option<i64>,
    operation: fn(i64, i64) -> Option<i64>,
) -> Option<i64> {
    operation(lhs?, rhs?)
}

fn max_endpoint(lhs: Option<i64>, rhs: Option<i64>) -> Option<i64> {
    match (lhs, rhs) {
        (Some(a), Some(b)) => Some(a.max(b)),
        (value, None) | (None, value) => value,
    }
}

fn min_endpoint(lhs: Option<i64>, rhs: Option<i64>) -> Option<i64> {
    match (lhs, rhs) {
        (Some(a), Some(b)) => Some(a.min(b)),
        (value, None) | (None, value) => value,
    }
}
