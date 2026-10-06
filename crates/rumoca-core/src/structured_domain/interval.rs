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
pub struct IntegerInterval {
    pub lower: Option<i64>,
    pub upper: Option<i64>,
}

impl IntegerInterval {
    pub const UNBOUNDED: Self = Self {
        lower: None,
        upper: None,
    };

    pub fn finite(lower: i64, upper: i64) -> Self {
        Self {
            lower: Some(lower.min(upper)),
            upper: Some(lower.max(upper)),
        }
    }

    pub fn exact(value: i64) -> Self {
        Self::finite(value, value)
    }

    /// Both endpoints, when both are proven.
    pub fn bounds(self) -> Option<(i64, i64)> {
        Some((self.lower?, self.upper?))
    }

    pub fn is_unbounded(self) -> bool {
        self.lower.is_none() && self.upper.is_none()
    }

    /// Whether no Integer satisfies both endpoints (a contradiction).
    pub fn is_empty(self) -> bool {
        matches!((self.lower, self.upper), (Some(lower), Some(upper)) if lower > upper)
    }

    /// Values in both intervals (a conjunction of facts).
    pub fn meet(self, other: Self) -> Self {
        Self {
            lower: max_endpoint(self.lower, other.lower),
            upper: min_endpoint(self.upper, other.upper),
        }
    }

    /// The smallest interval holding both (a join of alternative paths).
    pub fn hull(self, other: Self) -> Self {
        Self {
            lower: self.lower.zip(other.lower).map(|(a, b)| a.min(b)),
            upper: self.upper.zip(other.upper).map(|(a, b)| a.max(b)),
        }
    }

    pub fn negate(self) -> Self {
        Self {
            lower: self.upper.and_then(i64::checked_neg),
            upper: self.lower.and_then(i64::checked_neg),
        }
    }

    pub fn plus(self, other: Self) -> Self {
        Self {
            lower: checked(self.lower, other.lower, i64::checked_add),
            upper: checked(self.upper, other.upper, i64::checked_add),
        }
    }

    pub fn minus(self, other: Self) -> Self {
        self.plus(other.negate())
    }

    /// Products of finite intervals take the four corner products; a
    /// half-bounded factor is kept only when scaled by an exact constant.
    pub fn times(self, other: Self) -> Self {
        if let (Some(lhs), Some(rhs)) = (self.bounds(), other.bounds()) {
            return corner_products(lhs, rhs)
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

/// The least and greatest of the four corner products of two finite
/// intervals.
fn corner_products(lhs: (i64, i64), rhs: (i64, i64)) -> Option<(i64, i64)> {
    let products = [
        lhs.0.checked_mul(rhs.0)?,
        lhs.0.checked_mul(rhs.1)?,
        lhs.1.checked_mul(rhs.0)?,
        lhs.1.checked_mul(rhs.1)?,
    ];
    Some((*products.iter().min()?, *products.iter().max()?))
}

/// A closed set of Real values `lower <= v <= upper`; `None` leaves that side
/// unconstrained.
///
/// Facts about a Real value come only from literal assignments and from
/// comparisons with literals (MLS §3.7.1), whose values are never NaN; a
/// strict comparison is kept as its closed hull, a superset, so an interval
/// that is empty proves a contradiction while a nonempty one proves nothing.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct RealInterval {
    pub lower: Option<f64>,
    pub upper: Option<f64>,
}

impl RealInterval {
    pub const UNBOUNDED: Self = Self {
        lower: None,
        upper: None,
    };

    /// The single value of a literal; `None` for a NaN, which no literal is.
    pub fn exact(value: f64) -> Option<Self> {
        (!value.is_nan()).then_some(Self {
            lower: Some(value),
            upper: Some(value),
        })
    }

    pub fn is_unbounded(self) -> bool {
        self.lower.is_none() && self.upper.is_none()
    }

    /// Whether no Real satisfies both endpoints (a contradiction).
    pub fn is_empty(self) -> bool {
        matches!((self.lower, self.upper), (Some(lower), Some(upper)) if lower > upper)
    }

    /// Values in both intervals (a conjunction of facts).
    pub fn meet(self, other: Self) -> Self {
        let pick = |lhs: Option<f64>, rhs: Option<f64>, larger: bool| match (lhs, rhs) {
            (Some(a), Some(b)) => Some(if larger { a.max(b) } else { a.min(b) }),
            (value, None) | (None, value) => value,
        };
        Self {
            lower: pick(self.lower, other.lower, true),
            upper: pick(self.upper, other.upper, false),
        }
    }

    /// The smallest interval holding both (a join of alternative paths).
    pub fn hull(self, other: Self) -> Self {
        Self {
            lower: self.lower.zip(other.lower).map(|(a, b)| a.min(b)),
            upper: self.upper.zip(other.upper).map(|(a, b)| a.max(b)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::IntegerInterval;

    fn half(lower: Option<i64>, upper: Option<i64>) -> IntegerInterval {
        IntegerInterval { lower, upper }
    }

    #[test]
    fn a_half_bounded_fact_bounds_a_symmetric_range() {
        // `radius <= 4` and `radius >= 0`: `-radius:radius` lies in -4..4.
        let radius = half(None, Some(4)).meet(half(Some(0), None));
        assert_eq!(radius, IntegerInterval::finite(0, 4));
        assert_eq!(radius.negate(), IntegerInterval::finite(-4, 0));
        assert_eq!(radius.negate().hull(radius), IntegerInterval::finite(-4, 4));
    }

    #[test]
    fn arithmetic_keeps_each_proven_endpoint() {
        let upper_only = half(None, Some(4));
        assert_eq!(
            upper_only.plus(IntegerInterval::exact(1)),
            half(None, Some(5))
        );
        assert_eq!(upper_only.negate(), half(Some(-4), None));
        assert_eq!(
            upper_only.times(IntegerInterval::exact(-2)),
            half(Some(-8), None)
        );
        assert!(upper_only.times(upper_only).is_unbounded());
        assert_eq!(
            IntegerInterval::finite(-2, 3).times(IntegerInterval::finite(-1, 4)),
            IntegerInterval::finite(-8, 12)
        );
        assert_eq!(
            upper_only.hull(IntegerInterval::exact(9)),
            half(None, Some(9))
        );
    }

    #[test]
    fn an_overflowing_endpoint_becomes_unconstrained() {
        let near_max = IntegerInterval::finite(0, i64::MAX);
        assert_eq!(
            near_max.plus(IntegerInterval::exact(1)),
            half(Some(1), None)
        );
        assert_eq!(
            IntegerInterval::exact(i64::MIN).negate(),
            IntegerInterval::UNBOUNDED
        );
    }
}

#[cfg(test)]
mod real_tests {
    use super::{IntegerInterval, RealInterval};

    #[test]
    fn literal_facts_meet_to_a_contradiction_and_hull_to_a_superset() {
        let zero = RealInterval::exact(0.0).unwrap();
        let one = RealInterval::exact(1.0).unwrap();
        assert!(zero.meet(one).is_empty());
        assert!(!zero.meet(RealInterval::UNBOUNDED).is_empty());
        assert_eq!(
            zero.hull(one),
            RealInterval {
                lower: Some(0.0),
                upper: Some(1.0)
            }
        );
        assert!(zero.hull(RealInterval::UNBOUNDED).is_unbounded());
        assert_eq!(RealInterval::exact(f64::NAN), None);
        assert!(
            IntegerInterval::exact(0)
                .meet(IntegerInterval::exact(1))
                .is_empty()
        );
    }
}
