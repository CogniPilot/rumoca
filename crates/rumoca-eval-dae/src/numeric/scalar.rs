//! Typed payload of the single source execution owner.

use rumoca_core::{IntegerBinaryOperator as Op, eval_integer_binary};

#[derive(Clone, Copy, Debug)]
/// Typed payload read from the canonical evaluator's checked source runs.
/// A scalar alone grants neither evaluated-value nor IR construction authority.
pub enum Scalar {
    Real(f64),
    Integer(i64),
    Enumeration(i64),
    Boolean(bool),
    Invalid,
}

impl std::fmt::Display for Scalar {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(value) | Self::Enumeration(value) => value.fmt(formatter),
            Self::Boolean(value) => value.fmt(formatter),
            _ => self.real().fmt(formatter),
        }
    }
}

impl Scalar {
    /// Project this payload through the canonical numerical Real conversion.
    ///
    /// This grants no evaluated-value or IR construction authority. Consumers
    /// must establish that their source operation admits this conversion.
    pub fn real(self) -> f64 {
        match self {
            Self::Real(value) => value,
            Self::Integer(value) | Self::Enumeration(value) => value as f64,
            Self::Boolean(value) => f64::from(u8::from(value)),
            Self::Invalid => f64::NAN,
        }
    }
    pub(super) fn integer(self) -> Option<i64> {
        match self {
            Self::Integer(value) | Self::Enumeration(value) => Some(value),
            _ => None,
        }
    }
    pub(super) fn is_finite(self) -> bool {
        !matches!(self, Self::Invalid) && self.real().is_finite()
    }
    fn checked(self, rhs: Self, op: Op, real: impl FnOnce(f64, f64) -> f64) -> Self {
        match (self.integer(), rhs.integer()) {
            (Some(lhs), Some(rhs)) => eval_integer_binary(op, lhs, rhs)
                .map(Self::Integer)
                .unwrap_or(Self::Invalid),
            _ => Self::Real(real(self.real(), rhs.real())),
        }
    }
    pub(super) fn abs(self) -> Self {
        self.integer()
            .map(|value| {
                value
                    .checked_abs()
                    .map(Self::Integer)
                    .unwrap_or(Self::Invalid)
            })
            .unwrap_or_else(|| Self::Real(self.real().abs()))
    }
    pub(super) fn sign(self) -> Self {
        self.integer()
            .map(|value| Self::Integer(value.signum()))
            .unwrap_or_else(|| {
                let value = self.real();
                if value.is_finite() {
                    Self::Integer(rumoca_core::modelica_sign(value) as i64)
                } else {
                    Self::Invalid
                }
            })
    }
    pub(super) fn integer_builtin(self) -> Self {
        if let Some(value) = self.integer() {
            return Self::Integer(value);
        }
        let value = rumoca_core::modelica_integer_value(self.real());
        if value.is_finite()
            && (-9_223_372_036_854_775_808.0..9_223_372_036_854_775_808.0).contains(&value)
        {
            Self::Integer(value as i64)
        } else {
            Self::Invalid
        }
    }
    pub(super) fn powf(self, rhs: Self) -> Self {
        Self::Real(match rhs.integer() {
            Some(exponent) => rumoca_core::modelica_integer_power(self.real(), exponent),
            None => self.real().powf(rhs.real()),
        })
    }
    pub(super) fn atan2(self, rhs: Self) -> Self {
        Self::Real(self.real().atan2(rhs.real()))
    }
    pub(super) fn min(self, rhs: Self) -> Self {
        if self <= rhs { self } else { rhs }
    }
    pub(super) fn max(self, rhs: Self) -> Self {
        if self >= rhs { self } else { rhs }
    }
}

macro_rules! real_unary {
    ($($method:ident),*) => { impl Scalar { $(pub(super) fn $method(self) -> Self { Self::Real(self.real().$method()) })* } };
}
real_unary!(
    sqrt, floor, ceil, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, exp, ln, log10
);

impl PartialEq for Scalar {
    fn eq(&self, rhs: &Self) -> bool {
        match (self.integer(), rhs.integer()) {
            (Some(lhs), Some(rhs)) => lhs == rhs,
            _ => self.real() == rhs.real(),
        }
    }
}
impl PartialOrd for Scalar {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        match (self.integer(), rhs.integer()) {
            (Some(lhs), Some(rhs)) => lhs.partial_cmp(&rhs),
            _ => self.real().partial_cmp(&rhs.real()),
        }
    }
}
impl PartialEq<f64> for Scalar {
    fn eq(&self, rhs: &f64) -> bool {
        self.real() == *rhs
    }
}
impl PartialOrd<f64> for Scalar {
    fn partial_cmp(&self, rhs: &f64) -> Option<std::cmp::Ordering> {
        self.real().partial_cmp(rhs)
    }
}
impl std::ops::Add for Scalar {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        self.checked(rhs, Op::Add, |a, b| a + b)
    }
}
impl std::ops::Sub for Scalar {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        self.checked(rhs, Op::Sub, |a, b| a - b)
    }
}
impl std::ops::Mul for Scalar {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        self.checked(rhs, Op::Mul, |a, b| a * b)
    }
}
impl std::ops::Div for Scalar {
    type Output = Self;
    fn div(self, rhs: Self) -> Self {
        Self::Real(self.real() / rhs.real())
    }
}
impl std::ops::Neg for Scalar {
    type Output = Self;
    fn neg(self) -> Self {
        self.integer()
            .map(|value| {
                value
                    .checked_neg()
                    .map(Self::Integer)
                    .unwrap_or(Self::Invalid)
            })
            .unwrap_or_else(|| Self::Real(-self.real()))
    }
}
impl std::ops::AddAssign for Scalar {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}
impl std::ops::MulAssign for Scalar {
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs;
    }
}
impl<'a> std::iter::Sum<&'a Scalar> for Scalar {
    fn sum<I: Iterator<Item = &'a Scalar>>(values: I) -> Self {
        values
            .copied()
            .reduce(|a, b| a + b)
            .unwrap_or(Self::Integer(0))
    }
}
impl<'a> std::iter::Product<&'a Scalar> for Scalar {
    fn product<I: Iterator<Item = &'a Scalar>>(values: I) -> Self {
        values
            .copied()
            .reduce(|a, b| a * b)
            .unwrap_or(Self::Integer(1))
    }
}

#[cfg(test)]
mod tests {
    use super::Scalar;

    #[test]
    fn display_prints_the_typed_payload_without_integer_widening() {
        assert_eq!(Scalar::Integer(3).to_string(), "3");
        assert_eq!(Scalar::Enumeration(2).to_string(), "2");
        assert_eq!(Scalar::Boolean(true).to_string(), "true");
        assert_eq!(Scalar::Real(1.5).to_string(), "1.5");
        assert_eq!(Scalar::Invalid.to_string(), "NaN");
    }

    #[test]
    fn real_unary_builtins_apply_the_matching_function_to_the_real_projection() {
        let x = Scalar::Real(0.5);
        let cases: [(Scalar, f64); 7] = [
            (x.asin(), 0.5_f64.asin()),
            (x.acos(), 0.5_f64.acos()),
            (x.cosh(), 0.5_f64.cosh()),
            (x.sinh(), 0.5_f64.sinh()),
            (x.tanh(), 0.5_f64.tanh()),
            (Scalar::Real(100.0).log10(), 2.0),
            (Scalar::Real(1.2).ceil(), 2.0),
        ];
        for (got, want) in cases {
            assert!(
                matches!(got, Scalar::Real(v) if v == want),
                "{got:?} != {want}"
            );
        }
    }

    #[test]
    fn atan2_uses_the_quadrant_of_both_operands() {
        let got = Scalar::Real(1.0).atan2(Scalar::Real(-1.0));
        assert!(matches!(got, Scalar::Real(v) if v == 1.0_f64.atan2(-1.0)));
    }

    #[test]
    fn comparison_against_a_real_uses_the_real_projection() {
        assert_eq!(
            Scalar::Integer(2).partial_cmp(&3.0),
            Some(std::cmp::Ordering::Less)
        );
        assert!(Scalar::Real(f64::NAN).partial_cmp(&1.0).is_none());
    }

    #[test]
    fn mul_assign_keeps_integer_arithmetic_exact_and_invalidates_overflow() {
        let mut value = Scalar::Integer(i64::MAX / 2);
        value *= Scalar::Integer(2);
        assert!(matches!(value, Scalar::Integer(v) if v == i64::MAX - 1));
        value *= Scalar::Integer(2);
        assert!(matches!(value, Scalar::Invalid));
    }
}
