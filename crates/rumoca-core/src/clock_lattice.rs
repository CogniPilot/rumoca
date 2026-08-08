//! Exact rational base-clock lattices (MLS §16).
//!
//! MLS §16.5 states that every clocked partition has exactly one base clock and
//! that every other clock of that partition is derived from it with the integer
//! sub-clock conversion operators of §16.5.2 — `subSample(u, factor)`,
//! `superSample(u, factor)`, `shiftSample(u, shiftCounter, resolution)` and
//! `backSample(u, backCounter, resolution)`. All four are exact *integer*
//! relations, so the period and the phase of every clock in a partition are
//! exact rational multiples of the base clock's: the partition is a rational
//! lattice. MLS §16.3's `Clock(intervalCounter, resolution)` constructor names
//! a rational period directly, and §16.3 defines the tick instants of a
//! periodic clock as `phase + k * period` for `k = 0, 1, 2, ...`.
//!
//! This module carries that lattice exactly. Clock identity, sub-clock
//! composition and tick instants are computed in reduced integer arithmetic;
//! `f64` appears only when a tick instant is handed to the numeric scheduler.
//!
//! Every operation is overflow-checked. SPEC_0008 forbids silent recovery, so a
//! value that does not fit the representation reports a spanned error instead of
//! wrapping, saturating or substituting an approximation.

use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::fmt;

use crate::Span;

/// Why an exact clock-lattice operation could not be carried out.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum ClockLatticeErrorKind {
    /// A seconds value was NaN or infinite.
    NonFiniteSeconds,
    /// A rational was constructed with a zero denominator.
    ZeroDenominator,
    /// MLS §16.3 requires a strictly positive clock interval.
    NonPositivePeriod,
    /// MLS §16.5.2 requires strictly positive `factor`/`resolution` arguments.
    NonPositiveFactor,
    /// Exact integer arithmetic left the representable range.
    IntegerOverflow,
    /// A seconds value has no reduced rational form in range.
    NotRationallyRepresentable,
    /// MLS §16.5.2 Operator 16.12: `backSample` moved the first activation of
    /// the result before the first activation of its base clock.
    ClockStartsBeforeBaseClock,
}

impl ClockLatticeErrorKind {
    /// Human-readable reason, used verbatim in phase diagnostics.
    pub fn message(self) -> &'static str {
        match self {
            Self::NonFiniteSeconds => "clock lattice value is not a finite number of seconds",
            Self::ZeroDenominator => "clock lattice rational has a zero denominator",
            Self::NonPositivePeriod => {
                "clock interval must be strictly positive (MLS §16.3 interval > 0)"
            }
            Self::NonPositiveFactor => {
                "clock conversion factor must be strictly positive (MLS §16.5.2)"
            }
            Self::IntegerOverflow => "exact clock lattice arithmetic overflowed 128-bit integers",
            Self::NotRationallyRepresentable => {
                "clock interval has no exact reduced rational representation"
            }
            Self::ClockStartsBeforeBaseClock => {
                "backSample would start the clock before its base clock (MLS §16.5.2)"
            }
        }
    }

    /// Attach the source provenance of the clock expression that failed.
    pub fn at(self, span: Span) -> ClockLatticeError {
        ClockLatticeError { kind: self, span }
    }
}

impl fmt::Display for ClockLatticeErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.message())
    }
}

impl std::error::Error for ClockLatticeErrorKind {}

/// A clock-lattice failure carrying the span of the offending clock expression.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct ClockLatticeError {
    pub kind: ClockLatticeErrorKind,
    pub span: Span,
}

impl fmt::Display for ClockLatticeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.kind.message())
    }
}

impl std::error::Error for ClockLatticeError {}

type LatticeResult<T> = Result<T, ClockLatticeErrorKind>;

/// An exact rational number in reduced form with a strictly positive
/// denominator.
///
/// Arithmetic stays in reduced `i128` form. Products cross-cancel before
/// multiplication, and every remaining operation is checked, so values outside
/// the representation are reported rather than wrapped.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct ClockRational {
    num: i128,
    den: i128,
}

impl ClockRational {
    /// Exact zero.
    pub const ZERO: Self = Self { num: 0, den: 1 };
    /// Exact one.
    pub const ONE: Self = Self { num: 1, den: 1 };

    /// Reduce `num / den` to canonical form.
    pub fn new(num: impl Into<i128>, den: impl Into<i128>) -> LatticeResult<Self> {
        Self::reduce(num.into(), den.into())
    }

    /// The exact integer `value`.
    pub const fn integer(value: i128) -> Self {
        Self { num: value, den: 1 }
    }

    /// Reduced numerator; the sign of the rational lives here.
    pub const fn numerator(self) -> i128 {
        self.num
    }

    /// Reduced denominator; always strictly positive.
    pub const fn denominator(self) -> i128 {
        self.den
    }

    /// Whether this rational is exactly zero.
    pub const fn is_zero(self) -> bool {
        self.num == 0
    }

    /// Whether this rational is strictly greater than zero.
    pub const fn is_positive(self) -> bool {
        self.num > 0
    }

    fn reduce(num: i128, den: i128) -> LatticeResult<Self> {
        if den == 0 {
            return Err(ClockLatticeErrorKind::ZeroDenominator);
        }
        if num == 0 {
            return Ok(Self::ZERO);
        }
        let negative = (num < 0) != (den < 0);
        Self::from_signed_magnitude(num.unsigned_abs(), negative, den.unsigned_abs())
    }

    fn from_signed_magnitude(
        numerator: u128,
        negative: bool,
        denominator: u128,
    ) -> LatticeResult<Self> {
        if numerator == 0 {
            return Ok(Self::ZERO);
        }
        let divisor = gcd_u128(numerator, denominator);
        Ok(Self {
            num: signed_from_magnitude(numerator / divisor, negative)?,
            den: positive_from_magnitude(denominator / divisor)?,
        })
    }

    fn checked_same_denominator(self, other: Self, subtract: bool) -> LatticeResult<Self> {
        let lhs_negative = self.num.is_negative();
        let rhs_negative = other.num.is_negative() != subtract;
        let lhs = self.num.unsigned_abs();
        let rhs = other.num.unsigned_abs();
        let (magnitude, negative) = if lhs_negative == rhs_negative {
            (
                lhs.checked_add(rhs)
                    .ok_or(ClockLatticeErrorKind::IntegerOverflow)?,
                lhs_negative,
            )
        } else if lhs >= rhs {
            (lhs - rhs, lhs_negative)
        } else {
            (rhs - lhs, rhs_negative)
        };
        Self::from_signed_magnitude(magnitude, negative, self.den as u128)
    }

    /// Exact sum.
    pub fn checked_add(self, other: Self) -> LatticeResult<Self> {
        if self.den == other.den {
            return self.checked_same_denominator(other, false);
        }
        let denominator_gcd = gcd_u128(self.den as u128, other.den as u128);
        let denominator_gcd = positive_from_magnitude(denominator_gcd)?;
        let lhs_scale = other.den / denominator_gcd;
        let rhs_scale = self.den / denominator_gcd;
        let lhs = self
            .num
            .checked_mul(lhs_scale)
            .ok_or(ClockLatticeErrorKind::IntegerOverflow)?;
        let rhs = other
            .num
            .checked_mul(rhs_scale)
            .ok_or(ClockLatticeErrorKind::IntegerOverflow)?;
        let num = lhs
            .checked_add(rhs)
            .ok_or(ClockLatticeErrorKind::IntegerOverflow)?;
        let den = self
            .den
            .checked_mul(lhs_scale)
            .ok_or(ClockLatticeErrorKind::IntegerOverflow)?;
        Self::reduce(num, den)
    }

    /// Exact difference.
    pub fn checked_sub(self, other: Self) -> LatticeResult<Self> {
        if self.den == other.den {
            return self.checked_same_denominator(other, true);
        }
        let denominator_gcd = gcd_u128(self.den as u128, other.den as u128);
        let denominator_gcd = positive_from_magnitude(denominator_gcd)?;
        let lhs_scale = other.den / denominator_gcd;
        let rhs_scale = self.den / denominator_gcd;
        let lhs = self
            .num
            .checked_mul(lhs_scale)
            .ok_or(ClockLatticeErrorKind::IntegerOverflow)?;
        let rhs = other
            .num
            .checked_mul(rhs_scale)
            .ok_or(ClockLatticeErrorKind::IntegerOverflow)?;
        let num = lhs
            .checked_sub(rhs)
            .ok_or(ClockLatticeErrorKind::IntegerOverflow)?;
        let den = self
            .den
            .checked_mul(lhs_scale)
            .ok_or(ClockLatticeErrorKind::IntegerOverflow)?;
        Self::reduce(num, den)
    }

    /// Exact negation.
    ///
    /// `-i128::MIN` is not representable, so negation is checked like every
    /// other operation in this module: it reports `IntegerOverflow` rather than
    /// panicking in debug builds or wrapping in release builds.
    pub fn checked_negate(self) -> LatticeResult<Self> {
        Ok(Self {
            num: self
                .num
                .checked_neg()
                .ok_or(ClockLatticeErrorKind::IntegerOverflow)?,
            den: self.den,
        })
    }

    /// Exact product.
    pub fn checked_mul(self, other: Self) -> LatticeResult<Self> {
        if self.is_zero() || other.is_zero() {
            return Ok(Self::ZERO);
        }
        let left_divisor = gcd_u128(self.num.unsigned_abs(), other.den as u128);
        let right_divisor = gcd_u128(other.num.unsigned_abs(), self.den as u128);
        let lhs_num = divide_signed_by_unsigned(self.num, left_divisor)?;
        let rhs_num = divide_signed_by_unsigned(other.num, right_divisor)?;
        let lhs_den = divide_positive_by_unsigned(self.den, right_divisor)?;
        let rhs_den = divide_positive_by_unsigned(other.den, left_divisor)?;
        let num = lhs_num
            .checked_mul(rhs_num)
            .ok_or(ClockLatticeErrorKind::IntegerOverflow)?;
        let den = lhs_den
            .checked_mul(rhs_den)
            .ok_or(ClockLatticeErrorKind::IntegerOverflow)?;
        Self::reduce(num, den)
    }

    /// Exact quotient; `other` must not be zero.
    pub fn checked_div(self, other: Self) -> LatticeResult<Self> {
        if other.is_zero() {
            return Err(ClockLatticeErrorKind::ZeroDenominator);
        }
        if self.is_zero() {
            return Ok(Self::ZERO);
        }
        let numerator_divisor = gcd_u128(self.num.unsigned_abs(), other.num.unsigned_abs());
        let denominator_divisor = gcd_u128(self.den as u128, other.den as u128);
        let lhs_num = divide_signed_by_unsigned(self.num, numerator_divisor)?;
        let rhs_num = divide_signed_by_unsigned(other.num, numerator_divisor)?;
        let lhs_den = divide_positive_by_unsigned(self.den, denominator_divisor)?;
        let rhs_den = divide_positive_by_unsigned(other.den, denominator_divisor)?;
        let num = lhs_num
            .checked_mul(rhs_den)
            .ok_or(ClockLatticeErrorKind::IntegerOverflow)?;
        let den = lhs_den
            .checked_mul(rhs_num)
            .ok_or(ClockLatticeErrorKind::IntegerOverflow)?;
        Self::reduce(num, den)
    }

    /// Exact product with an integer.
    pub fn checked_mul_integer(self, factor: impl Into<i128>) -> LatticeResult<Self> {
        self.checked_mul(Self::integer(factor.into()))
    }

    /// Exact quotient by a non-zero integer.
    pub fn checked_div_integer(self, divisor: impl Into<i128>) -> LatticeResult<Self> {
        let divisor = divisor.into();
        if divisor == 0 {
            return Err(ClockLatticeErrorKind::ZeroDenominator);
        }
        self.checked_div(Self::integer(divisor))
    }

    /// Convert to seconds. This is the only lossy step and belongs at the
    /// numeric-scheduler boundary.
    pub fn to_f64(self) -> f64 {
        self.num as f64 / self.den as f64
    }

    /// Largest integer `k` with `k <= self`.
    pub fn floor_integer(self) -> LatticeResult<i128> {
        Ok(self.num.div_euclid(self.den))
    }

    /// The reduced rational that is closest to `seconds` and reproduces it
    /// exactly when converted back to `f64`.
    ///
    /// The search walks the continued-fraction convergents of the exact binary
    /// value of `seconds`, so `0.1` yields `1/10` rather than the 55-bit dyadic
    /// expansion. Composing sub-clocks over the short form is what keeps the
    /// lattice inside the integer range.
    pub fn from_seconds(seconds: f64) -> LatticeResult<Self> {
        if !seconds.is_finite() {
            return Err(ClockLatticeErrorKind::NonFiniteSeconds);
        }
        if seconds == 0.0 {
            return Ok(Self::ZERO);
        }
        let (num, den) = exact_binary_fraction(seconds)?;
        shortest_round_tripping_convergent(num, den, seconds)
    }
}

impl PartialOrd for ClockRational {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ClockRational {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self.num.is_negative(), other.num.is_negative()) {
            (true, false) => Ordering::Less,
            (false, true) => Ordering::Greater,
            (false, false) => compare_positive_fractions(
                self.num as u128,
                self.den as u128,
                other.num as u128,
                other.den as u128,
            ),
            (true, true) => compare_positive_fractions(
                other.num.unsigned_abs(),
                other.den as u128,
                self.num.unsigned_abs(),
                self.den as u128,
            ),
        }
    }
}

impl fmt::Display for ClockRational {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.num, self.den)
    }
}

fn positive_from_magnitude(value: u128) -> LatticeResult<i128> {
    i128::try_from(value).map_err(|_| ClockLatticeErrorKind::IntegerOverflow)
}

fn signed_from_magnitude(value: u128, negative: bool) -> LatticeResult<i128> {
    if !negative {
        return positive_from_magnitude(value);
    }
    if value == 1u128 << 127 {
        return Ok(i128::MIN);
    }
    positive_from_magnitude(value).map(|value| -value)
}

fn gcd_u128(mut a: u128, mut b: u128) -> u128 {
    while b != 0 {
        let next = a % b;
        a = b;
        b = next;
    }
    if a == 0 { 1 } else { a }
}

fn divide_signed_by_unsigned(value: i128, divisor: u128) -> LatticeResult<i128> {
    debug_assert_ne!(divisor, 0);
    let magnitude = value.unsigned_abs() / divisor;
    signed_from_magnitude(magnitude, value.is_negative())
}

fn divide_positive_by_unsigned(value: i128, divisor: u128) -> LatticeResult<i128> {
    debug_assert!(value > 0);
    debug_assert_ne!(divisor, 0);
    positive_from_magnitude(value as u128 / divisor)
}

/// Compare two non-negative fractions without overflowing a cross-product.
///
/// Equal integer parts are stripped and the reciprocal remainders compared;
/// reciprocation reverses the ordering at each iteration.
fn compare_positive_fractions(
    mut lhs_num: u128,
    mut lhs_den: u128,
    mut rhs_num: u128,
    mut rhs_den: u128,
) -> Ordering {
    let mut reversed = false;
    loop {
        let lhs_integer = lhs_num / lhs_den;
        let rhs_integer = rhs_num / rhs_den;
        if lhs_integer != rhs_integer {
            let ordering = lhs_integer.cmp(&rhs_integer);
            return if reversed {
                ordering.reverse()
            } else {
                ordering
            };
        }
        let lhs_remainder = lhs_num % lhs_den;
        let rhs_remainder = rhs_num % rhs_den;
        match (lhs_remainder == 0, rhs_remainder == 0) {
            (true, true) => return Ordering::Equal,
            (true, false) => {
                return if reversed {
                    Ordering::Greater
                } else {
                    Ordering::Less
                };
            }
            (false, true) => {
                return if reversed {
                    Ordering::Less
                } else {
                    Ordering::Greater
                };
            }
            (false, false) => {
                (lhs_num, lhs_den) = (lhs_den, lhs_remainder);
                (rhs_num, rhs_den) = (rhs_den, rhs_remainder);
                reversed = !reversed;
            }
        }
    }
}

/// Decompose a finite non-zero `f64` into the exact fraction `num / den`.
fn exact_binary_fraction(value: f64) -> LatticeResult<(i128, i128)> {
    let bits = value.to_bits();
    let raw_exponent = ((bits >> 52) & 0x7ff) as i32;
    let raw_mantissa = bits & 0x000f_ffff_ffff_ffff;
    let (mantissa, exponent) = if raw_exponent == 0 {
        (raw_mantissa, -1074i32)
    } else {
        (raw_mantissa | 0x0010_0000_0000_0000, raw_exponent - 1075)
    };
    let removable_twos = mantissa.trailing_zeros().min(exponent.unsigned_abs());
    let magnitude = u128::from(mantissa >> removable_twos);
    let exponent = exponent
        .checked_add(removable_twos as i32)
        .ok_or(ClockLatticeErrorKind::IntegerOverflow)?;
    let signed = signed_from_magnitude(magnitude, value < 0.0)?;
    if exponent >= 0 {
        let shift = u32::try_from(exponent).map_err(|_| ClockLatticeErrorKind::IntegerOverflow)?;
        let num = signed
            .checked_shl(shift)
            .ok_or(ClockLatticeErrorKind::NotRationallyRepresentable)?;
        if num >> shift != signed {
            return Err(ClockLatticeErrorKind::NotRationallyRepresentable);
        }
        return Ok((num, 1));
    }
    let shift = u32::try_from(-exponent).map_err(|_| ClockLatticeErrorKind::IntegerOverflow)?;
    if shift >= 127 {
        return Err(ClockLatticeErrorKind::NotRationallyRepresentable);
    }
    Ok((signed, 1i128 << shift))
}

/// Walk the continued-fraction convergents of `num / den` and return the first
/// one that fits `i128` and converts back to exactly `seconds`.
fn shortest_round_tripping_convergent(
    num: i128,
    den: i128,
    seconds: f64,
) -> LatticeResult<ClockRational> {
    let negative = num < 0;
    let (mut remainder_num, mut remainder_den) = (num.unsigned_abs(), den as u128);
    let (mut prev_num, mut current_num) = (0u128, 1u128);
    let (mut prev_den, mut current_den) = (1u128, 0u128);
    loop {
        let term = remainder_num / remainder_den;
        let next_num = convergent_step(term, current_num, prev_num)?;
        let next_den = convergent_step(term, current_den, prev_den)?;
        if let Some(candidate) = round_tripping_candidate(negative, next_num, next_den, seconds) {
            return Ok(candidate);
        }
        prev_num = current_num;
        current_num = next_num;
        prev_den = current_den;
        current_den = next_den;
        let rest = remainder_num - term * remainder_den;
        if rest == 0 {
            return Err(ClockLatticeErrorKind::NotRationallyRepresentable);
        }
        remainder_num = remainder_den;
        remainder_den = rest;
    }
}

fn convergent_step(term: u128, current: u128, previous: u128) -> LatticeResult<u128> {
    term.checked_mul(current)
        .and_then(|scaled| scaled.checked_add(previous))
        .ok_or(ClockLatticeErrorKind::NotRationallyRepresentable)
}

fn round_tripping_candidate(
    negative: bool,
    num: u128,
    den: u128,
    seconds: f64,
) -> Option<ClockRational> {
    let candidate = ClockRational {
        num: signed_from_magnitude(num, negative).ok()?,
        den: positive_from_magnitude(den).ok()?,
    };
    (candidate.to_f64() == seconds).then_some(candidate)
}

/// An exact periodic clock: tick `k` happens at `phase + k * period`
/// (MLS §16.3), with `period > 0`.
///
/// Two lattices are equal exactly when their reduced period and phase agree, so
/// clock identity and tick simultaneity never depend on a floating-point
/// epsilon.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct ClockLattice {
    period: ClockRational,
    phase: ClockRational,
}

/// The runtime anchor of a periodic schedule's phase.
///
/// Ordinary clocks carry an absolute phase. Modelica's event form
/// `sample(t0 + offset, interval)`, where initialization proves `t0 = time`,
/// instead anchors the phase at the simulation start instant. Keeping that
/// distinction typed prevents translation from silently assuming a particular
/// `startTime`.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ClockPhaseAnchor {
    #[default]
    Absolute,
    SimulationStart,
}

/// An exact periodic lattice together with the runtime anchor of its phase.
///
/// For [`ClockPhaseAnchor::SimulationStart`], `lattice.phase()` is the exact
/// offset from the simulation start rather than an absolute time. The schedule
/// remains unresolved in compiler IR and is resolved exactly once at the
/// simulation boundary.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct PeriodicClockSchedule {
    lattice: ClockLattice,
    anchor: ClockPhaseAnchor,
}

impl PeriodicClockSchedule {
    /// Construct an absolute periodic schedule.
    pub fn absolute(lattice: ClockLattice) -> LatticeResult<Self> {
        Ok(Self {
            lattice: ClockLattice::new(lattice.period(), lattice.phase())?,
            anchor: ClockPhaseAnchor::Absolute,
        })
    }

    /// Construct a schedule whose phase is relative to simulation start.
    pub fn simulation_start_relative(lattice: ClockLattice) -> LatticeResult<Self> {
        Ok(Self {
            lattice: ClockLattice::new(lattice.period(), lattice.phase())?,
            anchor: ClockPhaseAnchor::SimulationStart,
        })
    }

    pub const fn lattice(self) -> ClockLattice {
        self.lattice
    }

    pub const fn anchor(self) -> ClockPhaseAnchor {
        self.anchor
    }

    /// Exact interval retained by this schedule.
    pub const fn period(self) -> ClockRational {
        self.lattice.period()
    }

    /// Exact absolute phase, or start-relative offset according to [`Self::anchor`].
    pub const fn phase(self) -> ClockRational {
        self.lattice.phase()
    }

    pub fn period_seconds(self) -> f64 {
        self.lattice.period_seconds()
    }

    /// Absolute phase seconds, or the exact offset from simulation start for a
    /// start-relative schedule.
    pub fn phase_seconds(self) -> f64 {
        self.lattice.phase_seconds()
    }

    /// Resolve a start-relative phase at the simulation boundary.
    pub fn resolve_at(self, start_time: f64) -> LatticeResult<Self> {
        if self.anchor == ClockPhaseAnchor::Absolute {
            return Self::absolute(self.lattice);
        }
        let start = ClockRational::from_seconds(start_time)?;
        let phase = start.checked_add(self.lattice.phase())?;
        Self::absolute(ClockLattice::new(self.lattice.period(), phase)?)
    }
}

impl ClockLattice {
    /// Build a lattice from an exact period and phase.
    ///
    /// Only `period > 0` (MLS §16.3) is a representation constraint. A negative
    /// phase is *not* rejected here: `from_seconds` must be able to recover any
    /// schedule the DAE already carries, including a `sample(start, interval)`
    /// whose `start` a model placed before the simulation start time. The
    /// "clock must not start before its base clock" condition belongs to the
    /// one operator that can move a phase backwards, and is enforced there —
    /// see [`ClockLattice::back_sample`].
    pub fn new(period: ClockRational, phase: ClockRational) -> LatticeResult<Self> {
        if !period.is_positive() {
            return Err(ClockLatticeErrorKind::NonPositivePeriod);
        }
        Ok(Self { period, phase })
    }

    /// MLS §16.3 `Clock(intervalCounter, resolution)`: an exact rational period
    /// of `intervalCounter / resolution` seconds with zero phase.
    pub fn from_interval_counter(
        interval_counter: impl Into<i128>,
        resolution: impl Into<i128>,
    ) -> LatticeResult<Self> {
        let interval_counter = interval_counter.into();
        let resolution = resolution.into();
        if interval_counter <= 0 || resolution <= 0 {
            return Err(ClockLatticeErrorKind::NonPositiveFactor);
        }
        Self::new(
            ClockRational::new(interval_counter, resolution)?,
            ClockRational::ZERO,
        )
    }

    /// Recover the exact lattice of a clock whose timing survived only as
    /// seconds. Both values must round-trip to a reduced rational.
    pub fn from_seconds(period_seconds: f64, phase_seconds: f64) -> LatticeResult<Self> {
        Self::new(
            ClockRational::from_seconds(period_seconds)?,
            ClockRational::from_seconds(phase_seconds)?,
        )
    }

    /// Exact tick period.
    pub const fn period(self) -> ClockRational {
        self.period
    }

    /// Exact phase of tick zero.
    pub const fn phase(self) -> ClockRational {
        self.phase
    }

    /// Tick period in seconds.
    pub fn period_seconds(self) -> f64 {
        self.period.to_f64()
    }

    /// Phase in seconds.
    pub fn phase_seconds(self) -> f64 {
        self.phase.to_f64()
    }

    /// MLS §16.5.2 `subSample(u, factor)`: tick every `factor`-th tick of `u`,
    /// with the first activation of both clocks coinciding.
    pub fn sub_sample(self, factor: impl Into<i128>) -> LatticeResult<Self> {
        let factor = factor.into();
        if factor <= 0 {
            return Err(ClockLatticeErrorKind::NonPositiveFactor);
        }
        Self::new(self.period.checked_mul_integer(factor)?, self.phase)
    }

    /// MLS §16.5.2 `superSample(u, factor)`: tick `factor` times per tick of
    /// `u`, with the first activation of both clocks coinciding.
    pub fn super_sample(self, factor: impl Into<i128>) -> LatticeResult<Self> {
        let factor = factor.into();
        if factor <= 0 {
            return Err(ClockLatticeErrorKind::NonPositiveFactor);
        }
        Self::new(self.period.checked_div_integer(factor)?, self.phase)
    }

    /// MLS §16.5.2 `shiftSample(u, shiftCounter, resolution)`: shift the phase
    /// forward by `shiftCounter / resolution` of `interval(u)`.
    pub fn shift_sample(
        self,
        shift_counter: impl Into<i128>,
        resolution: impl Into<i128>,
    ) -> LatticeResult<Self> {
        let offset = self.shift_offset(shift_counter, resolution)?;
        Self::new(self.period, self.phase.checked_add(offset)?)
    }

    /// MLS §16.5.2 `backSample(u, backCounter, resolution)`: shift the phase
    /// backward by `backCounter / resolution` of `interval(u)`.
    ///
    /// Operator 16.12 states "It is an error if the clock of `y` starts before
    /// the base-clock of `u`", so a shift that would move the first activation
    /// before tick zero is reported rather than turned into a negative phase.
    pub fn back_sample(
        self,
        back_counter: impl Into<i128>,
        resolution: impl Into<i128>,
    ) -> LatticeResult<Self> {
        let offset = self.shift_offset(back_counter, resolution)?;
        let phase = self.phase.checked_sub(offset)?;
        if phase.numerator() < 0 {
            return Err(ClockLatticeErrorKind::ClockStartsBeforeBaseClock);
        }
        Self::new(self.period, phase)
    }

    fn shift_offset(
        self,
        counter: impl Into<i128>,
        resolution: impl Into<i128>,
    ) -> LatticeResult<ClockRational> {
        let counter = counter.into();
        let resolution = resolution.into();
        if counter < 0 || resolution <= 0 {
            return Err(ClockLatticeErrorKind::NonPositiveFactor);
        }
        self.period
            .checked_mul(ClockRational::new(counter, resolution)?)
    }

    /// Exact instant of tick `index`: `phase + index * period` (MLS §16.3).
    pub fn tick_time(self, index: impl Into<i128>) -> LatticeResult<ClockRational> {
        self.phase
            .checked_add(self.period.checked_mul_integer(index)?)
    }

    /// Instant of tick `index` in seconds, rounded exactly once.
    pub fn tick_time_seconds(self, index: impl Into<i128>) -> LatticeResult<f64> {
        Ok(self.tick_time(index)?.to_f64())
    }

    /// Index of the last tick at or before `instant`, clamped at tick zero.
    pub fn tick_index_at_or_before(self, instant: ClockRational) -> LatticeResult<i128> {
        let elapsed = instant.checked_sub(self.phase)?;
        if !elapsed.is_positive() {
            return Ok(0);
        }
        elapsed.checked_div(self.period)?.floor_integer()
    }

    /// Whether `instant` is exactly a tick of this clock.
    pub fn ticks_at(self, instant: ClockRational) -> LatticeResult<bool> {
        let elapsed = instant.checked_sub(self.phase)?;
        if elapsed.is_zero() {
            return Ok(true);
        }
        if !elapsed.is_positive() {
            return Ok(false);
        }
        let ratio = elapsed.checked_div(self.period)?;
        Ok(ratio.denominator() == 1)
    }

    /// Exact clock identity (MLS §16.5: same base clock and same conversion
    /// chain give the same lattice point).
    pub fn is_same_clock(self, other: Self) -> bool {
        self == other
    }

    /// Whether both clocks activate at the same instants, i.e. equal periods
    /// and phases differing by a whole number of periods.
    pub fn ticks_simultaneously_with(self, other: Self) -> LatticeResult<bool> {
        if self.period != other.period {
            return Ok(false);
        }
        let offset = self.phase.checked_sub(other.phase)?;
        Ok(offset.checked_div(self.period)?.denominator() == 1)
    }
}

impl fmt::Display for ClockLattice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "period={} phase={}", self.period, self.phase)
    }
}

#[cfg(test)]
mod tests;
