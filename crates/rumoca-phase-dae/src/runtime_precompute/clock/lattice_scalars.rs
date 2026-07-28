//! Folded-scalar to exact-lattice conversions for MLS §16 clock inference.
//!
//! Constant folding hands clock inference `f64` seconds and `f64` conversion
//! factors. These helpers are the single boundary where those become the exact
//! rational period/phase and the exact integer factors that MLS §16.5.2
//! sub-clock composition requires.

use rumoca_core::{ClockLattice, ClockLatticeErrorKind};

/// MLS §16.5: a statically periodic clock is an exact rational lattice point.
/// Seconds coming out of constant folding are rationalized here, once, and all
/// derived-clock composition afterwards is exact integer arithmetic.
pub(super) fn periodic_lattice_from_seconds(
    period: f64,
    phase: f64,
) -> Result<Option<ClockLattice>, ClockLatticeErrorKind> {
    if !period.is_finite() || period <= 0.0 {
        return Ok(None);
    }
    ClockLattice::from_seconds(period, phase).map(Some)
}

/// Accept a folded scalar as a strictly positive MLS clock counter only when it
/// really is an integer, within the shared clock-factor integer tolerance.
pub(super) fn exact_positive_integer(value: f64) -> Result<Option<i128>, ClockLatticeErrorKind> {
    let rounded = value.round();
    let tolerance = rumoca_core::CLOCK_FACTOR_INTEGER_TOLERANCE * rounded.abs().max(1.0);
    if (value - rounded).abs() > tolerance {
        return Ok(None);
    }
    positive_integer(rounded)
}

/// Accept a folded scalar as a strictly positive MLS clock factor/counter.
pub(super) fn positive_integer(value: f64) -> Result<Option<i128>, ClockLatticeErrorKind> {
    let rounded = value.round();
    if !rounded.is_finite() || rounded <= 0.0 {
        return Ok(None);
    }
    integer_in_range(rounded)
}

/// Accept a folded scalar as a non-negative MLS shift counter.
pub(super) fn non_negative_integer(value: f64) -> Result<Option<i128>, ClockLatticeErrorKind> {
    let rounded = value.round();
    if !rounded.is_finite() || rounded < 0.0 {
        return Ok(None);
    }
    integer_in_range(rounded)
}

fn integer_in_range(rounded: f64) -> Result<Option<i128>, ClockLatticeErrorKind> {
    if rounded < i128::MIN as f64 || rounded >= i128::MAX as f64 {
        return Err(ClockLatticeErrorKind::IntegerOverflow);
    }
    Ok(Some(rounded as i128))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decimal_seconds_become_exact_rational_lattice_points() {
        let lattice = periodic_lattice_from_seconds(0.1, 0.0)
            .expect("0.1 s is representable")
            .expect("0.1 s is positive");
        assert_eq!(lattice.period().numerator(), 1);
        assert_eq!(lattice.period().denominator(), 10);
    }

    #[test]
    fn non_positive_or_non_finite_periods_are_rejected() {
        assert_eq!(periodic_lattice_from_seconds(0.0, 0.0), Ok(None));
        assert_eq!(periodic_lattice_from_seconds(-1.0, 0.0), Ok(None));
        assert_eq!(periodic_lattice_from_seconds(f64::NAN, 0.0), Ok(None));
    }

    #[test]
    fn only_true_integers_pass_the_exact_counter_check() {
        assert_eq!(exact_positive_integer(3.0), Ok(Some(3)));
        assert_eq!(exact_positive_integer(0.5), Ok(None));
        assert_eq!(exact_positive_integer(0.0), Ok(None));
        // A folded ratio is accepted within the shared integer tolerance.
        assert_eq!(exact_positive_integer(3.0 + 1.0e-12), Ok(Some(3)));
    }

    #[test]
    fn conversion_factors_round_but_stay_in_range() {
        assert_eq!(positive_integer(2.7), Ok(Some(3)));
        assert_eq!(positive_integer(-1.0), Ok(None));
        assert_eq!(positive_integer(f64::INFINITY), Ok(None));
        assert_eq!(non_negative_integer(0.0), Ok(Some(0)));
        assert_eq!(non_negative_integer(-0.4), Ok(Some(0)));
        assert_eq!(non_negative_integer(-1.0), Ok(None));
        assert_eq!(positive_integer((1u64 << 63) as f64), Ok(Some(1i128 << 63)));
        assert_eq!(
            positive_integer(i128::MAX as f64),
            Err(ClockLatticeErrorKind::IntegerOverflow)
        );
    }
}
