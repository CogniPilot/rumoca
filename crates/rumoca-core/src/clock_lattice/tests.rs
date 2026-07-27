use super::*;
use crate::SourceId;

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name(file!()), 0, 1)
}

fn rational(num: i64, den: i64) -> ClockRational {
    ClockRational::new(num, den).expect("test rational must reduce")
}

fn lattice(period: ClockRational, phase: ClockRational) -> ClockLattice {
    ClockLattice::new(period, phase).expect("test lattice must be positive")
}

#[test]
fn rationals_reduce_and_normalize_denominator_sign() {
    let value = rational(6, -8);
    assert_eq!(value.numerator(), -3);
    assert_eq!(value.denominator(), 4);
}

#[test]
fn decimal_seconds_recover_their_short_rational_form() {
    assert_eq!(ClockRational::from_seconds(0.1), Ok(rational(1, 10)));
    assert_eq!(ClockRational::from_seconds(1.0e-4), Ok(rational(1, 10_000)));
    assert_eq!(ClockRational::from_seconds(0.025), Ok(rational(1, 40)));
    assert_eq!(ClockRational::from_seconds(-0.375), Ok(rational(-3, 8)));
    assert_eq!(ClockRational::from_seconds(0.0), Ok(ClockRational::ZERO));
}

#[test]
fn seconds_round_trip_is_faithful_not_merely_close() {
    // 0.1 * 3 is a different double from 3/10; the rationalization must not
    // silently collapse the two.
    let drifted = 0.1_f64 * 3.0;
    assert_ne!(drifted, 0.3_f64);
    let recovered = ClockRational::from_seconds(drifted).expect("finite value is representable");
    assert_ne!(recovered, rational(3, 10));
    assert_eq!(recovered.to_f64(), drifted);
}

#[test]
fn non_finite_seconds_are_rejected() {
    assert_eq!(
        ClockRational::from_seconds(f64::NAN),
        Err(ClockLatticeErrorKind::NonFiniteSeconds)
    );
    assert_eq!(
        ClockRational::from_seconds(f64::INFINITY),
        Err(ClockLatticeErrorKind::NonFiniteSeconds)
    );
}

#[test]
fn super_sample_then_sub_sample_returns_the_original_clock() {
    // MLS §16.5.2: superSample(u, 3) and subSample(., 3) are exact inverse
    // integer relations, so the composed clock must be identical to `u`.
    let base = lattice(rational(7, 1000), ClockRational::ZERO);
    let composed = base
        .super_sample(3)
        .and_then(|clock| clock.sub_sample(3))
        .expect("integer composition must stay exact");

    assert!(composed.is_same_clock(base));
    assert_eq!(composed.period_seconds(), base.period_seconds());

    // The same chain in f64 does not come back to the starting period.
    let drifted = base.period_seconds() / 3.0 * 3.0;
    assert_ne!(drifted, base.period_seconds());
}

#[test]
fn long_composition_chain_is_exact_where_f64_is_not() {
    let base = lattice(rational(1, 10), ClockRational::ZERO);
    let mut composed = base;
    let mut drifted = base.period_seconds();
    for factor in [3, 7, 11, 13] {
        composed = composed.super_sample(factor).expect("exact super sample");
        drifted /= factor as f64;
    }
    for factor in [3, 7, 11, 13] {
        composed = composed.sub_sample(factor).expect("exact sub sample");
        drifted *= factor as f64;
    }

    assert!(composed.is_same_clock(base));
    assert_ne!(drifted, base.period_seconds());
}

#[test]
fn rationally_equal_clocks_from_different_chains_tick_together() {
    // subSample(Clock(0.1), 3) and Clock(3, 10) are the same clock; their naive
    // f64 periods differ.
    let derived = lattice(rational(1, 10), ClockRational::ZERO)
        .sub_sample(3)
        .expect("exact sub sample");
    let declared = ClockLattice::from_interval_counter(3, 10).expect("MLS §16.3 rational clock");

    assert_ne!(0.1_f64 * 3.0, 3.0 / 10.0);
    assert!(derived.is_same_clock(declared));
    assert!(
        derived
            .ticks_simultaneously_with(declared)
            .expect("exact simultaneity test")
    );
    assert_eq!(derived.period_seconds(), declared.period_seconds());
}

#[test]
fn shift_and_back_sample_are_exact_inverses() {
    let base = lattice(rational(1, 10), ClockRational::ZERO);
    let shifted = base.shift_sample(1, 3).expect("exact shift");
    assert_eq!(shifted.phase(), rational(1, 30));
    assert_eq!(shifted.period(), base.period());

    let restored = shifted.back_sample(1, 3).expect("exact back sample");
    assert!(restored.is_same_clock(base));
}

#[test]
fn shift_by_a_whole_period_still_ticks_simultaneously() {
    let base = lattice(rational(1, 10), ClockRational::ZERO);
    let shifted = base.shift_sample(2, 1).expect("exact shift");

    assert!(!shifted.is_same_clock(base));
    assert!(
        shifted
            .ticks_simultaneously_with(base)
            .expect("exact simultaneity test")
    );
}

#[test]
fn half_period_shift_does_not_tick_simultaneously() {
    let base = lattice(rational(1, 10), ClockRational::ZERO);
    let shifted = base.shift_sample(1, 2).expect("exact shift");

    assert!(
        !shifted
            .ticks_simultaneously_with(base)
            .expect("exact simultaneity test")
    );
}

#[test]
fn long_horizon_tick_grid_does_not_drift() {
    let clock = lattice(rational(1, 1000), ClockRational::ZERO);
    let period = clock.period_seconds();
    let mut accumulated = 0.0_f64;
    let mut drift_seen = false;
    for index in 0..1_000_000i64 {
        if index % 100_000 == 0 {
            let exact = clock.tick_time_seconds(index).expect("exact tick time");
            assert_eq!(exact, index as f64 / 1000.0);
            drift_seen |= accumulated != exact;
        }
        accumulated += period;
    }
    assert!(drift_seen, "f64 accumulation must drift off the exact grid");

    // A single f64 multiply is no better than accumulation at this horizon.
    assert_eq!(clock.tick_time_seconds(999_983), Ok(999.983));
    assert_ne!(999_983.0_f64 * period, 999.983);
}

#[test]
fn tick_index_and_membership_are_exact() {
    let clock = lattice(rational(1, 10), rational(1, 20));
    assert_eq!(clock.tick_index_at_or_before(rational(1, 4)), Ok(2));
    assert_eq!(clock.tick_time(2), Ok(rational(1, 4)));
    assert_eq!(clock.ticks_at(rational(1, 4)), Ok(true));
    assert_eq!(clock.ticks_at(rational(1, 5)), Ok(false));
    assert_eq!(clock.ticks_at(ClockRational::ZERO), Ok(false));
    assert_eq!(clock.ticks_at(rational(1, 20)), Ok(true));
}

#[test]
fn non_positive_periods_and_factors_are_rejected() {
    assert_eq!(
        ClockLattice::new(ClockRational::ZERO, ClockRational::ZERO),
        Err(ClockLatticeErrorKind::NonPositivePeriod)
    );
    let clock = lattice(ClockRational::ONE, ClockRational::ZERO);
    assert_eq!(
        clock.sub_sample(0),
        Err(ClockLatticeErrorKind::NonPositiveFactor)
    );
    assert_eq!(
        clock.super_sample(-2),
        Err(ClockLatticeErrorKind::NonPositiveFactor)
    );
    assert_eq!(
        clock.shift_sample(1, 0),
        Err(ClockLatticeErrorKind::NonPositiveFactor)
    );
    assert_eq!(
        ClockLattice::from_interval_counter(0, 10),
        Err(ClockLatticeErrorKind::NonPositiveFactor)
    );
}

#[test]
fn overflowing_composition_reports_a_spanned_error_instead_of_wrapping() {
    let clock = lattice(rational(1_000_003, 1), ClockRational::ZERO);
    let kind = clock
        .sub_sample(i64::MAX)
        .expect_err("i64::MAX sub-sampling must not wrap");

    assert_eq!(kind, ClockLatticeErrorKind::IntegerOverflow);
    let spanned = kind.at(span());
    assert_eq!(spanned.span, span());
    assert_eq!(
        spanned.to_string(),
        "exact clock lattice arithmetic overflowed 64-bit integers"
    );
}

#[test]
fn overflowing_super_sample_reports_overflow() {
    let clock = lattice(rational(1, 3_037_000_500), ClockRational::ZERO);
    assert_eq!(
        clock.super_sample(3_037_000_500),
        Err(ClockLatticeErrorKind::IntegerOverflow)
    );
}

#[test]
fn back_sample_before_the_base_clock_start_is_an_error() {
    // MLS §16.5.2 Operator 16.12: "It is an error if the clock of y starts
    // before the base-clock of u." A zero-phase clock cannot be shifted back.
    let clock = lattice(rational(1, 10), ClockRational::ZERO);
    assert_eq!(
        clock.back_sample(1, 1),
        Err(ClockLatticeErrorKind::ClockStartsBeforeBaseClock)
    );
    // Even a fraction of one interval is an error at zero phase.
    assert_eq!(
        clock.back_sample(1, 3),
        Err(ClockLatticeErrorKind::ClockStartsBeforeBaseClock)
    );

    // A clock already shifted forward may be shifted back to, but not past,
    // tick zero.
    let shifted = clock.shift_sample(2, 3).expect("forward shift is exact");
    assert_eq!(
        shifted.back_sample(2, 3).map(ClockLattice::phase),
        Ok(ClockRational::ZERO)
    );
    assert_eq!(
        shifted.back_sample(3, 3),
        Err(ClockLatticeErrorKind::ClockStartsBeforeBaseClock)
    );
    assert_eq!(
        ClockLatticeErrorKind::ClockStartsBeforeBaseClock
            .at(span())
            .to_string(),
        "backSample would start the clock before its base clock (MLS §16.5.2)"
    );
}

#[test]
fn negation_of_the_minimum_integer_is_reported_not_wrapped() {
    // Every other operation in this module is overflow-checked; negation must
    // be too, because -i64::MIN panics in debug and wraps in release.
    let extreme = ClockRational::integer(i64::MIN);
    assert_eq!(
        extreme.checked_negate(),
        Err(ClockLatticeErrorKind::IntegerOverflow)
    );
    assert_eq!(
        ClockRational::ZERO.checked_sub(extreme),
        Err(ClockLatticeErrorKind::IntegerOverflow)
    );
    assert_eq!(rational(-3, 4).checked_negate(), Ok(rational(3, 4)));
}

#[test]
fn division_by_zero_is_reported() {
    assert_eq!(
        ClockRational::ONE.checked_div(ClockRational::ZERO),
        Err(ClockLatticeErrorKind::ZeroDenominator)
    );
    assert_eq!(
        ClockRational::new(1, 0),
        Err(ClockLatticeErrorKind::ZeroDenominator)
    );
}

#[test]
fn rational_ordering_uses_exact_cross_multiplication() {
    assert!(rational(1, 3) < rational(1, 2));
    assert!(rational(-1, 3) < ClockRational::ZERO);
    assert_eq!(rational(2, 4), rational(1, 2));
}
