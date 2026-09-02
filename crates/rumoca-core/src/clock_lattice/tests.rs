use super::*;
use crate::SourceId;

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name(file!()), 0, 1)
}

fn rational(num: i64, den: i64) -> ClockRational {
    ClockRational::new(num, den).expect("test rational must reduce")
}

fn rational128(num: i128, den: i128) -> ClockRational {
    ClockRational::new(num, den).expect("test rational must reduce")
}

fn lattice(period: ClockRational, phase: ClockRational) -> ClockLattice {
    ClockLattice::new(period, phase).expect("test lattice must be positive")
}

fn indices(clock: ClockLattice, start: ClockRational, end: ClockRational) -> Option<(i128, i128)> {
    clock
        .nonnegative_tick_indices_in(start, end)
        .expect("test horizon index arithmetic must fit")
        .map(|range| (*range.start(), *range.end()))
}

fn enumerated_indices(
    clock: ClockLattice,
    start: ClockRational,
    end: ClockRational,
) -> Option<(i128, i128)> {
    let mut matching = (0i128..=64).filter(|index| {
        let tick = clock.tick_time(*index).expect("small tick must fit");
        start <= tick && tick <= end
    });
    let first = matching.next()?;
    let last = match matching.next_back() {
        Some(last) => last,
        None => first,
    };
    Some((first, last))
}

#[test]
fn simulation_start_relative_schedule_resolves_once_at_the_runtime_boundary() {
    let schedule =
        PeriodicClockSchedule::simulation_start_relative(lattice(rational(1, 4), rational(1, 4)))
            .expect("relative schedule is valid");
    let resolved = schedule
        .resolve_at(2.0)
        .expect("finite start time is exactly representable");
    assert_eq!(resolved.anchor(), ClockPhaseAnchor::Absolute);
    assert_eq!(resolved.lattice().period(), rational(1, 4));
    assert_eq!(resolved.lattice().phase(), rational(9, 4));
}

#[test]
fn resolved_nonzero_start_uses_absolute_phase_for_index_bounds() {
    let schedule =
        PeriodicClockSchedule::simulation_start_relative(lattice(rational(1, 4), rational(1, 4)))
            .expect("relative schedule is valid");
    let clock = schedule
        .resolve_at(2.0)
        .expect("start-relative phase resolves exactly")
        .lattice();

    assert_eq!(
        indices(clock, rational(2, 1), rational(11, 4)),
        Some((0, 2))
    );
}

#[test]
fn closed_horizon_includes_coincident_tick_endpoints() {
    let clock = lattice(rational(1, 3), rational(-2, 3));
    assert_eq!(
        indices(clock, rational(-1, 3), rational(2, 3)),
        Some((1, 4))
    );
    assert_eq!(
        indices(clock, rational(-1, 3), rational(-1, 3)),
        Some((1, 1))
    );
}

#[test]
fn just_before_and_after_a_tick_round_in_the_correct_direction() {
    let clock = lattice(rational(1, 2), rational(-1, 4));
    let just_before = rational(249, 1000);
    let tick = rational(1, 4);
    let just_after = rational(251, 1000);

    assert_eq!(indices(clock, just_before, tick), Some((1, 1)));
    assert_eq!(indices(clock, tick, just_after), Some((1, 1)));
    assert_eq!(indices(clock, just_after, rational(3, 4)), Some((2, 2)));
    assert_eq!(indices(clock, just_before, just_before), None);
}

#[test]
fn valid_empty_horizons_are_distinct_from_inverted_horizons() {
    let clock = lattice(ClockRational::ONE, rational(2, 1));
    assert_eq!(indices(clock, rational(-3, 1), rational(1, 1)), None);
    assert_eq!(indices(clock, rational(5, 2), rational(11, 4)), None);
    assert_eq!(
        clock.nonnegative_tick_indices_in(rational(1, 1), rational(0, 1)),
        Err(ClockLatticeErrorKind::InvertedHorizon)
    );
    assert_eq!(
        ClockLatticeErrorKind::InvertedHorizon.message(),
        "clock lattice horizon starts after it ends"
    );
}

#[test]
fn exact_index_division_survives_maximum_cross_products() {
    let maximum = i128::MAX;
    let phase = rational128(maximum - 1, maximum);
    let endpoint = rational128(maximum, maximum - 1);
    let clock = lattice(rational128(1, maximum), phase);

    assert_eq!(indices(clock, phase, endpoint), Some((0, 2)));
    assert_eq!(indices(clock, endpoint, endpoint), None);
    assert_eq!(
        endpoint.checked_sub(phase),
        Err(ClockLatticeErrorKind::IntegerOverflow),
        "the index operation must not require the rational difference to fit"
    );
}

#[test]
fn exact_index_division_covers_the_full_382_bit_numerator() {
    let maximum = i128::MAX;
    let phase = rational128(i128::MIN, maximum);
    let endpoint = rational128(maximum, maximum - 1);
    let period = rational128(maximum - 2, maximum);
    let clock = lattice(period, phase);

    assert_eq!(indices(clock, phase, endpoint), Some((0, 2)));
}

#[test]
fn minimum_and_maximum_numerators_have_exact_bounds() {
    let clock = lattice(
        ClockRational::integer(i128::MAX),
        ClockRational::integer(i128::MIN),
    );
    assert_eq!(
        indices(
            clock,
            ClockRational::integer(i128::MIN),
            ClockRational::integer(i128::MAX),
        ),
        Some((0, 2))
    );
}

#[test]
fn index_overflow_is_reported_at_the_adjacent_horizon() {
    let maximum_tick = lattice(ClockRational::ONE, ClockRational::ZERO);
    assert_eq!(
        indices(
            maximum_tick,
            ClockRational::integer(i128::MAX),
            ClockRational::integer(i128::MAX),
        ),
        Some((i128::MAX, i128::MAX))
    );

    let adjacent = lattice(ClockRational::ONE, rational128(-1, i128::MAX));
    assert_eq!(
        adjacent.nonnegative_tick_indices_in(
            ClockRational::integer(i128::MAX),
            ClockRational::integer(i128::MAX),
        ),
        Err(ClockLatticeErrorKind::IntegerOverflow)
    );
}

#[test]
fn index_overflow_rejects_a_quotient_with_its_first_high_bit_in_limb_two() {
    let clock = lattice(rational(1, 1024), ClockRational::ZERO);
    let endpoint = ClockRational::integer(1i128 << 120);

    assert_eq!(
        clock.nonnegative_tick_indices_in(endpoint, endpoint),
        Err(ClockLatticeErrorKind::IntegerOverflow),
        "the exact tick index is 2^130 and must not be truncated to its low 128 bits"
    );
}

#[test]
fn small_rational_horizons_match_exhaustive_tick_enumeration() {
    let clock_cases = (1..=4).flat_map(|period_num| {
        (1..=4).flat_map(move |period_den| {
            (-4..=4).map(move |phase_num| (period_num, period_den, phase_num))
        })
    });
    for (period_num, period_den, phase_num) in clock_cases {
        let clock = lattice(rational(period_num, period_den), rational(phase_num, 3));
        for start_num in -6..=6 {
            for end_num in start_num..=6 {
                let start = rational(start_num, 2);
                let end = rational(end_num, 2);
                let expected = enumerated_indices(clock, start, end);
                assert_eq!(
                    indices(clock, start, end),
                    expected,
                    "period={period_num}/{period_den}, phase={phase_num}/3, horizon={start_num}/2..={end_num}/2"
                );
            }
        }
    }
}

#[test]
fn rationals_reduce_and_normalize_denominator_sign() {
    let value = rational(6, -8);
    assert_eq!(value.numerator(), -3);
    assert_eq!(value.denominator(), 4);
}

#[test]
fn rational_current_wire_replays_through_checked_reduction() {
    let negative_denominator: ClockRational =
        serde_json::from_str(r#"{"num":6,"den":-8}"#).expect("negative denominator normalizes");
    let noncanonical: ClockRational =
        serde_json::from_str(r#"{"num":2,"den":4}"#).expect("noncanonical rational reduces");

    assert_eq!(negative_denominator, rational(-3, 4));
    assert_eq!(noncanonical, rational(1, 2));
    assert_eq!(
        serde_json::to_value(noncanonical).expect("rational serializes"),
        serde_json::json!({"num": 1, "den": 2})
    );
}

#[test]
fn rational_wire_rejects_zero_denominator() {
    let error = serde_json::from_str::<ClockRational>(r#"{"num":1,"den":0}"#)
        .expect_err("zero denominator must not enter the exact-clock domain");

    assert!(error.to_string().contains("zero denominator"));
}

#[test]
fn rational_wire_rejects_unknown_fields() {
    let wire = serde_json::json!({
        "num": 1,
        "den": 2,
        "unchecked": true,
    });
    let error = serde_json::from_value::<ClockRational>(wire)
        .expect_err("unknown rational fields must fail closed");

    assert!(error.to_string().contains("unknown field"));
}

#[test]
fn lattice_wire_rejects_nonpositive_periods() {
    for period in [
        serde_json::json!({"num": 0, "den": 1}),
        serde_json::json!({"num": -1, "den": 2}),
    ] {
        let wire = serde_json::json!({
            "period": period,
            "phase": {"num": 0, "den": 1},
        });
        let error = serde_json::from_value::<ClockLattice>(wire)
            .expect_err("nonpositive period must not enter the exact-clock domain");
        assert!(error.to_string().contains("strictly positive"));
    }
}

#[test]
fn lattice_wire_rejects_unknown_fields() {
    let wire = serde_json::json!({
        "period": {"num": 1, "den": 4},
        "phase": {"num": 0, "den": 1},
        "unchecked": true,
    });
    let error = serde_json::from_value::<ClockLattice>(wire)
        .expect_err("unknown lattice fields must fail closed");

    assert!(error.to_string().contains("unknown field"));
}

#[test]
fn schedule_current_wire_replays_through_the_selected_checked_constructor() {
    let lattice = lattice(rational(1, 4), rational(-1, 8));
    let absolute = PeriodicClockSchedule::absolute(lattice).expect("absolute schedule is valid");
    assert_eq!(
        serde_json::to_value(absolute).expect("schedule serializes"),
        serde_json::json!({
            "lattice": {
                "period": {"num": 1, "den": 4},
                "phase": {"num": -1, "den": 8},
            },
            "anchor": "absolute",
        })
    );

    for (anchor, expected) in [
        ("absolute", absolute),
        (
            "simulation_start",
            PeriodicClockSchedule::simulation_start_relative(lattice)
                .expect("start-relative schedule is valid"),
        ),
    ] {
        let wire = serde_json::json!({
            "lattice": {
                "period": {"num": 1, "den": 4},
                "phase": {"num": -1, "den": 8},
            },
            "anchor": anchor,
        });
        let decoded: PeriodicClockSchedule =
            serde_json::from_value(wire).expect("current schedule wire decodes");
        assert_eq!(decoded, expected);
    }
}

#[test]
fn schedule_wire_rejects_unknown_fields() {
    let wire = serde_json::json!({
        "lattice": {
            "period": {"num": 1, "den": 4},
            "phase": {"num": 0, "den": 1},
        },
        "anchor": "absolute",
        "unchecked": true,
    });
    let error = serde_json::from_value::<PeriodicClockSchedule>(wire)
        .expect_err("unknown schedule fields must fail closed");

    assert!(error.to_string().contains("unknown field"));
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
fn large_and_small_finite_seconds_round_trip_exactly() {
    let large = ClockRational::from_seconds(1.0e20).expect("1e20 must fit i128");
    let small = ClockRational::from_seconds(1.0e-20).expect("1e-20 must fit i128");

    assert_eq!(large, rational128(100_000_000_000_000_000_000, 1));
    assert!(
        small.denominator() > i128::from(i64::MAX),
        "the regression boundary must require the widened representation"
    );
    assert_eq!(large.to_f64(), 1.0e20);
    assert_eq!(small.to_f64(), 1.0e-20);
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

    assert_eq!(composed, base);
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

    assert_eq!(composed, base);
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
    assert_eq!(derived, declared);
    assert_eq!(derived.period_seconds(), declared.period_seconds());
}

#[test]
fn shift_and_back_sample_are_exact_inverses() {
    let base = lattice(rational(1, 10), ClockRational::ZERO);
    let shifted = base.shift_sample(1, 3).expect("exact shift");
    assert_eq!(shifted.phase(), rational(1, 30));
    assert_eq!(shifted.period(), base.period());

    let restored = shifted.back_sample(1, 3).expect("exact back sample");
    assert_eq!(restored, base);
}

#[test]
fn shift_by_a_whole_period_preserves_period_and_exact_phase() {
    let base = lattice(rational(1, 10), ClockRational::ZERO);
    let shifted = base.shift_sample(2, 1).expect("exact shift");

    assert_ne!(shifted, base);
    assert_eq!(shifted.period(), base.period());
    assert_eq!(shifted.phase(), rational(1, 5));
}

#[test]
fn half_period_shift_has_exact_phase() {
    let base = lattice(rational(1, 10), ClockRational::ZERO);
    let shifted = base.shift_sample(1, 2).expect("exact shift");

    assert_eq!(shifted.period(), base.period());
    assert_eq!(shifted.phase(), rational(1, 20));
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
fn tick_time_with_phase_is_exact() {
    let clock = lattice(rational(1, 10), rational(1, 20));
    assert_eq!(clock.tick_time(2), Ok(rational(1, 4)));
}

#[test]
fn accumulated_factor_and_tick_index_support_two_to_the_sixty_third() {
    let boundary = 1i128 << 63;
    let base = lattice(ClockRational::ONE, ClockRational::ZERO);
    let slow = base
        .sub_sample(1i64 << 32)
        .and_then(|clock| clock.sub_sample(1i64 << 31))
        .expect("CLK-017 requires accumulated factor 2^63");

    assert_eq!(slow.period(), ClockRational::integer(boundary));
    assert_eq!(
        base.tick_time(boundary),
        Ok(ClockRational::integer(boundary))
    );
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
    let clock = lattice(rational128(i128::MAX / 2 + 1, 1), ClockRational::ZERO);
    let kind = clock
        .sub_sample(2)
        .expect_err("a product above i128::MAX must not wrap");

    assert_eq!(kind, ClockLatticeErrorKind::IntegerOverflow);
    let spanned = kind.at(span());
    assert_eq!(spanned.span, span());
    assert_eq!(
        spanned.to_string(),
        "exact clock lattice arithmetic overflowed 128-bit integers"
    );
}

#[test]
fn overflowing_super_sample_reports_overflow() {
    let clock = lattice(rational128(1, i128::MAX / 2 + 1), ClockRational::ZERO);
    assert_eq!(
        clock.super_sample(2),
        Err(ClockLatticeErrorKind::IntegerOverflow)
    );
}

#[test]
fn multiplication_cross_cancels_before_checked_products() {
    let left = rational128(i128::MAX, 2);
    let right = rational128(2, i128::MAX);
    assert_eq!(left.checked_mul(right), Ok(ClockRational::ONE));

    let quotient_left = rational128(i128::MIN, i128::MAX);
    let quotient_right = rational128(i128::MIN, i128::MAX);
    assert_eq!(
        quotient_left.checked_div(quotient_right),
        Ok(ClockRational::ONE)
    );
}

#[test]
fn same_denominator_addition_reduces_before_narrowing() {
    let half_max = rational128(i128::MAX, 2);
    assert_eq!(
        half_max.checked_add(half_max),
        Ok(ClockRational::integer(i128::MAX))
    );
    assert_eq!(
        half_max.checked_sub(rational128(-i128::MAX, 2)),
        Ok(ClockRational::integer(i128::MAX))
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
fn subtraction_of_the_minimum_integer_is_reported_not_wrapped() {
    let extreme = ClockRational::integer(i128::MIN);
    assert_eq!(
        ClockRational::ZERO.checked_sub(extreme),
        Err(ClockLatticeErrorKind::IntegerOverflow)
    );
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
fn rational_ordering_is_exact_without_overflowing_cross_products() {
    assert!(rational(1, 3) < rational(1, 2));
    assert!(rational(-1, 3) < ClockRational::ZERO);
    assert_eq!(rational(2, 4), rational(1, 2));

    let almost_one = rational128(i128::MAX - 1, i128::MAX);
    assert!(almost_one < ClockRational::ONE);
    assert!(rational128(i128::MIN, i128::MAX) < rational(-1, 1));
}

#[test]
fn denominator_above_positive_i128_range_is_reported() {
    assert_eq!(
        ClockRational::new(1, i128::MIN),
        Err(ClockLatticeErrorKind::IntegerOverflow)
    );
}
