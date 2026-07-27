use super::*;
use rumoca_core::SourceId;

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name(file!()), 0, 1)
}

fn rational(num: i64, den: i64) -> ClockRational {
    ClockRational::new(num, den).expect("test rational must reduce")
}

#[test]
fn periodic_base_clock_stores_the_exact_rational_interval() {
    let clock = BaseClock::periodic(0.1, span());
    let interval = clock.base_interval.expect("periodic clock has an interval");

    assert_eq!(interval.exact_period(), Ok(rational(1, 10)));
    assert_eq!(interval.seconds(), 0.1);
    assert_eq!(
        clock.lattice().map(ClockLattice::period),
        Ok(rational(1, 10))
    );
}

#[test]
fn interval_counter_base_clock_matches_the_decimal_form() {
    // MLS §16.3: Clock(1, 10) and Clock(0.1) name the same clock.
    let counted = BaseClock::rational(1, 10, span()).expect("exact rational clock");
    let decimal = BaseClock::periodic(0.1, span());

    assert!(
        counted
            .lattice()
            .expect("counted lattice")
            .is_same_clock(decimal.lattice().expect("decimal lattice"))
    );
    assert!(matches!(
        counted.kind,
        ClockKind::Rational {
            interval_counter: 1,
            resolution: 10
        }
    ));
}

#[test]
fn inferred_clock_has_no_lattice() {
    let clock = BaseClock::inferred(span());
    let error = clock
        .lattice()
        .expect_err("an inferred clock has no period");

    assert_eq!(
        error.kind,
        ClockLatticeErrorKind::NotRationallyRepresentable
    );
    assert_eq!(error.span, span());
}

#[test]
fn super_sample_then_sub_sample_reproduces_the_base_clock_exactly() {
    let base = BaseClock::periodic(0.007, span())
        .lattice()
        .expect("exact base lattice");
    let up = SubClock::super_sample(3, span())
        .derive(base)
        .expect("exact super sample");
    let down = SubClock::sub_sample(3, span())
        .derive(up)
        .expect("exact sub sample");

    assert!(down.is_same_clock(base));
    assert_eq!(down.period_seconds(), base.period_seconds());
    assert_ne!(base.period_seconds() / 3.0 * 3.0, base.period_seconds());
}

#[test]
fn sub_clock_shift_and_back_shift_cancel_exactly() {
    let base = BaseClock::periodic(0.1, span())
        .lattice()
        .expect("exact base lattice");
    let shifted = SubClock::shift_sample(1, 3, span())
        .derive(base)
        .expect("exact shift sample");
    assert_eq!(shifted.phase(), rational(1, 30));

    let restored = SubClock::back_sample(1, 3, span())
        .derive(shifted)
        .expect("exact back sample");
    assert!(restored.is_same_clock(base));
}

#[test]
fn no_clock_sub_partitions_have_no_lattice() {
    let mut sub_clock = SubClock::empty_with_span(span());
    sub_clock.no_clock = true;
    let base = BaseClock::periodic(0.1, span())
        .lattice()
        .expect("exact base lattice");

    let error = sub_clock
        .derive(base)
        .expect_err("noClock() has no periodic lattice");
    assert_eq!(
        error.kind,
        ClockLatticeErrorKind::NotRationallyRepresentable
    );
}

#[test]
fn overflowing_sub_sample_reports_a_spanned_error() {
    let base = BaseClock::rational(1_000_003, 1, span())
        .expect("exact rational clock")
        .lattice()
        .expect("exact base lattice");

    let error = SubClock::sub_sample(i64::MAX, span())
        .derive(base)
        .expect_err("i64::MAX sub-sampling must not wrap");

    assert_eq!(error.kind, ClockLatticeErrorKind::IntegerOverflow);
    assert_eq!(error.span, span());
}

#[test]
fn non_positive_interval_counter_is_rejected() {
    let error = BaseClock::rational(1, 0, span()).expect_err("resolution must be positive");
    assert_eq!(error.kind, ClockLatticeErrorKind::NonPositiveFactor);
    assert_eq!(error.span, span());
}
