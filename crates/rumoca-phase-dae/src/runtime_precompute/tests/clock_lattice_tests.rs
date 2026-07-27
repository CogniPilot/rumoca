//! MLS §16.5 exact rational base-clock lattice regressions.
//!
//! Each case here fails when derived-clock composition is carried out in `f64`
//! and passes only when the composition is exact integer arithmetic over a
//! rational base clock.

use super::*;

fn call(name: &str, args: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(name).into(),
        args,
        is_constructor: false,
        span: test_span(30, 42),
    }
}

fn declare_discrete(dae_model: &mut dae::Dae, name: &str) {
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new(name),
        dae::Variable::new(rumoca_core::VarName::new(name), test_span(1, 2)),
    );
}

fn assign(dae_model: &mut dae::Dae, name: &str, rhs: rumoca_core::Expression, origin: &str) {
    declare_discrete(dae_model, name);
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new(name).into()),
        rhs,
        span: test_span(1, 2),
        origin: origin.to_string(),
        scalar_count: 1,
    });
}

fn declare_parameter(dae_model: &mut dae::Dae, name: &str, value: f64) {
    let mut parameter = dae::Variable::new(rumoca_core::VarName::new(name), test_span(1, 2));
    parameter.start = Some(lit(value));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new(name), parameter);
}

#[test]
fn sub_sample_of_an_integer_clock_multiplies_the_period() {
    // MLS §16.5.2 Operator 16.9: "The clock of y = subSample(u, factor) is
    // factor times slower than the clock of u." The period is therefore
    // *multiplied* by factor, never divided by it.
    let mut dae_model = dae::Dae::default();
    assign(
        &mut dae_model,
        "minutes",
        call("subSample", vec![clock_call(10.0), lit(60.0)]),
        "minutes = subSample(Clock(10), 60)",
    );

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    let minutes = dae_model.clocks.timings["minutes"].clone();
    assert_eq!(
        minutes.period_seconds, 600.0,
        "subSample(Clock(10), 60) is 60x slower than a 10 s clock"
    );
    assert_eq!(minutes.phase_seconds, 0.0);
}

#[test]
fn periodic_exact_clock_shape_ticks_on_the_multiplied_period() {
    // MSL 4.1.0 Clocked.ClockSignals.Clocks.PeriodicExactClock emits
    //     c = subSample(Clock(factor), resolutionFactor);
    // for resolutions coarser than a second, and its own source comment states
    // that this "corresponds to the simpler Clock(factor*resolutionFactor, 1)".
    // With factor = 10 and resolution = min (resolutionFactor = 60) the block
    // must tick every 600 s.
    let mut dae_model = dae::Dae::default();
    declare_parameter(&mut dae_model, "periodicClock.factor", 10.0);
    declare_parameter(&mut dae_model, "periodicClock.resolutionFactor", 60.0);
    assign(
        &mut dae_model,
        "periodicClock.c",
        call(
            "subSample",
            vec![
                call("Clock", vec![var("periodicClock.factor")]),
                var("periodicClock.resolutionFactor"),
            ],
        ),
        "periodicClock.c = subSample(Clock(factor), resolutionFactor)",
    );
    assign(
        &mut dae_model,
        "periodicClock.y",
        var("periodicClock.c"),
        "periodicClock.y = periodicClock.c",
    );

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    let output = dae_model.clocks.timings["periodicClock.y"].clone();
    assert_eq!(
        output.period_seconds, 600.0,
        "PeriodicExactClock(factor = 10, resolution = min) ticks every 600 s"
    );
    let declared = dae::ClockSchedule::from_lattice(
        rumoca_core::ClockLattice::from_interval_counter(600, 1).expect("600 s is rational"),
        test_span(1, 2),
    );
    assert!(
        output.is_same_clock(&declared),
        "the subSample form must be the same clock as Clock(factor*resolutionFactor, 1)"
    );
}

#[test]
fn super_sample_then_sub_sample_recovers_the_base_clock_exactly() {
    // MLS §16.5.2: superSample(u, 3) followed by subSample(., 3) is the
    // identity on the clock. In f64, 0.007 / 3 * 3 != 0.007.
    let mut dae_model = dae::Dae::default();
    assign(
        &mut dae_model,
        "base",
        clock_call(0.007),
        "base = Clock(0.007)",
    );
    assign(
        &mut dae_model,
        "fast",
        call("superSample", vec![var("base"), lit(3.0)]),
        "fast = superSample(base, 3)",
    );
    assign(
        &mut dae_model,
        "slow",
        call("subSample", vec![var("fast"), lit(3.0)]),
        "slow = subSample(fast, 3)",
    );

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    let base = dae_model.clocks.timings["base"].clone();
    let slow = dae_model.clocks.timings["slow"].clone();
    assert_ne!(0.007_f64 / 3.0 * 3.0, 0.007_f64);
    assert_eq!(slow.period_seconds, base.period_seconds);
    assert!(slow.is_same_clock(&base));
}

#[test]
fn rationally_equal_clocks_from_different_chains_share_one_schedule() {
    // subSample(Clock(0.1), 3) and Clock(3, 10) are the same clock, but their
    // naive f64 periods differ (0.1 * 3 != 0.3), so an epsilon-based dedup on
    // seconds keeps two schedules.
    let mut dae_model = dae::Dae::default();
    assign(
        &mut dae_model,
        "derived",
        call("subSample", vec![clock_call(0.1), lit(3.0)]),
        "derived = subSample(Clock(0.1), 3)",
    );
    assign(
        &mut dae_model,
        "declared",
        call("Clock", vec![lit(3.0), lit(10.0)]),
        "declared = Clock(3, 10)",
    );

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert_ne!(0.1_f64 * 3.0, 3.0_f64 / 10.0);
    let derived = dae_model.clocks.timings["derived"].clone();
    let declared = dae_model.clocks.timings["declared"].clone();
    assert_eq!(derived.period_seconds, 0.3);
    assert_eq!(declared.period_seconds, 0.3);
    assert!(derived.is_same_clock(&declared));

    // The two 0.3 s clocks collapse into a single schedule; the remaining
    // schedule is the nested 0.1 s base clock of the subSample chain.
    let quarter_second_schedules = dae_model
        .clocks
        .schedules
        .iter()
        .filter(|schedule| schedule.is_same_clock(&declared))
        .count();
    assert_eq!(
        quarter_second_schedules, 1,
        "clocks that are equal as rationals must collapse to one schedule"
    );
    assert!(
        dae_model
            .clocks
            .schedules
            .iter()
            .all(|schedule| schedule.period_seconds == 0.1 || schedule.period_seconds == 0.3),
        "every schedule must land exactly on the rational grid"
    );
}

#[test]
fn long_conversion_chain_stays_on_the_lattice() {
    // Seven nested integer conversions: any f64 rescaling accumulates error,
    // the exact lattice does not.
    let mut dae_model = dae::Dae::default();
    assign(&mut dae_model, "c0", clock_call(0.007), "c0 = Clock(0.007)");
    let factors = [3.0, 7.0, 11.0];
    for (index, factor) in factors.iter().enumerate() {
        let source = var(&format!("c{index}"));
        assign(
            &mut dae_model,
            &format!("c{}", index + 1),
            call("superSample", vec![source, lit(*factor)]),
            "superSample chain",
        );
    }
    for (index, factor) in factors.iter().rev().enumerate() {
        let source = var(&format!("c{}", index + 3));
        assign(
            &mut dae_model,
            &format!("c{}", index + 4),
            call("subSample", vec![source, lit(*factor)]),
            "subSample chain",
        );
    }

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    let start = dae_model.clocks.timings["c0"].clone();
    let end = dae_model.clocks.timings["c6"].clone();
    let mut drifted = 0.007_f64;
    for factor in factors {
        drifted /= factor;
    }
    for factor in factors.iter().rev() {
        drifted *= factor;
    }
    assert_ne!(drifted, 0.007_f64, "the f64 chain must drift");
    assert_eq!(end.period_seconds, start.period_seconds);
    assert!(end.is_same_clock(&start));
}

#[test]
fn shift_and_back_sample_cancel_on_the_lattice() {
    // MLS §16.5.2: backSample undoes shiftSample with the same counter and
    // resolution, exactly.
    let mut dae_model = dae::Dae::default();
    assign(
        &mut dae_model,
        "third",
        call("shiftSample", vec![clock_call(0.007), lit(1.0), lit(3.0)]),
        "third = shiftSample(Clock(0.007), 1, 3)",
    );
    assign(
        &mut dae_model,
        "whole",
        call("shiftSample", vec![var("third"), lit(2.0), lit(3.0)]),
        "whole = shiftSample(third, 2, 3)",
    );
    assign(
        &mut dae_model,
        "restored",
        call("backSample", vec![var("whole"), lit(1.0), lit(1.0)]),
        "restored = backSample(whole, 1, 1)",
    );

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    // 1/3 + 2/3 of the interval is exactly one interval, so backSample by a
    // whole interval must land back on zero phase. In f64 it does not.
    let drifted = (1.0 / 3.0) * 0.007 + (2.0 / 3.0) * 0.007 - 0.007;
    assert_ne!(drifted, 0.0, "the f64 shift chain must drift");

    let restored = dae_model.clocks.timings["restored"].clone();
    assert_eq!(restored.period_seconds, 0.007);
    assert_eq!(
        restored.phase_seconds, 0.0,
        "an exact shift/back-shift pair must return to zero phase"
    );
}

#[test]
fn interval_counter_clock_keeps_its_exact_rational_period() {
    // MLS §16.3 `Clock(intervalCounter, resolution)` names the exact rational
    // 1/3 s. A one-third second period is not an `f64`, so the only way a
    // §16.5.2 conversion round trip over it can return to the *same* clock is
    // if the constructor's rational — not its rounded seconds — is what the
    // conversions rescale.
    let mut dae_model = dae::Dae::default();
    assign(
        &mut dae_model,
        "third",
        call("Clock", vec![lit(1.0), lit(3.0)]),
        "third = Clock(1, 3)",
    );
    // 169 is one of the factors for which the f64 rescale of 1/3 does not
    // return to its starting double, so the round trip is observable.
    assign(
        &mut dae_model,
        "fast",
        call("superSample", vec![var("third"), lit(169.0)]),
        "fast = superSample(third, 169)",
    );
    assign(
        &mut dae_model,
        "restored",
        call("subSample", vec![var("fast"), lit(169.0)]),
        "restored = subSample(fast, 169)",
    );

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    // Rescaling the rounded seconds instead loses the clock.
    assert_ne!((1.0_f64 / 3.0) / 169.0 * 169.0, 1.0_f64 / 3.0);

    let third = dae_model.clocks.timings["third"].clone();
    let restored = dae_model.clocks.timings["restored"].clone();
    assert_eq!(restored.period_seconds, third.period_seconds);
    assert!(
        restored.is_same_clock(&third),
        "superSample/subSample by the same factor must return the identical clock"
    );
    let period = third.lattice().expect("Clock(1, 3) is rational").period();
    assert_eq!(period.numerator(), 1);
    assert_eq!(period.denominator(), 3);
}

#[test]
fn back_sample_before_the_base_clock_start_is_reported() {
    // MLS §16.5.2 Operator 16.12: "It is an error if the clock of y starts
    // before the base-clock of u." Shifting a zero-phase clock backwards must
    // surface as a spanned phase error (SPEC_0008), not as a clock whose first
    // activation silently sits at a negative time.
    let mut dae_model = dae::Dae::default();
    assign(
        &mut dae_model,
        "base",
        clock_call(0.02),
        "base = Clock(0.02)",
    );
    assign(
        &mut dae_model,
        "early",
        call("backSample", vec![var("base"), lit(1.0), lit(100.0)]),
        "early = backSample(base, 1, 100)",
    );

    let error = populate_runtime_precompute(&mut dae_model)
        .expect_err("a clock that starts before its base clock must not resolve");

    assert!(
        matches!(error, ToDaeError::UnresolvedClockSchedule { .. }),
        "expected an unresolved clock schedule error, got {error:?}"
    );
}

#[test]
fn overflowing_conversion_factor_is_reported_not_wrapped() {
    // A conversion factor that cannot be applied without leaving the exact
    // integer range must surface as an unresolved-clock phase error rather
    // than a wrapped or rounded period (SPEC_0008).
    let mut dae_model = dae::Dae::default();
    assign(
        &mut dae_model,
        "base",
        call("subSample", vec![clock_call(0.1), lit(3.0)]),
        "base = subSample(Clock(0.1), 3)",
    );
    assign(
        &mut dae_model,
        "huge",
        call("subSample", vec![var("base"), lit(i64::MAX as f64)]),
        "huge = subSample(base, 9223372036854775807)",
    );

    let error = populate_runtime_precompute(&mut dae_model)
        .expect_err("an overflowing clock conversion must not be silently wrapped");

    assert!(
        matches!(error, ToDaeError::UnresolvedClockSchedule { .. }),
        "expected an unresolved clock schedule error, got {error:?}"
    );
}
