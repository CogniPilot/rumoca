//! MLS §16 exact-lattice regressions for interpreted clock evaluation.

use super::*;

fn span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 0, 1)
}

fn lit(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: span(),
    }
}

fn call(name: &str, args: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new(name),
        args,
        is_constructor: false,
        span: span(),
    }
}

fn timing(expr: &rumoca_core::Expression) -> ClockTiming {
    let env = VarEnv::<f64>::new();
    infer_clock_timing_from_expr(expr, &env)
        .expect("clock timing inference must not fail")
        .expect("expression must denote a periodic clock")
}

#[test]
fn super_sample_then_sub_sample_returns_the_base_period_exactly() {
    let base = timing(&call("Clock", vec![lit(0.007)]));
    let fast = call(
        "superSample",
        vec![call("Clock", vec![lit(0.007)]), lit(3.0)],
    );
    let restored = timing(&call("subSample", vec![fast, lit(3.0)]));

    assert_ne!(0.007_f64 / 3.0 * 3.0, 0.007_f64);
    assert_eq!(restored.period, base.period);
    assert_eq!(restored.phase, base.phase);
}

#[test]
fn sub_sample_of_an_integer_clock_multiplies_the_period() {
    // MLS §16.5.2 Operator 16.9: subSample(u, factor) is factor times *slower*
    // than u, so the period is multiplied. This is the shape MSL 4.1.0
    // PeriodicExactClock emits: subSample(Clock(factor), resolutionFactor),
    // documented there as equivalent to Clock(factor*resolutionFactor, 1).
    let minutes = timing(&call(
        "subSample",
        vec![call("Clock", vec![lit(10.0)]), lit(60.0)],
    ));

    assert_eq!(minutes.period, 600.0);
    assert_eq!(minutes.phase, 0.0);
}

#[test]
fn interval_counter_and_decimal_clocks_agree_exactly() {
    // MLS §16.3: Clock(3, 10) and subSample(Clock(0.1), 3) are the same clock.
    let declared = timing(&call("Clock", vec![lit(3.0), lit(10.0)]));
    let derived = timing(&call(
        "subSample",
        vec![call("Clock", vec![lit(0.1)]), lit(3.0)],
    ));

    assert_ne!(0.1_f64 * 3.0, 0.3_f64);
    assert_eq!(declared.period, 0.3);
    assert_eq!(derived.period, declared.period);
}

#[test]
fn shift_then_back_sample_returns_to_zero_phase() {
    let third = call(
        "shiftSample",
        vec![call("Clock", vec![lit(0.007)]), lit(1.0), lit(3.0)],
    );
    let whole = call("shiftSample", vec![third, lit(2.0), lit(3.0)]);
    let restored = timing(&call("backSample", vec![whole, lit(1.0), lit(1.0)]));

    let drifted = (1.0 / 3.0) * 0.007 + (2.0 / 3.0) * 0.007 - 0.007;
    assert_ne!(drifted, 0.0, "the f64 shift chain must drift");
    assert_eq!(restored.phase, 0.0);
    assert_eq!(restored.period, 0.007);
}

#[test]
fn clock_ticks_use_the_shared_scheduler_tolerance() {
    // The acceptance window is on the *dimensionless* tick coordinate:
    // |ticks - round(ticks)| <= SCHEDULE_TIME_RELATIVE_TOLERANCE * (1 + max(|ticks|, |round|)).
    // At tick 2 that is 3 * tol tick units, i.e. 3 * tol * period seconds.
    let period = 0.02_f64;
    let event = 0.04_f64;
    let window_seconds = 3.0 * rumoca_core::SCHEDULE_TIME_RELATIVE_TOLERANCE * period;

    assert!(is_clock_tick(event, period, 0.0));
    // A quarter of the window in is still an activation ...
    assert!(is_clock_tick(event + 0.25 * window_seconds, period, 0.0));
    assert!(is_clock_tick(event - 0.25 * window_seconds, period, 0.0));
    // ... four times the window out is not.
    assert!(!is_clock_tick(event + 4.0 * window_seconds, period, 0.0));
    assert!(!is_clock_tick(event - 4.0 * window_seconds, period, 0.0));

    // The stale 1e-9 *seconds* window this predicate used to apply is three
    // orders of magnitude wider than the scheduler ever stops at.
    let stale_window = 1.0e-9 * period.max(1.0);
    assert!(!is_clock_tick(event + stale_window, period, 0.0));
}

#[test]
fn clock_does_not_tick_before_its_phase() {
    assert!(!is_clock_tick(0.0, 0.02, 0.01));
    assert!(is_clock_tick(0.01, 0.02, 0.01));
    assert!(is_clock_tick(0.03, 0.02, 0.01));
}

#[test]
fn two_to_the_sixty_third_factor_remains_exact() {
    let boundary = (1u64 << 63) as f64;
    let timing = timing(&call(
        "subSample",
        vec![call("Clock", vec![lit(1.0)]), lit(boundary)],
    ));

    assert_eq!(timing.period, boundary);
}

#[test]
fn overflowing_conversion_factor_preserves_the_lattice_error() {
    let env = VarEnv::<f64>::new();
    let huge = call(
        "subSample",
        vec![call("Clock", vec![lit(1.0e20)]), lit(1.0e20)],
    );

    assert!(matches!(
        infer_clock_timing_from_expr(&huge, &env),
        Err(EvalError::ClockLattice {
            kind: rumoca_core::ClockLatticeErrorKind::IntegerOverflow,
        })
    ));
}
