use super::super::RootLocationPlan;
use super::RootBracket;

/// A bracket of the size an integrator step leaves around a crossing, with the
/// standard plan's tolerance at that time scale.
const LOW: f64 = 0.3;
const WIDTH: f64 = 1.0e-5;
const TOLERANCE: f64 = 100.0 * f64::EPSILON * 0.125;

fn positive(value: f64) -> bool {
    value > 0.0
}

/// Narrow `[low, high]` over the indicators `shape` returns until located,
/// as every executor does; returns the bracket and the evaluation count.
fn locate(
    shape: &dyn Fn(f64) -> Vec<f64>,
    low: f64,
    high: f64,
    tolerance: f64,
) -> (RootBracket, usize) {
    let reference = shape(low).into_iter().map(positive).collect::<Vec<_>>();
    let entered_at = |values: &[f64], index: usize| positive(values[index]) != reference[index];
    let mut bracket = RootLocationPlan::STANDARD.open_bracket(low, high, tolerance);
    let mut at_low = shape(low);
    let mut at_high = shape(high);
    let mut evaluations = 0;
    while !bracket.is_located() {
        let least = (0..at_high.len())
            .filter(|&index| entered_at(&at_high, index))
            .map(|index| bracket.crossing_fraction(at_low[index], at_high[index]))
            .fold(1.0, f64::min);
        let Some(trial) = bracket.trial(least) else {
            break;
        };
        let values = shape(trial);
        evaluations += 1;
        let entered = (0..values.len()).any(|index| entered_at(&values, index));
        bracket.narrow(trial, entered);
        if entered {
            at_high = values;
        } else {
            at_low = values;
        }
        assert!(evaluations <= bracket.evaluation_bound() + 1, "runaway");
    }
    (bracket, evaluations)
}

/// The previous method: bisection to the same tolerance.
fn bisect(shape: &dyn Fn(f64) -> f64, mut low: f64, mut high: f64, tolerance: f64) -> (f64, usize) {
    let reference = positive(shape(low));
    let mut evaluations = 0;
    while high - low > tolerance {
        let middle = low + 0.5 * (high - low);
        evaluations += 1;
        if positive(shape(middle)) != reference {
            high = middle;
        } else {
            low = middle;
        }
    }
    (high, evaluations)
}

/// A named indicator and the bracket fraction of its crossing.
type Shape = (&'static str, Box<dyn Fn(f64) -> f64>, f64);

/// Shapes with one domain change in the bracket at a known fraction, or at
/// its end.
fn shapes() -> Vec<Shape> {
    let unit = |t: f64| (t - LOW) / WIDTH;
    vec![
        ("linear", Box::new(move |t| unit(t) - 0.37), 0.37),
        (
            "quadratic",
            Box::new(move |t| unit(t) * unit(t) - 0.4),
            0.4_f64.sqrt(),
        ),
        (
            "flat then steep",
            Box::new(move |t| unit(t).powi(12) - 1.0e-3),
            1.0e-3_f64.powf(1.0 / 12.0),
        ),
        (
            "cubic inflection",
            Box::new(move |t| (unit(t) - 0.5).powi(3)),
            0.5,
        ),
        (
            "exponential",
            Box::new(move |t| (20.0 * unit(t)).exp() - 14.0_f64.exp()),
            0.7,
        ),
        (
            "jump",
            Box::new(move |t| if unit(t) < 0.61 { -1.0 } else { 1.0 }),
            0.61,
        ),
        (
            "near the low end",
            Box::new(move |t| unit(t) - 1.0e-9),
            1.0e-9,
        ),
        (
            "near the high end",
            Box::new(move |t| unit(t) - (1.0 - 1.0e-9)),
            1.0 - 1.0e-9,
        ),
        (
            "zero at the high end",
            Box::new(move |t| 1.0 - unit(t)),
            1.0,
        ),
        ("zero at the low end", Box::new(unit), 0.0),
        (
            "sine",
            Box::new(move |t| (2.0 * unit(t) + 0.3).sin() - 0.5),
            (0.5_f64.asin() - 0.3) / 2.0,
        ),
    ]
}

/// Every shape is located on the correct side of its crossing, within the
/// tolerance of where bisection locates it, and in no more evaluations than
/// bisection plus the plan's slack.
#[test]
fn every_crossing_shape_is_located_where_bisection_locates_it() {
    let slack = RootLocationPlan::STANDARD.minmax_slack() as usize;
    for (name, shape, fraction) in shapes() {
        let vector = |t: f64| vec![shape(t)];
        let (bracket, evaluations) = locate(&vector, LOW, LOW + WIDTH, TOLERANCE);
        let (bisected, bisections) = bisect(&*shape, LOW, LOW + WIDTH, TOLERANCE);
        let reference = positive(shape(LOW));
        assert!(bracket.is_located(), "{name}: {bracket:?}");
        assert_eq!(
            positive(shape(bracket.low())),
            reference,
            "{name}: low side"
        );
        assert_ne!(
            positive(shape(bracket.high())),
            reference,
            "{name}: high side"
        );
        let crossing = LOW + WIDTH * fraction;
        assert!(
            bracket.low() - TOLERANCE <= crossing && crossing <= bracket.high() + TOLERANCE,
            "{name}: crossing {crossing} outside {bracket:?}"
        );
        assert!(
            (bracket.high() - bisected).abs() <= TOLERANCE,
            "{name}: {} against bisection {bisected}",
            bracket.high()
        );
        assert!(
            evaluations <= bisections + slack,
            "{name}: {evaluations} evaluations, bisection {bisections}"
        );
        assert!(evaluations <= bracket.evaluation_bound(), "{name}");
    }
}

/// A smooth indicator is located in a few evaluations where bisection needs
/// about thirty.
#[test]
fn a_smooth_indicator_is_located_in_far_fewer_evaluations_than_bisection() {
    for (name, shape, _) in shapes()
        .into_iter()
        .filter(|(name, ..)| matches!(*name, "linear" | "quadratic" | "sine" | "near the low end"))
    {
        let vector = |t: f64| vec![shape(t)];
        let (_, evaluations) = locate(&vector, LOW, LOW + WIDTH, TOLERANCE);
        let (_, bisections) = bisect(&*shape, LOW, LOW + WIDTH, TOLERANCE);
        assert!(bisections >= 30, "{name}: {bisections}");
        assert!(
            evaluations <= 12,
            "{name}: {evaluations} evaluations, bisection {bisections}"
        );
    }
}

/// An indicator that reaches zero exactly at the high end is applied at that
/// end, as bisection applies it.
#[test]
fn a_domain_change_at_the_high_end_is_applied_at_the_high_end() {
    let shape = |t: f64| vec![(LOW + WIDTH - t) / WIDTH];
    let (bracket, evaluations) = locate(&shape, LOW, LOW + WIDTH, TOLERANCE);
    assert_eq!(bracket.high().to_bits(), (LOW + WIDTH).to_bits());
    assert_eq!(evaluations, 1);
}

/// Several indicators crossing in one bracket are located at the earliest
/// crossing, which is also where that indicator alone is located.
#[test]
fn simultaneous_changes_are_located_at_the_earliest_crossing() {
    let offsets = [0.6, 0.25, 0.4, 0.9];
    let all = |t: f64| {
        offsets
            .iter()
            .map(|offset| (t - LOW) / WIDTH - offset)
            .collect::<Vec<_>>()
    };
    let (joint, _) = locate(&all, LOW, LOW + WIDTH, TOLERANCE);
    let earliest = LOW + WIDTH * 0.25;
    assert!(joint.low() <= earliest && earliest <= joint.high() + TOLERANCE);
    let alone = |t: f64| vec![(t - LOW) / WIDTH - 0.25];
    let (single, _) = locate(&alone, LOW, LOW + WIDTH, TOLERANCE);
    assert!((joint.high() - single.high()).abs() <= TOLERANCE);
}

/// The evaluation bound is the bisection count plus the slack, and a bracket
/// between adjacent coordinates has no trial.
#[test]
fn the_bound_and_the_representable_limit() {
    let bracket = RootLocationPlan::STANDARD.open_bracket(0.0, 1.0, 0.25);
    assert_eq!(bracket.evaluation_bound(), 2 + 4);
    let narrow = RootLocationPlan::STANDARD.open_bracket(1.0, 1.0 + f64::EPSILON, 1.0e-300);
    assert_eq!(narrow.trial(0.5), None);
    assert!(!narrow.is_located());
}

/// Values whose weighted magnitudes do not sum to a positive finite number
/// give the midpoint fraction.
#[test]
fn a_degenerate_secant_gives_the_midpoint_fraction() {
    let bracket = RootLocationPlan::STANDARD.open_bracket(0.0, 1.0, 1.0e-3);
    assert_eq!(bracket.crossing_fraction(f64::MAX, -f64::MAX), 0.5);
    assert_eq!(bracket.crossing_fraction(-1.0, 3.0), 0.25);
}

/// An end retained twice in a row has its weight halved, so the next secant
/// moves toward the other end.
#[test]
fn an_end_retained_twice_has_its_weight_halved() {
    let mut bracket = RootLocationPlan::STANDARD.open_bracket(0.0, 1.0, 1.0e-6);
    bracket.narrow(0.9, true);
    assert_eq!(bracket.crossing_fraction(-1.0, 1.0), 0.5);
    bracket.narrow(0.8, true);
    assert_eq!(bracket.crossing_fraction(-1.0, 1.0), 1.0 / 3.0);
    bracket.narrow(0.1, false);
    assert_eq!(bracket.crossing_fraction(-1.0, 1.0), 0.5);
    bracket.narrow(0.2, false);
    assert_eq!(bracket.crossing_fraction(-1.0, 1.0), 2.0 / 3.0);
}

/// The window of a located bracket ends one tolerance after its low end, at
/// or after its high end, and never past the scan coordinate bounding it.
#[test]
fn the_application_window_ends_one_tolerance_after_the_low_end() {
    for (name, shape, _) in shapes() {
        let vector = |t: f64| vec![shape(t)];
        let (bracket, _) = locate(&vector, LOW, LOW + WIDTH, TOLERANCE);
        let window = bracket.application_window(LOW + WIDTH);
        assert!(window >= bracket.high(), "{name}");
        assert_eq!(
            window.to_bits(),
            (bracket.low() + TOLERANCE).min(LOW + WIDTH).to_bits(),
            "{name}"
        );
    }
    let bracket = RootLocationPlan::STANDARD.open_bracket(0.0, 0.5, 1.0);
    assert_eq!(bracket.application_window(0.75), 0.75);
}
