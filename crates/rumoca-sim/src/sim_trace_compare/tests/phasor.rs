//! The phasor comparison rule: angles compare on the circle, and a sample is
//! dropped only where the phasor is zero in both traces and its components
//! are compared and high.

use super::*;
use rumoca_ir_solve::{SolvePhasor, SolvePhasorFunction};
use std::f64::consts::PI;

const SAMPLES: usize = 101;

fn times() -> Vec<f64> {
    (0..SAMPLES)
        .map(|i| i as f64 / (SAMPLES - 1) as f64)
        .collect()
}

fn column(f: impl Fn(f64) -> f64) -> Vec<f64> {
    times().into_iter().map(f).collect()
}

/// A trace of the channels `y`, `y.re`, and `y.im`, carrying the phasor
/// record of `y` when `function` is given.
fn phasor_trace(
    model: &str,
    channels: [Vec<f64>; 3],
    function: Option<SolvePhasorFunction>,
) -> SimTrace {
    let mut trace = trace(
        model,
        times(),
        vec!["y", "y.re", "y.im"],
        channels.into_iter().collect(),
    );
    trace.variable_meta = function.map(|function| {
        ["y", "y.re", "y.im"]
            .into_iter()
            .map(|name| SimTraceVariableMeta {
                name: name.to_string(),
                role: Some("algebraic".to_string()),
                value_type: Some("Real".to_string()),
                variability: Some("continuous".to_string()),
                time_domain: Some("continuous-time".to_string()),
                state_coordinate: None,
                phasor: (name == "y").then(|| SolvePhasor {
                    function,
                    re: "y.re".to_string(),
                    im: "y.im".to_string(),
                }),
            })
            .collect()
    });
    trace
}

fn compare(rumoca: &SimTrace, omc: &SimTrace) -> ModelDeviationMetric {
    compare_model_traces("PhasorFixture", rumoca, omc).expect("fixture traces compare")
}

fn error_of(metric: &ModelDeviationMetric, name: &str) -> f64 {
    metric
        .worst_variables
        .iter()
        .find(|channel| channel.name == name)
        .unwrap_or_else(|| panic!("fixture must report channel {name}"))
        .bounded_normalized_l1_error
}

fn is_high(error: f64) -> bool {
    error < HIGH_AGREEMENT_CHANNEL_THRESHOLD
}

/// A unit phasor on the negative real axis lies on the `atan2` branch cut:
/// `pi` in one trace and `-pi` in the other are the same angle.
#[test]
fn angles_across_the_branch_cut_agree() {
    let re = column(|_| -1.0);
    let im = column(|_| 0.0);
    let rumoca =
        |function| phasor_trace("rumoca", [column(|_| PI), re.clone(), im.clone()], function);
    let omc = phasor_trace("omc", [column(|_| -PI), re.clone(), im.clone()], None);

    let pointwise = compare(&rumoca(None), &omc);
    assert!(!is_high(error_of(&pointwise, "y")));

    let metric = compare(&rumoca(Some(SolvePhasorFunction::Angle)), &omc);
    assert_eq!(error_of(&metric, "y"), 0.0);
    assert!(metric.undefined_phasor_channels.is_empty());
}

/// A real phase error at a nonzero magnitude keeps its full arc, also when it
/// straddles the branch cut, where the shortest arc is not the raw difference.
#[test]
fn a_phase_error_at_nonzero_magnitude_is_flagged() {
    let omc_angle = |t: f64| 2.6 + 0.5 * t;
    let omc = phasor_trace(
        "omc",
        [
            column(omc_angle),
            column(|t| omc_angle(t).cos()),
            column(|t| omc_angle(t).sin()),
        ],
        None,
    );
    let rumoca_angle = |t: f64| omc_angle(t) + 0.4;
    let wrapped = |t: f64| {
        let angle = rumoca_angle(t);
        if angle > PI { angle - 2.0 * PI } else { angle }
    };
    let rumoca = phasor_trace(
        "rumoca",
        [
            column(wrapped),
            column(|t| rumoca_angle(t).cos()),
            column(|t| rumoca_angle(t).sin()),
        ],
        Some(SolvePhasorFunction::Angle),
    );

    let metric = compare(&rumoca, &omc);
    let channel = metric
        .worst_variables
        .iter()
        .find(|channel| channel.name == "y")
        .expect("angle channel is compared");
    assert!(!is_high(channel.bounded_normalized_l1_error));
    assert!(
        (channel.mean_abs_error - 0.4).abs() < 1e-9,
        "the error is the 0.4 rad arc, not the raw difference across the cut: {}",
        channel.mean_abs_error
    );
    assert_eq!(
        channel.phasor.as_ref().map(|p| p.undefined_samples),
        Some(0)
    );
}

/// While the phasor is exactly zero `atan2` returns whatever its signed zeros
/// select; those samples are dropped when the components agree.
#[test]
fn a_zero_phasor_with_agreeing_components_is_dropped() {
    let on = |t: f64| if t < 0.5 { 0.0 } else { 1.0 };
    let re = column(on);
    let im = column(|t| 0.5 * on(t));
    let angle =
        |zero_angle: f64| column(move |t| if t < 0.5 { zero_angle } else { 0.5_f64.atan() });
    let omc = phasor_trace("omc", [angle(0.0), re.clone(), im.clone()], None);
    let rumoca = |function| phasor_trace("rumoca", [angle(-PI), re.clone(), im.clone()], function);

    assert!(!is_high(error_of(&compare(&rumoca(None), &omc), "y")));

    let metric = compare(&rumoca(Some(SolvePhasorFunction::Angle)), &omc);
    assert_eq!(error_of(&metric, "y"), 0.0);
    let channel = metric
        .worst_variables
        .iter()
        .find(|channel| channel.name == "y")
        .unwrap();
    assert_eq!(
        channel.phasor.as_ref().map(|p| p.undefined_samples),
        Some(SAMPLES / 2)
    );
    assert_eq!(channel.initial_abs_error, None);
}

/// A phasor zero in both traces throughout leaves its angle without a sample:
/// the channel is reported as undefined, not compared.
#[test]
fn an_angle_of_a_phasor_zero_throughout_is_undefined() {
    let residue = |scale: f64| column(move |_| 4.0e-15 * scale);
    let omc = phasor_trace(
        "omc",
        [column(|_| -2.761), residue(1.0), residue(-1.0)],
        None,
    );
    let rumoca = phasor_trace(
        "rumoca",
        [column(|_| -2.678), residue(0.8), residue(-0.9)],
        Some(SolvePhasorFunction::Angle),
    );

    let metric = compare(&rumoca, &omc);
    assert_eq!(metric.undefined_phasor_channels, ["y"]);
    assert_eq!(metric.compared_variables, 2);
    assert!(
        metric
            .worst_variables
            .iter()
            .all(|channel| channel.name != "y")
    );
}

/// The zero test never decides on the angle alone: when a component channel
/// disagrees, a zero-phasor sample keeps its angle evidence.
#[test]
fn a_zero_phasor_with_disagreeing_components_is_flagged() {
    let on = |t: f64| if t < 0.5 { 0.0 } else { t - 0.5 };
    let omc = phasor_trace("omc", [column(|_| 0.0), column(on), column(|_| 0.0)], None);
    let rumoca = phasor_trace(
        "rumoca",
        [
            column(|t| if t < 0.5 { -PI } else { 0.0 }),
            column(|t| 3.0 * on(t)),
            column(|_| 0.0),
        ],
        Some(SolvePhasorFunction::Angle),
    );

    let metric = compare(&rumoca, &omc);
    assert!(!is_high(error_of(&metric, "y.re")));
    assert!(!is_high(error_of(&metric, "y")));
    let channel = metric
        .worst_variables
        .iter()
        .find(|channel| channel.name == "y")
        .unwrap();
    assert_eq!(
        channel.phasor.as_ref().map(|p| p.undefined_samples),
        Some(0)
    );
}

/// A power factor `cos(arg(Complex(P, Q)))` is undefined at zero power but
/// is not an angle: it takes the drop rule only, never a wrap.
#[test]
fn a_power_factor_takes_only_the_zero_rule() {
    let on = |t: f64| if t < 0.5 { 0.0 } else { t - 0.5 };
    let p = column(on);
    let q = column(|_| 0.0);
    let omc = phasor_trace("omc", [column(|_| 1.0), p.clone(), q.clone()], None);
    let rumoca = phasor_trace(
        "rumoca",
        [
            column(|t| if t < 0.5 { -1.0 } else { 1.0 }),
            p.clone(),
            q.clone(),
        ],
        Some(SolvePhasorFunction::CosineOfAngle),
    );
    assert_eq!(error_of(&compare(&rumoca, &omc), "y"), 0.0);

    let shifted = phasor_trace(
        "rumoca",
        [
            column(|t| if t < 0.5 { -1.0 } else { 1.0 - 2.0 * PI }),
            p,
            q,
        ],
        Some(SolvePhasorFunction::CosineOfAngle),
    );
    assert!(!is_high(error_of(&compare(&shifted, &omc), "y")));
}
