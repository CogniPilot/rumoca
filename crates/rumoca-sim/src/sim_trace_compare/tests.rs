//! Unit tests for trace comparison and reference-scale normalization.

use super::normalization::{
    CONTINUOUS_ABSOLUTE_SCALE_FLOOR, DISCRETE_SCALE_FLOOR, MAGNITUDE_SCALE_FRACTION,
    reference_scale,
};
use super::*;
use proptest::prelude::*;
use std::path::PathBuf;

fn ramp_series(len: usize, f: impl Fn(f64) -> f64) -> (Vec<f64>, Vec<Option<f64>>) {
    let times = (0..len)
        .map(|i| i as f64 / (len - 1) as f64)
        .collect::<Vec<_>>();
    let values = times.iter().map(|&t| Some(f(t))).collect::<Vec<_>>();
    (times, values)
}

fn trace(model_name: &str, times: Vec<f64>, names: Vec<&str>, data: Vec<Vec<f64>>) -> SimTrace {
    SimTrace {
        model_name: Some(model_name.to_string()),
        times,
        names: names.into_iter().map(ToOwned::to_owned).collect(),
        data: data
            .into_iter()
            .map(|col| col.into_iter().map(Some).collect())
            .collect(),
        variable_meta: None,
        certification_profile: None,
    }
}

#[test]
fn stochastic_profile_is_typed_sorted_and_uncertified() {
    let profile = TraceCertificationProfile::stochastic(vec![
        TraceRandomOpKind::RandomResult,
        TraceRandomOpKind::RandomInitialState,
        TraceRandomOpKind::RandomResult,
    ]);
    assert_eq!(profile.reason(), TraceNonidentifiabilityReason::Stochastic);
    assert_eq!(
        profile.evidence,
        TraceNonidentifiabilityEvidence::Stochastic {
            random_op_kinds: vec![
                TraceRandomOpKind::RandomInitialState,
                TraceRandomOpKind::RandomResult,
            ],
        }
    );
    profile
        .validate()
        .expect("compiler-derived profile validates");
    assert!(
        profile
            .outstanding_proof_obligations
            .contains(&TraceProofObligation::StatisticalRefinement),
        "non-identifiability records missing proof work; it is not certification"
    );
}

#[test]
fn malformed_chaos_profile_fails_closed() {
    let profile = TraceCertificationProfile {
        evidence: TraceNonidentifiabilityEvidence::DeterministicChaotic {
            maximum_lyapunov_exponent_lower_bound: 0.0,
            analysis_sha256: "0".repeat(64),
            analysis_samples: 100,
        },
        outstanding_proof_obligations: vec![
            TraceProofObligation::InvariantRefinement,
            TraceProofObligation::StatisticalRefinement,
        ],
    };
    assert!(profile.validate().is_err());
}

#[test]
fn channel_normalized_l1_matches_expected_value() {
    let metric = compare_channel(
        "x",
        ChannelSeries::new(&[0.0, 0.5, 1.0], &[Some(0.0), Some(1.0), Some(2.0)]),
        ChannelSeries::new(&[0.0, 0.5, 1.0], &[Some(0.0), Some(1.1), Some(2.1)]),
        false,
        None,
    )
    .expect("channel should compare");

    // integral_abs_error = 0.075, duration = 1.0
    // reference range uses P95-P05 over sampled reference values.
    let scale = reference_scale(&[0.0, 1.1, 1.1, 2.1], false, None);
    assert_eq!(scale.normalization_scale, scale.range);
    let expected = 0.075 / scale.range;
    assert!((metric.normalized_l1_error - expected).abs() < 1.0e-12);
    assert_eq!(metric.reference_range, scale.range);
    assert_eq!(metric.reference_magnitude, scale.magnitude);
}

#[test]
fn channel_normalized_l1_is_finite_when_reference_is_near_zero() {
    let metric = compare_channel(
        "u",
        ChannelSeries::new(&[0.0, 1.0], &[Some(1.0), Some(1.0)]),
        ChannelSeries::new(&[0.0, 1.0], &[Some(0.0), Some(0.0)]),
        false,
        None,
    )
    .expect("channel should compare");
    // A unit-size residual against an all-zero reference is still a full
    // disagreement, but it is now normalized against the absolute floor rather
    // than machine epsilon, so the reported error stays interpretable.
    assert!(metric.normalized_l1_error.is_finite());
    assert_eq!(metric.normalization_scale, CONTINUOUS_ABSOLUTE_SCALE_FLOOR);
    assert!((metric.normalized_l1_error - 1.0e6).abs() < 1.0);
    assert!(metric.bounded_normalized_l1_error > 0.99);
}

#[test]
fn channel_mean_abs_error_uses_time_weighted_integration() {
    let metric = compare_channel(
        "x",
        ChannelSeries::new(&[0.0, 0.001, 1.0], &[Some(200.0), Some(100.0), Some(100.0)]),
        ChannelSeries::new(&[0.0, 0.001, 1.0], &[Some(100.0), Some(100.0), Some(100.0)]),
        false,
        None,
    )
    .expect("channel should compare");

    // Error is concentrated in [0, 0.001], so the time-weighted mean absolute error is:
    // integral_abs_error = 0.5*(100 + 0)*0.001 = 0.05
    let expected = 0.05;
    assert!((metric.mean_abs_error - expected).abs() < 1.0e-12);
}

#[test]
fn channel_shape_labels_constant_offset() {
    let metric = compare_channel(
        "x",
        ChannelSeries::new(&[0.0, 0.5, 1.0], &[Some(1.0), Some(2.0), Some(3.0)]),
        ChannelSeries::new(&[0.0, 0.5, 1.0], &[Some(0.0), Some(1.0), Some(2.0)]),
        false,
        None,
    )
    .expect("channel should compare");

    assert_eq!(metric.shape, TraceDeviationShape::ConstantOffset);
}

#[test]
fn channel_shape_labels_scale_error() {
    let metric = compare_channel(
        "x",
        ChannelSeries::new(&[0.0, 0.5, 1.0], &[Some(0.0), Some(2.0), Some(4.0)]),
        ChannelSeries::new(&[0.0, 0.5, 1.0], &[Some(0.0), Some(1.0), Some(2.0)]),
        false,
        None,
    )
    .expect("channel should compare");

    assert_eq!(metric.shape, TraceDeviationShape::ScaleError);
}

#[test]
fn channel_shape_labels_discrete_event_time_mismatch() {
    let metric = compare_channel(
        "q",
        ChannelSeries::new(&[0.0, 0.6, 1.0], &[Some(0.0), Some(1.0), Some(1.0)]),
        ChannelSeries::new(&[0.0, 0.4, 1.0], &[Some(0.0), Some(1.0), Some(1.0)]),
        true,
        None,
    )
    .expect("channel should compare");

    assert_eq!(metric.shape, TraceDeviationShape::EventTimeMismatch);
}

/// Clocked sampler fixture: a 20 ms clock over `[0, 0.2]` whose sampled step
/// rises one tick earlier in the candidate than in the reference. Only the
/// switch instant moves, so this is the shape `EventTimeMismatch` names.
fn one_tick_shifted_step(lead_ticks: usize) -> (Vec<f64>, Vec<Option<f64>>) {
    let times = (0..=10).map(|k| k as f64 * 0.02).collect::<Vec<_>>();
    let rise = 5 - lead_ticks;
    let values = (0..=10)
        .map(|k| Some(if k >= rise { 1.0 } else { 0.0 }))
        .collect::<Vec<_>>();
    (times, values)
}

#[test]
fn one_tick_clock_lead_is_labelled_event_time_mismatch() {
    let (rumoca_times, rumoca_values) = one_tick_shifted_step(1);
    let (omc_times, omc_values) = one_tick_shifted_step(0);

    let metric = compare_channel(
        "sample1.y",
        ChannelSeries::new(&rumoca_times, &rumoca_values),
        ChannelSeries::new(&omc_times, &omc_values),
        true,
        None,
    )
    .expect("channel should compare");

    assert!(
        metric.bounded_normalized_l1_error > HIGH_AGREEMENT_CHANNEL_THRESHOLD,
        "the fixture must clear the high-agreement threshold to reach shape classification"
    );
    assert_eq!(metric.shape, TraceDeviationShape::EventTimeMismatch);
}

/// The hole this ordering closes: a clocked channel that settles on a level the
/// reference never reaches — the signature of a clocked partition solved as an
/// algebraic loop instead of a per-tick recurrence — is a real value
/// disagreement, not a sampling convention, and must not be labelled
/// `EventTimeMismatch`.
#[test]
fn discrete_value_disagreement_is_not_labelled_event_time_mismatch() {
    let times = (0..=10).map(|k| k as f64 * 0.02).collect::<Vec<_>>();
    // Reference: the clocked recurrence 0, 0, 1.2, -0.24, 1.488, ...
    let reference = [
        0.0,
        0.0,
        1.2,
        -0.24,
        1.488,
        -0.5856,
        1.90272,
        -1.083264,
        2.4999168,
        -1.79990016,
        3.359880192,
    ];
    let omc_values = reference.iter().copied().map(Some).collect::<Vec<_>>();
    // Candidate: the algebraic-loop steady state held for the whole run.
    let rumoca_values = times
        .iter()
        .map(|&t| {
            Some(if t < 0.04 {
                0.0
            } else {
                0.545_454_545_454_545_5
            })
        })
        .collect::<Vec<_>>();

    let metric = compare_channel(
        "feedback.u2",
        ChannelSeries::new(&times, &rumoca_values),
        ChannelSeries::new(&times, &omc_values),
        true,
        None,
    )
    .expect("channel should compare");

    assert!(
        metric.bounded_normalized_l1_error > HIGH_AGREEMENT_CHANNEL_THRESHOLD,
        "the fixture must clear the high-agreement threshold to reach shape classification"
    );
    assert_ne!(
        metric.shape,
        TraceDeviationShape::EventTimeMismatch,
        "a discrete channel holding a level the reference never reaches is a value disagreement"
    );
}

/// A step-hold channel whose levels are simply inverted visits the same level
/// set as the reference, so only the *confinement* condition separates it from
/// a timing shift.
#[test]
fn discrete_inversion_over_the_whole_horizon_is_not_labelled_event_time_mismatch() {
    let times = (0..=10).map(|k| k as f64 * 0.02).collect::<Vec<_>>();
    let rumoca_values = (0..=10)
        .map(|k| Some(if k % 2 == 0 { 1.0 } else { 0.0 }))
        .collect::<Vec<_>>();
    let omc_values = (0..=10)
        .map(|k| Some(if k % 2 == 0 { 0.0 } else { 1.0 }))
        .collect::<Vec<_>>();

    let metric = compare_channel(
        "q",
        ChannelSeries::new(&times, &rumoca_values),
        ChannelSeries::new(&times, &omc_values),
        true,
        None,
    )
    .expect("channel should compare");

    assert_ne!(metric.shape, TraceDeviationShape::EventTimeMismatch);
}

#[test]
fn numerically_coincident_discrete_events_compare_on_the_right_limit() {
    let rumoca_times = [0.0, 0.5, 1.0];
    let rumoca_values = [Some(0.0), Some(1.0), Some(1.0)];
    let omc_times = [0.0, 0.5 + 1.6e-13, 1.0];
    let omc_values = [Some(0.0), Some(1.0), Some(1.0)];

    let metric = compare_channel(
        "q",
        ChannelSeries::new(&rumoca_times, &rumoca_values),
        ChannelSeries::new(&omc_times, &omc_values),
        true,
        None,
    )
    .expect("channel should compare");

    assert!(
        metric.bounded_normalized_l1_error < 1.0e-12,
        "a sub-grid-epsilon event shift must compare at its settled right limit, got {}",
        metric.bounded_normalized_l1_error
    );
}

#[test]
fn discrete_event_shift_integrates_only_the_different_hold_interval() {
    let rumoca_times = [0.0, 0.5, 1.0];
    let rumoca_values = [Some(0.0), Some(1.0), Some(1.0)];
    let omc_times = [0.0, 0.51, 1.0];
    let omc_values = [Some(0.0), Some(1.0), Some(1.0)];

    let metric = compare_channel(
        "q",
        ChannelSeries::new(&rumoca_times, &rumoca_values),
        ChannelSeries::new(&omc_times, &omc_values),
        true,
        None,
    )
    .expect("channel should compare");

    assert!(
        (metric.integral_abs_error - 0.01).abs() < 1.0e-12,
        "step-hold L1 must equal the event-time shift, got {}",
        metric.integral_abs_error
    );
}

/// A two-level step-hold trace on a uniform tick grid; `level(k)` is the level
/// the trace holds from tick `k` onwards.
fn step_hold_series(
    ticks: usize,
    tick: f64,
    level: impl Fn(usize) -> bool,
) -> (Vec<f64>, Vec<Option<f64>>) {
    let times = (0..=ticks).map(|k| k as f64 * tick).collect::<Vec<_>>();
    let values = (0..=ticks)
        .map(|k| Some(if level(k) { 1.0 } else { 0.0 }))
        .collect::<Vec<_>>();
    (times, values)
}

fn discrete_channel_metric(
    rumoca: &(Vec<f64>, Vec<Option<f64>>),
    omc: &(Vec<f64>, Vec<Option<f64>>),
) -> ChannelDeviationMetric {
    let metric = compare_channel(
        "q",
        ChannelSeries::new(&rumoca.0, &rumoca.1),
        ChannelSeries::new(&omc.0, &omc.1),
        true,
        None,
    )
    .expect("channel should compare");
    assert!(
        metric.bounded_normalized_l1_error > HIGH_AGREEMENT_CHANNEL_THRESHOLD,
        "the fixture must clear the high-agreement threshold to reach shape classification"
    );
    metric
}

/// A genuine event-time shift, and a *large* one: a five-tick square wave whose
/// candidate switches two ticks early spends 40% of the horizon disagreeing.
/// Every mismatched sample still sits within two ticks of a transition and no
/// level hold loses its majority, so the shape stays `EventTimeMismatch`. The
/// share is not what decides this.
#[test]
fn event_shift_confined_to_transitions_stays_event_time_mismatch() {
    let square = |lead: usize| step_hold_series(103, 0.01, move |k| ((k + lead) / 5) % 2 == 1);
    let metric = discrete_channel_metric(&square(2), &square(0));

    assert!(
        (metric.mean_abs_error - 0.4).abs() < 0.02,
        "fixture must disagree over ~40% of the horizon, got {}",
        metric.mean_abs_error
    );
    assert_eq!(metric.shape, TraceDeviationShape::EventTimeMismatch);
}

/// The hole the per-hold rule closes. The candidate tracks the reference for the
/// first half of the run and then holds the opposite level across four whole
/// ten-tick plateaus. That is 45% of the horizon — under the horizon-level cap
/// the old rule applied, and it visits only levels the reference also reaches,
/// so the old rule called it `EventTimeMismatch`. No event instant can move a
/// level the reference holds for a whole plateau, so it is a value
/// disagreement.
#[test]
fn sustained_level_disagreement_across_whole_holds_is_not_event_time_mismatch() {
    let reference = step_hold_series(100, 0.01, |k| (k / 10) % 2 == 1);
    let candidate = step_hold_series(100, 0.01, |k| ((k / 10) % 2 == 1) != (k >= 55));
    let metric = discrete_channel_metric(&candidate, &reference);

    assert!(
        metric.mean_abs_error < EVENT_MISMATCH_MAX_HOLD_DISAGREEMENT_SHARE,
        "fixture must stay under the share the old horizon-level cap allowed, got {}",
        metric.mean_abs_error
    );
    assert_eq!(metric.shape, TraceDeviationShape::DiscreteLevelDisagreement);
}

/// Boundary: a displacement that eats exactly half of the hold it moves into is
/// still a displacement. Switching at 0.75 against a reference that switches at
/// 0.5 consumes 0.25 of the reference's 0.5-long second hold.
#[test]
fn a_hold_disagreeing_for_exactly_half_its_span_is_still_event_time_mismatch() {
    let reference = step_hold_series(8, 0.125, |k| k >= 4);
    let candidate = step_hold_series(8, 0.125, |k| k >= 6);
    let metric = discrete_channel_metric(&candidate, &reference);

    assert_eq!(metric.shape, TraceDeviationShape::EventTimeMismatch);
}

/// One tick past that boundary the same fixture is a level disagreement, and it
/// gets there while still disagreeing over only 37.5% of the horizon — the
/// horizon-level cap cannot see the difference between this and the case above.
#[test]
fn a_hold_disagreeing_for_more_than_half_its_span_is_a_level_disagreement() {
    let reference = step_hold_series(8, 0.125, |k| k >= 4);
    let candidate = step_hold_series(8, 0.125, |k| k >= 7);
    let metric = discrete_channel_metric(&candidate, &reference);

    assert!(
        metric.mean_abs_error < EVENT_MISMATCH_MAX_HOLD_DISAGREEMENT_SHARE,
        "fixture must stay under the share the old horizon-level cap allowed, got {}",
        metric.mean_abs_error
    );
    assert_eq!(metric.shape, TraceDeviationShape::DiscreteLevelDisagreement);
}

/// The corpus case, at the shape the landed traces have.
/// `Modelica.Electrical.Analog.Examples.CharacteristicIdealDiodes` switches
/// `Ideal.off` at mid-horizon in OpenModelica and holds it; our trace holds the
/// opposite level until the final sample. That is 0.498 of the horizon — under
/// the old 0.50 cap by 0.002, which is why a diode that conducts for the entire
/// second half of the run reported as an event-timing artifact. Measured against
/// the hold it lands in it is 0.996.
///
/// The label also has to be the level disagreement it is: the numeric ladder
/// below the timing gate would have claimed it as a `ScaleError`, fitting a
/// least-squares gain of 0.002 to a Boolean.
#[test]
fn a_discrete_level_held_past_the_reference_switch_is_a_level_disagreement() {
    let reference = step_hold_series(500, 0.002, |k| k >= 250);
    let candidate = step_hold_series(500, 0.002, |k| k >= 499);
    let metric = discrete_channel_metric(&candidate, &reference);

    assert!(
        (metric.mean_abs_error - 0.498).abs() < 1.0e-9,
        "fixture must reproduce the landed 0.498 disagreement share, got {}",
        metric.mean_abs_error
    );
    assert!(
        metric.mean_abs_error < EVENT_MISMATCH_MAX_HOLD_DISAGREEMENT_SHARE,
        "the landed channel cleared the old horizon-level cap; the fixture must too"
    );
    assert_eq!(metric.shape, TraceDeviationShape::DiscreteLevelDisagreement);
}

#[test]
fn model_score_uses_median_bounded_l1() {
    let rumoca = trace(
        "M",
        vec![0.0, 0.5, 1.0],
        vec!["x", "y", "z"],
        vec![
            vec![0.0, 1.0, 2.0],
            vec![0.05, 1.05, 2.05],
            vec![0.5, 1.5, 2.5],
        ],
    );
    let omc = trace(
        "M",
        vec![0.0, 0.5, 1.0],
        vec!["x", "y", "z"],
        vec![
            vec![0.0, 1.0, 2.0],
            vec![0.0, 1.0, 2.0],
            vec![0.0, 1.0, 2.0],
        ],
    );

    let metric = compare_model_traces("M", &rumoca, &omc).expect("model compare");
    assert_eq!(metric.compared_variables, 3);
    let mut channel_scores = metric
        .worst_variables
        .iter()
        .map(|channel| channel.bounded_normalized_l1_error)
        .collect::<Vec<_>>();
    channel_scores.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    let expected_median = median_of_sorted(&channel_scores).expect("median");
    assert!((metric.bounded_normalized_l1_score - expected_median).abs() < 1.0e-15);
    assert!(metric.bounded_normalized_l1_score > 0.0);
    assert!(!metric.worst_variables.is_empty());
    assert_eq!(
        metric.channel_high_count + metric.channel_minor_count + metric.channel_deviation_count,
        metric.compared_variables
    );
    assert!(metric.channel_violation_mass >= 0.0);
}

#[test]
fn compare_model_requires_common_variables() {
    let rumoca = trace("M", vec![0.0, 1.0], vec!["x"], vec![vec![0.0, 1.0]]);
    let omc = trace("M", vec![0.0, 1.0], vec!["z"], vec![vec![0.0, 1.0]]);
    let err = compare_model_traces("M", &rumoca, &omc).expect_err("no common vars");
    assert!(matches!(err, TraceCompareError::NoCommonVariables));
}

#[test]
fn trajectory_and_initial_metric_use_settled_exact_start_time_value() {
    let rumoca = trace("M", vec![0.0, 0.1], vec!["x"], vec![vec![1.0, 1.0]]);
    let mut omc = trace(
        "M",
        vec![0.0, 0.0, 0.1],
        vec!["x"],
        vec![vec![0.0, 1.0, 1.0]],
    );
    normalize_trace(&mut omc);

    let metric = compare_model_traces("M", &rumoca, &omc).expect("model compare");
    assert!(
        metric.bounded_normalized_l1_score < 1.0e-12,
        "trajectory comparison should use the settled event value"
    );
    assert_eq!(metric.initial_condition.deviation_count, 0);
    assert_eq!(metric.initial_condition.high_count, 1);
}

#[test]
fn common_left_limit_is_not_replaced_by_different_start_event_grids() {
    let rumoca = trace(
        "M",
        vec![0.0, 2.0e-11, 0.1],
        vec!["off"],
        vec![vec![0.0, 1.0, 1.0]],
    );
    let omc = trace(
        "M",
        vec![0.0, 6.0e-21, 6.0e-21, 0.1],
        vec!["off"],
        vec![vec![0.0, 0.0, 1.0, 1.0]],
    );

    let metric = compare_model_traces("M", &rumoca, &omc).expect("model compare");

    assert_eq!(metric.initial_condition.channels_compared, 1);
    assert_eq!(metric.initial_condition.high_count, 1);
    assert_eq!(metric.initial_condition.deviation_count, 0);
    assert_eq!(metric.worst_variables[0].initial_abs_error, Some(0.0));
}

#[test]
fn exact_start_time_event_rows_settle_before_initial_comparison() {
    let rumoca = trace("M", vec![0.0, 0.1], vec!["step.y"], vec![vec![1.0, 1.0]]);
    let omc = trace(
        "M",
        vec![0.0, 0.0, 0.0, 0.1],
        vec!["step.y"],
        vec![vec![0.0, 0.0, 1.0, 1.0]],
    );

    let metric = compare_model_traces("M", &rumoca, &omc).expect("model compare");

    assert_eq!(metric.initial_condition.channels_compared, 1);
    assert_eq!(metric.initial_condition.high_count, 1);
    assert_eq!(metric.initial_condition.deviation_count, 0);
    assert_eq!(metric.worst_variables[0].initial_abs_error, Some(0.0));
}

#[test]
fn discrete_channel_uses_step_hold_interpolation() {
    let metric = compare_channel(
        "q",
        ChannelSeries::new(&[0.0, 1.0], &[Some(0.0), Some(1.0)]),
        ChannelSeries::new(&[0.0, 0.5, 1.0], &[Some(0.0), Some(0.0), Some(1.0)]),
        true,
        None,
    )
    .expect("channel compare");
    assert!(
        metric.bounded_normalized_l1_error < 1.0e-12,
        "step-hold should avoid synthetic mid-step interpolation error for discrete channels"
    );
}

#[test]
fn event_discontinuous_real_channel_uses_step_hold_interpolation() {
    let rumoca = SimTrace {
        model_name: Some("M".to_string()),
        times: vec![0.0, 1.0],
        names: vec!["y".to_string()],
        data: vec![vec![Some(0.0), Some(1.0)]],
        variable_meta: Some(vec![SimTraceVariableMeta {
            name: "y".to_string(),
            role: Some("output".to_string()),
            value_type: Some("Real".to_string()),
            variability: Some("continuous".to_string()),
            time_domain: Some("event-discontinuous".to_string()),
        }]),
        certification_profile: None,
    };
    let omc = SimTrace {
        model_name: Some("M".to_string()),
        times: vec![0.0, 0.5, 1.0],
        names: vec!["y".to_string()],
        data: vec![vec![Some(0.0), Some(0.0), Some(1.0)]],
        variable_meta: None,
        certification_profile: None,
    };

    let metric = compare_model_traces("M", &rumoca, &omc)
        .expect("event-discontinuous Real trace should compare");
    assert!(
        metric.bounded_normalized_l1_score < 1.0e-12,
        "event-discontinuous Real channels should avoid synthetic linear ramp error"
    );
}

#[test]
fn discrete_only_model_traces_contribute_to_metrics() {
    let rumoca = SimTrace {
        model_name: Some("M".to_string()),
        times: vec![0.0, 1.0],
        names: vec!["q".to_string()],
        data: vec![vec![Some(0.0), Some(1.0)]],
        variable_meta: Some(vec![SimTraceVariableMeta {
            name: "q".to_string(),
            role: Some("algebraic".to_string()),
            value_type: Some("Boolean".to_string()),
            variability: Some("discrete".to_string()),
            time_domain: Some("event-discrete".to_string()),
        }]),
        certification_profile: None,
    };
    let omc = SimTrace {
        model_name: Some("M".to_string()),
        times: vec![0.0, 0.5, 1.0],
        names: vec!["q".to_string()],
        data: vec![vec![Some(0.0), Some(0.0), Some(1.0)]],
        variable_meta: Some(vec![SimTraceVariableMeta {
            name: "q".to_string(),
            role: Some("algebraic".to_string()),
            value_type: Some("Boolean".to_string()),
            variability: Some("discrete".to_string()),
            time_domain: Some("event-discrete".to_string()),
        }]),
        certification_profile: None,
    };

    let metric = compare_model_traces("M", &rumoca, &omc)
        .expect("discrete-only traces should still produce comparison metrics");
    assert_eq!(metric.compared_variables, 1);
    assert_eq!(metric.samples_compared, 4);
    assert!(metric.bounded_normalized_l1_score < 1.0e-12);
}

#[test]
fn initial_condition_stats_use_first_comparable_sample() {
    let rumoca = trace(
        "M",
        vec![0.0, 0.5, 1.0],
        vec!["x", "y"],
        vec![vec![1.0, 1.0, 1.0], vec![2.0, 2.0, 2.0]],
    );
    let omc = trace(
        "M",
        vec![0.0, 0.5, 1.0],
        vec!["x", "y"],
        vec![vec![0.0, 1.0, 1.0], vec![2.0, 2.0, 2.0]],
    );

    let metric = compare_model_traces("M", &rumoca, &omc).expect("model compare");

    assert_eq!(metric.initial_condition.channels_compared, 2);
    assert_eq!(metric.initial_condition.high_count, 1);
    assert_eq!(metric.initial_condition.deviation_count, 1);
    assert!(metric.initial_condition.violation_mass_total > 0.0);
    assert!(
        metric
            .worst_variables
            .iter()
            .any(|channel| channel.initial_bounded_normalized_error.is_some())
    );
}

#[test]
fn agreement_band_thresholds_classify_score_as_expected() {
    assert_eq!(
        classify_trace_score(0.01, 0.02, 0.05),
        AgreementBand::HighAgreement
    );
    assert_eq!(
        classify_trace_score(0.03, 0.02, 0.05),
        AgreementBand::MinorAgreement
    );
    assert_eq!(
        classify_trace_score(0.2, 0.02, 0.05),
        AgreementBand::Deviation
    );
}

#[test]
fn agreement_band_thresholds_classify_model_rollups_as_expected() {
    let high_metric = ModelDeviationMetric {
        model_name: "high".to_string(),
        compared_variables: 1,
        samples_compared: 2,
        bounded_normalized_l1_score: 0.01,
        mean_channel_bounded_normalized_l1: 0.009,
        max_channel_bounded_normalized_l1: 0.04,
        channel_high_count: 1,
        channel_minor_count: 0,
        channel_deviation_count: 0,
        channel_severe_count: 0,
        channel_high_percent: 1.0,
        channel_minor_percent: 0.0,
        channel_deviation_percent: 0.0,
        channel_severe_percent: 0.0,
        channel_violation_mass: 0.0,
        initial_condition: InitialConditionStats::default(),
        worst_variables: Vec::new(),
    };
    assert_eq!(
        classify_trace_metric(
            &high_metric,
            HIGH_AGREEMENT_MAX_CHANNEL_THRESHOLD,
            HIGH_AGREEMENT_MEAN_CHANNEL_THRESHOLD,
            MINOR_AGREEMENT_MAX_CHANNEL_THRESHOLD,
            MINOR_AGREEMENT_MEAN_CHANNEL_THRESHOLD
        ),
        AgreementBand::HighAgreement
    );

    let near_metric = ModelDeviationMetric {
        model_name: "near".to_string(),
        compared_variables: 1,
        samples_compared: 2,
        bounded_normalized_l1_score: 0.01,
        mean_channel_bounded_normalized_l1: 0.03,
        max_channel_bounded_normalized_l1: 0.12,
        channel_high_count: 0,
        channel_minor_count: 1,
        channel_deviation_count: 0,
        channel_severe_count: 0,
        channel_high_percent: 0.0,
        channel_minor_percent: 1.0,
        channel_deviation_percent: 0.0,
        channel_severe_percent: 0.0,
        channel_violation_mass: 0.0,
        initial_condition: InitialConditionStats::default(),
        worst_variables: Vec::new(),
    };
    assert_eq!(
        classify_trace_metric(
            &near_metric,
            HIGH_AGREEMENT_MAX_CHANNEL_THRESHOLD,
            HIGH_AGREEMENT_MEAN_CHANNEL_THRESHOLD,
            MINOR_AGREEMENT_MAX_CHANNEL_THRESHOLD,
            MINOR_AGREEMENT_MEAN_CHANNEL_THRESHOLD
        ),
        AgreementBand::MinorAgreement
    );

    let deviation_metric = ModelDeviationMetric {
        model_name: "deviation".to_string(),
        compared_variables: 1,
        samples_compared: 2,
        bounded_normalized_l1_score: 0.01,
        mean_channel_bounded_normalized_l1: 0.01,
        max_channel_bounded_normalized_l1: 0.30,
        channel_high_count: 0,
        channel_minor_count: 0,
        channel_deviation_count: 1,
        channel_severe_count: 0,
        channel_high_percent: 0.0,
        channel_minor_percent: 0.0,
        channel_deviation_percent: 1.0,
        channel_severe_percent: 0.0,
        channel_violation_mass: 0.1,
        initial_condition: InitialConditionStats::default(),
        worst_variables: Vec::new(),
    };
    assert_eq!(
        classify_trace_metric(
            &deviation_metric,
            HIGH_AGREEMENT_MAX_CHANNEL_THRESHOLD,
            HIGH_AGREEMENT_MEAN_CHANNEL_THRESHOLD,
            MINOR_AGREEMENT_MAX_CHANNEL_THRESHOLD,
            MINOR_AGREEMENT_MEAN_CHANNEL_THRESHOLD
        ),
        AgreementBand::Deviation
    );
}

#[test]
fn synthetic_metrics_produce_expected_agreement_counts() {
    let high_rumoca = trace(
        "high",
        vec![0.0, 0.5, 1.0],
        vec!["x"],
        vec![vec![1.0, 1.0, 1.0]],
    );
    let high_omc = trace(
        "high",
        vec![0.0, 0.5, 1.0],
        vec!["x"],
        vec![vec![1.0, 1.0, 1.0]],
    );
    let high = compare_model_traces("high", &high_rumoca, &high_omc).expect("high compare");

    let minor_rumoca = trace(
        "minor",
        vec![0.0, 0.5, 1.0],
        vec!["x", "y", "z"],
        vec![
            vec![0.2, 1.2, 2.2],
            vec![1.0, 1.0, 1.0],
            vec![2.0, 2.0, 2.0],
        ],
    );
    let minor_omc = trace(
        "minor",
        vec![0.0, 0.5, 1.0],
        vec!["x", "y", "z"],
        vec![
            vec![0.0, 1.0, 2.0],
            vec![1.0, 1.0, 1.0],
            vec![2.0, 2.0, 2.0],
        ],
    );
    let minor = compare_model_traces("minor", &minor_rumoca, &minor_omc).expect("minor compare");
    assert!(minor.max_channel_bounded_normalized_l1 <= MINOR_AGREEMENT_MAX_CHANNEL_THRESHOLD);
    assert!(minor.max_channel_bounded_normalized_l1 > HIGH_AGREEMENT_MAX_CHANNEL_THRESHOLD);
    assert!(minor.mean_channel_bounded_normalized_l1 <= MINOR_AGREEMENT_MEAN_CHANNEL_THRESHOLD);

    let dev_rumoca = trace(
        "dev",
        vec![0.0, 0.5, 1.0],
        vec!["x"],
        vec![vec![1.0, 2.0, 3.0]],
    );
    let dev_omc = trace(
        "dev",
        vec![0.0, 0.5, 1.0],
        vec!["x"],
        vec![vec![0.0, 1.0, 2.0]],
    );
    let dev = compare_model_traces("dev", &dev_rumoca, &dev_omc).expect("dev compare");
    assert!(dev.max_channel_bounded_normalized_l1 > MINOR_AGREEMENT_MAX_CHANNEL_THRESHOLD);

    let metrics = [high, minor, dev];
    let counts = count_agreement_bands_default(metrics.iter());
    assert_eq!(counts.high_agreement, 1);
    assert_eq!(counts.minor_agreement, 1);
    assert_eq!(counts.deviation, 1);
}

#[test]
fn channel_distribution_thresholds_classify_model_as_expected() {
    let high = channel_distribution_metric("high", 9, 1, 0, 0);
    assert_eq!(
        classify_trace_metric_channel_distribution(
            &high,
            MODEL_HIGH_MIN_HIGH_CHANNEL_SHARE,
            MODEL_HIGH_MAX_DEVIATION_CHANNEL_SHARE,
            MODEL_MINOR_MIN_HIGH_PLUS_MINOR_CHANNEL_SHARE,
            MODEL_MINOR_MAX_DEVIATION_CHANNEL_SHARE
        ),
        AgreementBand::HighAgreement
    );

    let near = channel_distribution_metric("near", 4, 5, 1, 0);
    assert_eq!(
        classify_trace_metric_channel_distribution(
            &near,
            MODEL_HIGH_MIN_HIGH_CHANNEL_SHARE,
            MODEL_HIGH_MAX_DEVIATION_CHANNEL_SHARE,
            MODEL_MINOR_MIN_HIGH_PLUS_MINOR_CHANNEL_SHARE,
            MODEL_MINOR_MAX_DEVIATION_CHANNEL_SHARE
        ),
        AgreementBand::MinorAgreement
    );

    let near_exact_boundary = channel_distribution_metric("near-exact-boundary", 2, 7, 1, 0);
    assert_eq!(
        classify_trace_metric_channel_distribution(
            &near_exact_boundary,
            MODEL_HIGH_MIN_HIGH_CHANNEL_SHARE,
            MODEL_HIGH_MAX_DEVIATION_CHANNEL_SHARE,
            MODEL_MINOR_MIN_HIGH_PLUS_MINOR_CHANNEL_SHARE,
            MODEL_MINOR_MAX_DEVIATION_CHANNEL_SHARE
        ),
        AgreementBand::MinorAgreement
    );

    let deviation = channel_distribution_metric("deviation", 2, 5, 3, 1);
    assert_eq!(
        classify_trace_metric_channel_distribution(
            &deviation,
            MODEL_HIGH_MIN_HIGH_CHANNEL_SHARE,
            MODEL_HIGH_MAX_DEVIATION_CHANNEL_SHARE,
            MODEL_MINOR_MIN_HIGH_PLUS_MINOR_CHANNEL_SHARE,
            MODEL_MINOR_MAX_DEVIATION_CHANNEL_SHARE
        ),
        AgreementBand::Deviation
    );

    let one_hidden_deviation = channel_distribution_metric("one-hidden-deviation", 1000, 0, 1, 0);
    assert_eq!(
        classify_trace_metric_channel_distribution(
            &one_hidden_deviation,
            MODEL_HIGH_MIN_HIGH_CHANNEL_SHARE,
            MODEL_HIGH_MAX_DEVIATION_CHANNEL_SHARE,
            MODEL_MINOR_MIN_HIGH_PLUS_MINOR_CHANNEL_SHARE,
            MODEL_MINOR_MAX_DEVIATION_CHANNEL_SHARE
        ),
        AgreementBand::MinorAgreement,
        "one wrong observable cannot be strict-high, however many aliases surround it"
    );
}

fn channel_distribution_metric(
    name: &str,
    high: usize,
    minor: usize,
    deviation: usize,
    severe: usize,
) -> ModelDeviationMetric {
    let total = high + minor + deviation;
    let total = total.max(1) as f64;
    ModelDeviationMetric {
        model_name: name.to_string(),
        compared_variables: total as usize,
        samples_compared: total as usize,
        bounded_normalized_l1_score: 0.0,
        mean_channel_bounded_normalized_l1: 0.0,
        max_channel_bounded_normalized_l1: 0.0,
        channel_high_count: high,
        channel_minor_count: minor,
        channel_deviation_count: deviation,
        channel_severe_count: severe,
        channel_high_percent: high as f64 / total,
        channel_minor_percent: minor as f64 / total,
        channel_deviation_percent: deviation as f64 / total,
        channel_severe_percent: severe as f64 / total,
        channel_violation_mass: deviation as f64,
        initial_condition: InitialConditionStats::default(),
        worst_variables: Vec::new(),
    }
}

fn fixture_path(rel: &str) -> PathBuf {
    let local = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("sim_traces")
        .join(rel);
    if local.is_file() {
        return local;
    }
    let shared = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("rumoca")
        .join("tests")
        .join("fixtures")
        .join("sim_traces")
        .join(rel);
    if shared.is_file() {
        return shared;
    }
    local
}

#[test]
fn curated_fixture_traces_produce_expected_agreement_counts() {
    let pairs = vec![
        ("high_agreement", "Modelica.Fixture.HighAgreement"),
        ("minor_agreement", "Modelica.Fixture.MinorAgreement"),
        ("deviation", "Modelica.Fixture.Deviation"),
    ];
    let mut metrics = Vec::new();
    for (slug, model_name) in pairs {
        let rumoca = load_trace_json(&fixture_path(&format!("rumoca/{slug}.json")))
            .expect("load rumoca curated fixture trace");
        let omc = load_trace_json(&fixture_path(&format!("omc/{slug}.json")))
            .expect("load omc curated fixture trace");
        let metric = compare_model_traces(model_name, &rumoca, &omc)
            .expect("compare curated fixture traces should succeed");
        metrics.push(metric);
    }

    let counts = count_agreement_bands_default(metrics.iter());
    assert_eq!(counts.high_agreement, 1);
    assert_eq!(counts.minor_agreement, 0);
    assert_eq!(counts.deviation, 2);
}

// ---------------------------------------------------------------------------
// Reference-scale normalization (near-zero channels)
// ---------------------------------------------------------------------------

/// Constant-zero reference channels (a grounded current, a disabled actuator)
/// used to divide solver noise by the old `1e-12` floor and report a *severe*
/// deviation. They must now normalize against the absolute tolerance floor.
#[test]
fn constant_zero_reference_channel_is_not_severe() {
    let len = 101;
    let (times, rumoca_values) = ramp_series(len, |t| 1.0e-9 * (t * 37.0).sin());
    let omc_values = vec![Some(0.0); len];

    let metric = compare_channel(
        "ground.p.i",
        ChannelSeries::new(&times, &rumoca_values),
        ChannelSeries::new(&times, &omc_values),
        false,
        None,
    )
    .expect("channel should compare");

    assert!(metric.normalization_scale >= CONTINUOUS_ABSOLUTE_SCALE_FLOOR);
    assert!(
        metric.bounded_normalized_l1_error < HIGH_AGREEMENT_CHANNEL_THRESHOLD,
        "1e-9 noise against a zero reference must not read as a deviation, got {}",
        metric.bounded_normalized_l1_error
    );
    assert_eq!(metric.shape, TraceDeviationShape::WithinTolerance);
}

/// A small but non-zero constant reference normalizes against its own
/// magnitude, not the absolute floor, so real relative error stays visible.
#[test]
fn small_constant_reference_uses_magnitude_floor() {
    let len = 21;
    let times = (0..len)
        .map(|i| i as f64 / (len - 1) as f64)
        .collect::<Vec<_>>();
    let omc_values = vec![Some(1.0e-3); len];
    let rumoca_values = vec![Some(1.1e-3); len];

    let metric = compare_channel(
        "leakage.i",
        ChannelSeries::new(&times, &rumoca_values),
        ChannelSeries::new(&times, &omc_values),
        false,
        None,
    )
    .expect("channel should compare");

    let expected_scale = MAGNITUDE_SCALE_FRACTION * 1.0e-3;
    assert!((metric.normalization_scale - expected_scale).abs() < 1.0e-15);
    assert!(metric.normalization_scale > CONTINUOUS_ABSOLUTE_SCALE_FLOOR);
    assert!(
        metric.bounded_normalized_l1_error > MINOR_AGREEMENT_CHANNEL_THRESHOLD,
        "a 10% relative error must still be reported, got {}",
        metric.bounded_normalized_l1_error
    );
}

/// Well-scaled channels keep the historical range-based normalization exactly.
#[test]
fn genuine_deviation_on_large_signal_is_unchanged() {
    let len = 11;
    let (times, omc_values) = ramp_series(len, |t| 10.0 * t);
    let rumoca_values = omc_values
        .iter()
        .map(|value| value.map(|v| v + 4.0))
        .collect::<Vec<_>>();

    let metric = compare_channel(
        "x",
        ChannelSeries::new(&times, &rumoca_values),
        ChannelSeries::new(&times, &omc_values),
        false,
        None,
    )
    .expect("channel should compare");

    assert_eq!(metric.normalization_scale, metric.reference_range);
    assert!(metric.reference_range > 1.0);
    let expected_normalized = metric.mean_abs_error / metric.reference_range;
    assert!(
        (metric.normalized_l1_error - expected_normalized).abs() < 1.0e-15,
        "range-normalized channels must be bit-for-bit unchanged"
    );
}

/// The degenerate-reference taxonomy bucket must survive the higher floor: it
/// is now keyed on the raw reference statistics instead of the flooring value.
#[test]
fn missing_channel_mapping_shape_still_reachable() {
    let times = vec![0.0, 0.25, 0.5, 0.75, 1.0];
    let rumoca_values = vec![Some(5.0), Some(-5.0), Some(5.0), Some(-5.0), Some(5.0)];
    let omc_values = vec![Some(0.0); 5];

    let metric = compare_channel(
        "unmapped",
        ChannelSeries::new(&times, &rumoca_values),
        ChannelSeries::new(&times, &omc_values),
        false,
        None,
    )
    .expect("channel should compare");

    assert_eq!(
        metric.shape,
        TraceDeviationShape::MissingOrWrongChannelMapping
    );
}

/// A *flat but non-zero* reference is just as unable to explain a large
/// residual as a flat zero one, so it belongs in the same bucket. Requiring the
/// magnitude to also be near zero silently demoted these channels to `Unknown`
/// and lost the mapping diagnosis that the pre-normalization implementation
/// (`normalization_scale <= 1e-11`) produced.
#[test]
fn flat_non_zero_reference_still_reports_channel_mapping() {
    let times = vec![0.0, 0.25, 0.5, 0.75, 1.0];
    // The reference never moves but sits at a non-zero level: the range is
    // degenerate, the magnitude is comfortably above the absolute floor.
    let omc_values = vec![Some(0.5); 5];
    let rumoca_values = vec![Some(5.0), Some(-5.0), Some(5.0), Some(-5.0), Some(5.0)];

    let metric = compare_channel(
        "unmapped.level",
        ChannelSeries::new(&times, &rumoca_values),
        ChannelSeries::new(&times, &omc_values),
        false,
        None,
    )
    .expect("channel should compare");

    assert!(metric.reference_range < CONTINUOUS_ABSOLUTE_SCALE_FLOOR);
    assert!(
        metric.reference_magnitude > CONTINUOUS_ABSOLUTE_SCALE_FLOOR,
        "fixture must have a non-degenerate magnitude, got {}",
        metric.reference_magnitude
    );
    assert_eq!(
        metric.shape,
        TraceDeviationShape::MissingOrWrongChannelMapping,
        "a flat non-zero reference must stay in the mapping bucket"
    );
}

/// A DC-offset channel — level far above its p95-p05 spread, e.g. an absolute
/// temperature — must keep being normalized by its real dynamic range. Applying
/// the `0.05 * magnitude` term unconditionally divides by 5% of the *level*
/// instead, which hides genuine deviations.
#[test]
fn large_dc_offset_channel_still_reports_a_real_deviation() {
    let len = 41;
    // Reference: a 300 K baseline with a 1 K swing. magnitude/range is ~150,
    // far past the 20x point at which `0.05 * magnitude` overtakes the range.
    let (times, omc_values) = ramp_series(len, |t| 300.0 + (t * std::f64::consts::TAU).sin());
    // rumoca reproduces the baseline but gets 60% of the dynamics wrong.
    let (_, rumoca_values) = ramp_series(len, |t| 300.0 + 0.4 * (t * std::f64::consts::TAU).sin());

    let metric = compare_channel(
        "wall.T",
        ChannelSeries::new(&times, &rumoca_values),
        ChannelSeries::new(&times, &omc_values),
        false,
        None,
    )
    .expect("channel should compare");

    assert!(
        metric.reference_magnitude > 20.0 * metric.reference_range,
        "fixture must actually be DC dominated: magnitude={}, range={}",
        metric.reference_magnitude,
        metric.reference_range
    );
    assert_eq!(
        metric.normalization_scale, metric.reference_range,
        "a channel with a usable range must normalize against that range, not \
         against 5% of its DC level"
    );
    // Against the range (~2 K) the 0.6 K mean error is a real deviation.
    // Against 5% of the 300 K level (15 K) it would score ~0.025 and be
    // silently filed as high agreement.
    assert!(
        metric.bounded_normalized_l1_error > HIGH_AGREEMENT_CHANNEL_THRESHOLD,
        "a 60% dynamic-range error on a DC-offset channel must still be reported, got {}",
        metric.bounded_normalized_l1_error
    );
}

// ---------------------------------------------------------------------------
// Array-valued quantities: components with no scale of their own
// ---------------------------------------------------------------------------

fn channel_metric<'a>(metric: &'a ModelDeviationMetric, name: &str) -> &'a ChannelDeviationMetric {
    metric
        .worst_variables
        .iter()
        .find(|channel| channel.name == name)
        .unwrap_or_else(|| panic!("fixture must report channel {name}"))
}

#[test]
fn array_element_base_accepts_only_literal_subscripts() {
    assert_eq!(array_element_base("boxBody2.r_0[3]"), Some("boxBody2.r_0"));
    assert_eq!(array_element_base("frame_a.R.T[1,2]"), Some("frame_a.R.T"));
    assert_eq!(array_element_base("frame_a.R.T[1, 2]"), Some("frame_a.R.T"));
    assert_eq!(array_element_base("scalar"), None);
    assert_eq!(array_element_base("[1]"), None);
    assert_eq!(array_element_base("y[i]"), None);
    assert_eq!(array_element_base("y[]"), None);
    assert_eq!(array_element_base("y[1"), None);
}

/// A vector component whose reference is identically zero has neither a range
/// nor a level, so the metric used to divide its residual by a unit-free
/// `1e-6`. The same absolute residual then scored high agreement on one
/// component of a force and near-total disagreement on another. The scale of
/// such a component comes from its siblings, which Modelica guarantees share
/// its type and unit.
#[test]
fn information_free_array_element_normalizes_against_its_siblings() {
    let len = 21;
    let times = (0..len)
        .map(|i| i as f64 / (len - 1) as f64)
        .collect::<Vec<_>>();
    let big = times.iter().map(|t| 800.0 * t).collect::<Vec<_>>();
    let small = times.iter().map(|t| 400.0 * t).collect::<Vec<_>>();
    let omc = trace(
        "M",
        times.clone(),
        vec!["frame_b.f[1]", "frame_b.f[2]", "frame_b.f[3]", "loose"],
        vec![big.clone(), small.clone(), vec![0.0; len], vec![0.0; len]],
    );
    let rumoca = trace(
        "M",
        times.clone(),
        vec!["frame_b.f[1]", "frame_b.f[2]", "frame_b.f[3]", "loose"],
        vec![big, small, vec![2.9e-5; len], vec![2.9e-5; len]],
    );

    let metric = compare_model_traces("M", &rumoca, &omc).expect("model compare");
    let component = channel_metric(&metric, "frame_b.f[3]");
    let sibling_floor = component
        .reference_array_group_floor
        .expect("an array element with informative siblings must carry a group floor");
    assert_eq!(component.normalization_scale, sibling_floor);
    assert!(
        (sibling_floor - 360.0).abs() < 1.0,
        "the floor must be the *smallest* usable sibling range, got {sibling_floor}"
    );
    assert!(
        component.bounded_normalized_l1_error < HIGH_AGREEMENT_CHANNEL_THRESHOLD,
        "a 2.9e-5 N residual on a force whose siblings carry hundreds of newtons \
         is not a disagreement, got {}",
        component.bounded_normalized_l1_error
    );

    // The identical residual on a *scalar* zero channel has no measured scale
    // to fall back on and keeps the absolute floor: nothing is granted for
    // free, only what a sibling of the same quantity already earned.
    let scalar = channel_metric(&metric, "loose");
    assert_eq!(scalar.reference_array_group_floor, None);
    assert_eq!(scalar.normalization_scale, CONTINUOUS_ABSOLUTE_SCALE_FLOOR);
    assert!(scalar.bounded_normalized_l1_error > MINOR_AGREEMENT_CHANNEL_THRESHOLD);
}

/// The sibling floor is a scale, not an amnesty: an error that is large against
/// that scale is still reported. This is what stops the rule from crediting
/// agreement we have not earned.
#[test]
fn real_error_on_information_free_array_element_is_still_reported() {
    let len = 21;
    let times = (0..len)
        .map(|i| i as f64 / (len - 1) as f64)
        .collect::<Vec<_>>();
    let sibling = times.iter().map(|t| 400.0 * t).collect::<Vec<_>>();
    let omc = trace(
        "M",
        times.clone(),
        vec!["frame_b.f[1]", "frame_b.f[2]"],
        vec![sibling.clone(), vec![0.0; len]],
    );
    let rumoca = trace(
        "M",
        times.clone(),
        vec!["frame_b.f[1]", "frame_b.f[2]"],
        vec![sibling, vec![200.0; len]],
    );

    let metric = compare_model_traces("M", &rumoca, &omc).expect("model compare");
    let component = channel_metric(&metric, "frame_b.f[2]");
    assert!(
        component.bounded_normalized_l1_error > MINOR_AGREEMENT_CHANNEL_THRESHOLD,
        "a 200 N residual against a 360 N sibling scale must stay a deviation, got {}",
        component.bounded_normalized_l1_error
    );
}

/// The smallest usable sibling range wins, so a degenerate component is never
/// granted more tolerance than the least tolerant component of its own array.
#[test]
fn array_group_floor_takes_the_smallest_usable_sibling_range() {
    let len = 21;
    let times = (0..len)
        .map(|i| i as f64 / (len - 1) as f64)
        .collect::<Vec<_>>();
    let loud = times.iter().map(|t| 1000.0 * t).collect::<Vec<_>>();
    let quiet = times.iter().map(|t| 0.01 * t).collect::<Vec<_>>();
    let omc = trace(
        "M",
        times.clone(),
        vec!["s.v[1]", "s.v[2]", "s.v[3]"],
        vec![loud.clone(), quiet.clone(), vec![0.0; len]],
    );
    let rumoca = trace(
        "M",
        times.clone(),
        vec!["s.v[1]", "s.v[2]", "s.v[3]"],
        vec![loud, quiet, vec![5.0e-3; len]],
    );

    let metric = compare_model_traces("M", &rumoca, &omc).expect("model compare");
    let component = channel_metric(&metric, "s.v[3]");
    assert!(
        component.normalization_scale < 0.02,
        "the quiet sibling must set the floor, got {}",
        component.normalization_scale
    );
    assert!(
        component.bounded_normalized_l1_error > MINOR_AGREEMENT_CHANNEL_THRESHOLD,
        "a 5e-3 residual against a 0.009 sibling scale is a deviation, got {}",
        component.bounded_normalized_l1_error
    );
}

/// An array element that *does* carry a level keeps normalizing against its own
/// magnitude: the sibling floor only replaces the unit-free constant, it never
/// displaces a scale measured from the channel itself.
#[test]
fn array_element_with_a_level_keeps_its_own_magnitude_scale() {
    let len = 21;
    let times = (0..len)
        .map(|i| i as f64 / (len - 1) as f64)
        .collect::<Vec<_>>();
    let sibling = times.iter().map(|t| 100.0 * t).collect::<Vec<_>>();
    let omc = trace(
        "M",
        times.clone(),
        vec!["s.v[1]", "s.v[2]"],
        vec![sibling.clone(), vec![0.5; len]],
    );
    let rumoca = trace(
        "M",
        times.clone(),
        vec!["s.v[1]", "s.v[2]"],
        vec![sibling, vec![5.0; len]],
    );

    let metric = compare_model_traces("M", &rumoca, &omc).expect("model compare");
    let component = channel_metric(&metric, "s.v[2]");
    let expected_scale = MAGNITUDE_SCALE_FRACTION * 0.5;
    assert!(
        (component.normalization_scale - expected_scale).abs() < 1.0e-15,
        "a flat non-zero element must keep 5% of its own level, got {}",
        component.normalization_scale
    );
    assert!(component.bounded_normalized_l1_error > MINOR_AGREEMENT_CHANNEL_THRESHOLD);
}

/// When no sibling carries a usable range either, there is still nothing to
/// measure and the absolute floor stands.
#[test]
fn array_without_a_usable_sibling_keeps_the_absolute_floor() {
    let len = 21;
    let times = (0..len)
        .map(|i| i as f64 / (len - 1) as f64)
        .collect::<Vec<_>>();
    let omc = trace(
        "M",
        times.clone(),
        vec!["s.v[1]", "s.v[2]"],
        vec![vec![0.0; len], vec![0.0; len]],
    );
    let rumoca = trace(
        "M",
        times.clone(),
        vec!["s.v[1]", "s.v[2]"],
        vec![vec![2.9e-5; len], vec![0.0; len]],
    );

    let metric = compare_model_traces("M", &rumoca, &omc).expect("model compare");
    let component = channel_metric(&metric, "s.v[1]");
    assert_eq!(component.reference_array_group_floor, None);
    assert_eq!(
        component.normalization_scale,
        CONTINUOUS_ABSOLUTE_SCALE_FLOOR
    );
    assert!(component.bounded_normalized_l1_error > MINOR_AGREEMENT_CHANNEL_THRESHOLD);
}

/// Discrete channels are scored in level units and neither feed nor consume the
/// continuous array floor.
#[test]
fn discrete_array_elements_ignore_the_group_floor() {
    let len = 3;
    let times = vec![0.0, 0.5, 1.0];
    let meta = |name: &str| SimTraceVariableMeta {
        name: name.to_string(),
        role: Some("algebraic".to_string()),
        value_type: Some("Real".to_string()),
        variability: Some("discrete".to_string()),
        time_domain: Some("event-discrete".to_string()),
    };
    let names = vec!["q[1]".to_string(), "q[2]".to_string()];
    let omc = SimTrace {
        model_name: Some("M".to_string()),
        times: times.clone(),
        names: names.clone(),
        data: vec![
            vec![Some(0.0), Some(400.0), Some(800.0)],
            vec![Some(0.0); len],
        ],
        variable_meta: Some(vec![meta("q[1]"), meta("q[2]")]),
        certification_profile: None,
    };
    let rumoca = SimTrace {
        model_name: Some("M".to_string()),
        times,
        names,
        data: vec![
            vec![Some(0.0), Some(400.0), Some(800.0)],
            vec![Some(0.5); len],
        ],
        variable_meta: Some(vec![meta("q[1]"), meta("q[2]")]),
        certification_profile: None,
    };

    let metric = compare_model_traces("M", &rumoca, &omc).expect("model compare");
    let component = channel_metric(&metric, "q[2]");
    assert_eq!(component.reference_array_group_floor, None);
    assert_eq!(component.normalization_scale, DISCRETE_SCALE_FLOOR);
}

proptest! {
    /// The bounded score is a squashed ratio and must stay a probability-like
    /// number no matter how degenerate the inputs are.
    #[test]
    fn bounded_normalized_l1_error_is_always_in_unit_interval(
        rumoca_values in prop::collection::vec(-1.0e6_f64..1.0e6, 2..40),
        offset in -1.0e6_f64..1.0e6,
    ) {
        let len = rumoca_values.len();
        let times = (0..len).map(|i| i as f64 / (len - 1) as f64).collect::<Vec<_>>();
        let rumoca = rumoca_values.iter().map(|&v| Some(v)).collect::<Vec<_>>();
        let omc = rumoca_values.iter().map(|&v| Some(v + offset)).collect::<Vec<_>>();

        let metric = compare_channel(
            "x",
            ChannelSeries::new(&times, &rumoca),
            ChannelSeries::new(&times, &omc),
            false,
            None,
        )
        .expect("channel should compare");

        prop_assert!(metric.normalization_scale.is_finite());
        prop_assert!(metric.normalization_scale > 0.0);
        prop_assert!((0.0..1.0).contains(&metric.bounded_normalized_l1_error));
        prop_assert!(metric.normalized_l1_error.is_finite());
    }

    /// Comparing a linear channel against itself on a refined grid must not
    /// manufacture error: the grid merge and interpolation have to be stable.
    #[test]
    fn channel_metric_is_invariant_under_grid_refinement(
        slope in -1.0e3_f64..1.0e3,
        intercept in -1.0e3_f64..1.0e3,
        refinement in 2usize..9,
    ) {
        let coarse_len = 5;
        let (coarse_times, coarse_values) =
            ramp_series(coarse_len, |t| slope * t + intercept);
        let fine_len = (coarse_len - 1) * refinement + 1;
        let (fine_times, fine_values) = ramp_series(fine_len, |t| slope * t + intercept);

        let metric = compare_channel(
            "x",
            ChannelSeries::new(&coarse_times, &coarse_values),
            ChannelSeries::new(&fine_times, &fine_values),
            false,
            None,
        )
        .expect("channel should compare");

        prop_assert!(
            metric.bounded_normalized_l1_error < HIGH_AGREEMENT_CHANNEL_THRESHOLD,
            "grid refinement produced error {}",
            metric.bounded_normalized_l1_error
        );
    }

    /// Discrete channels keep their one-level floor: a level mismatch of one is
    /// a full disagreement, and a tiny reference spread must not inflate it.
    #[test]
    fn discrete_channel_scale_never_below_one(
        levels in prop::collection::vec(-4.0_f64..4.0, 2..30),
    ) {
        let scale = reference_scale(&levels, true, None);
        prop_assert!(scale.normalization_scale >= DISCRETE_SCALE_FLOOR);
        prop_assert!(scale.normalization_scale >= scale.range);
    }
}
