//! Excursions of information-free references: a run outside a flat bulk is
//! classified against the reference at a tighter tolerance, and scored as an
//! event or as noise (see the `excursion` module).

use super::super::excursion::{
    EVENT_SURVIVAL_AMPLITUDE_FRACTION, MIN_EXCURSION_SAMPLES, SupportedRun, classify_run,
    dominant_run, supported_runs,
};
use super::*;

/// Grid points of the synthetic traces, on [0, 1].
const POINTS: usize = 1001;

fn grid() -> Vec<f64> {
    (0..POINTS)
        .map(|i| i as f64 / (POINTS - 1) as f64)
        .collect()
}

/// A flat zero channel with a triangular bump of `amplitude` over samples
/// `first..=last`: less than 5% of the run, so its robust band stays flat,
/// as `SpeedControlledDCPM`'s 19.6 V saturation does over 1.1% of its run.
fn bump(first: usize, last: usize, amplitude: f64) -> Vec<f64> {
    let middle = 0.5 * (first + last) as f64;
    let half_width = 0.5 * (last - first) as f64 + 1.0;
    (0..POINTS)
        .map(|i| {
            if (first..=last).contains(&i) {
                amplitude * (1.0 - (i as f64 - middle).abs() / half_width)
            } else {
                0.0
            }
        })
        .collect()
}

fn single(name: &str, values: Vec<f64>) -> SimTrace {
    trace("Excursion", grid(), vec![name], vec![values])
}

/// The model's channels whose excursion is still unclassified.
fn unclassified(model: &ModelDeviationMetric) -> Vec<String> {
    model
        .excursion_channels
        .iter()
        .filter(|channel| channel.class == ExcursionClass::Unclassified)
        .map(|channel| channel.name.clone())
        .collect()
}

fn channel(
    rumoca: &SimTrace,
    omc: &SimTrace,
    tight: Option<&SimTrace>,
) -> (ChannelDeviationMetric, ModelDeviationMetric) {
    let model = compare_model_traces_with_tight_reference("Excursion", rumoca, omc, tight)
        .expect("traces compare");
    (model.worst_variables[0].clone(), model)
}

/// An event a tighter reference reproduces, which rumoca tracks closely: the
/// channel is scaled by the event's amplitude and scores as agreement. Without
/// the tighter reference it is scored as before, against the bare floor, as a
/// deviation: this is `SpeedControlledDCPM`'s anti-windup channel.
#[test]
fn a_tracked_event_is_scaled_by_its_amplitude() {
    let omc = single("u", bump(500, 530, 20.0));
    let tracked = bump(500, 530, 20.0)
        .into_iter()
        .map(|value| if value > 0.0 { value + 1.0e-3 } else { value })
        .collect();
    let rumoca = single("u", tracked);

    let (before, model) = channel(&rumoca, &omc, None);
    assert_eq!(before.normalization_scale, CONTINUOUS_ABSOLUTE_SCALE_FLOOR);
    assert!(before.bounded_normalized_l1_error > MINOR_AGREEMENT_CHANNEL_THRESHOLD);
    let evidence = before.reference_excursion.expect("a supported run");
    assert_eq!(evidence.class, ExcursionClass::Unclassified);
    assert_eq!(unclassified(&model), vec!["u".to_string()]);

    let (after, model) = channel(&rumoca, &omc, Some(&omc));
    let evidence = after.reference_excursion.expect("a supported run");
    assert_eq!(evidence.class, ExcursionClass::Event);
    assert!((after.normalization_scale - evidence.amplitude).abs() < 1.0e-9);
    assert!(after.bounded_normalized_l1_error < HIGH_AGREEMENT_CHANNEL_THRESHOLD);
    assert!(unclassified(&model).is_empty());
    assert_eq!(
        model.excursion_channels,
        vec![ExcursionChannel {
            name: "u".to_string(),
            class: ExcursionClass::Event,
        }]
    );
}

/// The same event, missed: scored over the event's own window it is a
/// deviation. A whole-run mean against the event's amplitude would have
/// diluted it to agreement, which is the amnesty this rule must not grant.
#[test]
fn a_missed_event_is_a_deviation() {
    let omc = single("u", bump(500, 530, 20.0));
    let rumoca = single("u", vec![0.0; POINTS]);

    let (metric, _) = channel(&rumoca, &omc, Some(&omc));
    assert_eq!(
        metric.reference_excursion.map(|evidence| evidence.class),
        Some(ExcursionClass::Event)
    );
    assert!(metric.bounded_normalized_l1_error > MINOR_AGREEMENT_CHANNEL_THRESHOLD);
    let whole_run_score = bounded_error(metric.mean_abs_error / metric.normalization_scale);
    assert!(whole_run_score < HIGH_AGREEMENT_CHANNEL_THRESHOLD);
}

/// OMC noise that vanishes at the tighter tolerance, where rumoca is clean:
/// the scale widens to the noise amplitude and the score is never worse than
/// before, so noise cannot raise an alarm. This is `RoomCO2WithControls`'
/// mass-flow channels.
#[test]
fn noise_that_vanishes_only_widens_the_scale() {
    let noisy = bump(300, 305, 3.0e-5);
    let omc = single("m_flow", noisy);
    let omc_tight = single("m_flow", vec![0.0; POINTS]);
    let rumoca = single("m_flow", vec![0.0; POINTS]);

    let (before, _) = channel(&rumoca, &omc, None);
    let (after, _) = channel(&rumoca, &omc, Some(&omc_tight));
    assert_eq!(
        after.reference_excursion.map(|evidence| evidence.class),
        Some(ExcursionClass::Noise)
    );
    assert!(after.normalization_scale >= before.normalization_scale);
    assert!(after.bounded_normalized_l1_error <= before.bounded_normalized_l1_error);
}

/// Noise whose amplitude is barely above the floor widens nothing: the scale
/// is the larger of the two, never narrower than before.
#[test]
fn a_noise_scale_is_never_narrower_than_before() {
    let omc = single("m_flow", bump(300, 305, 1.5e-6));
    let omc_tight = single("m_flow", vec![0.0; POINTS]);
    let rumoca = single("m_flow", vec![0.0; POINTS]);

    let (before, _) = channel(&rumoca, &omc, None);
    let (after, _) = channel(&rumoca, &omc, Some(&omc_tight));
    assert!(after.normalization_scale >= before.normalization_scale);
}

/// A tighter reference that lacks the channel is no evidence: the channel is
/// scored exactly as without one, and stays listed for classification.
#[test]
fn a_missing_tight_channel_changes_nothing() {
    let omc = single("u", bump(500, 530, 20.0));
    let rumoca = single("u", vec![0.0; POINTS]);
    let other = single("v", vec![0.0; POINTS]);

    let (without, _) = channel(&rumoca, &omc, None);
    let (with_other, model) = channel(&rumoca, &omc, Some(&other));
    assert_eq!(with_other.normalization_scale, without.normalization_scale);
    assert_eq!(
        with_other.bounded_normalized_l1_error,
        without.bounded_normalized_l1_error
    );
    assert_eq!(unclassified(&model), vec!["u".to_string()]);
}

/// An isolated one- or two-sample spike is not a run: it offers no scale and
/// the channel is scored as before.
#[test]
fn a_spike_is_not_an_excursion() {
    for last in [500, 501] {
        let mut spiked = vec![0.0; POINTS];
        for value in &mut spiked[500..=last] {
            *value = 30.0;
        }
        let omc = single("q", spiked);
        let rumoca = single("q", vec![0.0; POINTS]);
        let (metric, model) = channel(&rumoca, &omc, Some(&omc));
        assert!(
            metric.reference_excursion.is_none(),
            "{} samples",
            last - 499
        );
        assert_eq!(metric.normalization_scale, CONTINUOUS_ABSOLUTE_SCALE_FLOOR);
        assert!(unclassified(&model).is_empty());
    }
}

/// A reference with a usable robust range is untouched, whatever lies
/// outside its band.
#[test]
fn a_channel_with_a_usable_range_has_no_excursion() {
    let ramp = (0..POINTS).map(|i| i as f64 / 100.0).collect::<Vec<_>>();
    let omc = single("x", ramp.clone());
    let rumoca = single("x", ramp);
    let (metric, _) = channel(&rumoca, &omc, Some(&omc));
    assert!(metric.reference_excursion.is_none());
}

#[test]
fn supported_runs_need_the_minimum_length_and_end_at_a_gap() {
    let floor = CONTINUOUS_ABSOLUTE_SCALE_FLOOR;
    let mut values = vec![0.0; 20];
    values[2..4].fill(1.0);
    values[6..6 + MIN_EXCURSION_SAMPLES].fill(2.0);
    values[12..16].fill(-3.0);
    values[14] = f64::NAN;
    values[17..20].fill(10.0 * floor);
    let runs = supported_runs(&values, 0.0, 0.0);
    assert_eq!(
        runs,
        vec![
            SupportedRun {
                first: 6,
                last: 5 + MIN_EXCURSION_SAMPLES,
                amplitude: 2.0,
            },
            SupportedRun {
                first: 17,
                last: 19,
                amplitude: 10.0 * floor,
            },
        ]
    );
    let dominant = dominant_run(&values, 0.0, 0.0).expect("a run");
    assert_eq!(dominant.first, 6);
}

#[test]
fn a_run_is_an_event_only_when_the_tight_run_overlaps_with_its_amplitude() {
    let run = SupportedRun {
        first: 10,
        last: 19,
        amplitude: 4.0,
    };
    let at = |first: usize, last: usize, amplitude: f64| {
        let mut values = vec![0.0; 40];
        values[first..=last].fill(amplitude);
        values
    };
    let kept = 4.0 * EVENT_SURVIVAL_AMPLITUDE_FRACTION;
    assert_eq!(
        classify_run(run, &at(12, 18, kept), 0.0, 0.0),
        ExcursionClass::Event
    );
    assert_eq!(
        classify_run(run, &at(12, 18, 0.9 * kept), 0.0, 0.0),
        ExcursionClass::Noise
    );
    assert_eq!(
        classify_run(run, &at(25, 30, 4.0), 0.0, 0.0),
        ExcursionClass::Noise
    );
    assert_eq!(
        classify_run(run, &vec![0.0; 40], 0.0, 0.0),
        ExcursionClass::Noise
    );
}
