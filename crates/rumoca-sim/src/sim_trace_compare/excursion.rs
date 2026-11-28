//! Scale for a reference whose robust statistics cannot see its only event.
//!
//! [`super::normalization`] judges a channel's scale from its 5th-95th
//! percentiles. A channel whose bulk is flat but which carries a real event
//! covering less than 5% of the run (`SpeedControlledDCPM`'s current
//! controller saturates by 19.6 V for 11 ms of a 1 s run) reads as carrying no
//! scale at all, and its error is normalized against the bare unit-free floor.
//!
//! The samples outside the robust band say what the scale is, but only if they
//! are an *event*. OMC's own numerical noise also leaves short runs outside a
//! flat bulk, and on amplitude alone noise that rumoca does not reproduce looks
//! exactly like an event that rumoca missed. A fixed run length cannot separate
//! them either: noise runs reach 18 samples on `RoomCO2WithControls` against
//! the event's 112. What does separate them is a measurement: an event is a
//! property of the model and survives a tighter OMC tolerance, while noise is a
//! property of the integrator and shrinks or vanishes with it.
//!
//! So every supported run of the reference is classified, on its own, against
//! the same reference simulated at a tighter tolerance, and the channel is
//! scored by what survives:
//!
//! * **Event** (some run survives): the scale is the largest event's
//!   amplitude, and the score is the worst over the **union of every excursion
//!   window from either trace**, each scored over its own span: every reference
//!   event against its own amplitude, every run of Rumoca's samples outside the
//!   reference's band against the channel scale, and the whole run. A missed,
//!   spurious or outlasting event is then one case, not three: some window
//!   holds it. A tracked event stays tiny in every window.
//! * **Noise** (no run survives): the scale is widened to the largest run's amplitude when that is larger
//!   (never narrowed), and the error is averaged over the whole run as before.
//!   Noise can therefore widen a scale, but never raise an alarm.
//! * **Unclassified** (no tighter reference, or it has no such channel): the
//!   channel is scored exactly as before. No evidence, no change. A caller
//!   whose reference has an excursion channel must supply the tighter
//!   reference to get this rule at all: the MSL comparison declares it with
//!   the OMC reference and does not score a flagged model without one.

use super::normalization::{
    CONTINUOUS_ABSOLUTE_SCALE_FLOOR, ReferenceScale, reference_is_information_free,
    reference_scale, robust_reference_percentiles,
};
use super::{ChannelRule, interp_channel, saturate};
use serde::{Deserialize, Serialize};

/// Fewest consecutive reference samples outside the robust band that form a
/// run rather than a spike.
///
/// This is not a boundary between noise and events; the tighter-tolerance
/// reference draws that boundary. It only excludes an isolated one- or
/// two-sample spike, whose amplitude would otherwise be offered as the scale
/// of an otherwise flat channel.
pub(super) const MIN_EXCURSION_SAMPLES: usize = 3;

/// Fraction of a run's amplitude that the tighter-tolerance reference must
/// reproduce, over an overlapping window, for the run to count as an event.
///
/// Measured: `SpeedControlledDCPM`'s saturation reproduces its 19.6 V
/// amplitude exactly at a hundredth of the tolerance (ratio 1.0), while every
/// run on `RoomCO2WithControls` either vanishes or shrinks to at most 0.26 of
/// its amplitude.
pub(super) const EVENT_SURVIVAL_AMPLITUDE_FRACTION: f64 = 0.5;

/// A run of consecutive reference samples outside the robust band.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) struct SupportedRun {
    /// Index of the run's first sample in the ordered reference.
    pub first: usize,
    /// Index of the run's last sample in the ordered reference.
    pub last: usize,
    /// The furthest any of the run's samples lies outside the band. Measured
    /// from the band rather than as the run's own max-min, so a plateau-shaped
    /// event has the size it actually has.
    pub amplitude: f64,
}

impl SupportedRun {
    fn len(self) -> usize {
        self.last - self.first + 1
    }

    fn overlaps(self, other: Self) -> bool {
        self.first <= other.last && other.first <= self.last
    }
}

/// How a run was classified against the tighter-tolerance reference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ExcursionClass {
    /// The run survives at the tighter tolerance: a property of the model.
    Event,
    /// The run vanishes or shrinks at the tighter tolerance: OMC noise.
    Noise,
    /// No tighter-tolerance reference was available for this channel.
    Unclassified,
}

/// The excursion a channel's scale was judged from.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) struct ReferenceExcursion {
    pub run: SupportedRun,
    pub class: ExcursionClass,
}

/// Evidence recorded on a channel whose reference has a supported excursion,
/// so a reviewer can see which channels the rule touched and why.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct ExcursionEvidence {
    pub class: ExcursionClass,
    pub amplitude: f64,
    pub window_start: f64,
    pub window_end: f64,
    pub samples: usize,
}

/// Every run of at least [`MIN_EXCURSION_SAMPLES`] consecutive samples of
/// `ordered_reference` lying more than the absolute floor beyond `[p05, p95]`.
///
/// `ordered_reference` is in time order, one value per compared grid point.
/// A non-finite sample ends a run.
pub(super) fn supported_runs(ordered_reference: &[f64], p05: f64, p95: f64) -> Vec<SupportedRun> {
    let outside_by = |value: f64| {
        if !value.is_finite() {
            return None;
        }
        let beyond = if value > p95 {
            value - p95
        } else if value < p05 {
            p05 - value
        } else {
            0.0
        };
        (beyond > CONTINUOUS_ABSOLUTE_SCALE_FLOOR).then_some(beyond)
    };
    let mut runs = Vec::new();
    let mut open: Option<SupportedRun> = None;
    for (index, &value) in ordered_reference.iter().enumerate() {
        match (outside_by(value), open.as_mut()) {
            (Some(beyond), Some(run)) => {
                run.last = index;
                run.amplitude = run.amplitude.max(beyond);
            }
            (Some(beyond), None) => {
                open = Some(SupportedRun {
                    first: index,
                    last: index,
                    amplitude: beyond,
                });
            }
            (None, _) => {
                if let Some(run) = open.take().filter(|run| run.len() >= MIN_EXCURSION_SAMPLES) {
                    runs.push(run);
                }
            }
        }
    }
    if let Some(run) = open.filter(|run| run.len() >= MIN_EXCURSION_SAMPLES) {
        runs.push(run);
    }
    runs
}

/// The longest supported run (the larger amplitude breaks a tie), if any.
pub(super) fn dominant_run(ordered_reference: &[f64], p05: f64, p95: f64) -> Option<SupportedRun> {
    supported_runs(ordered_reference, p05, p95)
        .into_iter()
        .max_by(|a, b| {
            a.len()
                .cmp(&b.len())
                .then(a.amplitude.total_cmp(&b.amplitude))
        })
}

/// The runs of the reference that survive at the tighter tolerance: those a
/// supported run of the tighter reference (resampled onto the same grid and
/// judged against the same robust band) overlaps with at least
/// [`EVENT_SURVIVAL_AMPLITUDE_FRACTION`] of their amplitude. Every run is
/// classified on its own, so a second event on the same channel is an event
/// too, and noise beside an event stays noise.
pub(super) fn event_runs(
    runs: &[SupportedRun],
    tight_on_grid: &[f64],
    p05: f64,
    p95: f64,
) -> Vec<SupportedRun> {
    let tight_runs = supported_runs(tight_on_grid, p05, p95);
    runs.iter()
        .copied()
        .filter(|run| {
            tight_runs.iter().any(|tight| {
                tight.overlaps(*run)
                    && tight.amplitude >= EVENT_SURVIVAL_AMPLITUDE_FRACTION * run.amplitude
            })
        })
        .collect()
}

/// The scale an excursion gives a channel whose robust statistics have none,
/// given `amplitude` (the largest event's amplitude for an event, the largest
/// run's for noise) and `fallback`, the scale the channel would have without it.
pub(super) fn excursion_scale(class: ExcursionClass, amplitude: f64, fallback: f64) -> f64 {
    match class {
        ExcursionClass::Event => amplitude.max(CONTINUOUS_ABSOLUTE_SCALE_FLOOR),
        ExcursionClass::Noise => fallback.max(amplitude),
        ExcursionClass::Unclassified => fallback,
    }
}

/// A channel of a model whose reference has a supported excursion, with the
/// excursion's class.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExcursionChannel {
    pub name: String,
    pub class: ExcursionClass,
}

/// The normalization scale of one compared channel, with an
/// information-free reference's runs classified against the tighter reference
/// when there is one, and the reference's event runs (empty unless the channel
/// is scaled by an event).
///
/// `ordered` is the channel's `(t, rumoca, omc)` samples in time order, one per
/// grid point: `ref_samples` repeats every interior point, which would
/// double-count a run's length. A phasor channel is compared under its own
/// alignment and has no excursion.
pub(super) fn channel_reference_scale(
    ref_samples: &[f64],
    ordered: &[(f64, f64, f64)],
    rule: ChannelRule<'_>,
) -> (ReferenceScale, Vec<SupportedRun>) {
    let ordered_reference = if rule.phasor.is_none() {
        ordered.iter().map(|&(_, _, omc)| omc).collect::<Vec<_>>()
    } else {
        Vec::new()
    };
    let scale = reference_scale(
        ref_samples,
        &ordered_reference,
        rule.use_step_hold,
        rule.array_group_floor,
    );
    let (Some(_), Some(tight)) = (scale.excursion, rule.omc_tight) else {
        return (scale, Vec::new());
    };
    // The same resampler the comparison grid uses, so both references are read
    // at the same instants; a gap in the tight trace ends a run.
    let tight_on_grid = ordered
        .iter()
        .map(|&(t, _, _)| interp_channel(tight.times, tight.values, t, false).unwrap_or(f64::NAN))
        .collect::<Vec<_>>();
    let runs = supported_runs(&ordered_reference, scale.p05, scale.p95);
    let events = event_runs(&runs, &tight_on_grid, scale.p05, scale.p95);
    let largest = |runs: &[SupportedRun]| {
        runs.iter()
            .copied()
            .max_by(|a, b| a.amplitude.total_cmp(&b.amplitude))
    };
    let scale = match (largest(&events), largest(&runs)) {
        (Some(event), _) => scale.classified(ExcursionClass::Event, event),
        (None, Some(run)) => scale.classified(ExcursionClass::Noise, run),
        (None, None) => scale,
    };
    (scale, events)
}

/// Normalized error of a channel scaled by its reference's events: the worst over
/// the union of every excursion window from either trace, each window's mean
/// absolute error over the amplitude it is judged against (the caller takes the
/// larger of this and the whole-run score).
///
/// * Each reference event is judged against its own amplitude, so a missed
///   event, the largest or a smaller second one, scores about one half.
/// * Each run of Rumoca's own samples outside the reference's robust band is
///   judged against the channel's scale, not its own amplitude, so a blip far
///   below the channel's events is not a deviation while a spurious or
///   outlasting event is.
///
/// `None` when there are no reference events, so the channel keeps its
/// whole-run score.
pub(super) fn excursion_normalized_error(
    ordered: &[(f64, f64, f64)],
    events: &[SupportedRun],
    scale: &ReferenceScale,
) -> Option<f64> {
    if events.is_empty() {
        return None;
    }
    let window_score = |run: SupportedRun, amplitude: f64| {
        window_mean_abs_error(&ordered[run.first..=run.last]).map(|error| error / amplitude)
    };
    let rumoca_ordered = ordered
        .iter()
        .map(|&(_, rumoca, _)| rumoca)
        .collect::<Vec<_>>();
    let rumoca_runs = supported_runs(&rumoca_ordered, scale.p05, scale.p95);
    events
        .iter()
        .filter_map(|&event| window_score(event, event.amplitude))
        .chain(
            rumoca_runs
                .into_iter()
                .filter_map(|run| window_score(run, scale.normalization_scale)),
        )
        .reduce(f64::max)
        .map(saturate)
}

/// Time-averaged absolute error over consecutive `(t, rumoca, omc)` samples,
/// trapezoidal as the whole-run integral is; `None` for a window without
/// duration.
fn window_mean_abs_error(window: &[(f64, f64, f64)]) -> Option<f64> {
    let (first, last) = (window.first()?, window.last()?);
    let duration = last.0 - first.0;
    if duration.partial_cmp(&0.0) != Some(std::cmp::Ordering::Greater) {
        return None;
    }
    let integral = window
        .windows(2)
        .map(|pair| {
            let (t0, r0, o0) = pair[0];
            let (t1, r1, o1) = pair[1];
            0.5 * ((r0 - o0).abs() + (r1 - o1).abs()) * (t1 - t0)
        })
        .sum::<f64>();
    Some(saturate(integral / duration))
}

/// The channels of a reference trace whose robust statistics see no scale but
/// which carry a supported run outside their band: the channels a tighter
/// reference would classify. Read from the reference alone, before any
/// comparison, so a runner can decide whether to simulate the reference again
/// at a tighter tolerance.
///
/// The test is the comparator's own (the same percentiles, information-free
/// test and run scanner) on the reference's native grid. If the comparison
/// grid later finds a run this missed, that channel is simply scored as
/// before.
pub fn reference_excursion_channels(reference: &super::SimTrace) -> Vec<String> {
    reference
        .names
        .iter()
        .zip(&reference.data)
        .filter(|(_, column)| {
            let ordered = column
                .iter()
                .map(|value| value.unwrap_or(f64::NAN))
                .collect::<Vec<_>>();
            let Some((p05, p95)) = robust_reference_percentiles(&ordered) else {
                return false;
            };
            let range = (p95 - p05).abs();
            let magnitude = p05.abs().max(p95.abs());
            reference_is_information_free(range, magnitude)
                && dominant_run(&ordered, p05, p95).is_some()
        })
        .map(|(name, _)| name.clone())
        .collect()
}
