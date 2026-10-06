//! Comparison of a channel the Rumoca trace records as a function of a
//! phasor (SPEC_0050).
//!
//! Solve lowering records, from the channel's defining equation, that it is
//! the angle `atan2(im, re)` of two component channels or the cosine of that
//! angle (`SolveVariableMeta::phasor`). Two facts follow, and the comparator
//! uses nothing else:
//!
//! - An angle is meaningful only modulo `2*pi`, so an angle channel compares
//!   by the shortest signed difference on the circle and interpolates along
//!   the shortest arc; `pi` and `-pi` across the `atan2` branch cut agree, and
//!   a real phase error at any nonzero magnitude is still its full arc.
//! - Neither function is defined where the phasor is zero. A grid sample
//!   carries no angle evidence when the phasor is zero at the comparator's
//!   own resolution in both traces: `|(re, im)| <= n_h * min(S_re, S_im)`,
//!   where `S` is a component channel's normalization scale and `n_h` the
//!   largest normalized error a high-agreement channel may carry, so every
//!   angle is the angle of a phasor the component comparison cannot tell
//!   from this one. Such a sample is dropped only when both component
//!   channels are themselves compared in both traces and each is high, so
//!   the components carry the verdict there. A sample is never dropped on the
//!   angle alone.
//!
//! A channel the dropped samples leave without a comparable sample pair is
//! reported among the model's undefined phasor channels instead of as a
//! compared channel.

use std::collections::BTreeMap;

use rumoca_ir_solve::{SolvePhasor, SolvePhasorFunction};
use serde::{Deserialize, Serialize};

use super::{
    AgreementBand, ChannelDeviationMetric, ChannelSeries, HIGH_AGREEMENT_CHANNEL_THRESHOLD,
    MINOR_AGREEMENT_CHANNEL_THRESHOLD, SimTrace, TracePair, classify_channel_error, interp_channel,
    interp_linear_by,
};

/// How one channel was compared as a function of a phasor.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct PhasorChannelEvidence {
    pub function: SolvePhasorFunction,
    pub re: String,
    pub im: String,
    /// Grid samples dropped because the phasor is zero at the comparator's
    /// resolution in both traces and its components agree.
    pub undefined_samples: usize,
}

/// Every channel the Rumoca trace records as a function of a phasor.
pub(super) fn recorded_phasors(trace: &SimTrace) -> impl Iterator<Item = (&String, &SolvePhasor)> {
    trace
        .variable_meta
        .iter()
        .flatten()
        .filter_map(|meta| Some((&meta.name, meta.phasor.as_ref()?)))
}

/// The comparison rule of one phasor channel.
pub(super) struct PhasorRule<'a> {
    phasor: &'a SolvePhasor,
    zero: Option<ZeroPhasor<'a>>,
}

/// Both traces' component channels of a phasor whose components agree, and
/// the radius below which the comparator cannot tell the phasor from zero.
struct ZeroPhasor<'a> {
    rumoca: [ChannelSeries<'a>; 2],
    omc: [ChannelSeries<'a>; 2],
    use_step_hold: [bool; 2],
    resolution: f64,
}

impl<'a> PhasorRule<'a> {
    /// The rule for `phasor`, given the pointwise metrics of every compared
    /// channel; the zero test applies only when both components are compared
    /// and high.
    pub(super) fn new(
        phasor: &'a SolvePhasor,
        pair: &'a TracePair<'a>,
        pointwise: &BTreeMap<String, ChannelDeviationMetric>,
    ) -> Self {
        Self {
            phasor,
            zero: ZeroPhasor::of(phasor, pair, pointwise),
        }
    }

    /// True when the phasor is zero at the comparator's resolution in both
    /// traces at `t` and its components agree.
    pub(super) fn undefined_at(&self, t: f64) -> bool {
        self.zero.as_ref().is_some_and(|zero| zero.is_zero_at(t))
    }

    /// One trace's channel value at `t`: an angle interpolates along the
    /// shortest arc.
    pub(super) fn interp(
        &self,
        series: ChannelSeries<'_>,
        t: f64,
        use_step_hold: bool,
    ) -> Option<f64> {
        match self.phasor.function {
            SolvePhasorFunction::Angle if !use_step_hold => {
                interp_linear_by(series.times, series.values, t, |v0, v1| {
                    shortest_arc(v1 - v0)
                })
            }
            _ => interp_channel(series.times, series.values, t, use_step_hold),
        }
    }

    /// The Rumoca value `rumoca` as compared with the OMC value `omc`: an
    /// angle is moved to the branch nearest `omc`, so the two differ by their
    /// shortest signed arc.
    pub(super) fn align(&self, rumoca: f64, omc: f64) -> f64 {
        match self.phasor.function {
            SolvePhasorFunction::Angle => omc + shortest_arc(rumoca - omc),
            SolvePhasorFunction::CosineOfAngle => rumoca,
        }
    }

    pub(super) fn evidence(&self, undefined_samples: usize) -> PhasorChannelEvidence {
        PhasorChannelEvidence {
            function: self.phasor.function,
            re: self.phasor.re.clone(),
            im: self.phasor.im.clone(),
            undefined_samples,
        }
    }
}

impl<'a> ZeroPhasor<'a> {
    /// The zero test of `phasor`, when both its components are compared and
    /// high.
    fn of(
        phasor: &SolvePhasor,
        pair: &'a TracePair<'a>,
        pointwise: &BTreeMap<String, ChannelDeviationMetric>,
    ) -> Option<Self> {
        let components = [phasor.re.as_str(), phasor.im.as_str()];
        let metrics = components.map(|component| pointwise.get(component));
        let scale = metrics
            .into_iter()
            .map(|metric| {
                metric
                    .filter(|metric| is_high(metric))
                    .map(|metric| metric.normalization_scale)
            })
            .collect::<Option<Vec<_>>>()?
            .into_iter()
            .fold(f64::INFINITY, f64::min);
        let (rumoca_re, omc_re) = pair.series(components[0])?;
        let (rumoca_im, omc_im) = pair.series(components[1])?;
        Some(Self {
            rumoca: [rumoca_re, rumoca_im],
            omc: [omc_re, omc_im],
            use_step_hold: components.map(|component| pair.is_discrete(component)),
            resolution: high_agreement_normalized_error() * scale,
        })
    }

    fn is_zero_at(&self, t: f64) -> bool {
        [self.rumoca, self.omc].iter().all(|series| {
            self.magnitude(series, t)
                .is_some_and(|magnitude| magnitude <= self.resolution)
        })
    }

    /// One trace's phasor magnitude at `t`.
    fn magnitude(&self, series: &[ChannelSeries<'_>; 2], t: f64) -> Option<f64> {
        let component =
            |k: usize| interp_channel(series[k].times, series[k].values, t, self.use_step_hold[k]);
        Some(component(0)?.hypot(component(1)?))
    }
}

fn is_high(metric: &ChannelDeviationMetric) -> bool {
    classify_channel_error(
        metric.bounded_normalized_l1_error,
        HIGH_AGREEMENT_CHANNEL_THRESHOLD,
        MINOR_AGREEMENT_CHANNEL_THRESHOLD,
    ) == AgreementBand::HighAgreement
}

/// The largest normalized L1 error a high-agreement channel may carry: a
/// channel is high when `e / (1 + e)` is below the high threshold.
fn high_agreement_normalized_error() -> f64 {
    HIGH_AGREEMENT_CHANNEL_THRESHOLD / (1.0 - HIGH_AGREEMENT_CHANNEL_THRESHOLD)
}

/// `difference` moved by a whole number of turns into `[-pi, pi]`.
fn shortest_arc(difference: f64) -> f64 {
    difference - std::f64::consts::TAU * (difference / std::f64::consts::TAU).round()
}
