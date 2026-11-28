//! Rule B's speed gauge: a shard's runner speed, read from OMC.
//!
//! CI runners come in speed classes about 2.3x apart, so a completion time
//! judged against an absolute budget certifies by runner lottery. OMC is
//! pinned and does the same work in every run, so the ratio of a model's OMC
//! wall time now to its stored reference measures the runner, not rumoca. For
//! a judged model `m` on shard `s`:
//!
//! `k_s(m) = median over fresh OMC timings j on s, j != m, of now(j) / ref(j)`
//!
//! Rumoca's stage times are divided by it before the margin is applied, so a
//! slow runner does not uncertify a model and a fast one does not certify a
//! near-budget model on luck; a genuine rumoca slowdown still reads in full.
//! A gauge it cannot trust (too few fresh timings, a speed outside the
//! bounds, or a spread that says the runner changed speed mid-shard) is
//! inconclusive, never an absolute judgement.

use std::collections::BTreeMap;

/// The fewest fresh same-shard OMC timings a gauge is read from.
pub const MIN_FRESH_TIMINGS: usize = 30;
/// The slowest and fastest runner a gauge is trusted for, relative to the frame.
pub const SPEED_BOUNDS: (f64, f64) = (1.0 / 3.0, 3.0);
/// The widest p90/p10 spread of a shard's OMC ratios a gauge is trusted for.
pub const MAX_RATIO_SPREAD: f64 = 1.5;

/// One model's OMC timing in this run.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct OmcTiming {
    pub shard: u32,
    pub wall_seconds: f64,
    /// Taken in this run on this shard's runner (not cached or resumed).
    pub fresh: bool,
}

/// This run's OMC timings against the stored reference walls.
#[derive(Debug, Clone, Default)]
pub struct SpeedGauge {
    /// Per shard: each fresh model's ratio of its wall now to its reference.
    ratios: BTreeMap<u32, Vec<(String, f64)>>,
}

impl SpeedGauge {
    /// The gauge of `now` against `reference` (model -> reference wall).
    pub fn new(now: &BTreeMap<String, OmcTiming>, reference: &BTreeMap<String, f64>) -> Self {
        let mut ratios: BTreeMap<u32, Vec<(String, f64)>> = BTreeMap::new();
        for (model, timing) in now {
            let Some(&reference) = reference.get(model) else {
                continue;
            };
            let usable = |seconds: f64| seconds.is_finite() && seconds > 0.0;
            if timing.fresh && usable(timing.wall_seconds) && usable(reference) {
                ratios
                    .entry(timing.shard)
                    .or_default()
                    .push((model.clone(), timing.wall_seconds / reference));
            }
        }
        Self { ratios }
    }

    /// `k_s(model)` for `model` on `shard`, leaving the model's own timing
    /// out, or why the gauge cannot be trusted.
    pub fn speed_for(&self, model: &str, shard: u32) -> Result<f64, String> {
        let mut ratios = self
            .ratios
            .get(&shard)
            .map(|ratios| {
                ratios
                    .iter()
                    .filter(|(other, _)| other != model)
                    .map(|(_, ratio)| *ratio)
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        judged_speed(&mut ratios, shard)
    }

    /// The shard's own speed over all its fresh timings, when trusted.
    pub fn shard_speed(&self, shard: u32) -> Result<f64, String> {
        let mut ratios = self
            .ratios
            .get(&shard)
            .map(|ratios| ratios.iter().map(|(_, ratio)| *ratio).collect::<Vec<_>>())
            .unwrap_or_default();
        judged_speed(&mut ratios, shard)
    }

    /// The median of the trusted shard speeds: the nominal speed of the frame
    /// a first promotion fixes, so one unusual runner cannot set it.
    pub fn nominal_speed(&self) -> Option<f64> {
        let mut speeds = self
            .ratios
            .keys()
            .filter_map(|shard| self.shard_speed(*shard).ok())
            .collect::<Vec<_>>();
        (!speeds.is_empty()).then(|| median(&mut speeds))
    }
}

fn judged_speed(ratios: &mut [f64], shard: u32) -> Result<f64, String> {
    if ratios.len() < MIN_FRESH_TIMINGS {
        return Err(format!(
            "shard {shard} has {} fresh OMC timings with a reference, fewer than {MIN_FRESH_TIMINGS}",
            ratios.len()
        ));
    }
    ratios.sort_by(f64::total_cmp);
    let speed = median(ratios);
    let (p10, p90) = (percentile(ratios, 0.1), percentile(ratios, 0.9));
    if p90 / p10 > MAX_RATIO_SPREAD {
        return Err(format!(
            "shard {shard}'s OMC ratios spread {p10:.2}..{p90:.2} (p10..p90), more than \
             {MAX_RATIO_SPREAD}x: the runner's speed changed"
        ));
    }
    let (slowest, fastest) = SPEED_BOUNDS;
    if !(slowest..=fastest).contains(&speed) {
        return Err(format!(
            "shard {shard}'s speed {speed:.2} is outside {slowest:.2}..{fastest:.2}"
        ));
    }
    Ok(speed)
}

fn median(values: &mut [f64]) -> f64 {
    values.sort_by(f64::total_cmp);
    let mid = values.len() / 2;
    if values.len().is_multiple_of(2) {
        (values[mid - 1] + values[mid]) / 2.0
    } else {
        values[mid]
    }
}

/// Nearest-rank percentile of sorted `values`.
fn percentile(values: &[f64], fraction: f64) -> f64 {
    let rank = (fraction * values.len() as f64).ceil() as usize;
    values[rank.clamp(1, values.len()) - 1]
}

#[cfg(test)]
mod tests {
    use super::*;

    fn shard(shard: u32, ratios: &[f64]) -> (BTreeMap<String, OmcTiming>, BTreeMap<String, f64>) {
        let mut now = BTreeMap::new();
        let mut reference = BTreeMap::new();
        for (i, ratio) in ratios.iter().enumerate() {
            let model = format!("S{shard}M{i}");
            reference.insert(model.clone(), 10.0);
            now.insert(
                model,
                OmcTiming {
                    shard,
                    wall_seconds: 10.0 * ratio,
                    fresh: true,
                },
            );
        }
        (now, reference)
    }

    #[test]
    fn a_slow_shard_reads_its_own_speed_leaving_the_model_out() {
        // Shard 3 of run 46055840822: OMC ran about 2.46x its reference.
        let ratios = (0..40)
            .map(|i| 2.40 + 0.003 * f64::from(i))
            .collect::<Vec<_>>();
        let (now, reference) = shard(3, &ratios);
        let gauge = SpeedGauge::new(&now, &reference);
        let speed = gauge.speed_for("CauerLowPassSC", 3).unwrap();
        assert!((speed - 2.4585).abs() < 1e-9, "{speed}");
        // CauerLowPassSC's 42.9 s run reads 17.4 s in the frame.
        assert!((42.9 / speed - 17.45).abs() < 0.01);
        // Its own timing never enters its gauge.
        let own = gauge.speed_for("S3M39", 3).unwrap();
        assert!(own < speed);
    }

    #[test]
    fn a_gauge_it_cannot_trust_is_inconclusive() {
        let (now, reference) = shard(1, &[1.0; 30]);
        let gauge = SpeedGauge::new(&now, &reference);
        // 30 timings, one of them the model's own: 29 left.
        assert!(
            gauge
                .speed_for("S1M0", 1)
                .unwrap_err()
                .contains("fewer than 30")
        );
        assert_eq!(gauge.speed_for("Other", 1), Ok(1.0));
        // Cached or resumed timings do not count.
        let mut stale = now.clone();
        stale.values_mut().for_each(|timing| timing.fresh = false);
        assert!(
            SpeedGauge::new(&stale, &reference)
                .speed_for("Other", 1)
                .is_err()
        );
        // A runner that changed speed mid-shard.
        let mixed = (0..40)
            .map(|i| if i < 20 { 1.0 } else { 2.0 })
            .collect::<Vec<_>>();
        let (now, reference) = shard(2, &mixed);
        let error = SpeedGauge::new(&now, &reference)
            .speed_for("Other", 2)
            .unwrap_err();
        assert!(error.contains("spread"), "{error}");
        // A speed outside the bounds.
        let (now, reference) = shard(4, &[3.5; 40]);
        let error = SpeedGauge::new(&now, &reference)
            .speed_for("Other", 4)
            .unwrap_err();
        assert!(error.contains("outside"), "{error}");
        // No timings at all.
        assert!(SpeedGauge::default().speed_for("Other", 1).is_err());
    }

    #[test]
    fn the_frame_is_the_median_of_the_trusted_shards() {
        let mut now = BTreeMap::new();
        let mut reference = BTreeMap::new();
        for (index, speed) in [(1, 1.84), (2, 0.39), (3, 2.46), (4, 0.40)] {
            let (n, r) = shard(index, &[speed; 40]);
            now.extend(n);
            reference.extend(r);
        }
        let nominal = SpeedGauge::new(&now, &reference).nominal_speed().unwrap();
        assert!((nominal - 1.12).abs() < 1e-9, "{nominal}");
    }
}
