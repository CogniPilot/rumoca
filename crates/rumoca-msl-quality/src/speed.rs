//! Rule B's speed gauge: a shard's runner speed, read from OMC.
//!
//! CI runners come in speed classes about 2.3x apart, so a completion time
//! judged against an absolute budget certifies by runner lottery. OMC is
//! pinned and does the same work in every run, so a fixed **calibration set**
//! of models, OMC-timed on every shard, measures each shard's runner against
//! one common frame: for a judged model `m` on shard `s`,
//!
//! `k_s(m) = median over calibration models j != m timed on s of now_s(j) / ref(j)`
//!
//! where `ref(j)` is the model's wall time in the frame, fixed when the frame
//! was set (the median of the shards of the run that set it). Every shard
//! times the same models, so shard speeds compare directly, and no shard
//! inherits the runner an earlier run happened to draw for it.
//!
//! Rumoca's stage times are divided by `k` before the margin is applied, so a
//! slow runner does not uncertify a model and a fast one does not certify a
//! near-budget model on luck; a genuine rumoca slowdown still reads in full.
//! A gauge it cannot trust (too few fresh timings, a speed outside the
//! bounds, or a spread that says the runner changed speed mid-shard) is
//! inconclusive, never an absolute judgement.

use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet};

/// The calibration set: models whose OMC wall is a few seconds and mostly
/// simulation (not process start and C compile, which scale with the disk and
/// compiler rather than with simulation throughput), chosen on seven CI runs
/// where each one's ratio stayed within 15% of its shard's median, and on a
/// held-out run within 3.5% of every shard's median. About 285 s of OMC wall
/// per shard on a nominal runner.
pub const CALIBRATION_MODELS: &[&str] = &[
    "Modelica.Electrical.Analog.Examples.CauerLowPassSC",
    "Modelica.Electrical.Analog.Examples.CompareTransformers",
    "Modelica.Electrical.Analog.Examples.DemoPowerSupplyWithBuffer",
    "Modelica.Electrical.Batteries.Examples.BatteryDischargeCharge",
    "Modelica.Electrical.Batteries.Examples.SuperCapDischargeCharge",
    "Modelica.Electrical.Machines.Examples.ControlledDCDrives.CurrentControlledDCPM",
    "Modelica.Electrical.Machines.Examples.ControlledDCDrives.PositionControlledDCPM",
    "Modelica.Electrical.Machines.Examples.DCMachines.DCPM_CurrentControlled",
    "Modelica.Electrical.Machines.Examples.DCMachines.DCPM_Drive",
    "Modelica.Electrical.Machines.Examples.DCMachines.DCPM_QuasiStatic",
    "Modelica.Electrical.Machines.Examples.DCMachines.DCPM_Temperature",
    "Modelica.Electrical.Machines.Examples.DCMachines.DCPM_withLosses",
    "Modelica.Electrical.Machines.Examples.InductionMachines.IMC_DOL",
    "Modelica.Electrical.Machines.Examples.InductionMachines.IMC_Initialize",
    "Modelica.Electrical.Machines.Examples.InductionMachines.IMC_Inverter",
    "Modelica.Electrical.Machines.Examples.SynchronousMachines.SMEE_DOL",
    "Modelica.Electrical.Machines.Examples.SynchronousMachines.SMPM_CurrentSource",
    "Modelica.Electrical.Machines.Examples.SynchronousMachines.SMPM_Inverter",
    "Modelica.Electrical.Machines.Examples.SynchronousMachines.SMPM_VoltageSource",
    "Modelica.Electrical.Machines.Examples.SynchronousMachines.SMR_Inverter",
    "Modelica.Electrical.Polyphase.Examples.PolyphaseRectifier",
    "Modelica.Electrical.Polyphase.Examples.Rectifier",
    "Modelica.Electrical.PowerConverters.Examples.ACAC.Dimmer_R",
    "Modelica.Electrical.PowerConverters.Examples.ACDC.Rectifier1Pulse.Thyristor1Pulse_R_Characteristic",
    "Modelica.Electrical.PowerConverters.Examples.ACDC.RectifierCenterTap2Pulse.ThyristorCenterTap2Pulse_RLV_Characteristic",
    "Modelica.Electrical.PowerConverters.Examples.ACDC.RectifierCenterTap2mPulse.ThyristorCenterTap2mPulse_RLV_Characteristic",
    "Modelica.Electrical.PowerConverters.Examples.ACDC.RectifierCenterTapmPulse.ThyristorCenterTapmPulse_RLV_Characteristic",
    "Modelica.Electrical.PowerConverters.Examples.DCDC.ChopperBuckBoost.ChopperBuckBoost_DutyCycle",
    "Modelica.Magnetic.FundamentalWave.Examples.BasicMachines.InductionMachines.IMC_Inverter",
    "Modelica.Magnetic.FundamentalWave.Examples.BasicMachines.InductionMachines.IMC_Steinmetz",
    "Modelica.Magnetic.FundamentalWave.Examples.BasicMachines.InductionMachines.IMC_YD",
    "Modelica.Magnetic.FundamentalWave.Examples.BasicMachines.InductionMachines.IMS_Start",
    "Modelica.Magnetic.FundamentalWave.Examples.BasicMachines.SynchronousMachines.SMEE_DOL",
    "Modelica.Magnetic.FundamentalWave.Examples.BasicMachines.SynchronousMachines.SMPM_CurrentSource",
    "Modelica.Magnetic.FundamentalWave.Examples.BasicMachines.SynchronousMachines.SMR_Inverter",
    "Modelica.Thermal.HeatTransfer.Examples.Motor",
];

/// The quality gate version that introduced the frame: a baseline older than
/// it has no calibration references, and only then does a run set the frame.
/// A baseline at or after it without them is refused, so a lost field can
/// never re-set the frame on whichever runner ran today.
pub const FRAME_GATE_VERSION: u64 = 15;

/// The fewest fresh same-shard OMC timings a gauge is read from. With 36
/// calibration models, seven failed calibration timings on one shard make its
/// gauge inconclusive (fail closed, re-run the shard job).
pub const MIN_FRESH_TIMINGS: usize = 30;
/// The slowest and fastest runner a gauge is trusted for, relative to the frame.
pub const SPEED_BOUNDS: (f64, f64) = (1.0 / 3.0, 3.0);
/// The widest p90/p10 spread of a shard's OMC ratios a gauge is trusted for.
pub const MAX_RATIO_SPREAD: f64 = 1.5;

/// One calibration model's OMC timing on one shard in this run.
#[derive(Debug, Clone, PartialEq)]
pub struct OmcTiming {
    pub shard: u32,
    pub model: String,
    pub wall_seconds: f64,
    /// Taken in this run on this shard's runner (not cached or resumed).
    pub fresh: bool,
}

fn usable(seconds: f64) -> bool {
    seconds.is_finite() && seconds > 0.0
}

/// The frame a run sets when its baseline predates it: each calibration
/// model's median fresh wall over the shards, so the frame's nominal runner is
/// the run's median one, not any one shard's. Every pinned model must have
/// been timed fresh on every shard, or no frame is set: a frame missing a
/// model would hold less headroom over [`MIN_FRESH_TIMINGS`] forever.
pub fn bootstrap_references(timings: &[OmcTiming]) -> Result<BTreeMap<String, f64>, String> {
    let shards = timings
        .iter()
        .map(|timing| timing.shard)
        .collect::<BTreeSet<_>>();
    if shards.is_empty() {
        return Err("no calibration timings to set the frame from".to_string());
    }
    let mut walls: BTreeMap<&str, Vec<f64>> = BTreeMap::new();
    let mut missing = Vec::new();
    for &shard in &shards {
        for &model in CALIBRATION_MODELS {
            match timings.iter().find(|timing| {
                timing.shard == shard
                    && timing.model == model
                    && timing.fresh
                    && usable(timing.wall_seconds)
            }) {
                Some(timing) => walls.entry(model).or_default().push(timing.wall_seconds),
                None => missing.push(format!("{model} on shard {shard}")),
            }
        }
    }
    if !missing.is_empty() {
        return Err(format!(
            "cannot set the frame: {} calibration timings are missing or stale ({}{})",
            missing.len(),
            missing
                .iter()
                .take(3)
                .cloned()
                .collect::<Vec<_>>()
                .join(", "),
            if missing.len() > 3 { ", ..." } else { "" }
        ));
    }
    Ok(walls
        .into_iter()
        .map(|(model, mut walls)| (model.to_string(), median(&mut walls)))
        .collect())
}

/// Stored references must be exactly the pinned calibration set, each a
/// usable wall: changing the set is a reviewed frame migration, not drift.
pub fn validate_references(references: &BTreeMap<String, f64>) -> Result<(), String> {
    let pinned = CALIBRATION_MODELS.iter().copied().collect::<BTreeSet<_>>();
    let stored = references
        .keys()
        .map(String::as_str)
        .collect::<BTreeSet<_>>();
    let unpinned = stored.difference(&pinned).count();
    let unstored = pinned.difference(&stored).count();
    if unpinned + unstored > 0 {
        return Err(format!(
            "the stored calibration references are not the pinned set ({unpinned} not pinned, \
             {unstored} missing); changing the set is a reviewed frame migration"
        ));
    }
    match references.iter().find(|(_, wall)| !usable(**wall)) {
        Some((model, wall)) => Err(format!(
            "calibration reference {model} = {wall} is unusable"
        )),
        None => Ok(()),
    }
}

/// This run's calibration timings against the frame's references.
#[derive(Debug, Clone, Default)]
pub struct SpeedGauge {
    /// Per shard: each fresh calibration model's ratio of its wall now to its
    /// reference.
    ratios: BTreeMap<u32, Vec<(String, f64)>>,
}

impl SpeedGauge {
    /// The gauge of `timings` against `reference` (model -> reference wall).
    pub fn new(timings: &[OmcTiming], reference: &BTreeMap<String, f64>) -> Self {
        let mut ratios: BTreeMap<u32, Vec<(String, f64)>> = BTreeMap::new();
        for timing in timings {
            let Some(&reference) = reference.get(&timing.model) else {
                continue;
            };
            if timing.fresh && usable(timing.wall_seconds) && usable(reference) {
                ratios
                    .entry(timing.shard)
                    .or_default()
                    .push((timing.model.clone(), timing.wall_seconds / reference));
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
}

/// The calibration timings of a (merged) reference file: a merged file's
/// `calibration_by_shard`, or an unsharded run's own section as shard 0.
pub fn calibration_timings(reference: &Value) -> Vec<OmcTiming> {
    let own = reference
        .get("calibration_timings")
        .filter(|section| !section.is_null());
    let sections: Vec<(u32, &Value, bool)> = match reference
        .get("calibration_by_shard")
        .and_then(Value::as_object)
    {
        Some(by_shard) => by_shard
            .iter()
            .filter_map(|(shard, section)| {
                let fresh = section.get("fresh").and_then(Value::as_bool);
                Some((shard.parse().ok()?, section, fresh.unwrap_or(false)))
            })
            .collect(),
        None => own
            .map(|section| {
                let fresh = timing_run_of(section)
                    == reference.get("timing_run").cloned().unwrap_or_default();
                vec![(0, section, fresh)]
            })
            .unwrap_or_default(),
    };
    sections
        .into_iter()
        .flat_map(|(shard, section, fresh)| {
            section
                .get("models")
                .and_then(Value::as_object)
                .into_iter()
                .flatten()
                .filter(|(_, entry)| entry.get("status").and_then(Value::as_str) == Some("success"))
                .filter_map(move |(model, entry)| {
                    Some(OmcTiming {
                        shard,
                        model: model.clone(),
                        wall_seconds: entry.get("omc_wall_seconds")?.as_f64()?,
                        fresh,
                    })
                })
        })
        .collect()
}

/// The run provenance a timing context carries (null when none), from an OMC
/// model entry or a calibration section.
pub fn timing_run_of(entry: &Value) -> Value {
    entry
        .get("omc_timing_context")
        .or_else(|| entry.get("context"))
        .and_then(|context| context.get("run"))
        .cloned()
        .unwrap_or_default()
}

/// The calibration references the baseline stores, if any.
fn stored_references(baseline: &Value) -> Result<Option<BTreeMap<String, f64>>, String> {
    baseline
        .get("omc_calibration_references")
        .map(|references| {
            serde_json::from_value(references.clone())
                .map_err(|error| format!("omc_calibration_references: {error}"))
        })
        .transpose()
}

/// The frame this run is judged in: the baseline's stored references, which
/// must be the pinned set, or, only when the baseline predates the frame, the
/// frame this run sets (`Err` inside when it cannot set one: certification is
/// then inconclusive). A baseline at or after the frame's gate version without
/// references is refused.
pub fn frame_references(
    baseline: &Value,
    timings: &[OmcTiming],
) -> Result<Result<BTreeMap<String, f64>, String>, String> {
    if let Some(references) = stored_references(baseline)? {
        validate_references(&references)?;
        return Ok(Ok(references));
    }
    match baseline.get("quality_gate_version").and_then(Value::as_u64) {
        Some(version) if version < FRAME_GATE_VERSION => Ok(bootstrap_references(timings)),
        Some(version) => Err(format!(
            "the baseline (quality gate version {version}) has no omc_calibration_references; \
             a baseline from version {FRAME_GATE_VERSION} on must carry its frame"
        )),
        None => Err(
            "the baseline has no quality_gate_version to tell whether it predates the frame"
                .to_string(),
        ),
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

    /// The calibration set timed on `shard` at `speed` times `wall`.
    fn shard(shard: u32, speed: f64, wall: impl Fn(usize) -> f64) -> Vec<OmcTiming> {
        CALIBRATION_MODELS
            .iter()
            .enumerate()
            .map(|(i, model)| OmcTiming {
                shard,
                model: (*model).to_string(),
                wall_seconds: wall(i) * speed,
                fresh: true,
            })
            .collect()
    }

    fn walls(i: usize) -> f64 {
        2.0 + 0.25 * i as f64
    }

    #[test]
    fn the_calibration_set_is_large_enough_and_unique() {
        assert!(CALIBRATION_MODELS.len() >= MIN_FRESH_TIMINGS + 6);
        let mut sorted = CALIBRATION_MODELS.to_vec();
        sorted.dedup();
        assert_eq!(sorted.len(), CALIBRATION_MODELS.len());
    }

    #[test]
    fn two_runners_2_3x_apart_give_the_same_normalized_time_for_the_same_work() {
        // Shard 1 on a nominal runner, shard 2 on one 2.3x slower; the frame is
        // set by this run (no stored references).
        let mut timings = shard(1, 1.0, walls);
        timings.extend(shard(2, 2.3, walls));
        let reference = bootstrap_references(&timings).unwrap();
        let gauge = SpeedGauge::new(&timings, &reference);
        let (fast, slow) = (
            gauge.speed_for("Judged", 1).unwrap(),
            gauge.speed_for("Judged", 2).unwrap(),
        );
        assert!((slow / fast - 2.3).abs() < 1e-9, "{fast} {slow}");
        // The same rumoca work: 20 s on the nominal runner, 46 s on the slow one.
        assert!((20.0 / fast - 46.0 / slow).abs() < 1e-9);
        // The frame is the run's median runner: 1.65x nominal here.
        assert!((fast - 1.0 / 1.65).abs() < 1e-9, "{fast}");
    }

    #[test]
    fn stored_references_fix_the_frame_for_later_runs() {
        // References set by an earlier run; this run's shard 3 is 2.46x slower.
        let reference = bootstrap_references(&shard(1, 1.0, walls)).unwrap();
        let timings = shard(3, 2.46, walls);
        let gauge = SpeedGauge::new(&timings, &reference);
        let speed = gauge.speed_for("CauerLowPassSC", 3).unwrap();
        assert!((speed - 2.46).abs() < 1e-9, "{speed}");
        // A calibration model never enters its own gauge.
        let own = CALIBRATION_MODELS[0];
        assert!(gauge.speed_for(own, 3).is_ok());
    }

    #[test]
    fn a_gauge_it_cannot_trust_is_inconclusive() {
        let reference = bootstrap_references(&shard(1, 1.0, walls)).unwrap();
        // Too few fresh timings: all but 29 cached.
        let mut stale = shard(1, 1.0, walls);
        stale
            .iter_mut()
            .skip(29)
            .for_each(|timing| timing.fresh = false);
        let error = SpeedGauge::new(&stale, &reference)
            .speed_for("Judged", 1)
            .unwrap_err();
        assert!(error.contains("fewer than 30"), "{error}");
        // A runner that changed speed mid-shard.
        let mut mixed = shard(2, 1.0, walls);
        mixed
            .iter_mut()
            .skip(18)
            .for_each(|timing| timing.wall_seconds *= 2.0);
        let error = SpeedGauge::new(&mixed, &reference)
            .speed_for("Judged", 2)
            .unwrap_err();
        assert!(error.contains("spread"), "{error}");
        // A speed outside the bounds.
        let error = SpeedGauge::new(&shard(4, 3.5, walls), &reference)
            .speed_for("Judged", 4)
            .unwrap_err();
        assert!(error.contains("outside"), "{error}");
        // A shard without calibration timings.
        assert!(
            SpeedGauge::new(&[], &reference)
                .speed_for("Judged", 1)
                .is_err()
        );
    }

    #[test]
    fn a_frame_is_set_only_from_every_pinned_model_on_every_shard() {
        let mut timings = shard(1, 1.0, walls);
        timings.extend(shard(2, 2.0, walls));
        assert!(bootstrap_references(&timings).is_ok());
        // One pinned model failed on shard 2: no frame.
        let model = CALIBRATION_MODELS[5];
        let partial = timings
            .iter()
            .filter(|timing| !(timing.shard == 2 && timing.model == model))
            .cloned()
            .collect::<Vec<_>>();
        let error = bootstrap_references(&partial).unwrap_err();
        assert!(
            error.contains("1 calibration timings are missing") && error.contains("shard 2"),
            "{error}"
        );
        assert!(bootstrap_references(&[]).is_err());
    }

    #[test]
    fn stored_references_must_be_the_pinned_set() {
        let references = bootstrap_references(&shard(1, 1.0, walls)).unwrap();
        assert_eq!(validate_references(&references), Ok(()));
        let mut short = references.clone();
        short.remove(CALIBRATION_MODELS[0]);
        assert!(
            validate_references(&short)
                .unwrap_err()
                .contains("1 missing")
        );
        let mut extra = references.clone();
        extra.insert("Unpinned".to_string(), 3.0);
        assert!(
            validate_references(&extra)
                .unwrap_err()
                .contains("1 not pinned")
        );
        let mut bad = references;
        bad.insert(CALIBRATION_MODELS[0].to_string(), 0.0);
        assert!(validate_references(&bad).unwrap_err().contains("unusable"));
    }
}
