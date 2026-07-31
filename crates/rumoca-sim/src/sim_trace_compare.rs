mod normalization;
#[cfg(test)]
mod tests;

use normalization::{
    ReferenceScale, array_element_base, range_is_degenerate, reference_scale,
    robust_reference_percentiles, sample_range,
};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::path::Path;

const GRID_DEDUP_EPS: f64 = 1.0e-12;
const THRESHOLD_COMPARE_EPS: f64 = 1.0e-12;
const RANGE_LOW_QUANTILE: f64 = 0.05;
const RANGE_HIGH_QUANTILE: f64 = 0.95;
const NORMALIZATION_SCALE_EPS: f64 = 1.0e-12;
pub const HIGH_AGREEMENT_CHANNEL_THRESHOLD: f64 = 0.05;
pub const MINOR_AGREEMENT_CHANNEL_THRESHOLD: f64 = 0.20;
pub const MODEL_HIGH_MIN_HIGH_CHANNEL_SHARE: f64 = 0.80;
pub const MODEL_HIGH_MAX_DEVIATION_CHANNEL_SHARE: f64 = 0.01;
pub const MODEL_MINOR_MIN_HIGH_PLUS_MINOR_CHANNEL_SHARE: f64 = 0.90;
pub const MODEL_MINOR_MAX_DEVIATION_CHANNEL_SHARE: f64 = 0.10;
pub const HIGH_AGREEMENT_MAX_CHANNEL_THRESHOLD: f64 = 0.05;
pub const HIGH_AGREEMENT_MEAN_CHANNEL_THRESHOLD: f64 = 0.01;
pub const MINOR_AGREEMENT_MAX_CHANNEL_THRESHOLD: f64 = 0.20;
pub const MINOR_AGREEMENT_MEAN_CHANNEL_THRESHOLD: f64 = 0.05;
/// Level agreement tolerance for the event-timing predicate, as a fraction of
/// the channel's own normalization scale.
const EVENT_MISMATCH_LEVEL_TOLERANCE_FRACTION: f64 = 0.02;
/// Largest share of the compared horizon a timing-shifted step-hold channel may
/// spend disagreeing before the deviation is treated as a value disagreement.
const EVENT_MISMATCH_MAX_DISAGREEMENT_SHARE: f64 = 0.50;
/// Share of the disagreeing samples whose value must be a level the other trace
/// also reaches for the deviation to be a shift rather than a wrong value.
const EVENT_MISMATCH_MIN_LEVEL_MATCH_SHARE: f64 = 0.90;
pub const BAD_CHANNEL_MAX_THRESHOLD: f64 = 0.20;
pub const SEVERE_CHANNEL_MAX_THRESHOLD: f64 = 0.80;

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct SimTrace {
    #[serde(default)]
    pub model_name: Option<String>,
    pub times: Vec<f64>,
    pub names: Vec<String>,
    pub data: Vec<Vec<Option<f64>>>,
    #[serde(default)]
    pub variable_meta: Option<Vec<SimTraceVariableMeta>>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct SimTraceVariableMeta {
    pub name: String,
    #[serde(default)]
    pub role: Option<String>,
    #[serde(default)]
    pub value_type: Option<String>,
    #[serde(default)]
    pub variability: Option<String>,
    #[serde(default)]
    pub time_domain: Option<String>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ChannelDeviationMetric {
    pub name: String,
    #[serde(default)]
    pub shape: TraceDeviationShape,
    pub samples: usize,
    pub integral_duration: f64,
    pub integral_abs_error: f64,
    pub mean_abs_error: f64,
    pub normalization_scale: f64,
    /// Robust (p95 - p05) spread of the reference channel before flooring.
    #[serde(default)]
    pub reference_range: f64,
    /// Robust `max(|p05|, |p95|)` magnitude of the reference channel.
    #[serde(default)]
    pub reference_magnitude: f64,
    /// Sibling-derived scale of the array this channel belongs to, recorded
    /// whenever one was available so triage can see why an information-free
    /// channel was normalized the way it was.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub reference_array_group_floor: Option<f64>,
    pub normalized_l1_error: f64,
    pub bounded_normalized_l1_error: f64,
    pub normalized_max_abs_error: f64,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub initial_abs_error: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub initial_bounded_normalized_error: Option<f64>,
}

#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct InitialConditionStats {
    pub channels_compared: usize,
    pub high_count: usize,
    pub minor_count: usize,
    pub deviation_count: usize,
    pub severe_count: usize,
    pub high_percent: f64,
    pub minor_percent: f64,
    pub deviation_percent: f64,
    pub severe_percent: f64,
    pub violation_mass_total: f64,
    pub violation_mass_mean_per_channel: f64,
    pub mean_channel_bounded_normalized_error: f64,
    pub max_channel_bounded_normalized_error: f64,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum TraceDeviationShape {
    #[default]
    Unknown,
    WithinTolerance,
    ConstantOffset,
    WrongInitialValueOnly,
    SignInversion,
    ScaleError,
    PhaseTimeShift,
    EventTimeMismatch,
    MonotonicDrift,
    StepSizeIntegrationError,
    MissingOrWrongChannelMapping,
}

impl TraceDeviationShape {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Unknown => "unknown",
            Self::WithinTolerance => "within_tolerance",
            Self::ConstantOffset => "constant_offset",
            Self::WrongInitialValueOnly => "wrong_initial_value_only",
            Self::SignInversion => "sign_inversion",
            Self::ScaleError => "scale_error",
            Self::PhaseTimeShift => "phase_time_shift",
            Self::EventTimeMismatch => "event_time_mismatch",
            Self::MonotonicDrift => "monotonic_drift",
            Self::StepSizeIntegrationError => "step_size_integration_error",
            Self::MissingOrWrongChannelMapping => "missing_or_wrong_channel_mapping",
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ModelDeviationMetric {
    pub model_name: String,
    pub compared_variables: usize,
    pub samples_compared: usize,
    pub bounded_normalized_l1_score: f64,
    pub mean_channel_bounded_normalized_l1: f64,
    pub max_channel_bounded_normalized_l1: f64,
    #[serde(default)]
    pub channel_high_count: usize,
    #[serde(default)]
    pub channel_minor_count: usize,
    #[serde(default)]
    pub channel_deviation_count: usize,
    #[serde(default)]
    pub channel_severe_count: usize,
    #[serde(default)]
    pub channel_high_percent: f64,
    #[serde(default)]
    pub channel_minor_percent: f64,
    #[serde(default)]
    pub channel_deviation_percent: f64,
    #[serde(default)]
    pub channel_severe_percent: f64,
    #[serde(default)]
    pub channel_violation_mass: f64,
    #[serde(default)]
    pub initial_condition: InitialConditionStats,
    pub worst_variables: Vec<ChannelDeviationMetric>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum AgreementBand {
    HighAgreement,
    MinorAgreement,
    Deviation,
}

#[derive(Debug, Clone, Copy, Default, Deserialize, Serialize)]
pub struct AgreementCounts {
    pub high_agreement: usize,
    pub minor_agreement: usize,
    pub deviation: usize,
}

#[derive(Debug, thiserror::Error)]
pub enum TraceCompareError {
    #[error("failed to read trace JSON '{path}': {source}")]
    Read {
        path: String,
        #[source]
        source: std::io::Error,
    },
    #[error("failed to parse trace JSON '{path}': {source}")]
    Parse {
        path: String,
        #[source]
        source: serde_json::Error,
    },
    #[error("trace has no valid time samples")]
    MissingTimes,
    #[error("trace has no common variables")]
    NoCommonVariables,
    #[error("trace has no comparable variable samples")]
    NoComparableSamples,
}

pub fn load_trace_json(path: &Path) -> Result<SimTrace, TraceCompareError> {
    let payload = std::fs::read_to_string(path).map_err(|source| TraceCompareError::Read {
        path: path.display().to_string(),
        source,
    })?;
    let mut trace: SimTrace =
        serde_json::from_str(&payload).map_err(|source| TraceCompareError::Parse {
            path: path.display().to_string(),
            source,
        })?;
    normalize_trace(&mut trace);
    Ok(trace)
}

pub fn compare_trace_files(
    model_name: &str,
    rumoca_path: &Path,
    omc_path: &Path,
) -> Result<ModelDeviationMetric, TraceCompareError> {
    let rumoca = load_trace_json(rumoca_path)?;
    let omc = load_trace_json(omc_path)?;
    compare_model_traces(model_name, &rumoca, &omc)
}

pub fn compare_model_traces(
    model_name: &str,
    rumoca: &SimTrace,
    omc: &SimTrace,
) -> Result<ModelDeviationMetric, TraceCompareError> {
    if rumoca.times.is_empty() || omc.times.is_empty() {
        return Err(TraceCompareError::MissingTimes);
    }

    let mut channels = compare_common_channels(rumoca, omc)?;
    channels.sort_by(|a, b| {
        b.bounded_normalized_l1_error
            .partial_cmp(&a.bounded_normalized_l1_error)
            .unwrap_or(std::cmp::Ordering::Equal)
    });

    let compared_variables = channels.len();
    let samples_compared = channels.iter().map(|m| m.samples).sum::<usize>();
    let mean_channel_bounded_l1 = channels
        .iter()
        .map(|m| m.bounded_normalized_l1_error)
        .sum::<f64>()
        / compared_variables as f64;
    let max_channel_bounded_l1 = channels
        .iter()
        .map(|m| m.bounded_normalized_l1_error)
        .fold(0.0_f64, f64::max);
    let mut channel_scores = channels
        .iter()
        .map(|m| m.bounded_normalized_l1_error)
        .collect::<Vec<_>>();
    channel_scores.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    let bounded_normalized_l1_score = median_of_sorted(&channel_scores).unwrap_or(0.0);
    let channel_counts = count_channel_agreement_bands_default(
        channels
            .iter()
            .map(|channel| channel.bounded_normalized_l1_error),
    );
    let channel_severe_count = channels
        .iter()
        .filter(|channel| channel.bounded_normalized_l1_error >= SEVERE_CHANNEL_MAX_THRESHOLD)
        .count();
    let channel_violation_mass = channels
        .iter()
        .map(|channel| (channel.bounded_normalized_l1_error - BAD_CHANNEL_MAX_THRESHOLD).max(0.0))
        .sum::<f64>();
    let initial_condition = initial_condition_stats(&channels);
    let channel_total = compared_variables.max(1) as f64;
    let worst_variables = channels.into_iter().take(10).collect();

    Ok(ModelDeviationMetric {
        model_name: model_name.to_string(),
        compared_variables,
        samples_compared,
        bounded_normalized_l1_score,
        mean_channel_bounded_normalized_l1: mean_channel_bounded_l1,
        max_channel_bounded_normalized_l1: max_channel_bounded_l1,
        channel_high_count: channel_counts.high_agreement,
        channel_minor_count: channel_counts.minor_agreement,
        channel_deviation_count: channel_counts.deviation,
        channel_severe_count,
        channel_high_percent: channel_counts.high_agreement as f64 / channel_total,
        channel_minor_percent: channel_counts.minor_agreement as f64 / channel_total,
        channel_deviation_percent: channel_counts.deviation as f64 / channel_total,
        channel_severe_percent: channel_severe_count as f64 / channel_total,
        channel_violation_mass,
        initial_condition,
        worst_variables,
    })
}

/// Per-channel metrics for every variable the two traces have in common.
fn compare_common_channels(
    rumoca: &SimTrace,
    omc: &SimTrace,
) -> Result<Vec<ChannelDeviationMetric>, TraceCompareError> {
    let rumoca_series = series_map(rumoca);
    let omc_series = series_map(omc);
    let rumoca_discrete_channels = discrete_channel_names(rumoca);
    let omc_discrete_channels = discrete_channel_names(omc);
    let rumoca_names: HashSet<String> = rumoca_series.keys().cloned().collect();
    let omc_names: HashSet<String> = omc_series.keys().cloned().collect();
    let common: HashSet<String> = rumoca_names.intersection(&omc_names).cloned().collect();
    if common.is_empty() {
        return Err(TraceCompareError::NoCommonVariables);
    }

    let array_group_floors = reference_array_group_floors(
        &omc.times,
        &omc_series,
        &common,
        &rumoca_discrete_channels,
        &omc_discrete_channels,
        comparison_window(&rumoca.times, &omc.times),
    );

    let channels: Vec<ChannelDeviationMetric> = common
        .into_iter()
        .filter_map(|name| {
            let is_discrete_channel =
                rumoca_discrete_channels.contains(&name) || omc_discrete_channels.contains(&name);
            let array_group_floor = if is_discrete_channel {
                None
            } else {
                array_element_base(&name)
                    .and_then(|base| array_group_floors.get(base))
                    .copied()
            };
            compare_channel(
                &name,
                ChannelSeries::new(&rumoca.times, rumoca_series.get(&name)?),
                ChannelSeries::new(&omc.times, omc_series.get(&name)?),
                is_discrete_channel,
                array_group_floor,
            )
        })
        .collect();
    if channels.is_empty() {
        return Err(TraceCompareError::NoComparableSamples);
    }
    Ok(channels)
}

/// Time span over which the two traces can be compared at all.
fn comparison_window(rumoca_times: &[f64], omc_times: &[f64]) -> Option<(f64, f64)> {
    let start = rumoca_times.first()?.max(*omc_times.first()?);
    let end = rumoca_times.last()?.min(*omc_times.last()?);
    (end > start).then_some((start, end))
}

/// Smallest usable robust reference range among the elements of each
/// array-valued reference quantity.
///
/// This is measured on the *reference* trace alone — never on our own output —
/// so the normalization of an information-free channel stays a property of what
/// we are being compared against. See the `normalization` module docs for why
/// the sibling elements of an array are the right estimate and why the smallest
/// of them is the conservative choice.
///
/// Only elements that are themselves compared contribute, and only when their
/// range is non-degenerate — which is exactly what an information-free channel's
/// own range is not, so a channel cannot set its own floor. The floor is
/// therefore always at least `CONTINUOUS_ABSOLUTE_SCALE_FLOOR`, the value it
/// replaces.
fn reference_array_group_floors(
    omc_times: &[f64],
    omc_series: &HashMap<String, Vec<Option<f64>>>,
    names: &HashSet<String>,
    rumoca_discrete_channels: &HashSet<String>,
    omc_discrete_channels: &HashSet<String>,
    window: Option<(f64, f64)>,
) -> HashMap<String, f64> {
    let mut floors: HashMap<String, f64> = HashMap::new();
    let Some((start, end)) = window else {
        return floors;
    };
    for name in names {
        if rumoca_discrete_channels.contains(name) || omc_discrete_channels.contains(name) {
            continue;
        }
        let Some(base) = array_element_base(name) else {
            continue;
        };
        let Some(values) = omc_series.get(name) else {
            continue;
        };
        let samples = omc_times
            .iter()
            .zip(values.iter())
            .filter(|(time, _)| **time >= start && **time <= end)
            .filter_map(|(_, value)| value.filter(|value| value.is_finite()))
            .collect::<Vec<_>>();
        if samples.len() < 2 {
            continue;
        }
        let Some((p05, p95)) = robust_reference_percentiles(&samples) else {
            continue;
        };
        let range = (p95 - p05).abs();
        if range_is_degenerate(range) {
            continue;
        }
        floors
            .entry(base.to_string())
            .and_modify(|floor| *floor = floor.min(range))
            .or_insert(range);
    }
    floors
}

fn initial_condition_stats(channels: &[ChannelDeviationMetric]) -> InitialConditionStats {
    let errors = channels
        .iter()
        .filter_map(|channel| channel.initial_bounded_normalized_error)
        .filter(|value| value.is_finite())
        .collect::<Vec<_>>();
    let channels_compared = errors.len();
    if channels_compared == 0 {
        return InitialConditionStats::default();
    }

    let counts = count_channel_agreement_bands_default(errors.iter().copied());
    let severe_count = errors
        .iter()
        .filter(|error| **error >= SEVERE_CHANNEL_MAX_THRESHOLD)
        .count();
    let violation_mass_total = errors
        .iter()
        .map(|error| (*error - BAD_CHANNEL_MAX_THRESHOLD).max(0.0))
        .sum::<f64>();
    let total = channels_compared as f64;
    InitialConditionStats {
        channels_compared,
        high_count: counts.high_agreement,
        minor_count: counts.minor_agreement,
        deviation_count: counts.deviation,
        severe_count,
        high_percent: counts.high_agreement as f64 / total,
        minor_percent: counts.minor_agreement as f64 / total,
        deviation_percent: counts.deviation as f64 / total,
        severe_percent: severe_count as f64 / total,
        violation_mass_total,
        violation_mass_mean_per_channel: violation_mass_total / total,
        mean_channel_bounded_normalized_error: errors.iter().sum::<f64>() / total,
        max_channel_bounded_normalized_error: errors.into_iter().fold(0.0_f64, f64::max),
    }
}

pub fn classify_trace_score(
    score: f64,
    high_agreement_threshold: f64,
    minor_agreement_threshold: f64,
) -> AgreementBand {
    if score < high_agreement_threshold {
        return AgreementBand::HighAgreement;
    }
    if score < minor_agreement_threshold {
        return AgreementBand::MinorAgreement;
    }
    AgreementBand::Deviation
}

pub fn classify_trace_metric(
    metric: &ModelDeviationMetric,
    high_max_channel_threshold: f64,
    high_mean_channel_threshold: f64,
    minor_max_channel_threshold: f64,
    minor_mean_channel_threshold: f64,
) -> AgreementBand {
    if metric.max_channel_bounded_normalized_l1 <= high_max_channel_threshold
        && metric.mean_channel_bounded_normalized_l1 <= high_mean_channel_threshold
    {
        return AgreementBand::HighAgreement;
    }
    if metric.max_channel_bounded_normalized_l1 <= minor_max_channel_threshold
        && metric.mean_channel_bounded_normalized_l1 <= minor_mean_channel_threshold
    {
        return AgreementBand::MinorAgreement;
    }
    AgreementBand::Deviation
}

pub fn classify_channel_error(
    bounded_normalized_l1_error: f64,
    high_agreement_threshold: f64,
    minor_agreement_threshold: f64,
) -> AgreementBand {
    classify_trace_score(
        bounded_normalized_l1_error,
        high_agreement_threshold,
        minor_agreement_threshold,
    )
}

fn channel_share_triplet(metric: &ModelDeviationMetric) -> Option<(f64, f64, f64)> {
    let counted_total =
        metric.channel_high_count + metric.channel_minor_count + metric.channel_deviation_count;
    if counted_total > 0 {
        let total = counted_total as f64;
        return Some((
            metric.channel_high_count as f64 / total,
            metric.channel_minor_count as f64 / total,
            metric.channel_deviation_count as f64 / total,
        ));
    }
    let sum = metric.channel_high_percent
        + metric.channel_minor_percent
        + metric.channel_deviation_percent;
    if sum > 0.0 {
        return Some((
            metric.channel_high_percent / sum,
            metric.channel_minor_percent / sum,
            metric.channel_deviation_percent / sum,
        ));
    }
    None
}

fn channel_count_share_at_least(count: usize, total: usize, threshold: f64) -> bool {
    (count as f64) + THRESHOLD_COMPARE_EPS >= threshold * total as f64
}

fn channel_count_share_at_most(count: usize, total: usize, threshold: f64) -> bool {
    count as f64 <= threshold * total as f64 + THRESHOLD_COMPARE_EPS
}

pub fn classify_trace_metric_channel_distribution(
    metric: &ModelDeviationMetric,
    high_min_high_channel_share: f64,
    high_max_deviation_channel_share: f64,
    minor_min_high_plus_minor_channel_share: f64,
    minor_max_deviation_channel_share: f64,
) -> AgreementBand {
    let counted_total =
        metric.channel_high_count + metric.channel_minor_count + metric.channel_deviation_count;
    if counted_total > 0 {
        if channel_count_share_at_least(
            metric.channel_high_count,
            counted_total,
            high_min_high_channel_share,
        ) && channel_count_share_at_most(
            metric.channel_deviation_count,
            counted_total,
            high_max_deviation_channel_share,
        ) {
            return AgreementBand::HighAgreement;
        }
        if channel_count_share_at_least(
            metric.channel_high_count + metric.channel_minor_count,
            counted_total,
            minor_min_high_plus_minor_channel_share,
        ) && channel_count_share_at_most(
            metric.channel_deviation_count,
            counted_total,
            minor_max_deviation_channel_share,
        ) {
            return AgreementBand::MinorAgreement;
        }
        return AgreementBand::Deviation;
    }

    let Some((high_share, minor_share, deviation_share)) = channel_share_triplet(metric) else {
        return classify_trace_metric(
            metric,
            HIGH_AGREEMENT_MAX_CHANNEL_THRESHOLD,
            HIGH_AGREEMENT_MEAN_CHANNEL_THRESHOLD,
            MINOR_AGREEMENT_MAX_CHANNEL_THRESHOLD,
            MINOR_AGREEMENT_MEAN_CHANNEL_THRESHOLD,
        );
    };
    if high_share >= high_min_high_channel_share
        && deviation_share <= high_max_deviation_channel_share
    {
        return AgreementBand::HighAgreement;
    }
    if high_share + minor_share >= minor_min_high_plus_minor_channel_share
        && deviation_share <= minor_max_deviation_channel_share
    {
        return AgreementBand::MinorAgreement;
    }
    AgreementBand::Deviation
}

pub fn count_agreement_bands<'a>(
    metrics: impl IntoIterator<Item = &'a ModelDeviationMetric>,
    high_agreement_threshold: f64,
    minor_agreement_threshold: f64,
) -> AgreementCounts {
    let mut counts = AgreementCounts::default();
    for metric in metrics {
        match classify_trace_score(
            metric.bounded_normalized_l1_score,
            high_agreement_threshold,
            minor_agreement_threshold,
        ) {
            AgreementBand::HighAgreement => counts.high_agreement += 1,
            AgreementBand::MinorAgreement => counts.minor_agreement += 1,
            AgreementBand::Deviation => counts.deviation += 1,
        }
    }
    counts
}

pub fn count_agreement_bands_default<'a>(
    metrics: impl IntoIterator<Item = &'a ModelDeviationMetric>,
) -> AgreementCounts {
    let mut counts = AgreementCounts::default();
    for metric in metrics {
        match classify_trace_metric_channel_distribution(
            metric,
            MODEL_HIGH_MIN_HIGH_CHANNEL_SHARE,
            MODEL_HIGH_MAX_DEVIATION_CHANNEL_SHARE,
            MODEL_MINOR_MIN_HIGH_PLUS_MINOR_CHANNEL_SHARE,
            MODEL_MINOR_MAX_DEVIATION_CHANNEL_SHARE,
        ) {
            AgreementBand::HighAgreement => counts.high_agreement += 1,
            AgreementBand::MinorAgreement => counts.minor_agreement += 1,
            AgreementBand::Deviation => counts.deviation += 1,
        }
    }
    counts
}

pub fn count_channel_agreement_bands(
    channel_errors: impl IntoIterator<Item = f64>,
    high_agreement_threshold: f64,
    minor_agreement_threshold: f64,
) -> AgreementCounts {
    let mut counts = AgreementCounts::default();
    for channel_error in channel_errors {
        match classify_channel_error(
            channel_error,
            high_agreement_threshold,
            minor_agreement_threshold,
        ) {
            AgreementBand::HighAgreement => counts.high_agreement += 1,
            AgreementBand::MinorAgreement => counts.minor_agreement += 1,
            AgreementBand::Deviation => counts.deviation += 1,
        }
    }
    counts
}

pub fn count_channel_agreement_bands_default(
    channel_errors: impl IntoIterator<Item = f64>,
) -> AgreementCounts {
    count_channel_agreement_bands(
        channel_errors,
        HIGH_AGREEMENT_CHANNEL_THRESHOLD,
        MINOR_AGREEMENT_CHANNEL_THRESHOLD,
    )
}

fn normalize_trace(trace: &mut SimTrace) {
    for column in &mut trace.data {
        if column.len() < trace.times.len() {
            column.resize(trace.times.len(), None);
        } else if column.len() > trace.times.len() {
            column.truncate(trace.times.len());
        }
    }
    collapse_duplicate_timestamps(trace);
}

fn collapse_duplicate_timestamps(trace: &mut SimTrace) {
    if trace.times.len() < 2 {
        return;
    }

    let mut dedup_times: Vec<f64> = Vec::with_capacity(trace.times.len());
    let mut dedup_indices: Vec<usize> = Vec::with_capacity(trace.times.len());
    for (idx, &time) in trace.times.iter().enumerate() {
        if dedup_times
            .last()
            .is_some_and(|last| (time - *last).abs() <= GRID_DEDUP_EPS)
        {
            if let Some(last_time) = dedup_times.last_mut() {
                *last_time = time;
            }
            if let Some(last_idx) = dedup_indices.last_mut() {
                *last_idx = idx;
            }
        } else {
            dedup_times.push(time);
            dedup_indices.push(idx);
        }
    }

    if dedup_times.len() == trace.times.len() {
        return;
    }

    trace.times = dedup_times;
    for column in &mut trace.data {
        let mut dedup_column = Vec::with_capacity(dedup_indices.len());
        for &idx in &dedup_indices {
            dedup_column.push(column.get(idx).copied().unwrap_or(None));
        }
        *column = dedup_column;
    }
}

fn series_map(trace: &SimTrace) -> HashMap<String, Vec<Option<f64>>> {
    let mut out = HashMap::new();
    for (idx, name) in trace.names.iter().enumerate() {
        let mut values = trace.data.get(idx).cloned().unwrap_or_default();
        if values.len() < trace.times.len() {
            values.resize(trace.times.len(), None);
        } else if values.len() > trace.times.len() {
            values.truncate(trace.times.len());
        }
        out.insert(name.clone(), values);
    }
    out
}

/// One tool's samples for a single channel: a time grid and the aligned values.
#[derive(Debug, Clone, Copy)]
pub(crate) struct ChannelSeries<'a> {
    pub times: &'a [f64],
    pub values: &'a [Option<f64>],
}

impl<'a> ChannelSeries<'a> {
    pub(crate) fn new(times: &'a [f64], values: &'a [Option<f64>]) -> Self {
        Self { times, values }
    }

    fn is_comparable(&self) -> bool {
        self.times.len() >= 2 && self.times.len() == self.values.len()
    }
}

/// Accumulated per-window error statistics for a single channel.
struct ChannelErrorAccumulator {
    ref_samples: Vec<f64>,
    paired_samples: Vec<(f64, f64, f64)>,
    integral_abs_error: f64,
    integral_duration: f64,
    max_abs_error: f64,
}

fn accumulate_channel_error(
    samples: &[(f64, Option<f64>, Option<f64>)],
) -> ChannelErrorAccumulator {
    let mut acc = ChannelErrorAccumulator {
        ref_samples: Vec::with_capacity(samples.len() * 2),
        paired_samples: Vec::with_capacity(samples.len() * 2),
        integral_abs_error: 0.0,
        integral_duration: 0.0,
        max_abs_error: 0.0,
    };
    for window in samples.windows(2) {
        let (t0, r0, o0) = window[0];
        let (t1, r1, o1) = window[1];
        let dt = t1 - t0;
        if dt <= 0.0 {
            continue;
        }
        let (Some(r0), Some(o0), Some(r1), Some(o1)) = (r0, o0, r1, o1) else {
            continue;
        };
        let e0 = (r0 - o0).abs();
        let e1 = (r1 - o1).abs();

        acc.integral_abs_error += 0.5 * (e0 + e1) * dt;
        acc.integral_duration += dt;
        acc.max_abs_error = acc.max_abs_error.max(e0).max(e1);
        acc.ref_samples.push(o0);
        acc.ref_samples.push(o1);
        acc.paired_samples.push((t0, r0, o0));
        acc.paired_samples.push((t1, r1, o1));
    }
    acc
}

fn compare_channel(
    name: &str,
    rumoca: ChannelSeries<'_>,
    omc: ChannelSeries<'_>,
    use_step_hold: bool,
    array_group_floor: Option<f64>,
) -> Option<ChannelDeviationMetric> {
    if !rumoca.is_comparable() || !omc.is_comparable() {
        return None;
    }

    let deduped_grid = channel_comparison_grid(rumoca.times, omc.times)?;
    if deduped_grid.len() < 2 {
        return None;
    }

    let samples = deduped_grid
        .iter()
        .map(|&t| {
            (
                t,
                interp_channel(rumoca.times, rumoca.values, t, use_step_hold),
                interp_channel(omc.times, omc.values, t, use_step_hold),
            )
        })
        .collect::<Vec<_>>();

    let acc = accumulate_channel_error(&samples);
    if acc.ref_samples.len() < 2 || acc.integral_duration <= 0.0 {
        return None;
    }

    let mean_abs_error = acc.integral_abs_error / acc.integral_duration;
    let scale = reference_scale(&acc.ref_samples, use_step_hold, array_group_floor);
    let normalization_scale = scale.normalization_scale;
    let normalized_l1_error = mean_abs_error / normalization_scale;
    let bounded_normalized_l1_error = normalized_l1_error / (1.0 + normalized_l1_error);
    let initial_abs_error = initial_abs_error(&samples);
    let initial_bounded_normalized_error = initial_abs_error.map(|error| {
        let normalized = error / normalization_scale;
        normalized / (1.0 + normalized)
    });
    let shape = classify_channel_deviation_shape(
        &acc.paired_samples,
        ChannelErrorSummary {
            mean_abs_error,
            max_abs_error: acc.max_abs_error,
            bounded_normalized_l1_error,
        },
        scale,
    );

    Some(ChannelDeviationMetric {
        name: name.to_string(),
        shape,
        samples: acc.ref_samples.len(),
        integral_duration: acc.integral_duration,
        integral_abs_error: acc.integral_abs_error,
        mean_abs_error,
        normalization_scale,
        reference_range: scale.range,
        reference_magnitude: scale.magnitude,
        reference_array_group_floor: scale.array_group_floor,
        normalized_l1_error,
        bounded_normalized_l1_error,
        normalized_max_abs_error: acc.max_abs_error / normalization_scale,
        initial_abs_error,
        initial_bounded_normalized_error,
    })
}

fn initial_abs_error(samples: &[(f64, Option<f64>, Option<f64>)]) -> Option<f64> {
    samples.iter().find_map(|(_, rumoca, omc)| {
        let (Some(rumoca), Some(omc)) = (*rumoca, *omc) else {
            return None;
        };
        let error = (rumoca - omc).abs();
        error.is_finite().then_some(error)
    })
}

fn channel_comparison_grid(rumoca_times: &[f64], omc_times: &[f64]) -> Option<Vec<f64>> {
    let overlap_start = rumoca_times[0].max(omc_times[0]);
    let overlap_end = rumoca_times[rumoca_times.len() - 1].min(omc_times[omc_times.len() - 1]);
    if overlap_end <= overlap_start {
        return None;
    }

    let mut grid = Vec::with_capacity(rumoca_times.len() + omc_times.len() + 2);
    grid.push(overlap_start);
    grid.push(overlap_end);
    grid.extend(
        rumoca_times
            .iter()
            .copied()
            .filter(|&t| t >= overlap_start && t <= overlap_end),
    );
    grid.extend(
        omc_times
            .iter()
            .copied()
            .filter(|&t| t >= overlap_start && t <= overlap_end),
    );
    grid.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    Some(dedup_grid_times(grid))
}

fn dedup_grid_times(grid: Vec<f64>) -> Vec<f64> {
    let mut deduped_grid: Vec<f64> = Vec::with_capacity(grid.len());
    for time in grid {
        if deduped_grid
            .last()
            .is_some_and(|last| (time - *last).abs() <= GRID_DEDUP_EPS)
        {
            continue;
        }
        deduped_grid.push(time);
    }
    deduped_grid
}

/// Scalar error summary handed to shape classification.
#[derive(Debug, Clone, Copy)]
struct ChannelErrorSummary {
    mean_abs_error: f64,
    max_abs_error: f64,
    bounded_normalized_l1_error: f64,
}

fn classify_channel_deviation_shape(
    samples: &[(f64, f64, f64)],
    error: ChannelErrorSummary,
    scale: ReferenceScale,
) -> TraceDeviationShape {
    if error.bounded_normalized_l1_error <= HIGH_AGREEMENT_CHANNEL_THRESHOLD {
        return TraceDeviationShape::WithinTolerance;
    }
    if samples.len() < 3 || error.max_abs_error <= NORMALIZATION_SCALE_EPS {
        return TraceDeviationShape::Unknown;
    }
    if initial_error_dominates(samples) {
        return TraceDeviationShape::WrongInitialValueOnly;
    }
    // A step-hold channel is only an *event-timing* disagreement once the
    // timing predicate says so. Labelling every over-threshold discrete channel
    // `EventTimeMismatch` before testing anything would make a real discrete
    // value disagreement read as a sampling convention, so this branch is
    // gated, not unconditional.
    if scale.use_step_hold && event_time_mismatch_dominates(samples, scale) {
        return TraceDeviationShape::EventTimeMismatch;
    }
    if sign_inversion_dominates(samples) {
        return TraceDeviationShape::SignInversion;
    }
    if scale_error_dominates(samples, error.mean_abs_error) {
        return TraceDeviationShape::ScaleError;
    }
    if constant_offset_dominates(samples) {
        return TraceDeviationShape::ConstantOffset;
    }
    if monotonic_drift_dominates(samples) {
        return TraceDeviationShape::MonotonicDrift;
    }
    if phase_shift_likely(samples, error.mean_abs_error, error.max_abs_error) {
        return TraceDeviationShape::PhaseTimeShift;
    }
    // A flat-zero reference with a large absolute residual is almost always a
    // channel mapping problem rather than a numeric deviation. This is keyed on
    // the *raw* reference statistics, not on `normalization_scale`, which now
    // has an absolute floor and would otherwise make this branch unreachable.
    if scale.reference_is_degenerate() && error.mean_abs_error > 0.1 {
        return TraceDeviationShape::MissingOrWrongChannelMapping;
    }
    if error.bounded_normalized_l1_error <= MINOR_AGREEMENT_CHANNEL_THRESHOLD {
        return TraceDeviationShape::StepSizeIntegrationError;
    }
    TraceDeviationShape::Unknown
}

/// Does a step-hold channel disagree about *when* it switches rather than about
/// *what* it holds?
///
/// Two independent conditions have to hold, and both are about the shape of the
/// disagreement rather than its size:
///
/// 1. **The disagreement is confined.** Held between transitions, a
///    timing-shifted signal only differs across the interval between the two
///    switch instants, so the *time* it spends disagreeing is a minority of the
///    horizon. A channel that holds a different value for most of the run is
///    disagreeing about the value.
/// 2. **Both traces only ever hold levels the other one also reaches.** A
///    shifted signal replays the same levels early or late; a channel that
///    settles on a level the reference never visits — the signature of a
///    clocked partition solved as an algebraic loop instead of a recurrence —
///    fails this and drops through to the numeric shape ladder.
///
/// Level comparison uses a fraction of the channel's own normalization scale so
/// the predicate is unit-free, and sample weighting is left-endpoint because
/// that is exactly the step-hold reconstruction the samples were built with.
fn event_time_mismatch_dominates(samples: &[(f64, f64, f64)], scale: ReferenceScale) -> bool {
    let tolerance = EVENT_MISMATCH_LEVEL_TOLERANCE_FRACTION * scale.normalization_scale;
    let (Some(first), Some(last)) = (samples.first(), samples.last()) else {
        return false;
    };
    let horizon = last.0 - first.0;
    // A NaN horizon (incomparable) must bail out exactly like a non-positive one.
    if horizon.partial_cmp(&0.0) != Some(std::cmp::Ordering::Greater) {
        return false;
    }
    let mut disagreeing_duration = 0.0;
    let mut disagreeing = 0usize;
    let mut level_matched = 0usize;
    let rumoca_levels = sorted_channel_values(samples.iter().map(|(_, rumoca, _)| *rumoca));
    let omc_levels = sorted_channel_values(samples.iter().map(|(_, _, omc)| *omc));
    for (index, (time, rumoca, omc)) in samples.iter().copied().enumerate() {
        if (rumoca - omc).abs() <= tolerance {
            continue;
        }
        disagreeing += 1;
        if let Some((next_time, _, _)) = samples.get(index + 1) {
            disagreeing_duration += next_time - time;
        }
        if channel_holds_level(&omc_levels, rumoca, tolerance)
            && channel_holds_level(&rumoca_levels, omc, tolerance)
        {
            level_matched += 1;
        }
    }
    if disagreeing == 0 {
        return false;
    }
    if disagreeing_duration > EVENT_MISMATCH_MAX_DISAGREEMENT_SHARE * horizon {
        return false;
    }
    level_matched as f64 >= EVENT_MISMATCH_MIN_LEVEL_MATCH_SHARE * disagreeing as f64
}

fn sorted_channel_values(values: impl Iterator<Item = f64>) -> Vec<f64> {
    let mut sorted = values.collect::<Vec<_>>();
    sorted.sort_by(f64::total_cmp);
    sorted
}

/// True when `value` is a level the sorted channel actually reaches.
fn channel_holds_level(sorted: &[f64], value: f64, tolerance: f64) -> bool {
    match sorted.binary_search_by(|level| level.total_cmp(&value)) {
        Ok(_) => true,
        Err(index) => [index.checked_sub(1), Some(index)]
            .into_iter()
            .flatten()
            .filter_map(|index| sorted.get(index))
            .any(|level| (level - value).abs() <= tolerance),
    }
}

fn initial_error_dominates(samples: &[(f64, f64, f64)]) -> bool {
    let Some((_, first_r, first_o)) = samples.first().copied() else {
        return false;
    };
    let first_error = (first_r - first_o).abs();
    let tail = &samples[1..];
    if tail.is_empty() {
        return false;
    }
    let tail_mean = tail.iter().map(|(_, r, o)| (r - o).abs()).sum::<f64>() / tail.len() as f64;
    first_error > 0.1 && tail_mean <= 0.1 * first_error
}

fn sign_inversion_dominates(samples: &[(f64, f64, f64)]) -> bool {
    let (dot, rumoca_energy, omc_energy) = samples
        .iter()
        .fold((0.0, 0.0, 0.0), |(dot, re, oe), (_, r, o)| {
            (dot + r * o, re + r * r, oe + o * o)
        });
    if rumoca_energy <= NORMALIZATION_SCALE_EPS || omc_energy <= NORMALIZATION_SCALE_EPS {
        return false;
    }
    let correlation = dot / (rumoca_energy.sqrt() * omc_energy.sqrt());
    correlation < -0.90
}

fn scale_error_dominates(samples: &[(f64, f64, f64)], mean_abs_error: f64) -> bool {
    let (dot, omc_energy) = samples
        .iter()
        .fold((0.0, 0.0), |(dot, oe), (_, r, o)| (dot + r * o, oe + o * o));
    if omc_energy <= NORMALIZATION_SCALE_EPS {
        return false;
    }
    let scale = dot / omc_energy;
    if !(scale.is_finite() && (scale - 1.0).abs() >= 0.20) {
        return false;
    }
    let scaled_residual = samples
        .iter()
        .map(|(_, r, o)| (r - scale * o).abs())
        .sum::<f64>()
        / samples.len() as f64;
    scaled_residual <= 0.35 * mean_abs_error.max(NORMALIZATION_SCALE_EPS)
}

fn constant_offset_dominates(samples: &[(f64, f64, f64)]) -> bool {
    let offsets = samples.iter().map(|(_, r, o)| r - o).collect::<Vec<_>>();
    let mean = offsets.iter().sum::<f64>() / offsets.len() as f64;
    if mean.abs() <= 0.1 {
        return false;
    }
    let variance = offsets
        .iter()
        .map(|offset| {
            let residual = offset - mean;
            residual * residual
        })
        .sum::<f64>()
        / offsets.len() as f64;
    variance.sqrt() <= 0.20 * mean.abs()
}

fn monotonic_drift_dominates(samples: &[(f64, f64, f64)]) -> bool {
    let errors = samples.iter().map(|(_, r, o)| r - o).collect::<Vec<_>>();
    let Some(first) = errors.first().copied() else {
        return false;
    };
    let Some(last) = errors.last().copied() else {
        return false;
    };
    if last.abs() < 0.2 || last.abs() <= 2.0 * first.abs().max(0.05) {
        return false;
    }
    let increasing_steps = errors
        .windows(2)
        .filter(|window| window[1].abs() + 1.0e-9 >= window[0].abs())
        .count();
    increasing_steps as f64 >= 0.80 * (errors.len().saturating_sub(1)).max(1) as f64
}

fn phase_shift_likely(
    samples: &[(f64, f64, f64)],
    mean_abs_error: f64,
    max_abs_error: f64,
) -> bool {
    if max_abs_error <= 1.5 * mean_abs_error.max(NORMALIZATION_SCALE_EPS) {
        return false;
    }
    let rumoca_range = sample_range(samples.iter().map(|(_, r, _)| *r));
    let omc_range = sample_range(samples.iter().map(|(_, _, o)| *o));
    let similar_range =
        (rumoca_range - omc_range).abs() <= 0.25 * rumoca_range.max(omc_range).max(1.0);
    similar_range && rumoca_range.max(omc_range) > 0.1
}

fn median_of_sorted(sorted: &[f64]) -> Option<f64> {
    if sorted.is_empty() {
        return None;
    }
    let len = sorted.len();
    let median = if len.is_multiple_of(2) {
        (sorted[len / 2 - 1] + sorted[len / 2]) / 2.0
    } else {
        sorted[len / 2]
    };
    Some(median)
}

fn discrete_channel_names(trace: &SimTrace) -> HashSet<String> {
    let mut names = HashSet::new();
    let Some(meta) = trace.variable_meta.as_ref() else {
        return names;
    };
    for entry in meta {
        let is_discrete = entry
            .variability
            .as_deref()
            .is_some_and(|v| v.eq_ignore_ascii_case("discrete"))
            || entry.time_domain.as_deref().is_some_and(|d| {
                d.eq_ignore_ascii_case("event-discrete")
                    || d.eq_ignore_ascii_case("event-discontinuous")
            })
            || entry
                .role
                .as_deref()
                .is_some_and(|r| r.starts_with("discrete"));
        if is_discrete {
            names.insert(entry.name.clone());
        }
    }
    names
}

fn interp_channel(
    times: &[f64],
    values: &[Option<f64>],
    t: f64,
    use_step_hold: bool,
) -> Option<f64> {
    if use_step_hold {
        interp_step_hold(times, values, t)
    } else {
        interp_linear(times, values, t)
    }
}

fn interp_linear(times: &[f64], values: &[Option<f64>], t: f64) -> Option<f64> {
    if times.len() < 2 || times.len() != values.len() {
        return None;
    }
    if t < times[0] || t > times[times.len() - 1] {
        return None;
    }

    match times.binary_search_by(|probe| probe.partial_cmp(&t).unwrap_or(std::cmp::Ordering::Less))
    {
        Ok(idx) => values.get(idx).copied().flatten(),
        Err(right) => {
            if right == 0 {
                return None;
            }
            let left = right - 1;
            if right >= times.len() {
                return values.last().copied().flatten();
            }
            let (t0, t1) = (times[left], times[right]);
            let (Some(v0), Some(v1)) = (values[left], values[right]) else {
                return None;
            };
            if t1 <= t0 {
                return Some(v0);
            }
            let alpha = (t - t0) / (t1 - t0);
            Some(v0 + alpha * (v1 - v0))
        }
    }
}

fn interp_step_hold(times: &[f64], values: &[Option<f64>], t: f64) -> Option<f64> {
    if times.is_empty() || times.len() != values.len() {
        return None;
    }
    if t < times[0] || t > times[times.len() - 1] {
        return None;
    }
    match times.binary_search_by(|probe| probe.partial_cmp(&t).unwrap_or(std::cmp::Ordering::Less))
    {
        Ok(idx) => values.get(idx).copied().flatten(),
        Err(right) => {
            if right == 0 {
                None
            } else {
                values.get(right - 1).copied().flatten()
            }
        }
    }
}
