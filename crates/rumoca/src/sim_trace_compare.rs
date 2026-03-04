use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::path::Path;

const GRID_DEDUP_EPS: f64 = 1.0e-12;
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
pub const BAD_CHANNEL_MAX_THRESHOLD: f64 = 0.20;
pub const SEVERE_CHANNEL_MAX_THRESHOLD: f64 = 0.80;
// Legacy score-threshold aliases retained for compatibility with score-only
// callers; agreement buckets in CI use model max/mean channel thresholds.
pub const HIGH_AGREEMENT_THRESHOLD: f64 = HIGH_AGREEMENT_MAX_CHANNEL_THRESHOLD;
pub const MINOR_AGREEMENT_THRESHOLD: f64 = MINOR_AGREEMENT_MAX_CHANNEL_THRESHOLD;

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
    pub samples: usize,
    pub integral_duration: f64,
    pub integral_abs_error: f64,
    pub mean_abs_error: f64,
    pub normalization_scale: f64,
    pub normalized_l1_error: f64,
    pub bounded_normalized_l1_error: f64,
    pub normalized_max_abs_error: f64,
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

    let mut channels: Vec<ChannelDeviationMetric> = common
        .into_iter()
        .filter_map(|name| {
            let is_discrete_channel =
                rumoca_discrete_channels.contains(&name) || omc_discrete_channels.contains(&name);
            compare_channel(
                &name,
                &rumoca.times,
                rumoca_series.get(&name)?,
                &omc.times,
                omc_series.get(&name)?,
                is_discrete_channel,
            )
        })
        .collect();
    if channels.is_empty() {
        return Err(TraceCompareError::NoComparableSamples);
    }

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
        worst_variables,
    })
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

pub fn classify_trace_metric_channel_distribution(
    metric: &ModelDeviationMetric,
    high_min_high_channel_share: f64,
    high_max_deviation_channel_share: f64,
    minor_min_high_plus_minor_channel_share: f64,
    minor_max_deviation_channel_share: f64,
) -> AgreementBand {
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

fn compare_channel(
    name: &str,
    rumoca_times: &[f64],
    rumoca_values: &[Option<f64>],
    omc_times: &[f64],
    omc_values: &[Option<f64>],
    use_step_hold: bool,
) -> Option<ChannelDeviationMetric> {
    if rumoca_times.len() < 2
        || omc_times.len() < 2
        || rumoca_times.len() != rumoca_values.len()
        || omc_times.len() != omc_values.len()
    {
        return None;
    }

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

    let mut deduped_grid: Vec<f64> = Vec::with_capacity(grid.len());
    for t in grid {
        if deduped_grid
            .last()
            .is_some_and(|last| (t - *last).abs() <= GRID_DEDUP_EPS)
        {
            continue;
        }
        deduped_grid.push(t);
    }
    if deduped_grid.len() < 2 {
        return None;
    }

    let mut samples = Vec::with_capacity(deduped_grid.len());
    for &t in &deduped_grid {
        let r = interp_channel(rumoca_times, rumoca_values, t, use_step_hold);
        let o = interp_channel(omc_times, omc_values, t, use_step_hold);
        samples.push((t, r, o));
    }

    let mut ref_samples = Vec::new();
    let mut integral_abs_error = 0.0_f64;
    let mut integral_duration = 0.0_f64;
    let mut max_abs_error = 0.0_f64;

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
        let d0 = r0 - o0;
        let d1 = r1 - o1;
        let e0 = d0.abs();
        let e1 = d1.abs();

        integral_abs_error += 0.5 * (e0 + e1) * dt;
        integral_duration += dt;
        max_abs_error = max_abs_error.max(e0).max(e1);
        ref_samples.push(o0);
        ref_samples.push(o1);
    }

    if ref_samples.len() < 2 || integral_duration <= 0.0 {
        return None;
    }

    let mean_abs_error = integral_abs_error / integral_duration;
    let reference_range = robust_reference_range(&ref_samples).unwrap_or(0.0);
    let normalization_scale = if use_step_hold {
        reference_range.max(1.0).max(NORMALIZATION_SCALE_EPS)
    } else {
        reference_range.max(NORMALIZATION_SCALE_EPS)
    };
    let normalized_l1_error = mean_abs_error / normalization_scale;
    let bounded_normalized_l1_error = normalized_l1_error / (1.0 + normalized_l1_error);

    Some(ChannelDeviationMetric {
        name: name.to_string(),
        samples: ref_samples.len(),
        integral_duration,
        integral_abs_error,
        mean_abs_error,
        normalization_scale,
        normalized_l1_error,
        bounded_normalized_l1_error,
        normalized_max_abs_error: max_abs_error / normalization_scale,
    })
}

fn robust_reference_range(values: &[f64]) -> Option<f64> {
    let mut sorted = values
        .iter()
        .copied()
        .filter(|value| value.is_finite())
        .collect::<Vec<_>>();
    if sorted.is_empty() {
        return None;
    }
    sorted.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    let p05 = percentile_sorted(&sorted, RANGE_LOW_QUANTILE);
    let p95 = percentile_sorted(&sorted, RANGE_HIGH_QUANTILE);
    Some((p95 - p05).abs())
}

fn percentile_sorted(sorted: &[f64], quantile: f64) -> f64 {
    if sorted.is_empty() {
        return 0.0;
    }
    if sorted.len() == 1 {
        return sorted[0];
    }
    let clamped = quantile.clamp(0.0, 1.0);
    let position = clamped * (sorted.len() - 1) as f64;
    let lower_idx = position.floor() as usize;
    let upper_idx = position.ceil() as usize;
    if lower_idx == upper_idx {
        return sorted[lower_idx];
    }
    let weight = position - lower_idx as f64;
    sorted[lower_idx] * (1.0 - weight) + sorted[upper_idx] * weight
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
            || entry
                .time_domain
                .as_deref()
                .is_some_and(|d| d.eq_ignore_ascii_case("event-discrete"))
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

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
        }
    }

    #[test]
    fn channel_normalized_l1_matches_expected_value() {
        let metric = compare_channel(
            "x",
            &[0.0, 0.5, 1.0],
            &[Some(0.0), Some(1.0), Some(2.0)],
            &[0.0, 0.5, 1.0],
            &[Some(0.0), Some(1.1), Some(2.1)],
            false,
        )
        .expect("channel should compare");

        // integral_abs_error = 0.075, duration = 1.0
        // reference range uses P95-P05 over sampled reference values.
        let scale = robust_reference_range(&[0.0, 1.1, 1.1, 2.1]).expect("range");
        let expected = 0.075 / scale;
        assert!((metric.normalized_l1_error - expected).abs() < 1.0e-12);
    }

    #[test]
    fn channel_normalized_l1_is_finite_when_reference_is_near_zero() {
        let metric = compare_channel(
            "u",
            &[0.0, 1.0],
            &[Some(1.0), Some(1.0)],
            &[0.0, 1.0],
            &[Some(0.0), Some(0.0)],
            false,
        )
        .expect("channel should compare");
        assert!(metric.normalized_l1_error.is_finite());
        assert!(metric.normalized_l1_error > 1.0e10);
        assert!((metric.bounded_normalized_l1_error - 1.0).abs() < 1.0e-10);
    }

    #[test]
    fn channel_mean_abs_error_uses_time_weighted_integration() {
        let metric = compare_channel(
            "x",
            &[0.0, 0.001, 1.0],
            &[Some(200.0), Some(100.0), Some(100.0)],
            &[0.0, 0.001, 1.0],
            &[Some(100.0), Some(100.0), Some(100.0)],
            false,
        )
        .expect("channel should compare");

        // Error is concentrated in [0, 0.001], so the time-weighted mean absolute error is:
        // integral_abs_error = 0.5*(100 + 0)*0.001 = 0.05
        let expected = 0.05;
        assert!((metric.mean_abs_error - expected).abs() < 1.0e-12);
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
    fn compare_trace_collapses_duplicate_timestamps_to_last_value() {
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
            "duplicate timestamp collapse should keep settled event value"
        );
    }

    #[test]
    fn discrete_channel_uses_step_hold_interpolation() {
        let metric = compare_channel(
            "q",
            &[0.0, 1.0],
            &[Some(0.0), Some(1.0)],
            &[0.0, 0.5, 1.0],
            &[Some(0.0), Some(0.0), Some(1.0)],
            true,
        )
        .expect("channel compare");
        assert!(
            metric.bounded_normalized_l1_error < 1.0e-12,
            "step-hold should avoid synthetic mid-step interpolation error for discrete channels"
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
        };

        let metric = compare_model_traces("M", &rumoca, &omc)
            .expect("discrete-only traces should still produce comparison metrics");
        assert_eq!(metric.compared_variables, 1);
        assert_eq!(metric.samples_compared, 4);
        assert!(metric.bounded_normalized_l1_score < 1.0e-12);
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
        let minor =
            compare_model_traces("minor", &minor_rumoca, &minor_omc).expect("minor compare");
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
        let high = ModelDeviationMetric {
            model_name: "high".to_string(),
            compared_variables: 10,
            samples_compared: 10,
            bounded_normalized_l1_score: 0.0,
            mean_channel_bounded_normalized_l1: 0.0,
            max_channel_bounded_normalized_l1: 0.0,
            channel_high_count: 9,
            channel_minor_count: 1,
            channel_deviation_count: 0,
            channel_severe_count: 0,
            channel_high_percent: 0.9,
            channel_minor_percent: 0.1,
            channel_deviation_percent: 0.0,
            channel_severe_percent: 0.0,
            channel_violation_mass: 0.0,
            worst_variables: Vec::new(),
        };
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

        let near = ModelDeviationMetric {
            model_name: "near".to_string(),
            compared_variables: 10,
            samples_compared: 10,
            bounded_normalized_l1_score: 0.0,
            mean_channel_bounded_normalized_l1: 0.0,
            max_channel_bounded_normalized_l1: 0.0,
            channel_high_count: 4,
            channel_minor_count: 5,
            channel_deviation_count: 1,
            channel_severe_count: 0,
            channel_high_percent: 0.4,
            channel_minor_percent: 0.5,
            channel_deviation_percent: 0.1,
            channel_severe_percent: 0.0,
            channel_violation_mass: 0.01,
            worst_variables: Vec::new(),
        };
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

        let deviation = ModelDeviationMetric {
            model_name: "deviation".to_string(),
            compared_variables: 10,
            samples_compared: 10,
            bounded_normalized_l1_score: 0.0,
            mean_channel_bounded_normalized_l1: 0.0,
            max_channel_bounded_normalized_l1: 0.0,
            channel_high_count: 2,
            channel_minor_count: 5,
            channel_deviation_count: 3,
            channel_severe_count: 1,
            channel_high_percent: 0.2,
            channel_minor_percent: 0.5,
            channel_deviation_percent: 0.3,
            channel_severe_percent: 0.1,
            channel_violation_mass: 0.5,
            worst_variables: Vec::new(),
        };
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
    }

    fn fixture_path(rel: &str) -> PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests")
            .join("fixtures")
            .join("sim_traces")
            .join(rel)
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
}
