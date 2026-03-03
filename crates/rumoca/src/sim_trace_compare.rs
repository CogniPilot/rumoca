use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::path::Path;

const GRID_DEDUP_EPS: f64 = 1.0e-12;
pub const HIGH_AGREEMENT_THRESHOLD: f64 = 0.02;
pub const MINOR_AGREEMENT_THRESHOLD: f64 = 0.05;

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct SimTrace {
    #[serde(default)]
    pub model_name: Option<String>,
    pub times: Vec<f64>,
    pub names: Vec<String>,
    pub data: Vec<Vec<Option<f64>>>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ChannelDeviationMetric {
    pub name: String,
    pub samples: usize,
    pub integral_duration: f64,
    pub integral_abs_error: f64,
    pub integral_abs_ref: f64,
    pub normalized_integral_abs_error: f64,
    pub normalized_rmse: f64,
    pub normalized_max_abs_error: f64,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ModelDeviationMetric {
    pub model_name: String,
    pub compared_variables: usize,
    pub samples_compared: usize,
    pub deviation_score: f64,
    pub mean_normalized_integral_abs_error: f64,
    pub max_normalized_integral_abs_error: f64,
    pub mean_normalized_rmse: f64,
    pub max_normalized_rmse: f64,
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
    let rumoca_names: HashSet<String> = rumoca_series.keys().cloned().collect();
    let omc_names: HashSet<String> = omc_series.keys().cloned().collect();
    let common: HashSet<String> = rumoca_names.intersection(&omc_names).cloned().collect();
    if common.is_empty() {
        return Err(TraceCompareError::NoCommonVariables);
    }

    let mut channels: Vec<ChannelDeviationMetric> = common
        .into_iter()
        .filter_map(|name| {
            compare_channel(
                &name,
                &rumoca.times,
                rumoca_series.get(&name)?,
                &omc.times,
                omc_series.get(&name)?,
            )
        })
        .collect();
    if channels.is_empty() {
        return Err(TraceCompareError::NoComparableSamples);
    }

    channels.sort_by(|a, b| {
        b.normalized_integral_abs_error
            .partial_cmp(&a.normalized_integral_abs_error)
            .unwrap_or(std::cmp::Ordering::Equal)
    });

    let compared_variables = channels.len();
    let samples_compared = channels.iter().map(|m| m.samples).sum::<usize>();
    let mean_integral = channels
        .iter()
        .map(|m| m.normalized_integral_abs_error)
        .sum::<f64>()
        / compared_variables as f64;
    let max_integral = channels
        .iter()
        .map(|m| m.normalized_integral_abs_error)
        .fold(0.0_f64, f64::max);
    let mean_rmse =
        channels.iter().map(|m| m.normalized_rmse).sum::<f64>() / compared_variables as f64;
    let max_rmse = channels
        .iter()
        .map(|m| m.normalized_rmse)
        .fold(0.0_f64, f64::max);
    let worst_variables = channels.into_iter().take(10).collect();

    Ok(ModelDeviationMetric {
        model_name: model_name.to_string(),
        compared_variables,
        samples_compared,
        deviation_score: mean_integral,
        mean_normalized_integral_abs_error: mean_integral,
        max_normalized_integral_abs_error: max_integral,
        mean_normalized_rmse: mean_rmse,
        max_normalized_rmse: max_rmse,
        worst_variables,
    })
}

pub fn classify_deviation_score(
    deviation_score: f64,
    high_agreement_threshold: f64,
    minor_agreement_threshold: f64,
) -> AgreementBand {
    if deviation_score < high_agreement_threshold {
        return AgreementBand::HighAgreement;
    }
    if deviation_score < minor_agreement_threshold {
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
        match classify_deviation_score(
            metric.deviation_score,
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
    count_agreement_bands(metrics, HIGH_AGREEMENT_THRESHOLD, MINOR_AGREEMENT_THRESHOLD)
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
        let r = interp_linear(rumoca_times, rumoca_values, t);
        let o = interp_linear(omc_times, omc_values, t);
        samples.push((t, r, o));
    }

    let mut diffs = Vec::new();
    let mut ref_peak = 0.0_f64;
    let mut integral_abs_error = 0.0_f64;
    let mut integral_abs_ref = 0.0_f64;
    let mut integral_duration = 0.0_f64;

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
        let ref0 = o0.abs();
        let ref1 = o1.abs();

        integral_abs_error += 0.5 * (e0 + e1) * dt;
        integral_abs_ref += 0.5 * (ref0 + ref1) * dt;
        integral_duration += dt;
        diffs.push(r0 - o0);
        diffs.push(r1 - o1);
        ref_peak = ref_peak.max(ref0).max(ref1);
    }

    if diffs.len() < 2 || integral_duration <= 0.0 {
        return None;
    }

    let mse = diffs.iter().map(|d| d * d).sum::<f64>() / diffs.len() as f64;
    let rmse = mse.sqrt();
    let max_abs_error = diffs.iter().map(|d| d.abs()).fold(0.0_f64, f64::max);
    let rmse_scale = ref_peak.max(1.0e-9);
    let integral_norm_scale = integral_duration * ref_peak.max(1.0);
    let normalized_integral_abs_error = integral_abs_error / integral_norm_scale.max(1.0e-12);

    Some(ChannelDeviationMetric {
        name: name.to_string(),
        samples: diffs.len(),
        integral_duration,
        integral_abs_error,
        integral_abs_ref,
        normalized_integral_abs_error,
        normalized_rmse: rmse / rmse_scale,
        normalized_max_abs_error: max_abs_error / rmse_scale,
    })
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
        }
    }

    #[test]
    fn channel_integral_metric_matches_expected_value() {
        let metric = compare_channel(
            "x",
            &[0.0, 0.5, 1.0],
            &[Some(0.0), Some(1.0), Some(2.0)],
            &[0.0, 0.5, 1.0],
            &[Some(0.0), Some(1.1), Some(2.1)],
        )
        .expect("channel should compare");

        // integral_abs_error = 0.075, duration = 1.0, omc_peak = 2.1
        let expected = 0.075 / 2.1;
        assert!((metric.normalized_integral_abs_error - expected).abs() < 1.0e-12);
    }

    #[test]
    fn channel_integral_metric_is_finite_when_reference_is_near_zero() {
        let metric = compare_channel(
            "u",
            &[0.0, 1.0],
            &[Some(1.0), Some(1.0)],
            &[0.0, 1.0],
            &[Some(0.0), Some(0.0)],
        )
        .expect("channel should compare");
        assert!(metric.normalized_integral_abs_error.is_finite());
        assert!((metric.normalized_integral_abs_error - 1.0).abs() < 1.0e-12);
    }

    #[test]
    fn model_deviation_score_uses_mean_normalized_integral_error() {
        let rumoca = trace(
            "M",
            vec![0.0, 0.5, 1.0],
            vec!["x", "y"],
            vec![vec![0.0, 1.0, 2.0], vec![0.0, 2.0, 4.0]],
        );
        let omc = trace(
            "M",
            vec![0.0, 0.5, 1.0],
            vec!["x", "y"],
            vec![vec![0.0, 1.1, 2.1], vec![0.0, 2.2, 4.2]],
        );

        let metric = compare_model_traces("M", &rumoca, &omc).expect("model compare");
        assert_eq!(metric.compared_variables, 2);
        assert!(
            (metric.deviation_score - metric.mean_normalized_integral_abs_error).abs() < 1.0e-15
        );
        assert!(metric.deviation_score > 0.0);
        assert!(!metric.worst_variables.is_empty());
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
            metric.deviation_score < 1.0e-12,
            "duplicate timestamp collapse should keep settled event value"
        );
    }

    #[test]
    fn agreement_band_thresholds_classify_as_expected() {
        assert_eq!(
            classify_deviation_score(0.01, HIGH_AGREEMENT_THRESHOLD, MINOR_AGREEMENT_THRESHOLD),
            AgreementBand::HighAgreement
        );
        assert_eq!(
            classify_deviation_score(0.03, HIGH_AGREEMENT_THRESHOLD, MINOR_AGREEMENT_THRESHOLD),
            AgreementBand::MinorAgreement
        );
        assert_eq!(
            classify_deviation_score(0.2, HIGH_AGREEMENT_THRESHOLD, MINOR_AGREEMENT_THRESHOLD),
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
            vec!["x"],
            vec![vec![1.02, 1.02, 1.02]],
        );
        let minor_omc = trace(
            "minor",
            vec![0.0, 0.5, 1.0],
            vec!["x"],
            vec![vec![1.0, 1.0, 1.0]],
        );
        let minor =
            compare_model_traces("minor", &minor_rumoca, &minor_omc).expect("minor compare");
        assert!(minor.deviation_score >= HIGH_AGREEMENT_THRESHOLD);
        assert!(minor.deviation_score < MINOR_AGREEMENT_THRESHOLD);

        let dev_rumoca = trace(
            "dev",
            vec![0.0, 0.5, 1.0],
            vec!["x"],
            vec![vec![1.2, 1.2, 1.2]],
        );
        let dev_omc = trace(
            "dev",
            vec![0.0, 0.5, 1.0],
            vec!["x"],
            vec![vec![1.0, 1.0, 1.0]],
        );
        let dev = compare_model_traces("dev", &dev_rumoca, &dev_omc).expect("dev compare");
        assert!(dev.deviation_score >= MINOR_AGREEMENT_THRESHOLD);

        let metrics = [high, minor, dev];
        let counts = count_agreement_bands_default(metrics.iter());
        assert_eq!(counts.high_agreement, 1);
        assert_eq!(counts.minor_agreement, 1);
        assert_eq!(counts.deviation, 1);
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
        assert_eq!(counts.minor_agreement, 1);
        assert_eq!(counts.deviation, 1);
    }
}
