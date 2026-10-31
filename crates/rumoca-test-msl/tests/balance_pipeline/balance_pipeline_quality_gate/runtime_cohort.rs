use super::*;

/// A runtime comparison remains representative while at least this fraction of
/// the committed baseline cohort still produces timings in both tools.
const RUNTIME_RATIO_COHORT_MIN_COVERAGE: f64 = 0.90;

#[derive(Debug, Clone)]
pub(super) struct MslRuntimeGateStats {
    pub(super) stats: MslRuntimeRatioStatsBaseline,
    pub(super) cohort_coverage: Option<(usize, usize)>,
}

fn positive_f64(model: &serde_json::Value, keys: &[&str]) -> Option<f64> {
    keys.iter().find_map(|key| {
        let value = model.get(key)?.as_f64()?;
        (value.is_finite() && value > 0.0).then_some(value)
    })
}

fn model_runtime_ratio(model: &serde_json::Value) -> Option<MslRuntimeModelRatio> {
    if model.get("status")?.as_str()? != "success"
        || model.get("rumoca_status")?.as_str()? != "sim_ok"
    {
        return None;
    }
    let rumoca_system = positive_f64(model, &["rumoca_sim_run_seconds", "rumoca_sim_seconds"])?;
    let omc_system = positive_f64(model, &["sim_system_seconds"])?;
    let rumoca_wall = positive_f64(model, &["rumoca_sim_wall_seconds"])?;
    let omc_wall = positive_f64(model, &["omc_wall_seconds"])?;
    Some(MslRuntimeModelRatio {
        system: omc_system / rumoca_system,
        wall: omc_wall / rumoca_wall,
    })
}

pub(super) fn parse_runtime_model_ratios(
    payload: &serde_json::Value,
) -> IndexMap<String, MslRuntimeModelRatio> {
    let Some(models) = payload.get("models").and_then(serde_json::Value::as_object) else {
        return IndexMap::new();
    };
    let mut names = models.keys().cloned().collect::<Vec<_>>();
    names.sort();
    names
        .into_iter()
        .filter_map(|name| {
            let ratio = model_runtime_ratio(models.get(&name)?)?;
            Some((name, ratio))
        })
        .collect()
}

fn distribution(mut values: Vec<f64>) -> Option<MslDistributionStats> {
    if values.is_empty() {
        return None;
    }
    values.sort_by(f64::total_cmp);
    let sample_count = values.len();
    let median = if sample_count.is_multiple_of(2) {
        values[sample_count / 2 - 1]
            + (values[sample_count / 2] - values[sample_count / 2 - 1]) / 2.0
    } else {
        values[sample_count / 2]
    };
    Some(MslDistributionStats {
        sample_count,
        min: values[0],
        median,
        mean: values.iter().sum::<f64>() / sample_count as f64,
        max: values[sample_count - 1],
    })
}

fn cohort_runtime_stats(
    cohort: &IndexSet<String>,
    current: &IndexMap<String, MslRuntimeModelRatio>,
) -> Option<MslRuntimeRatioStatsBaseline> {
    let ratios = cohort
        .iter()
        .filter_map(|name| current.get(name))
        .collect::<Vec<_>>();
    Some(MslRuntimeRatioStatsBaseline {
        system_ratio_both_success: distribution(ratios.iter().map(|ratio| ratio.system).collect())?,
        wall_ratio_both_success: distribution(ratios.iter().map(|ratio| ratio.wall).collect())?,
    })
}

pub(super) fn runtime_ratio_gate_stats(
    baseline: &MslQualityBaseline,
    parity: &MslParityGateInput,
) -> Option<MslRuntimeGateStats> {
    if let Some(cohort) = baseline
        .runtime_ratio_cohort_models
        .as_ref()
        .filter(|cohort| !cohort.is_empty())
    {
        let stats = cohort_runtime_stats(cohort, &parity.runtime_model_ratios)?;
        return Some(MslRuntimeGateStats {
            cohort_coverage: Some((stats.wall_ratio_both_success.sample_count, cohort.len())),
            stats,
        });
    }
    Some(MslRuntimeGateStats {
        stats: parity.runtime_ratio_stats.clone()?,
        cohort_coverage: None,
    })
}

pub(super) fn push_runtime_median_reason(
    reasons: &mut Vec<String>,
    label: &str,
    current: f64,
    baseline: f64,
) {
    let tolerance = if label == "wall" {
        RUNTIME_WALL_RATIO_MEDIAN_REL_TOLERANCE
    } else {
        RUNTIME_RATIO_MEDIAN_REL_TOLERANCE
    };
    let floor = baseline * (1.0 - tolerance);
    if current + SIM_RATE_GATE_EPSILON < floor {
        reasons.push(format!(
            "runtime {label} speedup median regressed: current={current:.6e} < floor={floor:.6e} (baseline={baseline:.6e}, tolerance={:.1}%)",
            tolerance * 100.0
        ));
    }
}

pub(super) fn push_runtime_ratio_regression_reasons(
    reasons: &mut Vec<String>,
    baseline: &MslQualityBaseline,
    parity_input: Option<&MslParityGateInput>,
) {
    let (Some(parity), Some(baseline_runtime)) =
        (parity_input, baseline.runtime_ratio_stats.as_ref())
    else {
        return;
    };
    let Some(current) = runtime_ratio_gate_stats(baseline, parity) else {
        reasons.push("runtime baseline cohort produced no comparable timings".to_string());
        return;
    };
    if let Some((matched, expected)) = current.cohort_coverage
        && (matched as f64) + SIM_RATE_GATE_EPSILON
            < expected as f64 * RUNTIME_RATIO_COHORT_MIN_COVERAGE
    {
        reasons.push(format!(
            "runtime baseline cohort coverage regressed: current={matched}/{expected} < {:.1}%",
            RUNTIME_RATIO_COHORT_MIN_COVERAGE * 100.0
        ));
    }
    push_runtime_median_reason(
        reasons,
        "system",
        current.stats.system_ratio_both_success.median,
        baseline_runtime.system_ratio_both_success.median,
    );
    push_runtime_median_reason(
        reasons,
        "wall",
        current.stats.wall_ratio_both_success.median,
        baseline_runtime.wall_ratio_both_success.median,
    );
}

/// Bind runtime reference metadata before a recorded aggregate defines a floor.
/// Older snapshots without per-model ratios retain their existing aggregate path.
pub(super) fn validate_runtime_baseline_evidence(document: &serde_json::Value) -> io::Result<()> {
    let invalid =
        |message: &str| io::Error::other(format!("invalid MSL runtime baseline: {message}"));
    let Some(recorded) = document.get("runtime_model_ratios") else {
        let modern = document
            .get("quality_gate_version")
            .and_then(serde_json::Value::as_u64)
            .is_some_and(|version| version >= 14);
        if modern
            && document
                .get("runtime_ratio_stats")
                .is_some_and(|stats| !stats.is_null())
        {
            return Err(invalid("current-schema runtime_model_ratios are missing"));
        }
        return Ok(());
    };
    let ratios: IndexMap<String, MslRuntimeModelRatio> =
        serde_json::from_value(recorded.clone())
            .map_err(|_| invalid("runtime_model_ratios must be an object of numeric ratios"))?;
    let cohort = document
        .get("runtime_ratio_cohort_models")
        .and_then(serde_json::Value::as_array)
        .ok_or_else(|| invalid("runtime cohort is missing"))?;
    let names = cohort
        .iter()
        .filter_map(serde_json::Value::as_str)
        .collect::<IndexSet<_>>();
    if ratios.is_empty()
        || cohort.len() != ratios.len()
        || names.len() != cohort.len()
        || !ratios.keys().all(|name| names.contains(name.as_str()))
        || !ratios.values().all(|ratio| {
            [ratio.system, ratio.wall]
                .iter()
                .all(|value| value.is_finite() && *value > 0.0)
        })
    {
        return Err(invalid(
            "runtime_model_ratios do not bind the exact finite positive cohort",
        ));
    }
    let recorded: MslRuntimeRatioStatsBaseline = serde_json::from_value(
        document
            .get("runtime_ratio_stats")
            .cloned()
            .unwrap_or_default(),
    )
    .map_err(|_| invalid("runtime_ratio_stats are missing or malformed"))?;
    for (stats, values) in [
        (
            recorded.system_ratio_both_success,
            ratios.values().map(|ratio| ratio.system).collect(),
        ),
        (
            recorded.wall_ratio_both_success,
            ratios.values().map(|ratio| ratio.wall).collect(),
        ),
    ] {
        let derived = distribution(values).ok_or_else(|| invalid("runtime cohort is empty"))?;
        let scale = 1.0_f64.max(derived.median.abs()).max(stats.median.abs());
        if stats.sample_count != derived.sample_count
            || !derived.median.is_finite()
            || !stats.median.is_finite()
            || (stats.median - derived.median).abs() > 1.0e-9 * scale
        {
            return Err(invalid(
                "runtime_model_ratios disagree with aggregate count or median",
            ));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::distribution;

    #[test]
    fn runtime_distribution_keeps_finite_huge_and_subnormal_midpoints() {
        for value in [1.0e308, f64::from_bits(1)] {
            let stats = distribution(vec![value, value]).expect("two positive ratios");
            assert_eq!(stats.median, value);
        }
        let stats = distribution(vec![f64::from_bits(1), f64::MAX]).expect("positive ratios");
        assert!(stats.median.is_finite());
    }
}
