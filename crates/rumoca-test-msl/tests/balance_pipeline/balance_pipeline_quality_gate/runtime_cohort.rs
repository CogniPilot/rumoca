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
        (values[sample_count / 2 - 1] + values[sample_count / 2]) / 2.0
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

fn push_runtime_median_reason(reasons: &mut Vec<String>, label: &str, current: f64, baseline: f64) {
    let floor = baseline * (1.0 - RUNTIME_RATIO_MEDIAN_REL_TOLERANCE);
    if current + SIM_RATE_GATE_EPSILON < floor {
        reasons.push(format!(
            "runtime {label} speedup median regressed: current={current:.6e} < floor={floor:.6e} (baseline={baseline:.6e}, tolerance={:.1}%)",
            RUNTIME_RATIO_MEDIAN_REL_TOLERANCE * 100.0
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
