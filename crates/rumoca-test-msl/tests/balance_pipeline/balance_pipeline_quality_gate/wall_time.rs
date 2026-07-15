use super::*;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub(super) struct MslWallTimeProvenance {
    pub(super) omc_fresh_sample_count: usize,
    pub(super) omc_cached_sample_count: usize,
    pub(super) affinity_requested_worker_count: usize,
    pub(super) affinity_applied_worker_count: usize,
    pub(super) affinity_failed_worker_count: usize,
    pub(super) normalized_load_before: Option<f64>,
    pub(super) normalized_load_after: Option<f64>,
    pub(super) rumoca_workers_used: usize,
    pub(super) workers_used: usize,
    pub(super) omc_threads: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct WallTimeTrustDecision {
    pub(super) trusted: bool,
    pub(super) reasons: Vec<String>,
}

pub(super) fn wall_time_trust_decision(
    baseline: &MslQualityBaseline,
    parity_input: Option<&MslParityGateInput>,
) -> WallTimeTrustDecision {
    let Some(expected_context) = complete_runtime_context(baseline.runtime_context.as_ref()) else {
        return advisory("baseline runtime context missing; no wall-time policy comparator");
    };
    let Some(parity) = parity_input else {
        return advisory("missing parity input");
    };
    let Some(provenance) = parity.wall_time_provenance.as_ref() else {
        return advisory("missing provenance for wall time");
    };

    let mut reasons = Vec::new();
    push_sample_count_reasons(&mut reasons, parity, provenance);
    push_affinity_reasons(&mut reasons, provenance);
    push_load_reason(&mut reasons, "before", provenance.normalized_load_before);
    push_load_reason(&mut reasons, "after", provenance.normalized_load_after);
    if !runtime_context_matches(expected_context, parity, provenance) {
        reasons.push(format!(
            "baseline runtime context mismatch: baseline={:?}, current={:?}, provenance=workers:{},omc_threads:{}",
            baseline.runtime_context,
            parity.runtime_context,
            provenance.workers_used,
            provenance.omc_threads
        ));
    }

    WallTimeTrustDecision {
        trusted: reasons.is_empty(),
        reasons,
    }
}

fn push_sample_count_reasons(
    reasons: &mut Vec<String>,
    parity: &MslParityGateInput,
    provenance: &MslWallTimeProvenance,
) {
    let Some(wall_sample_count) = parity
        .runtime_ratio_stats
        .as_ref()
        .map(|stats| stats.wall_ratio_both_success.sample_count)
    else {
        reasons.push("missing wall-time runtime samples".to_string());
        return;
    };
    if provenance.omc_fresh_sample_count == 0 {
        reasons.push("no fresh OMC wall-time samples".to_string());
    }
    if provenance.omc_cached_sample_count > 0 {
        reasons.push(format!(
            "cached OMC wall-time samples present: {}",
            provenance.omc_cached_sample_count
        ));
    }
    let classified_samples = provenance
        .omc_fresh_sample_count
        .checked_add(provenance.omc_cached_sample_count);
    if classified_samples != Some(wall_sample_count) {
        reasons.push(format!(
            "wall-time sample count mismatch: fresh={} + cached={} != compared={wall_sample_count}",
            provenance.omc_fresh_sample_count, provenance.omc_cached_sample_count
        ));
    }
}

fn advisory(reason: &str) -> WallTimeTrustDecision {
    WallTimeTrustDecision {
        trusted: false,
        reasons: vec![reason.to_string()],
    }
}

fn push_affinity_reasons(reasons: &mut Vec<String>, provenance: &MslWallTimeProvenance) {
    if provenance.affinity_requested_worker_count != provenance.rumoca_workers_used {
        reasons.push(format!(
            "affinity coverage mismatch: requested={} rumoca_workers_used={}",
            provenance.affinity_requested_worker_count, provenance.rumoca_workers_used
        ));
    }
    if provenance.affinity_requested_worker_count == 0
        || provenance.affinity_applied_worker_count != provenance.affinity_requested_worker_count
        || provenance.affinity_failed_worker_count > 0
    {
        reasons.push(format!(
            "affinity not fully applied: requested={} applied={} failed={}",
            provenance.affinity_requested_worker_count,
            provenance.affinity_applied_worker_count,
            provenance.affinity_failed_worker_count
        ));
    }
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub(super) struct WallTimeStatusContent {
    pub(super) status: &'static str,
    pub(super) trusted: bool,
    pub(super) reasons: Vec<String>,
    pub(super) observed_median: Option<f64>,
    pub(super) baseline_median: Option<f64>,
    pub(super) floor: Option<f64>,
}

pub(super) fn wall_time_status_content(
    baseline: &MslQualityBaseline,
    parity_input: Option<&MslParityGateInput>,
) -> WallTimeStatusContent {
    let observed = parity_input
        .and_then(|parity| parity.runtime_ratio_stats.as_ref())
        .map(|stats| stats.wall_ratio_both_success.median);
    let baseline_median = baseline
        .runtime_ratio_stats
        .as_ref()
        .map(|stats| stats.wall_ratio_both_success.median);
    let Some(baseline_median) = baseline_median else {
        return WallTimeStatusContent {
            status: "ADVISORY",
            trusted: false,
            reasons: vec!["runtime baseline missing".to_string()],
            observed_median: observed,
            baseline_median: None,
            floor: None,
        };
    };
    let floor = Some(baseline_median * (1.0 - RUNTIME_RATIO_MEDIAN_REL_TOLERANCE));
    let trust = wall_time_trust_decision(baseline, parity_input);
    let status = match (trust.trusted, observed, floor) {
        (true, Some(observed), Some(floor)) if observed + SIM_RATE_GATE_EPSILON < floor => "FAIL",
        (true, Some(_), Some(_)) => "PASS",
        _ => "ADVISORY",
    };
    WallTimeStatusContent {
        status,
        trusted: trust.trusted,
        reasons: trust.reasons,
        observed_median: observed,
        baseline_median: Some(baseline_median),
        floor,
    }
}

pub(super) fn format_wall_time_status(content: &WallTimeStatusContent) -> String {
    let reason_text = if content.reasons.is_empty() {
        "trusted provenance".to_string()
    } else {
        content.reasons.join("; ")
    };
    match (
        content.observed_median,
        content.baseline_median,
        content.floor,
    ) {
        (Some(observed), Some(baseline), Some(floor)) => format!(
            "MSL wall speed gate: {} median={observed:.3e}, baseline={baseline:.3e}, floor={floor:.3e} (tolerance={:.1}%); provenance: {reason_text}.",
            content.status,
            RUNTIME_RATIO_MEDIAN_REL_TOLERANCE * 100.0,
        ),
        _ => format!(
            "MSL wall speed gate: {}; provenance: {reason_text}.",
            content.status
        ),
    }
}

fn push_load_reason(reasons: &mut Vec<String>, phase: &str, load: Option<f64>) {
    match load {
        None => reasons.push(format!("missing load {phase}")),
        Some(load) if !load.is_finite() || load < 0.0 => {
            reasons.push(format!("invalid load {phase}: {load}"));
        }
        Some(load) if load > WALL_TIME_NORMALIZED_LOAD_MAX => reasons.push(format!(
            "excessive load {phase}: {load:.3} > {WALL_TIME_NORMALIZED_LOAD_MAX:.3}"
        )),
        Some(_) => {}
    }
}

fn complete_runtime_context(context: Option<&MslParityRuntimeContext>) -> Option<(usize, usize)> {
    let context = context?;
    Some((context.workers_used?, context.omc_threads?))
}

fn runtime_context_matches(
    expected: (usize, usize),
    parity: &MslParityGateInput,
    provenance: &MslWallTimeProvenance,
) -> bool {
    expected.0 > 0
        && expected.1 > 0
        && parity.runtime_context.as_ref().is_some_and(|context| {
            context.workers_used == Some(expected.0) && context.omc_threads == Some(expected.1)
        })
        && provenance.workers_used == expected.0
        && provenance.omc_threads == expected.1
}
