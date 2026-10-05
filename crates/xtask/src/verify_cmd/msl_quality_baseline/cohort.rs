//! Cohort-relative comparison of two MSL quality baselines.
//!
//! Initial-condition deviations, state-set disagreements, and runtime ratios
//! accrue on every compared model whatever its trace band, so a baseline that
//! compares more models raises those totals without any model getting worse.
//! They are compared over the models both baselines measured: per-model
//! evidence when both record it, the reference's whole-cohort total when only
//! its strict-high roster names that cohort, the recorded totals otherwise.
//! Runtime medians follow the quality gate's rule (`runtime_cohort.rs`).
//!
//! The baseline ratchet applies the same rule
//! (`.github/scripts/msl-baseline-cohort.mjs`); the shared cases in
//! `.github/scripts/msl-baseline-cohort-cases.json` hold both to one verdict.

use anyhow::{Result, ensure};
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet};

use super::{RuntimeRatioStats, TraceAccuracyStats};

const RUNTIME_COHORT_MIN_COVERAGE: f64 = 0.90;
const RUNTIME_MEDIAN_REL_TOLERANCE: f64 = 0.35;
const FLOAT_EPSILON: f64 = 1.0e-9;

/// The per-model records a baseline carries beside its aggregates.
#[derive(Debug, Clone, Default, Deserialize)]
pub(super) struct CohortEvidence {
    #[serde(default)]
    certified_strict_high_models: Vec<String>,
    #[serde(default)]
    trace_model_evidence: Option<BTreeMap<String, ModelEvidence>>,
    #[serde(default)]
    runtime_ratio_cohort_models: Vec<String>,
    #[serde(default)]
    runtime_model_ratios: Option<BTreeMap<String, ModelRatio>>,
}

#[derive(Debug, Clone, Deserialize)]
struct ModelEvidence {
    ic_deviation_channels: usize,
    ic_severe_channels: usize,
    ic_violation_mass: f64,
    #[serde(default)]
    state_set: Option<StateSetEvidence>,
}

#[derive(Debug, Clone, Deserialize)]
struct StateSetEvidence {
    rumoca_only: usize,
    omc_only: usize,
    exact: bool,
}

#[derive(Debug, Clone, Copy, Deserialize)]
struct ModelRatio {
    system: f64,
    wall: f64,
}

/// One baseline as the cohort comparison reads it.
#[derive(Clone, Copy)]
pub(super) struct CohortSnapshot<'a> {
    pub(super) evidence: &'a CohortEvidence,
    pub(super) trace: &'a TraceAccuracyStats,
    pub(super) runtime: &'a RuntimeRatioStats,
}

#[derive(Debug, Default)]
pub(super) struct CohortVerdict {
    pub(super) improvements: Vec<String>,
    pub(super) regressions: Vec<String>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Family {
    Trace,
    State,
}

#[derive(Clone, Copy)]
enum Field {
    IcDeviation,
    IcSevere,
    IcMass,
    RumocaOnly,
    OmcOnly,
    Exact,
}

struct Metric {
    label: &'static str,
    family: Family,
    field: Field,
    higher_is_better: bool,
}

const METRICS: [Metric; 6] = [
    Metric {
        label: "initial-condition deviation channels",
        family: Family::Trace,
        field: Field::IcDeviation,
        higher_is_better: false,
    },
    Metric {
        label: "initial-condition severe channels",
        family: Family::Trace,
        field: Field::IcSevere,
        higher_is_better: false,
    },
    Metric {
        label: "initial-condition violation mass",
        family: Family::Trace,
        field: Field::IcMass,
        higher_is_better: false,
    },
    Metric {
        label: "state-set rumoca-only states",
        family: Family::State,
        field: Field::RumocaOnly,
        higher_is_better: false,
    },
    Metric {
        label: "state-set omc-only states",
        family: Family::State,
        field: Field::OmcOnly,
        higher_is_better: false,
    },
    Metric {
        label: "state-set exact matches",
        family: Family::State,
        field: Field::Exact,
        higher_is_better: true,
    },
];

/// A family's population: `values` when the baseline records per-model
/// evidence, only `models` when its strict-high roster provably is its
/// compared set.
struct Cohort<'a> {
    models: BTreeSet<&'a str>,
    values: Option<BTreeMap<&'a str, [f64; 6]>>,
}

/// Compare `candidate` against `reference`; errors when either baseline's
/// per-model evidence disagrees with its own aggregates.
pub(super) fn cohort_comparison(
    reference: CohortSnapshot<'_>,
    candidate: CohortSnapshot<'_>,
) -> Result<CohortVerdict> {
    let mut verdict = CohortVerdict::default();
    let reference_cohorts = cohorts(reference, "reference")?;
    let candidate_cohorts = cohorts(candidate, "candidate")?;
    for metric in &METRICS {
        let index = metric.family as usize;
        if let Some((regressed, line)) = compare_metric(
            metric,
            reference,
            candidate,
            reference_cohorts[index].as_ref(),
            candidate_cohorts[index].as_ref(),
        ) {
            push(&mut verdict, regressed, line);
        }
    }
    compare_runtime(reference, candidate, &mut verdict)?;
    Ok(verdict)
}

fn push(verdict: &mut CohortVerdict, regressed: bool, line: String) {
    if regressed {
        verdict.regressions.push(line);
    } else {
        verdict.improvements.push(line);
    }
}

fn cohorts<'a>(snapshot: CohortSnapshot<'a>, name: &str) -> Result<[Option<Cohort<'a>>; 2]> {
    let Some(evidence) = snapshot.evidence.trace_model_evidence.as_ref() else {
        return Ok(legacy_cohorts(snapshot));
    };
    let mut trace = BTreeMap::new();
    let mut state = BTreeMap::new();
    for (model, entry) in evidence {
        ensure!(
            entry.ic_violation_mass.is_finite() && entry.ic_violation_mass >= 0.0,
            "{name} baseline: {model} records a non-finite initial-condition violation mass"
        );
        let mut values = [0.0; 6];
        values[Field::IcDeviation as usize] = entry.ic_deviation_channels as f64;
        values[Field::IcSevere as usize] = entry.ic_severe_channels as f64;
        values[Field::IcMass as usize] = entry.ic_violation_mass;
        if let Some(set) = entry.state_set.as_ref() {
            let mut state_values = values;
            state_values[Field::RumocaOnly as usize] = set.rumoca_only as f64;
            state_values[Field::OmcOnly as usize] = set.omc_only as f64;
            state_values[Field::Exact as usize] = f64::from(u8::from(set.exact));
            state.insert(model.as_str(), state_values);
        }
        trace.insert(model.as_str(), values);
    }
    let cohorts = [Some(recorded(trace)), Some(recorded(state))];
    ensure_evidence_matches_totals(snapshot, &cohorts, name)?;
    Ok(cohorts)
}

fn recorded(values: BTreeMap<&str, [f64; 6]>) -> Cohort<'_> {
    Cohort {
        models: values.keys().copied().collect(),
        values: Some(values),
    }
}

fn ensure_evidence_matches_totals(
    snapshot: CohortSnapshot<'_>,
    cohorts: &[Option<Cohort<'_>>; 2],
    name: &str,
) -> Result<()> {
    for (family, size) in [
        (
            Family::Trace,
            snapshot.trace.initial_condition.models_compared,
        ),
        (
            Family::State,
            snapshot.trace.state_selection.models_compared,
        ),
    ] {
        let cohort = cohorts[family as usize].as_ref().expect("recorded cohort");
        ensure!(
            Some(cohort.models.len()) == size,
            "{name} baseline: per-model evidence covers {} models, its totals {size:?}",
            cohort.models.len()
        );
    }
    for metric in &METRICS {
        let cohort = cohorts[metric.family as usize]
            .as_ref()
            .expect("recorded cohort");
        let recorded = sum_over(cohort, metric, cohort.models.iter().copied());
        let total = total(snapshot, metric.field);
        ensure!(
            same_value(recorded, total),
            "{name} baseline: per-model evidence sums {} to {recorded}, its totals record {total}",
            metric.label
        );
    }
    Ok(())
}

fn legacy_cohorts(snapshot: CohortSnapshot<'_>) -> [Option<Cohort<'_>>; 2] {
    let roster = &snapshot.evidence.certified_strict_high_models;
    let models = roster.iter().map(String::as_str).collect::<BTreeSet<_>>();
    let trace = snapshot.trace;
    let is_compared = |count: usize| !models.is_empty() && count == models.len();
    let trace_known = models.len() == roster.len()
        && is_compared(trace.models_compared)
        && is_compared(trace.agreement_high)
        && trace
            .initial_condition
            .models_compared
            .is_some_and(is_compared);
    let state_known = trace_known
        && trace
            .state_selection
            .models_compared
            .is_some_and(is_compared);
    let cohort = |known: bool| {
        known.then(|| Cohort {
            models: models.clone(),
            values: None,
        })
    };
    [cohort(trace_known), cohort(state_known)]
}

fn compare_metric(
    metric: &Metric,
    reference: CohortSnapshot<'_>,
    candidate: CohortSnapshot<'_>,
    reference_cohort: Option<&Cohort<'_>>,
    candidate_cohort: Option<&Cohort<'_>>,
) -> Option<(bool, String)> {
    let (Some(reference_cohort), Some(candidate_cohort)) = (
        reference_cohort,
        candidate_cohort.filter(|cohort| cohort.values.is_some()),
    ) else {
        return verdict(
            metric,
            total(reference, metric.field),
            total(candidate, metric.field),
            String::new(),
        );
    };
    let shared = reference_cohort
        .models
        .intersection(&candidate_cohort.models)
        .copied()
        .collect::<Vec<_>>();
    let reference_value = if reference_cohort.values.is_some() {
        sum_over(reference_cohort, metric, shared.iter().copied())
    } else if shared.len() == reference_cohort.models.len() {
        total(reference, metric.field)
    } else {
        return Some((
            true,
            format!(
                "{}: {} of the reference's {} models have no candidate evidence",
                metric.label,
                reference_cohort.models.len() - shared.len(),
                reference_cohort.models.len()
            ),
        ));
    };
    let candidate_value = sum_over(candidate_cohort, metric, shared.iter().copied());
    verdict(
        metric,
        reference_value,
        candidate_value,
        format!(" over {} shared models", shared.len()),
    )
}

fn verdict(
    metric: &Metric,
    reference: f64,
    candidate: f64,
    scope: String,
) -> Option<(bool, String)> {
    if same_value(reference, candidate) {
        return None;
    }
    let improved = if metric.higher_is_better {
        candidate > reference
    } else {
        candidate < reference
    };
    Some((
        !improved,
        format!("{}{scope}: {reference} -> {candidate}", metric.label),
    ))
}

fn sum_over<'a>(
    cohort: &Cohort<'a>,
    metric: &Metric,
    models: impl Iterator<Item = &'a str>,
) -> f64 {
    let values = cohort.values.as_ref().expect("recorded cohort");
    models
        .map(|model| values[model][metric.field as usize])
        .sum()
}

fn total(snapshot: CohortSnapshot<'_>, field: Field) -> f64 {
    let trace = snapshot.trace;
    match field {
        Field::IcDeviation => trace.initial_condition.deviation_channels_total as f64,
        Field::IcSevere => trace.initial_condition.severe_channels_total as f64,
        Field::IcMass => trace.initial_condition.violation_mass_total,
        Field::RumocaOnly => trace.state_selection.total_rumoca_only_states as f64,
        Field::OmcOnly => trace.state_selection.total_omc_only_states as f64,
        Field::Exact => trace.state_selection.exact_state_set_match_models as f64,
    }
}

fn same_value(left: f64, right: f64) -> bool {
    (left - right).abs() <= FLOAT_EPSILON * 1.0_f64.max(left.abs()).max(right.abs())
}

/// The gate's runtime rule: the candidate's medians over the reference's
/// runtime cohort, which it must still time at least 90 percent of, may not
/// fall more than 35 percent below the reference medians. Without a reference
/// cohort or candidate ratios, the recorded whole-run medians are compared.
fn compare_runtime(
    reference: CohortSnapshot<'_>,
    candidate: CohortSnapshot<'_>,
    verdict: &mut CohortVerdict,
) -> Result<()> {
    let cohort = &reference.evidence.runtime_ratio_cohort_models;
    let ratios = candidate_ratios(candidate)?;
    let matched = match ratios {
        Some(ratios) if !cohort.is_empty() => Some(
            cohort
                .iter()
                .filter_map(|model| ratios.get(model).copied())
                .collect::<Vec<_>>(),
        ),
        _ => None,
    };
    if let Some(matched) = matched.as_ref()
        && (matched.len() as f64) < cohort.len() as f64 * RUNTIME_COHORT_MIN_COVERAGE
    {
        verdict.regressions.push(format!(
            "runtime cohort coverage: {}/{} reference models timed",
            matched.len(),
            cohort.len()
        ));
        return Ok(());
    }
    for (label, reference_median, recorded_median, pick) in [
        (
            "runtime system speedup median",
            reference.runtime.system_ratio_both_success.median,
            candidate.runtime.system_ratio_both_success.median,
            (|ratio: &ModelRatio| ratio.system) as fn(&ModelRatio) -> f64,
        ),
        (
            "runtime wall speedup median",
            reference.runtime.wall_ratio_both_success.median,
            candidate.runtime.wall_ratio_both_success.median,
            |ratio: &ModelRatio| ratio.wall,
        ),
    ] {
        let (candidate_median, scope) = match matched.as_ref() {
            Some(matched) => (
                median(matched.iter().map(pick).collect()),
                format!(" over {} reference cohort models", matched.len()),
            ),
            None => (recorded_median, String::new()),
        };
        let line = format!("{label}{scope}: {reference_median} -> {candidate_median}");
        if candidate_median < reference_median * (1.0 - RUNTIME_MEDIAN_REL_TOLERANCE) {
            verdict.regressions.push(line);
        } else if candidate_median > reference_median {
            verdict.improvements.push(line);
        }
    }
    Ok(())
}

/// The candidate's per-model ratios, bound to its recorded cohort and medians.
fn candidate_ratios(
    candidate: CohortSnapshot<'_>,
) -> Result<Option<&BTreeMap<String, ModelRatio>>> {
    let Some(ratios) = candidate.evidence.runtime_model_ratios.as_ref() else {
        return Ok(None);
    };
    ensure!(
        ratios.values().all(|ratio| [ratio.system, ratio.wall]
            .iter()
            .all(|value| value.is_finite() && *value > 0.0)),
        "candidate baseline: runtime_model_ratios must be finite positive ratios"
    );
    let cohort = candidate
        .evidence
        .runtime_ratio_cohort_models
        .iter()
        .map(String::as_str)
        .collect::<BTreeSet<_>>();
    ensure!(
        ratios.keys().map(String::as_str).eq(cohort.iter().copied()),
        "candidate baseline: runtime_model_ratios does not cover runtime_ratio_cohort_models"
    );
    for (recorded, derived) in [
        (
            candidate.runtime.system_ratio_both_success.median,
            median(ratios.values().map(|ratio| ratio.system).collect()),
        ),
        (
            candidate.runtime.wall_ratio_both_success.median,
            median(ratios.values().map(|ratio| ratio.wall).collect()),
        ),
    ] {
        ensure!(
            same_value(recorded, derived),
            "candidate baseline: runtime_model_ratios give a median of {derived}, the stats record {recorded}"
        );
    }
    Ok(Some(ratios))
}

fn median(mut values: Vec<f64>) -> f64 {
    values.sort_by(f64::total_cmp);
    let middle = values.len() / 2;
    match values.len() {
        0 => f64::NAN,
        len if len % 2 == 1 => values[middle],
        _ => (values[middle - 1] + values[middle]) / 2.0,
    }
}

#[cfg(test)]
mod tests;
