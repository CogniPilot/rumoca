//! Baseline-owned per-model certification roster checks.
//!
//! Workflow artifacts are useful transition diagnostics, but the resolved
//! quality baseline is the only ratchet owner. These checks bind its aggregate
//! strict-high count to exact model identities and prove each identity remains
//! strict-high in the current full-cohort table.

use super::*;
use rumoca_test_msl::msl_tools::band_table::{BandLabel, BandRow};

pub(super) fn validate_certified_strict_high_roster(
    baseline: &MslQualityBaseline,
) -> io::Result<()> {
    let trace = baseline
        .trace_accuracy_stats
        .as_ref()
        .ok_or_else(|| io::Error::other("MSL quality baseline has no trace-accuracy statistics"))?;
    if baseline.certified_strict_high_models.len() != trace.agreement_high {
        return Err(io::Error::other(format!(
            "MSL quality baseline certifies {} strict-high models but owns {} model identities",
            trace.agreement_high,
            baseline.certified_strict_high_models.len()
        )));
    }
    if let Some(model) = baseline
        .certified_strict_high_models
        .iter()
        .find(|model| model.trim().is_empty())
    {
        return Err(io::Error::other(format!(
            "MSL quality baseline contains an empty certified model identity: {model:?}"
        )));
    }
    Ok(())
}

pub(super) fn certified_cohort_regression_reasons(
    baseline: &MslQualityBaseline,
    measurement: &MslParityMeasurement,
) -> Vec<String> {
    let Some(trace) = baseline.trace_accuracy_stats.as_ref() else {
        return vec!["resolved baseline has no trace-accuracy evidence".to_string()];
    };
    if baseline.certified_strict_high_models.len() != trace.agreement_high {
        return vec![format!(
            "resolved baseline certifies {} strict-high models but owns {} model identities",
            trace.agreement_high,
            baseline.certified_strict_high_models.len()
        )];
    }
    let Some(cohort) = measurement.cohort() else {
        return vec![
            "current certification has no full-cohort band table to compare with the baseline roster"
                .to_string(),
        ];
    };
    baseline
        .certified_strict_high_models
        .iter()
        .filter_map(|model| certified_model_regression(model, cohort.table.row(model)))
        .collect()
}

fn certified_model_regression(model: &str, current: Option<&BandRow>) -> Option<String> {
    let Some(row) = current else {
        return Some(format!(
            "baseline-certified strict-high model {model} has no current cohort row"
        ));
    };
    if row.band == BandLabel::High {
        return None;
    }
    let detail = row.exit_reason.map_or_else(
        || format!("moved to the {} band", row.band.as_str()),
        |reason| {
            format!(
                "{}: {}",
                reason.as_str(),
                row.exit_detail.as_deref().unwrap_or("no detail recorded")
            )
        },
    );
    Some(format!(
        "baseline-certified strict-high model {model} regressed ({detail})"
    ))
}
