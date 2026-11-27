//! The gate on per-model transitions (`rumoca_msl_quality::judge`).
//!
//! When the checked-in baseline records `model_states`, a run is judged model
//! by model against it: a model worse than its own baseline state fails, a
//! model that must be re-measured (a watchdog kill, a failed OMC reference)
//! fails until it is, and every model whose mean channel error rose is listed
//! (reported, not ruled on: a pull request may trade accuracy, and the
//! promotion states it). This replaces the stage-count, aggregate trace and
//! certified-cohort reasons; the soundness, tensor, runtime, roster and
//! exception-file checks still apply.

use super::*;
use rumoca_msl_quality::{AccuracyRule, ModelStates, PerModelVerdict, judge};

fn raw_snapshot(path: &Path) -> io::Result<serde_json::Value> {
    serde_json::from_slice(&fs::read(path)?).map_err(|error| {
        io::Error::other(format!(
            "invalid quality snapshot {}: {error}",
            path.display()
        ))
    })
}

fn states_at(snapshot: &serde_json::Value, path: &Path) -> io::Result<Option<ModelStates>> {
    snapshot
        .get("model_states")
        .map(|states| {
            serde_json::from_value(states.clone()).map_err(|error| {
                io::Error::other(format!("{}: model_states: {error}", path.display()))
            })
        })
        .transpose()
}

/// Judge the current snapshot against the checked-in baseline per model, or
/// `None` when the baseline predates `model_states` (then the aggregate rule
/// applies). A baseline with states and a run without them is refused.
pub(super) fn per_model_verdict(baseline_path: &Path) -> io::Result<Option<PerModelVerdict>> {
    let baseline = raw_snapshot(baseline_path)?;
    let Some(before) = states_at(&baseline, baseline_path)? else {
        return Ok(None);
    };
    let current_path = msl_quality_current_path();
    let current = raw_snapshot(&current_path)?;
    let after = states_at(&current, &current_path)?.ok_or_else(|| {
        io::Error::other(format!(
            "{} records no model_states, but the baseline does",
            current_path.display()
        ))
    })?;
    let omc = |snapshot: &serde_json::Value| snapshot.get("omc_version").cloned();
    let omc_context_changed = omc(&baseline) != omc(&current);
    Ok(Some(judge(
        &before,
        &after,
        AccuracyRule::Reported,
        omc_context_changed,
    )))
}

/// The gate's failure under the per-model rule, after the parity and context
/// checks every run must pass, and with the checks transitions do not replace.
pub(super) fn per_model_gate_failure_message(
    gate_input: MslQualityGateInput<'_>,
    baseline: &MslQualityBaseline,
    measurement: &MslParityMeasurement,
    verdict: &PerModelVerdict,
) -> Option<String> {
    if let Some(reason) = measurement.gate_failure_reason() {
        return Some(reason);
    }
    let parity_input = measurement.gate_input();
    if let Some(reason) = msl_quality_context_mismatch_reason(gate_input, baseline, parity_input) {
        return Some(format!(
            "MSL quality baseline context mismatch: {reason}. Update {} only with explicit review approval",
            MSL_QUALITY_BASELINE_FILE_REL
        ));
    }
    for line in verdict.report.iter().chain(&verdict.improvements) {
        println!("MSL per-model: {line}");
    }
    let mut reasons = verdict.regressions.clone();
    reasons.extend(
        verdict
            .inconclusive
            .iter()
            .map(|line| format!("{line} (re-measure first)")),
    );
    push_tensor_preservation_regression_reasons(&mut reasons, gate_input, baseline);
    push_trace_soundness_reasons(&mut reasons, gate_input, baseline, parity_input);
    push_runtime_ratio_regression_reasons(&mut reasons, baseline, parity_input);
    (!reasons.is_empty()).then(|| reasons.join("; "))
}
