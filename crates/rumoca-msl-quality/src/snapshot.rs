//! Judging quality snapshots (JSON) per model: the one implementation the PR
//! gate and the promotion ratchet share.
//!
//! The old side's states come from the baseline, or, for the first crossing
//! from a baseline that predates them, from the evidence file a crossed
//! reviewed boundary pins by sha256. The changes the crossed boundaries review
//! (`reviewed_changes`) are honored, and the accuracy statement must be keyed
//! to the baseline's sha256.

use crate::{
    AccuracyChange, ModelStates, PerModelVerdict, ReviewedChanges, accuracy_change, judge,
    stated_accuracy, verify_evidence,
};
use serde_json::Value;
use std::path::Path;

/// A snapshot's `model_states`, or `None` when it predates them.
pub fn states_at(snapshot: &Value, name: &str) -> Result<Option<ModelStates>, String> {
    snapshot
        .get("model_states")
        .map(|states| {
            serde_json::from_value(states.clone())
                .map_err(|error| format!("{name}: read model_states: {error}"))
        })
        .transpose()
}

/// The links of `current`'s reference boundary chain that a judgment against
/// `baseline` crosses: those reaching a quality gate version above the
/// baseline's.
pub fn crossed_links<'a>(current: &'a Value, baseline: &Value) -> Vec<&'a Value> {
    let version = |value: &Value, key: &str| value.get(key).and_then(Value::as_u64);
    let from = version(baseline, "quality_gate_version").unwrap_or(0);
    let mut crossed = Vec::new();
    let mut link = current.get("reference_boundary_migration");
    while let Some(boundary) = link.filter(|link| link.is_object()) {
        if version(boundary, "to_quality_gate_version").unwrap_or(0) <= from {
            break;
        }
        crossed.push(boundary);
        link = boundary.get("previous");
    }
    crossed
}

/// The old side's states: the baseline's own, or the evidence a crossed
/// boundary pins for a baseline that predates them (`file` relative to
/// `evidence_root`).
pub fn previous_states(
    baseline: &Value,
    crossed: &[&Value],
    evidence_root: &Path,
) -> Result<ModelStates, String> {
    if let Some(states) = states_at(baseline, "baseline snapshot")? {
        return Ok(states);
    }
    let Some(evidence) = crossed
        .iter()
        .find_map(|link| link.get("evidence_model_states"))
    else {
        return Err(
            "cannot judge per model: the baseline records no model_states and no crossed \
                    reference boundary pins evidence for them"
                .to_string(),
        );
    };
    let field = |key: &str| {
        evidence
            .get(key)
            .and_then(Value::as_str)
            .ok_or_else(|| format!("evidence_model_states.{key} must be a string"))
    };
    let (file, sha256) = (field("file")?, field("sha256")?);
    let path = evidence_root.join(file);
    let bytes =
        std::fs::read(&path).map_err(|error| format!("read {}: {error}", path.display()))?;
    let evidence = verify_evidence(&bytes, sha256).map_err(|error| format!("{file}: {error}"))?;
    Ok(evidence.model_states)
}

/// Judge `current` against `baseline` (whose file has `baseline_sha256`) per
/// model, or `None` when `current` predates `model_states`. `statement` is
/// the checked-in accuracy statement's bytes, if there is one.
pub fn judge_snapshot(
    current: &Value,
    baseline: &Value,
    baseline_sha256: &str,
    crossed: &[&Value],
    statement: Option<&[u8]>,
    evidence_root: &Path,
) -> Result<Option<PerModelVerdict>, String> {
    let Some(after) = states_at(current, "current snapshot")? else {
        return Ok(None);
    };
    let before = previous_states(baseline, crossed, evidence_root)?;
    let reviewed = ReviewedChanges::from_links(crossed)?;
    let stated = stated_accuracy(statement, baseline_sha256)?;
    let omc_context_changed = current.get("omc_version") != baseline.get("omc_version");
    Ok(Some(judge(
        &before,
        &after,
        &reviewed,
        stated.as_ref(),
        omc_context_changed,
    )))
}

/// The accuracy change `current` measures against `baseline`, for the
/// generated statement.
pub fn measured_accuracy_change(
    current: &Value,
    baseline: &Value,
    crossed: &[&Value],
    evidence_root: &Path,
) -> Result<Option<AccuracyChange>, String> {
    let after = states_at(current, "current snapshot")?
        .ok_or("the current snapshot records no model_states")?;
    let before = previous_states(baseline, crossed, evidence_root)?;
    Ok(accuracy_change(&before, &after))
}
