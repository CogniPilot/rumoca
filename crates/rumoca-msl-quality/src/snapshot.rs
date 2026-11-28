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
/// baseline's. A snapshot or link without its version is refused: read as
/// zero, it would cross every link.
pub fn crossed_links<'a>(current: &'a Value, baseline: &Value) -> Result<Vec<&'a Value>, String> {
    let version = |value: &Value, key: &str, what: &str| {
        value
            .get(key)
            .and_then(Value::as_u64)
            .ok_or_else(|| format!("{what} has no {key}"))
    };
    let from = version(baseline, "quality_gate_version", "the baseline")?;
    let to = version(current, "quality_gate_version", "the current snapshot")?;
    if from > to {
        return Err(format!(
            "the baseline's quality gate version {from} is newer than the run's {to}"
        ));
    }
    let mut crossed = Vec::new();
    let mut link = current.get("reference_boundary_migration");
    while from < to
        && let Some(boundary) = link.filter(|link| link.is_object())
    {
        if version(boundary, "to_quality_gate_version", "a reference boundary")? <= from {
            break;
        }
        crossed.push(boundary);
        link = boundary.get("previous");
    }
    Ok(crossed)
}

/// Whether the old side has states to judge against: the baseline's own, or
/// evidence a crossed boundary pins. Only a baseline without either has none;
/// a pinned evidence file that cannot be read or does not match its pin is an
/// error for [`previous_states`], never "no old side".
pub fn has_old_side(baseline: &Value, crossed: &[&Value]) -> bool {
    baseline.get("model_states").is_some()
        || crossed
            .iter()
            .any(|link| link.get("evidence_model_states").is_some())
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

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn chain() -> Value {
        json!({
            "to_quality_gate_version": 15,
            "previous": { "to_quality_gate_version": 14, "previous": { "to_quality_gate_version": 13 } },
        })
    }

    #[test]
    fn only_the_links_above_the_baseline_are_crossed() {
        let current =
            json!({ "quality_gate_version": 15, "reference_boundary_migration": chain() });
        let crossed = crossed_links(&current, &json!({ "quality_gate_version": 13 })).unwrap();
        assert_eq!(crossed.len(), 2);
        let same = crossed_links(&current, &json!({ "quality_gate_version": 15 })).unwrap();
        assert!(same.is_empty());
    }

    #[test]
    fn a_missing_version_is_refused_instead_of_crossing_every_link() {
        let current =
            json!({ "quality_gate_version": 15, "reference_boundary_migration": chain() });
        let error = crossed_links(&current, &json!({})).unwrap_err();
        assert!(
            error.contains("baseline has no quality_gate_version"),
            "{error}"
        );
        let unversioned = json!({ "reference_boundary_migration": chain() });
        assert!(crossed_links(&unversioned, &json!({ "quality_gate_version": 13 })).is_err());
        let broken = json!({
            "quality_gate_version": 15,
            "reference_boundary_migration": { "previous": {} },
        });
        let error = crossed_links(&broken, &json!({ "quality_gate_version": 13 })).unwrap_err();
        assert!(error.contains("reference boundary has no"), "{error}");
        let newer = crossed_links(&current, &json!({ "quality_gate_version": 16 })).unwrap_err();
        assert!(newer.contains("newer"), "{newer}");
    }

    #[test]
    fn tampered_evidence_is_an_error_not_no_old_side() {
        let dir = std::env::temp_dir().join(format!("msl-evidence-tamper-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("evidence.json"), "{}").unwrap();
        let baseline = json!({ "quality_gate_version": 14 });
        let link = json!({
            "to_quality_gate_version": 15,
            "evidence_model_states": { "file": "evidence.json", "sha256": "0".repeat(64) },
        });
        assert!(has_old_side(&baseline, &[&link]));
        let error = previous_states(&baseline, &[&link], &dir).unwrap_err();
        assert!(error.contains("the reviewed boundary pins"), "{error}");
        assert!(!has_old_side(&baseline, &[]));
        let _ = std::fs::remove_dir_all(&dir);
    }
}
