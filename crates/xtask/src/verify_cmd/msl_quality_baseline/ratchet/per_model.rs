//! The ratchet on per-model transitions (`rumoca_msl_quality`).
//!
//! When the current snapshot records `model_states`, a model that got worse
//! than its own previous state refuses promotion, a model that must be
//! re-measured holds it, and progress is listed. The old side's states come
//! from the promoted baseline, or, for the first crossing from a baseline that
//! predates them, from the evidence file the crossed reviewed boundary pins by
//! sha256. Accuracy inside the band is judged per model: every scored model
//! whose mean channel error rose must be stated, exactly, by a crossed
//! boundary.

use anyhow::{Context, Result, bail, ensure};
use rumoca_msl_quality::{
    AccuracyChange, ModelStates, Regression, ReviewedChanges, Transition, accuracy_change,
    parse_evidence_states, shared_floors, transitions,
};
use serde_json::Value;
use sha2::{Digest, Sha256};
use std::fs;
use std::path::{Path, PathBuf};

/// What the per-model rule decided, as the ratchet's lines.
#[derive(Debug, Default)]
pub(super) struct PerModelVerdict {
    pub(super) regressions: Vec<String>,
    pub(super) inconclusive: Vec<String>,
    pub(super) improvements: Vec<String>,
    /// Printed, not ruled on.
    pub(super) report: Vec<String>,
}

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn states_at(snapshot: &Value, name: &str) -> Result<Option<ModelStates>> {
    snapshot
        .get("model_states")
        .map(|states| {
            serde_json::from_value(states.clone())
                .with_context(|| format!("{name}: read model_states"))
        })
        .transpose()
}

/// The old side's states: the baseline's own, or the evidence a crossed
/// boundary pins for a baseline that predates them.
fn previous_states(baseline: &Value, crossed: &[&Value], root: &Path) -> Result<ModelStates> {
    if let Some(states) = states_at(baseline, "baseline snapshot")? {
        return Ok(states);
    }
    let Some(evidence) = crossed
        .iter()
        .find_map(|link| link.get("evidence_model_states"))
    else {
        bail!(
            "cannot ratchet per model: the baseline records no model_states and no crossed \
             reference boundary pins evidence for them"
        );
    };
    let file = evidence
        .get("file")
        .and_then(Value::as_str)
        .context("evidence_model_states.file must be a string")?;
    let sha256 = evidence
        .get("sha256")
        .and_then(Value::as_str)
        .context("evidence_model_states.sha256 must be a string")?;
    let path = root.join(file);
    let bytes = fs::read(&path).with_context(|| format!("read {}", path.display()))?;
    let actual = format!("{:x}", Sha256::digest(&bytes));
    ensure!(
        actual == sha256,
        "evidence model states {file} have sha256 {actual}, the reviewed boundary pins {sha256}"
    );
    let text = String::from_utf8(bytes).with_context(|| format!("{file} is not UTF-8"))?;
    let evidence = parse_evidence_states(&text).map_err(anyhow::Error::msg)?;
    Ok(evidence.model_states)
}

/// The accuracy change the crossed boundaries state, if any.
fn stated_accuracy_change(crossed: &[&Value]) -> Result<Option<AccuracyChange>> {
    crossed
        .iter()
        .filter_map(|link| link.get("accuracy_change"))
        .map(|stated| {
            serde_json::from_value(stated.clone()).context("read a boundary's accuracy_change")
        })
        .next()
        .transpose()
}

/// Judge `current` against `baseline` per model, or `None` when the current
/// snapshot predates `model_states` (then the aggregate rule applies).
///
/// Across an OMC context change only OMC-independent transitions count (a
/// stage drop, a target that leaves), as the aggregate rule compares only
/// OMC-independent metrics there.
pub(super) fn judge(
    current: &Value,
    baseline: &Value,
    crossed: &[&Value],
    omc_context_changed: bool,
) -> Result<Option<PerModelVerdict>> {
    judge_at(
        current,
        baseline,
        crossed,
        omc_context_changed,
        &workspace_root(),
    )
}

pub(super) fn judge_at(
    current: &Value,
    baseline: &Value,
    crossed: &[&Value],
    omc_context_changed: bool,
    root: &Path,
) -> Result<Option<PerModelVerdict>> {
    let Some(after) = states_at(current, "current snapshot")? else {
        return Ok(None);
    };
    let before = previous_states(baseline, crossed, root)?;
    let mut verdict = PerModelVerdict::default();
    for (model, transition) in transitions(&before, &after, &ReviewedChanges::default()) {
        match transition {
            Transition::Regressed(why) => {
                let omc_independent = matches!(
                    why,
                    Regression::StageDropped { .. } | Regression::LeftTargets
                );
                if omc_independent || !omc_context_changed {
                    verdict.regressions.push(format!("{model}: {why}"));
                }
            }
            Transition::Inconclusive(why) => verdict.inconclusive.push(format!("{model}: {why}")),
            Transition::Progressed => verdict.improvements.push(format!("{model}: progressed")),
            Transition::New => verdict
                .improvements
                .push(format!("{model}: new sim target")),
            Transition::Reviewed | Transition::Unchanged => {}
        }
    }
    if omc_context_changed {
        return Ok(Some(verdict));
    }
    let floors = shared_floors(&before, &after);
    verdict.regressions.extend(floors.regressions());
    let (was, is) = floors.mean_channel_bounded_l1;
    verdict.report.push(format!(
        "mean channel bounded L1 over {} models scored in both: {was:.6e} -> {is:.6e} (reported)",
        floors.models
    ));
    let measured = accuracy_change(&before, &after);
    let stated = stated_accuracy_change(crossed)?;
    if measured != stated {
        match &measured {
            Some(change) => verdict.regressions.push(format!(
                "{} models' mean channel error rose and the crossed boundary does not state \
                 exactly that: {}",
                change.rose.len(),
                change
                    .rose
                    .iter()
                    .map(|model| format!("{} {:e} -> {:e}", model.model, model.before, model.after))
                    .collect::<Vec<_>>()
                    .join(", ")
            )),
            None => verdict.regressions.push(
                "a crossed boundary states an accuracy change, but no model's mean channel \
                 error rose"
                    .to_string(),
            ),
        }
    }
    Ok(Some(verdict))
}
