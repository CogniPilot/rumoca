//! The ratchet on per-model transitions (`rumoca_msl_quality`).
//!
//! When the current snapshot records `model_states`, a model that got worse
//! than its own previous state refuses promotion, a model that must be
//! re-measured holds it, and progress is listed. The old side's states come
//! from the promoted baseline, or, for the first crossing from a baseline that
//! predates them, from the evidence file the crossed reviewed boundary pins by
//! sha256. The changes the crossed boundaries review (`reviewed_changes`) are
//! honored. Accuracy inside the band is judged per model: every scored model
//! whose mean channel error rose must be stated, exactly, by the generated
//! `msl_accuracy_changes.json` keyed to the baseline's sha256.

use anyhow::{Context, Result};
use rumoca_msl_quality::snapshot::{judge_snapshot as judge_shared, measured_accuracy_change};
use rumoca_msl_quality::{AccuracyChange, PerModelVerdict, sha256_hex};
use serde_json::Value;
use std::fs;
use std::path::{Path, PathBuf};

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

/// What the accuracy rule reads: the sha256 of the baseline file judged
/// against, and the checked-in statement's bytes when there is one.
#[derive(Debug, Clone, Default)]
pub(crate) struct AccuracyInput {
    pub(crate) baseline_sha256: String,
    pub(crate) statement: Option<Vec<u8>>,
}

impl AccuracyInput {
    /// The input for `baseline_bytes`, with the statement at `statement_path`
    /// if that file exists.
    pub(crate) fn read(baseline_bytes: &[u8], statement_path: &Path) -> Result<Self> {
        let statement = match fs::read(statement_path) {
            Ok(bytes) => Some(bytes),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => None,
            Err(error) => {
                return Err(error).with_context(|| format!("read {}", statement_path.display()));
            }
        };
        Ok(Self {
            baseline_sha256: sha256_hex(baseline_bytes),
            statement,
        })
    }
}

/// Judge `current` against `baseline` per model, or `None` when the current
/// snapshot predates `model_states` (then the aggregate rule applies).
pub(super) fn judge_snapshot(
    current: &Value,
    baseline: &Value,
    crossed: &[&Value],
    accuracy: &AccuracyInput,
) -> Result<Option<PerModelVerdict>> {
    judge_snapshot_at(current, baseline, crossed, accuracy, &workspace_root())
}

pub(super) fn judge_snapshot_at(
    current: &Value,
    baseline: &Value,
    crossed: &[&Value],
    accuracy: &AccuracyInput,
    root: &Path,
) -> Result<Option<PerModelVerdict>> {
    judge_shared(
        current,
        baseline,
        &accuracy.baseline_sha256,
        crossed,
        accuracy.statement.as_deref(),
        root,
    )
    .map_err(anyhow::Error::msg)
}

/// The accuracy change `current` measures against `baseline`, for the
/// generated statement.
pub(super) fn measured(
    current: &Value,
    baseline: &Value,
    crossed: &[&Value],
) -> Result<Option<AccuracyChange>> {
    measured_accuracy_change(current, baseline, crossed, &workspace_root())
        .map_err(anyhow::Error::msg)
}
