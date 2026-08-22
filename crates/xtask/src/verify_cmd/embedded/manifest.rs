//! The checked-in embedded budget: which artifacts are gated, and how big each
//! one is allowed to be.
//!
//! Every ceiling lives in `infra/verification/embedded-budget.json` rather than
//! in Rust, so raising one is a reviewed diff on a data file instead of an edit
//! buried in gate logic. That placement is the whole mechanism: the ceilings
//! are fall-only in spirit, lowered as size work lands and never raised without
//! a justification someone signed off on, and the only way to enforce a social
//! rule like that is to make the change visible where review happens.
//!
//! The loader is strict on purpose: `deny_unknown_fields` everywhere, an
//! explicit schema version, and a validation pass that refuses a manifest which
//! cannot be measured (a zero ceiling, a duplicate id, an absolute entry
//! point). A manifest the gate silently half-understood would certify less than
//! no manifest at all.

use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

/// Location of the budget manifest, relative to the workspace root.
pub(crate) const MANIFEST_PATH: &str = "infra/verification/embedded-budget.json";

/// Bumped whenever a field changes meaning. A manifest written for another
/// version is refused rather than reinterpreted.
pub(crate) const MANIFEST_SCHEMA_VERSION: u32 = 1;

/// The whole gated budget.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub(crate) struct BudgetManifest {
    pub(crate) schema_version: u32,
    /// The doc comment carried with the data: what a ceiling means and which
    /// direction it is allowed to move. Prose for the reviewer, never parsed,
    /// but required to be present so the rule travels with the file it governs.
    pub(crate) ceiling_policy: String,
    pub(crate) entries: Vec<BudgetEntry>,
}

/// One gated (model, target) artifact. A model built for two targets appears
/// twice, each with its own id and its own ceilings, so a failure names exactly
/// one artifact.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub(crate) struct BudgetEntry {
    /// Stable key for this row, used in the report and in the summary JSON.
    pub(crate) id: String,
    /// Fully qualified Modelica class name.
    pub(crate) model: String,
    /// Package entry point passed to the compiler, relative to the models root.
    pub(crate) entry_point: String,
    /// `rumoca compile --target` value.
    pub(crate) target: String,
    /// Why this artifact is gated. Prose for the reviewer, not parsed.
    pub(crate) why: String,
    pub(crate) budget: Budget,
}

/// The ceilings for one artifact, beside the measurement they were set from.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub(crate) struct Budget {
    /// Ceiling on the summed `.text` of every emitted translation unit, in
    /// bytes. The sum rather than the largest file: an artifact that moved code
    /// from one translation unit into another did not get smaller.
    pub(crate) text_bytes: u64,
    /// Ceiling on `sizeof(<Model>State)` for the target ABI, in bytes. This is
    /// the single allocation the integrator has to find room for, so it is
    /// gated separately from code size.
    pub(crate) state_bytes: u64,
    pub(crate) measured: Measured,
}

/// What was actually measured when the ceilings above were set.
///
/// Recorded so a reviewer reading a ceiling can see the headroom it was chosen
/// with, and so the gate can print the drift since the ceiling was set rather
/// than only the distance to the limit.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub(crate) struct Measured {
    pub(crate) text_bytes: u64,
    pub(crate) state_bytes: u64,
    /// When, with which cross compiler, and under which flags. Prose for the
    /// reviewer: a ceiling is a statement about one toolchain, and this field
    /// is where that toolchain is named.
    pub(crate) comment: String,
}

pub(crate) fn manifest_path(root: &Path) -> PathBuf {
    root.join(MANIFEST_PATH)
}

/// Read and validate the manifest.
pub(crate) fn load(path: &Path) -> Result<BudgetManifest> {
    let raw = fs::read_to_string(path)
        .with_context(|| format!("failed to read embedded budget manifest {}", path.display()))?;
    let manifest: BudgetManifest = serde_json::from_str(&raw).with_context(|| {
        format!(
            "failed to parse embedded budget manifest {}",
            path.display()
        )
    })?;
    validate(&manifest).with_context(|| {
        format!(
            "embedded budget manifest {} is not admissible",
            path.display()
        )
    })?;
    Ok(manifest)
}

pub(crate) fn validate(manifest: &BudgetManifest) -> Result<()> {
    ensure!(
        manifest.schema_version == MANIFEST_SCHEMA_VERSION,
        "embedded budget manifest schema_version {} is not the supported version {}",
        manifest.schema_version,
        MANIFEST_SCHEMA_VERSION
    );
    ensure!(
        !manifest.ceiling_policy.trim().is_empty(),
        "ceiling_policy must state which direction a ceiling is allowed to move"
    );
    ensure!(
        !manifest.entries.is_empty(),
        "an embedded budget manifest with no entries would certify nothing"
    );
    let mut seen = BTreeSet::new();
    for entry in &manifest.entries {
        ensure!(
            seen.insert(entry.id.as_str()),
            "duplicate embedded budget entry id `{}`",
            entry.id
        );
        validate_entry(entry).with_context(|| format!("embedded budget entry `{}`", entry.id))?;
    }
    Ok(())
}

fn validate_entry(entry: &BudgetEntry) -> Result<()> {
    ensure!(!entry.id.trim().is_empty(), "id must not be empty");
    ensure!(!entry.model.trim().is_empty(), "model must not be empty");
    ensure!(
        !entry.entry_point.trim().is_empty(),
        "entry_point must not be empty"
    );
    ensure!(!entry.target.trim().is_empty(), "target must not be empty");
    ensure!(
        !entry.why.trim().is_empty(),
        "why must say what this row is for"
    );
    ensure!(
        !Path::new(&entry.entry_point).is_absolute(),
        "entry_point `{}` must be relative to the models root",
        entry.entry_point
    );
    validate_budget(&entry.budget)
}

fn validate_budget(budget: &Budget) -> Result<()> {
    ensure!(
        budget.text_bytes > 0,
        "text_bytes must be a positive ceiling; a zero ceiling no artifact can meet is a \
         permanently red row, not a budget"
    );
    ensure!(
        budget.state_bytes > 0,
        "state_bytes must be a positive ceiling; a zero ceiling no artifact can meet is a \
         permanently red row, not a budget"
    );
    ensure!(
        budget.measured.text_bytes > 0 && budget.measured.state_bytes > 0,
        "measured sizes must be positive: a ceiling recorded against a zero measurement \
         states no headroom at all"
    );
    ensure!(
        !budget.measured.comment.trim().is_empty(),
        "measured.comment must name the toolchain and date the ceilings were set from"
    );
    Ok(())
}
