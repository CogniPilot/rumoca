//! Owner-directed, path-scoped revisions of the legacy-backend freeze.
//!
//! The audit ledger stays byte-identical. A revision names its decision and
//! the exact before/after identity of every covered path it changes; nothing
//! else may move. There is no flag, wildcard or count that admits a change.

#[cfg(test)]
mod tests;

use super::{BACKEND, Ledger, relative_path};
use anyhow::{Context, Result, ensure};
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct Revisions {
    schema: u32,
    revisions: Vec<Revision>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct Revision {
    id: String,
    decision: String,
    change: String,
    resolves: Vec<String>,
    files: Vec<Transition>,
}

/// `before: None` admits exactly one new covered file; removal is not offered.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct Transition {
    path: String,
    before: Option<String>,
    after: String,
}

fn is_digest(hash: &str) -> bool {
    hash.len() == 64 && hash.bytes().all(|c| c.is_ascii_hexdigit())
}

/// The covered inventory the revisions require, and the findings they resolve.
/// Revisions apply in order; each `before` must equal the identity in force.
pub(super) fn expected_inventory(
    ledger: &Ledger,
    text: &str,
) -> Result<(BTreeMap<String, String>, BTreeSet<String>)> {
    let record: Revisions =
        serde_json::from_str(text).context("invalid backend revision record schema")?;
    ensure!(record.schema == 1, "unsupported backend revision schema");
    let mut expected = ledger.files.clone();
    let mut resolved = BTreeSet::new();
    for (index, revision) in record.revisions.iter().enumerate() {
        let id = &revision.id;
        ensure!(
            *id == format!("R{:03}", index + 1),
            "backend revisions must be numbered R001 onward without gaps"
        );
        ensure!(
            !revision.decision.trim().is_empty() && !revision.change.trim().is_empty(),
            "{id} loses its owner decision or change statement"
        );
        ensure!(!revision.files.is_empty(), "{id} names no covered path");
        let mut paths = BTreeSet::new();
        for transition in &revision.files {
            let path = &transition.path;
            relative_path(path)?;
            ensure!(
                path.starts_with(&format!("{BACKEND}/")),
                "{id} revises out-of-scope path {path}"
            );
            ensure!(paths.insert(path.as_str()), "{id} repeats {path}");
            ensure!(
                is_digest(&transition.after) && transition.before.as_deref().is_none_or(is_digest),
                "{id} has an invalid hash for {path}"
            );
            ensure!(
                expected.get(path) == transition.before.as_ref(),
                "{id} before-identity of {path} does not match the frozen source in force"
            );
            ensure!(
                transition.before.as_ref() != Some(&transition.after),
                "{id} records no change for {path}"
            );
            expected.insert(path.clone(), transition.after.clone());
        }
        for finding in &revision.resolves {
            let row = ledger.findings.iter().find(|row| row.id == *finding);
            let row = row.with_context(|| format!("{id} resolves unknown finding {finding}"))?;
            ensure!(
                paths.contains(row.path.as_str()),
                "{id} cannot resolve {finding} without revising {}",
                row.path
            );
            ensure!(resolved.insert(finding.clone()), "{finding} resolved twice");
        }
    }
    Ok((expected, resolved))
}
