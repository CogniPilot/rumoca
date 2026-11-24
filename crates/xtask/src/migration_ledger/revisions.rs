//! Owner-directed, path-scoped revisions of the legacy-backend freeze.
//!
//! The audit ledger stays byte-identical. A revision names its decision and
//! the exact before/after identity of every covered path it changes; nothing
//! else may move. There is no flag, wildcard or count that admits a change.

#[cfg(test)]
mod tests;

use super::{BACKEND, Ledger, SourceRegion, relative_path};
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
    /// Pinned source regions of the revised paths that move or change.
    #[serde(default)]
    regions: Vec<RegionTransition>,
}

/// `before: None` admits exactly one new covered file; removal is not offered.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct Transition {
    path: String,
    before: Option<String>,
    after: String,
}

/// The new extent and identity of one pinned source region of a path the same
/// revision revises. `before` is the region's identity in force.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct RegionTransition {
    path: String,
    name: String,
    before: String,
    start_byte: usize,
    end_byte: usize,
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

/// The pinned source regions the revisions require. The audit ledger stays
/// byte-identical: a revision re-pins a region it names, of a path it revises,
/// and every other region keeps the ledger's extent and identity.
pub(super) fn regions_in_force(ledger: &Ledger, text: &str) -> Result<Vec<SourceRegion>> {
    let record: Revisions =
        serde_json::from_str(text).context("invalid backend revision record schema")?;
    let mut regions = ledger.source_regions.clone();
    for revision in &record.revisions {
        let id = &revision.id;
        let revised: BTreeSet<_> = revision.files.iter().map(|file| &file.path).collect();
        let mut identities = BTreeSet::new();
        for transition in &revision.regions {
            let (path, name) = (&transition.path, &transition.name);
            ensure!(
                revised.contains(path),
                "{id} re-pins region {name} of {path}, which it does not revise"
            );
            ensure!(
                identities.insert((path, name)),
                "{id} repeats region {name} of {path}"
            );
            ensure!(
                is_digest(&transition.before)
                    && is_digest(&transition.after)
                    && transition.start_byte < transition.end_byte,
                "{id} has an invalid hash or extent for region {name} of {path}"
            );
            let region = regions
                .iter_mut()
                .find(|region| region.path == *path && region.name == *name)
                .with_context(|| format!("{id} re-pins unknown region {name} of {path}"))?;
            ensure!(
                region.sha256 == transition.before,
                "{id} before-identity of region {name} of {path} does not match the region in force"
            );
            ensure!(
                (region.start_byte, region.end_byte, &region.sha256)
                    != (
                        transition.start_byte,
                        transition.end_byte,
                        &transition.after
                    ),
                "{id} records no change for region {name} of {path}"
            );
            region.start_byte = transition.start_byte;
            region.end_byte = transition.end_byte;
            region.sha256.clone_from(&transition.after);
        }
    }
    Ok(regions)
}
