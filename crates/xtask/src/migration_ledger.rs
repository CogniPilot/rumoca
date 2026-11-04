//! Exact legacy-backend change freeze, not a claim of print-only compliance.
//!
//! Retirement remains fail-closed until the real issued-plan authorization and
//! executable-correspondence checks can bind the replacement consumer.

#[cfg(test)]
mod tests;

pub mod baseline;
pub mod captured;

// Component controls are ordinary Cargo tests. This byte relation is not yet
// consumed by the production authorization path, which remains fail-closed.
#[cfg(test)]
mod source_complement;

use anyhow::{Context, Result, bail, ensure};
use serde::Deserialize;
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Component, Path};

const REFERENCE: &str = include_str!("../policy/backend-migration-ledger.json");
const REFERENCE_SHA256: &str = "51f97c10ba2b189ac327597782b00d8bbee3c3b21013186cc6314778322094c0";
const LEDGER_PATH: &str = "crates/xtask/policy/backend-migration-ledger.json";
const BACKEND: &str = "crates/rumoca-phase-codegen";

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct Ledger {
    schema: u32,
    source_revision: String,
    scope: Vec<String>,
    findings: Vec<Finding>,
    files: BTreeMap<String, String>,
    source_regions: Vec<SourceRegion>,
}

/// Supporting source identities, never an exemption or retirement permission.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct SourceRegion {
    path: String,
    name: String,
    start_byte: usize,
    end_byte: usize,
    sha256: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct Finding {
    id: String,
    kind: String,
    path: String,
    line: usize,
    anchor: String,
    decision: String,
    owner: String,
}

fn digest(bytes: &[u8]) -> String {
    format!("{:x}", Sha256::digest(bytes))
}

fn relative_path(path: &str) -> Result<&Path> {
    let path = Path::new(path);
    ensure!(
        !path.as_os_str().is_empty()
            && path
                .components()
                .all(|part| matches!(part, Component::Normal(_))),
        "migration ledger path must be a plain relative path: {}",
        path.display()
    );
    Ok(path)
}

fn parse_ledger(text: &str) -> Result<Ledger> {
    let ledger: Ledger = serde_json::from_str(text).context("invalid migration ledger schema")?;
    ensure!(ledger.schema == 1, "unsupported migration ledger schema");
    ensure!(
        ledger.source_revision == "187eee7a8a1b321302747be733678c49ac3bdfa1",
        "migration source revision cannot be rebased to accept backend changes"
    );
    ensure!(
        ledger.scope == [BACKEND],
        "migration coverage cannot be narrowed"
    );
    let mut ids = BTreeSet::new();
    for finding in &ledger.findings {
        validate_finding(finding, &ledger.files)?;
        ensure!(
            ids.insert(finding.id.clone()),
            "duplicate finding {}",
            finding.id
        );
    }
    let expected = (1..=96).map(|n| format!("D{n:03}")).collect();
    ensure!(
        ids == expected,
        "migration ledger must retain exactly D001-D096"
    );
    for (path, hash) in &ledger.files {
        relative_path(path)?;
        ensure!(
            path.starts_with(&format!("{BACKEND}/")),
            "out-of-scope snapshot {path}"
        );
        ensure!(
            hash.len() == 64 && hash.bytes().all(|c| c.is_ascii_hexdigit()),
            "invalid hash for {path}"
        );
    }
    validate_regions(&ledger)?;
    Ok(ledger)
}

fn validate_regions(ledger: &Ledger) -> Result<()> {
    let mut identities = BTreeSet::new();
    for region in &ledger.source_regions {
        ensure!(
            ledger.files.contains_key(&region.path),
            "uncovered source region"
        );
        ensure!(
            !region.name.is_empty()
                && region
                    .name
                    .bytes()
                    .all(|b| b.is_ascii_alphanumeric() || b == b'_')
                && region.start_byte < region.end_byte,
            "invalid source-region identity or extent"
        );
        ensure!(
            region.sha256.len() == 64 && region.sha256.bytes().all(|b| b.is_ascii_hexdigit()),
            "invalid source-region hash"
        );
        ensure!(
            identities.insert((&region.path, &region.name)),
            "duplicate source region"
        );
    }
    Ok(())
}

fn validate_finding(finding: &Finding, files: &BTreeMap<String, String>) -> Result<()> {
    relative_path(&finding.path)?;
    ensure!(
        files.contains_key(&finding.path),
        "{} has no covered source",
        finding.id
    );
    ensure!(finding.line > 0, "{} has no source line", finding.id);
    ensure!(
        !finding.anchor.trim().is_empty()
            && !finding.decision.trim().is_empty()
            && !finding.owner.trim().is_empty(),
        "{} loses its source anchor, decision or canonical owner",
        finding.id
    );
    ensure!(
        matches!(
            finding.kind.as_str(),
            "MOVE" | "RUNTIME-PLAN" | "SOURCE-VIEW" | "ABI-CHECK"
        ),
        "{} has an unknown classification",
        finding.id
    );
    Ok(())
}

fn changed_sources(
    expected: &BTreeMap<String, String>,
    actual: &BTreeMap<String, String>,
) -> Vec<String> {
    let paths: BTreeSet<_> = expected.keys().chain(actual.keys()).collect();
    paths
        .into_iter()
        .filter(|path| expected.get(*path) != actual.get(*path))
        .map(|path| {
            let kind = match (expected.contains_key(path), actual.contains_key(path)) {
                (false, true) => "new",
                (true, false) => "removed",
                _ => "changed",
            };
            format!("{kind}: {path}")
        })
        .collect()
}

fn check(root: &Path, reference: &str) -> Result<usize> {
    let roots = BTreeSet::from([BACKEND.into(), LEDGER_PATH.into()]);
    let capture = crate::print_boundary_check::source_closure::capture(root, &roots)?;
    captured::check_with_reference(&capture, reference)
}

fn check_inventory(ledger: &Ledger, actual: &BTreeMap<String, String>) -> Result<usize> {
    let changes = changed_sources(&ledger.files, actual);
    if !changes.is_empty() {
        bail!(
            "legacy backend freeze violated ({} paths):\n{}\nRetirement needs the real source-bound plan/API/conformance gate; counts and approved flags cannot authorize a change.",
            changes.len(),
            changes.join("\n")
        );
    }
    Ok(ledger.findings.len())
}

/// Enforce the frozen legacy boundary. This deliberately cannot retire a row by
/// editing data; verified retirement must be integrated with the shared gates.
pub fn run(root: &Path) -> Result<()> {
    ensure!(
        digest(REFERENCE.as_bytes()) == REFERENCE_SHA256,
        "immutable audit baseline changed"
    );
    let unresolved = check(root, REFERENCE)?;
    println!(
        "Backend migration freeze PASS: {unresolved} unresolved audit groups; \
         legacy backends remain noncompliant. No print-only, M0-complete or WCET claim."
    );
    Ok(())
}
