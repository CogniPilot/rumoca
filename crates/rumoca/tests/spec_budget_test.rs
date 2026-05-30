//! SPEC_0000 §3 / §3a enforcement: spec set size and per-spec word/line
//! budgets. Runs on every CI build so spec sprawl and spec bloat cannot
//! regress without an explicit status change.
//!
//! Caps (SPEC_0000 §3):
//!   - active spec count (ACCEPTED + DRAFT): <= 15
//!   - REFERENCE specs (lookup catalogs like SPEC_0022): uncapped
//!
//! Per-spec budgets (SPEC_0000 §3a):
//!   - ideal: < 1800 words, < 250 lines
//!   - hard cap: <= 2500 words, <= 350 lines

use std::fs;
use std::path::{Path, PathBuf};

const HARD_WORDS: usize = 2500;
const HARD_LINES: usize = 350;
const ACTIVE_SPEC_CAP: usize = 15;

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("workspace root")
        .to_path_buf()
}

fn spec_status(content: &str) -> Option<&str> {
    // Accept both `## Status\nVALUE` and inline `**Status:** VALUE` forms.
    let mut lines = content.lines();
    while let Some(line) = lines.next() {
        let trimmed = line.trim();
        if trimmed.eq_ignore_ascii_case("## Status") {
            return lines.by_ref().map(str::trim).find(|n| !n.is_empty());
        }
        if let Some(rest) = trimmed.strip_prefix("**Status:**") {
            return Some(rest.trim());
        }
    }
    None
}

fn word_count(text: &str) -> usize {
    text.split_whitespace().count()
}

fn line_count(text: &str) -> usize {
    text.lines().count()
}

#[test]
fn test_specs_respect_size_budgets() {
    let spec_dir = workspace_root().join("spec");
    let mut offenders = Vec::new();

    for entry in fs::read_dir(&spec_dir).expect("read spec dir") {
        let entry = entry.expect("spec entry");
        let path = entry.path();
        if !path.is_file() {
            continue;
        }
        let Some(name) = path.file_name().and_then(|n| n.to_str()) else {
            continue;
        };
        if !name.starts_with("SPEC_") || !name.ends_with(".md") {
            continue;
        }

        let content = fs::read_to_string(&path).expect("read spec");
        let status = spec_status(&content).unwrap_or("UNKNOWN");
        if status.eq_ignore_ascii_case("REFERENCE") {
            // SPEC_0022-style catalogs are exempt per SPEC_0000 §3.
            continue;
        }

        let words = word_count(&content);
        let lines = line_count(&content);

        if words > HARD_WORDS {
            offenders.push(format!(
                "{name}: {words} words exceeds hard cap of {HARD_WORDS} (status={status}). \
SPEC_0000 §3: split, trim, or mark as REFERENCE."
            ));
        }
        if lines > HARD_LINES {
            offenders.push(format!(
                "{name}: {lines} lines exceeds hard cap of {HARD_LINES} (status={status}). \
SPEC_0000 §3: split, trim, or mark as REFERENCE."
            ));
        }
    }

    assert!(
        offenders.is_empty(),
        "specs violate SPEC_0000 §3 size budget:\n  {}",
        offenders.join("\n  "),
    );
}

#[test]
fn test_active_spec_count_under_cap() {
    let spec_dir = workspace_root().join("spec");
    let mut active = Vec::new();

    for entry in fs::read_dir(&spec_dir).expect("read spec dir") {
        let entry = entry.expect("spec entry");
        let path = entry.path();
        if !path.is_file() {
            continue;
        }
        let Some(name) = path.file_name().and_then(|n| n.to_str()) else {
            continue;
        };
        if !name.starts_with("SPEC_") || !name.ends_with(".md") {
            continue;
        }
        let content = fs::read_to_string(&path).expect("read spec");
        let status = spec_status(&content).unwrap_or("").to_ascii_uppercase();
        if status == "ACCEPTED" || status == "DRAFT" {
            active.push(name.to_string());
        }
    }

    assert!(
        active.len() <= ACTIVE_SPEC_CAP,
        "{} active (ACCEPTED+DRAFT) specs exceeds cap of {ACTIVE_SPEC_CAP} (SPEC_0000 §3). \
Either merge specs, move future work to spec/archive/deferred, delete an inactive proposal, or mark one as REFERENCE if it's actually a lookup catalog.\n  Active: {:#?}",
        active.len(),
        active,
    );
}

#[test]
fn test_spec_0025_aligns_with_pr_template() {
    // SPEC_0025 mandates the PR template at .github/pull_request_template.md
    // contains a section for every mandatory rule. Mechanical check: every
    // section header named in SPEC_0025's alignment table appears as a header
    // in the PR template, and the size-budget fields match.
    let root = workspace_root();
    let spec = fs::read_to_string(root.join("spec/SPEC_0025_PR_REVIEW_PROCESS.md"))
        .expect("read SPEC_0025");
    let template = fs::read_to_string(root.join(".github/pull_request_template.md"))
        .expect("read PR template");

    // Sections required in the PR template per SPEC_0025 §"PR Template Alignment".
    let required_sections = [
        "## Summary",
        "## Spec / MLS Alignment",
        "## Risk and Design Notes",
        "## Testing",
        "## Code Size Budget",
        "## Reviewer Checklist",
    ];
    let mut missing = Vec::new();
    for section in required_sections {
        if !template.contains(section) {
            missing.push(format!("PR template missing section header `{section}`"));
        }
    }

    // Size-budget fields must appear in both. SPEC_0025 §5 fenced block holds
    // the canonical list.
    let size_fields = [
        "production_lines_added",
        "production_lines_deleted",
        "test_lines_added",
        "test_lines_deleted",
        "public_items_added",
        "public_items_removed",
        "files_touched",
        "net_added_lines",
    ];
    for field in size_fields {
        if !spec.contains(field) {
            missing.push(format!("SPEC_0025 missing size-budget field `{field}`"));
        }
        if !template.contains(field) {
            missing.push(format!("PR template missing size-budget field `{field}`"));
        }
    }

    // PR template MUST cite SPEC_0025 as its rule source.
    if !template.contains("SPEC_0025") {
        missing.push("PR template missing reference to SPEC_0025".to_string());
    }
    // SPEC_0025 MUST cite the PR template as the canonical artifact.
    if !spec.contains(".github/pull_request_template.md") {
        missing.push("SPEC_0025 missing reference to .github/pull_request_template.md".to_string());
    }

    assert!(
        missing.is_empty(),
        "SPEC_0025 ↔ PR template are out of sync:\n  {}",
        missing.join("\n  "),
    );
}

#[test]
fn test_specs_have_required_status_marker() {
    // SPEC_0000 §"Required Sections": every spec must declare a parseable
    // Status. This catches specs that drop the marker during edits.
    let spec_dir = workspace_root().join("spec");
    let mut missing = Vec::new();

    for entry in fs::read_dir(&spec_dir).expect("read spec dir") {
        let entry = entry.expect("spec entry");
        let path = entry.path();
        if !path.is_file() {
            continue;
        }
        let Some(name) = path.file_name().and_then(|n| n.to_str()) else {
            continue;
        };
        if !name.starts_with("SPEC_") || !name.ends_with(".md") {
            continue;
        }
        let content = fs::read_to_string(&path).expect("read spec");
        if spec_status(&content).is_none() {
            missing.push(name.to_string());
        }
    }

    assert!(
        missing.is_empty(),
        "specs missing a Status marker (## Status + value, or **Status:** value): {missing:?}",
    );
}
