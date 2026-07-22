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
fn test_spec_0025_preserves_authorized_broken_main_recovery_contract() {
    // The narrow recovery path is documentation-enforced policy. Keep its
    // activation boundary, ordered evidence, expiry, and integration-only
    // restrictions mechanically visible so a later edit cannot broaden it.
    let root = workspace_root();
    let spec = fs::read_to_string(root.join("spec/SPEC_0025_PR_REVIEW_PROCESS.md"))
        .expect("read SPEC_0025");
    let template = fs::read_to_string(root.join(".github/pull_request_template.md"))
        .expect("read PR template");

    let spec_heading = "### 6a. Authorized Broken-Main Recovery (optional)";
    let spec_tail = &spec[spec.find(spec_heading).expect("SPEC_0025 recovery section")..];
    let spec_recovery = &spec_tail[..spec_tail
        .find("\n### 7.")
        .expect("SPEC_0025 recovery section end")];
    let template_heading = "## Authorized Broken-Main Recovery (optional)";
    let template_tail = &template[template
        .find(template_heading)
        .expect("PR template recovery section")..];
    let template_after_heading = &template_tail[template_heading.len()..];
    let template_end = template_after_heading
        .find("\n## ")
        .map_or(template_tail.len(), |offset| {
            template_heading.len() + offset
        });
    let template_recovery = &template_tail[..template_end];

    let required_spec_contract = [
        "Explicitly authorized ClimaMind Rumoca broken-main recovery batch",
        "normal reviewer gate remains unchanged",
        "authoritative record MUST live outside every owner PR branch",
        "published by a ClimaMind Rumoca repository maintainer",
        "required fields: `authorization_ref`, `authorized_by`, `batch_id`, authorized ordered `owner_prs`, `target_branch`, and RFC 3339 UTC `expires_at`",
        "ends automatically at `expires_at`",
        "Before each owner PR merge, the authoritative record MUST exist, match the recorded batch, PR, head, and target values, and remain unexpired",
        "independent technical review",
        "owner mechanism test",
        "evidence to that owner PR's final `head_sha`",
        "exact-head integration hosted CI is green",
        "all required hosted CI checks are green",
        "then merge in sequence",
        "owner PR `head_sha` values",
        "recorded integration PR `head_sha` MUST be constructed solely from the recorded `target_branch` baseline `head_sha` and the explicitly listed pending owner PR `head_sha` values",
        "hosted CI workflow `head_sha` MUST equal the recorded integration PR `head_sha`",
        "Any owner PR `head_sha`, target baseline `head_sha`, or integration PR `head_sha` change MUST invalidate affected evidence and fail closed",
        "reconstruct the integration PR, refresh affected review or mechanism-test evidence, and rerun all required hosted CI",
        "No GitHub approving review is required only for owner PRs in that active batch",
        "Draft",
        "validation-only",
        "MUST NEVER merge",
        "MUST NOT contain unique fixes",
        "MUST NOT weaken or bypass any existing gate",
        "MUST NOT apply to third-party contributors or an unauthorized batch",
    ];
    let required_template_contract = [
        "## Authorized Broken-Main Recovery (optional)",
        "Leave blank for normal PRs",
        "Explicitly authorized ClimaMind Rumoca broken-main recovery batch",
        "`authorization_ref` (authoritative record outside owner PR branches):",
        "`authorized_by` (ClimaMind Rumoca repository maintainer):",
        "`batch_id`:",
        "Authorized ordered `owner_prs`:",
        "`target_branch` / baseline `head_sha`:",
        "RFC 3339 UTC `expires_at`:",
        "Owner PR / final `head_sha`:",
        "Independent technical review / reviewed `head_sha`:",
        "Owner mechanism test / tested `head_sha`:",
        "Integration PR / `head_sha`:",
        "Hosted CI workflow / `head_sha`:",
        "Authorization exists, matches this merge, and is unexpired.",
        "Evidence is bound to the owner final head and recorded in order; merge only after all required hosted CI is green on the integration head.",
        "Integration input = target baseline + listed pending owner heads only; CI workflow head = integration head.",
        "Any owner, baseline, or integration head change fails closed; rebuild and rerun affected evidence and CI.",
        "Draft, validation-only, never merge; no unique fixes",
    ];

    let mut missing = Vec::new();
    for required in required_spec_contract {
        if !spec_recovery.contains(required) {
            missing.push(format!("SPEC_0025 missing recovery contract: `{required}`"));
        }
    }
    for required in required_template_contract {
        if !template_recovery.contains(required) {
            missing.push(format!(
                "PR template missing recovery linkage: `{required}`"
            ));
        }
    }

    assert!(
        missing.is_empty(),
        "authorized broken-main recovery contract is incomplete:\n  {}",
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
