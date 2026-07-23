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

fn collect_missing_spec_recovery_contract(spec_recovery: &str, missing: &mut Vec<String>) {
    let required_spec_contract = [
        "Explicitly authorized ClimaMind Rumoca broken-main recovery batch",
        "normal reviewer gate remains unchanged",
        "`authorization_ref` MUST identify a durable record in the validation integration PR body or a maintainer-controlled GitHub artifact",
        "`authorized_by` MUST identify a ClimaMind Rumoca repository maintainer",
        "this task's explicit maintainer authorization is sufficient; no additional maintainer or approval is required",
        "required fields: `authorization_ref`, `authorized_by`, `batch_id`, authorized ordered `owner_prs`, `target_branch`, and RFC 3339 UTC `expires_at`",
        "automatically becomes inactive and MUST fail closed as soon as any one of these conditions is true",
        "RFC 3339 `expires_at` has passed",
        "every authorized owner PR has landed",
        "all required CI checks on the target `main` are green",
        "Before each owner PR merge, the authoritative record MUST exist, match the recorded batch, PR, head, and target values, and remain unexpired",
        "independent technical review",
        "owner mechanism test",
        "evidence to that owner PR's final `head_sha`",
        "exact-head integration hosted CI is green",
        "all required hosted CI checks are green",
        "then merge in sequence",
        "owner PR `head_sha` values",
        "Every listed final owner `head_sha`, including the recovery-rule PR `head_sha`, MUST be a Git ancestor of the integration `head_sha`",
        "recorded target baseline `head_sha` MUST be a Git ancestor of the integration `head_sha`",
        "Cherry-pick, patch-id, squash, or content equivalence is not exact provenance",
        "target baseline, the listed exact owner histories, and signed merge commits only",
        "Every such merge commit MUST carry exactly one `Signed-off-by` trailer and no `Co-Authored-By` trailer",
        "MUST NOT contain any integration-only production, test, spec, workflow, baseline, validator, tolerance, fixture, or content commit",
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
    for required in required_spec_contract {
        if !spec_recovery.contains(required) {
            missing.push(format!("SPEC_0025 missing recovery contract: `{required}`"));
        }
    }
}

fn collect_missing_template_recovery_contract(template_recovery: &str, missing: &mut Vec<String>) {
    let required_template_contract = [
        "## Authorized Broken-Main Recovery (optional)",
        "Leave blank for normal PRs",
        "Explicitly authorized ClimaMind Rumoca broken-main recovery batch",
        "`authorization_ref` (durable validation integration PR body or maintainer-controlled GitHub artifact):",
        "`authorized_by` (ClimaMind Rumoca repository maintainer):",
        "`batch_id`:",
        "Authorized ordered `owner_prs`:",
        "`target_branch` / baseline `head_sha`:",
        "RFC 3339 UTC `expires_at`:",
        "Owner PR / final `head_sha`:",
        "Independent technical review / reviewed `head_sha`:",
        "Owner mechanism test / tested `head_sha`:",
        "Recovery-rule PR / final `head_sha`:",
        "Integration PR / `head_sha`:",
        "Hosted CI workflow / `head_sha`:",
        "Authorization exists, matches this merge, and is unexpired.",
        "`authorized_by` is a ClimaMind Rumoca maintainer; this task's explicit authorization is sufficient, with no additional maintainer or approval.",
        "Recovery is inactive and fails closed if `expires_at` passed, every authorized owner PR landed, or target `main` has all required CI green.",
        "Evidence is bound to the owner final head and recorded in order; merge only after all required hosted CI is green on the integration head.",
        "Every listed final owner head, including the recovery-rule PR head, is a Git ancestor of the integration head; no cherry-pick, patch-id, squash, or content-equivalent substitute.",
        "Recorded target baseline `head_sha` is a Git ancestor of the integration head.",
        "Integration history = target baseline + listed exact owner histories + signed merge commits only; no integration-only production, test, spec, workflow, baseline, validator, tolerance, fixture, or content commit.",
        "Every integration merge commit has exactly one `Signed-off-by` trailer and no `Co-Authored-By` trailer.",
        "CI workflow head = integration head.",
        "Any owner, baseline, or integration head change fails closed; rebuild and rerun affected evidence and CI.",
        "Draft, validation-only, never merge",
    ];
    for required in required_template_contract {
        if !template_recovery.contains(required) {
            missing.push(format!(
                "PR template missing recovery linkage: `{required}`"
            ));
        }
    }
}

fn collect_forbidden_recovery_contract(
    spec_recovery: &str,
    template_recovery: &str,
    missing: &mut Vec<String>,
) {
    let forbidden_contract = [
        "`authorization_url`",
        "independent maintainer",
        "another maintainer",
        "not the author of an owner PR",
        "not self-attested by an owner-PR author",
    ];
    for forbidden in forbidden_contract {
        if spec_recovery.contains(forbidden) {
            missing.push(format!(
                "SPEC_0025 recovery contract retains forbidden requirement: `{forbidden}`"
            ));
        }
        if template_recovery.contains(forbidden) {
            missing.push(format!(
                "PR template recovery linkage retains forbidden requirement: `{forbidden}`"
            ));
        }
    }
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

    let mut missing = Vec::new();
    collect_missing_spec_recovery_contract(spec_recovery, &mut missing);
    collect_missing_template_recovery_contract(template_recovery, &mut missing);
    collect_forbidden_recovery_contract(spec_recovery, template_recovery, &mut missing);

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
