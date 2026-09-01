//! SPEC_0000 §3 / §3a enforcement: spec set size and per-spec word/line
//! budgets. Runs on every CI build so spec sprawl and spec bloat cannot
//! regress without an explicit status change.
//!
//! Caps (SPEC_0000 §3):
//!   - active spec count (ACCEPTED + DRAFT): <= 20
//!   - REFERENCE specs (lookup catalogs like SPEC_0022): uncapped
//!
//! Per-spec budgets (SPEC_0000 §3a):
//!   - ideal: < 1800 words, < 250 lines
//!   - hard cap: <= 2500 words, <= 350 lines

use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

const HARD_WORDS: usize = 2500;
const HARD_LINES: usize = 350;
const ACTIVE_SPEC_CAP: usize = 20;

struct GovernedAnnex {
    parent: &'static str,
    annex: &'static str,
    rule_sections: &'static [(&'static str, usize)],
}

const GOVERNED_ANNEXES: &[GovernedAnnex] = &[
    GovernedAnnex {
        parent: "SPEC_0029_CRATE_BOUNDARIES.md",
        annex: "SPEC_0054_RUNTIME_LAYERING_CATALOG.md",
        rule_sections: &[(
            "### 12. Runtime, Backend, Simulation Session, And Visualization Layering",
            8,
        )],
    },
    GovernedAnnex {
        parent: "SPEC_0034_GALEC_EFMI_EXPORT.md",
        annex: "SPEC_0042_GALEC_LANGUAGE_CATALOG.md",
        rule_sections: &[("### Rules", 6)],
    },
    GovernedAnnex {
        parent: "SPEC_0048_TARGET_REFINEMENT_AND_PREPARED_PRODUCTS.md",
        annex: "SPEC_0055_TARGET_REFINEMENT_CATALOG.md",
        rule_sections: &[
            ("### 2. Prepared Execution Artifacts", 1),
            ("### 3. Target Build Session And Product Plans", 5),
            ("### 4. Final Expansion Boundary And Budgets", 1),
            ("### 5. eFMI Refinement Chain", 1),
        ],
    },
];

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

fn heading_body<'a>(content: &'a str, heading: &str) -> Option<&'a str> {
    let heading_start = content.find(heading)?;
    let body_start = heading_start.checked_add(heading.len())?;
    let remainder = content.get(body_start..)?;
    let Some(body_end) = remainder.find("\n#") else {
        return Some(remainder);
    };
    remainder.get(..body_end)
}

fn rule_table_row_count(section: &str) -> usize {
    section
        .lines()
        .filter(|line| {
            let line = line.trim();
            line.starts_with('|') && !line.starts_with("|---") && !line.starts_with("| Rule |")
        })
        .count()
}

fn rule_home_violations(contract: &GovernedAnnex, parent: &str, annex: &str) -> Vec<String> {
    let mut errors = Vec::new();
    let annex_link = format!("({}", contract.annex);
    let parent_link = format!("({}", contract.parent);
    if !parent.contains(&annex_link) {
        errors.push(format!(
            "{} does not link {}",
            contract.parent, contract.annex
        ));
    }
    if !annex.contains(&parent_link) {
        errors.push(format!(
            "{} does not link {}",
            contract.annex, contract.parent
        ));
    }
    for (heading, minimum_rule_rows) in contract.rule_sections {
        if let Some(section) = heading_body(parent, heading) {
            let normalized = section
                .split_whitespace()
                .collect::<Vec<_>>()
                .join(" ")
                .to_ascii_lowercase();
            let blanket_delegation = ["every rule in", "every row in", "every clause in"]
                .iter()
                .any(|phrase| normalized.contains(phrase));
            if blanket_delegation && normalized.contains("normative by reference") {
                errors.push(format!(
                    "{} delegates `{heading}` through a blanket annex reference",
                    contract.parent
                ));
            }
            let rows = rule_table_row_count(section);
            if rows < *minimum_rule_rows {
                errors.push(format!(
                    "{} has {rows} affirmative rows in `{heading}`, expected at least {minimum_rule_rows}",
                    contract.parent
                ));
            }
        } else {
            errors.push(format!(
                "{} lacks affirmative rule section `{heading}`",
                contract.parent
            ));
        }
    }
    let Some((annex_preamble, _)) = annex.split_once("\n### ") else {
        errors.push(format!(
            "{} lacks a detailed catalog section",
            contract.annex
        ));
        return errors;
    };
    let normalized_annex = annex_preamble
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ");
    let disclaims_independent_rule = normalized_annex
        .contains("introduce no additional requirement")
        || normalized_annex.contains("introduces no additional requirement");
    if !normalized_annex.contains("normative only through that parent link")
        || !disclaims_independent_rule
    {
        errors.push(format!(
            "{} must disclaim independent rule authority",
            contract.annex
        ));
    }
    errors
}

fn collect_source_files(root: &Path, files: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(root).expect("read source directory") {
        let entry = entry.expect("source directory entry");
        let path = entry.path();
        if path.is_dir() {
            collect_source_files(&path, files);
        } else if path
            .extension()
            .and_then(|extension| extension.to_str())
            .is_some_and(|extension| matches!(extension, "rs" | "md"))
        {
            files.push(path);
        }
    }
}

fn cited_spec_ids(content: &str) -> impl Iterator<Item = &str> {
    content.match_indices("SPEC_").filter_map(|(offset, _)| {
        let id = content.get(offset + 5..offset + 9)?;
        id.bytes().all(|byte| byte.is_ascii_digit()).then_some(id)
    })
}

#[test]
fn test_active_spec_ids_are_unique_and_indexed() {
    let root = workspace_root();
    let spec_dir = root.join("spec");
    let index = fs::read_to_string(spec_dir.join("README.md")).expect("read spec index");
    let mut by_id: BTreeMap<String, Vec<String>> = BTreeMap::new();
    let mut errors = Vec::new();

    for entry in fs::read_dir(&spec_dir).expect("read spec dir") {
        let entry = entry.expect("spec entry");
        let path = entry.path();
        let Some(name) = path.file_name().and_then(|name| name.to_str()) else {
            continue;
        };
        if !path.is_file() || !name.starts_with("SPEC_") || !name.ends_with(".md") {
            continue;
        }
        let Some(id) = name.get(5..9) else {
            errors.push(format!("{name}: malformed spec identifier"));
            continue;
        };
        let content = fs::read_to_string(&path).expect("read spec");
        let heading = format!("# SPEC_{id}:");
        if !content
            .lines()
            .next()
            .is_some_and(|line| line.starts_with(&heading))
        {
            errors.push(format!("{name}: first heading must start with `{heading}`"));
        }
        by_id
            .entry(id.to_string())
            .or_default()
            .push(name.to_string());

        let status = spec_status(&content).unwrap_or("").to_ascii_uppercase();
        if matches!(status.as_str(), "ACCEPTED" | "DRAFT" | "REFERENCE")
            && !index.contains(&format!("({name})"))
        {
            errors.push(format!(
                "{name}: active spec is missing from spec/README.md"
            ));
        }
    }

    for (id, names) in by_id {
        if names.len() > 1 {
            errors.push(format!("SPEC_{id} is used by {}", names.join(", ")));
        }
    }
    assert!(
        errors.is_empty(),
        "spec identity/index violations:\n  {}",
        errors.join("\n  "),
    );
}

#[test]
fn test_source_spec_citations_resolve_to_active_specs() {
    let root = workspace_root();
    let spec_dir = root.join("spec");
    let mut active_ids = BTreeSet::new();
    for entry in fs::read_dir(&spec_dir).expect("read spec dir") {
        let entry = entry.expect("spec entry");
        let path = entry.path();
        let Some(name) = path.file_name().and_then(|name| name.to_str()) else {
            continue;
        };
        if !path.is_file() || !name.starts_with("SPEC_") || !name.ends_with(".md") {
            continue;
        }
        let content = fs::read_to_string(&path).expect("read spec");
        let status = spec_status(&content).unwrap_or("").to_ascii_uppercase();
        if matches!(status.as_str(), "ACCEPTED" | "DRAFT" | "REFERENCE") {
            active_ids.insert(name[5..9].to_string());
        }
    }

    let crates_dir = root.join("crates");
    let mut source_files = Vec::new();
    collect_source_files(&crates_dir, &mut source_files);
    let mut stale = Vec::new();
    for path in source_files {
        let content = fs::read_to_string(&path).expect("read source file");
        for id in cited_spec_ids(&content) {
            if !active_ids.contains(id) {
                let relative = path.strip_prefix(&root).unwrap_or(&path);
                stale.push(format!(
                    "{} cites retired or missing SPEC_{id}",
                    relative.display()
                ));
            }
        }
    }
    stale.sort();
    stale.dedup();

    assert!(
        stale.is_empty(),
        "source citations must resolve to an active spec:\n  {}",
        stale.join("\n  "),
    );
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

const PROOF_PACKET_FIELDS: &[&str] = &[
    "spec_mls_anchors",
    "construction_invariant",
    "construction_authority",
    "concrete_reproduction",
    "first_divergence",
    "rejected_hypotheses",
    "producer_artifact_delta",
    "dependency_predecessors",
    "keystone_files_and_types",
    "reservation_window",
    "reservation_release_or_checkpoint",
    "positive_witness",
    "negative_witness",
    "mutation_witness",
    "claim_status",
    "command_results_with_exit_status",
    "review_verdict",
    "reviewed_revision",
    "commands_not_run",
];

fn proof_packet_alignment_violations(spec: &str, template: &str) -> Vec<String> {
    let mut missing = Vec::new();
    let spec_packet = heading_body(spec, "### 3a. Proof Packet").unwrap_or("");
    let template_packet = heading_body(template, "## Proof Packet").unwrap_or("");
    if spec_packet.is_empty() {
        missing.push("SPEC_0025 missing Proof Packet section".to_string());
    }
    if template_packet.is_empty() {
        missing.push("PR template missing Proof Packet section".to_string());
    }
    for field in PROOF_PACKET_FIELDS {
        if !proof_packet_has_field(spec_packet, field) {
            missing.push(format!("SPEC_0025 missing proof-packet field `{field}`"));
        }
        if !proof_packet_has_field(template_packet, field) {
            missing.push(format!("PR template missing proof-packet field `{field}`"));
        }
    }
    missing
}

fn proof_packet_has_field(section: &str, field: &str) -> bool {
    let label = format!("{field}:");
    section.lines().any(|line| {
        let line = line.trim();
        line.strip_prefix("- ").unwrap_or(line).starts_with(&label)
    })
}

#[test]
fn proof_packet_field_matching_rejects_superstrings() {
    assert!(proof_packet_has_field(
        "- construction_invariant: exact owner",
        "construction_invariant"
    ));
    assert!(!proof_packet_has_field(
        "- not_construction_invariant: deceptive superstring",
        "construction_invariant"
    ));
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
        "## Proof Packet",
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
    missing.extend(proof_packet_alignment_violations(&spec, &template));

    assert!(
        missing.is_empty(),
        "SPEC_0025 ↔ PR template are out of sync:\n  {}",
        missing.join("\n  "),
    );
}

#[test]
fn proof_packet_alignment_detects_each_missing_field_in_either_owner() {
    let root = workspace_root();
    let spec = fs::read_to_string(root.join("spec/SPEC_0025_PR_REVIEW_PROCESS.md"))
        .expect("read SPEC_0025");
    let template = fs::read_to_string(root.join(".github/pull_request_template.md"))
        .expect("read PR template");

    for field in PROOF_PACKET_FIELDS {
        let without_spec_field = spec
            .lines()
            .filter(|line| !line.contains(field))
            .collect::<Vec<_>>()
            .join("\n");
        let spec_violations = proof_packet_alignment_violations(&without_spec_field, &template);
        assert!(
            spec_violations
                .iter()
                .any(|violation| violation
                    == &format!("SPEC_0025 missing proof-packet field `{field}`")),
            "removing `{field}` from SPEC_0025 must fail that exact field"
        );

        let without_template_field = template
            .lines()
            .filter(|line| !line.contains(field))
            .collect::<Vec<_>>()
            .join("\n");
        let template_violations = proof_packet_alignment_violations(&spec, &without_template_field);
        assert!(
            template_violations.iter().any(|violation| violation
                == &format!("PR template missing proof-packet field `{field}`")),
            "removing `{field}` from the PR template must fail that exact field"
        );
    }
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

#[test]
fn test_reference_annexes_have_affirmative_parent_rule_homes() {
    let spec_dir = workspace_root().join("spec");
    let mut errors = Vec::new();
    for contract in GOVERNED_ANNEXES {
        let parent = fs::read_to_string(spec_dir.join(contract.parent)).expect("read parent spec");
        let annex = fs::read_to_string(spec_dir.join(contract.annex)).expect("read annex spec");
        errors.extend(rule_home_violations(contract, &parent, &annex));
    }
    assert!(
        errors.is_empty(),
        "REFERENCE annexes must not replace affirmative parent rules:\n  {}",
        errors.join("\n  "),
    );
}

#[test]
fn test_rule_home_gate_rejects_blanket_delegation_mutations() {
    let spec_dir = workspace_root().join("spec");
    for contract in GOVERNED_ANNEXES {
        let annex = fs::read_to_string(spec_dir.join(contract.annex)).expect("read annex spec");
        let first_heading = contract
            .rule_sections
            .first()
            .map(|(heading, _)| *heading)
            .expect("governed annex has a parent rule section");
        let hollow_parent = format!(
            "## Specification\n\n{}\n\nEvery rule in [{}] is REQUIRED and normative by reference.\n\n[annex]({})\n",
            first_heading, contract.annex, contract.annex
        );
        let errors = rule_home_violations(contract, &hollow_parent, &annex);
        assert!(
            errors
                .iter()
                .any(|error| error.contains("blanket annex reference")),
            "{} blanket-delegation mutation escaped: {errors:?}",
            contract.parent,
        );
        assert!(
            errors
                .iter()
                .any(|error| error.contains("affirmative rows")),
            "{} rule-table deletion mutation escaped: {errors:?}",
            contract.parent,
        );
    }
}
