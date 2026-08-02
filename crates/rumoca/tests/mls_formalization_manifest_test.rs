//! The formal MLS semantics must account for every contract in SPEC_0022.
//! Absent overrides resolve to the manifest's honest `unformalized` default;
//! this keeps gaps visible while the formal definition is built incrementally.

use serde_json::Value;
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

const MANIFEST_REL: &str = "verification/mls-formalization-coverage.json";
const ASSOCIATION_GAPS_REL: &str = "verification/modelica-association-gaps.json";
const ALLOWED_STATUSES: &[&str] = &["unformalized", "formalized_unproved", "machine_proved"];

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("workspace root")
        .to_path_buf()
}

fn looks_like_contract_id(value: &str) -> bool {
    let Some((prefix, ordinal)) = value.rsplit_once('-') else {
        return false;
    };
    !prefix.is_empty()
        && prefix
            .bytes()
            .all(|byte| byte.is_ascii_uppercase() || byte.is_ascii_digit() || byte == b'_')
        && ordinal.len() == 3
        && ordinal.bytes().all(|byte| byte.is_ascii_digit())
}

fn catalog_contract_ids(catalog: &str) -> BTreeSet<String> {
    let mut ids = BTreeSet::new();
    for line in catalog.lines() {
        let Some(first_cell) = line
            .strip_prefix("| ")
            .and_then(|line| line.split('|').next())
        else {
            continue;
        };
        let id = first_cell.trim();
        if looks_like_contract_id(id) {
            assert!(ids.insert(id.to_string()), "duplicate MLS contract `{id}`");
        }
    }
    ids
}

#[test]
fn mls_formalization_manifest_tracks_every_catalog_contract() {
    let root = workspace_root();
    let manifest_text =
        fs::read_to_string(root.join(MANIFEST_REL)).expect("read MLS formalization manifest");
    let manifest: Value = serde_json::from_str(&manifest_text).expect("parse manifest JSON");
    assert_eq!(manifest["schema_version"], 1);
    assert_eq!(manifest["mls_version"], "3.7");
    assert_eq!(
        manifest["ordinary_compilation_policy"], "informational_only",
        "proof coverage must not reject otherwise accepted Modelica"
    );

    let catalog_rel = manifest["contract_catalog"]
        .as_str()
        .expect("contract_catalog string");
    let catalog = fs::read_to_string(root.join(catalog_rel)).expect("read contract catalog");
    let ids = catalog_contract_ids(&catalog);
    assert_eq!(
        manifest["catalog_contract_count"].as_u64(),
        Some(ids.len() as u64),
        "catalog changes must update the reviewed manifest count"
    );

    let default_status = manifest["default_status"]
        .as_str()
        .expect("default_status string");
    assert!(ALLOWED_STATUSES.contains(&default_status));
    let entries = manifest["contracts"].as_object().expect("contracts object");
    for (id, entry) in entries {
        assert!(ids.contains(id), "manifest names unknown contract `{id}`");
        let status = entry["status"].as_str().expect("contract status string");
        assert!(
            ALLOWED_STATUSES.contains(&status),
            "contract `{id}` has unknown status `{status}`"
        );
        if status != "unformalized" {
            assert!(
                entry["evidence"]
                    .as_array()
                    .is_some_and(|items| !items.is_empty()),
                "contract `{id}` needs machine-readable evidence"
            );
        }
    }

    let complete = ids.iter().all(|id| {
        entries
            .get(id)
            .and_then(|entry| entry["status"].as_str())
            .unwrap_or(default_status)
            == "machine_proved"
    });
    assert_eq!(
        manifest["formalization_complete"].as_bool(),
        Some(complete),
        "formalization_complete must be derived from every catalog contract"
    );
}

#[test]
fn modelica_association_gap_records_are_traceable_to_the_catalog() {
    let root = workspace_root();
    let payload: Value = serde_json::from_str(
        &fs::read_to_string(root.join(ASSOCIATION_GAPS_REL))
            .expect("read Modelica Association gap tracker"),
    )
    .expect("parse gap tracker JSON");
    assert_eq!(payload["schema_version"], 1);
    assert_eq!(payload["mls_version"], "3.7");
    let catalog_rel = payload["contract_catalog"]
        .as_str()
        .expect("contract_catalog string");
    let ids = catalog_contract_ids(
        &fs::read_to_string(root.join(catalog_rel)).expect("read contract catalog"),
    );
    let allowed_states = ["discovered", "proposal_drafted", "submitted", "resolved"];
    let mut issue_ids = BTreeSet::new();
    for issue in payload["issues"].as_array().expect("issues array") {
        let issue_id = issue["id"].as_str().expect("gap id");
        assert!(issue_ids.insert(issue_id), "duplicate gap `{issue_id}`");
        assert!(
            allowed_states.contains(&issue["status"].as_str().expect("gap status")),
            "gap `{issue_id}` has unknown submission status"
        );
        assert!(
            issue["contract_ids"]
                .as_array()
                .is_some_and(|items| !items.is_empty()),
            "gap `{issue_id}` must name affected contracts"
        );
        for contract in issue["contract_ids"].as_array().expect("contract IDs") {
            let contract = contract.as_str().expect("contract ID string");
            assert!(
                ids.contains(contract),
                "gap `{issue_id}` names `{contract}`"
            );
        }
        for field in ["mls_clauses", "counterexample", "proposed_clarification"] {
            assert!(
                issue[field]
                    .as_str()
                    .is_some_and(|text| !text.trim().is_empty()),
                "gap `{issue_id}` must record `{field}`"
            );
        }
    }
}
