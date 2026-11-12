use super::super::tests::fixture;
use super::super::*;
use std::fs;

const SOURCE: &str = "pub fn print_issued_name(name: &str) -> &str { name }\n";
const REVISED: &str = "pub fn print_issued_name(name: &str) -> &str { name.trim() }\n";

fn render_path() -> String {
    format!("{BACKEND}/src/render.rs")
}

/// One synthetic revision of the fixture's only covered file, written where
/// the gate captures it. Every fixture finding names that file.
fn revised_fixture(revision: serde_json::Value) -> (tempfile::TempDir, String, String) {
    let (root, reference) = fixture();
    let record = serde_json::json!({"schema": 1, "revisions": [revision]}).to_string();
    fs::write(root.path().join(REVISIONS_PATH), &record).unwrap();
    fs::write(root.path().join(render_path()), REVISED).unwrap();
    (root, reference, record)
}

fn revision() -> serde_json::Value {
    serde_json::json!({
        "id": "R001",
        "decision": "d-test",
        "change": "trim issued names",
        "resolves": ["D001"],
        "files": [{
            "path": render_path(),
            "before": digest(SOURCE.as_bytes()),
            "after": digest(REVISED.as_bytes()),
        }],
    })
}

fn refusal(revision: serde_json::Value) -> String {
    let (root, reference, record) = revised_fixture(revision);
    check(root.path(), &reference, &record)
        .unwrap_err()
        .to_string()
}

#[test]
fn exact_pinned_transition_admits_only_its_bytes_and_resolves_only_its_row() {
    let (root, reference, record) = revised_fixture(revision());
    assert_eq!(check(root.path(), &reference, &record).unwrap(), 95);
    fs::write(root.path().join(render_path()), SOURCE).unwrap();
    let error = check(root.path(), &reference, &record)
        .unwrap_err()
        .to_string();
    assert!(error.contains("changed:"), "{error}");
}

#[test]
fn wrong_before_or_after_identity_is_refused() {
    let mut wrong = revision();
    wrong["files"][0]["before"] = digest(b"other").into();
    assert!(refusal(wrong).contains("before-identity"));
    let mut wrong = revision();
    wrong["files"][0]["after"] = digest(b"other").into();
    assert!(refusal(wrong).contains("changed:"));
    let mut same = revision();
    same["files"][0]["after"] = digest(SOURCE.as_bytes()).into();
    assert!(refusal(same).contains("records no change"));
}

#[test]
fn altered_revision_bytes_are_refused() {
    let (root, reference, record) = revised_fixture(revision());
    fs::write(root.path().join(REVISIONS_PATH), format!("{record}\n")).unwrap();
    let error = check(root.path(), &reference, &record)
        .unwrap_err()
        .to_string();
    assert!(error.contains("revision record changed"), "{error}");
}

#[test]
fn unrelated_covered_drift_and_unlisted_new_files_stay_frozen() {
    let (root, reference, record) = revised_fixture(revision());
    let extra = root.path().join(BACKEND).join("src/helper.rs");
    fs::write(&extra, "fn helper() {}\n").unwrap();
    let error = check(root.path(), &reference, &record)
        .unwrap_err()
        .to_string();
    assert!(
        error.contains("new:") && error.contains("helper.rs"),
        "{error}"
    );
    let mut new_file = revision();
    new_file["files"]
        .as_array_mut()
        .unwrap()
        .push(serde_json::json!({
            "path": format!("{BACKEND}/src/helper.rs"),
            "before": null,
            "after": digest(b"fn helper() {}\n"),
        }));
    let (root, reference, record) = revised_fixture(new_file);
    fs::write(
        root.path().join(BACKEND).join("src/helper.rs"),
        "fn helper() {}\n",
    )
    .unwrap();
    assert_eq!(check(root.path(), &reference, &record).unwrap(), 95);
    fs::write(
        root.path().join(BACKEND).join("src/helper.rs"),
        "fn helper() { drift(); }\n",
    )
    .unwrap();
    assert!(check(root.path(), &reference, &record).is_err());
}

#[test]
fn ledger_mismatch_is_refused_even_with_a_valid_revision() {
    let (root, reference, record) = revised_fixture(revision());
    fs::write(root.path().join(LEDGER_PATH), "{}").unwrap();
    let error = check(root.path(), &reference, &record)
        .unwrap_err()
        .to_string();
    assert!(error.contains("rebaselining"), "{error}");
}

#[test]
fn a_revision_cannot_resolve_rows_outside_its_paths_or_carry_a_flag() {
    let (_, reference) = fixture();
    let mut ledger: serde_json::Value = serde_json::from_str(&reference).unwrap();
    ledger["findings"][1]["path"] = format!("{BACKEND}/src/other.rs").into();
    ledger["files"][format!("{BACKEND}/src/other.rs")] = digest(b"other").into();
    let ledger = parse_ledger(&ledger.to_string()).unwrap();
    let mut elsewhere = revision();
    elsewhere["resolves"] = serde_json::json!(["D002"]);
    let record = serde_json::json!({"schema": 1, "revisions": [elsewhere]}).to_string();
    let error = revisions::expected_inventory(&ledger, &record)
        .unwrap_err()
        .to_string();
    assert!(error.contains("without revising"), "{error}");
    for (field, value) in [
        ("resolves", serde_json::json!(["D097"])),
        ("id", serde_json::json!("R002")),
        ("decision", serde_json::json!(" ")),
        ("approved", serde_json::json!(true)),
    ] {
        let mut bad = revision();
        bad[field] = value;
        let record = serde_json::json!({"schema": 1, "revisions": [bad]}).to_string();
        assert!(
            revisions::expected_inventory(&ledger, &record).is_err(),
            "{field}"
        );
    }
    let mut outside = revision();
    outside["files"][0]["path"] = "crates/xtask/src/lib.rs".into();
    let record = serde_json::json!({"schema": 1, "revisions": [outside]}).to_string();
    assert!(revisions::expected_inventory(&ledger, &record).is_err());
}

#[test]
fn shipped_record_is_pinned_and_resolves_only_d007() {
    assert_eq!(digest(REVISIONS.as_bytes()), REVISIONS_SHA256);
    let ledger = parse_ledger(REFERENCE).unwrap();
    let (expected, resolved) = revisions::expected_inventory(&ledger, REVISIONS).unwrap();
    assert_eq!(resolved, BTreeSet::from(["D007".to_owned()]));
    let moved: Vec<_> = expected
        .iter()
        .filter(|(path, hash)| ledger.files.get(*path) != Some(*hash))
        .map(|(path, _)| path.as_str())
        .collect();
    assert_eq!(moved.len(), 5, "{moved:?}");
    assert_eq!(expected.len(), ledger.files.len() + 1);
}
