use super::*;
use std::fs;

fn fixture() -> (tempfile::TempDir, String) {
    let root = tempfile::tempdir().expect("test directory");
    let path = format!("{BACKEND}/src/render.rs");
    let source = "pub fn print_issued_name(name: &str) -> &str { name }\n";
    let reference: serde_json::Value = serde_json::from_str(REFERENCE).expect("reference");
    let mut reference = reference;
    for finding in reference["findings"].as_array_mut().expect("findings") {
        finding["path"] = path.clone().into();
    }
    reference["files"] = serde_json::json!({path.clone(): digest(source.as_bytes())});
    reference["source_regions"] = serde_json::json!([]);
    let text = serde_json::to_string(&reference).expect("serialize");
    let file = root.path().join(&path);
    fs::create_dir_all(file.parent().expect("parent")).expect("mkdir");
    fs::write(file, source).expect("write source");
    let ledger = root.path().join(LEDGER_PATH);
    fs::create_dir_all(ledger.parent().expect("parent")).expect("mkdir");
    fs::write(ledger, &text).expect("write ledger");
    (root, text)
}

#[test]
fn unchanged_reference_retains_all_96_unresolved_groups() {
    let (root, reference) = fixture();
    assert_eq!(check(root.path(), &reference).expect("unchanged"), 96);
}

#[test]
fn a_new_helper_is_not_grandfathered_by_the_96_count() {
    let (root, reference) = fixture();
    fs::write(
        root.path().join(BACKEND).join("src/alias_helper.rs"),
        "fn alias() { derive_alias(); }",
    )
    .unwrap();
    let error = check(root.path(), &reference).unwrap_err().to_string();
    assert!(error.contains("new:") && error.contains("alias_helper.rs"));
}

#[test]
fn same_branch_count_with_changed_arithmetic_is_rejected() {
    let (root, reference) = fixture();
    fs::write(
        root.path().join(BACKEND).join("src/render.rs"),
        "pub fn index(i: usize) -> usize { i * 2 }\n",
    )
    .unwrap();
    assert!(
        check(root.path(), &reference)
            .unwrap_err()
            .to_string()
            .contains("changed:")
    );
}

#[test]
fn moving_a_helper_does_not_retire_it() {
    let (root, reference) = fixture();
    fs::rename(
        root.path().join(BACKEND).join("src/render.rs"),
        root.path().join(BACKEND).join("src/new_name.rs"),
    )
    .unwrap();
    let error = check(root.path(), &reference).unwrap_err().to_string();
    assert!(error.contains("removed:") && error.contains("new:"));
}

#[test]
fn a_literal_algorithm_is_a_source_change_even_without_jinja_ast_logic() {
    let (root, reference) = fixture();
    fs::write(
        root.path().join(BACKEND).join("src/gaussian.c.jinja"),
        "for (int i=0; i<n; ++i) A[i] /= pivot;\n",
    )
    .unwrap();
    assert!(check(root.path(), &reference).is_err());
}

#[test]
fn a_row_cannot_be_removed_or_duplicated() {
    let (_, reference) = fixture();
    let mut value: serde_json::Value = serde_json::from_str(&reference).unwrap();
    value["findings"].as_array_mut().unwrap().pop();
    assert!(parse_ledger(&value.to_string()).is_err());
    value["findings"][0]["id"] = "D002".into();
    assert!(
        parse_ledger(&value.to_string())
            .unwrap_err()
            .to_string()
            .contains("duplicate")
    );
}

#[test]
fn new_baseline_hash_or_approved_boolean_cannot_bypass_the_gate() {
    let (root, reference) = fixture();
    let mut value: serde_json::Value = serde_json::from_str(&reference).unwrap();
    value["approved"] = true.into();
    assert!(parse_ledger(&value.to_string()).is_err());
    fs::write(root.path().join(LEDGER_PATH), value.to_string()).unwrap();
    assert!(
        check(root.path(), &reference)
            .unwrap_err()
            .to_string()
            .contains("rebaselining")
    );
}

#[test]
fn source_root_escape_and_unknown_scope_are_rejected() {
    for path in ["../outside", "/absolute", "a/../b", "./a", ""] {
        assert!(relative_path(path).is_err(), "{path}");
    }
    let (_, reference) = fixture();
    let mut value: serde_json::Value = serde_json::from_str(&reference).unwrap();
    value["scope"] = serde_json::json!([]);
    assert!(parse_ledger(&value.to_string()).is_err());
}

#[cfg(unix)]
#[test]
fn symlinks_cannot_hide_or_redirect_covered_source() {
    let (root, reference) = fixture();
    std::os::unix::fs::symlink("render.rs", root.path().join(BACKEND).join("src/alias.rs"))
        .unwrap();
    assert!(
        check(root.path(), &reference)
            .unwrap_err()
            .to_string()
            .contains("symlink")
    );
}

#[test]
fn reference_has_exact_complete_source_bound_inventory() {
    let ledger = parse_ledger(REFERENCE).expect("shipped ledger");
    assert_eq!(ledger.findings.len(), 96);
    assert!(
        ledger
            .files
            .keys()
            .any(|path| path.ends_with("fmi3/typed_functions.jinja"))
    );
    assert!(
        ledger
            .files
            .keys()
            .any(|path| path.ends_with("solve_block_renderer.rs"))
    );
}

#[test]
fn public_gate_checks_the_actual_repository_and_embedded_baseline() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("repository root");
    run(root).expect("actual audited snapshot");
}

#[cfg(unix)]
#[test]
fn symlinked_scope_ancestors_are_rejected() {
    let (root, reference) = fixture();
    let original = root.path().join("crates");
    let moved = root.path().join("moved-crates");
    fs::rename(&original, &moved).unwrap();
    std::os::unix::fs::symlink(&moved, &original).unwrap();
    assert!(
        check(root.path(), &reference)
            .unwrap_err()
            .to_string()
            .contains("symlink")
    );
}

#[test]
fn source_regions_bind_actual_bytes_without_retiring_a_group() {
    let ledger = parse_ledger(REFERENCE).unwrap();
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap();
    assert_eq!(ledger.source_regions.len(), 22);
    for region in &ledger.source_regions {
        let source = fs::read(root.join(&region.path)).unwrap();
        let bytes = source.get(region.start_byte..region.end_byte).unwrap();
        assert_eq!(digest(bytes), region.sha256);
        let header = format!("macro {}(", region.name);
        assert!(String::from_utf8_lossy(bytes).contains(&header));
    }
    run(root).expect("regions confer no exemption");
}

#[test]
fn invalid_region_extents_and_duplicate_identities_are_rejected() {
    let mut ledger: serde_json::Value = serde_json::from_str(REFERENCE).unwrap();
    let original = ledger.clone();
    ledger["source_regions"][0]["end_byte"] = 0.into();
    assert!(parse_ledger(&ledger.to_string()).is_err());
    let mut ledger = original;
    let first = ledger["source_regions"][0].clone();
    ledger["source_regions"].as_array_mut().unwrap().push(first);
    assert!(parse_ledger(&ledger.to_string()).is_err());
}
