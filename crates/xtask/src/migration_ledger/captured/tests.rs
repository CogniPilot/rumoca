use super::*;
use crate::print_boundary_check::source_closure::capture;
use std::collections::BTreeSet;
use std::fs;
use std::path::PathBuf;

fn audited_capture() -> SourceClosure {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap();
    capture(
        root,
        &BTreeSet::from([PathBuf::from(BACKEND), PathBuf::from(LEDGER_PATH)]),
    )
    .unwrap()
}

fn copy_capture(source: &SourceClosure, root: &Path) {
    for (path, file) in source.files() {
        let output = root.join(path);
        fs::create_dir_all(output.parent().unwrap()).unwrap();
        fs::write(output, file.bytes()).unwrap();
    }
}

#[test]
fn full_retained_snapshot_keeps_all_96_groups_unresolved() {
    assert_eq!(check_unchanged_capture(&audited_capture()).unwrap(), 96);
}

#[test]
fn expected_file_subset_cannot_hide_an_unexpected_backend_file() {
    let source = audited_capture();
    let root = tempfile::tempdir().unwrap();
    copy_capture(&source, root.path());
    let unexpected = format!("{BACKEND}/unexpected.rs");
    fs::write(root.path().join(&unexpected), "fn hidden_algorithm() {}\n").unwrap();
    let expected = source
        .files()
        .map(|(path, _)| PathBuf::from(path))
        .collect();
    let subset = capture(root.path(), &expected).unwrap();
    assert!(check_unchanged_capture(&subset).is_err());
    let complete = capture(
        root.path(),
        &BTreeSet::from([BACKEND.into(), LEDGER_PATH.into()]),
    )
    .unwrap();
    let error = check_unchanged_capture(&complete).unwrap_err().to_string();
    assert!(error.contains("new:") && error.contains(&unexpected));
}

#[test]
fn changed_removed_and_tampered_ledger_bytes_refuse_in_the_same_capture() {
    let source = audited_capture();
    let root = tempfile::tempdir().unwrap();
    copy_capture(&source, root.path());
    let path = format!("{BACKEND}/src/templates/fmi-ls-wasm/model.c.jinja");
    let roots = BTreeSet::from([BACKEND.into(), LEDGER_PATH.into()]);
    fs::write(root.path().join(&path), "changed").unwrap();
    let changed = capture(root.path(), &roots).unwrap();
    assert!(
        check_unchanged_capture(&changed)
            .unwrap_err()
            .to_string()
            .contains("changed:")
    );
    fs::remove_file(root.path().join(&path)).unwrap();
    let removed = capture(root.path(), &roots).unwrap();
    assert!(
        check_unchanged_capture(&removed)
            .unwrap_err()
            .to_string()
            .contains("removed:")
    );
    fs::write(root.path().join(LEDGER_PATH), "{}").unwrap();
    let tampered = capture(root.path(), &roots).unwrap();
    assert!(
        check_unchanged_capture(&tampered)
            .unwrap_err()
            .to_string()
            .contains("rebaselining")
    );
    // A retained snapshot remains a fact about those captured bytes only. This
    // PASS proves no live checkout/build correspondence or migration authority.
    assert_eq!(check_unchanged_capture(&source).unwrap(), 96);
}
