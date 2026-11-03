use super::*;

fn roots() -> BTreeSet<PathBuf> {
    BTreeSet::from([PathBuf::from("renderer")])
}

fn fixture() -> tempfile::TempDir {
    let temp = tempfile::tempdir().unwrap();
    fs::create_dir_all(temp.path().join("renderer/helpers")).unwrap();
    fs::write(temp.path().join("renderer/entry.rs"), "approved transport").unwrap();
    fs::write(
        temp.path().join("renderer/helpers/format.rs"),
        "approved encoding",
    )
    .unwrap();
    temp
}

#[test]
fn a_capture_binds_the_entire_tree_including_unlisted_helpers_and_empty_directories() {
    let temp = fixture();
    let base = capture(temp.path(), &roots()).unwrap();
    assert_eq!(base.entries.len(), 4);
    let path = temp.path().join("renderer/helpers/format.rs");
    fs::write(&path, "changed helper").unwrap();
    assert_ne!(capture(temp.path(), &roots()).unwrap(), base);
    fs::write(path, "approved encoding").unwrap();
    assert_eq!(capture(temp.path(), &roots()).unwrap(), base);
    fs::create_dir(temp.path().join("renderer/new-directory")).unwrap();
    assert_ne!(capture(temp.path(), &roots()).unwrap(), base);
}

#[test]
fn a_new_or_deleted_or_relocated_route_changes_the_capture() {
    let temp = fixture();
    let base = capture(temp.path(), &roots()).unwrap();
    let extra = temp.path().join("renderer/hidden.rs");
    fs::write(&extra, "semantic helper").unwrap();
    assert_ne!(capture(temp.path(), &roots()).unwrap(), base);
    fs::remove_file(extra).unwrap();
    assert_eq!(capture(temp.path(), &roots()).unwrap(), base);
    fs::rename(
        temp.path().join("renderer/entry.rs"),
        temp.path().join("renderer/other.rs"),
    )
    .unwrap();
    assert_ne!(capture(temp.path(), &roots()).unwrap(), base);
}

#[test]
fn roots_cannot_escape_or_disappear_or_be_chosen_as_empty() {
    let temp = fixture();
    for name in ["", ".", "../elsewhere", "/absolute"] {
        assert_eq!(
            capture(temp.path(), &BTreeSet::from([PathBuf::from(name)]))
                .unwrap_err()
                .rule,
            "source-root"
        );
    }
    assert_eq!(
        capture(temp.path(), &BTreeSet::new()).unwrap_err().rule,
        "empty-source-closure"
    );
    assert_eq!(
        capture(temp.path(), &BTreeSet::from([PathBuf::from("gone")]))
            .unwrap_err()
            .rule,
        "source-metadata"
    );
}

#[test]
fn overlapping_roots_have_one_identity_and_are_checkout_portable() {
    let first = fixture();
    let second = fixture();
    let expected = capture(first.path(), &roots()).unwrap();
    let overlapping = BTreeSet::from([
        PathBuf::from("renderer"),
        PathBuf::from("renderer/entry.rs"),
    ]);
    assert_eq!(capture(first.path(), &overlapping).unwrap(), expected);
    assert_eq!(capture(second.path(), &roots()).unwrap(), expected);
}

#[cfg(unix)]
#[test]
fn a_symlink_in_the_tree_or_an_input_parent_is_not_a_frozen_source() {
    let temp = fixture();
    std::os::unix::fs::symlink("entry.rs", temp.path().join("renderer/alias.rs")).unwrap();
    assert_eq!(
        capture(temp.path(), &roots()).unwrap_err().rule,
        "source-file-kind"
    );
    std::os::unix::fs::symlink("renderer", temp.path().join("alternate")).unwrap();
    assert_eq!(
        capture(
            temp.path(),
            &BTreeSet::from([PathBuf::from("alternate/entry.rs")])
        )
        .unwrap_err()
        .rule,
        "source-symlink"
    );
}

#[test]
fn an_oversized_source_is_refused_without_reading_an_unbounded_file() {
    let temp = fixture();
    let path = temp.path().join("renderer/large.bin");
    fs::File::create(path)
        .unwrap()
        .set_len(MAX_FILE_BYTES + 1)
        .unwrap();
    assert_eq!(
        capture(temp.path(), &roots()).unwrap_err().rule,
        "source-file-bound"
    );
}

#[test]
fn captured_reads_and_identities_never_reopen_the_live_file() {
    let tree = fixture();
    let frozen = capture(tree.path(), &roots()).unwrap();
    let file = frozen.file(Path::new("renderer/entry.rs")).unwrap();
    assert_eq!(file.text().unwrap(), "approved transport");
    assert_eq!(file.sha256(), format!("{:x}", Sha256::digest(file.bytes())));
    fs::write(
        tree.path().join("renderer/entry.rs"),
        "different live source",
    )
    .unwrap();
    fs::remove_file(tree.path().join("renderer/helpers/format.rs")).unwrap();
    assert_eq!(file.text().unwrap(), "approved transport");
    assert_eq!(
        frozen
            .file(Path::new("renderer/helpers/format.rs"))
            .unwrap()
            .text()
            .unwrap(),
        "approved encoding"
    );
    assert_ne!(capture(tree.path(), &roots()).unwrap(), frozen);
    assert_eq!(frozen.files().count(), 2);
    for (_, item) in frozen.files() {
        assert_eq!(item.sha256(), format!("{:x}", Sha256::digest(item.bytes())));
    }
    let manifest = serde_json::to_string(&frozen).unwrap();
    assert!(!manifest.contains("approved transport"));
    assert!(manifest.contains(file.sha256()));
}

#[test]
fn unowned_directory_and_non_utf8_reads_refuse_instead_of_reading_disk() {
    let tree = fixture();
    fs::write(tree.path().join("renderer/binary"), [0xff, 0xfe]).unwrap();
    let frozen = capture(tree.path(), &roots()).unwrap();
    fs::write(tree.path().join("renderer/late.rs"), "not in capture").unwrap();
    assert_eq!(
        frozen.file(Path::new("renderer/late.rs")).unwrap_err().rule,
        "source-not-captured"
    );
    assert_eq!(
        frozen.file(Path::new("renderer/helpers")).unwrap_err().rule,
        "source-not-a-file"
    );
    assert_eq!(
        frozen
            .file(Path::new("renderer/binary"))
            .unwrap()
            .text()
            .unwrap_err()
            .rule,
        "source-text-encoding"
    );
    assert_eq!(
        frozen.file(Path::new("../late.rs")).unwrap_err().rule,
        "source-root"
    );
}

#[cfg(unix)]
#[test]
fn a_literal_backslash_name_cannot_alias_a_different_captured_path() {
    let tree = fixture();
    fs::write(
        tree.path().join("renderer/helpers\\format.rs"),
        "hidden helper",
    )
    .unwrap();
    assert_eq!(
        capture(tree.path(), &roots()).unwrap_err().rule,
        "source-path-encoding"
    );
}
