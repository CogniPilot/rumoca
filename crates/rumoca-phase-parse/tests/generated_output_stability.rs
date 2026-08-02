#[path = "../build_support.rs"]
mod build_support;

use std::fs;

use build_support::install_generated_if_changed;

#[test]
fn equal_generated_content_preserves_checked_in_mtime() {
    let temp = tempfile::tempdir().expect("temporary directory");
    let staged = temp.path().join("staged.rs");
    let checked_in = temp.path().join("checked_in.rs");
    let content = b"fn generated() {}\n";
    fs::write(&staged, content).expect("seed staged output");
    fs::write(&checked_in, content).expect("seed checked-in output");
    let modified_before = fs::metadata(&checked_in)
        .expect("checked-in metadata")
        .modified()
        .expect("checked-in mtime");

    let changed = install_generated_if_changed(&staged, &checked_in).expect("install output");

    let modified_after = fs::metadata(&checked_in)
        .expect("checked-in metadata")
        .modified()
        .expect("checked-in mtime");
    assert!(!changed);
    assert_eq!(modified_after, modified_before);
    assert_eq!(fs::read(&checked_in).expect("checked-in bytes"), content);
}

#[test]
fn changed_generated_content_updates_checked_in_file() {
    let temp = tempfile::tempdir().expect("temporary directory");
    let staged = temp.path().join("staged.rs");
    let checked_in = temp.path().join("checked_in.rs");
    fs::write(&staged, b"fn generated() { revised(); }\n").expect("seed staged output");
    fs::write(&checked_in, b"fn generated() {}\n").expect("seed checked-in output");

    let changed = install_generated_if_changed(&staged, &checked_in).expect("install output");

    assert!(changed);
    assert_eq!(
        fs::read(&checked_in).expect("checked-in bytes"),
        b"fn generated() { revised(); }\n"
    );
}
