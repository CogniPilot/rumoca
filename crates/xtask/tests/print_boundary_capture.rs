//! Exercise the ordinary library boundary, not its cfg(test) internals.
use std::collections::BTreeSet;
use std::error::Error;
use std::path::{Path, PathBuf};
use xtask::print_boundary_check::source_closure;

#[test]
fn ordinary_library_capture_retains_source_without_granting_late_reads() {
    let tree = tempfile::tempdir().unwrap();
    std::fs::create_dir(tree.path().join("sources")).unwrap();
    let live = tree.path().join("sources/printer.rs");
    std::fs::write(&live, "captured source").unwrap();
    let captured =
        source_closure::capture(tree.path(), &BTreeSet::from([PathBuf::from("sources")])).unwrap();
    let frozen = captured.file(Path::new("sources/printer.rs")).unwrap();
    std::fs::remove_file(live).unwrap();
    assert_eq!(frozen.text().unwrap(), "captured source");
    assert_eq!(frozen.bytes(), b"captured source");
    assert_eq!(frozen.sha256().len(), 64);
    assert_eq!(captured.files().count(), 1);
    std::fs::write(tree.path().join("sources/later.rs"), "not captured").unwrap();
    let refusal = captured.file(Path::new("sources/later.rs")).unwrap_err();
    assert_eq!(refusal.rule(), "source-not-captured");
    assert_eq!(refusal.item(), "sources/later.rs");
    assert_eq!(refusal.to_string(), "source-not-captured: sources/later.rs");
    assert!(refusal.source().is_none());
}
