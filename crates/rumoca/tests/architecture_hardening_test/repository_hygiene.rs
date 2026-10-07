//! Repository-wide source hygiene: no `*_tail*.rs` files and no ignored tests.

use crate::architecture_hardening_support::{collect_rs_files, workspace_root};
use std::fs;

#[test]
fn test_no_tail_rs_files_in_crates() {
    let root = workspace_root().join("crates");
    let mut rs_files = Vec::new();
    collect_rs_files(&root, &mut rs_files);

    let offenders: Vec<String> = rs_files
        .iter()
        .filter_map(|path| {
            let stem = path.file_stem()?.to_string_lossy();
            (stem.contains("_tail")).then(|| path.display().to_string())
        })
        .collect();

    assert!(
        offenders.is_empty(),
        "found banned *_tail*.rs files: {offenders:?}"
    );
}

#[test]
fn test_no_ignored_tests() {
    let root = workspace_root().join("crates");
    let mut rs_files = Vec::new();
    collect_rs_files(&root, &mut rs_files);

    let mut offenders = Vec::new();
    let ignore_attr = concat!("#[", "ignore");
    for path in rs_files {
        if path.ends_with("crates/rumoca/tests/architecture_hardening_test/repository_hygiene.rs") {
            continue;
        }
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        for (line_idx, line) in content.lines().enumerate() {
            if line.contains(ignore_attr) {
                offenders.push(format!("{}:{}", path.display(), line_idx + 1));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "found ignored tests; use Cargo features/filters for heavy suites or remove stale tests: {offenders:?}"
    );
}
