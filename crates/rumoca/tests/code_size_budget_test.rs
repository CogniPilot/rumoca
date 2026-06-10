//! SPEC_0021 code-size guard for production Rust sources.

use std::fs;
use std::path::{Path, PathBuf};

const ACTION_REQUIRED_LINES: usize = 2000;

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("workspace root")
        .to_path_buf()
}

fn is_generated(path: &Path) -> bool {
    path.components()
        .any(|component| component.as_os_str() == "generated")
}

fn is_test_source(path: &Path) -> bool {
    let path_text = path.to_string_lossy();
    path_text.contains("/tests/")
        || path.file_name().and_then(|name| name.to_str()) == Some("tests.rs")
        || path_text.ends_with("_tests.rs")
}

fn collect_rust_files(dir: &Path, out: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(dir).expect("read dir") {
        let entry = entry.expect("dir entry");
        let path = entry.path();
        if path.is_dir() {
            collect_rust_files(&path, out);
        } else if path.extension().and_then(|ext| ext.to_str()) == Some("rs") {
            out.push(path);
        }
    }
}

#[test]
fn test_production_rust_files_stay_under_action_required_size() {
    let root = workspace_root();
    let mut files = Vec::new();
    collect_rust_files(&root.join("crates"), &mut files);

    let mut offenders = Vec::new();
    for path in files {
        if is_generated(&path) || is_test_source(&path) {
            continue;
        }
        let content = fs::read_to_string(&path).expect("read rust file");
        let lines = content.lines().count();
        let has_exception = content.contains("SPEC_0021")
            && content.contains("file-size")
            && content.contains("split plan");
        if lines > ACTION_REQUIRED_LINES && !has_exception {
            offenders.push(format!(
                "{}: {lines} lines exceeds SPEC_0021 action threshold of {ACTION_REQUIRED_LINES}",
                path.strip_prefix(&root).unwrap_or(&path).display()
            ));
        }
    }

    assert!(
        offenders.is_empty(),
        "production Rust files exceed SPEC_0021 size budget:\n  {}",
        offenders.join("\n  "),
    );
}
