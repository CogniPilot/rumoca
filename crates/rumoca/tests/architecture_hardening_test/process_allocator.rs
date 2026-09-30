//! SPEC_0041 §6 process-allocator installation gate.

use super::*;

/// Every executable installs the one process allocator, so its fixed startup
/// configuration cannot be bypassed by a second allocator.
#[test]
fn test_executables_install_the_process_allocator() {
    let root = workspace_root();
    let mut rs_files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut rs_files);

    let attribute = concat!("#[", "global_allocator]");
    let install =
        "static GLOBAL: rumoca_allocator::ProcessAllocator = rumoca_allocator::ProcessAllocator;";
    let mut installers = BTreeSet::new();
    let mut offenders = Vec::new();
    for path in rs_files {
        let rel = path.strip_prefix(&root).unwrap_or(&path);
        if is_test_or_example_path(rel) {
            continue;
        }
        let content = fs::read_to_string(&path).expect("read Rust source");
        let lines: Vec<&str> = content.lines().map(str::trim).collect();
        for (line_idx, line) in lines.iter().enumerate() {
            if *line != attribute {
                continue;
            }
            if lines.get(line_idx + 1) == Some(&install) {
                installers.insert(normalized_rel_path(rel));
            } else {
                offenders.push(format!("{}:{}", rel.display(), line_idx + 1));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "every #[global_allocator] must be rumoca_allocator::ProcessAllocator (SPEC_0041 §6): {offenders:#?}"
    );
    assert!(
        installers.contains("crates/rumoca/src/main.rs")
            && installers.contains("crates/rumoca-worker/src/bin/rumoca-worker.rs"),
        "the rumoca CLI and the model worker must install the process allocator; found {installers:#?}"
    );
}
