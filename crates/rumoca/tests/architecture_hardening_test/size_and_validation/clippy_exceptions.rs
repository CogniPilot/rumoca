//! Mechanical enforcement for documented SPEC_0021 Clippy exceptions.

use super::super::*;

fn is_generated(path: &Path) -> bool {
    path.components()
        .any(|component| component.as_os_str() == "generated")
}

#[test]
fn clippy_allows_have_direct_spec_0021_exception_comments() {
    let root = workspace_root();
    let mut rs_files = Vec::new();
    for source_root in ["crates", "packages", "infra"] {
        let path = root.join(source_root);
        if path.is_dir() {
            collect_rs_files(&path, &mut rs_files);
        }
    }

    let mut offenders = Vec::new();
    for path in rs_files.into_iter().filter(|path| !is_generated(path)) {
        let content = fs::read_to_string(&path).expect("read Rust source");
        let lines = content.lines().collect::<Vec<_>>();
        for (line_index, line) in lines.iter().enumerate() {
            // SPEC_0021: Exception - the local invariant requires this narrowly scoped lint exception.
            let is_clippy_allow = line.contains("#[allow(clippy::")
                // SPEC_0021: Exception - the local invariant requires this narrowly scoped lint exception.
                || line.contains("#![allow(clippy::");
            if !is_clippy_allow {
                continue;
            }
            let documented = line_index.checked_sub(1).is_some_and(|previous| {
                lines[previous]
                    .trim()
                    .starts_with("// SPEC_0021: Exception - ")
            });
            if !documented {
                offenders.push(format!("{}:{}", path.display(), line_index + 1));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "Clippy allows require a `// SPEC_0021: Exception - <reason>` comment directly above the attribute: {offenders:#?}"
    );
}
