//! Mechanical enforcement for documented SPEC_0021 Clippy exceptions.
//!
//! SPEC_0021 governs exactly three complexity lints, so only those require the
//! exception marker. Other Clippy allows answer to their own lint's rationale
//! and are deliberately out of scope here; demanding a SPEC_0021 citation for
//! them would force authors to cite a spec that does not govern the lint.

use super::super::*;

/// The lints SPEC_0021 "Enforcement" sets to `deny` and whose waivers its
/// "Exceptions" section governs.
const GOVERNED_LINTS: [&str; 3] = ["too_many_lines", "excessive_nesting", "too_many_arguments"];

const MARKER: &str = "// SPEC_0021: Exception - ";

fn is_generated(path: &Path) -> bool {
    path.components()
        .any(|component| component.as_os_str() == "generated")
}

/// Whether `line` is an attribute waiving one of the governed lints.
///
/// The attribute text is assembled at runtime so this file does not contain a
/// literal occurrence of the pattern it scans for, which would otherwise make
/// the gate flag or exempt itself.
fn waives_governed_lint(line: &str) -> bool {
    let attribute = format!("#{}[allow(clippy::", "");
    let inner = format!("#{}![allow(clippy::", "");
    let Some(start) = line.find(&attribute).or_else(|| line.find(&inner)) else {
        return false;
    };
    let tail = &line[start..];
    GOVERNED_LINTS.iter().any(|lint| tail.contains(lint))
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
            if !waives_governed_lint(line) {
                continue;
            }
            let documented = line_index
                .checked_sub(1)
                .is_some_and(|previous| lines[previous].trim().starts_with(MARKER));
            if !documented {
                offenders.push(format!("{}:{}", path.display(), line_index + 1));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "SPEC_0021 complexity-lint allows require a `{MARKER}<reason>` comment directly above the attribute: {offenders:#?}"
    );
}
