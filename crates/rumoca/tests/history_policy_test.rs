use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("workspace root")
        .to_path_buf()
}

fn collect_policy_text_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let entries = fs::read_dir(dir).unwrap_or_else(|error| {
        panic!("failed to read {}: {error}", dir.display());
    });
    for entry in entries {
        let entry = entry.expect("directory entry");
        let path = entry.path();
        if path.is_dir() {
            if should_skip_policy_dir(&path) {
                continue;
            }
            collect_policy_text_files(&path, out);
            continue;
        }
        if is_policy_scanned_text_file(&path) {
            out.push(path);
        }
    }
}

fn should_skip_policy_dir(path: &Path) -> bool {
    if path.ends_with(Path::new("docs/dev-guide/book")) {
        return true;
    }
    let name = path.file_name().and_then(OsStr::to_str);
    // Python virtualenvs (e.g. `.venv`, `.venv-pyapi`) and Pyodide build trees
    // hold third-party `site-packages` whose docstrings use the banned history
    // terms; the policy scan is for *our* source, not installed dependencies.
    if name.is_some_and(|n| {
        n.starts_with(".venv") || n == "venv" || n == "__pycache__" || n == ".pyodide_build"
    }) {
        return true;
    }
    matches!(
        name,
        Some(".git" | ".vscode-test" | "dev" | "target" | "node_modules" | "pkg" | "vendor")
    )
}

fn is_policy_scanned_text_file(path: &Path) -> bool {
    matches!(
        path.extension().and_then(OsStr::to_str),
        Some(
            "rs" | "toml"
                | "md"
                | "jinja"
                | "js"
                | "ts"
                | "html"
                | "css"
                | "py"
                | "sh"
                | "nix"
                | "yml"
                | "yaml"
                | "json"
                | "fbs"
                | "mo"
        )
    )
}

fn normalized_rel_path(path: &Path) -> String {
    path.components()
        .map(|component| component.as_os_str().to_string_lossy())
        .collect::<Vec<_>>()
        .join("/")
}

fn policy_banned_history_terms() -> [&'static str; 7] {
    [
        concat!("leg", "acy"),
        concat!("backward", " compatibility"),
        concat!("backwards", " compatibility"),
        concat!("backward", "-compatible"),
        concat!("backwards", "-compatible"),
        concat!("backward", " compatability"),
        concat!("backwards", " compatability"),
    ]
}

fn policy_failure_message(offenders: &[String]) -> String {
    format!(
        "{}",
        format_args!(
            "found history-preserving terminology in source/docs: {offenders:?}. \
Pre 1.0, we want no {} {}/ {} code; delete the old path instead.",
            concat!("back", "wards"),
            concat!("compat", "ibility"),
            concat!("leg", "acy")
        )
    )
}

/// The eFMI 1.0.0 standard mandates a `SupportedPlatform` enumeration whose
/// value is a capitalized proper noun (`efmiSupportedPlatforms.xsd`) naming a
/// plain-C, non-AUTOSAR deployment target. Rumoca emits that value verbatim
/// (`SupportedPlatform` enum + `platform="…"` manifest attribute), so the word
/// appears in our source as a schema literal, not as history-preserving code.
///
/// A hit is exempt only when the offending word IS that capitalized identifier:
/// the lowercase adjective (as in "… code" / "… path") still fails, and the
/// exemption is scoped to the single-word platform term so the multi-word
/// compatibility terms are never affected.
fn term_is_efmi_platform_proper_noun(line: &str, banned_term: &str) -> bool {
    if banned_term != concat!("leg", "acy") {
        return false;
    }
    // Every hit on this line must be the exact capitalized standard token.
    !line.contains(banned_term) && line.contains(concat!("Leg", "acy"))
}

/// Append `rel:line contains \`term\`` for every banned term on every line of
/// one file, skipping the eFMI platform proper-noun exemption.
fn collect_line_offenders(
    rel: &str,
    content: &str,
    banned_terms: &[&str],
    offenders: &mut Vec<String>,
) {
    for (line_idx, line) in content.lines().enumerate() {
        let lower = line.to_lowercase();
        let hits = banned_terms
            .iter()
            .filter(|term| lower.contains(**term))
            .filter(|term| !term_is_efmi_platform_proper_noun(line, term));
        for term in hits {
            offenders.push(format!("{rel}:{} contains `{term}`", line_idx + 1));
        }
    }
}

#[test]
fn test_no_history_preserving_paths_before_one_dot_zero() {
    let root = workspace_root();
    let mut offenders = Vec::new();
    let banned_terms = policy_banned_history_terms();
    let mut files = Vec::new();
    collect_policy_text_files(&root, &mut files);

    for path in files {
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        let rel = normalized_rel_path(path.strip_prefix(&root).expect("relative path"));
        collect_line_offenders(&rel, &content, &banned_terms, &mut offenders);
    }

    assert!(
        offenders.is_empty(),
        "{}",
        policy_failure_message(&offenders)
    );
}
