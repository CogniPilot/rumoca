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
    matches!(
        path.file_name().and_then(OsStr::to_str),
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
        for (line_idx, line) in content.lines().enumerate() {
            let lower = line.to_lowercase();
            if let Some(term) = banned_terms.iter().find(|term| lower.contains(**term)) {
                let rel = normalized_rel_path(path.strip_prefix(&root).expect("relative path"));
                offenders.push(format!("{rel}:{} contains `{term}`", line_idx + 1));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "{}",
        policy_failure_message(&offenders)
    );
}
