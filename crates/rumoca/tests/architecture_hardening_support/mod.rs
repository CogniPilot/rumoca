use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};
pub(super) fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("workspace root")
        .to_path_buf()
}
pub(super) fn collect_rs_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let entries = fs::read_dir(dir).unwrap_or_else(|error| {
        panic!("failed to read {}: {error}", dir.display());
    });
    for entry in entries {
        let entry = entry.expect("directory entry");
        let path = entry.path();
        if path.is_dir() {
            collect_rs_files(&path, out);
            continue;
        }
        if path.extension().is_some_and(|ext| ext == "rs") {
            out.push(path);
        }
    }
}
pub(super) fn collect_ir_crate_rs_files(root: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    let ir_crate_dirs = fs::read_dir(root.join("crates"))
        .expect("read crates dir")
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| {
            p.is_dir()
                && p.file_name()
                    .is_some_and(|n| n.to_string_lossy().starts_with("rumoca-ir-"))
        });
    for crate_dir in ir_crate_dirs {
        collect_rs_files(&crate_dir.join("src"), &mut files);
    }
    files
}
pub(super) fn public_hash_collection_field_location(
    path: &Path,
    line_idx: usize,
    line: &str,
) -> Option<String> {
    let trimmed = line.trim_start();
    if trimmed.starts_with("//") || trimmed.contains("fn ") || trimmed.contains("->") {
        return None;
    }
    let is_pub_field = trimmed.starts_with("pub ") && trimmed.contains(": ");
    if !is_pub_field {
        return None;
    }
    let type_part = &trimmed[trimmed.find(": ").unwrap() + 2..];
    let uses_std_map = type_part.starts_with("HashMap<")
        || type_part.starts_with("HashSet<")
        || type_part.starts_with("std::collections::HashMap<")
        || type_part.starts_with("std::collections::HashSet<");
    uses_std_map.then(|| format!("{}:{}", path.display(), line_idx + 1))
}
pub(super) fn public_scalarize_function_location(
    path: &Path,
    line_idx: usize,
    line: &str,
) -> Option<String> {
    let trimmed = line.trim_start();
    (!trimmed.starts_with("//")
        && (trimmed.starts_with("pub fn scalarize_")
            || trimmed.starts_with("pub fn to_scalar_")
            || trimmed.starts_with("pub fn scalar_programs_")))
    .then(|| format!("{}:{}", path.display(), line_idx + 1))
}
pub(super) fn has_direct_ir_symbol_import(line: &str) -> bool {
    [
        "use rumoca_ir_ast::{",
        "use rumoca_ir_flat::{",
        "use rumoca_ir_dae::{",
    ]
    .iter()
    .any(|needle| line.contains(needle))
}
pub(super) fn collect_direct_import_offenders(path: &Path) -> Vec<String> {
    let Ok(content) = fs::read_to_string(path) else {
        return Vec::new();
    };
    content
        .lines()
        .enumerate()
        .filter(|(_, line)| has_direct_ir_symbol_import(line))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}
pub(super) fn section_contains_dependency(content: &str, section: &str, dependency: &str) -> bool {
    let header = format!("[{section}]");
    let mut in_section = false;
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') && trimmed.ends_with(']') {
            in_section = trimmed == header;
            continue;
        }
        if !in_section || trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }
        if let Some((name, _)) = trimmed.split_once('=')
            && name.trim() == dependency
        {
            return true;
        }
    }
    false
}
pub(super) fn section_dependency_line<'a>(
    content: &'a str,
    section: &str,
    dependency: &str,
) -> Option<&'a str> {
    let header = format!("[{section}]");
    let mut in_section = false;
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') && trimmed.ends_with(']') {
            in_section = trimmed == header;
            continue;
        }
        if !in_section || trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }
        if let Some((name, _)) = trimmed.split_once('=')
            && name.trim() == dependency
        {
            return Some(trimmed);
        }
    }

    None
}

pub(super) fn section_dependency_names(content: &str, section: &str) -> Vec<String> {
    let header = format!("[{section}]");
    let mut in_section = false;
    let mut names = Vec::new();

    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') && trimmed.ends_with(']') {
            in_section = trimmed == header;
            continue;
        }
        if !in_section || trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }
        if let Some((name, _)) = trimmed.split_once('=') {
            names.push(name.trim().to_string());
        }
    }

    names
}

pub(super) fn workspace_crate_dirs(root: &Path) -> Vec<PathBuf> {
    fs::read_dir(root.join("crates"))
        .expect("read crates dir")
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.path())
        .filter(|path| path.is_dir())
        .collect()
}

pub(super) fn is_cross_crate_pub_type_alias(trimmed: &str) -> bool {
    if !trimmed.starts_with("pub type ") {
        return false;
    }
    let Some((_, rhs)) = trimmed.split_once('=') else {
        return false;
    };
    let rhs = rhs.trim_start();
    rhs.starts_with("rumoca_") || rhs.starts_with("::rumoca_")
}

pub(super) fn cross_crate_alias(line: &str) -> Option<String> {
    let trimmed = line.trim_start();
    if !trimmed.starts_with("use rumoca_") {
        return None;
    }
    let (_, alias) = trimmed.split_once(" as ")?;
    alias
        .trim_end_matches(';')
        .split_whitespace()
        .next()
        .map(str::to_string)
}

pub(super) fn cross_crate_public_export_statement<'a>(
    line: &'a str,
    cross_crate_aliases: &BTreeSet<String>,
) -> Option<&'a str> {
    let trimmed = line.trim_start();
    if trimmed.starts_with("pub use rumoca_") || is_cross_crate_pub_type_alias(trimmed) {
        Some(trimmed)
    } else if let Some(rest) = trimmed.strip_prefix("pub use ")
        && let Some((alias, _)) = rest.split_once("::")
        && cross_crate_aliases.contains(alias)
    {
        Some(trimmed)
    } else {
        None
    }
}

pub(super) fn normalized_rel_path(rel: &Path) -> String {
    rel.to_string_lossy().replace('\\', "/")
}

pub(super) fn crate_name_for_rel_path(rel: &Path) -> Option<String> {
    let rel = normalized_rel_path(rel);
    let mut parts = rel.split('/');
    (parts.next() == Some("crates"))
        .then(|| parts.next().map(str::to_string))
        .flatten()
}

pub(super) fn is_approved_facade_crate(crate_name: &str) -> bool {
    matches!(crate_name, "rumoca-compile" | "rumoca-sim" | "rumoca-codec")
}

pub(super) fn is_non_facade_crate_file(rel: &Path) -> bool {
    crate_name_for_rel_path(rel)
        .as_deref()
        .is_some_and(|crate_name| !is_approved_facade_crate(crate_name))
}

pub(super) fn collect_root_pub_use_statements(content: &str) -> Vec<String> {
    let mut statements = Vec::new();
    let mut lines = content.lines();

    while let Some(line) = lines.next() {
        if !line.starts_with("pub use ") {
            continue;
        }
        let mut statement = line.trim().to_string();
        while !statement.trim_end().ends_with(';') {
            let Some(next_line) = lines.next() else {
                break;
            };
            statement.push(' ');
            statement.push_str(next_line.trim());
        }
        statements.push(statement);
    }

    statements
}

const TEXTUAL_MODEL_PATH_RECOVERY_DEBT: &[(&str, usize)] = &[];

const TEXTUAL_MODEL_PATH_HELPERS: &[&str] = &[
    "find_first_top_level_dot",
    "find_last_top_level_dot",
    "has_top_level_dot",
    "parent_scope",
    "rendered_top_level_segment",
    "split_first_top_level",
    "split_last_top_level",
    "split_path_with_indices",
    "top_level_last_segment",
];

pub(super) fn assert_no_direct_dot_tokenization_for_model_paths() {
    let root = workspace_root();
    let mut rs_files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut rs_files);
    let patterns = direct_dot_tokenization_patterns();

    let mut offenders = Vec::new();
    for path in rs_files {
        let rel = normalized_rel_path(path.strip_prefix(&root).unwrap_or(&path));
        if rel == "crates/rumoca/tests/architecture_hardening_test.rs" {
            continue;
        }
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        for (line_idx, line) in content.lines().enumerate() {
            let trimmed = line.trim_start();
            if trimmed.starts_with("//") {
                continue;
            }
            if patterns.iter().any(|pattern| line.contains(pattern)) {
                offenders.push(format!("{rel}:{}", line_idx + 1));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "found direct dot tokenization. Author reminder: Modelica name hierarchy \
must come from AST/IR structure (`ComponentReference.parts`, `QualifiedName`, `DefId`, scopes), \
not from splitting flattened display text. Boundary text parsers should use a named parser/helper \
with a narrow owner: {offenders:#?}"
    );
}

pub(super) fn assert_semantic_code_does_not_add_textual_model_path_recovery() {
    let root = workspace_root();
    let mut rs_files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut rs_files);

    let mut actual = BTreeMap::new();
    for path in rs_files {
        let rel = normalized_rel_path(path.strip_prefix(&root).unwrap_or(&path));
        if !is_textual_model_path_recovery_debt_checked_source(&rel) {
            continue;
        }
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        let count = textual_model_path_helper_count(&content);
        if count > 0 {
            actual.insert(rel, count);
        }
    }

    let expected = TEXTUAL_MODEL_PATH_RECOVERY_DEBT
        .iter()
        .map(|(path, count)| (path.to_string(), *count))
        .collect::<BTreeMap<_, _>>();

    let mut increased_or_untracked = Vec::new();
    for (path, count) in &actual {
        let baseline = expected.get(path).copied().unwrap_or(0);
        if *count > baseline {
            increased_or_untracked.push(format!("{path}: {count} > baseline {baseline}"));
        }
    }
    let stale_debt = expected
        .iter()
        .filter_map(|(path, baseline)| {
            let count = actual.get(path).copied().unwrap_or(0);
            (count < *baseline).then(|| format!("{path}: {count} < baseline {baseline}"))
        })
        .collect::<Vec<_>>();

    assert!(
        increased_or_untracked.is_empty(),
        "semantic compiler/evaluator code added textual Modelica path recovery. \
Author reminder: do not replace direct string splitting with `rumoca_core` path helpers in \
semantic code. Preserve/use structured IR name data instead. Existing entries are debt only, \
not precedent: {increased_or_untracked:#?}"
    );
    assert!(
        stale_debt.is_empty(),
        "textual Modelica path recovery debt decreased; update \
TEXTUAL_MODEL_PATH_RECOVERY_DEBT so CI tracks the lower baseline: {stale_debt:#?}"
    );
}

fn direct_dot_tokenization_patterns() -> Vec<String> {
    let dot = ".";
    ["split", "rsplit", "split_once", "rsplit_once"]
        .into_iter()
        .flat_map(|method| {
            [
                format!(".{method}('{dot}')"),
                format!(".{method}(\"{dot}\")"),
            ]
        })
        .collect()
}

fn textual_model_path_helper_count(content: &str) -> usize {
    content
        .lines()
        .filter(|line| !line.trim_start().starts_with("//"))
        .map(|line| {
            TEXTUAL_MODEL_PATH_HELPERS
                .iter()
                .map(|helper| line.matches(helper).count())
                .sum::<usize>()
        })
        .sum()
}

fn is_textual_model_path_recovery_debt_checked_source(rel: &str) -> bool {
    if !rel.contains("/src/") || !rel.ends_with(".rs") {
        return false;
    }
    if rel.contains("/tests/")
        || rel.ends_with("/tests.rs")
        || rel.ends_with("_test.rs")
        || rel.ends_with("_tests.rs")
        || rel.ends_with("/path_utils.rs")
        || rel.starts_with("crates/rumoca-phase-codegen/")
    {
        return false;
    }

    rel.starts_with("crates/rumoca-phase-")
        || rel.starts_with("crates/rumoca-eval-")
        || rel.starts_with("crates/rumoca-ir-")
}
