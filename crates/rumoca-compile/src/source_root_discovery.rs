use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;
use std::path::PathBuf;

use crate::session::{
    SourceRootActivityKind, SourceRootActivityPhase, SourceRootKind, SourceRootStatusSnapshot,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceRootDuplicateSkip {
    pub source_root_path: String,
    pub root_name: String,
    pub provider_path: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SourceRootLoadPlan {
    pub load_paths: Vec<String>,
    pub duplicate_root_skips: Vec<SourceRootDuplicateSkip>,
}

fn is_identifier_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

fn sanitize_root(name: &str) -> Option<String> {
    let root = name.trim_matches(|c: char| !c.is_ascii_alphanumeric() && c != '_');
    if root.is_empty() {
        return None;
    }
    Some(root.to_string())
}

/// Returns true if `needle` appears in `source` as an identifier token.
fn source_contains_identifier(source: &str, needle: &str) -> bool {
    if needle.is_empty() {
        return false;
    }
    let source_bytes = source.as_bytes();
    let needle_len = needle.len();
    let mut start = 0;
    while let Some(found) = source[start..].find(needle) {
        let idx = start + found;
        let left_ok = idx == 0 || !is_identifier_char(source_bytes[idx - 1]);
        let right_idx = idx + needle_len;
        let right_ok =
            right_idx >= source_bytes.len() || !is_identifier_char(source_bytes[right_idx]);
        if left_ok && right_ok {
            return true;
        }
        start = idx + 1;
    }
    false
}

/// Extract likely root class/package names declared at top-level in a file.
fn extract_declared_root_names(source: &str) -> Vec<String> {
    let mut roots = Vec::new();
    for line in source.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("//") {
            continue;
        }
        let mut iter = trimmed.split_whitespace().peekable();
        while matches!(
            iter.peek().copied(),
            Some("encapsulated" | "partial" | "final" | "redeclare")
        ) {
            iter.next();
        }

        // operator record Foo
        if matches!(iter.peek().copied(), Some("operator")) {
            iter.next();
            if !matches!(iter.peek().copied(), Some("record")) {
                continue;
            }
            iter.next();
            let Some(name) = iter.next() else {
                continue;
            };
            if let Some(root) = sanitize_root(name) {
                roots.push(root);
            }
            continue;
        }

        if matches!(
            iter.peek().copied(),
            Some(
                "package"
                    | "model"
                    | "block"
                    | "class"
                    | "record"
                    | "connector"
                    | "function"
                    | "type"
            )
        ) {
            iter.next();
            let Some(name) = iter.next() else {
                continue;
            };
            if let Some(root) = sanitize_root(name) {
                roots.push(root);
            }
        }
    }
    roots.sort();
    roots.dedup();
    roots
}

fn extract_top_level_roots_from_file(path: &Path) -> std::io::Result<Vec<String>> {
    let source = fs::read_to_string(path)?;
    let file_name = path.to_string_lossy().to_string();
    let mut roots = match rumoca_phase_parse::parse_to_ast(&source, &file_name) {
        Ok(def) => def.classes.keys().cloned().collect::<Vec<_>>(),
        Err(_) => extract_declared_root_names(&source),
    };
    roots.sort();
    roots.dedup();
    Ok(roots)
}

fn collect_nested_package_roots(level1: &[PathBuf]) -> std::io::Result<Vec<String>> {
    let mut roots = Vec::new();
    for dir in level1 {
        let mut level2: Vec<_> = fs::read_dir(dir)?
            .collect::<std::io::Result<Vec<_>>>()?
            .into_iter()
            .map(|entry| entry.path())
            .filter(|entry| entry.is_dir())
            .collect();
        level2.sort();
        for subdir in level2 {
            let pkg = subdir.join("package.mo");
            if !pkg.is_file() {
                continue;
            }
            roots.extend(extract_top_level_roots_from_file(&pkg)?);
        }
    }
    Ok(roots)
}

/// Infer root package/class names for a source-root path.
///
/// If inference fails to determine roots (e.g. directory without package.mo), returns
/// an empty list so callers can conservatively load.
fn infer_source_root_names(path: &Path) -> std::io::Result<Vec<String>> {
    if path.is_file() {
        return extract_top_level_roots_from_file(path);
    }

    if path.is_dir() {
        let package_file = path.join("package.mo");
        if package_file.is_file() {
            return extract_top_level_roots_from_file(&package_file);
        }

        // Support wrapped source-root layouts (e.g., ModelicaStandardLibrary_vX.Y.Z/Modelica X.Y.Z/package.mo)
        // by searching a shallow depth for nested package.mo files.
        let mut roots = Vec::new();
        let mut level1: Vec<_> = fs::read_dir(path)?
            .collect::<std::io::Result<Vec<_>>>()?
            .into_iter()
            .map(|entry| entry.path())
            .filter(|entry| entry.is_dir())
            .collect();
        level1.sort();

        for dir in &level1 {
            let pkg = dir.join("package.mo");
            if pkg.is_file() {
                roots.extend(extract_top_level_roots_from_file(&pkg)?);
            }
        }

        if roots.is_empty() {
            roots.extend(collect_nested_package_roots(&level1)?);
        }

        roots.sort();
        roots.dedup();
        return Ok(roots);
    }

    Err(std::io::Error::new(
        std::io::ErrorKind::NotFound,
        "source-root path does not exist",
    ))
}

/// Decide whether a source-root path should be loaded for this source.
///
/// Returns true when:
/// - root inference fails (conservative), or
/// - any inferred root package/class appears as an identifier token in source.
fn should_load_source_root_for_source(source: &str, path: &Path) -> std::io::Result<bool> {
    let roots = infer_source_root_names(path)?;
    if roots.is_empty() {
        return Ok(true);
    }
    Ok(roots
        .iter()
        .any(|root| source_contains_identifier(source, root)))
}

pub fn referenced_unloaded_source_root_paths(
    source: &str,
    source_root_paths: &[String],
    loaded_source_root_path_keys: &HashSet<String>,
) -> Vec<String> {
    let mut seen_source_root_paths = HashSet::new();
    let mut referenced_paths = Vec::new();
    for source_root_path in source_root_paths {
        let path_key = canonical_path_key(source_root_path);
        if !seen_source_root_paths.insert(path_key.clone()) {
            continue;
        }
        if loaded_source_root_path_keys.contains(&path_key) {
            continue;
        }
        let should_load =
            should_load_source_root_for_source(source, Path::new(source_root_path)).unwrap_or(true);
        if should_load {
            referenced_paths.push(source_root_path.clone());
        }
    }
    referenced_paths
}

fn existing_source_root_claims(
    loaded_source_root_path_keys: &HashSet<String>,
) -> (HashSet<String>, HashMap<String, String>) {
    let mut seen_source_root_paths = HashSet::new();
    let mut claimed_roots = HashMap::new();
    for loaded_path in loaded_source_root_path_keys {
        seen_source_root_paths.insert(canonical_path_key(loaded_path));
        for root in infer_source_root_names(Path::new(loaded_path)).unwrap_or_default() {
            claimed_roots
                .entry(root)
                .or_insert_with(|| loaded_path.clone());
        }
    }
    (seen_source_root_paths, claimed_roots)
}

fn duplicate_root_provider(
    inferred_roots: &[String],
    claimed_roots: &HashMap<String, String>,
) -> Option<(String, String)> {
    inferred_roots.iter().find_map(|root| {
        claimed_roots
            .get(root)
            .map(|provider| (root.clone(), provider.clone()))
    })
}

fn claim_roots(
    claimed_roots: &mut HashMap<String, String>,
    inferred_roots: Vec<String>,
    provider: &str,
) {
    for root in inferred_roots {
        claimed_roots
            .entry(root)
            .or_insert_with(|| provider.to_string());
    }
}

pub fn plan_source_root_loads(
    candidate_source_root_paths: &[String],
    loaded_source_root_path_keys: &HashSet<String>,
) -> SourceRootLoadPlan {
    let (mut seen_source_root_paths, mut claimed_roots) =
        existing_source_root_claims(loaded_source_root_path_keys);
    let mut load_paths = Vec::new();
    let mut duplicate_root_skips = Vec::new();
    for source_root_path in candidate_source_root_paths {
        let path_key = canonical_path_key(source_root_path);
        if !seen_source_root_paths.insert(path_key.clone())
            || loaded_source_root_path_keys.contains(&path_key)
        {
            continue;
        }
        let inferred_roots =
            infer_source_root_names(Path::new(source_root_path)).unwrap_or_default();
        if let Some((root_name, provider_path)) =
            duplicate_root_provider(&inferred_roots, &claimed_roots)
        {
            duplicate_root_skips.push(SourceRootDuplicateSkip {
                source_root_path: source_root_path.clone(),
                root_name,
                provider_path,
            });
            continue;
        }
        load_paths.push(source_root_path.clone());
        claim_roots(&mut claimed_roots, inferred_roots, source_root_path);
    }
    SourceRootLoadPlan {
        load_paths,
        duplicate_root_skips,
    }
}

pub fn source_requires_unloaded_source_roots(
    source: &str,
    source_root_paths: &[String],
    loaded_source_root_path_keys: &HashSet<String>,
) -> bool {
    !referenced_unloaded_source_root_paths(source, source_root_paths, loaded_source_root_path_keys)
        .is_empty()
}

pub fn sources_require_loaded_source_roots<'a, I>(
    sources: I,
    loaded_source_root_paths: &HashSet<String>,
) -> bool
where
    I: IntoIterator<Item = &'a str>,
{
    let sources = sources.into_iter().collect::<Vec<_>>();
    loaded_source_root_paths.iter().any(|source_root_path| {
        sources.iter().any(|source| {
            should_load_source_root_for_source(source, Path::new(source_root_path)).unwrap_or(true)
        })
    })
}

pub fn canonical_path_key(path: &str) -> String {
    fs::canonicalize(path)
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_else(|_| path.to_string())
}

pub fn merge_source_root_paths(
    project_source_root_paths: &[String],
    initial_source_root_paths: &[String],
) -> Vec<String> {
    let mut seen = HashSet::new();
    let mut merged = Vec::new();
    for path in project_source_root_paths
        .iter()
        .chain(initial_source_root_paths.iter())
    {
        let key = canonical_path_key(path);
        if seen.insert(key) {
            merged.push(path.clone());
        }
    }
    merged
}

pub fn source_root_paths_changed(previous_paths: &[String], next_paths: &[String]) -> bool {
    previous_paths
        .iter()
        .map(|path| canonical_path_key(path))
        .collect::<Vec<_>>()
        != next_paths
            .iter()
            .map(|path| canonical_path_key(path))
            .collect::<Vec<_>>()
}

pub fn classify_configured_source_root_kind(
    source_root_path: &str,
    durable_source_root_paths: &[String],
) -> SourceRootKind {
    let source_root_path_key = canonical_path_key(source_root_path);
    if durable_source_root_paths
        .iter()
        .any(|path| canonical_path_key(path) == source_root_path_key)
    {
        return SourceRootKind::DurableExternal;
    }
    SourceRootKind::External
}

pub fn source_root_source_set_key(source_root_path: &str) -> String {
    format!("source_root::{}", canonical_path_key(source_root_path))
}

pub fn source_root_status_display_name(path_or_key: &str) -> String {
    let inferred_roots = infer_source_root_names(Path::new(path_or_key)).unwrap_or_default();
    if inferred_roots.is_empty() {
        return Path::new(path_or_key)
            .file_stem()
            .or_else(|| Path::new(path_or_key).file_name())
            .and_then(|name| name.to_str())
            .unwrap_or(path_or_key)
            .to_string();
    }
    if inferred_roots.len() <= 3 {
        return inferred_roots.join(", ");
    }
    format!(
        "{}, {}, {} (+{} more)",
        inferred_roots[0],
        inferred_roots[1],
        inferred_roots[2],
        inferred_roots.len() - 3
    )
}

fn source_root_activity_label(kind: SourceRootActivityKind) -> &'static str {
    match kind {
        SourceRootActivityKind::ColdIndexBuild => "cold index build",
        SourceRootActivityKind::WarmCacheRestore => "warm cache restore",
        SourceRootActivityKind::SubtreeReindex => "subtree reindex",
    }
}

fn source_root_activity_scope_suffix(dirty_class_prefixes: &[String]) -> String {
    if dirty_class_prefixes.is_empty() {
        return String::new();
    }
    format!(" for {}", dirty_class_prefixes.join(", "))
}

fn source_root_cache_status_label(
    status: crate::source_root_cache::SourceRootCacheStatus,
) -> &'static str {
    match status {
        crate::source_root_cache::SourceRootCacheStatus::Hit => "cache hit",
        crate::source_root_cache::SourceRootCacheStatus::Miss => "cache miss",
        crate::source_root_cache::SourceRootCacheStatus::Disabled => "cache disabled",
    }
}

pub fn render_source_root_status_message(status: &SourceRootStatusSnapshot) -> String {
    let display_target = status
        .source_root_path
        .as_deref()
        .unwrap_or(&status.source_root_key);
    let source_root_name = source_root_status_display_name(display_target);
    let activity = status.current.as_ref().or(status.last_completed.as_ref());
    let Some(activity) = activity else {
        return format!("[rumoca] Source root {}: idle.", source_root_name);
    };
    let phase = match activity.phase {
        SourceRootActivityPhase::Pending => "pending",
        SourceRootActivityPhase::Running => "running",
        SourceRootActivityPhase::Completed => "completed",
    };
    format!(
        "[rumoca] Source root {}: {} {}{}.",
        source_root_name,
        source_root_activity_label(activity.kind),
        phase,
        source_root_activity_scope_suffix(&activity.dirty_class_prefixes),
    )
}

pub fn render_source_root_indexing_started_message(source_root_path: &str, reason: &str) -> String {
    let source_root_name = source_root_status_display_name(source_root_path);
    format!(
        "[rumoca] Indexing source root {} for {}. This may use CPU while Rumoca parses and resolves it. Path: {}",
        source_root_name, reason, source_root_path
    )
}

pub fn render_source_root_indexing_finished_message(
    source_root_path: &str,
    reason: &str,
    parsed_file_count: usize,
    inserted_file_count: usize,
    cache_status: crate::source_root_cache::SourceRootCacheStatus,
) -> String {
    let source_root_name = source_root_status_display_name(source_root_path);
    format!(
        "[rumoca] Indexing done for source root {} ({}; {} files indexed, {} inserted, {}).",
        source_root_name,
        reason,
        parsed_file_count,
        inserted_file_count,
        source_root_cache_status_label(cache_status),
    )
}

pub fn render_source_root_indexing_failed_message(
    source_root_path: &str,
    reason: &str,
    error: &str,
) -> String {
    let source_root_name = source_root_status_display_name(source_root_path);
    format!(
        "[rumoca] Indexing failed for source root {} during {}: {}",
        source_root_name, reason, error
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_contains_identifier_honors_boundaries() {
        assert!(source_contains_identifier(
            "import Modelica.Blocks;",
            "Modelica"
        ));
        assert!(!source_contains_identifier(
            "import SuperModelica.Blocks;",
            "Modelica"
        ));
    }

    #[test]
    fn extract_declared_root_names_finds_package_and_operator_record() {
        let source = r#"
package Modelica
end Modelica;
operator record SE2
end SE2;
"#;
        let roots = extract_declared_root_names(source);
        assert!(roots.contains(&"Modelica".to_string()));
        assert!(roots.contains(&"SE2".to_string()));
    }

    #[test]
    fn infer_source_root_names_supports_wrapped_layout() {
        let temp = tempfile::tempdir().expect("tempdir");
        let wrapped = temp.path().join("ModelicaStandardLibrary_v4.1.0");
        let nested = wrapped.join("Modelica 4.1.0");
        std::fs::create_dir_all(&nested).expect("mkdir");
        std::fs::write(nested.join("package.mo"), "package Modelica\nend Modelica;")
            .expect("write package.mo");

        let roots = infer_source_root_names(&wrapped).expect("infer roots");
        assert_eq!(roots, vec!["Modelica".to_string()]);
    }

    #[test]
    fn source_root_status_display_name_prefers_inferred_roots_for_wrapped_layout() {
        let temp = tempfile::tempdir().expect("tempdir");
        let wrapped = temp.path().join("wrapped-source-root-layout");
        let modelica = wrapped.join("Modelica 4.1.0");
        let services = wrapped.join("ModelicaServices 4.1.0");
        std::fs::create_dir_all(&modelica).expect("mkdir Modelica");
        std::fs::create_dir_all(&services).expect("mkdir ModelicaServices");
        std::fs::write(
            modelica.join("package.mo"),
            "package Modelica\nend Modelica;\n",
        )
        .expect("write Modelica package");
        std::fs::write(
            services.join("package.mo"),
            "package ModelicaServices\nend ModelicaServices;\n",
        )
        .expect("write ModelicaServices package");

        let display_name = source_root_status_display_name(wrapped.to_string_lossy().as_ref());
        assert_eq!(display_name, "Modelica, ModelicaServices");
    }

    #[test]
    fn render_source_root_status_message_prefers_pending_subtree_reindex() {
        let message = render_source_root_status_message(&SourceRootStatusSnapshot {
            source_root_key: "workspace::NewFolder".to_string(),
            source_root_path: Some("/tmp/NewFolder".to_string()),
            current: Some(crate::session::SourceRootActivitySnapshot {
                kind: SourceRootActivityKind::SubtreeReindex,
                phase: SourceRootActivityPhase::Pending,
                dirty_class_prefixes: vec!["NewFolder".to_string(), "NewFolder.Test".to_string()],
            }),
            last_completed: Some(crate::session::SourceRootActivitySnapshot {
                kind: SourceRootActivityKind::WarmCacheRestore,
                phase: SourceRootActivityPhase::Completed,
                dirty_class_prefixes: Vec::new(),
            }),
        });
        assert_eq!(
            message,
            "[rumoca] Source root NewFolder: subtree reindex pending for NewFolder, NewFolder.Test."
        );
    }

    #[test]
    fn render_source_root_indexing_started_message_explains_cpu_use() {
        let message = render_source_root_indexing_started_message(
            "/opt/modelica/Modelica",
            "editor completion/imports",
        );
        assert!(message.contains("Indexing source root Modelica"));
        assert!(message.contains("This may use CPU"));
        assert!(message.contains("editor completion/imports"));
        assert!(message.contains("Path: /opt/modelica/Modelica"));
    }

    #[test]
    fn render_source_root_indexing_finished_message_reports_outcome() {
        let message = render_source_root_indexing_finished_message(
            "/opt/modelica/Modelica",
            "save diagnostics",
            2510,
            2510,
            crate::source_root_cache::SourceRootCacheStatus::Miss,
        );
        assert!(message.contains("Indexing done for source root Modelica"));
        assert!(message.contains("save diagnostics"));
        assert!(message.contains("2510 files indexed"));
        assert!(message.contains("cache miss"));
    }

    #[test]
    fn render_source_root_indexing_failed_message_reports_reason() {
        let message = render_source_root_indexing_failed_message(
            "/opt/modelica/Modelica",
            "simulation compile after source-root edits",
            "missing package.mo",
        );
        assert!(message.contains("Indexing failed for source root Modelica"));
        assert!(message.contains("simulation compile after source-root edits"));
        assert!(message.contains("missing package.mo"));
    }

    #[test]
    fn source_requires_unloaded_source_roots_ignores_loaded_and_duplicate_paths() {
        let loaded = HashSet::from([canonical_path_key("/tmp/Modelica"), String::from("/tmp/B")]);
        let source_root_paths = vec![
            "/tmp/Modelica".to_string(),
            "/tmp/Modelica".to_string(),
            "/tmp/B".to_string(),
            "/tmp/C".to_string(),
        ];
        let source = "model Active\n  C.A a;\nend Active;\n";
        assert!(source_requires_unloaded_source_roots(
            source,
            &source_root_paths,
            &loaded
        ));
    }

    #[test]
    fn referenced_unloaded_source_root_paths_returns_only_referenced_unloaded_roots() {
        let loaded = HashSet::from([canonical_path_key("/tmp/B")]);
        let source_root_paths = vec![
            "/tmp/A".to_string(),
            "/tmp/A".to_string(),
            "/tmp/B".to_string(),
            "/tmp/C".to_string(),
        ];
        let source = "model Active\n  A.X ax;\n  C.Y cy;\nend Active;\n";
        assert_eq!(
            referenced_unloaded_source_root_paths(source, &source_root_paths, &loaded),
            vec!["/tmp/A".to_string(), "/tmp/C".to_string()]
        );
    }

    #[test]
    fn sources_require_loaded_source_roots_ignores_unreferenced_loaded_roots() {
        let temp = tempfile::tempdir().expect("tempdir");
        let modelica_root = temp.path().join("Modelica");
        let lib_root = temp.path().join("Lib");
        std::fs::create_dir_all(&modelica_root).expect("mkdir modelica root");
        std::fs::create_dir_all(&lib_root).expect("mkdir lib root");
        std::fs::write(
            modelica_root.join("package.mo"),
            "package Modelica\nend Modelica;",
        )
        .expect("write modelica package");
        std::fs::write(lib_root.join("package.mo"), "package Lib\nend Lib;")
            .expect("write lib package");
        let loaded_source_root_paths = HashSet::from([
            modelica_root.to_string_lossy().to_string(),
            lib_root.to_string_lossy().to_string(),
        ]);
        let local_sources = [
            "model Local\n  Real x;\nend Local;\n",
            "model UsesModelica\n  Modelica.SIunits.Time t;\nend UsesModelica;\n",
        ];
        assert!(sources_require_loaded_source_roots(
            local_sources.iter().copied(),
            &loaded_source_root_paths
        ));
        let local_only_sources = ["model Local\n  Real x;\nend Local;\n"];
        assert!(!sources_require_loaded_source_roots(
            local_only_sources.iter().copied(),
            &loaded_source_root_paths
        ));
    }

    #[test]
    fn plan_source_root_loads_skips_duplicate_roots_after_first_provider() {
        let temp = tempfile::tempdir().expect("tempdir");
        let lib_a = temp.path().join("lib_a");
        let lib_b = temp.path().join("lib_b");
        std::fs::create_dir_all(&lib_a).expect("mkdir lib_a");
        std::fs::create_dir_all(&lib_b).expect("mkdir lib_b");
        std::fs::write(lib_a.join("package.mo"), "package Lib\nend Lib;")
            .expect("write lib_a package");
        std::fs::write(lib_b.join("package.mo"), "package Lib\nend Lib;")
            .expect("write lib_b package");
        let candidate_source_root_paths = vec![
            lib_a.to_string_lossy().to_string(),
            lib_b.to_string_lossy().to_string(),
        ];
        let plan = plan_source_root_loads(&candidate_source_root_paths, &HashSet::new());
        assert_eq!(plan.load_paths, vec![lib_a.to_string_lossy().to_string()]);
        assert_eq!(plan.duplicate_root_skips.len(), 1);
        assert_eq!(
            plan.duplicate_root_skips[0],
            SourceRootDuplicateSkip {
                source_root_path: lib_b.to_string_lossy().to_string(),
                root_name: "Lib".to_string(),
                provider_path: lib_a.to_string_lossy().to_string(),
            }
        );
    }

    #[test]
    fn merge_source_root_paths_prefers_project_order_and_dedups_by_canonical_path() {
        let temp = tempfile::tempdir().expect("tempdir");
        let project_root = temp.path().join("ProjectRoot");
        let initial_root = temp.path().join("InitialRoot");
        std::fs::create_dir_all(&project_root).expect("mkdir project root");
        std::fs::create_dir_all(&initial_root).expect("mkdir initial root");
        let merged = merge_source_root_paths(
            &[
                project_root.to_string_lossy().to_string(),
                project_root.to_string_lossy().to_string(),
            ],
            &[
                project_root.to_string_lossy().to_string(),
                initial_root.to_string_lossy().to_string(),
            ],
        );
        assert_eq!(
            merged,
            vec![
                project_root.to_string_lossy().to_string(),
                initial_root.to_string_lossy().to_string()
            ]
        );
    }

    #[test]
    fn classify_configured_source_root_kind_marks_initial_roots_as_durable() {
        let durable_paths = vec!["/tmp/msl".to_string()];
        assert_eq!(
            classify_configured_source_root_kind("/tmp/msl", &durable_paths),
            SourceRootKind::DurableExternal
        );
        assert_eq!(
            classify_configured_source_root_kind("/tmp/local-lib", &durable_paths),
            SourceRootKind::External
        );
    }

    #[test]
    fn source_root_paths_changed_compares_canonicalized_path_sets_in_order() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root_a = temp.path().join("A");
        let root_b = temp.path().join("B");
        std::fs::create_dir_all(&root_a).expect("mkdir A");
        std::fs::create_dir_all(&root_b).expect("mkdir B");
        assert!(!source_root_paths_changed(
            &[root_a.to_string_lossy().to_string()],
            &[root_a.to_string_lossy().to_string()]
        ));
        assert!(source_root_paths_changed(
            &[root_a.to_string_lossy().to_string()],
            &[root_b.to_string_lossy().to_string()]
        ));
    }
}
