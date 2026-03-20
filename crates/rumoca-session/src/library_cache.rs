use anyhow::{Context, Result};
use rayon::prelude::*;
use rumoca_ir_ast::StoredDefinition;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::OnceLock;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use crate::instrumentation::record_library_cache_result;
use crate::package_layout::{collect_library_source_files, validate_library_package_layout};
use crate::parsed_artifact_cache::{
    parse_file_with_precomputed_hash_status, resolve_parsed_artifact_cache_dir_from_root,
};

const LIBRARY_CACHE_SCHEMA_VERSION: u32 = 1;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LibraryCacheStatus {
    Hit,
    Miss,
    Disabled,
}

#[derive(Debug, Clone)]
pub struct ParsedLibrary {
    pub documents: Vec<(String, StoredDefinition)>,
    pub file_count: usize,
    pub cache_status: LibraryCacheStatus,
    pub cache_key: String,
    pub cache_file: Option<PathBuf>,
    pub timing: LibraryCacheTiming,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct LibraryCacheTiming {
    pub collect_files_ms: u64,
    pub hash_inputs_ms: u64,
    pub cache_lookup_ms: u64,
    pub cache_deserialize_ms: u64,
    pub parse_files_ms: u64,
    pub validate_layout_ms: u64,
    pub cache_write_ms: u64,
    pub total_ms: u64,
}

#[derive(Debug, Clone)]
struct HashedLibraryFile {
    path: PathBuf,
    source_hash: String,
}

#[derive(Debug, Clone)]
struct LibraryInputHash {
    cache_key: String,
    files: Vec<HashedLibraryFile>,
}

fn env_flag_is_truthy(var: &str) -> bool {
    std::env::var(var)
        .map(|value| {
            matches!(
                value.trim().to_ascii_lowercase().as_str(),
                "1" | "true" | "yes" | "on"
            )
        })
        .unwrap_or(false)
}

fn cache_exe_fingerprint() -> String {
    std::env::current_exe()
        .ok()
        .and_then(|path| {
            let metadata = fs::metadata(&path).ok()?;
            let modified = metadata
                .modified()
                .map(system_time_to_nanos)
                .unwrap_or_default();
            Some(format!(
                "{}:{}:{}:{}",
                path.display(),
                metadata.len(),
                modified.as_secs(),
                modified.subsec_nanos()
            ))
        })
        .unwrap_or_else(|| "unknown-exe".to_string())
}

fn recursive_collect_compiler_source_files(
    dir: &Path,
    out: &mut Vec<PathBuf>,
) -> std::io::Result<()> {
    let mut entries: Vec<_> = fs::read_dir(dir)?.collect::<std::io::Result<Vec<_>>>()?;
    entries.sort_by_key(|entry| entry.path());
    for entry in entries {
        let path = entry.path();
        if path.is_dir() {
            recursive_collect_compiler_source_files(&path, out)?;
            continue;
        }
        let file_name = path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("");
        let extension = path.extension().and_then(|ext| ext.to_str()).unwrap_or("");
        if file_name == "Cargo.toml" || extension == "rs" || extension == "toml" {
            out.push(path);
        }
    }
    Ok(())
}

fn compiler_source_fingerprint() -> Option<String> {
    let session_crate_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = session_crate_dir.parent()?.parent()?;
    if !workspace_root.join("Cargo.toml").is_file() {
        return None;
    }

    let mut files = Vec::new();
    for name in ["Cargo.toml", "Cargo.lock", "rust-toolchain.toml"] {
        let candidate = workspace_root.join(name);
        if candidate.is_file() {
            files.push(candidate);
        }
    }

    let crates_dir = workspace_root.join("crates");
    if !crates_dir.is_dir() {
        return None;
    }
    recursive_collect_compiler_source_files(&crates_dir, &mut files).ok()?;
    files.sort();

    let mut entries: Vec<(String, u64, [u8; 32])> = files
        .par_iter()
        .map(|file| -> std::io::Result<(String, u64, [u8; 32])> {
            let rel = file
                .strip_prefix(workspace_root)
                .unwrap_or(file)
                .to_string_lossy()
                .to_string();
            let bytes = fs::read(file)?;
            let digest = *blake3::hash(&bytes).as_bytes();
            Ok((rel, bytes.len() as u64, digest))
        })
        .collect::<std::io::Result<Vec<_>>>()
        .ok()?;
    entries.sort_by(|a, b| a.0.cmp(&b.0));

    let mut hasher = blake3::Hasher::new();
    hasher.update(b"compiler-source-v2\n");
    for (rel, size, digest) in entries {
        hasher.update(rel.as_bytes());
        hasher.update(b"\n");
        hasher.update(&size.to_le_bytes());
        hasher.update(&digest);
    }

    Some(hasher.finalize().to_hex().to_string())
}

pub(crate) fn cache_compiler_version() -> String {
    static CACHED: OnceLock<String> = OnceLock::new();

    if let Some(explicit) = std::env::var_os("RUMOCA_LIBRARY_CACHE_COMPILER_FINGERPRINT") {
        let explicit = explicit.to_string_lossy().trim().to_string();
        if !explicit.is_empty() {
            return format!("rumoca-session/{}/{}", env!("CARGO_PKG_VERSION"), explicit);
        }
    }

    CACHED
        .get_or_init(|| {
            if env_flag_is_truthy("RUMOCA_LIBRARY_CACHE_STRICT_EXE_FINGERPRINT") {
                return format!(
                    "rumoca-session/{}/exe:{}",
                    env!("CARGO_PKG_VERSION"),
                    cache_exe_fingerprint()
                );
            }

            if let Some(source_fingerprint) = compiler_source_fingerprint() {
                return format!(
                    "rumoca-session/{}/src:{}",
                    env!("CARGO_PKG_VERSION"),
                    source_fingerprint
                );
            }

            format!(
                "rumoca-session/{}/exe:{}",
                env!("CARGO_PKG_VERSION"),
                cache_exe_fingerprint()
            )
        })
        .clone()
}

fn system_time_to_nanos(time: SystemTime) -> Duration {
    time.duration_since(UNIX_EPOCH).unwrap_or_default()
}

fn recursive_collect_dirs(dir: &Path, out: &mut Vec<PathBuf>) -> std::io::Result<()> {
    out.push(dir.to_path_buf());
    let mut entries: Vec<_> = fs::read_dir(dir)?.collect::<std::io::Result<Vec<_>>>()?;
    entries.sort_by_key(|entry| entry.path());
    for entry in entries {
        let path = entry.path();
        if path.is_dir() {
            recursive_collect_dirs(&path, out)?;
        }
    }
    Ok(())
}

fn collect_modelica_files(path: &Path) -> std::io::Result<Vec<PathBuf>> {
    collect_library_source_files(path).map_err(|err| std::io::Error::other(err.to_string()))
}

fn hash_library_inputs(path: &Path, files: &[PathBuf]) -> std::io::Result<LibraryInputHash> {
    let canonical_root = fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf());
    let mut dirs = Vec::new();
    if path.is_dir() {
        recursive_collect_dirs(path, &mut dirs)?;
        dirs.sort();
    }
    let mut entries: Vec<(PathBuf, String, u64, [u8; 32])> = files
        .par_iter()
        .map(
            |file| -> std::io::Result<(PathBuf, String, u64, [u8; 32])> {
                let canonical_file = fs::canonicalize(file).unwrap_or_else(|_| file.to_path_buf());
                let rel = canonical_file
                    .strip_prefix(&canonical_root)
                    .unwrap_or(&canonical_file)
                    .to_string_lossy()
                    .to_string();
                let bytes = fs::read(file)?;
                let digest = *blake3::hash(&bytes).as_bytes();
                Ok((file.clone(), rel, bytes.len() as u64, digest))
            },
        )
        .collect::<std::io::Result<Vec<_>>>()?;
    let hashed_files = entries
        .iter()
        .map(|(path, _, _, digest)| HashedLibraryFile {
            path: path.clone(),
            source_hash: blake3::Hash::from(*digest).to_hex().to_string(),
        })
        .collect();
    entries.sort_by(|a, b| a.1.cmp(&b.1));

    let mut hasher = blake3::Hasher::new();
    hasher.update(format!("schema={}\n", LIBRARY_CACHE_SCHEMA_VERSION).as_bytes());
    hasher.update(format!("compiler={}\n", cache_compiler_version()).as_bytes());
    hasher.update(canonical_root.to_string_lossy().as_bytes());
    hasher.update(b"\n");
    for dir in dirs {
        let canonical_dir = fs::canonicalize(&dir).unwrap_or(dir);
        let rel = canonical_dir
            .strip_prefix(&canonical_root)
            .unwrap_or(&canonical_dir)
            .to_string_lossy()
            .to_string();
        hasher.update(b"dir:");
        hasher.update(rel.as_bytes());
        hasher.update(b"\n");
    }
    for (_, rel, size, digest) in entries {
        hasher.update(rel.as_bytes());
        hasher.update(b"\n");
        hasher.update(&size.to_le_bytes());
        hasher.update(&digest);
    }

    Ok(LibraryInputHash {
        cache_key: hasher.finalize().to_hex().to_string(),
        files: hashed_files,
    })
}

fn absolutize_cache_path(path: PathBuf) -> PathBuf {
    if path.is_absolute() {
        return path;
    }
    match std::env::current_dir() {
        Ok(cwd) => cwd.join(path),
        Err(_) => path,
    }
}

#[cfg(any(target_os = "windows", target_os = "macos", unix))]
fn home_dir() -> Option<PathBuf> {
    #[cfg(windows)]
    {
        if let Some(profile) = std::env::var_os("USERPROFILE") {
            let profile = PathBuf::from(profile);
            if !profile.as_os_str().is_empty() {
                return Some(profile);
            }
        }

        let drive = std::env::var_os("HOMEDRIVE");
        let path = std::env::var_os("HOMEPATH");
        if let (Some(drive), Some(path)) = (drive, path) {
            let mut root = PathBuf::from(drive);
            root.push(path);
            if !root.as_os_str().is_empty() {
                return Some(root);
            }
        }
        None
    }
    #[cfg(not(windows))]
    {
        std::env::var_os("HOME")
            .map(PathBuf::from)
            .filter(|path| !path.as_os_str().is_empty())
    }
}

fn default_cache_root_dir() -> PathBuf {
    #[cfg(target_os = "windows")]
    {
        if let Some(local_app_data) = std::env::var_os("LOCALAPPDATA") {
            let base = PathBuf::from(local_app_data);
            if !base.as_os_str().is_empty() {
                return base.join("Rumoca");
            }
        }
        if let Some(home) = home_dir() {
            return home.join("AppData").join("Local").join("Rumoca");
        }
    }

    #[cfg(target_os = "macos")]
    {
        if let Some(home) = home_dir() {
            return home.join("Library").join("Caches").join("rumoca");
        }
    }

    #[cfg(all(unix, not(target_os = "macos")))]
    {
        if let Some(xdg_cache_home) = std::env::var_os("XDG_CACHE_HOME") {
            let base = PathBuf::from(xdg_cache_home);
            if !base.as_os_str().is_empty() {
                return absolutize_cache_path(base).join("rumoca");
            }
        }
        if let Some(home) = home_dir() {
            return home.join(".cache").join("rumoca");
        }
    }

    std::env::temp_dir().join("rumoca")
}

fn resolve_library_cache_dir_from_override(override_dir: Option<PathBuf>) -> Option<PathBuf> {
    if let Some(path) = override_dir {
        if path.as_os_str().is_empty() {
            return None;
        }
        return Some(absolutize_cache_path(path).join("library"));
    }
    Some(default_cache_root_dir().join("library"))
}

pub fn resolve_library_cache_dir() -> Option<PathBuf> {
    let override_dir = std::env::var_os("RUMOCA_CACHE_DIR").map(PathBuf::from);
    resolve_library_cache_dir_from_override(override_dir)
}

fn elapsed_ms(started: Instant) -> u64 {
    started.elapsed().as_millis() as u64
}

fn usable_cache_dir(cache_dir: Option<PathBuf>) -> Option<PathBuf> {
    let cache_dir = cache_dir?;
    fs::create_dir_all(&cache_dir).ok()?;
    Some(cache_dir)
}

fn load_library_documents_from_artifact_cache(
    files: &[HashedLibraryFile],
    cache_dir: Option<&Path>,
) -> Result<(Vec<(String, StoredDefinition)>, bool)> {
    let docs = files
        .par_iter()
        .map(|file| {
            parse_file_with_precomputed_hash_status(&file.path, &file.source_hash, cache_dir)
        })
        .collect::<Result<Vec<_>>>()?;
    let all_hits = docs.iter().all(|(_, _, status)| {
        *status == crate::parsed_artifact_cache::ParsedArtifactCacheStatus::Hit
    });
    Ok((
        docs.into_iter()
            .map(|(uri, definition, _)| (uri, definition))
            .collect(),
        all_hits,
    ))
}

pub fn parse_library_with_cache_in(path: &Path, cache_dir: Option<&Path>) -> Result<ParsedLibrary> {
    let total_started = Instant::now();
    let mut timing = LibraryCacheTiming::default();

    let collect_started = Instant::now();
    let files = collect_modelica_files(path)
        .with_context(|| format!("collect .mo files under {}", path.display()))?;
    timing.collect_files_ms = elapsed_ms(collect_started);

    let hash_started = Instant::now();
    let input_hash = hash_library_inputs(path, &files)
        .with_context(|| format!("fingerprint {}", path.display()))?;
    timing.hash_inputs_ms = elapsed_ms(hash_started);
    let parsed_artifact_cache_dir =
        usable_cache_dir(resolve_parsed_artifact_cache_dir_from_root(cache_dir));
    let load_started = Instant::now();
    let (docs, all_hits) = load_library_documents_from_artifact_cache(
        &input_hash.files,
        parsed_artifact_cache_dir.as_deref(),
    )
    .with_context(|| format!("parse library files under {}", path.display()))?;
    let load_ms = elapsed_ms(load_started);
    if cache_dir.is_some() {
        if all_hits {
            timing.cache_deserialize_ms = load_ms;
        } else {
            timing.parse_files_ms = load_ms;
        }
    } else {
        timing.parse_files_ms = load_ms;
    }
    let validate_started = Instant::now();
    validate_library_package_layout(path, &docs)?;
    timing.validate_layout_ms = elapsed_ms(validate_started);
    let cache_status = match (parsed_artifact_cache_dir.is_some(), all_hits) {
        (true, true) => LibraryCacheStatus::Hit,
        (true, false) => LibraryCacheStatus::Miss,
        (false, _) => LibraryCacheStatus::Disabled,
    };
    record_library_cache_result(
        cache_status,
        if cache_status == LibraryCacheStatus::Hit {
            0
        } else {
            files.len()
        },
    );
    timing.total_ms = elapsed_ms(total_started);
    Ok(ParsedLibrary {
        documents: docs,
        file_count: files.len(),
        cache_status,
        cache_key: input_hash.cache_key,
        cache_file: None,
        timing,
    })
}

pub fn parse_library_with_cache(path: &Path) -> Result<ParsedLibrary> {
    parse_library_with_cache_in(path, resolve_library_cache_dir().as_deref())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::libraries::PackageLayoutError;
    use crate::parse::parse_files_parallel_with_cache_statuses;

    #[test]
    fn library_cache_hits_after_first_parse() {
        let temp = tempfile::tempdir().expect("tempdir");
        let lib_dir = temp.path().join("lib");
        let cache_dir = temp.path().join("cache");
        std::fs::create_dir_all(&lib_dir).expect("mkdir");
        std::fs::write(
            lib_dir.join("package.mo"),
            "package Lib\n  model M\n    Real x;\n  equation\n    der(x)=1;\n  end M;\nend Lib;",
        )
        .expect("write package");

        let first = parse_library_with_cache_in(&lib_dir, Some(&cache_dir)).expect("first parse");
        assert_eq!(first.cache_status, LibraryCacheStatus::Miss);
        assert_eq!(first.file_count, 1);
        assert!(
            first.timing.total_ms
                >= first.timing.collect_files_ms
                    + first.timing.hash_inputs_ms
                    + first.timing.parse_files_ms
                    + first.timing.validate_layout_ms
                    + first.timing.cache_write_ms
        );

        let second = parse_library_with_cache_in(&lib_dir, Some(&cache_dir)).expect("second parse");
        assert_eq!(second.cache_status, LibraryCacheStatus::Hit);
        assert_eq!(second.file_count, 1);
        assert!(
            second.timing.total_ms
                >= second.timing.collect_files_ms
                    + second.timing.hash_inputs_ms
                    + second.timing.cache_lookup_ms
                    + second.timing.cache_deserialize_ms
                    + second.timing.validate_layout_ms
        );
        assert!(second.cache_file.is_none());
    }

    #[test]
    fn resolve_library_cache_dir_is_absolute_and_stable() {
        let path = resolve_library_cache_dir().expect("cache dir should resolve by default");
        assert!(path.is_absolute(), "cache dir must be absolute: {path:?}");
        assert_eq!(
            path.file_name().and_then(|name| name.to_str()),
            Some("library")
        );
    }

    #[test]
    fn resolve_library_cache_dir_uses_override_and_appends_library_dir() {
        let path =
            resolve_library_cache_dir_from_override(Some(PathBuf::from("custom-cache-root")))
                .expect("override should resolve");
        assert!(
            path.is_absolute(),
            "override should resolve to absolute path"
        );
        assert_eq!(
            path.file_name().and_then(|name| name.to_str()),
            Some("library")
        );
        assert_eq!(
            path.parent()
                .and_then(|parent| parent.file_name())
                .and_then(|name| name.to_str()),
            Some("custom-cache-root")
        );
    }

    #[test]
    fn resolve_library_cache_dir_empty_override_disables_cache() {
        let path = resolve_library_cache_dir_from_override(Some(PathBuf::new()));
        assert!(path.is_none(), "empty override should disable cache");
    }

    #[test]
    fn parse_library_with_cache_falls_back_when_cache_path_is_unusable() {
        let temp = tempfile::tempdir().expect("tempdir");
        let lib_dir = temp.path().join("lib");
        std::fs::create_dir_all(&lib_dir).expect("mkdir");
        std::fs::write(
            lib_dir.join("package.mo"),
            "package Lib model M Real x; equation der(x)=1; end M; end Lib;",
        )
        .expect("write package");

        let blocked_path = temp.path().join("blocked-cache-path");
        std::fs::write(&blocked_path, "this is a file, not a directory").expect("write file");

        let parsed = parse_library_with_cache_in(&lib_dir, Some(&blocked_path))
            .expect("cache failure should not fail parsing");
        assert_eq!(parsed.cache_status, LibraryCacheStatus::Disabled);
        assert_eq!(parsed.file_count, 1);
        assert!(parsed.cache_file.is_none());
    }

    #[test]
    fn parse_library_with_cache_ignores_non_package_resource_mo_files() {
        let temp = tempfile::tempdir().expect("tempdir");
        let lib_dir = temp.path().join("lib");
        let cache_dir = temp.path().join("cache");
        std::fs::create_dir_all(lib_dir.join("Resources/Images/Docs")).expect("mkdir");
        std::fs::write(lib_dir.join("package.mo"), "package Lib\nend Lib;").expect("write package");
        std::fs::write(lib_dir.join("A.mo"), "within Lib;\nmodel A\nend A;").expect("write child");
        std::fs::write(
            lib_dir.join("Resources/Images/Docs/Demo.mo"),
            "model Demo\nend Demo;",
        )
        .expect("write resource demo");

        let parsed = parse_library_with_cache_in(&lib_dir, Some(&cache_dir)).expect("parse");
        assert_eq!(parsed.file_count, 2);
        assert_eq!(parsed.documents.len(), 2);
        assert!(
            parsed
                .documents
                .iter()
                .all(|(uri, _)| !uri.contains("Resources/Images/Docs/Demo.mo")),
            "resource .mo outside package tree must not be parsed as a library entity"
        );
    }

    #[test]
    fn parse_library_with_cache_keeps_top_level_wrapper_mo_files() {
        let temp = tempfile::tempdir().expect("tempdir");
        let lib_dir = temp.path().join("lib");
        let cache_dir = temp.path().join("cache");
        std::fs::create_dir_all(lib_dir.join("Pkg")).expect("mkdir");
        std::fs::write(
            lib_dir.join("Complex.mo"),
            "within ; operator record Complex end Complex;",
        )
        .expect("write wrapper root file");
        std::fs::write(
            lib_dir.join("Pkg/package.mo"),
            "within ; package Pkg end Pkg;",
        )
        .expect("write package");
        std::fs::write(lib_dir.join("Pkg/A.mo"), "within Pkg; model A end A;")
            .expect("write child");

        let parsed = parse_library_with_cache_in(&lib_dir, Some(&cache_dir)).expect("parse");
        assert_eq!(parsed.file_count, 3);
        assert!(
            parsed
                .documents
                .iter()
                .any(|(uri, _)| uri.ends_with("Complex.mo")),
            "top-level wrapper-root .mo files must be kept"
        );
    }

    #[test]
    fn parse_library_with_cache_preserves_package_layout_error_type() {
        let temp = tempfile::tempdir().expect("tempdir");
        let lib_dir = temp.path().join("Pkg");
        std::fs::create_dir_all(&lib_dir).expect("mkdir");
        std::fs::write(lib_dir.join("package.mo"), "package Pkg end Pkg;").expect("write package");
        std::fs::write(lib_dir.join("A.mo"), "model A end A;").expect("write child");

        let err =
            parse_library_with_cache_in(&lib_dir, None).expect_err("missing within must fail");
        let layout = err
            .downcast_ref::<PackageLayoutError>()
            .expect("package layout error type must be preserved");
        assert_eq!(layout.diagnostics()[0].code.as_deref(), Some("PKG-009"));
    }

    #[test]
    fn library_miss_reuses_unchanged_parsed_file_artifacts() {
        let temp = tempfile::tempdir().expect("tempdir");
        let lib_dir = temp.path().join("Lib");
        let cache_dir = temp.path().join("cache");
        std::fs::create_dir_all(&lib_dir).expect("mkdir");
        std::fs::write(
            lib_dir.join("package.mo"),
            "package Lib\n  model A\n    Real x;\n  end A;\nend Lib;",
        )
        .expect("write package");
        std::fs::write(
            lib_dir.join("B.mo"),
            "within Lib;\nmodel B\n  Real y;\nend B;",
        )
        .expect("write B");

        let first = parse_library_with_cache_in(&lib_dir, Some(&cache_dir)).expect("first parse");
        assert_eq!(first.cache_status, LibraryCacheStatus::Miss);

        std::fs::write(
            lib_dir.join("B.mo"),
            "within Lib;\nmodel B\n  Real y;\n  Real z;\nend B;",
        )
        .expect("update B");
        let parsed_artifact_cache_dir =
            resolve_parsed_artifact_cache_dir_from_root(Some(&cache_dir))
                .expect("parsed artifact cache dir");
        let files = [lib_dir.join("package.mo"), lib_dir.join("B.mo")];
        let statuses =
            parse_files_parallel_with_cache_statuses(&files, Some(&parsed_artifact_cache_dir))
                .expect("parse with statuses");
        assert_eq!(
            statuses
                .iter()
                .map(|(_, _, status)| *status)
                .collect::<Vec<_>>(),
            vec![
                crate::parsed_artifact_cache::ParsedArtifactCacheStatus::Hit,
                crate::parsed_artifact_cache::ParsedArtifactCacheStatus::Miss
            ],
            "the changed library file should be reparsed while unchanged files reuse cached ASTs"
        );
    }
}
