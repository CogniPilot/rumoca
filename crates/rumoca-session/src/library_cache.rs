use anyhow::{Context, Result};
use rayon::prelude::*;
use rumoca_ir_ast::StoredDefinition;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::OnceLock;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use crate::package_layout::{collect_library_source_files, validate_library_package_layout};
use crate::parse::parse_files_parallel;

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
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CachedLibraryFile {
    uri: String,
    definition: StoredDefinition,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CachedLibraryPayload {
    schema_version: u32,
    compiler_version: String,
    cache_key: String,
    source_path: String,
    files: Vec<CachedLibraryFile>,
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

fn cache_compiler_version() -> String {
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

fn hash_library_inputs(path: &Path, files: &[PathBuf]) -> std::io::Result<String> {
    let canonical_root = fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf());
    let mut dirs = Vec::new();
    if path.is_dir() {
        recursive_collect_dirs(path, &mut dirs)?;
        dirs.sort();
    }
    let mut entries: Vec<(String, u64, [u8; 32])> = files
        .par_iter()
        .map(|file| -> std::io::Result<(String, u64, [u8; 32])> {
            let canonical_file = fs::canonicalize(file).unwrap_or_else(|_| file.to_path_buf());
            let rel = canonical_file
                .strip_prefix(&canonical_root)
                .unwrap_or(&canonical_file)
                .to_string_lossy()
                .to_string();
            let bytes = fs::read(file)?;
            let digest = *blake3::hash(&bytes).as_bytes();
            Ok((rel, bytes.len() as u64, digest))
        })
        .collect::<std::io::Result<Vec<_>>>()?;
    entries.sort_by(|a, b| a.0.cmp(&b.0));

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
    for (rel, size, digest) in entries {
        hasher.update(rel.as_bytes());
        hasher.update(b"\n");
        hasher.update(&size.to_le_bytes());
        hasher.update(&digest);
    }

    Ok(hasher.finalize().to_hex().to_string())
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

fn cache_file_path(cache_dir: &Path, cache_key: &str) -> PathBuf {
    cache_dir.join(format!("{cache_key}.json"))
}

fn try_read_cache(path: &Path, cache_key: &str) -> Option<Vec<(String, StoredDefinition)>> {
    let text = fs::read_to_string(path).ok()?;
    let mut deserializer = serde_json::Deserializer::from_str(&text);
    deserializer.disable_recursion_limit();
    let payload = CachedLibraryPayload::deserialize(&mut deserializer).ok()?;
    if payload.schema_version != LIBRARY_CACHE_SCHEMA_VERSION {
        return None;
    }
    if payload.compiler_version != cache_compiler_version() {
        return None;
    }
    if payload.cache_key != cache_key {
        return None;
    }
    Some(
        payload
            .files
            .into_iter()
            .map(|file| (file.uri, file.definition))
            .collect(),
    )
}

fn write_cache(
    path: &Path,
    source_path: &Path,
    cache_key: &str,
    docs: &[(String, StoredDefinition)],
) -> Result<()> {
    let files = docs
        .iter()
        .map(|(uri, definition)| CachedLibraryFile {
            uri: uri.clone(),
            definition: definition.clone(),
        })
        .collect();

    let payload = CachedLibraryPayload {
        schema_version: LIBRARY_CACHE_SCHEMA_VERSION,
        compiler_version: cache_compiler_version(),
        cache_key: cache_key.to_string(),
        source_path: source_path.to_string_lossy().to_string(),
        files,
    };
    let text = serde_json::to_string(&payload).context("serialize library cache payload")?;
    let tmp_path = path.with_extension(format!("{}.tmp", std::process::id()));
    fs::write(&tmp_path, text).with_context(|| format!("write {}", tmp_path.display()))?;
    if let Err(rename_err) = fs::rename(&tmp_path, path) {
        fs::copy(&tmp_path, path)
            .with_context(|| format!("copy {} -> {}", tmp_path.display(), path.display()))?;
        let _ = fs::remove_file(&tmp_path);
        if !path.is_file() {
            return Err(rename_err).context("finalize library cache file");
        }
    }
    Ok(())
}

pub fn parse_library_with_cache_in(path: &Path, cache_dir: Option<&Path>) -> Result<ParsedLibrary> {
    let files = collect_modelica_files(path)
        .with_context(|| format!("collect .mo files under {}", path.display()))?;
    let cache_key = hash_library_inputs(path, &files)
        .with_context(|| format!("fingerprint {}", path.display()))?;

    if let Some(cache_dir) = cache_dir
        && fs::create_dir_all(cache_dir).is_ok()
    {
        let cache_file = cache_file_path(cache_dir, &cache_key);
        if let Some(docs) = try_read_cache(&cache_file, &cache_key) {
            validate_library_package_layout(path, &docs)?;
            return Ok(ParsedLibrary {
                documents: docs,
                file_count: files.len(),
                cache_status: LibraryCacheStatus::Hit,
                cache_key,
                cache_file: Some(cache_file),
            });
        }

        let docs = parse_files_parallel(&files)
            .with_context(|| format!("parse library files under {}", path.display()))?;
        validate_library_package_layout(path, &docs)?;
        if write_cache(&cache_file, path, &cache_key, &docs).is_ok() {
            return Ok(ParsedLibrary {
                documents: docs,
                file_count: files.len(),
                cache_status: LibraryCacheStatus::Miss,
                cache_key,
                cache_file: Some(cache_file),
            });
        }
        return Ok(ParsedLibrary {
            documents: docs,
            file_count: files.len(),
            cache_status: LibraryCacheStatus::Disabled,
            cache_key,
            cache_file: None,
        });
    }

    let docs = parse_files_parallel(&files)
        .with_context(|| format!("parse library files under {}", path.display()))?;
    validate_library_package_layout(path, &docs)?;
    Ok(ParsedLibrary {
        documents: docs,
        file_count: files.len(),
        cache_status: LibraryCacheStatus::Disabled,
        cache_key,
        cache_file: None,
    })
}

pub fn parse_library_with_cache(path: &Path) -> Result<ParsedLibrary> {
    parse_library_with_cache_in(path, resolve_library_cache_dir().as_deref())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::libraries::PackageLayoutError;

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

        let second = parse_library_with_cache_in(&lib_dir, Some(&cache_dir)).expect("second parse");
        assert_eq!(second.cache_status, LibraryCacheStatus::Hit);
        assert_eq!(second.file_count, 1);
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
}
