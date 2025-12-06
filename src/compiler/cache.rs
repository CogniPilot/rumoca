//! AST caching for faster library loading.
//!
//! This module provides disk-based caching of parsed Modelica ASTs to avoid
//! re-parsing unchanged library files on subsequent compilations.
//!
//! The cache stores serialized `StoredDefinition` structs in `~/.cache/rumoca/ast/`,
//! with filenames based on the MD5 hash of the source file content.
//!
//! ## Caching behavior
//!
//! Caching is always enabled. Each build gets its own cache based on:
//! - Rumoca version
//! - Git version (includes build timestamp for dirty builds)
//!
//! ## Cache invalidation
//!
//! The entire cache is automatically cleared when:
//! - The rumoca version changes (new release)
//! - The git version changes (includes timestamp for dirty builds)
//! - The CACHE_VERSION constant is incremented (AST structure changes)
//!
//! Individual cache entries are invalidated when:
//! - The source file content changes (different MD5 hash)

use crate::ir::ast::StoredDefinition;
use anyhow::{Context, Result};
use std::fs;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

/// Cache format version - increment when cache file format or AST structure changes
const CACHE_VERSION: u32 = 1;

/// Rumoca version at compile time - used for automatic cache invalidation
const RUMOCA_VERSION: &str = env!("CARGO_PKG_VERSION");

/// Git version at compile time - includes commit hash and build timestamp for dirty builds
/// Format: "v0.7.18" (clean release), "v0.7.18-dirty-1701853200" (dirty with timestamp)
///         "v0.7.18-5-g1234567" (commits after tag), "v0.7.18-5-g1234567-dirty-1701853200" (both)
const GIT_VERSION: &str = env!("RUMOCA_GIT_VERSION");

/// Header stored at the beginning of each cache file for validation
#[derive(serde::Serialize, serde::Deserialize)]
struct CacheHeader {
    /// Cache format version
    version: u32,
    /// Rumoca compiler version that created this cache
    rumoca_version: String,
    /// Git version (commit hash + build timestamp for dirty) that created this cache
    git_version: String,
    /// MD5 hash of the source file content
    source_hash: String,
}

/// Get the cache directory path (~/.cache/rumoca/ast/)
pub fn get_cache_dir() -> Option<PathBuf> {
    dirs::cache_dir().map(|d| d.join("rumoca").join("ast"))
}

/// Check and update the cache version marker.
/// If the version has changed, clear the entire cache to avoid accumulation.
fn check_and_update_version_marker(cache_dir: &Path) -> bool {
    let version_file = cache_dir.join(".version");
    // Include cache version, rumoca version, and git version (with timestamp for dirty builds)
    let current_version = format!("{}:{}:{}", CACHE_VERSION, RUMOCA_VERSION, GIT_VERSION);

    // Check if version matches
    if version_file.exists() {
        if let Ok(stored_version) = fs::read_to_string(&version_file) {
            if stored_version.trim() == current_version {
                return true; // Cache is valid
            }
        }
    }

    // Version mismatch or missing - clear the entire cache
    if cache_dir.exists() {
        let _ = fs::remove_dir_all(cache_dir);
    }

    // Create fresh cache directory and version marker
    if fs::create_dir_all(cache_dir).is_ok() {
        let _ = fs::write(&version_file, &current_version);
    }

    false // Cache was cleared
}

/// Compute MD5 hash of file contents
pub fn compute_file_hash(path: &Path) -> Result<String> {
    let content = fs::read(path).with_context(|| format!("Failed to read file: {:?}", path))?;
    Ok(format!("{:x}", chksum_md5::hash(&content)))
}

/// Get the cache file path for a given source file hash
fn get_cache_path(cache_dir: &Path, source_hash: &str) -> PathBuf {
    cache_dir.join(format!("{}.ast", source_hash))
}

/// Try to load a cached AST for the given source file.
///
/// Returns `Some(StoredDefinition)` if a valid cache exists, `None` otherwise.
/// Automatically clears the entire cache if the compiler version or build timestamp has changed.
pub fn load_cached_ast(_path: &Path, source_hash: &str) -> Option<StoredDefinition> {
    let cache_dir = get_cache_dir()?;

    // Check version marker - clears cache if version changed
    if !check_and_update_version_marker(&cache_dir) {
        return None; // Cache was just cleared
    }

    let cache_path = get_cache_path(&cache_dir, source_hash);

    if !cache_path.exists() {
        return None;
    }

    // Read and validate cache file
    let mut file = fs::File::open(&cache_path).ok()?;
    let mut data = Vec::new();
    file.read_to_end(&mut data).ok()?;

    // Deserialize header first
    let header_size: usize = bincode::deserialize(&data[..8]).ok()?;
    if data.len() < 8 + header_size {
        // Cache file is corrupted
        let _ = fs::remove_file(&cache_path);
        return None;
    }

    let header: CacheHeader = bincode::deserialize(&data[8..8 + header_size]).ok()?;

    // Validate cache - check version, rumoca version, git version, and source hash
    if header.version != CACHE_VERSION
        || header.rumoca_version != RUMOCA_VERSION
        || header.git_version != GIT_VERSION
        || header.source_hash != source_hash
    {
        // Cache is stale, from different compiler version/build, or different source
        let _ = fs::remove_file(&cache_path);
        return None;
    }

    // Deserialize the AST
    let ast: StoredDefinition = bincode::deserialize(&data[8 + header_size..]).ok()?;

    Some(ast)
}

/// Store a parsed AST in the cache.
pub fn store_cached_ast(_path: &Path, source_hash: &str, ast: &StoredDefinition) -> Result<()> {
    let cache_dir = match get_cache_dir() {
        Some(d) => d,
        None => return Ok(()), // No cache directory available, skip caching
    };

    // Ensure version marker is up-to-date (clears old cache if needed)
    check_and_update_version_marker(&cache_dir);

    // Create cache directory if it doesn't exist
    fs::create_dir_all(&cache_dir)
        .with_context(|| format!("Failed to create cache directory: {:?}", cache_dir))?;

    let cache_path = get_cache_path(&cache_dir, source_hash);

    // Create header with version info for automatic invalidation
    let header = CacheHeader {
        version: CACHE_VERSION,
        rumoca_version: RUMOCA_VERSION.to_string(),
        git_version: GIT_VERSION.to_string(),
        source_hash: source_hash.to_string(),
    };

    // Serialize header and AST
    let header_bytes =
        bincode::serialize(&header).with_context(|| "Failed to serialize cache header")?;
    let ast_bytes = bincode::serialize(ast).with_context(|| "Failed to serialize AST")?;

    // Write to cache file: [header_size: u64][header][ast]
    let header_size = header_bytes.len() as u64;
    let mut file = fs::File::create(&cache_path)
        .with_context(|| format!("Failed to create cache file: {:?}", cache_path))?;

    file.write_all(&bincode::serialize(&header_size)?)?;
    file.write_all(&header_bytes)?;
    file.write_all(&ast_bytes)?;

    Ok(())
}

/// Clear the entire AST cache.
pub fn clear_cache() -> Result<()> {
    if let Some(cache_dir) = get_cache_dir() {
        if cache_dir.exists() {
            fs::remove_dir_all(&cache_dir)
                .with_context(|| format!("Failed to clear cache directory: {:?}", cache_dir))?;
        }
    }
    Ok(())
}

/// Get cache statistics (number of files, total size).
pub fn get_cache_stats() -> Option<(usize, u64)> {
    let cache_dir = get_cache_dir()?;
    if !cache_dir.exists() {
        return Some((0, 0));
    }

    let mut count = 0;
    let mut size = 0u64;

    for entry in (fs::read_dir(&cache_dir).ok()?).flatten() {
        if let Ok(metadata) = entry.metadata() {
            if metadata.is_file() {
                count += 1;
                size += metadata.len();
            }
        }
    }

    Some((count, size))
}
