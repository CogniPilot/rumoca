//! AST caching for faster library loading.
//!
//! This module provides disk-based caching of parsed Modelica ASTs to avoid
//! re-parsing unchanged library files on subsequent compilations.
//!
//! The cache stores serialized `StoredDefinition` structs in `~/.cache/rumoca/ast/`,
//! with filenames based on the MD5 hash of the source file content.
//!
//! When the `cache` feature is disabled (e.g., for WASM builds), all functions
//! become no-ops that return appropriate default values.

use crate::ir::ast::StoredDefinition;
use anyhow::Result;
use std::path::Path;

// =============================================================================
// Full disk cache implementation (requires cache feature)
// =============================================================================

#[cfg(feature = "cache")]
mod disk {
    use super::*;
    use anyhow::Context;
    use std::fs;
    use std::io::{Read, Write};
    use std::path::PathBuf;

    /// Cache format version - increment when cache file format or AST structure changes
    const CACHE_VERSION: u32 = 1;

    /// Rumoca version at compile time - used for automatic cache invalidation
    const RUMOCA_VERSION: &str = env!("CARGO_PKG_VERSION");

    /// Git version at compile time - includes commit hash and build timestamp for dirty builds
    const GIT_VERSION: &str = env!("RUMOCA_GIT_VERSION");

    /// Header stored at the beginning of each cache file for validation
    #[derive(serde::Serialize, serde::Deserialize)]
    struct CacheHeader {
        version: u32,
        rumoca_version: String,
        git_version: String,
        source_hash: String,
    }

    /// Get the cache directory path (~/.cache/rumoca/ast/)
    pub fn get_cache_dir() -> Option<PathBuf> {
        dirs::cache_dir().map(|d| d.join("rumoca").join("ast"))
    }

    /// Check and update the cache version marker.
    fn check_and_update_version_marker(cache_dir: &Path) -> bool {
        let version_file = cache_dir.join(".version");
        let current_version = format!("{}:{}:{}", CACHE_VERSION, RUMOCA_VERSION, GIT_VERSION);

        if version_file.exists()
            && let Ok(stored_version) = fs::read_to_string(&version_file)
            && stored_version.trim() == current_version
        {
            return true;
        }

        if cache_dir.exists() {
            let _ = fs::remove_dir_all(cache_dir);
        }

        if fs::create_dir_all(cache_dir).is_ok() {
            let _ = fs::write(&version_file, &current_version);
        }

        false
    }

    /// Compute MD5 hash of file contents
    pub fn compute_file_hash(path: &Path) -> Result<String> {
        let content = fs::read(path).with_context(|| format!("Failed to read file: {:?}", path))?;
        Ok(format!("{:x}", chksum_md5::hash(&content)))
    }

    fn get_cache_path(cache_dir: &Path, source_hash: &str) -> PathBuf {
        cache_dir.join(format!("{}.ast", source_hash))
    }

    /// Try to load a cached AST for the given source file.
    pub fn load_cached_ast(_path: &Path, source_hash: &str) -> Option<StoredDefinition> {
        let cache_dir = get_cache_dir()?;

        if !check_and_update_version_marker(&cache_dir) {
            return None;
        }

        let cache_path = get_cache_path(&cache_dir, source_hash);

        if !cache_path.exists() {
            return None;
        }

        let mut file = fs::File::open(&cache_path).ok()?;
        let mut data = Vec::new();
        file.read_to_end(&mut data).ok()?;

        let header_size: usize = bincode::deserialize(&data[..8]).ok()?;
        if data.len() < 8 + header_size {
            let _ = fs::remove_file(&cache_path);
            return None;
        }

        let header: CacheHeader = bincode::deserialize(&data[8..8 + header_size]).ok()?;

        if header.version != CACHE_VERSION
            || header.rumoca_version != RUMOCA_VERSION
            || header.git_version != GIT_VERSION
            || header.source_hash != source_hash
        {
            let _ = fs::remove_file(&cache_path);
            return None;
        }

        bincode::deserialize(&data[8 + header_size..]).ok()
    }

    /// Store a parsed AST in the cache.
    pub fn store_cached_ast(_path: &Path, source_hash: &str, ast: &StoredDefinition) -> Result<()> {
        let cache_dir = match get_cache_dir() {
            Some(d) => d,
            None => return Ok(()),
        };

        check_and_update_version_marker(&cache_dir);

        fs::create_dir_all(&cache_dir)
            .with_context(|| format!("Failed to create cache directory: {:?}", cache_dir))?;

        let cache_path = get_cache_path(&cache_dir, source_hash);

        let header = CacheHeader {
            version: CACHE_VERSION,
            rumoca_version: RUMOCA_VERSION.to_string(),
            git_version: GIT_VERSION.to_string(),
            source_hash: source_hash.to_string(),
        };

        let header_bytes =
            bincode::serialize(&header).with_context(|| "Failed to serialize cache header")?;
        let ast_bytes = bincode::serialize(ast).with_context(|| "Failed to serialize AST")?;

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
        if let Some(cache_dir) = get_cache_dir()
            && cache_dir.exists()
        {
            fs::remove_dir_all(&cache_dir)
                .with_context(|| format!("Failed to clear cache directory: {:?}", cache_dir))?;
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
            if let Ok(metadata) = entry.metadata()
                && metadata.is_file()
            {
                count += 1;
                size += metadata.len();
            }
        }

        Some((count, size))
    }
}

// =============================================================================
// Public API - delegates to disk module or provides no-op stubs
// =============================================================================

/// Compute MD5 hash of file contents
#[cfg(feature = "cache")]
pub fn compute_file_hash(path: &Path) -> Result<String> {
    disk::compute_file_hash(path)
}

#[cfg(not(feature = "cache"))]
pub fn compute_file_hash(_path: &Path) -> Result<String> {
    // Without cache feature, just return empty hash (caching disabled)
    Ok(String::new())
}

/// Try to load a cached AST for the given source file.
#[cfg(feature = "cache")]
pub fn load_cached_ast(path: &Path, source_hash: &str) -> Option<StoredDefinition> {
    disk::load_cached_ast(path, source_hash)
}

#[cfg(not(feature = "cache"))]
pub fn load_cached_ast(_path: &Path, _source_hash: &str) -> Option<StoredDefinition> {
    None // Caching disabled
}

/// Store a parsed AST in the cache.
#[cfg(feature = "cache")]
pub fn store_cached_ast(path: &Path, source_hash: &str, ast: &StoredDefinition) -> Result<()> {
    disk::store_cached_ast(path, source_hash, ast)
}

#[cfg(not(feature = "cache"))]
pub fn store_cached_ast(_path: &Path, _source_hash: &str, _ast: &StoredDefinition) -> Result<()> {
    Ok(()) // No-op when caching disabled
}

/// Clear the entire AST cache.
#[cfg(feature = "cache")]
pub fn clear_cache() -> Result<()> {
    disk::clear_cache()
}

#[cfg(not(feature = "cache"))]
pub fn clear_cache() -> Result<()> {
    Ok(()) // No-op when caching disabled
}

/// Get cache statistics (number of files, total size).
#[cfg(feature = "cache")]
pub fn get_cache_stats() -> Option<(usize, u64)> {
    disk::get_cache_stats()
}

#[cfg(not(feature = "cache"))]
pub fn get_cache_stats() -> Option<(usize, u64)> {
    Some((0, 0)) // No cache when disabled
}

/// Get the cache directory path (~/.cache/rumoca/ast/)
#[cfg(feature = "cache")]
pub fn get_cache_dir() -> Option<std::path::PathBuf> {
    disk::get_cache_dir()
}

#[cfg(not(feature = "cache"))]
pub fn get_cache_dir() -> Option<std::path::PathBuf> {
    None // No cache directory when disabled
}
