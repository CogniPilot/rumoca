//! Cache control for flatten operations.
//!
//! Provides functions to enable/disable in-memory caching during flattening.
//! Caching is opt-in and should be enabled when parsing large libraries (like MSL).

use super::{CLASS_DICT_CACHE, EXTENDS_CHAIN_CACHE, FILE_HASH_CACHE, RESOLVED_CLASS_CACHE};
use std::sync::RwLock;

/// Global flag to enable/disable in-memory caching.
/// Default is OFF (false) for predictable behavior during interactive editing.
/// Enable when parsing large libraries (like MSL), then disable after.
pub(super) static CACHE_ENABLED: RwLock<bool> = RwLock::new(false);

/// Enable in-memory caching for flatten operations.
/// Call this before parsing large libraries to speed up resolution.
pub fn enable_cache() {
    *CACHE_ENABLED.write().expect("cache enabled lock poisoned") = true;
}

/// Disable in-memory caching for flatten operations.
/// Call this after loading libraries, before compiling user code.
pub fn disable_cache() {
    *CACHE_ENABLED.write().expect("cache enabled lock poisoned") = false;
    clear_caches();
}

/// Check if caching is currently enabled
pub fn is_cache_enabled() -> bool {
    *CACHE_ENABLED.read().expect("cache enabled lock poisoned")
}

/// Clear all caches (in-memory only, use clear_all_caches for disk too)
pub fn clear_caches() {
    CLASS_DICT_CACHE
        .write()
        .expect("class dict cache lock poisoned")
        .clear();
    RESOLVED_CLASS_CACHE
        .write()
        .expect("resolved class cache lock poisoned")
        .clear();
    FILE_HASH_CACHE
        .write()
        .expect("file hash cache lock poisoned")
        .clear();
    EXTENDS_CHAIN_CACHE
        .write()
        .expect("extends chain cache lock poisoned")
        .clear();
}

/// Clear all caches (alias for clear_caches)
pub fn clear_all_caches() {
    clear_caches();
}

/// Get cache statistics for diagnostics
/// Returns (class_dict_entries, resolved_entries)
pub fn get_cache_stats() -> (usize, usize) {
    let class_dict_size = CLASS_DICT_CACHE
        .read()
        .expect("class dict cache lock poisoned")
        .len();
    let resolved_size = RESOLVED_CLASS_CACHE
        .read()
        .expect("resolved class cache lock poisoned")
        .len();
    (class_dict_size, resolved_size)
}
