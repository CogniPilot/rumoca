//! Class dictionary building and lookup for flatten operations.
//!
//! This module provides utilities for building a flat dictionary of all classes
//! from a hierarchical StoredDefinition, with support for caching.
//!
//! # Architecture
//!
//! The class dictionary system uses a two-level caching strategy for performance:
//!
//! 1. **Library Dictionary Cache** (`LIBRARY_DICT_CACHE`): Stores pre-built class
//!    dictionaries for each library (e.g., Modelica Standard Library). These are
//!    built once when a library is first loaded and reused across all compiles.
//!
//! 2. **Combined Dictionary**: On each compile, library dictionaries are combined
//!    with the user's classes by cloning `Arc` references (cheap) rather than
//!    cloning the actual `ClassDefinition` structures (expensive).
//!
//! # Performance
//!
//! This architecture achieves ~6-8ms compile times for incremental changes by:
//! - Building library dictionaries once (~1 second for MSL with ~50,000 classes)
//! - Combining dictionaries in ~1ms by cloning Arc references
//! - Only rebuilding the user's portion on each edit

use crate::ir;
use std::sync::Arc;

use super::{CLASS_DICT_CACHE, ClassDict, is_cache_enabled};

// =============================================================================
// Library Class Dictionary Cache
// =============================================================================

/// Global cache for library class dictionaries, keyed by library name.
/// This allows reusing pre-built library dictionaries without rebuilding.
static LIBRARY_DICT_CACHE: std::sync::LazyLock<
    std::sync::RwLock<std::collections::HashMap<String, Arc<ClassDict>>>,
> = std::sync::LazyLock::new(|| std::sync::RwLock::new(std::collections::HashMap::new()));

/// Get or build a cached class dictionary for a library.
/// This is more efficient than rebuilding on every compile.
pub fn get_or_build_library_dict(name: &str, def: &ir::ast::StoredDefinition) -> Arc<ClassDict> {
    // Try cache first
    if let Some(dict) = LIBRARY_DICT_CACHE
        .read()
        .expect("library dict cache lock poisoned")
        .get(name)
    {
        return Arc::clone(dict);
    }

    // Build the dictionary
    let mut dict = ClassDict::new();
    for (_name, class) in &def.class_list {
        build_class_dict_internal(class, "", &mut dict);
    }

    let dict = Arc::new(dict);

    // Store in cache
    LIBRARY_DICT_CACHE
        .write()
        .expect("library dict cache lock poisoned")
        .insert(name.to_string(), Arc::clone(&dict));

    dict
}

/// Clear the library dictionary cache (for testing)
pub fn clear_library_dict_cache() {
    LIBRARY_DICT_CACHE
        .write()
        .expect("library dict cache lock poisoned")
        .clear();
}

/// Build a combined class dictionary from user source and pre-built library dictionaries.
/// This avoids cloning library class definitions by reusing Arc references.
pub fn build_combined_class_dict(
    user_def: &ir::ast::StoredDefinition,
    library_dicts: &[Arc<ClassDict>],
) -> Arc<ClassDict> {
    let mut dict = ClassDict::new();

    // Add library classes first (they get overridden by user classes if same name)
    // We clone the Arc, not the ClassDefinition - this is cheap!
    for lib_dict in library_dicts {
        for (name, class_arc) in lib_dict.iter() {
            dict.insert(name.clone(), Arc::clone(class_arc));
        }
    }

    // Add user's classes (override any library classes with same name)
    for (_name, class) in &user_def.class_list {
        build_class_dict_internal(class, "", &mut dict);
    }

    // Inject built-in types (Complex, etc.) from the builtins module
    // Only add types that aren't already defined
    if let Some(builtin_defs) = crate::ir::transform::builtins::get_builtin_definitions() {
        for (name, class) in &builtin_defs.class_list {
            if !dict.contains_key(name) {
                build_class_dict_internal(class, "", &mut dict);
            }
        }
    }

    Arc::new(dict)
}

// =============================================================================
// Class Dictionary Building
// =============================================================================

/// Recursively builds a class dictionary with full path names.
///
/// This function traverses the class hierarchy and adds all classes
/// to the dictionary with their fully qualified names (e.g., "Package.SubPackage.Model").
pub(super) fn build_class_dict_internal(
    class: &ir::ast::ClassDefinition,
    prefix: &str,
    dict: &mut ClassDict,
) {
    // Add the class itself with its full path
    let full_name = if prefix.is_empty() {
        class.name.text.clone()
    } else {
        format!("{}.{}", prefix, class.name.text)
    };
    dict.insert(full_name.clone(), Arc::new(class.clone()));

    // Recursively add nested classes
    for (_name, nested_class) in &class.classes {
        build_class_dict_internal(nested_class, &full_name, dict);
    }
}

/// Builds or retrieves a cached class dictionary for the given StoredDefinition.
pub(super) fn get_or_build_class_dict(
    def: &ir::ast::StoredDefinition,
    def_hash: u64,
) -> Arc<ClassDict> {
    // Try to get from cache first (read lock) - only if caching is enabled
    if is_cache_enabled()
        && let Some(dict) = CLASS_DICT_CACHE
            .read()
            .expect("class dict cache lock poisoned")
            .get(&def_hash)
    {
        return Arc::clone(dict);
    }

    // Build the dictionary from user-provided definitions
    let mut dict = ClassDict::new();
    for (_name, class) in &def.class_list {
        build_class_dict_internal(class, "", &mut dict);
    }

    // Inject built-in types (Complex, etc.) from the builtins module
    // Only add types that aren't already defined by the user
    if let Some(builtin_defs) = crate::ir::transform::builtins::get_builtin_definitions() {
        for (name, class) in &builtin_defs.class_list {
            if !dict.contains_key(name) {
                build_class_dict_internal(class, "", &mut dict);
            }
        }
    }

    let dict = Arc::new(dict);

    // Store in cache (only if caching is enabled)
    if is_cache_enabled() {
        CLASS_DICT_CACHE
            .write()
            .expect("class dict cache lock poisoned")
            .insert(def_hash, Arc::clone(&dict));
    }

    dict
}

// =============================================================================
// Class Lookup
// =============================================================================

/// Looks up a class by path in the stored definition.
///
/// Supports both simple names (e.g., "Model") and dotted paths (e.g., "Package.Model").
pub(super) fn lookup_class(
    def: &ir::ast::StoredDefinition,
    class_dict: &ClassDict,
    path: &str,
) -> Option<Arc<ir::ast::ClassDefinition>> {
    // First try a direct lookup in the class dictionary
    if let Some(class) = class_dict.get(path) {
        return Some(Arc::clone(class));
    }

    // Fallback: navigate nested class path manually
    let parts: Vec<&str> = path.split('.').collect();
    if parts.is_empty() {
        return None;
    }

    // Start from the root class
    let root = def.class_list.get(parts[0])?;
    if parts.len() == 1 {
        return Some(Arc::new(root.clone()));
    }

    // Navigate through nested classes
    let mut current = root;
    for part in &parts[1..] {
        current = current.classes.get(*part)?;
    }
    Some(Arc::new(current.clone()))
}
