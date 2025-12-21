//! Class dictionary building and lookup for flatten operations.
//!
//! This module provides utilities for building a flat dictionary of all classes
//! from a hierarchical StoredDefinition, with support for caching.

use crate::ir;
use std::sync::Arc;

use super::{CLASS_DICT_CACHE, ClassDict, is_cache_enabled};

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
