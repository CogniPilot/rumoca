//! File hashing and dependency tracking for cache invalidation.
//!
//! This module provides utilities for tracking file dependencies during
//! flattening and computing content-based hashes for cache keys.

use crate::ir;
use crate::ir::transform::constants::is_primitive_type;
use indexmap::{IndexMap, IndexSet};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::{LazyLock, RwLock};

use super::ClassDict;

// =============================================================================
// File Hash Cache
// =============================================================================

/// Global cache for file hashes (shared across threads)
pub(super) static FILE_HASH_CACHE: LazyLock<RwLock<HashMap<String, String>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Get file hash from global cache, computing if not present
pub(super) fn get_cached_file_hash(file_path: &str) -> String {
    // Fast path: check read lock
    {
        let cache = FILE_HASH_CACHE
            .read()
            .expect("file hash cache lock poisoned");
        if let Some(h) = cache.get(file_path) {
            return h.clone();
        }
    }

    // Slow path: compute hash and insert with write lock
    let path = std::path::Path::new(file_path);
    let hash = if let Ok(content) = std::fs::read(path) {
        format!("{:x}", chksum_md5::hash(&content))
    } else {
        "invalid".to_string() // File doesn't exist or can't be read
    };

    FILE_HASH_CACHE
        .write()
        .expect("file hash cache lock poisoned")
        .insert(file_path.to_string(), hash.clone());
    hash
}

// =============================================================================
// Dependency Tracking
// =============================================================================

/// Tracks file dependencies during flattening for cache invalidation.
/// Each dependency is a (file_path, file_hash) pair.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct FileDependencies {
    /// Map of file_path -> MD5 hash of file content
    pub files: IndexMap<String, String>,
}

impl FileDependencies {
    pub fn new() -> Self {
        Self {
            files: IndexMap::new(),
        }
    }

    /// Record a file dependency if we haven't seen it before
    pub fn record(&mut self, file_path: &str, file_hash: &str) {
        if !file_path.is_empty() && !self.files.contains_key(file_path) {
            self.files
                .insert(file_path.to_string(), file_hash.to_string());
        }
    }

    /// Check if all dependencies are still valid (files exist and hashes match)
    /// Uses thread-local cache to avoid re-hashing files
    pub fn is_valid(&self) -> bool {
        for (file_path, expected_hash) in &self.files {
            let current_hash = get_cached_file_hash(file_path);
            if current_hash != *expected_hash {
                return false;
            }
        }
        true
    }
}

/// Record a file dependency, using cached hash if available
pub(super) fn record_file_dep(deps: &mut FileDependencies, file_name: &str) {
    if file_name.is_empty() || file_name == "<test>" {
        return;
    }

    // Check if already recorded
    if deps.files.contains_key(file_name) {
        return;
    }

    // Get or compute hash using thread-local cache
    let hash = get_cached_file_hash(file_name);

    deps.record(file_name, &hash);
}

/// Result of flattening with dependency information
#[derive(Debug, Clone)]
pub struct FlattenResult {
    /// The flattened class definition
    pub class: ir::ast::ClassDefinition,
    /// File dependencies used during flattening
    pub dependencies: FileDependencies,
}

// =============================================================================
// Content Hashing
// =============================================================================

/// Compute a content-based hash for a StoredDefinition.
/// This hashes the actual content including component names, type names, and equation structure.
pub(super) fn compute_def_hash(def: &ir::ast::StoredDefinition) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    let mut hasher = DefaultHasher::new();

    // Hash class names and their full content
    for (name, class) in &def.class_list {
        hash_class_content(name, class, &mut hasher);
    }

    hasher.finish()
}

/// Hash the content of a class definition for cache key computation
fn hash_class_content(name: &str, class: &ir::ast::ClassDefinition, hasher: &mut impl Hasher) {
    // Hash class name and type
    name.hash(hasher);
    std::mem::discriminant(&class.class_type).hash(hasher);

    // Hash all component names and their type names
    for (comp_name, comp) in &class.components {
        comp_name.hash(hasher);
        comp.type_name.to_string().hash(hasher);
        // Hash variability and causality as they affect flattening
        std::mem::discriminant(&comp.variability).hash(hasher);
        std::mem::discriminant(&comp.causality).hash(hasher);
        // Hash array shape
        comp.shape.hash(hasher);
    }

    // Hash extends clauses (parent class names)
    for ext in &class.extends {
        ext.comp.to_string().hash(hasher);
    }

    // Hash equation count and a representation of equations
    class.equations.len().hash(hasher);
    for eq in &class.equations {
        // Hash equation structure using debug representation
        format!("{:?}", eq).hash(hasher);
    }

    // Hash algorithm statement count
    class.algorithms.len().hash(hasher);
    for stmt in &class.algorithms {
        format!("{:?}", stmt).hash(hasher);
    }

    // Recursively hash nested classes
    for (nested_name, nested_class) in &class.classes {
        hash_class_content(nested_name, nested_class, hasher);
    }
}

// =============================================================================
// Dependency Graph
// =============================================================================

/// Build inheritance dependency graph from class dictionary.
/// Returns a map of class_name -> Vec<parent_class_names>
pub(super) fn build_dependency_graph(class_dict: &ClassDict) -> HashMap<String, Vec<String>> {
    let mut deps: HashMap<String, Vec<String>> = HashMap::new();

    for (class_name, class_def) in class_dict.iter() {
        let mut parents = Vec::new();
        for extend in &class_def.extends {
            let parent_name = extend.comp.to_string();
            // Skip primitive types
            if !is_primitive_type(&parent_name) {
                // Try to resolve the parent name
                if class_dict.contains_key(&parent_name) {
                    parents.push(parent_name);
                } else {
                    // Try to find it with class context (simplified resolution)
                    // Check if it's a sibling or nested class
                    let parts: Vec<&str> = class_name.split('.').collect();
                    for i in (0..parts.len()).rev() {
                        let prefix = parts[..i].join(".");
                        let candidate = if prefix.is_empty() {
                            parent_name.clone()
                        } else {
                            format!("{}.{}", prefix, parent_name)
                        };
                        if class_dict.contains_key(&candidate) {
                            parents.push(candidate);
                            break;
                        }
                    }
                }
            }
        }
        deps.insert(class_name.clone(), parents);
    }

    deps
}

/// Compute dependency levels for wavefront parallelism.
/// Returns classes grouped by level (level 0 = no dependencies, level 1 = depends only on level 0, etc.)
pub(super) fn compute_dependency_levels(
    class_dict: &ClassDict,
    deps: &HashMap<String, Vec<String>>,
) -> Vec<Vec<String>> {
    let mut levels: Vec<Vec<String>> = Vec::new();
    let mut assigned: HashMap<String, usize> = HashMap::new();

    // Keep iterating until all classes are assigned a level
    let mut remaining: IndexSet<String> = class_dict.keys().cloned().collect();

    while !remaining.is_empty() {
        let mut current_level = Vec::new();

        for class_name in remaining.iter() {
            let parents = deps.get(class_name).map(|v| v.as_slice()).unwrap_or(&[]);

            // Check if all parents are already assigned
            let all_parents_assigned = parents.iter().all(|p| {
                // Parent is assigned OR parent is not in our class_dict (external dependency)
                assigned.contains_key(p) || !class_dict.contains_key(p)
            });

            if all_parents_assigned {
                current_level.push(class_name.clone());
            }
        }

        // If no progress, break to avoid infinite loop (circular deps)
        if current_level.is_empty() {
            // Add remaining classes to final level
            current_level = remaining.iter().cloned().collect();
        }

        // Assign level to current batch
        let level_num = levels.len();
        for class_name in &current_level {
            assigned.insert(class_name.clone(), level_num);
            remaining.swap_remove(class_name);
        }

        levels.push(current_level);
    }

    levels
}
