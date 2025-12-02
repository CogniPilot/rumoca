//! Function collection utilities for the compiler.
//!
//! This module provides utilities for collecting function definitions
//! from a Modelica AST, including nested functions in packages.

use crate::ir::ast::{ClassDefinition, ClassType, StoredDefinition};
use indexmap::IndexMap;

/// Recursively collects all function names from a class and its nested classes.
///
/// Returns a tuple of (full_path, ClassDefinition) for each function found.
pub fn collect_functions_from_class(
    class: &ClassDefinition,
    prefix: &str,
    functions: &mut IndexMap<String, ClassDefinition>,
) {
    // Build the full path for this class
    let full_name = if prefix.is_empty() {
        class.name.text.clone()
    } else {
        format!("{}.{}", prefix, class.name.text)
    };

    // If this is a function, add it with full path
    if matches!(class.class_type, ClassType::Function) {
        functions.insert(full_name.clone(), class.clone());
        // Also add short name for calls within the same package
        functions.insert(class.name.text.clone(), class.clone());
    }

    // Also register package names so they're valid in function call paths
    if matches!(class.class_type, ClassType::Package) {
        functions.insert(full_name.clone(), class.clone());
        // Also add the short name
        functions.insert(class.name.text.clone(), class.clone());
    }

    // Recursively process nested classes
    for (_name, nested_class) in &class.classes {
        collect_functions_from_class(nested_class, &full_name, functions);
    }

    // For packages, also add relative paths for their children
    // This allows Package.function to be called from sibling classes
    if matches!(class.class_type, ClassType::Package) {
        collect_functions_with_relative_paths(class, &class.name.text, functions);
    }
}

/// Collect functions with relative paths from a given package root.
/// This allows functions to be called with package-relative names.
pub fn collect_functions_with_relative_paths(
    class: &ClassDefinition,
    relative_prefix: &str,
    functions: &mut IndexMap<String, ClassDefinition>,
) {
    for (_name, nested_class) in &class.classes {
        let relative_name = format!("{}.{}", relative_prefix, nested_class.name.text);

        if matches!(nested_class.class_type, ClassType::Function) {
            functions.insert(relative_name.clone(), nested_class.clone());
        }

        // Recursively process nested packages
        if matches!(nested_class.class_type, ClassType::Package) {
            collect_functions_with_relative_paths(nested_class, &relative_name, functions);
        }
    }
}

/// Collects all function definitions from a stored definition.
///
/// Returns a vector of function names (with their full paths for nested functions)
/// and an IndexMap mapping function paths to their definitions.
pub fn collect_all_functions(def: &StoredDefinition) -> Vec<String> {
    let mut functions = IndexMap::new();
    for (_class_name, class) in &def.class_list {
        collect_functions_from_class(class, "", &mut functions);
    }
    functions.keys().cloned().collect()
}
