//! Function collection utilities for the compiler.
//!
//! This module provides utilities for collecting function definitions
//! from a Modelica AST, including nested functions in packages.

use crate::ir::ast::{ClassDefinition, ClassType, StoredDefinition};
use indexmap::IndexSet;

/// Recursively collects all function names from a class and its nested classes.
fn collect_function_names_from_class(
    class: &ClassDefinition,
    prefix: &str,
    names: &mut IndexSet<String>,
) {
    // Build the full path for this class
    let full_name = if prefix.is_empty() {
        class.name.text.clone()
    } else {
        format!("{}.{}", prefix, class.name.text)
    };

    // If this is a function, add it with full path
    if matches!(class.class_type, ClassType::Function) {
        names.insert(full_name.clone());
        // Also add short name for calls within the same package
        names.insert(class.name.text.clone());
    }

    // Also register package names so they're valid in function call paths
    if matches!(class.class_type, ClassType::Package) {
        names.insert(full_name.clone());
        // Also add the short name
        names.insert(class.name.text.clone());
    }

    // Recursively process nested classes
    for (_name, nested_class) in &class.classes {
        collect_function_names_from_class(nested_class, &full_name, names);
    }

    // For packages, also add relative paths for their children
    // This allows Package.function to be called from sibling classes
    if matches!(class.class_type, ClassType::Package) {
        collect_function_names_with_relative_paths(class, &class.name.text, names);
    }
}

/// Collect function names with relative paths from a given package root.
/// This allows functions to be called with package-relative names.
fn collect_function_names_with_relative_paths(
    class: &ClassDefinition,
    relative_prefix: &str,
    names: &mut IndexSet<String>,
) {
    for (_name, nested_class) in &class.classes {
        let relative_name = format!("{}.{}", relative_prefix, nested_class.name.text);

        if matches!(nested_class.class_type, ClassType::Function) {
            names.insert(relative_name.clone());
        }

        // Recursively process nested packages
        if matches!(nested_class.class_type, ClassType::Package) {
            collect_function_names_with_relative_paths(nested_class, &relative_name, names);
        }
    }
}

/// Built-in Modelica function names that should be recognized without explicit definition.
/// These include mathematical functions, array operations, event operators, and clocked operators.
pub const BUILTIN_FUNCTIONS: &[&str] = &[
    // Mathematical functions (MLS §3.7.1.2)
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "atan2",
    "sinh",
    "cosh",
    "tanh",
    "asinh",
    "acosh",
    "atanh",
    "exp",
    "log",
    "log10",
    "sqrt",
    "abs",
    "sign",
    "min",
    "max",
    "sum",
    "product",
    "floor",
    "ceil",
    "integer",
    "mod",
    "rem",
    "div",
    // Array functions (MLS §10.3)
    "size",
    "ndims",
    "scalar",
    "vector",
    "matrix",
    "zeros",
    "ones",
    "fill",
    "identity",
    "diagonal",
    "linspace",
    "transpose",
    "symmetric",
    "cross",
    "skew",
    "outerProduct",
    "cat",
    "array",
    // Event and state functions (MLS §3.7.5)
    "der",
    "delay",
    "cardinality",
    "homotopy",
    "semiLinear",
    "inStream",
    "actualStream",
    "pre",
    "edge",
    "change",
    "reinit",
    "initial",
    "terminal",
    "smooth",
    "noEvent",
    "sample",
    "spatialDistribution",
    "getInstanceName",
    // Clocked operators (MLS §16.2, §16.3)
    "previous",
    "interval",
    "hold",
    "shiftSample",
    "backSample",
    "noClock",
    "firstTick",
    "subSample",
    "superSample",
    "Clock",
    // Type conversion
    "String",
    "Integer",
    "Real",
    "Boolean",
    // Assertion and output
    "assert",
    "print",
    // Special
    "Modelica",
];

/// Collects all function names from a stored definition.
///
/// Returns a vector of function names (with their full paths for nested functions).
/// This function is optimized to avoid cloning ClassDefinition objects.
/// Also includes all built-in Modelica function names.
pub fn collect_all_functions(def: &StoredDefinition) -> Vec<String> {
    let mut names = IndexSet::new();

    // Add all built-in function names first
    for &name in BUILTIN_FUNCTIONS {
        names.insert(name.to_string());
    }

    // Then add user-defined functions
    for (_class_name, class) in &def.class_list {
        collect_function_names_from_class(class, "", &mut names);
    }
    names.into_iter().collect()
}
