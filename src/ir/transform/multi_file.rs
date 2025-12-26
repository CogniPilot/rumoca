//! Multi-file compilation support
//!
//! This module provides utilities for compiling multiple Modelica files together,
//! merging their class definitions into a single StoredDefinition.
//!
//! # Features
//!
//! - **Directory-based packages (Spec 13.4)**: Support for `package.mo` and `package.order` files
//! - **MODELICAPATH support (Spec 13.3)**: Environment variable for library root directories
//! - **Auto-discovery**: Automatically discover package structure from directories

use crate::ir::ast::{ClassDefinition, StoredDefinition};
use anyhow::{Context, Result};
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// Directories to skip when discovering Modelica files.
/// These are common directories that should never contain Modelica code.
const IGNORED_DIRECTORIES: &[&str] = &[
    // Version control
    ".git",
    ".hg",
    ".svn",
    // Build artifacts
    "target",
    "build",
    "out",
    "dist",
    "_build",
    "cmake-build-debug",
    "cmake-build-release",
    // Dependencies
    "node_modules",
    ".npm",
    "vendor",
    // Virtual environments
    "venv",
    ".venv",
    "env",
    ".env",
    "__pycache__",
    ".tox",
    // IDE/Editor
    ".idea",
    ".vscode",
    ".vs",
    // Rust
    ".cargo",
    // Python
    ".eggs",
    ".mypy_cache",
    ".pytest_cache",
    // Other
    ".cache",
    ".tmp",
    "tmp",
    "temp",
    ".DS_Store",
];

/// Check if a directory should be ignored during file discovery.
///
/// This checks against a list of common directories that should never contain
/// Modelica code (build artifacts, version control, IDE files, etc.) and also
/// ignores hidden directories (starting with `.`).
pub fn should_ignore_directory(path: &Path) -> bool {
    if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
        // Check against the ignore list
        if IGNORED_DIRECTORIES.contains(&name) {
            return true;
        }
        // Also ignore hidden directories (starting with .)
        if name.starts_with('.') && name != "." && name != ".." {
            return true;
        }
    }
    false
}

/// Merge multiple StoredDefinitions into a single one.
///
/// This function combines class definitions from multiple files, using the `within`
/// clause to place classes in their correct package hierarchy.
///
/// For example, if a file has:
/// ```modelica
/// within MyPackage;
/// model MyModel ... end MyModel;
/// ```
///
/// The model will be placed at `MyPackage.MyModel` in the merged definition.
///
/// # Arguments
///
/// * `definitions` - A list of (file_path, StoredDefinition) tuples
///
/// # Returns
///
/// A merged StoredDefinition containing all classes from all files
pub fn merge_stored_definitions(
    definitions: Vec<(String, StoredDefinition)>,
) -> Result<StoredDefinition> {
    let mut merged = StoredDefinition::default();

    for (file_path, def) in definitions {
        merge_single_definition(&mut merged, def, &file_path)?;
    }

    Ok(merged)
}

/// Merge Arc-wrapped library definitions with a user's source definition.
///
/// This is optimized for the LSP use case where we have pre-merged library
/// ASTs stored in Arc and want to avoid cloning them on every compile.
/// We clone the library class_list entries into the result, but since
/// IndexMap entries are cloned by reference for large nested structures,
/// this is more efficient than cloning the entire library AST.
///
/// # Arguments
///
/// * `libraries` - Arc-wrapped pre-merged library definitions
/// * `user_source` - The user's source definition (small, gets moved)
/// * `source_path` - Path to the user's source file
///
/// # Returns
///
/// A merged StoredDefinition containing library classes and user's classes
pub fn merge_with_arc_libraries(
    libraries: &[Arc<StoredDefinition>],
    user_source: StoredDefinition,
    source_path: &str,
) -> Result<StoredDefinition> {
    let mut merged = StoredDefinition::default();

    // Merge library classes into the result
    // We clone entries from the Arc, but the library Arc itself is not cloned
    for lib in libraries {
        for (class_name, class_def) in &lib.class_list {
            if merged.class_list.contains_key(class_name) {
                // Merge packages if both are packages
                let existing = merged.class_list.get_mut(class_name).unwrap();
                if matches!(existing.class_type, crate::ir::ast::ClassType::Package)
                    && matches!(class_def.class_type, crate::ir::ast::ClassType::Package)
                {
                    merge_package_contents(existing, class_def.clone())?;
                }
                // Otherwise skip - first library takes precedence
            } else {
                merged
                    .class_list
                    .insert(class_name.clone(), class_def.clone());
            }
        }
    }

    // Merge user's source (takes precedence over library)
    merge_single_definition(&mut merged, user_source, source_path)?;

    Ok(merged)
}

/// Merge a single StoredDefinition into the merged result
fn merge_single_definition(
    merged: &mut StoredDefinition,
    def: StoredDefinition,
    file_path: &str,
) -> Result<()> {
    // Get the package prefix from the within clause
    let prefix = def
        .within
        .as_ref()
        .map(|n| n.to_string())
        .unwrap_or_default();

    for (class_name, class_def) in def.class_list {
        if prefix.is_empty() {
            // No within clause - add at top level
            if merged.class_list.contains_key(&class_name) {
                // Check if it's a package that can be merged
                let existing = merged.class_list.get_mut(&class_name).unwrap();
                if matches!(existing.class_type, crate::ir::ast::ClassType::Package)
                    && matches!(class_def.class_type, crate::ir::ast::ClassType::Package)
                {
                    // Merge package contents
                    merge_package_contents(existing, class_def)?;
                } else {
                    anyhow::bail!(
                        "Duplicate class '{}' found in '{}' (already defined)",
                        class_name,
                        file_path
                    );
                }
            } else {
                merged.class_list.insert(class_name, class_def);
            }
        } else {
            // Has within clause - place in package hierarchy
            place_class_in_hierarchy(merged, &prefix, class_name, class_def, file_path)?;
        }
    }

    Ok(())
}

/// Place a class in the correct position in the package hierarchy
fn place_class_in_hierarchy(
    merged: &mut StoredDefinition,
    prefix: &str,
    class_name: String,
    class_def: ClassDefinition,
    file_path: &str,
) -> Result<()> {
    let parts: Vec<&str> = prefix.split('.').collect();

    // Ensure the package hierarchy exists
    let mut current_map = &mut merged.class_list;
    let mut current_path = String::new();

    for (i, part) in parts.iter().enumerate() {
        if !current_path.is_empty() {
            current_path.push('.');
        }
        current_path.push_str(part);

        if i == 0 {
            // Top-level package
            if !current_map.contains_key(*part) {
                // Create the package
                let pkg = ClassDefinition {
                    name: crate::ir::ast::Token {
                        text: part.to_string(),
                        ..Default::default()
                    },
                    class_type: crate::ir::ast::ClassType::Package,
                    ..Default::default()
                };
                current_map.insert(part.to_string(), pkg);
            }

            let pkg = current_map.get_mut(*part).with_context(|| {
                format!(
                    "Failed to get package '{}' when placing class from '{}'",
                    part, file_path
                )
            })?;

            current_map = &mut pkg.classes;
        } else {
            // Nested package
            if !current_map.contains_key(*part) {
                let pkg = ClassDefinition {
                    name: crate::ir::ast::Token {
                        text: part.to_string(),
                        ..Default::default()
                    },
                    class_type: crate::ir::ast::ClassType::Package,
                    ..Default::default()
                };
                current_map.insert(part.to_string(), pkg);
            }

            let pkg = current_map.get_mut(*part).with_context(|| {
                format!(
                    "Failed to get nested package '{}' when placing class from '{}'",
                    part, file_path
                )
            })?;

            current_map = &mut pkg.classes;
        }
    }

    // Now add the class to the final package
    if current_map.contains_key(&class_name) {
        // Check if it's a package that can be merged
        let existing = current_map.get_mut(&class_name).unwrap();
        if matches!(existing.class_type, crate::ir::ast::ClassType::Package)
            && matches!(class_def.class_type, crate::ir::ast::ClassType::Package)
        {
            merge_package_contents(existing, class_def)?;
        } else {
            anyhow::bail!(
                "Duplicate class '{}.{}' found in '{}' (already defined)",
                prefix,
                class_name,
                file_path
            );
        }
    } else {
        current_map.insert(class_name, class_def);
    }

    Ok(())
}

/// Merge contents of two packages
fn merge_package_contents(existing: &mut ClassDefinition, new: ClassDefinition) -> Result<()> {
    // Merge nested classes
    for (name, class) in new.classes {
        if existing.classes.contains_key(&name) {
            let existing_nested = existing.classes.get_mut(&name).unwrap();
            if matches!(
                existing_nested.class_type,
                crate::ir::ast::ClassType::Package
            ) && matches!(class.class_type, crate::ir::ast::ClassType::Package)
            {
                merge_package_contents(existing_nested, class)?;
            } else {
                anyhow::bail!("Duplicate class '{}' in package", name);
            }
        } else {
            existing.classes.insert(name, class);
        }
    }

    // Merge components (shouldn't have duplicates in packages usually)
    for (name, comp) in new.components {
        if existing.components.contains_key(&name) {
            anyhow::bail!("Duplicate component '{}' in package", name);
        }
        existing.components.insert(name, comp);
    }

    // Merge extends
    existing.extends.extend(new.extends);

    // Merge imports
    existing.imports.extend(new.imports);

    Ok(())
}

/// Find all Modelica files in the given directories
pub fn find_modelica_files(search_paths: &[std::path::PathBuf]) -> Result<Vec<std::path::PathBuf>> {
    let mut files = Vec::new();

    for path in search_paths {
        if path.is_file() && path.extension().is_some_and(|e| e == "mo") {
            files.push(path.clone());
        } else if path.is_dir() {
            find_files_recursive(path, &mut files)?;
        }
    }

    Ok(files)
}

/// Recursively find .mo files in a directory
fn find_files_recursive(dir: &std::path::Path, files: &mut Vec<std::path::PathBuf>) -> Result<()> {
    // Skip ignored directories
    if should_ignore_directory(dir) {
        return Ok(());
    }

    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_file() && path.extension().is_some_and(|e| e == "mo") {
            files.push(path);
        } else if path.is_dir() && !should_ignore_directory(&path) {
            find_files_recursive(&path, files)?;
        }
    }

    Ok(())
}

/// Get the expected package path from a file path
///
/// This uses Modelica's file naming conventions:
/// - `PackageName/package.mo` -> PackageName
/// - `PackageName/ClassName.mo` -> PackageName.ClassName
/// - `PackageName/SubPackage/ClassName.mo` -> PackageName.SubPackage.ClassName
pub fn package_path_from_file(
    file_path: &std::path::Path,
    base_dir: &std::path::Path,
) -> Option<String> {
    let relative = file_path.strip_prefix(base_dir).ok()?;

    let mut parts: Vec<&str> = relative
        .components()
        .filter_map(|c| c.as_os_str().to_str())
        .collect();

    // Remove .mo extension from last part
    if let Some(last) = parts.last_mut()
        && last.ends_with(".mo")
    {
        *last = &last[..last.len() - 3];
    }

    // Handle package.mo specially - remove the "package" part
    if parts.last() == Some(&"package") {
        parts.pop();
    }

    if parts.is_empty() {
        None
    } else {
        Some(parts.join("."))
    }
}

/// Parse a `package.order` file and return the ordered list of entity names.
///
/// According to Modelica Spec 13.4, the `package.order` file contains one Modelica
/// identifier per line, specifying the order of classes and constants within a package.
///
/// # Arguments
///
/// * `path` - Path to the `package.order` file
///
/// # Returns
///
/// A vector of entity names in the order they should appear
pub fn parse_package_order(path: &Path) -> Result<Vec<String>> {
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Failed to read package.order: {}", path.display()))?;

    let mut order = Vec::new();
    for line in content.lines() {
        let trimmed = line.trim();
        // Skip empty lines and comments (lines starting with //)
        if !trimmed.is_empty() && !trimmed.starts_with("//") {
            order.push(trimmed.to_string());
        }
    }

    Ok(order)
}

/// Check if a directory is a Modelica package (contains package.mo)
pub fn is_modelica_package(dir: &Path) -> bool {
    dir.is_dir() && dir.join("package.mo").exists()
}

/// Parse a MODELICAPATH-style string into a vector of paths.
///
/// According to Modelica Spec 13.3, MODELICAPATH is an ordered list of library
/// root directories, separated by `:` on Unix or `;` on Windows.
///
/// # Arguments
///
/// * `path_str` - The path string to parse
/// * `separator` - The separator character (`:` on Unix, `;` on Windows)
///
/// # Returns
///
/// A vector of paths parsed from the string
pub fn parse_modelica_path_string(path_str: &str, separator: char) -> Vec<PathBuf> {
    path_str
        .split(separator)
        .filter(|s| !s.is_empty())
        .map(PathBuf::from)
        .collect()
}

/// Get the MODELICAPATH directories from the environment variable.
///
/// According to Modelica Spec 13.3, MODELICAPATH is an ordered list of library
/// root directories, separated by `:` on Unix or `;` on Windows.
///
/// # Returns
///
/// A vector of paths from the MODELICAPATH environment variable
pub fn get_modelica_path() -> Vec<PathBuf> {
    let separator = if cfg!(windows) { ';' } else { ':' };
    parse_modelica_path_string(
        &std::env::var("MODELICAPATH").unwrap_or_default(),
        separator,
    )
}

/// Find a top-level package by name in the given search paths.
///
/// Searches for either:
/// - A directory named `package_name` containing `package.mo`
/// - A file named `package_name.mo`
///
/// Extract the package name from a directory name that may include a version suffix.
///
/// According to Modelica Spec 13.4, a directory can be named either exactly as
/// the package (e.g., "Modelica") or with a version suffix (e.g., "Modelica 4.1.0").
///
/// # Arguments
///
/// * `dir_name` - The directory name to parse
///
/// # Returns
///
/// The package name without the version suffix
pub fn extract_package_name(dir_name: &str) -> &str {
    // Check for pattern: "PackageName X.Y.Z" where X.Y.Z is a version number
    // Version pattern: space followed by digits and dots (e.g., " 4.1.0", " 3.2.3")
    if let Some(space_idx) = dir_name.rfind(' ') {
        let suffix = &dir_name[space_idx + 1..];
        // Check if suffix looks like a version (starts with digit, contains only digits and dots)
        if !suffix.is_empty()
            && suffix.chars().next().is_some_and(|c| c.is_ascii_digit())
            && suffix.chars().all(|c| c.is_ascii_digit() || c == '.')
        {
            return &dir_name[..space_idx];
        }
    }
    dir_name
}

/// # Arguments
///
/// * `package_name` - The name of the top-level package to find
/// * `search_paths` - Library directories to search
///
/// # Returns
///
/// The path to the package directory or file, if found
pub fn find_package_in_paths(package_name: &str, search_paths: &[PathBuf]) -> Option<PathBuf> {
    for base_path in search_paths {
        // Check for directory-based package (exact name match)
        let dir_path = base_path.join(package_name);
        if is_modelica_package(&dir_path) {
            return Some(dir_path);
        }

        // Check for single-file package
        let file_path = base_path.join(format!("{}.mo", package_name));
        if file_path.is_file() {
            return Some(file_path);
        }

        // Check for versioned directory names (Modelica Spec 13.4)
        // e.g., looking for "Modelica" should also find "Modelica 4.1.0"
        if let Ok(entries) = std::fs::read_dir(base_path) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir()
                    && let Some(name) = path.file_name().and_then(|n| n.to_str())
                {
                    // Check if this directory's package name (without version) matches
                    if extract_package_name(name) == package_name && is_modelica_package(&path) {
                        return Some(path);
                    }
                }
            }
        }
    }
    None
}

/// Find a top-level package by name in the MODELICAPATH directories.
///
/// This is a convenience wrapper around `find_package_in_paths` that uses
/// the MODELICAPATH environment variable.
///
/// # Arguments
///
/// * `package_name` - The name of the top-level package to find
///
/// # Returns
///
/// The path to the package directory or file, if found
pub fn find_package_in_modelica_path(package_name: &str) -> Option<PathBuf> {
    find_package_in_paths(package_name, &get_modelica_path())
}

/// Discover all Modelica files in a directory-based package structure.
///
/// This function follows Modelica Spec 13.4 conventions:
/// - Each directory with `package.mo` is a package
/// - `package.order` specifies the order of nested entities
/// - Files named `ClassName.mo` define classes
/// - Subdirectories with `package.mo` define sub-packages
///
/// # Arguments
///
/// * `package_dir` - Root directory of the package
///
/// # Returns
///
/// A vector of paths to all `.mo` files in the correct order
pub fn discover_package_files(package_dir: &Path) -> Result<Vec<PathBuf>> {
    if !is_modelica_package(package_dir) {
        anyhow::bail!(
            "Directory '{}' is not a Modelica package (missing package.mo)",
            package_dir.display()
        );
    }

    let mut files = Vec::new();

    // First, add package.mo
    let package_mo = package_dir.join("package.mo");
    files.push(package_mo);

    // Check for package.order to determine ordering
    let order_file = package_dir.join("package.order");
    let ordered_names: Option<Vec<String>> = if order_file.exists() {
        Some(parse_package_order(&order_file)?)
    } else {
        None
    };

    // Collect all entities in the directory
    let mut entities: Vec<(String, PathBuf)> = Vec::new();

    for entry in std::fs::read_dir(package_dir)? {
        let entry = entry?;
        let path = entry.path();
        let name = entry.file_name().to_string_lossy().to_string();

        // Skip package.mo and package.order
        if name == "package.mo" || name == "package.order" {
            continue;
        }

        if path.is_file() && name.ends_with(".mo") {
            // Single-file class: ClassName.mo
            let class_name = name.trim_end_matches(".mo").to_string();
            entities.push((class_name, path));
        } else if path.is_dir() && is_modelica_package(&path) {
            // Sub-package directory
            entities.push((name, path));
        }
    }

    // Sort entities according to package.order if present, otherwise alphabetically
    if let Some(order) = ordered_names {
        // Create a map of name -> position for quick lookup
        let order_map: std::collections::HashMap<&str, usize> = order
            .iter()
            .enumerate()
            .map(|(i, name)| (name.as_str(), i))
            .collect();

        entities.sort_by(|(a, _), (b, _)| {
            let pos_a = order_map.get(a.as_str()).copied().unwrap_or(usize::MAX);
            let pos_b = order_map.get(b.as_str()).copied().unwrap_or(usize::MAX);
            pos_a.cmp(&pos_b).then_with(|| a.cmp(b))
        });
    } else {
        // Sort alphabetically if no package.order
        entities.sort_by(|(a, _), (b, _)| a.cmp(b));
    }

    // Add files in order, recursively processing sub-packages
    for (_, path) in entities {
        if path.is_file() {
            files.push(path);
        } else if path.is_dir() {
            // Recursively discover sub-package files
            let sub_files = discover_package_files(&path)?;
            files.extend(sub_files);
        }
    }

    Ok(files)
}

/// Discover Modelica files from a path, handling both files and package directories.
///
/// This is a convenience function that:
/// - If given a `.mo` file, returns just that file
/// - If given a directory with `package.mo`, discovers all files in the package structure
/// - If given a directory without `package.mo`, finds all `.mo` files recursively
///
/// # Arguments
///
/// * `path` - Path to a file or directory
///
/// # Returns
///
/// A vector of paths to all `.mo` files
pub fn discover_modelica_files(path: &Path) -> Result<Vec<PathBuf>> {
    if path.is_file() {
        if path.extension().is_some_and(|e| e == "mo") {
            Ok(vec![path.to_path_buf()])
        } else {
            anyhow::bail!("File '{}' is not a Modelica file (.mo)", path.display());
        }
    } else if path.is_dir() {
        if is_modelica_package(path) {
            discover_package_files(path)
        } else {
            // Fall back to simple recursive discovery
            find_modelica_files(&[path.to_path_buf()])
        }
    } else {
        anyhow::bail!("Path '{}' does not exist", path.display());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_merge_empty_definitions() {
        let result = merge_stored_definitions(vec![]).unwrap();
        assert!(result.class_list.is_empty());
    }

    #[test]
    fn test_merge_single_definition() {
        let mut def = StoredDefinition::default();
        let class = ClassDefinition {
            name: crate::ir::ast::Token {
                text: "TestModel".to_string(),
                ..Default::default()
            },
            class_type: crate::ir::ast::ClassType::Model,
            ..Default::default()
        };
        def.class_list.insert("TestModel".to_string(), class);

        let result = merge_stored_definitions(vec![("test.mo".to_string(), def)]).unwrap();
        assert!(result.class_list.contains_key("TestModel"));
    }

    #[test]
    fn test_merge_with_within_clause() {
        let mut def = StoredDefinition {
            within: Some(crate::ir::ast::Name {
                name: vec![crate::ir::ast::Token {
                    text: "MyPackage".to_string(),
                    ..Default::default()
                }],
            }),
            ..Default::default()
        };
        let class = ClassDefinition {
            name: crate::ir::ast::Token {
                text: "TestModel".to_string(),
                ..Default::default()
            },
            class_type: crate::ir::ast::ClassType::Model,
            ..Default::default()
        };
        def.class_list.insert("TestModel".to_string(), class);

        let result = merge_stored_definitions(vec![("test.mo".to_string(), def)]).unwrap();

        // Should have created MyPackage
        assert!(result.class_list.contains_key("MyPackage"));

        // And TestModel should be inside it
        let pkg = result.class_list.get("MyPackage").unwrap();
        assert!(pkg.classes.contains_key("TestModel"));
    }

    #[test]
    fn test_package_path_from_file() {
        use std::path::Path;

        let base = Path::new("/home/user/models");

        // Simple model file
        let path = Path::new("/home/user/models/MyModel.mo");
        assert_eq!(
            package_path_from_file(path, base),
            Some("MyModel".to_string())
        );

        // Nested model file
        let path = Path::new("/home/user/models/MyPackage/MyModel.mo");
        assert_eq!(
            package_path_from_file(path, base),
            Some("MyPackage.MyModel".to_string())
        );

        // Package file
        let path = Path::new("/home/user/models/MyPackage/package.mo");
        assert_eq!(
            package_path_from_file(path, base),
            Some("MyPackage".to_string())
        );
    }

    #[test]
    fn test_parse_package_order() {
        use std::io::Write;

        // Create a temp file for testing
        let temp_dir = std::env::temp_dir();
        let order_path = temp_dir.join("test_package.order");

        let mut file = std::fs::File::create(&order_path).unwrap();
        writeln!(file, "// This is a comment").unwrap();
        writeln!(file, "Types").unwrap();
        writeln!(file).unwrap();
        writeln!(file, "Functions").unwrap();
        writeln!(file, "  Examples  ").unwrap(); // with whitespace
        drop(file);

        let order = parse_package_order(&order_path).unwrap();
        assert_eq!(order, vec!["Types", "Functions", "Examples"]);

        // Clean up
        std::fs::remove_file(&order_path).ok();
    }

    #[test]
    fn test_is_modelica_package() {
        // Create temp directory structure
        let temp_dir = std::env::temp_dir().join("test_modelica_pkg");
        std::fs::create_dir_all(&temp_dir).ok();

        // Initially not a package (no package.mo)
        assert!(!is_modelica_package(&temp_dir));

        // Create package.mo to make it a package
        std::fs::write(temp_dir.join("package.mo"), "package Test end Test;").unwrap();
        assert!(is_modelica_package(&temp_dir));

        // Clean up
        std::fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn test_parse_modelica_path_empty() {
        // Empty string should produce empty paths
        let paths = parse_modelica_path_string("", ':');
        assert!(paths.is_empty());

        // Also test with semicolon separator (Windows style)
        let paths = parse_modelica_path_string("", ';');
        assert!(paths.is_empty());
    }

    #[test]
    fn test_parse_modelica_path_single() {
        let paths = parse_modelica_path_string("/path/one", ':');
        assert_eq!(paths.len(), 1);
        assert_eq!(paths[0], PathBuf::from("/path/one"));
    }

    #[test]
    fn test_parse_modelica_path_multiple_unix() {
        let paths = parse_modelica_path_string("/path/one:/path/two:/path/three", ':');
        assert_eq!(paths.len(), 3);
        assert_eq!(paths[0], PathBuf::from("/path/one"));
        assert_eq!(paths[1], PathBuf::from("/path/two"));
        assert_eq!(paths[2], PathBuf::from("/path/three"));
    }

    #[test]
    fn test_parse_modelica_path_multiple_windows() {
        let paths = parse_modelica_path_string("C:\\path\\one;D:\\path\\two", ';');
        assert_eq!(paths.len(), 2);
        assert_eq!(paths[0], PathBuf::from("C:\\path\\one"));
        assert_eq!(paths[1], PathBuf::from("D:\\path\\two"));
    }

    #[test]
    fn test_parse_modelica_path_trailing_separator() {
        // Trailing separators should not create empty path entries
        let paths = parse_modelica_path_string("/path/one:/path/two:", ':');
        assert_eq!(paths.len(), 2);
    }

    #[test]
    fn test_find_package_not_found() {
        let result = find_package_in_modelica_path("NonExistentPackage12345");
        assert!(result.is_none());
    }

    #[test]
    fn test_extract_package_name_no_version() {
        // Simple package name without version suffix
        assert_eq!(extract_package_name("Modelica"), "Modelica");
        assert_eq!(extract_package_name("MyPackage"), "MyPackage");
        assert_eq!(extract_package_name("Some_Package"), "Some_Package");
    }

    #[test]
    fn test_extract_package_name_with_version() {
        // Package names with version suffixes (Modelica Spec 13.4)
        assert_eq!(extract_package_name("Modelica 4.1.0"), "Modelica");
        assert_eq!(extract_package_name("Modelica 3.2.3"), "Modelica");
        assert_eq!(
            extract_package_name("ModelicaServices 4.0.0"),
            "ModelicaServices"
        );
        assert_eq!(extract_package_name("Complex 4.1.0"), "Complex");
        assert_eq!(extract_package_name("MyLibrary 1.0"), "MyLibrary");
        assert_eq!(extract_package_name("SomePackage 2.0.0.1"), "SomePackage");
    }

    #[test]
    fn test_extract_package_name_non_version_suffix() {
        // Space followed by non-version text should not be stripped
        assert_eq!(extract_package_name("My Package Name"), "My Package Name");
        assert_eq!(
            extract_package_name("Package With Suffix"),
            "Package With Suffix"
        );
        // Text after space that doesn't start with a digit
        assert_eq!(extract_package_name("Modelica dev"), "Modelica dev");
        assert_eq!(extract_package_name("Package beta1"), "Package beta1");
    }
}
