//! Multi-file merging support.
//!
//! This module provides utilities for merging multiple StoredDefinitions into one,
//! handling within clauses and package hierarchies.

use anyhow::{Context, Result};
use rumoca_ir_ast as ast;
use serde::Serialize;
use serde_json::Value;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub(crate) struct MergeSemanticError {
    pub message: String,
    pub labels: Vec<MergeSemanticLabel>,
}

#[derive(Debug, Clone)]
pub(crate) struct MergeSemanticLabel {
    pub file_name: String,
    pub start: usize,
    pub end: usize,
    pub primary: bool,
    pub message: &'static str,
}

impl MergeSemanticError {
    fn label_from_token(
        token: &rumoca_core::Token,
        primary: bool,
        message: &'static str,
    ) -> MergeSemanticLabel {
        let start = token.location.start as usize;
        let mut end = token.location.end as usize;
        if end <= start {
            end = start.saturating_add(1);
        }
        MergeSemanticLabel {
            file_name: token.location.file_name.clone(),
            start,
            end,
            primary,
            message,
        }
    }

    fn from_primary_with_related(
        message: impl Into<String>,
        primary: &rumoca_core::Token,
        related: &rumoca_core::Token,
    ) -> Self {
        Self {
            message: message.into(),
            labels: vec![
                Self::label_from_token(primary, true, "duplicate declaration"),
                Self::label_from_token(related, false, "previous declaration"),
            ],
        }
    }
}

impl std::fmt::Display for MergeSemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for MergeSemanticError {}

fn merge_error_from_primary_with_related(
    message: impl Into<String>,
    primary: &rumoca_core::Token,
    related: &rumoca_core::Token,
) -> anyhow::Error {
    anyhow::Error::new(MergeSemanticError::from_primary_with_related(
        message, primary, related,
    ))
}

/// Merge multiple StoredDefinitions into a single one.
///
/// This function combines class definitions from multiple files, using the `within`
/// clause to place classes in their correct package hierarchy.
///
/// # Arguments
///
/// * `definitions` - A list of (file_path, ast::StoredDefinition) tuples
///
/// # Returns
///
/// A merged ast::StoredDefinition containing all classes from all files
pub fn merge_stored_definitions(
    mut definitions: Vec<(String, ast::StoredDefinition)>,
) -> Result<ast::StoredDefinition> {
    // Deterministic merge order across runs/filesystems.
    definitions.sort_by(|(a, _), (b, _)| a.cmp(b));

    let mut merged = ast::StoredDefinition::default();

    for (file_path, def) in definitions {
        merge_single_definition(&mut merged, def, &file_path)?;
    }

    Ok(merged)
}

/// Merge a single ast::StoredDefinition into the merged result
fn merge_single_definition(
    merged: &mut ast::StoredDefinition,
    def: ast::StoredDefinition,
    file_path: &str,
) -> Result<()> {
    let ast::StoredDefinition { classes, within } = def;
    // Get the package prefix from the within clause
    let prefix = within.as_ref().map(|n| n.to_string()).unwrap_or_default();

    for (class_name, class_def) in classes {
        if !prefix.is_empty() {
            // Has within clause - place in package hierarchy
            place_class_in_hierarchy(
                merged,
                within.as_ref(),
                &prefix,
                class_name,
                class_def,
                file_path,
            )?;
            continue;
        }
        // No within clause - add at top level
        merge_class_at_top_level(merged, class_name, class_def, file_path)?;
    }

    Ok(())
}

/// Merge a class at top level, handling duplicate package merging.
fn merge_class_at_top_level(
    merged: &mut ast::StoredDefinition,
    class_name: String,
    class_def: ast::ClassDef,
    file_path: &str,
) -> Result<()> {
    let Some(existing) = merged.classes.get_mut(&class_name) else {
        merged.classes.insert(class_name, class_def);
        return Ok(());
    };

    // Check if both are packages that can be merged
    let both_packages = matches!(existing.class_type, rumoca_core::ClassType::Package)
        && matches!(class_def.class_type, rumoca_core::ClassType::Package);

    if both_packages {
        merge_package_contents(existing, class_def, &class_name, file_path)?;
    } else if classes_semantically_identical(existing, &class_def) {
        // Identical duplicate definition; keep first definition.
    } else {
        return Err(merge_error_from_primary_with_related(
            format!(
                "Duplicate class '{}' found in '{}' with non-identical definition",
                class_name, file_path
            ),
            &class_def.name,
            &existing.name,
        ));
    }
    Ok(())
}

/// Place a class in the correct position in the package hierarchy
fn place_class_in_hierarchy(
    merged: &mut ast::StoredDefinition,
    within: Option<&ast::Name>,
    prefix: &str,
    class_name: String,
    class_def: ast::ClassDef,
    file_path: &str,
) -> Result<()> {
    let parts: Vec<&str> = rumoca_core::split_path_with_indices(prefix);

    // Ensure the package hierarchy exists
    let mut current_map = &mut merged.classes;
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
                let location = package_placeholder_location(within, i, part, &class_def)?;
                let pkg = package_placeholder(part, location);
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
                let location = package_placeholder_location(within, i, part, &class_def)?;
                let pkg = package_placeholder(part, location);
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
    if let Some(existing) = current_map.get_mut(&class_name) {
        // Check if it's a package that can be merged
        if matches!(existing.class_type, rumoca_core::ClassType::Package)
            && matches!(class_def.class_type, rumoca_core::ClassType::Package)
        {
            let qualified = format!("{}.{}", prefix, class_name);
            merge_package_contents(existing, class_def, &qualified, file_path)?;
        } else if classes_semantically_identical(existing, &class_def) {
            // Identical duplicate definition; keep first definition.
        } else {
            return Err(merge_error_from_primary_with_related(
                format!(
                    "Duplicate class '{}.{}' found in '{}' with non-identical definition",
                    prefix, class_name, file_path
                ),
                &class_def.name,
                &existing.name,
            ));
        }
    } else {
        current_map.insert(class_name, class_def);
    }

    Ok(())
}

fn package_placeholder(part: &str, location: rumoca_core::Location) -> ast::ClassDef {
    ast::ClassDef {
        name: rumoca_core::Token {
            text: std::sync::Arc::from(part),
            location: location.clone(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Package,
        class_type_token: rumoca_core::Token {
            text: std::sync::Arc::from("package"),
            location: location.clone(),
            ..Default::default()
        },
        location,
        ..Default::default()
    }
}

fn package_placeholder_location(
    within: Option<&ast::Name>,
    index: usize,
    part: &str,
    owner: &ast::ClassDef,
) -> Result<rumoca_core::Location> {
    if let Some(location) = within
        .and_then(|name| name.name.get(index))
        .map(|token| token.location.clone())
        .filter(source_location_is_nonempty)
        .or_else(|| source_location_for_class(owner))
    {
        return Ok(location);
    }
    let within_name = match within {
        Some(name) => name.to_string(),
        None => "<none>".to_string(),
    };
    anyhow::bail!(
        "missing source location for synthesized package placeholder `{part}` in within `{within_name}`"
    );
}

fn source_location_for_class(class: &ast::ClassDef) -> Option<rumoca_core::Location> {
    [
        &class.location,
        &class.name.location,
        &class.class_type_token.location,
    ]
    .into_iter()
    .find(|location| source_location_is_nonempty(location))
    .cloned()
}

/// Merge contents of two packages
fn merge_package_contents(
    existing: &mut ast::ClassDef,
    new: ast::ClassDef,
    package_name: &str,
    file_path: &str,
) -> Result<()> {
    if is_synthetic_package_placeholder(existing) && class_has_source_location(&new) {
        promote_package_placeholder(existing, new, package_name, file_path)?;
        return Ok(());
    }

    // Merge nested classes
    for (name, class) in new.classes {
        merge_package_child_class(existing, name, class, package_name, file_path)?;
    }

    // Merge components, rejecting conflicting duplicates.
    for (name, comp) in new.components {
        merge_package_component(existing, name, comp, package_name, file_path)?;
    }

    // Merge imports
    for import in new.imports {
        existing.imports.push(import);
    }

    Ok(())
}

fn promote_package_placeholder(
    placeholder: &mut ast::ClassDef,
    mut declared: ast::ClassDef,
    package_name: &str,
    file_path: &str,
) -> Result<()> {
    let placeholder_classes = std::mem::take(&mut placeholder.classes);
    let placeholder_components = std::mem::take(&mut placeholder.components);
    let mut placeholder_imports = std::mem::take(&mut placeholder.imports);

    for (name, class) in placeholder_classes {
        merge_package_child_class(&mut declared, name, class, package_name, file_path)?;
    }
    for (name, component) in placeholder_components {
        merge_package_component(&mut declared, name, component, package_name, file_path)?;
    }
    placeholder_imports.append(&mut declared.imports);
    declared.imports = placeholder_imports;

    *placeholder = declared;
    Ok(())
}

fn merge_package_child_class(
    package: &mut ast::ClassDef,
    name: String,
    class: ast::ClassDef,
    package_name: &str,
    file_path: &str,
) -> Result<()> {
    let Some(existing_class) = package.classes.get_mut(&name) else {
        package.classes.insert(name, class);
        return Ok(());
    };

    if matches!(existing_class.class_type, rumoca_core::ClassType::Package)
        && matches!(class.class_type, rumoca_core::ClassType::Package)
    {
        let nested_name = format!("{}.{}", package_name, name);
        return merge_package_contents(existing_class, class, &nested_name, file_path);
    }
    if classes_semantically_identical(existing_class, &class) {
        return Ok(());
    }
    Err(merge_error_from_primary_with_related(
        format!(
            "Duplicate class '{}.{}' found in '{}' with non-identical definition",
            package_name, name, file_path
        ),
        &class.name,
        &existing_class.name,
    ))
}

fn merge_package_component(
    package: &mut ast::ClassDef,
    name: String,
    component: ast::Component,
    package_name: &str,
    file_path: &str,
) -> Result<()> {
    let Some(existing_component) = package.components.get(&name) else {
        package.components.insert(name, component);
        return Ok(());
    };

    if components_semantically_identical(existing_component, &component) {
        return Ok(());
    }
    Err(merge_error_from_primary_with_related(
        format!(
            "Duplicate component '{}.{}' found in '{}' with non-identical definition",
            package_name, name, file_path
        ),
        &component.name_token,
        &existing_component.name_token,
    ))
}

fn is_synthetic_package_placeholder(class: &ast::ClassDef) -> bool {
    matches!(class.class_type, rumoca_core::ClassType::Package)
        && class.description.is_empty()
        && class.extends.is_empty()
        && class.imports.is_empty()
        && class.components.is_empty()
        && class.equations.is_empty()
        && class.initial_equations.is_empty()
        && class.algorithms.is_empty()
        && class.initial_algorithms.is_empty()
        && class.equation_keyword.is_none()
        && class.initial_equation_keyword.is_none()
        && class.algorithm_keyword.is_none()
        && class.initial_algorithm_keyword.is_none()
        && class.end_name_token.is_none()
        && class.enum_literals.is_empty()
        && class.annotation.is_empty()
        && class.external.is_none()
}

fn class_has_source_location(class: &ast::ClassDef) -> bool {
    source_location_is_nonempty(&class.location)
        || source_location_is_nonempty(&class.name.location)
        || source_location_is_nonempty(&class.class_type_token.location)
}

fn source_location_is_nonempty(location: &rumoca_core::Location) -> bool {
    !location.file_name.is_empty() && location.start < location.end
}

fn classes_semantically_identical(existing: &ast::ClassDef, new: &ast::ClassDef) -> bool {
    semantically_identical(existing, new)
}

fn components_semantically_identical(existing: &ast::Component, new: &ast::Component) -> bool {
    semantically_identical(existing, new)
}

fn semantically_identical<T>(existing: &T, new: &T) -> bool
where
    T: Serialize,
{
    let Ok(mut existing) = serde_json::to_value(existing) else {
        return false;
    };
    let Ok(mut new) = serde_json::to_value(new) else {
        return false;
    };

    strip_nonsemantic_fields(&mut existing);
    strip_nonsemantic_fields(&mut new);
    existing == new
}

fn strip_nonsemantic_fields(value: &mut Value) {
    match value {
        Value::Object(map) => {
            map.remove("def_id");
            map.remove("scope_id");
            map.remove("base_def_id");
            map.remove("location");
            map.remove("token_number");
            map.remove("token_type");

            for nested in map.values_mut() {
                strip_nonsemantic_fields(nested);
            }
        }
        Value::Array(arr) => {
            for nested in arr {
                strip_nonsemantic_fields(nested);
            }
        }
        _ => {}
    }
}

/// Collect all model names from a ast::StoredDefinition recursively.
///
/// Returns names of models, blocks, and classes suitable for compilation.
pub fn collect_model_names(def: &ast::StoredDefinition) -> Vec<String> {
    let mut names = Vec::new();
    let prefix = stored_definition_within_prefix(def);
    collect_models_from_classes(&def.classes, &prefix, &mut names);
    names
}

/// Qualify a top-level class name by the stored definition's `within` clause.
pub fn qualify_stored_definition_class_name(def: &ast::StoredDefinition, name: &str) -> String {
    let prefix = stored_definition_within_prefix(def);
    if prefix.is_empty() {
        name.to_string()
    } else {
        format!("{prefix}.{name}")
    }
}

fn stored_definition_within_prefix(def: &ast::StoredDefinition) -> String {
    def.within
        .as_ref()
        .map(ToString::to_string)
        .filter(|path| !path.is_empty())
        .unwrap_or_default()
}

/// Check if a model name refers to a built-in operator (MLS §3.7.2).
fn is_builtin_operator(name: &str) -> bool {
    name.contains("Connections.branch")
        || name.contains("Connections.root")
        || name.contains("Connections.potentialRoot")
        || name.contains("Connections.isRoot")
        || name.contains("Connections.rooted")
}

fn collect_models_from_classes(
    classes: &ast::AstIndexMap<String, ast::ClassDef>,
    prefix: &str,
    names: &mut Vec<String>,
) {
    for (name, class) in classes {
        let full_name = if prefix.is_empty() {
            name.clone()
        } else {
            format!("{}.{}", prefix, name)
        };

        // Add if it's a model, block, or class (not package, connector, record, function, type)
        match class.class_type {
            rumoca_core::ClassType::Model
            | rumoca_core::ClassType::Block
            | rumoca_core::ClassType::Class
                if !is_builtin_operator(&full_name) =>
            {
                names.push(full_name.clone());
            }
            _ => {}
        }

        // Recurse into nested classes
        if !class.classes.is_empty() {
            collect_models_from_classes(&class.classes, &full_name, names);
        }
    }
}

/// Count all class types in a `ast::StoredDefinition` recursively.
///
/// Returns a map from class type name (e.g. "model", "connector") to count.
/// Skips builtin operators (same filter as `collect_model_names`).
pub fn collect_class_type_counts(def: &ast::StoredDefinition) -> HashMap<String, usize> {
    let mut counts = HashMap::new();
    count_class_types_recursive(&def.classes, "", &mut counts);
    counts
}

fn count_class_types_recursive(
    classes: &ast::AstIndexMap<String, ast::ClassDef>,
    prefix: &str,
    counts: &mut HashMap<String, usize>,
) {
    for (name, class) in classes {
        let full_name = if prefix.is_empty() {
            name.clone()
        } else {
            format!("{}.{}", prefix, name)
        };

        if !is_builtin_operator(&full_name) {
            *counts
                .entry(class.class_type.as_str().to_string())
                .or_insert(0) += 1;
        }

        if !class.classes.is_empty() {
            count_class_types_recursive(&class.classes, &full_name, counts);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn token(text: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: std::sync::Arc::from(text),
            ..Default::default()
        }
    }

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2)
    }

    fn model_with_real(name: &str, component: &str) -> ast::ClassDef {
        let mut class = ast::ClassDef {
            name: token(name),
            class_type: rumoca_core::ClassType::Model,
            ..Default::default()
        };
        class.components.insert(
            component.to_string(),
            ast::Component {
                name: component.to_string(),
                name_token: token(component),
                type_name: ast::Name::from_string("Real"),
                ..ast::Component::empty_with_span(test_span())
            },
        );
        class
    }

    fn parse_definition(source: &str, file_name: &str) -> ast::StoredDefinition {
        rumoca_phase_parse::parse_to_ast(source, file_name).expect("parse test definition")
    }

    #[test]
    fn test_merge_simple() {
        let mut def1 = ast::StoredDefinition::default();
        def1.classes
            .insert("Model1".to_string(), model_with_real("Model1", "x"));

        let mut def2 = ast::StoredDefinition::default();
        def2.classes
            .insert("Model2".to_string(), model_with_real("Model2", "y"));

        let merged = merge_stored_definitions(vec![
            ("file1.mo".to_string(), def1),
            ("file2.mo".to_string(), def2),
        ])
        .unwrap();

        assert_eq!(merged.classes.len(), 2);
        assert!(merged.classes.contains_key("Model1"));
        assert!(merged.classes.contains_key("Model2"));
    }

    #[test]
    fn test_merge_with_within() {
        let def = parse_definition(
            "within MyPackage.SubPackage;\nmodel MyModel\n  Real x;\nend MyModel;",
            "file.mo",
        );
        let merged = merge_stored_definitions(vec![("file.mo".to_string(), def)]).unwrap();

        // Should have created MyPackage at top level
        assert!(merged.classes.contains_key("MyPackage"));
        let pkg = merged.classes.get("MyPackage").unwrap();
        assert!(matches!(pkg.class_type, rumoca_core::ClassType::Package));

        // Should have SubPackage inside
        assert!(pkg.classes.contains_key("SubPackage"));
        let subpkg = pkg.classes.get("SubPackage").unwrap();

        // Should have MyModel inside SubPackage
        assert!(subpkg.classes.contains_key("MyModel"));
    }

    #[test]
    fn package_declaration_promotes_source_free_within_placeholder() {
        let merged = merge_stored_definitions(vec![
            (
                "workspace/Pkg/Sub/A.mo".to_string(),
                parse_definition("within Pkg.Sub; model A end A;", "workspace/Pkg/Sub/A.mo"),
            ),
            (
                "workspace/Pkg/Sub/package.mo".to_string(),
                parse_definition(
                    "within Pkg; package Sub end Sub;",
                    "workspace/Pkg/Sub/package.mo",
                ),
            ),
            (
                "workspace/Pkg/package.mo".to_string(),
                parse_definition("package Pkg end Pkg;", "workspace/Pkg/package.mo"),
            ),
        ])
        .expect("merge package tree");

        let pkg = merged.classes.get("Pkg").expect("Pkg package");
        assert_eq!(pkg.location.file_name, "workspace/Pkg/package.mo");
        assert!(pkg.location.start < pkg.location.end);

        let sub = pkg.classes.get("Sub").expect("Sub package");
        assert_eq!(sub.location.file_name, "workspace/Pkg/Sub/package.mo");
        assert!(sub.location.start < sub.location.end);
        assert!(sub.classes.contains_key("A"));
    }

    #[test]
    fn within_placeholder_inherits_source_location_from_child_source() {
        let merged = merge_stored_definitions(vec![(
            "workspace/Examples/Ball.mo".to_string(),
            parse_definition(
                "within Examples;\nmodel Ball\n  Real x;\nend Ball;",
                "workspace/Examples/Ball.mo",
            ),
        )])
        .expect("merge within-qualified model");

        let examples = merged.classes.get("Examples").expect("Examples package");
        assert_eq!(examples.location.file_name, "workspace/Examples/Ball.mo");
        assert!(examples.location.start < examples.location.end);
        assert!(examples.classes.contains_key("Ball"));
    }

    #[test]
    fn test_collect_model_names_includes_within_prefix() {
        let mut def = ast::StoredDefinition {
            within: Some(ast::Name::from_string("Examples")),
            ..Default::default()
        };
        def.classes
            .insert("Ball".to_string(), model_with_real("Ball", "x"));

        assert_eq!(collect_model_names(&def), vec!["Examples.Ball"]);
        assert_eq!(
            qualify_stored_definition_class_name(&def, "Ball"),
            "Examples.Ball"
        );
    }

    #[test]
    fn test_identical_duplicate_class_is_accepted() {
        let mut def1 = ast::StoredDefinition::default();
        def1.classes
            .insert("M".to_string(), model_with_real("M", "x"));

        let mut def2 = ast::StoredDefinition::default();
        def2.classes
            .insert("M".to_string(), model_with_real("M", "x"));

        let merged =
            merge_stored_definitions(vec![("b.mo".to_string(), def2), ("a.mo".to_string(), def1)])
                .expect("identical duplicate should be accepted");

        let model = merged.classes.get("M").expect("M should exist");
        assert!(model.components.contains_key("x"));
        assert_eq!(merged.classes.len(), 1);
    }

    #[test]
    fn test_non_identical_duplicate_class_is_rejected() {
        let mut def1 = ast::StoredDefinition::default();
        def1.classes
            .insert("M".to_string(), model_with_real("M", "x"));

        let mut def2 = ast::StoredDefinition::default();
        def2.classes
            .insert("M".to_string(), model_with_real("M", "y"));

        let err =
            merge_stored_definitions(vec![("a.mo".to_string(), def1), ("b.mo".to_string(), def2)])
                .expect_err("conflicting duplicate should fail");
        let msg = err.to_string();
        assert!(msg.contains("Duplicate class 'M'"));
        assert!(msg.contains("b.mo"));
    }

    #[test]
    fn test_non_identical_duplicate_within_class_is_rejected() {
        let def1 = parse_definition("within Pkg;\nmodel M\n  Real x;\nend M;", "a.mo");
        let def2 = parse_definition("within Pkg;\nmodel M\n  Real y;\nend M;", "b.mo");

        let err =
            merge_stored_definitions(vec![("a.mo".to_string(), def1), ("b.mo".to_string(), def2)])
                .expect_err("conflicting nested duplicate should fail");
        let msg = err.to_string();
        assert!(msg.contains("Duplicate class 'Pkg.M'"));
        assert!(msg.contains("b.mo"));
    }

    #[test]
    fn test_merge_order_is_deterministic_by_file_path() {
        let mut first_lexical = ast::StoredDefinition::default();
        first_lexical
            .classes
            .insert("M".to_string(), model_with_real("M", "x"));

        let mut second_lexical = ast::StoredDefinition::default();
        second_lexical
            .classes
            .insert("M".to_string(), model_with_real("M", "y"));

        let err = merge_stored_definitions(vec![
            // Intentionally pass reverse order to validate internal sorting.
            ("z.mo".to_string(), second_lexical),
            ("a.mo".to_string(), first_lexical),
        ])
        .expect_err("non-identical duplicates must fail deterministically");

        // If merge order is deterministic by file path, "z.mo" is always the duplicate
        // that conflicts with the earlier "a.mo" definition.
        assert!(err.to_string().contains("z.mo"));
    }
}
