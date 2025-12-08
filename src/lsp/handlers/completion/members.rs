//! Member completion handling for dot completion.
//!
//! Provides completions for component members when typing after a dot.
//!
//! This module uses canonical scope resolution functions from
//! `crate::ir::transform::scope_resolver` to avoid duplication.

use lsp_types::{CompletionItem, CompletionItemKind, Position};

use crate::ir::ast::{ClassType, StoredDefinition, Variability};
use crate::ir::transform::scope_resolver::{
    ImportResolver, find_class_in_ast, resolve_type_candidates,
};
use crate::lsp::workspace::WorkspaceState;

/// Get completions for component members (dot completion)
pub fn get_member_completions(
    ast: &StoredDefinition,
    prefix: &str,
    _position: Position,
    mut workspace: Option<&mut WorkspaceState>,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    let parts: Vec<&str> = prefix.split('.').collect();
    if parts.len() < 2 {
        return items;
    }

    let component_name = parts[0];

    for class in ast.class_list.values() {
        if let Some(comp) = class.components.get(component_name) {
            let type_name = comp.type_name.to_string();

            // Try to find the type class locally first (using canonical function)
            if let Some(type_class) = find_class_in_ast(ast, &type_name) {
                items.extend(get_class_member_completions(type_class));
            } else if let Some(ws) = workspace.as_deref_mut() {
                // Try to resolve through imports and look up in workspace
                let import_resolver = ImportResolver::from_imports(&class.imports);
                let resolved_type = import_resolver
                    .resolve(&type_name)
                    .map(|s| s.to_string())
                    .unwrap_or(type_name.clone());

                // Ensure the package is indexed and get completions from workspace
                ws.ensure_package_indexed(&resolved_type);
                items.extend(get_workspace_type_members(ws, &resolved_type));
            }

            // Built-in type attributes
            items.extend(get_type_attributes(&type_name));
        }

        // Also check nested classes for the component
        for nested_class in class.classes.values() {
            if let Some(comp) = nested_class.components.get(component_name) {
                let type_name = comp.type_name.to_string();
                if let Some(type_class) = find_class_in_ast(ast, &type_name) {
                    items.extend(get_class_member_completions(type_class));
                } else if let Some(ws) = workspace.as_deref_mut() {
                    // Try imports from the nested class or parent
                    let import_resolver = ImportResolver::from_imports(&nested_class.imports);
                    let resolved_type = import_resolver
                        .resolve(&type_name)
                        .map(|s| s.to_string())
                        .unwrap_or(type_name.clone());
                    ws.ensure_package_indexed(&resolved_type);
                    items.extend(get_workspace_type_members(ws, &resolved_type));
                }
            }
        }
    }

    items
}

// Note: build_import_aliases is replaced by ImportResolver::from_imports from canonical module

/// Get member completions for a type from the workspace
fn get_workspace_type_members(
    workspace: &mut WorkspaceState,
    qualified_type: &str,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    let mut visited = std::collections::HashSet::new();
    collect_type_members_recursive(workspace, qualified_type, &mut items, &mut visited);
    items
}

/// Recursively collect members from a type and its base classes
fn collect_type_members_recursive(
    workspace: &mut WorkspaceState,
    qualified_type: &str,
    items: &mut Vec<CompletionItem>,
    visited: &mut std::collections::HashSet<String>,
) {
    // Prevent infinite loops from circular inheritance
    if !visited.insert(qualified_type.to_string()) {
        return;
    }

    // Ensure the type's package is indexed
    workspace.ensure_package_indexed(qualified_type);

    let prefix_with_dot = format!("{}.", qualified_type);

    // Find all symbols that are direct children of this type
    for symbol in workspace.find_symbols("") {
        if symbol.qualified_name.starts_with(&prefix_with_dot) {
            let remainder = &symbol.qualified_name[prefix_with_dot.len()..];
            // Only show direct children (no more dots)
            if !remainder.contains('.') {
                let kind = match symbol.kind {
                    crate::lsp::workspace::SymbolKind::Function => CompletionItemKind::FUNCTION,
                    crate::lsp::workspace::SymbolKind::Parameter => CompletionItemKind::CONSTANT,
                    crate::lsp::workspace::SymbolKind::Constant => CompletionItemKind::CONSTANT,
                    crate::lsp::workspace::SymbolKind::Component => CompletionItemKind::FIELD,
                    _ => CompletionItemKind::FIELD,
                };

                items.push(CompletionItem {
                    label: remainder.to_string(),
                    kind: Some(kind),
                    detail: symbol.detail.clone(),
                    ..Default::default()
                });
            }
        }
    }

    // Collect extends information before releasing the borrow
    let extends_raw: Vec<String> = workspace
        .get_parsed_ast_by_name(qualified_type)
        .and_then(|ast| find_class_in_ast(ast, qualified_type))
        .map(|class_def| {
            class_def
                .extends
                .iter()
                .map(|ext| ext.comp.to_string())
                .collect()
        })
        .unwrap_or_default();

    // Follow inheritance chain - recursively get members from base classes
    // Try each candidate resolution for each extends clause (using canonical function)
    for ext_name in extends_raw {
        let candidates = resolve_type_candidates(qualified_type, &ext_name);
        for candidate in candidates {
            // Try to index this candidate
            workspace.ensure_package_indexed(&candidate);
            // Check if we found any symbols for this candidate
            let prefix = format!("{}.", candidate);
            let has_symbols = workspace
                .find_symbols("")
                .iter()
                .any(|s| s.qualified_name.starts_with(&prefix) || s.qualified_name == candidate);
            if has_symbols {
                collect_type_members_recursive(workspace, &candidate, items, visited);
                break; // Found the right resolution, don't try others
            }
        }
    }
}

// Note: find_class_in_ast, find_nested_class, resolve_type_candidates, and ImportResolver
// are now imported from crate::ir::transform::scope_resolver (canonical implementations)

/// Get completion items for all members of a class
pub fn get_class_member_completions(
    type_class: &crate::ir::ast::ClassDefinition,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    for (member_name, member) in &type_class.components {
        let kind = match member.variability {
            Variability::Parameter(_) => CompletionItemKind::CONSTANT,
            Variability::Constant(_) => CompletionItemKind::CONSTANT,
            _ => CompletionItemKind::FIELD,
        };

        let mut detail = format!("{}", member.type_name);
        if !member.shape.is_empty() {
            detail += &format!(
                "[{}]",
                member
                    .shape
                    .iter()
                    .map(|d| d.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }

        items.push(CompletionItem {
            label: member_name.clone(),
            kind: Some(kind),
            detail: Some(detail),
            documentation: if member.description.is_empty() {
                None
            } else {
                Some(lsp_types::Documentation::String(
                    member
                        .description
                        .iter()
                        .map(|t| t.text.trim_matches('"').to_string())
                        .collect::<Vec<_>>()
                        .join(" "),
                ))
            },
            ..Default::default()
        });
    }

    for (nested_name, nested_class) in &type_class.classes {
        let kind = match nested_class.class_type {
            ClassType::Function => CompletionItemKind::FUNCTION,
            _ => CompletionItemKind::CLASS,
        };
        items.push(CompletionItem {
            label: nested_name.clone(),
            kind: Some(kind),
            detail: Some(format!("{:?}", nested_class.class_type)),
            ..Default::default()
        });
    }

    items
}

/// Get attributes for built-in types
pub fn get_type_attributes(type_name: &str) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    let attrs: &[(&str, &str)] = match type_name {
        "Real" => &[
            ("start", "Initial value"),
            ("fixed", "Whether start is fixed"),
            ("min", "Minimum value"),
            ("max", "Maximum value"),
            ("unit", "Physical unit"),
            ("displayUnit", "Display unit"),
            ("nominal", "Nominal value"),
            ("stateSelect", "State selection hint"),
        ],
        "Integer" => &[
            ("start", "Initial value"),
            ("fixed", "Whether start is fixed"),
            ("min", "Minimum value"),
            ("max", "Maximum value"),
        ],
        "Boolean" => &[
            ("start", "Initial value"),
            ("fixed", "Whether start is fixed"),
        ],
        _ => &[],
    };

    for (name, doc) in attrs {
        items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some(doc.to_string()),
            ..Default::default()
        });
    }

    items
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::parse_source_simple;

    #[test]
    fn test_find_class_in_ast_by_simple_name() {
        // Simulates a file with "within" clause where the class is stored by simple name
        let code = r#"
            within Modelica.Blocks.Continuous;
            model PID
                Real x;
            end PID;
        "#;

        let ast = parse_source_simple(code, "PID.mo").expect("Failed to parse");

        // Should find PID when searching by qualified name
        let result = find_class_in_ast(&ast, "Modelica.Blocks.Continuous.PID");
        assert!(result.is_some(), "Should find PID by qualified name");
        assert!(result.unwrap().components.contains_key("x"));
    }

    #[test]
    fn test_find_class_in_ast_by_direct_name() {
        let code = r#"
            model TestModel
                Real y;
            end TestModel;
        "#;

        let ast = parse_source_simple(code, "test.mo").expect("Failed to parse");

        // Should find TestModel by direct name
        let result = find_class_in_ast(&ast, "TestModel");
        assert!(result.is_some(), "Should find TestModel by direct name");
        assert!(result.unwrap().components.contains_key("y"));
    }

    #[test]
    fn test_find_class_in_ast_nested() {
        let code = r#"
            package MyPackage
                model InnerModel
                    Real z;
                end InnerModel;
            end MyPackage;
        "#;

        let ast = parse_source_simple(code, "test.mo").expect("Failed to parse");

        // Should find nested class
        let result = find_class_in_ast(&ast, "MyPackage.InnerModel");
        assert!(result.is_some(), "Should find nested InnerModel");
        assert!(result.unwrap().components.contains_key("z"));
    }

    #[test]
    fn test_find_class_in_ast_with_within_and_nested() {
        // Test finding a nested class when the file has a "within" clause
        // This is common in MSL where packages contain nested classes
        let code = r#"
            within MyLib;
            package Controllers
                block PID
                    parameter Real K = 1.0;
                end PID;
            end Controllers;
        "#;

        let ast = parse_source_simple(code, "Controllers.mo").expect("Failed to parse");

        // Should find PID using the full qualified name including the within prefix
        let result = find_class_in_ast(&ast, "MyLib.Controllers.PID");
        assert!(
            result.is_some(),
            "Should find PID via within-prefixed qualified name"
        );
        assert!(result.unwrap().components.contains_key("K"));

        // Should also find the package itself
        let result = find_class_in_ast(&ast, "MyLib.Controllers");
        assert!(result.is_some(), "Should find Controllers package");
    }

    #[test]
    fn test_import_resolver() {
        let code = r#"
            model Test
                import Modelica.Blocks.Continuous.PID;
                import SI = Modelica.Units.SI;
                import Modelica.Constants.{pi, e};
            end Test;
        "#;

        let ast = parse_source_simple(code, "test.mo").expect("Failed to parse");
        let class = ast.class_list.get("Test").expect("Test class not found");

        let resolver = ImportResolver::from_imports(&class.imports);

        // Qualified import: PID -> Modelica.Blocks.Continuous.PID
        assert_eq!(
            resolver.resolve("PID"),
            Some("Modelica.Blocks.Continuous.PID")
        );

        // Renamed import: SI -> Modelica.Units.SI
        assert_eq!(resolver.resolve("SI"), Some("Modelica.Units.SI"));

        // Selective import: pi -> Modelica.Constants.pi
        assert_eq!(resolver.resolve("pi"), Some("Modelica.Constants.pi"));
        assert_eq!(resolver.resolve("e"), Some("Modelica.Constants.e"));
    }

    #[test]
    fn test_get_class_member_completions() {
        let code = r#"
            model TestModel
                parameter Real K = 1.0 "Gain";
                Real x "State variable";
                constant Integer N = 10;
            end TestModel;
        "#;

        let ast = parse_source_simple(code, "test.mo").expect("Failed to parse");
        let class = ast
            .class_list
            .get("TestModel")
            .expect("TestModel not found");

        let completions = get_class_member_completions(class);

        // Check that all components are in completions
        let labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        assert!(labels.contains(&"K"), "Should contain parameter K");
        assert!(labels.contains(&"x"), "Should contain variable x");
        assert!(labels.contains(&"N"), "Should contain constant N");

        // Check that parameters/constants have CONSTANT kind
        let k_item = completions.iter().find(|c| c.label == "K").unwrap();
        assert_eq!(k_item.kind, Some(CompletionItemKind::CONSTANT));

        // Check that regular variables have FIELD kind
        let x_item = completions.iter().find(|c| c.label == "x").unwrap();
        assert_eq!(x_item.kind, Some(CompletionItemKind::FIELD));
    }

    #[test]
    fn test_resolve_type_candidates() {
        // Test relative name with dots (like Interfaces.SISO from PID)
        // Now uses canonical function from scope_resolver
        let candidates =
            resolve_type_candidates("Modelica.Blocks.Continuous.PID", "Interfaces.SISO");
        // Should try: Modelica.Blocks.Continuous.Interfaces.SISO,
        //             Modelica.Blocks.Interfaces.SISO,
        //             Modelica.Interfaces.SISO,
        //             Interfaces.SISO
        assert_eq!(candidates.len(), 4);
        assert_eq!(candidates[0], "Modelica.Blocks.Continuous.Interfaces.SISO");
        assert_eq!(candidates[1], "Modelica.Blocks.Interfaces.SISO");
        assert_eq!(candidates[2], "Modelica.Interfaces.SISO");
        assert_eq!(candidates[3], "Interfaces.SISO");

        // Test simple name resolution
        let candidates = resolve_type_candidates("Modelica.Blocks.Continuous.PID", "SISO");
        assert_eq!(candidates.len(), 4);
        assert_eq!(candidates[0], "Modelica.Blocks.Continuous.SISO");
        assert_eq!(candidates[1], "Modelica.Blocks.SISO");
        assert_eq!(candidates[2], "Modelica.SISO");
        assert_eq!(candidates[3], "SISO");

        // Test with single-part current name
        let candidates = resolve_type_candidates("PID", "SISO");
        assert_eq!(candidates.len(), 1);
        assert_eq!(candidates[0], "SISO");
    }
}
