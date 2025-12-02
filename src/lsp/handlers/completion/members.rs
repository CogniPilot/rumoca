//! Member completion handling for dot completion.
//!
//! Provides completions for component members when typing after a dot.

use lsp_types::{CompletionItem, CompletionItemKind, Position};

use crate::ir::ast::{ClassType, StoredDefinition, Variability};

/// Get completions for component members (dot completion)
pub fn get_member_completions(
    ast: &StoredDefinition,
    prefix: &str,
    _position: Position,
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

            // Try to find the type class - handle qualified names like "test.BouncingBall"
            if let Some(type_class) = find_class_by_name(ast, &type_name) {
                items.extend(get_class_member_completions(type_class));
            }

            // Built-in type attributes
            items.extend(get_type_attributes(&type_name));
        }

        // Also check nested classes for the component
        for nested_class in class.classes.values() {
            if let Some(comp) = nested_class.components.get(component_name) {
                let type_name = comp.type_name.to_string();
                if let Some(type_class) = find_class_by_name(ast, &type_name) {
                    items.extend(get_class_member_completions(type_class));
                }
            }
        }
    }

    items
}

/// Find a class by name, handling qualified names like "package.Model"
pub fn find_class_by_name<'a>(
    ast: &'a StoredDefinition,
    type_name: &str,
) -> Option<&'a crate::ir::ast::ClassDefinition> {
    // First try direct lookup
    if let Some(class) = ast.class_list.get(type_name) {
        return Some(class);
    }

    // Handle qualified names like "test.BouncingBall"
    let parts: Vec<&str> = type_name.split('.').collect();
    if parts.len() >= 2 {
        // Look for the first part as a top-level class/package
        if let Some(parent) = ast.class_list.get(parts[0]) {
            return find_nested_class(parent, &parts[1..]);
        }
    }

    None
}

/// Find a nested class by path
fn find_nested_class<'a>(
    parent: &'a crate::ir::ast::ClassDefinition,
    path: &[&str],
) -> Option<&'a crate::ir::ast::ClassDefinition> {
    if path.is_empty() {
        return Some(parent);
    }

    if let Some(child) = parent.classes.get(path[0]) {
        if path.len() == 1 {
            return Some(child);
        }
        return find_nested_class(child, &path[1..]);
    }

    None
}

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
