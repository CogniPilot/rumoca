//! Scoped completion handling.
//!
//! Provides completions from the current scope (local variables, types, imports).
//!
//! This module uses canonical scope resolution functions from
//! `crate::ir::transform::scope_resolver` to avoid duplication.

use lsp_types::{CompletionItem, CompletionItemKind, InsertTextFormat, Position};

use crate::ir::ast::{
    Causality, ClassDefinition, ClassType, Import, StoredDefinition, Variability,
};
use crate::ir::transform::scope_resolver::ScopeResolver;

/// Get completions from the current scope
pub fn get_scoped_completions(ast: &StoredDefinition, position: Position) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // Use canonical ScopeResolver for position-based scope detection
    let resolver = ScopeResolver::new(ast);
    if let Some(class) = resolver.class_at_0indexed(position.line, position.character) {
        // Add completions from the class containing the cursor
        add_class_completions(class, &mut items);
    }

    // Add top-level classes
    for (class_name, class_def) in &ast.class_list {
        let kind = match class_def.class_type {
            ClassType::Model => CompletionItemKind::CLASS,
            ClassType::Function => CompletionItemKind::FUNCTION,
            ClassType::Record => CompletionItemKind::STRUCT,
            ClassType::Type => CompletionItemKind::TYPE_PARAMETER,
            ClassType::Connector => CompletionItemKind::INTERFACE,
            ClassType::Package => CompletionItemKind::MODULE,
            ClassType::Block => CompletionItemKind::CLASS,
            _ => CompletionItemKind::CLASS,
        };

        items.push(CompletionItem {
            label: class_name.clone(),
            kind: Some(kind),
            detail: Some(format!("{:?}", class_def.class_type)),
            ..Default::default()
        });
    }

    items
}

/// Add completions from a class (components, nested classes, imports)
fn add_class_completions(class: &ClassDefinition, items: &mut Vec<CompletionItem>) {
    // Add components
    for (comp_name, comp) in &class.components {
        let kind = match (&comp.variability, &comp.causality) {
            (Variability::Parameter(_), _) => CompletionItemKind::CONSTANT,
            (Variability::Constant(_), _) => CompletionItemKind::CONSTANT,
            (_, Causality::Input(_)) => CompletionItemKind::VARIABLE,
            (_, Causality::Output(_)) => CompletionItemKind::VARIABLE,
            _ => CompletionItemKind::VARIABLE,
        };

        let mut detail = comp.type_name.to_string();
        if !comp.shape.is_empty() {
            detail += &format!(
                "[{}]",
                comp.shape
                    .iter()
                    .map(|d| d.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }

        items.push(CompletionItem {
            label: comp_name.clone(),
            kind: Some(kind),
            detail: Some(detail),
            documentation: if comp.description.is_empty() {
                None
            } else {
                Some(lsp_types::Documentation::String(
                    comp.description
                        .iter()
                        .map(|t| t.text.trim_matches('"').to_string())
                        .collect::<Vec<_>>()
                        .join(" "),
                ))
            },
            ..Default::default()
        });
    }

    // Add nested classes
    for (nested_name, nested_class) in &class.classes {
        let kind = match nested_class.class_type {
            ClassType::Function => CompletionItemKind::FUNCTION,
            ClassType::Record => CompletionItemKind::STRUCT,
            ClassType::Type => CompletionItemKind::TYPE_PARAMETER,
            ClassType::Connector => CompletionItemKind::INTERFACE,
            ClassType::Package => CompletionItemKind::MODULE,
            _ => CompletionItemKind::CLASS,
        };

        let insert_text = if nested_class.class_type == ClassType::Function {
            Some(format!("{}($0)", nested_name))
        } else {
            None
        };

        items.push(CompletionItem {
            label: nested_name.clone(),
            kind: Some(kind),
            detail: Some(format!("{:?}", nested_class.class_type)),
            insert_text,
            insert_text_format: if nested_class.class_type == ClassType::Function {
                Some(InsertTextFormat::SNIPPET)
            } else {
                None
            },
            ..Default::default()
        });
    }

    // Add imported types to completions
    for import in &class.imports {
        match import {
            Import::Qualified { path, .. } => {
                // import A.B.C; -> C is available as a short name
                if let Some(last) = path.name.last() {
                    items.push(CompletionItem {
                        label: last.text.clone(),
                        kind: Some(CompletionItemKind::CLASS),
                        detail: Some(format!("import {}", path)),
                        ..Default::default()
                    });
                }
            }
            Import::Renamed { alias, path, .. } => {
                // import D = A.B.C; -> D is available as a short name
                items.push(CompletionItem {
                    label: alias.text.clone(),
                    kind: Some(CompletionItemKind::CLASS),
                    detail: Some(format!("import {} = {}", alias.text, path)),
                    ..Default::default()
                });
            }
            Import::Selective { path, names, .. } => {
                // import A.B.{C, D}; -> C and D are available as short names
                for name_token in names {
                    items.push(CompletionItem {
                        label: name_token.text.clone(),
                        kind: Some(CompletionItemKind::CLASS),
                        detail: Some(format!("import {}.{}", path, name_token.text)),
                        ..Default::default()
                    });
                }
            }
            Import::Unqualified { path, .. } => {
                // import A.B.*; -> We can't know all names, but we can suggest the package
                items.push(CompletionItem {
                    label: format!("{}.*", path),
                    kind: Some(CompletionItemKind::MODULE),
                    detail: Some(format!("import {}.*", path)),
                    ..Default::default()
                });
            }
        }
    }
}
