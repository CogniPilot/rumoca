//! Code completion handler for Modelica files.
//!
//! Provides:
//! - Local completions (variables, types in current file)
//! - Workspace-wide completions (symbols from all open files)
//! - Package/import completions

// Allow mutable key type warning - Uri has interior mutability but we use it correctly
#![allow(clippy::mutable_key_type)]

use std::collections::HashMap;

use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse, InsertTextFormat,
    Position, Uri,
};

use crate::ir::ast::{Causality, ClassType, StoredDefinition, Variability};

use super::builtin_functions::get_builtin_functions;
use super::utils::{get_text_before_cursor, parse_document};
use super::workspace::{SymbolKind, WorkspaceState};

/// Handle completion request with workspace support
///
/// Provides completions from:
/// - Local scope (variables, types in current file)
/// - Workspace symbols (classes, models from other files)
/// - Package/import completions
/// - Built-in functions and keywords
pub fn handle_completion_workspace(
    workspace: &WorkspaceState,
    params: CompletionParams,
) -> Option<CompletionResponse> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;
    let text = workspace.get_document(uri)?;
    let path = uri.path().as_str();

    let mut items = Vec::new();

    // Check if we're doing dot completion or import completion
    let text_before = get_text_before_cursor(text, position)?;
    let is_dot_completion = text_before.ends_with('.');
    let is_import_context = is_in_import_context(&text_before);

    if is_dot_completion {
        let before_dot = &text_before[..text_before.len() - 1];
        let prefix: String = before_dot
            .chars()
            .rev()
            .take_while(|c| c.is_alphanumeric() || *c == '_' || *c == '.')
            .collect::<String>()
            .chars()
            .rev()
            .collect();

        if !prefix.is_empty() {
            // Try local AST first
            if let Some(ast) = parse_document(text, path) {
                items.extend(get_member_completions(&ast, &format!("{}.", prefix), position));
            }

            // Also get workspace completions for qualified names
            items.extend(get_workspace_member_completions(workspace, &prefix));
        }

        if items.is_empty() {
            return Some(CompletionResponse::Array(vec![]));
        }

        return Some(CompletionResponse::Array(items));
    }

    // Get scoped completions from the AST
    if let Some(ast) = parse_document(text, path) {
        items.extend(get_scoped_completions(&ast, position));
    }

    // Add workspace symbols (classes, models from other files)
    items.extend(get_workspace_completions(workspace, is_import_context));

    // Add built-in functions with snippets
    let functions = get_builtin_functions();
    for func in &functions {
        let snippet = if func.parameters.is_empty() {
            format!("{}()", func.name)
        } else {
            let params: Vec<String> = func
                .parameters
                .iter()
                .enumerate()
                .map(|(i, (name, _))| format!("${{{}:{}}}", i + 1, name))
                .collect();
            format!("{}({})", func.name, params.join(", "))
        };

        items.push(CompletionItem {
            label: func.name.to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some(func.signature.to_string()),
            documentation: Some(lsp_types::Documentation::String(
                func.documentation.to_string(),
            )),
            insert_text: Some(snippet),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        });
    }

    // Modelica keywords
    items.extend(get_keyword_completions());

    Some(CompletionResponse::Array(items))
}

/// Handle completion request (non-workspace version for backwards compatibility)
pub fn handle_completion(
    documents: &HashMap<Uri, String>,
    params: CompletionParams,
) -> Option<CompletionResponse> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;
    let text = documents.get(uri)?;
    let path = uri.path().as_str();

    let mut items = Vec::new();

    // Check if we're doing dot completion
    let text_before = get_text_before_cursor(text, position)?;
    let is_dot_completion = text_before.ends_with('.');

    if is_dot_completion {
        let before_dot = &text_before[..text_before.len() - 1];
        let prefix: String = before_dot
            .chars()
            .rev()
            .take_while(|c| c.is_alphanumeric() || *c == '_' || *c == '.')
            .collect::<String>()
            .chars()
            .rev()
            .collect();

        if !prefix.is_empty() {
            if let Some(ast) = parse_document(text, path) {
                items.extend(get_member_completions(&ast, &format!("{}.", prefix), position));
            }
        }

        if items.is_empty() {
            return Some(CompletionResponse::Array(vec![]));
        }

        return Some(CompletionResponse::Array(items));
    }

    // Get scoped completions from the AST
    if let Some(ast) = parse_document(text, path) {
        items.extend(get_scoped_completions(&ast, position));
    }

    // Add built-in functions with snippets
    let functions = get_builtin_functions();
    for func in &functions {
        let snippet = if func.parameters.is_empty() {
            format!("{}()", func.name)
        } else {
            let params: Vec<String> = func
                .parameters
                .iter()
                .enumerate()
                .map(|(i, (name, _))| format!("${{{}:{}}}", i + 1, name))
                .collect();
            format!("{}({})", func.name, params.join(", "))
        };

        items.push(CompletionItem {
            label: func.name.to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some(func.signature.to_string()),
            documentation: Some(lsp_types::Documentation::String(
                func.documentation.to_string(),
            )),
            insert_text: Some(snippet),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        });
    }

    // Modelica keywords
    items.extend(get_keyword_completions());

    Some(CompletionResponse::Array(items))
}

/// Get completions for component members (dot completion)
fn get_member_completions(
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

            if let Some(type_class) = ast.class_list.get(&type_name) {
                for (member_name, member) in &type_class.components {
                    let kind = match member.variability {
                        Variability::Parameter(_) => CompletionItemKind::CONSTANT,
                        Variability::Constant(_) => CompletionItemKind::CONSTANT,
                        _ => CompletionItemKind::FIELD,
                    };
                    items.push(CompletionItem {
                        label: member_name.clone(),
                        kind: Some(kind),
                        detail: Some(format!("{}: {}", member_name, member.type_name)),
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
            }

            // Built-in type attributes
            items.extend(get_type_attributes(&type_name));
        }

        for nested_class in class.classes.values() {
            if let Some(comp) = nested_class.components.get(component_name) {
                let type_name = comp.type_name.to_string();
                if let Some(type_class) = class.classes.get(&type_name) {
                    for (member_name, member) in &type_class.components {
                        items.push(CompletionItem {
                            label: member_name.clone(),
                            kind: Some(CompletionItemKind::FIELD),
                            detail: Some(format!("{}: {}", member_name, member.type_name)),
                            ..Default::default()
                        });
                    }
                }
            }
        }
    }

    items
}

/// Get attributes for built-in types
fn get_type_attributes(type_name: &str) -> Vec<CompletionItem> {
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

/// Get completions from the current scope
fn get_scoped_completions(ast: &StoredDefinition, position: Position) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    for class in ast.class_list.values() {
        let class_start = class.name.location.start_line;
        let pos_line = position.line + 1;

        if pos_line >= class_start {
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
        }
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

/// Check if the cursor is in an import statement context
fn is_in_import_context(text_before: &str) -> bool {
    // Look for "import" keyword before the cursor (on the same line or recent context)
    let lines: Vec<&str> = text_before.lines().collect();
    if let Some(last_line) = lines.last() {
        let trimmed = last_line.trim();
        return trimmed.starts_with("import ") || trimmed == "import";
    }
    false
}

/// Get completions from workspace symbols
fn get_workspace_completions(workspace: &WorkspaceState, is_import_context: bool) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    let mut seen = std::collections::HashSet::new();

    // Get all workspace symbols
    for symbol in workspace.find_symbols("") {
        // Skip if we've already added this symbol
        if !seen.insert(&symbol.qualified_name) {
            continue;
        }

        let kind = match symbol.kind {
            SymbolKind::Package => CompletionItemKind::MODULE,
            SymbolKind::Model => CompletionItemKind::CLASS,
            SymbolKind::Class => CompletionItemKind::CLASS,
            SymbolKind::Block => CompletionItemKind::CLASS,
            SymbolKind::Connector => CompletionItemKind::INTERFACE,
            SymbolKind::Record => CompletionItemKind::STRUCT,
            SymbolKind::Type => CompletionItemKind::TYPE_PARAMETER,
            SymbolKind::Function => CompletionItemKind::FUNCTION,
            SymbolKind::Operator => CompletionItemKind::OPERATOR,
            SymbolKind::Component => CompletionItemKind::FIELD,
            SymbolKind::Parameter => CompletionItemKind::CONSTANT,
            SymbolKind::Constant => CompletionItemKind::CONSTANT,
        };

        // For import context, prefer showing qualified names
        // For other contexts, show simple names with qualified name in detail
        let simple_name = symbol
            .qualified_name
            .rsplit('.')
            .next()
            .unwrap_or(&symbol.qualified_name);

        if is_import_context {
            // In import context, show full qualified name
            items.push(CompletionItem {
                label: symbol.qualified_name.clone(),
                kind: Some(kind),
                detail: symbol.detail.clone(),
                ..Default::default()
            });
        } else {
            // In normal context, show simple name with qualified in detail
            items.push(CompletionItem {
                label: simple_name.to_string(),
                kind: Some(kind),
                detail: Some(symbol.qualified_name.clone()),
                filter_text: Some(format!("{} {}", simple_name, symbol.qualified_name)),
                ..Default::default()
            });
        }
    }

    items
}

/// Get member completions from workspace symbols for dot completion
fn get_workspace_member_completions(workspace: &WorkspaceState, prefix: &str) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    let prefix_with_dot = format!("{}.", prefix);

    // Find all symbols that start with the prefix
    for symbol in workspace.find_symbols("") {
        if symbol.qualified_name.starts_with(&prefix_with_dot) {
            // Get the part after the prefix
            let remainder = &symbol.qualified_name[prefix_with_dot.len()..];

            // Only show direct children (no more dots in remainder)
            if !remainder.contains('.') {
                let kind = match symbol.kind {
                    SymbolKind::Package => CompletionItemKind::MODULE,
                    SymbolKind::Model => CompletionItemKind::CLASS,
                    SymbolKind::Class => CompletionItemKind::CLASS,
                    SymbolKind::Block => CompletionItemKind::CLASS,
                    SymbolKind::Connector => CompletionItemKind::INTERFACE,
                    SymbolKind::Record => CompletionItemKind::STRUCT,
                    SymbolKind::Type => CompletionItemKind::TYPE_PARAMETER,
                    SymbolKind::Function => CompletionItemKind::FUNCTION,
                    SymbolKind::Operator => CompletionItemKind::OPERATOR,
                    SymbolKind::Component => CompletionItemKind::FIELD,
                    SymbolKind::Parameter => CompletionItemKind::CONSTANT,
                    SymbolKind::Constant => CompletionItemKind::CONSTANT,
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

    items
}

/// Get keyword completions
fn get_keyword_completions() -> Vec<CompletionItem> {
    let keywords = vec![
        ("model", "model declaration", CompletionItemKind::KEYWORD),
        ("class", "class declaration", CompletionItemKind::KEYWORD),
        ("connector", "connector declaration", CompletionItemKind::KEYWORD),
        ("package", "package declaration", CompletionItemKind::KEYWORD),
        ("function", "function declaration", CompletionItemKind::KEYWORD),
        ("record", "record declaration", CompletionItemKind::KEYWORD),
        ("block", "block declaration", CompletionItemKind::KEYWORD),
        ("type", "type declaration", CompletionItemKind::KEYWORD),
        ("parameter", "parameter variable", CompletionItemKind::KEYWORD),
        ("constant", "constant variable", CompletionItemKind::KEYWORD),
        ("input", "input connector", CompletionItemKind::KEYWORD),
        ("output", "output connector", CompletionItemKind::KEYWORD),
        ("flow", "flow variable", CompletionItemKind::KEYWORD),
        ("stream", "stream variable", CompletionItemKind::KEYWORD),
        ("discrete", "discrete variable", CompletionItemKind::KEYWORD),
        ("Real", "Real number type", CompletionItemKind::TYPE_PARAMETER),
        ("Integer", "Integer type", CompletionItemKind::TYPE_PARAMETER),
        ("Boolean", "Boolean type", CompletionItemKind::TYPE_PARAMETER),
        ("String", "String type", CompletionItemKind::TYPE_PARAMETER),
        ("extends", "inheritance", CompletionItemKind::KEYWORD),
        ("import", "import statement", CompletionItemKind::KEYWORD),
        ("within", "within statement", CompletionItemKind::KEYWORD),
        ("equation", "equation section", CompletionItemKind::KEYWORD),
        ("algorithm", "algorithm section", CompletionItemKind::KEYWORD),
        ("initial equation", "initial equation section", CompletionItemKind::KEYWORD),
        ("initial algorithm", "initial algorithm section", CompletionItemKind::KEYWORD),
        ("protected", "protected section", CompletionItemKind::KEYWORD),
        ("public", "public section", CompletionItemKind::KEYWORD),
        ("final", "final modifier", CompletionItemKind::KEYWORD),
        ("partial", "partial class", CompletionItemKind::KEYWORD),
        ("replaceable", "replaceable element", CompletionItemKind::KEYWORD),
        ("redeclare", "redeclare element", CompletionItemKind::KEYWORD),
        ("inner", "inner element", CompletionItemKind::KEYWORD),
        ("outer", "outer element", CompletionItemKind::KEYWORD),
        ("encapsulated", "encapsulated class", CompletionItemKind::KEYWORD),
        ("annotation", "annotation", CompletionItemKind::KEYWORD),
        ("if", "if statement", CompletionItemKind::KEYWORD),
        ("then", "then clause", CompletionItemKind::KEYWORD),
        ("else", "else clause", CompletionItemKind::KEYWORD),
        ("elseif", "elseif clause", CompletionItemKind::KEYWORD),
        ("for", "for loop", CompletionItemKind::KEYWORD),
        ("loop", "loop keyword", CompletionItemKind::KEYWORD),
        ("while", "while loop", CompletionItemKind::KEYWORD),
        ("when", "when statement", CompletionItemKind::KEYWORD),
        ("end", "end keyword", CompletionItemKind::KEYWORD),
        ("time", "simulation time", CompletionItemKind::VARIABLE),
        ("true", "boolean true", CompletionItemKind::VALUE),
        ("false", "boolean false", CompletionItemKind::VALUE),
    ];

    keywords
        .into_iter()
        .map(|(label, detail, kind)| CompletionItem {
            label: label.to_string(),
            kind: Some(kind),
            detail: Some(detail.to_string()),
            ..Default::default()
        })
        .collect()
}
