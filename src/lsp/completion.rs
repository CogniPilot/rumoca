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
/// - Modifier completions (start, fixed, min, max, etc.)
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

    // Check if we're in a modifier context (inside parentheses after type declaration)
    // Try parsing for class lookup (for class instance member modifiers)
    let ast_for_modifiers =
        parse_document(text, path).or_else(|| workspace.get_cached_ast(uri).cloned());
    if let Some(modifier_items) = get_modifier_completions(&text_before, ast_for_modifiers.as_ref())
    {
        return Some(CompletionResponse::Array(modifier_items));
    }

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
            // Try local AST first - this handles component member access (e.g., ball.h)
            // First try parsing current text, then fall back to cached AST
            let ast_option = parse_document(text, path).or_else(|| {
                // Current parse failed (syntax error while typing), use cached AST
                workspace.get_cached_ast(uri).cloned()
            });

            if let Some(ast) = ast_option {
                items.extend(get_member_completions(
                    &ast,
                    &format!("{}.", prefix),
                    position,
                ));
            }

            // Only get workspace package completions if we didn't find local members
            // This handles package navigation (e.g., Modelica.Math.) but not
            // local variable member access (e.g., ball.)
            if items.is_empty() {
                items.extend(get_workspace_member_completions(workspace, &prefix));
            }
        }

        // For dot completion, only return the member items (no keywords/functions)
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

    // Check if we're in a modifier context (inside parentheses after type declaration)
    let ast_for_modifiers = parse_document(text, path);
    if let Some(modifier_items) = get_modifier_completions(&text_before, ast_for_modifiers.as_ref())
    {
        return Some(CompletionResponse::Array(modifier_items));
    }

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
                items.extend(get_member_completions(
                    &ast,
                    &format!("{}.", prefix),
                    position,
                ));
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
fn find_class_by_name<'a>(
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
fn get_class_member_completions(
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
fn get_workspace_completions(
    workspace: &WorkspaceState,
    is_import_context: bool,
) -> Vec<CompletionItem> {
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
fn get_workspace_member_completions(
    workspace: &WorkspaceState,
    prefix: &str,
) -> Vec<CompletionItem> {
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
        (
            "connector",
            "connector declaration",
            CompletionItemKind::KEYWORD,
        ),
        (
            "package",
            "package declaration",
            CompletionItemKind::KEYWORD,
        ),
        (
            "function",
            "function declaration",
            CompletionItemKind::KEYWORD,
        ),
        ("record", "record declaration", CompletionItemKind::KEYWORD),
        ("block", "block declaration", CompletionItemKind::KEYWORD),
        ("type", "type declaration", CompletionItemKind::KEYWORD),
        (
            "parameter",
            "parameter variable",
            CompletionItemKind::KEYWORD,
        ),
        ("constant", "constant variable", CompletionItemKind::KEYWORD),
        ("input", "input connector", CompletionItemKind::KEYWORD),
        ("output", "output connector", CompletionItemKind::KEYWORD),
        ("flow", "flow variable", CompletionItemKind::KEYWORD),
        ("stream", "stream variable", CompletionItemKind::KEYWORD),
        ("discrete", "discrete variable", CompletionItemKind::KEYWORD),
        (
            "Real",
            "Real number type",
            CompletionItemKind::TYPE_PARAMETER,
        ),
        (
            "Integer",
            "Integer type",
            CompletionItemKind::TYPE_PARAMETER,
        ),
        (
            "Boolean",
            "Boolean type",
            CompletionItemKind::TYPE_PARAMETER,
        ),
        ("String", "String type", CompletionItemKind::TYPE_PARAMETER),
        ("extends", "inheritance", CompletionItemKind::KEYWORD),
        ("import", "import statement", CompletionItemKind::KEYWORD),
        ("within", "within statement", CompletionItemKind::KEYWORD),
        ("equation", "equation section", CompletionItemKind::KEYWORD),
        (
            "algorithm",
            "algorithm section",
            CompletionItemKind::KEYWORD,
        ),
        (
            "initial equation",
            "initial equation section",
            CompletionItemKind::KEYWORD,
        ),
        (
            "initial algorithm",
            "initial algorithm section",
            CompletionItemKind::KEYWORD,
        ),
        (
            "protected",
            "protected section",
            CompletionItemKind::KEYWORD,
        ),
        ("public", "public section", CompletionItemKind::KEYWORD),
        ("final", "final modifier", CompletionItemKind::KEYWORD),
        ("partial", "partial class", CompletionItemKind::KEYWORD),
        (
            "replaceable",
            "replaceable element",
            CompletionItemKind::KEYWORD,
        ),
        (
            "redeclare",
            "redeclare element",
            CompletionItemKind::KEYWORD,
        ),
        ("inner", "inner element", CompletionItemKind::KEYWORD),
        ("outer", "outer element", CompletionItemKind::KEYWORD),
        (
            "encapsulated",
            "encapsulated class",
            CompletionItemKind::KEYWORD,
        ),
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

/// Check if we're in a modifier context and return appropriate completions
///
/// Detects patterns like:
/// - `Real x(` - just opened paren (trigger: '(')
/// - `Real x(start=1,` - after comma (trigger: ',')
/// - `Real x(start=1, ` - after comma with space
/// - `Real x(st` - typing a modifier name
/// - `test.BouncingBall ball(` - class instance with member modifiers
fn get_modifier_completions(
    text_before: &str,
    ast: Option<&StoredDefinition>,
) -> Option<Vec<CompletionItem>> {
    // Find the last unmatched opening parenthesis
    let mut paren_depth = 0;
    let mut last_open_paren_pos = None;

    for (i, c) in text_before.char_indices() {
        match c {
            '(' => {
                paren_depth += 1;
                last_open_paren_pos = Some(i);
            }
            ')' => {
                paren_depth -= 1;
                if paren_depth <= 0 {
                    last_open_paren_pos = None;
                    paren_depth = 0;
                }
            }
            _ => {}
        }
    }

    // If we're not inside parentheses, no modifier completions
    let open_pos = last_open_paren_pos?;

    // Check if this looks like a modifier context (Type name( pattern)
    let before_paren = &text_before[..open_pos];
    let type_name = extract_type_from_modifier_context(before_paren)?;

    // Get what's after the opening paren
    let after_paren = &text_before[open_pos + 1..];

    // Determine what position we're at within the modifier list
    // Find the last comma to see what we're currently typing
    let last_comma_pos = after_paren.rfind(',');

    let current_part = match last_comma_pos {
        Some(pos) => &after_paren[pos + 1..],
        None => after_paren,
    };

    let current_trimmed = current_part.trim();

    // Show modifier completions if:
    // 1. Just after '(' - empty after paren
    // 2. Just after ',' - current part is empty or whitespace only
    // 3. Typing a modifier name - no '=' in current part yet
    // 4. After a complete modifier value - ends with a value (not '=')
    let should_show = after_paren.is_empty()                           // Just typed '('
        || current_trimmed.is_empty()                                   // Just typed ',' (with optional space)
        || !current_trimmed.contains('=')                               // Typing modifier name
        || (current_trimmed.contains('=') && {                          // After modifier value
            // Check we're not in the middle of typing the value
            let after_eq = current_trimmed.split('=').next_back().unwrap_or("").trim();
            !after_eq.is_empty() && text_before.ends_with(' ')
        });

    if should_show {
        let mut items = Vec::new();

        // Check if the type is a primitive type - if so, add standard modifiers
        if is_primitive_type(&type_name) {
            items.extend(get_modifier_items());
        } else if let Some(ast) = ast {
            // For class types, add member overrides from the class definition
            if let Some(type_class) = find_class_by_name(ast, &type_name) {
                items.extend(get_class_modifier_completions(type_class));
            }
            // Also add standard modifiers that apply to any component (like each, redeclare, final)
            items.extend(get_general_modifier_items());
        }

        if !items.is_empty() { Some(items) } else { None }
    } else {
        None
    }
}

/// Check if a type name is a primitive/built-in type
fn is_primitive_type(type_name: &str) -> bool {
    matches!(
        type_name,
        "Real" | "Integer" | "Boolean" | "String" | "StateSelect" | "ExternalObject"
    )
}

/// Extract the type name from a modifier context
/// e.g., "Real x" -> "Real", "test.BouncingBall ball" -> "test.BouncingBall"
fn extract_type_from_modifier_context(before_paren: &str) -> Option<String> {
    let trimmed = before_paren.trim_end();

    // Must have at least a type and a name
    let parts: Vec<&str> = trimmed.split_whitespace().collect();
    if parts.len() < 2 {
        return None;
    }

    // Look for the type in the parts (skipping modifiers like parameter, constant, etc.)
    let modifiers = [
        "parameter",
        "constant",
        "input",
        "output",
        "flow",
        "stream",
        "discrete",
        "final",
        "replaceable",
        "redeclare",
        "inner",
        "outer",
    ];

    for (i, part) in parts.iter().enumerate() {
        // Skip known modifiers
        if modifiers.contains(part) {
            continue;
        }

        // Check if this looks like a type (followed by a variable name)
        if i + 1 < parts.len() {
            let next_part = parts[i + 1];
            // Type followed by variable name pattern
            // Handle array types like "Real[3]"
            let base_type = if let Some(bracket_pos) = part.find('[') {
                &part[..bracket_pos]
            } else {
                part
            };

            // Check if it looks like a type (starts with uppercase or is qualified like pkg.Type)
            let is_type = base_type.chars().next().is_some_and(|c| c.is_uppercase())
                || base_type.contains('.');

            // Check if next part looks like a variable name (starts with lowercase or underscore)
            let is_var_name = next_part
                .chars()
                .next()
                .is_some_and(|c| c.is_lowercase() || c == '_');

            if is_type && is_var_name {
                // Handle array types - return base type without dimensions
                return Some(base_type.to_string());
            }
        }
    }

    None
}

/// Get completion items for class member modifiers (for class instance modifications)
fn get_class_modifier_completions(
    type_class: &crate::ir::ast::ClassDefinition,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    for (member_name, member) in &type_class.components {
        // Create a snippet for the member modification
        let default_value = match member.type_name.to_string().as_str() {
            "Real" => "0.0",
            "Integer" => "0",
            "Boolean" => "false",
            "String" => "\"\"",
            _ => "...",
        };

        let snippet = format!("{} = ${{1:{}}}", member_name, default_value);

        let kind = match member.variability {
            Variability::Parameter(_) => CompletionItemKind::CONSTANT,
            Variability::Constant(_) => CompletionItemKind::CONSTANT,
            _ => CompletionItemKind::FIELD,
        };

        let mut detail = member.type_name.to_string();
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
            insert_text: Some(snippet),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        });
    }

    items
}

/// Get general modifier items that apply to any component type
fn get_general_modifier_items() -> Vec<CompletionItem> {
    let modifiers = [
        ("each", "Apply modifier to each element", "each "),
        ("redeclare", "Redeclare a replaceable element", "redeclare "),
        ("final", "Prevent further modification", "final "),
    ];

    modifiers
        .into_iter()
        .map(|(label, detail, snippet)| CompletionItem {
            label: label.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some(detail.to_string()),
            insert_text: Some(snippet.to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        })
        .collect()
}

/// Get completion items for modifiers
fn get_modifier_items() -> Vec<CompletionItem> {
    let modifiers = [
        // Common modifiers for Real
        (
            "start",
            "Initial value for the variable",
            "start = ${1:0.0}",
        ),
        (
            "fixed",
            "Whether start value is fixed (default: false for states, true for parameters)",
            "fixed = ${1|true,false|}",
        ),
        ("min", "Minimum value constraint", "min = ${1:-1e10}"),
        ("max", "Maximum value constraint", "max = ${1:1e10}"),
        ("nominal", "Nominal value for scaling", "nominal = ${1:1.0}"),
        ("unit", "Physical unit (SI)", "unit = \"${1:}\""),
        (
            "displayUnit",
            "Display unit for GUI",
            "displayUnit = \"${1:}\"",
        ),
        (
            "stateSelect",
            "Hint for state selection",
            "stateSelect = StateSelect.${1|default,never,avoid,prefer,always|}",
        ),
        // For arrays
        ("each", "Apply modifier to each element", "each "),
        // For replaceable
        ("redeclare", "Redeclare a replaceable element", "redeclare "),
        ("final", "Prevent further modification", "final "),
    ];

    modifiers
        .into_iter()
        .map(|(label, detail, snippet)| CompletionItem {
            label: label.to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some(detail.to_string()),
            insert_text: Some(snippet.to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        })
        .collect()
}
