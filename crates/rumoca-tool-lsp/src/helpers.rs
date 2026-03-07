//! Utility functions for LSP handlers.

use lsp_types::{Position, Range};
use rumoca_session::compile::core::DefId;
use rumoca_session::parsing::ast;
use rumoca_session::parsing::ir_core as rumoca_ir_core;

/// Convert a rumoca_ir_core::Token to an LSP Range (0-indexed).
pub fn token_to_range(token: &rumoca_ir_core::Token) -> Range {
    Range {
        start: Position {
            line: token.location.start_line.saturating_sub(1),
            character: token.location.start_column.saturating_sub(1),
        },
        end: Position {
            line: token.location.end_line.saturating_sub(1),
            character: token.location.end_column.saturating_sub(1),
        },
    }
}

/// Convert a rumoca_ir_core::Location to an LSP Range (0-indexed).
pub fn location_to_range(loc: &rumoca_ir_core::Location) -> Range {
    Range {
        start: Position {
            line: loc.start_line.saturating_sub(1),
            character: loc.start_column.saturating_sub(1),
        },
        end: Position {
            line: loc.end_line.saturating_sub(1),
            character: loc.end_column.saturating_sub(1),
        },
    }
}

/// Get the word at the given position in text.
pub fn get_word_at_position(text: &str, position: Position) -> Option<String> {
    let lines: Vec<&str> = text.lines().collect();
    let line = lines.get(position.line as usize)?;
    let col = position.character as usize;
    if col > line.len() {
        return None;
    }
    let start = line[..col]
        .rfind(|c: char| !c.is_alphanumeric() && c != '_')
        .map(|i| i + 1)
        .unwrap_or(0);
    let end = line[col..]
        .find(|c: char| !c.is_alphanumeric() && c != '_')
        .map(|i| col + i)
        .unwrap_or(line.len());
    if start >= end {
        return None;
    }
    Some(line[start..end].to_string())
}

/// Get the text before the cursor on the current line.
pub fn get_text_before_cursor(text: &str, position: Position) -> Option<String> {
    let lines: Vec<&str> = text.lines().collect();
    let line = lines.get(position.line as usize)?;
    let col = (position.character as usize).min(line.len());
    Some(line[..col].to_string())
}

/// Find a component by name anywhere in the AST.
pub fn find_component_at_position<'a>(
    ast: &'a ast::StoredDefinition,
    name: &str,
) -> Option<&'a ast::Component> {
    for (_, class) in &ast.classes {
        if let Some(comp) = find_component_in_class(class, name) {
            return Some(comp);
        }
    }
    None
}

fn find_component_in_class<'a>(class: &'a ast::ClassDef, name: &str) -> Option<&'a ast::Component> {
    if let Some(comp) = class.components.get(name) {
        return Some(comp);
    }
    for (_, nested) in &class.classes {
        if let Some(comp) = find_component_in_class(nested, name) {
            return Some(comp);
        }
    }
    None
}

/// Find a class by name anywhere in the AST.
pub fn find_class_at_position<'a>(
    ast: &'a ast::StoredDefinition,
    name: &str,
) -> Option<&'a ast::ClassDef> {
    for (class_name, class) in &ast.classes {
        if class_name == name {
            return Some(class);
        }
        if let Some(found) = find_nested_class(class, name) {
            return Some(found);
        }
    }
    None
}

fn find_nested_class<'a>(class: &'a ast::ClassDef, name: &str) -> Option<&'a ast::ClassDef> {
    for (nested_name, nested) in &class.classes {
        if nested_name == name {
            return Some(nested);
        }
        if let Some(found) = find_nested_class(nested, name) {
            return Some(found);
        }
    }
    None
}

/// Find the enclosing class at a given line (0-indexed).
pub fn find_enclosing_class(ast: &ast::StoredDefinition, line: u32) -> Option<&ast::ClassDef> {
    let target_line = line + 1; // Convert to 1-indexed
    for (_, class) in &ast.classes {
        if let Some(found) = find_enclosing_class_inner(class, target_line) {
            return Some(found);
        }
    }
    None
}

fn find_enclosing_class_inner(class: &ast::ClassDef, line: u32) -> Option<&ast::ClassDef> {
    if class.location.start_line <= line && line <= class.location.end_line {
        // Check nested classes first (most specific match)
        for (_, nested) in &class.classes {
            if let Some(found) = find_enclosing_class_inner(nested, line) {
                return Some(found);
            }
        }
        return Some(class);
    }
    None
}

/// Resolve a name at position using the resolved class tree.
pub fn resolve_at_position(
    ast: &ast::StoredDefinition,
    tree: &ast::ClassTree,
    name: &str,
) -> Option<DefId> {
    // Check if it's a top-level class name
    if let Some(def_id) = tree.get_def_id_by_name(name) {
        return Some(def_id);
    }

    // Check component def_ids
    for (_, class) in &ast.classes {
        if let Some(def_id) = resolve_in_class(class, name) {
            return Some(def_id);
        }
    }

    None
}

fn resolve_in_class(class: &ast::ClassDef, name: &str) -> Option<DefId> {
    // Check component type references
    for (comp_name, comp) in &class.components {
        if comp_name == name {
            return comp.type_def_id;
        }
        if comp.type_name.to_string() == name {
            return comp.type_name.def_id;
        }
    }

    // Check nested classes
    for (nested_name, nested) in &class.classes {
        if nested_name == name {
            return nested.def_id;
        }
        if let Some(def_id) = resolve_in_class(nested, name) {
            return Some(def_id);
        }
    }

    None
}
