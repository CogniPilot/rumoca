//! Utility functions for LSP handlers.

use lsp_types::{Position, Range};
use rumoca_compile::compile::core as rumoca_core;
use rumoca_compile::parsing::{self, DefId, ast};

/// Convert a Modelica token to an LSP Range (0-indexed).
pub fn token_to_range(token: &parsing::Token) -> Range {
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

/// Convert a Modelica source location to an LSP Range (0-indexed).
pub fn location_to_range(loc: &parsing::Location) -> Range {
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

/// Returns whether the position is inside a Modelica comment.
pub fn is_position_in_comment(text: &str, position: Position) -> bool {
    let Some(offset) = byte_offset_at_position(text, position) else {
        return false;
    };

    let bytes = text.as_bytes();
    let mut i = 0;
    let mut in_line_comment = false;
    let mut in_block_comment = false;
    let mut in_string = false;
    let mut escaped = false;

    while i < offset && i < bytes.len() {
        let byte = bytes[i];

        if in_line_comment {
            if byte == b'\n' {
                in_line_comment = false;
            }
            i += 1;
            continue;
        }

        if in_block_comment {
            if byte == b'*' && i + 1 < bytes.len() && bytes[i + 1] == b'/' {
                in_block_comment = false;
                i += 2;
            } else {
                i += 1;
            }
            continue;
        }

        if in_string {
            if escaped {
                escaped = false;
            } else if byte == b'\\' {
                escaped = true;
            } else if byte == b'"' {
                in_string = false;
            }
            i += 1;
            continue;
        }

        if byte == b'"' {
            in_string = true;
            i += 1;
            continue;
        }

        if byte == b'/' && i + 1 < bytes.len() {
            if bytes[i + 1] == b'/' {
                in_line_comment = true;
                i += 2;
                continue;
            }
            if bytes[i + 1] == b'*' {
                in_block_comment = true;
                i += 2;
                continue;
            }
        }

        i += 1;
    }

    in_line_comment || in_block_comment
}

fn byte_offset_at_position(text: &str, position: Position) -> Option<usize> {
    let mut offset = 0usize;
    for (line_index, line_with_newline) in text.split_inclusive('\n').enumerate() {
        let line = line_with_newline
            .strip_suffix('\n')
            .unwrap_or(line_with_newline);
        if line_index == position.line as usize {
            return Some(offset + (position.character as usize).min(line.len()));
        }
        offset += line_with_newline.len();
    }

    if position.line == text.lines().count() as u32 {
        return Some(text.len());
    }

    None
}

/// Get the dotted token at the given position in text.
pub fn get_dotted_token_at_position(text: &str, position: Position) -> Option<String> {
    let lines: Vec<&str> = text.lines().collect();
    let line = lines.get(position.line as usize)?;
    let col = position.character as usize;
    if col > line.len() {
        return None;
    }
    let start = line[..col]
        .rfind(|c: char| !c.is_alphanumeric() && c != '_' && c != '.')
        .map(|i| i + 1)
        .unwrap_or(0);
    let end = line[col..]
        .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '.')
        .map(|i| col + i)
        .unwrap_or(line.len());
    if start >= end {
        return None;
    }
    Some(line[start..end].to_string())
}

/// Return the dotted identifier token immediately before the cursor text.
///
/// This is intentionally token-oriented: dots inside bracketed subscript text
/// are not treated as qualified-name separators because `[`/`]` terminate the
/// token scan.
pub fn trailing_dotted_identifier_token(text_before_cursor: &str) -> Option<&str> {
    let trimmed = text_before_cursor.trim_end();
    let start = trimmed
        .char_indices()
        .rev()
        .find_map(|(idx, ch)| (!is_dotted_identifier_char(ch)).then_some(idx + ch.len_utf8()))
        .unwrap_or(0);
    let token = &trimmed[start..];
    if token.is_empty() || token.starts_with('.') || token.chars().all(|ch| ch == '.') {
        return None;
    }
    Some(token)
}

pub fn trailing_qualified_identifier_token(text_before_cursor: &str) -> Option<&str> {
    trailing_dotted_identifier_token(text_before_cursor)
        .filter(|token| rumoca_core::has_top_level_dot(token))
}

fn is_dotted_identifier_char(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_' || ch == '.'
}

/// Get a qualified class-like token at the given position in text.
pub fn get_qualified_class_name_at_position(text: &str, position: Position) -> Option<String> {
    let token = get_dotted_token_at_position(text, position)?;
    rumoca_core::has_top_level_dot(&token)
        .then_some(token)
        .filter(|token| token.chars().next().is_some_and(|c| c.is_ascii_uppercase()))
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

/// Find the qualified name of the enclosing class at a given line (0-indexed).
pub fn find_enclosing_class_qualified_name(
    ast: &ast::StoredDefinition,
    line: u32,
) -> Option<String> {
    let target_line = line + 1; // Convert to 1-indexed
    let prefix = ast
        .within
        .as_ref()
        .map(ToString::to_string)
        .unwrap_or_default();
    for (name, class) in &ast.classes {
        if let Some(found) =
            find_enclosing_class_qualified_name_inner(name, class, target_line, &prefix)
        {
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

fn find_enclosing_class_qualified_name_inner(
    name: &str,
    class: &ast::ClassDef,
    line: u32,
    prefix: &str,
) -> Option<String> {
    if class.location.start_line > line || line > class.location.end_line {
        return None;
    }

    let qualified = if prefix.is_empty() {
        name.to_string()
    } else {
        format!("{prefix}.{name}")
    };

    for (nested_name, nested) in &class.classes {
        if let Some(found) =
            find_enclosing_class_qualified_name_inner(nested_name, nested, line, &qualified)
        {
            return Some(found);
        }
    }

    Some(qualified)
}

/// Find a parsed class by fully qualified name within one stored definition.
pub fn parsed_class_by_qualified_name<'a>(
    ast: &'a ast::StoredDefinition,
    class_name: &str,
) -> Option<&'a ast::ClassDef> {
    let within_prefix = ast
        .within
        .as_ref()
        .map(ToString::to_string)
        .filter(|prefix| !prefix.is_empty());
    let relative_name = within_prefix
        .as_ref()
        .and_then(|prefix| class_name.strip_prefix(&format!("{prefix}.")))
        .unwrap_or(class_name);
    let mut parts = rumoca_core::split_path_with_indices(relative_name).into_iter();
    let first = parts.next()?;
    let mut class = ast.classes.get(first)?;
    for part in parts {
        class = class.classes.get(part)?;
    }
    Some(class)
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
        if let Some(def_id) = resolve_in_class(class, tree, name) {
            return Some(def_id);
        }
    }

    None
}

fn resolve_in_class(class: &ast::ClassDef, tree: &ast::ClassTree, name: &str) -> Option<DefId> {
    // Check component type references
    for (comp_name, comp) in &class.components {
        if comp_name == name {
            return comp.type_def_id;
        }
        if comp.type_name.to_string() == name {
            return comp.type_name.def_id;
        }
    }

    for import in &class.imports {
        if let Some(def_id) = imported_def_id(import, tree, name) {
            return Some(def_id);
        }
    }

    // Check nested classes
    for (nested_name, nested) in &class.classes {
        if nested_name == name {
            return nested.def_id;
        }
        if let Some(def_id) = resolve_in_class(nested, tree, name) {
            return Some(def_id);
        }
    }

    None
}

pub fn imported_def_id(import: &ast::Import, tree: &ast::ClassTree, name: &str) -> Option<DefId> {
    match import {
        ast::Import::Qualified { path, .. } => {
            let last = path.name.last()?.text.as_ref();
            if last == name {
                tree.get_def_id_by_name(&path.to_string())
            } else {
                None
            }
        }
        ast::Import::Renamed { alias, path, .. } => {
            if alias.text.as_ref() == name {
                tree.get_def_id_by_name(&path.to_string())
            } else {
                None
            }
        }
        ast::Import::Unqualified { path, .. } => {
            let qualified = format!("{}.{}", path, name);
            tree.get_def_id_by_name(&qualified)
        }
        ast::Import::Selective { path, names, .. } => {
            let matched = names.iter().find(|token| token.text.as_ref() == name)?;
            let qualified = format!("{}.{}", path, matched.text);
            tree.get_def_id_by_name(&qualified)
        }
    }
}
