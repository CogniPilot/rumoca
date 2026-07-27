//! Utility functions for LSP handlers.

use lsp_types::{Position, Range};
use rumoca_compile::compile::core as rumoca_core;
use rumoca_compile::parsing::{self, DefId, ast};
use rumoca_lsp_position::{
    char_column_to_utf16_column, line_text, position_to_byte_offset, span_to_range,
    utf16_column_to_byte_column,
};

/// Convert a Modelica source location to an LSP [`Range`] (0-indexed, UTF-16
/// columns) against the text the location was produced from.
///
/// `Location` carries both a UTF-8 byte span (`start`/`end`) and 1-based
/// **character** line/column pairs — the Modelica lexer advances the column
/// once per Unicode scalar. LSP `Position.character` counts UTF-16 code units,
/// so neither the lexer column nor a byte offset may be used directly. The byte
/// span is the authoritative form, so it is preferred; the character columns are
/// only a fallback for synthesized locations that carry no span.
#[must_use]
pub fn location_to_range_in_source(source: &str, loc: &parsing::Location) -> Range {
    if loc.end > loc.start && (loc.end as usize) <= source.len() {
        return span_to_range(source, loc.start as usize, loc.end as usize);
    }
    location_range_from_char_columns(source, loc)
}

/// Convert a Modelica token to an LSP [`Range`] against its own source text.
#[must_use]
pub fn token_to_range_in_source(source: &str, token: &parsing::Token) -> Range {
    location_to_range_in_source(source, &token.location)
}

/// [`location_to_range_in_source`] for callers that may not be able to resolve
/// the location's file text (a cross-file workspace hit whose document is not
/// loaded). Without text, the lexer's character columns are the only signal
/// available and are emitted as-is; that is exact for ASCII lines and the same
/// approximation the pre-UTF-16 code always produced.
#[must_use]
pub fn location_to_range_in_optional_source(
    source: Option<&str>,
    loc: &parsing::Location,
) -> Range {
    match source {
        Some(text) => location_to_range_in_source(text, loc),
        None => Range {
            start: Position {
                line: loc.start_line.saturating_sub(1),
                character: loc.start_column.saturating_sub(1),
            },
            end: Position {
                line: loc.end_line.saturating_sub(1),
                character: loc.end_column.saturating_sub(1),
            },
        },
    }
}

/// Fallback used when a location carries no usable byte span: translate the
/// lexer's 1-based character columns into 0-based UTF-16 columns using the
/// referenced source lines. Lines that are not present in `source` (a location
/// from a different file) degrade to the raw character column.
fn location_range_from_char_columns(source: &str, loc: &parsing::Location) -> Range {
    let start_line = loc.start_line.saturating_sub(1);
    let end_line = loc.end_line.saturating_sub(1);
    Range {
        start: Position {
            line: start_line,
            character: utf16_column_for(source, start_line, loc.start_column),
        },
        end: Position {
            line: end_line,
            character: utf16_column_for(source, end_line, loc.end_column),
        },
    }
}

fn utf16_column_for(source: &str, line: u32, char_column_1based: u32) -> u32 {
    match line_text(source, line) {
        Some(text) => char_column_to_utf16_column(text, char_column_1based),
        None => char_column_1based.saturating_sub(1),
    }
}

/// Get the word at the given position in text.
pub fn get_word_at_position(text: &str, position: Position) -> Option<String> {
    let (line, col) = line_and_byte_column(text, position)?;
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
    text.split('\n').nth(position.line as usize)?;
    Some(position_to_byte_offset(text, position))
}

fn line_and_byte_column(text: &str, position: Position) -> Option<(&str, usize)> {
    let line = line_text(text, position.line)?;
    Some((line, utf16_column_to_byte_column(line, position.character)))
}

/// Get the dotted token at the given position in text.
pub fn get_dotted_token_at_position(text: &str, position: Position) -> Option<String> {
    let (line, col) = line_and_byte_column(text, position)?;
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
    let (line, col) = line_and_byte_column(text, position)?;
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

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_lsp_position::byte_offset_to_position;

    fn position_of(source: &str, needle: &str) -> Position {
        byte_offset_to_position(
            source,
            source.find(needle).expect("test needle must be present"),
        )
    }

    #[test]
    fn cursor_helpers_use_utf16_columns_after_non_ascii_text() {
        let source = "model M\n  String label = \"温度 °\"; Real signal;\nend M;";
        let position = position_of(source, "signal");

        assert_eq!(
            get_word_at_position(source, position).as_deref(),
            Some("signal")
        );
        assert!(
            get_text_before_cursor(source, position)
                .is_some_and(|prefix| prefix.ends_with("Real "))
        );
    }

    #[test]
    fn dotted_token_handles_bmp_and_astral_utf16_prefixes() {
        let source = "° 𝔸 bus.温度.signal";
        let position = position_of(source, "signal");

        assert_eq!(
            get_dotted_token_at_position(source, position).as_deref(),
            Some("bus.温度.signal")
        );
    }

    #[test]
    fn comment_detection_uses_utf16_to_byte_conversion() {
        let source = "Real x; // 温度 𝔸 comment";
        let position = position_of(source, "comment");

        assert!(is_position_in_comment(source, position));
    }

    #[test]
    fn location_range_prefers_the_byte_span() {
        let source = "model M\n  Real 𝔸x = 1;\nend M;\n";
        let start = source.find("𝔸x").expect("component name");
        let loc = parsing::Location {
            // Deliberately wrong character columns: the byte span wins.
            start_line: 2,
            start_column: 1,
            end_line: 2,
            end_column: 2,
            start: start as u32,
            end: (start + "𝔸x".len()) as u32,
            ..Default::default()
        };

        let range = location_to_range_in_source(source, &loc);
        assert_eq!(range.start, byte_offset_to_position(source, start));
        assert_eq!(
            range.end,
            byte_offset_to_position(source, start + "𝔸x".len())
        );
    }

    #[test]
    fn location_range_falls_back_to_utf16_converted_char_columns() {
        // A synthesized location (no byte span) still has to report UTF-16
        // columns; `𝔸` counts as two units even though it is one lexer column.
        let source = "model M\n  Real 𝔸x = 1;\nend M;\n";
        let loc = parsing::Location {
            start_line: 2,
            start_column: 8,
            end_line: 2,
            end_column: 10,
            ..Default::default()
        };

        let range = location_to_range_in_source(source, &loc);
        assert_eq!(range.start, Position::new(1, 7));
        assert_eq!(range.end, Position::new(1, 10));
    }

    #[test]
    fn location_range_without_source_keeps_raw_char_columns() {
        // No file text available: the raw lexer columns are the only signal.
        let loc = parsing::Location {
            start_line: 3,
            start_column: 5,
            end_line: 3,
            end_column: 9,
            ..Default::default()
        };

        let range = location_to_range_in_optional_source(None, &loc);
        assert_eq!(range.start, Position::new(2, 4));
        assert_eq!(range.end, Position::new(2, 8));
    }
}
