//! Utility functions for LSP handlers.

use crate::ir::ast::{Location, Token};
use lsp_types::{Position, Range};

// Re-export compiler parsing functions for LSP use
pub use crate::compiler::{parse_file_cached, parse_source_simple as parse_document};

/// Get the text before the cursor on the current line
pub fn get_text_before_cursor(text: &str, position: Position) -> Option<String> {
    let lines: Vec<&str> = text.lines().collect();
    let line = lines.get(position.line as usize)?;
    let col = (position.character as usize).min(line.len());
    Some(line[..col].to_string())
}

/// Get the word at the given position in text
pub fn get_word_at_position(text: &str, position: Position) -> Option<String> {
    let lines: Vec<&str> = text.lines().collect();
    let line = lines.get(position.line as usize)?;
    let col = position.character as usize;

    if col > line.len() {
        return None;
    }

    // Find word boundaries
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

/// Get a qualified name (dotted path like SI.Mass) at the given position in text
/// This is useful for type references that span multiple identifiers
pub fn get_qualified_name_at_position(text: &str, position: Position) -> Option<String> {
    let lines: Vec<&str> = text.lines().collect();
    let line = lines.get(position.line as usize)?;
    let col = position.character as usize;

    if col > line.len() {
        return None;
    }

    // Find boundaries including dots for qualified names
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

    let qualified = line[start..end].to_string();

    // Clean up leading/trailing dots
    let qualified = qualified.trim_matches('.');

    if qualified.is_empty() {
        return None;
    }

    Some(qualified.to_string())
}

/// Find the function name being called at the cursor position (for signature help)
/// Returns (function_name, active_parameter_index)
pub fn find_function_at_cursor(text: &str, position: Position) -> Option<(String, usize)> {
    let text_before = get_text_before_cursor(text, position)?;

    // Count parentheses to handle nested calls
    let mut paren_depth = 0;
    let mut current_arg = 0;
    let mut func_start = None;

    for (i, ch) in text_before.chars().rev().enumerate() {
        match ch {
            ')' => paren_depth += 1,
            '(' => {
                if paren_depth == 0 {
                    // Found our opening paren
                    func_start = Some(text_before.len() - i - 1);
                    break;
                }
                paren_depth -= 1;
            }
            ',' if paren_depth == 0 => current_arg += 1,
            _ => {}
        }
    }

    let func_start = func_start?;

    // Extract function name before the opening paren
    let before_paren = &text_before[..func_start];
    let func_name: String = before_paren
        .chars()
        .rev()
        .take_while(|c| c.is_alphanumeric() || *c == '_' || *c == '.')
        .collect::<String>()
        .chars()
        .rev()
        .collect();

    if func_name.is_empty() {
        None
    } else {
        Some((func_name, current_arg))
    }
}

/// Convert a Token to an LSP Range (0-indexed).
///
/// LSP uses 0-indexed positions, while our AST uses 1-indexed.
pub fn token_to_range(token: &Token) -> Range {
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

/// Convert a Location to an LSP Range (0-indexed).
///
/// LSP uses 0-indexed positions, while our AST uses 1-indexed.
pub fn location_to_range(loc: &Location) -> Range {
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

/// Ensure a range is valid (end >= start).
///
/// Some edge cases can produce invalid ranges. This normalizes them.
pub fn validate_range(range: Range) -> Range {
    if range.end.line < range.start.line
        || (range.end.line == range.start.line && range.end.character < range.start.character)
    {
        Range {
            start: range.start,
            end: range.start,
        }
    } else {
        range
    }
}

/// Check if a position (0-indexed) is within an LSP Range.
pub fn position_in_range(pos: Position, range: &Range) -> bool {
    if pos.line < range.start.line || pos.line > range.end.line {
        return false;
    }
    if pos.line == range.start.line && pos.character < range.start.character {
        return false;
    }
    if pos.line == range.end.line && pos.character > range.end.character {
        return false;
    }
    true
}
