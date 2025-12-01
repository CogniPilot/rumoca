//! Utility functions for LSP handlers.

use crate::ir::ast::StoredDefinition;
use lsp_types::Position;

/// Parse the document and return the AST if successful
pub fn parse_document(text: &str, path: &str) -> Option<StoredDefinition> {
    use crate::modelica_grammar::ModelicaGrammar;
    use crate::modelica_parser::parse;

    let mut grammar = ModelicaGrammar::new();
    if parse(text, path, &mut grammar).is_ok() {
        grammar.modelica
    } else {
        None
    }
}

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
